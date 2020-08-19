/* Copyright (C) 2020  Julien Masson
 *
 * Author: Julien Masson <massonju.eseo@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "ops.h"
#include "emacs.h"
#define PURPLE_GLIB_READ_COND  (G_IO_IN  | G_IO_HUP | G_IO_ERR)
#define PURPLE_GLIB_WRITE_COND (G_IO_OUT | G_IO_HUP | G_IO_ERR | G_IO_NVAL)

/* core */
static void purple_disconnect_all(void)
{
	PurpleAccount *account;
	GList *acl;

	for (acl = purple_accounts_get_all(); acl; acl = acl->next) {
		account = acl->data;
		if (!account) continue;
		purple_account_set_enabled(account, EPURPLE_UI, FALSE);
	}
}

static void chat_buddy_joined(PurpleConversation *conv, const char *user,
			      PurpleConvChatBuddyFlags flags, gboolean new_arrival, void *data)
{
	printf("chat_buddy_joined\n");
}

static void chat_buddy_left(PurpleConversation *conv, const char *user, const char *reason,
			    void *data)
{
	printf("chat_buddy_left\n");
}

struct buddy_typing_update_data {
	char account_username[STR_NAME_SIZE];
	char buddy_name[STR_NAME_SIZE];
	char conv_name[STR_NAME_SIZE];
	int  typing;
};

static void buddy_typing_update(PurpleAccount *account, const char *name, void *data)
{
	struct epurple *epurple = (struct epurple *)data;
	struct buddy_typing_update_data buddy_typing_update_data;
	PurpleConversation *conv;
	PurpleTypingState state;
	int typing = 0;

	conv = purple_find_conversation_with_account(PURPLE_CONV_TYPE_IM, name, account);
	if (!conv) return;

	state = purple_conv_im_get_typing_state(PURPLE_CONV_IM(conv));
	if (state == PURPLE_TYPING)
		typing = 1;

	memset(buddy_typing_update_data.account_username, '\0', STR_NAME_SIZE);
	strncpy(buddy_typing_update_data.account_username, account->username, STR_NAME_SIZE);

	memset(buddy_typing_update_data.buddy_name, '\0', STR_NAME_SIZE);
	strncpy(buddy_typing_update_data.buddy_name, name, STR_NAME_SIZE);

	memset(buddy_typing_update_data.conv_name, '\0', STR_NAME_SIZE);
	strncpy(buddy_typing_update_data.conv_name, conv->name, STR_NAME_SIZE);

	buddy_typing_update_data.typing = typing;

	printf("buddy_typing_update: %s -> %d\n", name, typing);
	emacs_send(epurple, "buddy_typing_update", 0, (char *)&buddy_typing_update_data,
		   sizeof(struct buddy_typing_update_data));
}

static void ui_init(void)
{
	struct epurple *epurple = epurple_get();
	void *conv_instance;
	static int handle;

	purple_disconnect_all();

	/* connections */
	purple_connections_set_ui_ops(&connection_ops);

	/* conversations */
	purple_conversations_set_ui_ops(&conversation_ops);
	conv_instance = purple_conversations_get_handle();
	purple_signal_connect(conv_instance, "buddy-typing", &handle,
			      PURPLE_CALLBACK(buddy_typing_update), epurple);
	purple_signal_connect(conv_instance, "buddy-typing-stopped", &handle,
			      PURPLE_CALLBACK(buddy_typing_update), epurple);
	purple_signal_connect(conv_instance, "chat-buddy-joined", &handle,
			      PURPLE_CALLBACK(chat_buddy_joined), epurple);
	purple_signal_connect(conv_instance, "chat-buddy-left", &handle,
			      PURPLE_CALLBACK(chat_buddy_left), epurple);

	/* blist */
	purple_blist_set_ui_ops(&blist_ops);

	emacs_send(epurple, "purple_init_done", 0, NULL, 0);
}

PurpleCoreUiOps core_ops = {
	NULL, /* ui_prefs_init */
	NULL, /* debug_ui_init */
	ui_init,
	NULL, /* quit */
	NULL, /* get_ui_info */
	NULL,
	NULL,
	NULL
};

/* eventloop */
struct eventloop_input_data {
	PurpleInputFunction func;
	gpointer data;
};

static gboolean eventloop_input_handler(GIOChannel *source, GIOCondition cond, gpointer data)
{
	struct eventloop_input_data *input_data = data;
	PurpleInputCondition purple_cond = 0;
	int fd = g_io_channel_unix_get_fd(source);

	if (cond & PURPLE_GLIB_READ_COND)
		purple_cond |= PURPLE_INPUT_READ;
	if (cond & PURPLE_GLIB_WRITE_COND)
		purple_cond |= PURPLE_INPUT_WRITE;

	input_data->func(input_data->data, fd, purple_cond);

	return TRUE;
}

static guint eventloop_input_add(int fd, PurpleInputCondition cond, PurpleInputFunction func,
				 gpointer data)
{
	struct eventloop_input_data *input_data;
	GIOCondition gio_cond = 0;
	GIOChannel *channel;
	int source_id;

	if (cond & PURPLE_INPUT_READ)
		gio_cond |= PURPLE_GLIB_READ_COND;
	if (cond & PURPLE_INPUT_WRITE)
		gio_cond |= PURPLE_GLIB_WRITE_COND;

	input_data = malloc(sizeof(struct eventloop_input_data));
	input_data->func = func;
	input_data->data = data;

	channel = g_io_channel_unix_new(fd);
	source_id = g_io_add_watch_full(channel, G_PRIORITY_DEFAULT, gio_cond,
					eventloop_input_handler, input_data, g_free);
	g_io_channel_unref(channel);

	return source_id;
}

PurpleEventLoopUiOps eventloop_ops = {
	g_timeout_add,
	g_source_remove,
	eventloop_input_add,
	g_source_remove,
	NULL, /* input_get_error, */
	NULL, /* timeout_add_seconds */
	NULL,
	NULL,
	NULL
};

/* connections */
static void connect_progress(PurpleConnection *gc,const char *text, size_t step,
			     size_t step_count)
{
	PurpleAccount *acct = purple_connection_get_account(gc);
	if (acct)
		printf("Account progress: %s\n", acct->username);
}

static void connected(PurpleConnection *gc)
{
	PurpleAccount *acct = purple_connection_get_account(gc);
	struct epurple *epurple = epurple_get();
	char username[STR_NAME_SIZE];

	if (acct) {
		printf("Account connected: %s\n", acct->username);
		strncpy(username, acct->username, STR_NAME_SIZE);
		emacs_send(epurple, "account_connected", 0, username, STR_NAME_SIZE);
	}
}

static void disconnected(PurpleConnection *gc)
{
	PurpleAccount *acct = purple_connection_get_account(gc);
	struct epurple *epurple = epurple_get();
	char username[STR_NAME_SIZE];

	if (acct) {
		printf("Account disconnected: %s\n", acct->username);
		strncpy(username, acct->username, STR_NAME_SIZE);
		emacs_send(epurple, "account_disconnected", 0, username, STR_NAME_SIZE);
	}
}

PurpleConnectionUiOps connection_ops = {
    connect_progress,
    connected,
    disconnected,
    NULL, /* notice */
    NULL, /* report_disconnect */
    NULL, /* network_connected */
    NULL, /* network_disconnected */
    NULL, /* report_disconnect_reason */
    NULL,
    NULL,
    NULL
};

/* conversation */
struct new_msg_header {
	char username[STR_NAME_SIZE];
	int  conv_type;
	char conv_name[STR_NAME_SIZE];
	char sender[STR_NAME_SIZE];
	int  flags;
	int  time;
	int  msg_size;
};

static void create_conversation(PurpleConversation *conv)
{
	printf("create_conversation\n");
}

static void destroy_conversation(PurpleConversation *conv)
{
	printf("destroy_conversation\n");
}

static void new_msg(PurpleConversation *conv, const char *who, const char *message,
		    PurpleMessageFlags flags, time_t mtime)
{
	struct epurple *epurple = epurple_get();
	struct new_msg_header msg_header;
	char *data;
	int msg_header_size, data_size;

	memset(msg_header.username, '\0', STR_NAME_SIZE);
	snprintf(msg_header.username, STR_NAME_SIZE, "%s", conv->account->username);

	msg_header.conv_type = conv->type;

	memset(msg_header.conv_name, '\0', STR_NAME_SIZE);
	snprintf(msg_header.conv_name, STR_NAME_SIZE, "%s", conv->name);

	memset(msg_header.sender, '\0', STR_NAME_SIZE);
	snprintf(msg_header.sender, STR_NAME_SIZE, "%s", who);

	msg_header.flags = flags;
	msg_header.time = mtime;
	msg_header.msg_size = strlen(message);

	msg_header_size = sizeof(struct new_msg_header);
	data_size = msg_header_size + msg_header.msg_size;

	data = malloc(data_size);
	memcpy(data, &msg_header, msg_header_size);

	memset(data + msg_header_size, '\0', msg_header.msg_size);
	memcpy(data + msg_header_size, message, msg_header.msg_size);

	printf("new_msg: %s -> %d: %s\n", who, msg_header.msg_size, message);
	emacs_send(epurple, "new_msg", 0, data, data_size);
	free(data);
}

static void write_conv(PurpleConversation *conv, const char *name, const char *alias,
		       const char *message, PurpleMessageFlags flags, time_t mtime)
{
	printf("write_conv: %s (%s) -> %s\n", name, alias, message);
}

static void chat_add_users(PurpleConversation *conv, GList *cbuddies, gboolean new_arrivals)
{
	printf("chat_add_users\n");
}

static void chat_remove_users(PurpleConversation *conv, GList *users)
{
	printf("chat_remove_users\n");
}

static void chat_rename_user(PurpleConversation *conv, const char *old_name,
			     const char *new_name, const char *new_alias)
{
	printf("chat_rename_user\n");
}

static void chat_update_user(PurpleConversation *conv, const char *user)
{
	printf("chat_update_user\n");
}

static void present(PurpleConversation *conv)
{
	printf("present\n");
}

static void send_confirm(PurpleConversation *conv, const char *message)
{
	printf("send_confirm\n");
}

static gboolean has_focus(PurpleConversation *conv)
{
	printf("has_focus");
	return TRUE;
}

PurpleConversationUiOps conversation_ops = {
    create_conversation,
    destroy_conversation,
    new_msg,
    new_msg,
    write_conv,
    chat_add_users,
    chat_rename_user,
    chat_remove_users,
    chat_update_user,
    present,
    has_focus,
    NULL, /* custom_smiley_add */
    NULL, /* custom_smiley_write */
    NULL, /* custom_smiley_close */
    send_confirm,
    NULL,
    NULL,
    NULL,
    NULL
};

/* blist */
struct buddy_update_data {
	char account_username[STR_NAME_SIZE];
	char buddy_name[STR_NAME_SIZE];
	char icon[STR_NAME_SIZE];
	int  available;
};

static void blist_update(PurpleBuddyList *list, PurpleBlistNode *node)
{
	if (PURPLE_BLIST_NODE_IS_BUDDY(node)) {

		struct epurple *epurple = epurple_get();
		PurpleBuddy* buddy = (PurpleBuddy*)node;
		struct buddy_update_data buddy_update_data;
		PurplePresence *presence;
		char *icon = NULL;

		memset(buddy_update_data.account_username, '\0', STR_NAME_SIZE);
		strncpy(buddy_update_data.account_username, buddy->account->username, STR_NAME_SIZE);

		memset(buddy_update_data.buddy_name, '\0', STR_NAME_SIZE);
		strncpy(buddy_update_data.buddy_name, buddy->name, STR_NAME_SIZE);

		memset(buddy_update_data.icon, '\0',  STR_NAME_SIZE);
		if (buddy->icon)
			icon = purple_buddy_icon_get_full_path(buddy->icon);
		if (icon)
			strncpy(buddy_update_data.icon, icon, STR_NAME_SIZE);

		presence = purple_buddy_get_presence(buddy);
		if (!presence) return;
		buddy_update_data.available = purple_presence_is_available(presence);

		printf("Update: %s -> %d (%s)\n", buddy_update_data.buddy_name,
		       buddy_update_data.available, buddy_update_data.icon);
		emacs_send(epurple, "buddy_update", 0, (char *)&buddy_update_data,
			   sizeof(struct buddy_update_data));
	}
}

PurpleBlistUiOps blist_ops =
{
	NULL, /* new_list */
	NULL, /* new_node */
	NULL, /* show */
	blist_update,
	NULL, /* remove */
	NULL, /* destroy */
	NULL, /* set_visible */
	NULL, /* request_add_buddy */
	NULL, /* request_add_chat */
	NULL, /* request_add_group */
	NULL, /* save_node */
	NULL, /* remove_node */
	NULL, /* save_account */
	NULL
};
