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

#include "emacs.h"
#include "handlers.h"
#include "ops.h"

/* purple */
static void purple_init_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	char path[256];

	purple_debug_set_enabled(FALSE);

	purple_core_set_ui_ops(&core_ops);
	purple_eventloop_set_ui_ops(&eventloop_ops);

	if (purple_get_core()) {
		LOGW("libpurple already initialised");
		return;
	}

	snprintf(path, 256, "%splugins", purple_user_dir());
	purple_plugins_add_search_path(path);

	if (purple_core_init(EPURPLE_UI) == FALSE)
		perror("Failed to init purple core");
	else
		purple_set_blist(purple_blist_new());
}

/* accounts */
struct account {
	char username[STR_NAME_SIZE];
	char alias[STR_NAME_SIZE];
	char protocol_id[STR_NAME_SIZE];
};

static void accounts_get_all_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	struct account *accounts = NULL;
	struct account *account;
	PurpleAccount *a;
	GList *acl;
	int count = 0;

	for (acl = purple_accounts_get_all(); acl; acl = acl->next) {
		a = acl->data;
		if (!a) continue;

		count++;
		accounts = realloc(accounts, sizeof(struct account) * count);
		account = accounts + count - 1;

		strncpy(account->username, a->username, STR_NAME_SIZE);
		strncpy(account->alias, a->alias, STR_NAME_SIZE);
		strncpy(account->protocol_id, a->protocol_id, STR_NAME_SIZE);
	}

	emacs_send(epurple, NULL, id, (char *)accounts, sizeof(struct account) * count);
	free(accounts);
}

static void account_connect_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	struct account *account = (struct account *)payload;
	PurpleAccount *acct;

	if ((acct = purple_accounts_find(account->username, account->protocol_id)))
		purple_account_set_enabled(acct, EPURPLE_UI, TRUE);
	else
		LOGE("Cannot find account: %s %s", account->username, account->protocol_id);
}

static void account_disconnect_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	struct account *account = (struct account *)payload;
	PurpleAccount *acct;

	if ((acct = purple_accounts_find(account->username, account->protocol_id)))
		purple_account_set_enabled(acct, EPURPLE_UI, FALSE);
	else
		LOGE("Cannot find account: %s %s", account->username, account->protocol_id);
}

/* conv */
struct conv_data {
	char username[STR_NAME_SIZE];
	char protocol_id[STR_NAME_SIZE];
	int  conv_type;
	char conv_name[STR_NAME_SIZE];
};

static PurpleConversation *create_conv(PurpleAccount *acct, int conv_type, char *conv_name)
{
	PurpleConversation *conv;

	LOGD("Creating conv: %d - %s", conv_type, conv_name);
	conv = purple_conversation_new(conv_type, acct, conv_name);
	if (!conv) return NULL;

	if (conv_type == PURPLE_CONV_TYPE_CHAT) {
		PurpleConnection *gc = purple_account_get_connection(acct);
		if (!gc) return NULL;

		PurpleChat *chat = purple_blist_find_chat(acct, conv_name);
		if (!chat) return NULL;

		serv_join_chat(gc, purple_chat_get_components(chat));
	}

	return conv;
}

static PurpleConversation *find_conv(PurpleAccount *acct, int conv_type, char *conv_name)
{
	PurpleConversation *conv;
	conv = purple_find_conversation_with_account(conv_type, conv_name, acct);
	if (!conv)
		return create_conv(acct, conv_type, conv_name);
	return conv;
}

static void get_conv_url(PurpleConversation *conv, char *conv_url, size_t len)
{
	LOGW("Not implemented yet !");
	snprintf(conv_url, len, "http://google.com/");
}

static void find_conv_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	struct conv_data *conv_data = (struct conv_data *)payload;
	PurpleAccount *acct;
	PurpleConversation *conv;
	char conv_url[STR_NAME_SIZE];

	acct = purple_accounts_find(conv_data->username, conv_data->protocol_id);
	if (!acct) return;

	conv = find_conv(acct, conv_data->conv_type, conv_data->conv_name);
	if (conv) {
		memset(conv_url, '\0', STR_NAME_SIZE);
		get_conv_url(conv, conv_url, STR_NAME_SIZE);
		emacs_send(epurple, NULL, id, conv_url, STR_NAME_SIZE);
	}
}

static void update_conv_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	struct conv_data *conv_data = (struct conv_data *)payload;
	PurpleAccount *acct;
	PurpleConversation *conv;

	acct = purple_accounts_find(conv_data->username, conv_data->protocol_id);
	if (!acct) return;

	conv = purple_find_conversation_with_account(conv_data->conv_type,
						     conv_data->conv_name,
						     acct);
	if (!conv) return;

	LOGD("purple_conversation_update");
	purple_conversation_update(conv, PURPLE_CONV_UPDATE_UNSEEN);
}

/* buddies */
struct buddy_data {
	char name[STR_NAME_SIZE];
	char alias[STR_NAME_SIZE];
	char server_alias[STR_NAME_SIZE];
};

static void buddies_get_all_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	struct account *account = (struct account *)payload;
	struct buddy_data *buddies_data = NULL;
	struct buddy_data *buddy_data;
	GSList *buddies, *l;
	PurpleBuddy *buddy;
	PurpleAccount *acct;
	int count = 0;

	acct = purple_accounts_find(account->username, account->protocol_id);
	if (!acct) return;

	buddies = purple_find_buddies(acct, NULL);
	for (l = buddies; l; l = l->next) {
		buddy = l->data;
		if (!buddy) continue;

		count++;
		buddies_data = realloc(buddies_data, sizeof(struct buddy_data) * count);
		buddy_data = buddies_data + count - 1;

		memset(buddy_data->name, '\0',  STR_NAME_SIZE);
		if (buddy->name)
			strncpy(buddy_data->name, buddy->name, STR_NAME_SIZE);

		memset(buddy_data->alias, '\0',  STR_NAME_SIZE);
		if (buddy->alias)
			strncpy(buddy_data->alias, buddy->alias, STR_NAME_SIZE);

		memset(buddy_data->server_alias, '\0',  STR_NAME_SIZE);
		if (buddy->server_alias)
			strncpy(buddy_data->server_alias, buddy->server_alias, STR_NAME_SIZE);

		/* For Slack account, we need to create conv to get unread messages  */
		if (!strcmp(account->protocol_id, "prpl-slack"))
			create_conv(acct, PURPLE_CONV_TYPE_IM, buddy_data->name);
	}
	g_slist_free(buddies);

	emacs_send(epurple, NULL, id, (char *)buddies_data, sizeof(struct buddy_data) * count);
	free(buddies_data);
}

/* chats */
static void chats_get_all_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	struct account *account = (struct account *)payload;
	PurpleAccount *acct;
	PurpleBlistNode *node;
	PurpleChat *chat;
	int count = 0;
	char *chats_data = NULL;
	char *chat_data;

	acct = purple_accounts_find(account->username, account->protocol_id);
	if (!acct) return;

	node = purple_blist_get_root();
	while (node) {
		if (PURPLE_BLIST_NODE_IS_CHAT(node)) {
			chat = (PurpleChat*)node;
			if (chat->account == acct) {
				count++;
				chats_data = realloc(chats_data, STR_NAME_SIZE * count);
				chat_data = chats_data + (STR_NAME_SIZE * (count - 1));

				memset(chat_data, '\0',  STR_NAME_SIZE);
				strncpy(chat_data, chat->alias, STR_NAME_SIZE);

				/* For Slack account, we need to create conv to get unread messages  */
				if (!strcmp(account->protocol_id, "prpl-slack"))
					create_conv(acct, PURPLE_CONV_TYPE_CHAT, chat->alias);
			}
		}
		node = purple_blist_node_next(node, TRUE);
	}

	emacs_send(epurple, NULL, id, chats_data, STR_NAME_SIZE * count);
	free(chats_data);
}

/* log level */
static void log_level_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	log_level = *(int *)payload;
	LOGI("Set log level: %d", log_level);
}

/* msg */
struct send_msg_data {
	char username[STR_NAME_SIZE];
	char protocol_id[STR_NAME_SIZE];
	int  conv_type;
	char conv_name[STR_NAME_SIZE];
};

static void send_msg_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	struct send_msg_data *send_msg = (struct send_msg_data *)payload;
	char *msg = payload + sizeof(struct send_msg_data);
	PurpleAccount *acct;
	PurpleConversation *conv;

	acct = purple_accounts_find(send_msg->username, send_msg->protocol_id);
	if (!acct) return;

	conv = purple_find_conversation_with_account(send_msg->conv_type,
						     send_msg->conv_name,
						     acct);
	if (!conv) return;

	if (send_msg->conv_type == PURPLE_CONV_TYPE_IM)
		purple_conv_im_send(PURPLE_CONV_IM(conv), msg);
	else if (send_msg->conv_type == PURPLE_CONV_TYPE_CHAT)
		purple_conv_chat_send(PURPLE_CONV_CHAT(conv), msg);
	else
		printf("Unknown conv type\n");
}

/* handlers */
struct handler handlers[] = {
	{"accounts_get_all",   accounts_get_all_handler},
	{"account_connect",    account_connect_handler},
	{"account_disconnect", account_disconnect_handler},
	{"buddies_get_all",    buddies_get_all_handler},
	{"chats_get_all",      chats_get_all_handler},
	{"find_conv",          find_conv_handler},
	{"log_level",          log_level_handler},
	{"update_conv",        update_conv_handler},
	{"purple_init",        purple_init_handler},
	{"send_msg",           send_msg_handler}
};

struct handler *handlers_find(char *command)
{
	for (int i = 0; i < ARRAY_SIZE(handlers); i++) {
		if (!strcmp(command, handlers[i].command))
			return &handlers[i];
	}
	return NULL;
}
