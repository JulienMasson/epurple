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

#define EPURPLE_UI "epurple"

/* purple */
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

static void purple_init_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	char path[256];
	int success = 1;

	purple_debug_set_enabled(FALSE);

	purple_eventloop_set_ui_ops(&eventloop_ops);
	purple_connections_set_ui_ops(&connection_ops);
	purple_conversations_set_ui_ops(&conversation_ops);

	if (purple_get_core()) {
		printf("libpurple already initialised\n");
		goto out;
	}

	snprintf(path, 256, "%splugins", purple_user_dir());
	purple_plugins_add_search_path(path);

	if (purple_core_init(EPURPLE_UI) == FALSE) {
		perror("Failed to init purple core");
		success = 0;
		goto out;
	}
	purple_set_blist(purple_blist_new());
	purple_disconnect_all();

out:
	if (success)
		emacs_ack(epurple, id);
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
		printf("Cannot find account: %s %s\n", account->username, account->protocol_id);
}

static void account_disconnect_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	struct account *account = (struct account *)payload;
	PurpleAccount *acct;

	if ((acct = purple_accounts_find(account->username, account->protocol_id)))
		purple_account_set_enabled(acct, EPURPLE_UI, FALSE);
	else
		printf("Cannot find account: %s %s\n", account->username, account->protocol_id);
}

/* buddies */
struct buddy_data {
	char name[STR_NAME_SIZE];
	char alias[STR_NAME_SIZE];
	char server_alias[STR_NAME_SIZE];
	char icon[STR_NAME_SIZE];
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
	char *icon = NULL;

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

		memset(buddy_data->icon, '\0',  STR_NAME_SIZE);
		if (buddy->icon)
			icon = purple_buddy_icon_get_full_path(buddy->icon);
		if (icon)
			strncpy(buddy_data->icon, icon, STR_NAME_SIZE);
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
			}
		}
		node = purple_blist_node_next(node, TRUE);
	}

	emacs_send(epurple, NULL, id, chats_data, STR_NAME_SIZE * count);
	free(chats_data);
}

/* conv */
struct conv_data {
	char username[STR_NAME_SIZE];
	char protocol_id[STR_NAME_SIZE];
	int  conv_type;
	char conv_name[STR_NAME_SIZE];
};

static void create_conv_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	struct conv_data *conv_data = (struct conv_data *)payload;
	PurpleAccount *acct;
	PurpleConversation *conv;

	acct = purple_accounts_find(conv_data->username, conv_data->protocol_id);
	if (!acct) return;

	conv = purple_conversation_new(conv_data->conv_type, acct, conv_data->conv_name);
	if (!conv) return;

	emacs_ack(epurple, id);
}

/* msg */
struct send_msg_data {
	char username[STR_NAME_SIZE];
	char protocol_id[STR_NAME_SIZE];
	int  conv_type;
	char conv_name[STR_NAME_SIZE];
	char msg[MAX_MSG_SIZE];
};

static void send_msg_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	struct send_msg_data *send_msg = (struct send_msg_data *)payload;
	PurpleAccount *acct;
	PurpleConversation *conv;

	acct = purple_accounts_find(send_msg->username, send_msg->protocol_id);
	if (!acct) return;

	conv = purple_find_conversation_with_account(send_msg->conv_type,
						     send_msg->conv_name,
						     acct);
	if (!conv) return;

	if (send_msg->conv_type == PURPLE_CONV_TYPE_IM)
		purple_conv_im_send(PURPLE_CONV_IM(conv), send_msg->msg);
	else if (send_msg->conv_type == PURPLE_CONV_TYPE_CHAT)
		purple_conv_chat_send(PURPLE_CONV_CHAT(conv), send_msg->msg);
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
	{"create_conv",        create_conv_handler},
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
