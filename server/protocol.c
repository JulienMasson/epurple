/* Copyright (C) 2021  Julien Masson
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

#include "protocol.h"

struct protocol_ops {
	void (*fill_buddy) (PurpleBuddy *buddy, struct buddy_data *data);
	void (*hook_buddy) (PurpleAccount *acct, PurpleBuddy *buddy);
	void (*fill_chat)  (PurpleChat *chat, struct chat_data *data);
	void (*hook_chat)  (PurpleAccount *acct, PurpleChat *chat);
};

/* default */
static void default_fill_buddy(PurpleBuddy *buddy, struct buddy_data *data)
{
	if (buddy->name)
		strncpy(data->name, buddy->name, STR_NAME_SIZE);

	if (buddy->server_alias)
		strncpy(data->display_name, buddy->server_alias, STR_NAME_SIZE);
}

static void default_hook_buddy(PurpleAccount *acct, PurpleBuddy *buddy)
{
}

static void default_fill_chat(PurpleChat *chat, struct chat_data *data)
{
	if (chat->alias)
		strncpy(data->name, chat->alias, STR_NAME_SIZE);
}

static void default_hook_chat(PurpleAccount *acct, PurpleChat *chat)
{
}

/* slack */
static void slack_fill_buddy(PurpleBuddy *buddy, struct buddy_data *data)
{
	if (buddy->name) {
		strncpy(data->name, buddy->name, STR_NAME_SIZE);
		strncpy(data->display_name, buddy->name, STR_NAME_SIZE);
	}

	if (buddy->node.ui_data)
		snprintf(data->url, STR_URL_SIZE, "https://app.slack.com/client/%s",
			 buddy->node.ui_data);
}

static void slack_hook_buddy(PurpleAccount *acct, PurpleBuddy *buddy)
{
	/* create conv to get unread messages  */
	if (buddy->name)
		create_conv(acct, PURPLE_CONV_TYPE_IM, buddy->name);
}

static void slack_fill_chat(PurpleChat *chat, struct chat_data *data)
{
	if (chat->node.ui_data) {
		char chat_data[256];
		memset(chat_data, '\0', 256);
		strncpy(chat_data, chat->node.ui_data, 256);

		char *id = strtok(chat_data, ";");
		char *name = strtok(NULL, ";");

		char url[STR_URL_SIZE];
		memset(url, '\0',  STR_URL_SIZE);
		snprintf(url, STR_URL_SIZE, "https://app.slack.com/client/%s", id);

		strncpy(data->name, name, STR_NAME_SIZE);
		strncpy(data->url, url, STR_URL_SIZE);
	}
}

static void slack_hook_chat(PurpleAccount *acct, PurpleChat *chat)
{
	if (chat->node.ui_data) {
		/* create conv to get unread messages  */
		char chat_data[256];
		memset(chat_data, '\0', 256);
		strncpy(chat_data, chat->node.ui_data, 256);

		strtok(chat_data, ";");
		char *name = strtok(NULL, ";");

		create_conv(acct, PURPLE_CONV_TYPE_CHAT, name);
	}
}

static struct protocol_ops slack_ops = {
	slack_fill_buddy,
	slack_hook_buddy,
	slack_fill_chat,
	slack_hook_chat,
};

/* facebook */
static struct protocol_ops facebook_ops = {
	default_fill_buddy,
	default_hook_buddy,
	default_fill_chat,
	default_hook_chat,
};

/* whatsapp */
static struct protocol_ops whatsapp_ops = {
	default_fill_buddy,
	default_hook_buddy,
	default_fill_chat,
	default_hook_chat,
};

/* irc */
static struct protocol_ops irc_ops = {
	default_fill_buddy,
	default_hook_buddy,
	default_fill_chat,
	default_hook_chat,
};

/* ops */
static struct protocol_ops *protocol_find(char *protocol_id)
{
	struct protocol_ops *ops = NULL;

	if (!strcmp(protocol_id, "prpl-slack"))
		ops = &slack_ops;
	else if (!strcmp(protocol_id, "prpl-facebook"))
		ops = &facebook_ops;
	else if (!strcmp(protocol_id, "prpl-hehoe-gowhatsapp"))
		ops = &whatsapp_ops;
	else if (!strcmp(protocol_id, "prpl-irc"))
		ops = &irc_ops;
	else
		LOGE("Protocol %s not handled", protocol_id);

	return ops;
}

void protocol_fill_buddy(char *protocol_id, PurpleBuddy *buddy, struct buddy_data *data)
{
	struct protocol_ops *ops = protocol_find(protocol_id);
	if (ops) ops->fill_buddy(buddy, data);
}

void protocol_hook_buddy(char *protocol_id, PurpleAccount *acct, PurpleBuddy *buddy)
{
	struct protocol_ops *ops = protocol_find(protocol_id);
	if (ops) ops->hook_buddy(acct, buddy);
}

void protocol_fill_chat(char *protocol_id, PurpleChat *chat, struct chat_data *data)
{
	struct protocol_ops *ops = protocol_find(protocol_id);
	if (ops) ops->fill_chat(chat, data);
}

void protocol_hook_chat(char *protocol_id, PurpleAccount *acct, PurpleChat *chat)
{
	struct protocol_ops *ops = protocol_find(protocol_id);
	if (ops) ops->hook_chat(acct, chat);
}
