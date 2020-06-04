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

out:
	if (success)
		emacs_send(epurple, id, NULL, 0);
}

/* accounts */
struct account {
	char username[80];
	char alias[80];
	char protocol_id[80];
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

		strcpy(account->username, a->username);
		strcpy(account->alias, a->alias);
		strcpy(account->protocol_id, a->protocol_id);
	}

	emacs_send(epurple, id, (char *)accounts, sizeof(struct account) * count);
	free(accounts);
}

static void account_connect_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	struct account *account = (struct account *)payload;
	PurpleAccount *a;

	if ((a = purple_accounts_find(account->username, account->protocol_id)))
		purple_account_set_enabled(a, EPURPLE_UI, TRUE);
	else
		printf("Cannot find account: %s %s\n", account->username, account->protocol_id);
}

static void account_disconnect_handler(struct epurple *epurple, int id, char *payload, size_t len)
{
	struct account *account = (struct account *)payload;
	PurpleAccount *a;

	if ((a = purple_accounts_find(account->username, account->protocol_id)))
		purple_account_set_enabled(a, EPURPLE_UI, FALSE);
	else
		printf("Cannot find account: %s %s\n", account->username, account->protocol_id);
}

/* handlers */
struct handler handlers[] = {
	{"purple_init",        purple_init_handler},
	{"accounts_get_all",   accounts_get_all_handler},
	{"account_connect",    account_connect_handler},
	{"account_disconnect", account_disconnect_handler}
};

struct handler *handlers_find(char *command)
{
	for (int i = 0; i < ARRAY_SIZE(handlers); i++) {
		if (!strcmp(command, handlers[i].command))
			return &handlers[i];
	}
	return NULL;
}
