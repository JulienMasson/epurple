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

/* accounts */
struct account {
	int  index;
	char username[80];
	char alias[80];
	char protocol_id[80];
};

void accounts_get_all_handler(struct epurple *epurple, int id, char *data, size_t len)
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

		account->index = count;
		strcpy(account->username, a->username);
		strcpy(account->alias, a->alias);
		strcpy(account->protocol_id, a->protocol_id);
	}

	emacs_send(epurple, id, (char *)accounts, sizeof(struct account) * count);
	free(accounts);
}

/* handlers */
struct handler handlers[] = {
	{"accounts_get_all", accounts_get_all_handler}
};

struct handler *handlers_find(char *command)
{
	for (int i = 0; i < ARRAY_SIZE(handlers); i++) {
		if (!strcmp(command, handlers[i].command))
			return &handlers[i];
	}
	return NULL;
}
