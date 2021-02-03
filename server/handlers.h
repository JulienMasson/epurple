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

#ifndef HANDLERS_H
#define HANDLERS_H

#include "epurple.h"

struct buddy_data {
	char name[STR_NAME_SIZE];
	char display_name[STR_NAME_SIZE];
	char url[STR_URL_SIZE];
};

struct chat_data {
	char name[STR_NAME_SIZE];
	char url[STR_URL_SIZE];
};

struct handler {
	char command[STR_NAME_SIZE];
	void (*func)(struct epurple *epurple, int id, char *payload, size_t len);
};

struct handler *handlers_find(char *command);

PurpleConversation *create_conv(PurpleAccount *acct, int conv_type, char *conv_name);

#endif
