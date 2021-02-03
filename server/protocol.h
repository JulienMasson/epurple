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

#ifndef PROTOCOL_H
#define PROTOCOL_H

#include "epurple.h"
#include "handlers.h"

void protocol_fill_buddy(char *protocol_id, PurpleBuddy *buddy, struct buddy_data *data);
void protocol_hook_buddy(char *protocol_id, PurpleAccount *acct, PurpleBuddy *buddy);
void protocol_fill_chat(char *protocol_id, PurpleChat *chat, struct chat_data *data);
void protocol_hook_chat(char *protocol_id, PurpleAccount *acct, PurpleChat *chat);

#endif
