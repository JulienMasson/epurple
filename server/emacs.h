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

#ifndef EMACS_H
#define EMACS_H

#include "epurple.h"

gboolean emacs_handler(GIOChannel *in, GIOCondition cond, gpointer data);

void emacs_send(struct epurple *epurple, char *command, int id, char *payload, size_t len);

void emacs_ack(struct epurple *epurple, int id);

#endif
