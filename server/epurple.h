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

#ifndef EPURPLE_H
#define EPURPLE_H

#include <errno.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/timerfd.h>
#include <sys/un.h>
#include <unistd.h>

#include <glib.h>
#include <libpurple/account.h>
#include <libpurple/core.h>
#include <libpurple/debug.h>
#include <libpurple/eventloop.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof(x[0]))
#define STR_NAME_SIZE 80
#define MAX_BUF_SIZE  4096
#define MAX_MSG_SIZE  512
#define MAX_POLL_FD   256

struct epurple;

typedef void (*epurple_hander) (struct epurple *epurple, int fd, void *data);

struct epurple_event {
	epurple_hander handler;
	void *data;
};

struct epurple {
	int sock_fd;
	int emacs_fd;
	struct epurple_event events[MAX_POLL_FD];
};

struct epurple *epurple_get(void);

uint epurple_add_event(int fd, short events, epurple_hander handler, void *data);

void epurple_remove_event(uint handle);

#endif
