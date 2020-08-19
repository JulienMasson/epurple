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

#include "epurple.h"
#include "emacs.h"

#define SERVER_SOCK_FILE ".epurple.sock"

struct epurple epurple;

static void cleanup()
{
	unlink(SERVER_SOCK_FILE);

	if (epurple.sock_fd >= 0)
		close(epurple.sock_fd);

	if (epurple.emacs_fd >= 0)
		close(epurple.emacs_fd);
}

static int wait_connection()
{
	struct sockaddr_un addr;
	int sock_fd, emacs_fd;
	GIOChannel *emacs_channel;

	if ((sock_fd = socket(PF_UNIX, SOCK_STREAM, 0)) < 0) {
		perror("Failed to create socket");
		return -1;
	}
	epurple.sock_fd = sock_fd;

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strcpy(addr.sun_path, SERVER_SOCK_FILE);
	unlink(SERVER_SOCK_FILE);
	if (bind(sock_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
		perror("Failed to bind the socket");
		return -1;
	}

	if (listen(sock_fd, 5) < 0) {
		perror("Failed to mark the socket as passive");
		return -1;
	}

	printf("Waiting Emacs\n");
	if ((emacs_fd = accept(sock_fd, NULL, NULL)) < 0) {
		perror("Failed to accept connection");
		return -1;
	}
	epurple.emacs_fd = emacs_fd;
	emacs_channel = g_io_channel_unix_new(emacs_fd);
	g_io_add_watch(emacs_channel, G_IO_IN | G_IO_PRI, emacs_handler, &epurple);
	printf("Got connexion\n");

	return 0;
}

struct epurple *epurple_get(void)
{
	return &epurple;
}

int main(void)
{
	GMainLoop *loop = g_main_loop_new(NULL, FALSE);
	int status = EXIT_SUCCESS;

	if (wait_connection()) {
		status = EXIT_FAILURE;
		goto out;
	}

	g_main_loop_run(loop);
out:
	cleanup();
	return status;
}
