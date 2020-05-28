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
#define MAX_BUF_SIZE     4096

static void cleanup(struct epurple *epurple)
{
	unlink(SERVER_SOCK_FILE);

	if (epurple->sock_fd >= 0)
		close(epurple->sock_fd);

	if (epurple->emacs_fd >= 0)
		close(epurple->emacs_fd);
}

static void handler(struct epurple *epurple, int fd, char *buf, size_t len)
{
	if (epurple->emacs_fd == fd)
		emacs_handler(epurple, buf, len);
}

static void loop(struct epurple *epurple)
{
	struct pollfd *fds;
	char buf[MAX_BUF_SIZE];
	ssize_t len;
	int i, nfds = 1;

	fds = malloc(sizeof(struct pollfd *) * nfds);
	memset(fds, 0, sizeof(*fds));

	fds[0].fd = epurple->emacs_fd;
	fds[0].events = POLLIN;

	while (poll(fds, nfds, -1) != -1) {

		for (i = 0; i < nfds; i++) {

			if (fds[i].revents & (POLLERR | POLLHUP | POLLNVAL)) {
				printf("Poll Failed\n");
				goto out;
			}

			if (fds[i].revents & POLLIN) {
				if ((len = read(fds[i].fd, buf, MAX_BUF_SIZE)) != 1) {
					handler(epurple, fds[i].fd, buf, len);
					memset(buf, '\0', sizeof(buf));
				} else {
					perror("Failed to read");
					goto out;
				}
			}
		}
	}

out:
	printf("Exit main loop\n");
	free(fds);
}

static int wait_connection(struct epurple *epurple)
{
	struct sockaddr_un addr;
	int sock_fd, emacs_fd;

	if ((sock_fd = socket(PF_UNIX, SOCK_STREAM, 0)) < 0) {
		perror("Failed to create socket");
		return -1;
	}
	epurple->sock_fd = sock_fd;

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
	epurple->emacs_fd = emacs_fd;
	printf("Got connexion\n");

	return 0;
}

static guint input_add(int fd, PurpleInputCondition cond, PurpleInputFunction func,
		       gpointer data)
{
	printf("Input add\n");
	return 0;
}

static PurpleEventLoopUiOps eventloop_ops =
{
	g_timeout_add,
	g_source_remove,
	input_add,
	g_source_remove,
	NULL,
	g_timeout_add_seconds,
	NULL,
	NULL,
	NULL
};

static int purple_init(struct epurple *epurple)
{
	purple_eventloop_set_ui_ops(&eventloop_ops);

	if (purple_core_init("epurple") == FALSE) {
		perror("Failed to init purple core");
		return -1;
	}

	return 0;
}

int main(void)
{
	struct epurple epurple;
	int status = EXIT_SUCCESS;

	if (wait_connection(&epurple)) {
		status = EXIT_FAILURE;
		goto out;
	}

	if (purple_init(&epurple)) {
		status = EXIT_FAILURE;
		goto out;
	}

	loop(&epurple);
out:
	cleanup(&epurple);
	return status;
}
