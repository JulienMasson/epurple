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

struct pollfd fds[MAX_POLL_FD];
struct epurple epurple;

static void cleanup()
{
	unlink(SERVER_SOCK_FILE);

	if (epurple.sock_fd >= 0)
		close(epurple.sock_fd);

	if (epurple.emacs_fd >= 0)
		close(epurple.emacs_fd);

	for (int i = 0; i < MAX_POLL_FD; i++) {
		if (fds[i].fd != -1)
			close(fds[i].fd);
	}
}

static void events_loop()
{
	struct epurple_event event;

	while (poll(fds, MAX_POLL_FD, -1) != -1) {

		for (int i = 0; i < MAX_POLL_FD; i++) {

			if (fds[i].fd == -1)
				continue;

			if (fds[i].revents & (POLLERR | POLLHUP | POLLNVAL)) {
				printf("Poll Failed\n");
				goto out;
			}

			if (fds[i].revents & fds[i].events) {
				event = epurple.events[i];
				if (event.handler)
					event.handler(&epurple, fds[i].fd, event.data);
			}
		}
	}

out:
	printf("Exit events loop\n");
}

static int wait_connection()
{
	struct sockaddr_un addr;
	int sock_fd, emacs_fd;

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
	epurple_add_event(emacs_fd, POLLIN, emacs_handler, NULL);
	printf("Got connexion\n");

	return 0;
}

static int epurple_event_found(int fd, short events)
{
	for (int i = 0; i < MAX_POLL_FD; i++) {
		if ((fds[i].fd == fd) && (fds[i].events == events))
			return 1;
	}
	return 0;
}

uint epurple_add_event(int fd, short events, epurple_hander handler, void *data)
{
	if (epurple_event_found(fd, events)) {
		printf("Event already added: %d - %d\n", fd, events);
		return MAX_POLL_FD;
	}

	for (uint i = 0; i < MAX_POLL_FD; i++) {
		if (fds[i].fd == -1) {
			epurple.events[i].handler = handler;
			epurple.events[i].data = data;
			fds[i].fd = fd;
			fds[i].events = events;
			return i;
		}
	}
	printf("MAX_POLL_FD reached\n");
	return MAX_POLL_FD;
}

void epurple_remove_event(uint handle)
{
	if ((handle > 0) || (handle < MAX_POLL_FD)) {
		/* close(fd); */
		fds[handle].fd = -1;
		fds[handle].events = 0;
		epurple.events[handle].handler = NULL;
		epurple.events[handle].data = NULL;
	}
}

int main(void)
{
	int status = EXIT_SUCCESS;

	for (int i = 0; i < MAX_POLL_FD; i++) {
		fds[i].fd = -1;
		fds[i].events = 0;
		epurple.events[i].handler = NULL;
		epurple.events[i].data = NULL;
	}

	if (wait_connection()) {
		status = EXIT_FAILURE;
		goto out;
	}

	events_loop();
out:
	cleanup();
	return status;
}
