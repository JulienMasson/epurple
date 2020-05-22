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

#include "commands.h"
#include "epurple.h"

#define SERVER_SOCK_FILE ".epurple.sock"
#define MAX_BUF_SIZE     4096

static void cleanup(struct epurple *epurple)
{
	unlink(SERVER_SOCK_FILE);

	if (epurple->fd >= 0)
		close(epurple->fd);

	if (epurple->sockfd >= 0)
		close(epurple->sockfd);
}

static void handler(struct epurple *epurple, int fd, char *buf, size_t len)
{
	if (epurple->fd == fd)
		commands_handler(epurple, buf, len);
}

static void loop(struct epurple *epurple)
{
	struct pollfd *fds;
	char buf[MAX_BUF_SIZE];
	ssize_t len;
	int i, nfds = 1;

	fds = malloc(sizeof(struct pollfd *) * nfds);
	memset(fds, 0, sizeof(*fds));

	fds[0].fd = epurple->fd;
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
	int fd, sockfd;

	if ((sockfd = socket(PF_UNIX, SOCK_STREAM, 0)) < 0) {
		perror("Failed to create socket");
		return -1;
	}
	epurple->sockfd = sockfd;

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strcpy(addr.sun_path, SERVER_SOCK_FILE);
	unlink(SERVER_SOCK_FILE);
	if (bind(sockfd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
		perror("Failed to bind the socket");
		return -1;
	}

	if (listen(sockfd, 5) < 0) {
		perror("Failed to mark the socket as passive");
		return -1;
	}

	printf("Waiting Emacs\n");
	if ((fd = accept(sockfd, NULL, NULL)) < 0) {
		perror("Failed to accept connection");
		return -1;
	}
	epurple->fd = fd;
	printf("Got connexion\n");

	return 0;
}

void epurple_send(struct epurple *epurple, void *buf, size_t len)
{
	if (write(epurple->fd, buf, len) == -1)
		perror("Failed to send");
}

int main(void)
{
	struct epurple epurple;
	int status = EXIT_SUCCESS;

	if (wait_connection(&epurple)) {
		status = EXIT_FAILURE;
		goto out;
	}

	loop(&epurple);
out:
	cleanup(&epurple);
	return status;
}
