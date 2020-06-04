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

#define MAX_BUF_SIZE 4096

struct header {
	char command[COMMAND_NAME_SIZE];
	int id;
};

void emacs_handler(struct epurple *epurple, int fd, void *data)
{
	char buf[MAX_BUF_SIZE];
	size_t buffer_len;

	memset(buf, '\0', sizeof(buf));
	if ((buffer_len = read(fd, buf, MAX_BUF_SIZE)) == -1) {
		perror("Failed to read");
		return;
	}

	struct header *header = (struct header *)buf;
	size_t header_len = sizeof(struct header);
	char *payload = buf + header_len;
	struct handler *handler;

	if ((handler = handlers_find(header->command)))
		handler->func(epurple, header->id, payload, buffer_len - header_len);
	else
		printf("Unknown command: %s\n", header->command);
}

void emacs_send(struct epurple *epurple, int id, char *payload, size_t len)
{
	size_t buf_len = sizeof(struct header) + len;
	void *buf = malloc(buf_len);
	struct header *header = (struct header *)buf;
	char *payload_buf = buf + sizeof(struct header);

	memset(buf, '\0', buf_len);
	header->id = id;
	if (payload)
		memcpy(payload_buf, payload, len);

	if (write(epurple->emacs_fd, buf, buf_len) == -1)
		perror("Failed to send to Emacs");

	free(buf);
}
