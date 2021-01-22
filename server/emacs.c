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

struct header {
	char command[STR_NAME_SIZE];
	int id;
	int payload_size;
};

gboolean emacs_handler(GIOChannel *in, GIOCondition cond, gpointer data)
{
	int fd = g_io_channel_unix_get_fd(in);
	struct epurple *epurple = (struct epurple *)data;
	gboolean status = FALSE;

	/* read header */
	size_t header_size = sizeof(struct header);
	char *header_buf = malloc(header_size);
	memset(header_buf, '\0', header_size);

	if (read(fd, header_buf, header_size) == -1) {
		perror("Failed to read header");
		goto header_failed;
	}
	struct header *header = (struct header *)header_buf;

	/* read payload */
	char *payload_buf = malloc(header->payload_size);
	memset(payload_buf, '\0', header->payload_size);

	if (read(fd, payload_buf, header->payload_size) == -1) {
		perror("Failed to read payload");
		goto payload_failed;
	}

	/* pass payload to handler */
	struct handler *handler;
	if ((handler = handlers_find(header->command)))
		handler->func(epurple, header->id, payload_buf, header->payload_size);
	else
		printf("Unknown command: %s\n", header->command);
	status = TRUE;

payload_failed:
	free(payload_buf);
header_failed:
	free(header_buf);

	return status;
}

void emacs_send(struct epurple *epurple, char *command, int id, char *payload, size_t len)
{
	size_t buf_len = sizeof(struct header) + len;
	void *buf = malloc(buf_len);
	struct header *header = (struct header *)buf;
	char *payload_buf = buf + sizeof(struct header);

	memset(buf, '\0', buf_len);
	if (command)
		strncpy(header->command, command, STR_NAME_SIZE);
	header->id = id;
	header->payload_size = len;
	if (payload)
		memcpy(payload_buf, payload, len);

	if (write(epurple->emacs_fd, buf, buf_len) == -1)
		perror("Failed to send to Emacs");

	free(buf);
}

void emacs_ack(struct epurple *epurple, int id)
{
	emacs_send(epurple, NULL, id, NULL, 0);
}
