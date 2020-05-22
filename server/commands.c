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

#define ARRAY_SIZE(x)     (sizeof(x) / sizeof(x[0]))
#define COMMAND_NAME_SIZE 80

struct header {
	char command[COMMAND_NAME_SIZE];
	int id;
};

struct handler {
	char command[COMMAND_NAME_SIZE];
	void (*func)(struct epurple *, int, char *, size_t);
};

static struct handler handlers[] = {
	{"ping" , ping_handler}
};

static void command_send(struct epurple *epurple, int id, char *data, size_t len)
{
	size_t buf_len = sizeof(struct header) + len;
	void *buf = malloc(buf_len);
	struct header *header = (struct header *)buf;
	char *data_buf = buf + sizeof(struct header);

	memset(buf, '\0', buf_len);
	header->id = id;
	memcpy(data_buf, data, len);

	epurple_send(epurple, buf, buf_len);

	free(buf);
}

static struct handler *find_handler(char *command)
{
	for (int i = 0; i < ARRAY_SIZE(handlers); i++) {
		if (!strcmp(command, handlers[i].command))
			return &handlers[i];
	}
	return NULL;
}

void commands_handler(struct epurple *epurple, char *buf, size_t len)
{
	struct header *header = (struct header *)buf;
	size_t header_len = sizeof(struct header);
	char *data = buf + header_len;
	struct handler *handler;

	if ((handler = find_handler(header->command)))
		handler->func(epurple, header->id, data, len - header_len);
	else
		printf("Unknown command: %s\n", header->command);
}

/* ping */
struct ping {
	unsigned int value;
	char name[256];
	unsigned int another_value;
};

void ping_handler(struct epurple *epurple, int id, char *data, size_t len)
{
	struct ping test;

	printf("Ping Handler\n");
	test.value = 6;
	strcpy(test.name, "Hellooooo");
	test.another_value = 12;

	command_send(epurple, id, (char *)&test, sizeof(test));
}
