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

#include "log.h"

int log_level = LOG_INFO;

static const char *const level_names[] = {"ERROR", "WARN", "INFO", "DEBUG"};
static const char *const level_colors[] = {"\x1b[31m", "\x1b[33m", "\x1b[34m", "\x1b[32m"};

void log_print(int level, const char *fmt, ...)
{
	time_t t;
	struct tm *lt;
	va_list args;
	char date[16];

	if ((level > LOG_SILENT) && (level <= log_level)) {
		/* shift level to select right colors/name */
		level = level - 1;

		/* Get current date */
		t = time(NULL);
		lt = localtime(&t);
		date[strftime(date, sizeof(date), "%H:%M:%S", lt)] = '\0';

		/* print header: date level */
		printf("\x1b[36m%s\x1b[0m ", date);
		printf("%s%-5s\x1b[0m ", level_colors[level], level_names[level]);

		/* print args */
		va_start(args, fmt);
		vfprintf(stdout, fmt, args);
		va_end(args);
		printf("\n");
	}
}
