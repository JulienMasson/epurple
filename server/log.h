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

#ifndef LOG_H
#define LOG_H

#include <stdio.h>
#include <stdarg.h>
#include <time.h>

enum { LOG_SILENT, LOG_ERROR, LOG_WARN, LOG_INFO, LOG_DEBUG };

#define LOGE(...) log_print(LOG_ERROR, __VA_ARGS__)
#define LOGW(...) log_print(LOG_WARN,  __VA_ARGS__)
#define LOGI(...) log_print(LOG_INFO, __VA_ARGS__)
#define LOGD(...) log_print(LOG_DEBUG, __VA_ARGS__)

void log_print(int level, const char *fmt, ...);

extern int log_level;

#endif
