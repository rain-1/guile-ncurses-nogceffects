# Makefile for the toolsdirectory of GNU Guile-Ncurses.
#
# Copyright 2009, 2010 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

if DEMO_INSTALL
bin_SCRIPTS = ncurses-keyboard-test
else
noinst_SCRIPTS = ncurses-keyboard-test
endif

bin_PROGRAMS = guile-ncurses-shell
guile_ncurses_shell_SOURCES = shell.c

AM_CPPFLAGS = $(GUILE_CFLAGS) -I $(top_builddir)/lib \
	-DLOCALEDIR=\"$(localedir)\"

if WARN
AM_CPPFLAGS +=  -Wall -Wextra -Wstrict-prototypes			\
-Wmissing-prototypes -Wpointer-arith -Wreturn-type -Wcast-qual		\
-Wswitch -Wshadow -Wcast-align -Wwrite-strings -Wchar-subscripts	\
-Winline -Wnested-externs -Wredundant-decls -Wunused -Wformat=2
endif

if WIDE
AM_CPPFLAGS += -D_XOPEN_SOURCE_EXTENDED
endif

LDADD = $(NCURSES_LIB) -lutil $(GUILE_LDFLAGS)

EXTRA_DIST = uninstalled-env.in ncurses-keyboard-test
ETAGS_ARGS = shell.[ch] --language=scheme ncurses-keyboard-test
