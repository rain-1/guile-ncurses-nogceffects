# Process this file with autoconf to produce a configure script

# Copyright (C) 2009, 2010 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.


##################
# Package info
AC_INIT([GNU Guile-Ncurses],[0.9],[spk121@yahoo.com])

AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([gnu std-options -Wall])

AC_PREREQ(2.59)
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_MACRO_DIR([m4])

##################
# Set the library libtool version constant
LIBGUILE_NCURSES_INTERFACE="6:1:0"
AC_SUBST(LIBGUILE_NCURSES_INTERFACE)

##################
# Check for Programs

AC_PROG_CC
AC_CANONICAL_HOST
AC_PROG_INSTALL
AC_PROG_MKDIR_P

##################
# Check for Libtool

# New method for when Libtool 2.x is default
LT_INIT([dlopen disable-fast-install disable-static win32-dll])

##################
# Initial guess of platform specific options

case $host_os in
mingw* | cygwin* | os2* | pw32* | cegcc*)
	[building_dll=yes] ;;
*)
	[building_dll=no] ;;
esac

AM_CONDITIONAL([BUILDING_DLL], [test "$building_dll" = yes])

##################
# Check for libraries

AM_GNU_GETTEXT_VERSION([0.18.1])
AM_GNU_GETTEXT([external])

# from guile.m4
# sets: GUILE_CFLAGS and GUILE_LDFLAGS
GUILE_FLAGS

# GUILE_LDFLAGS is actually a mashup of GUILE_LIBS and GUILE_LDFLAGS

AC_MSG_CHECKING([if guile can handle UCS4 characters])
GUILE_CHECK([ac_cv_guile_ucs4],
            [(= 32764 (false-if-exception (char->integer (integer->char 32764))))])
if test "$ac_cv_guile_ucs4" = 0
then
  ac_cv_guile_ucs4=yes
else
  ac_cv_guile_ucs4=no
fi
AC_MSG_RESULT([$ac_cv_guile_ucs4])
AM_CONDITIONAL([UCS4],[test "$ac_cv_guile_ucs4" = yes])

# from the autoconf archive
# sets: ax_cv_curses, ax_cv_ncurses, ax_cv_ncursesw and CURSES_LIB
# ac_defines: HAVE_CURSES_H, HAVE_NCURSES_H, HAVE_NCURSES_W
AX_WITH_CURSES

if test "$ax_cv_ncurses" = no && "$ax_cv_ncursesw" = no
then
  AC_MSG_ERROR([ncurses is required])
fi

AM_CONDITIONAL([WIDE], [test "$ax_cv_ncursesw" = yes])


# Check for the menu, panel, and form libraries
if test "$ax_cv_ncursesw" != "yes"
then
  AC_CHECK_LIB([menu], [new_menu],
               [MENU_LIB="-lmenu"],
               [AC_MSG_ERROR([the ncurses menu library is required])])
  AC_CHECK_LIB([form], [new_form],
               [FORM_LIB="-lform"],
               [AC_MSG_ERROR([the ncurses form library is required])])
  AC_CHECK_LIB([panel], [new_panel],
               [PANEL_LIB="-lpanel"],
               [AC_MSG_ERROR([the ncurses panel library is required])])
else
  AC_CHECK_LIB([menuw], [new_menu],
               [MENU_LIB="-lmenuw"],
               [AC_MSG_ERROR([the ncurses menuw library is required])])
  AC_CHECK_LIB([formw], [new_form],
               [FORM_LIB="-lformw"],
               [AC_MSG_ERROR([the ncurses formw library is required])])
  AC_CHECK_LIB([panelw], [new_panel],
               [PANEL_LIB="-lpanelw"],
               [AC_MSG_ERROR([the ncurses panelw library is required])])
  AC_CHECK_LIB([unistring], [u32_strconv_from_locale],
               [UNISTRING_LIB="-lunistring"],
               [AC_MSG_ERROR([the unistring library is required when wide curses is used])])
fi

AC_SUBST([CURSES_LIB])
AC_SUBST([MENU_LIB])
AC_SUBST([PANEL_LIB])
AC_SUBST([FORM_LIB])
AC_SUBST([UNISTRING_LIB])

AC_ARG_ENABLE([warnings],
              [AS_HELP_STRING([--enable-warnings],
                              [Build with most GCC warnings enabled])])
AM_CONDITIONAL([WARN], [test "$enable_warnings" = yes])

AC_ARG_ENABLE([demo-install],
              [AS_HELP_STRING([--enable-demo-install],
                              [Install the demonstration programs])])
AM_CONDITIONAL([DEMO_INSTALL], [test "$enable_demo_install" = yes])


##################
# Check for header files
AC_CHECK_HEADERS([process.h pty.h util.h])

##################
# Check for types, structures, compiler characteristics
AX_C___ATTRIBUTE__
AC_CHECK_TYPE([off64_t],
              [AC_DEFINE([HAVE_OFF64_T],
                         [1],
                         [Define if you are have the off64_t type])],
              ,
              [#define _LARGEFILE64_SOURCE
               #include <unistd.h>
               ])
AC_CHECK_TYPE([cookie_io_functions_t],
              [AC_DEFINE([HAVE_COOKIE_IO_FUNCTIONS_T],
                         [1],
                         [Define if you are have the cookie_io_functions_t type])],
              ,
              [#define _GNU_SOURCE
               #include <stdio.h>
               ])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([chtype], ,[#include <curses.h>])
AC_CHECK_SIZEOF([attr_t], ,[#include <curses.h>])
AC_CHECK_SIZEOF([wchar_t], ,[#include <stddef.h>])
AC_CHECK_SIZEOF([wint_t], ,[#include <wchar.h>])

##################
# Check for C library functions
AC_CHECK_FUNCS([cfmakeraw fopencookie ptsname grantpt unlockpt])

# Check for ncurses library functions
ax_save_LIBS="$LIBS"
LIBS="$ax_save_LIBS $CURSES_LIB"
AC_CHECK_FUNCS([key_defined term_attrs set_tabsize])
LIBS="$ax_save_LIBS"

# Later something should be done with this
# AC_FUNC_MALLOC

##################
# Check for system services

##################
#  Output
AC_CONFIG_FILES([Makefile ncurses/Makefile doc/Makefile m4/Makefile po/Makefile.in])
AC_CONFIG_FILES([build_aux/Makefile tools/Makefile test/Makefile])
AC_CONFIG_FILES([tools/uninstalled-env], [chmod +x tools/uninstalled-env])
AC_OUTPUT
