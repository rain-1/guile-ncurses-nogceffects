# Process this file with autoconf to produce a configure script

# Copyright (C) 2009, 2010, 2011, 2013, 2014 Free Software Foundation, Inc.
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
AC_INIT([GNU Guile-Ncurses],[1.5],[bug-guile-ncurses@gnu.org])

AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([gnu std-options -Wall])

AC_PREREQ(2.60)
AC_USE_SYSTEM_EXTENSIONS([_GNU_SOURCE])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_MACRO_DIR([m4])

##################
# Set the library libtool version constant
LIBGUILE_NCURSES_INTERFACE="8:1:0"
AC_SUBST(LIBGUILE_NCURSES_INTERFACE)

##################
# Check for Programs

AC_PROG_CC
AC_CANONICAL_HOST
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AM_PROG_AR

AC_ARG_WITH([guilesitedir],
            [AS_HELP_STRING([--with-guilesitedir],
                            [use the specified installation path for Guile modules])],
            [case "x$withval" in
             xyes|xno) guilesitedir="";;
             *)        guilesitedir="$withval";;
             esac],
             [guilesitedir=""])


# The curses man page lists the following possibilities for curses
# headers

AC_MSG_NOTICE([checking for ncurses])

AC_CHECK_HEADER([curses.h],
                [AC_DEFINE([HAVE_CURSES_H],
                           [1],
                           [Define if you have curses.h])
                 have_curses_h=yes],
                [have_curses_h=no])

if test "$have_curses_h" = no
then
  AC_CHECK_HEADER([ncurses/curses.h],
                  [AC_DEFINE([HAVE_NCURSES_CURSES_H],
                             [1],
                             [Define if you have ncurses/curses.h])
                 have_ncurses_curses_h=yes],
                [have_ncurses_curses_h=no])
fi

if test "$have_curses_h" = no && test "$have_ncurses_curses_h" = no
then
  AC_CHECK_HEADER([ncursesw/curses.h],
                  [AC_DEFINE([HAVE_NCURSESW_CURSES_H],
                             [1],
                             [Define if you have ncursesw/curses.h])],
                  [AC_MSG_FAILURE([curses.h could not be found])])
fi

AC_ARG_WITH(ncursesw,
            [AS_HELP_STRING([--with-ncursesw],
                            [Use ncursesw (wide character support)])],
            [with_ncursesw=check],
            [with_ncursesw=no])

AS_IF([test "$with_ncursesw" = check],
      [AC_SEARCH_LIBS([initscr], [ncursesw],
                      [AC_DEFINE([HAVE_NCURSESW],
                                 [1],
                                 [Define if you have libncursesw])
                       with_ncursesw=yes],
                      [AC_MSG_FAILURE([--with-ncursesw was given, but, test for ncursesw failed])])])

AS_IF([test "$with_ncursesw" = no],
      [AC_SEARCH_LIBS([initscr], [ncurses],
                      [AC_DEFINE([HAVE_NCURSES],
                                 [1],
                                 [Define if you have libncurses])],
                      [AC_MSG_FAILURE([the ncurses library was not found])])])

AM_CONDITIONAL([WIDE], [test "$with_ncursesw" = yes])


# Check for the menu, panel, and form libraries
if test "$with_ncursesw" = no
then
  AC_SEARCH_LIBS([new_menu], [menu],
                 [],
                 [AC_MSG_ERROR([the ncurses menu library is required])])
  AC_SEARCH_LIBS([new_form], [form],
                 [],
                 [AC_MSG_ERROR([the ncurses form library is required])])
  AC_SEARCH_LIBS([new_panel], [panel],
                 [],
                 [AC_MSG_ERROR([the ncurses panel library is required])])
else
  AC_SEARCH_LIBS([u32_strconv_from_locale], [unistring],
                 [],
                 [AC_MSG_ERROR([the unistring library is required when wide curses is used])])
  AC_SEARCH_LIBS([new_menu], [menuw],
                 [],
                 [AC_MSG_ERROR([the ncurses menuw library is required])])
  AC_SEARCH_LIBS([new_form], [formw],
                 [],
                 [AC_MSG_ERROR([the ncurses formw library is required])])
  AC_SEARCH_LIBS([new_panel], [panelw],
                 [],
                 [AC_MSG_ERROR([the ncurses panelw library is required])])
fi

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
case $host_os in
mingw* | os2* | pw32* | cegcc*)
	[building_shell=no] ;;
*)
	[building_shell=yes] ;;
esac
case $host_os in
mingw* )
	[dll_version_info=no] ;;
*)
	[dll_version_info=yes] ;;
esac

AM_CONDITIONAL([BUILDING_DLL], [test "$building_dll" = yes])
AM_CONDITIONAL([BUILDING_SHELL], [test "$building_shell" = yes])
AM_CONDITIONAL([DLL_VERSION_INFO], [test "$dll_version_info" = yes])
##################
# Check for libraries

# from guile.m4
# sets: GUILE_CFLAGS and GUILE_LDFLAGS
GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

# Some versions of Cygwin have a bad guile-config
AC_MSG_CHECKING([if Guile flags have the janneke's path problem])
ac_cv_janneke_test="`echo $GUILE_CFLAGS | grep janneke`"
ac_cv_janneke_fix=no
if test -n "$ac_cv_janneke_test"
then
  GUILE_CFLAGS=/usr/include
  GUILE_LDFLAGS="-lguile -L/usr/lib -L/usr/bin -L/usr/lib/w32api -lgmp -lcrypt -lm -lltdl"
  ac_cv_janneke_fix=yes
fi
AC_MSG_RESULT([$ac_cv_janneke_fix])

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
              [#include <stdio.h>
               ])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([void *])
if test "$have_curses_h" = yes
then
  AC_CHECK_SIZEOF([chtype], ,[#include <curses.h>])
  AC_CHECK_SIZEOF([attr_t], ,[#include <curses.h>])
else
if test "$have_ncurses_curses_h" = yes
then
  AC_CHECK_SIZEOF([chtype], ,[#include <ncurses/curses.h>])
  AC_CHECK_SIZEOF([attr_t], ,[#include <ncurses/curses.h>])
else
  AC_CHECK_SIZEOF([chtype], ,[#include <ncursesw/curses.h>])
  AC_CHECK_SIZEOF([attr_t], ,[#include <ncursesw/curses.h>])
fi
fi
AC_CHECK_SIZEOF([wchar_t], ,[#include <stddef.h>])
AC_CHECK_SIZEOF([wint_t], ,[#include <wchar.h>])

##################
# Check for C library functions
AC_CHECK_FUNCS([cfmakeraw fopencookie ptsname grantpt unlockpt tcgetsid cfsetspeed wcwidth])

# Check for ncurses library functions
AC_CHECK_FUNCS([key_defined term_attrs set_tabsize is_pad is_subwin form_driver_w])

# Later something should be done with this
# AC_FUNC_MALLOC

AC_ARG_ENABLE([termios],
              [AS_HELP_STRING([--enable-termios],
                              [enable experimental support for termios functions])],
            [],
            [enable_termios=no])

AS_IF([test "x$enable_termios" != xno],
      [AC_DEFINE([ENABLE_TERMIOS], [1],
                 [Define to enable TERMIOS support])
       # Check for the XOpen and BSD termios constants.
       # Hopefully just one will suffice for the entire set.
       AC_CHECK_DECLS([BS0, ECHOCTL], [], [],
       [[
       #include <termios.h>
       ]])

       # Check for rarer termios constants
       AC_CHECK_DECLS([XCASE, ECHOPRT, PENDIN], [], [],
       [[
       #include <termios.h>
       ]])])

##################
# Check for system services

AC_CHECK_PROG([TOE],[toe],[yes],[no])
AS_IF([test "$TOE" = yes],
      [AC_MSG_CHECKING([for the terminfo of xterm])
       havexterm=no
       if toe -a | grep -q xterm
       then havexterm=yes
       fi
       AC_MSG_RESULT([$havexterm])])
AM_CONDITIONAL([HAVE_XTERM_TERMINFO],[test "$havexterm" = yes])

##################
#  Output

pkgdatadir="$datadir/$PACKAGE"
if test "x$guilesitedir" = "x"; then
   guilesitedir="$pkgdatadir"
fi
AC_SUBST([guilesitedir])

AC_CONFIG_FILES([Makefile ncurses/Makefile doc/Makefile m4/Makefile])
AC_CONFIG_FILES([build_aux/Makefile tools/Makefile])
AC_CONFIG_FILES([test/Makefile])
AC_CONFIG_FILES([tools/uninstalled-env], [chmod +x tools/uninstalled-env])
AC_OUTPUT

if test "$guilesitedir" != "$GUILE_SITE"; then
   # Guile has a different prefix than this module
   AC_MSG_WARN([]
   [The Guile modules will be installed in ${guilesitedir}.]
   [You should probably re-run `configure' with]
   [`--with-guilesitedir=$GUILE_SITE']
   [Otherwise, you will have to adjust the `GUILE_LOAD_PATH' environment]
   [variable.])
fi
