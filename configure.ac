# Process this file with autoconf to produce a configure script

##################
# Package info
AC_PREREQ(2.59)
AC_INIT([gucu],[0.3],[spk121@yahoo.com])
AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([gnu std-options -Wall])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])

##################
# Set some version constants
LIBGUILE_GUCU_CURSES_MAJOR=0
LIBGUILE_GUCU_CURSES_INTERFACE="1:0:0"
AC_SUBST(LIBGUILE_GUCU_CURSES_MAJOR)
AC_SUBST(LIBGUILE_GUCU_CURSES_INTERFACE)

##################
# Check for Programs

AC_PROG_CC

AC_CANONICAL_HOST
AC_PROG_INSTALL

# AC_PROG_MKDIR_P is the current version of the macro
# AM_PROG_MKDIR_P is deprecated
AM_PROG_MKDIR_P

AM_PROG_CC_C_O
#NCURSESW_PROGS

##################
# Check for Libtool

# New method for when Libtool 2.x is default
#LT_INIT([dlopen disable-fast-install win32-dll])

# Deprecated method for Libtool 1.x
AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL

##################
# Initial guess of platform specific options
case $host in
*-*-cygwin)
	[enable_acs_map_fix=yes] ;;
*-*-linux*)
	[enable_acs_map_fix=no] ;;
*)
	[enable_acs_map_fix=no] ;;
esac


##################
# Check for libraries

AC_CHECK_LIB([guile],	
             [scm_version], ,
             # if guile is not ok
             [AC_MSG_ERROR([libguile not found])])


AC_ARG_WITH([ncursesw],
            [AS_HELP_STRING([--with-ncursesw],
                            [use the wide version of the curses libraries])],
            # if option is selected
            [libncurses="ncursesw"
	     libform="formw"
	     libpanel="panelw"
	     libmenu="menuw"
             with_ncursesw="yes"],
            # if option is not selected
            [libncurses="ncurses"
	     libform="form"
	     libpanel="panel"
	     libmenu="menu"
             with_ncursesw="no"])
	     
# A conditional to use in the makefiles
AM_CONDITIONAL([WIDE], [test "$with_ncursesw" = yes])

AS_IF(
      # if
      [test "$with_ncursesw" = yes],
      # then
      [AC_DEFINE([HAVE_LIBNCURSESW],
                 [1],
                 [Define if you are using the wide version of the ncurses library])])
# The menu, panel, and form libraries should either use the wide or
# the narrow forms to match the version of the ncurses library being
# used.
AC_CHECK_LIB([$libncurses], [initscr], ,[AC_MSG_ERROR([ncurses not found])])
AC_CHECK_LIB([$libmenu], [new_menu], , ,[-l$libncurses])
AC_CHECK_LIB([$libpanel], [new_panel], , ,[-l$libncurses])
AC_CHECK_LIB([$libform], [new_form], , ,[-l$libncurses])
AC_CHECK_LIB([unistring], [u32_strconv_from_locale])

#
# In Cygwin NCurses v5.5.20061104, there is a bug in the acs_map table
# declaration in <curses.h>.  This enables a workaround.
#
AC_ARG_ENABLE([acs-map-fix],
              [AS_HELP_STRING([--enable-acs-map-fix],
                              [Work around acs_map bug in Cygwin NCurses v 5.5.20061104])])
AS_IF(
      # if 
      [test "$enable_acs_map_fix" = yes],
      # then
      [AC_DEFINE([ENABLE_ACS_MAP_FIX], 
                 [1],
                 [Define if you have to explicitly declare the curses acs_map])])

AC_ARG_ENABLE([warnings],
              [AS_HELP_STRING([--enable-warnings],
                              [Build with most GCC warnings enabled])])
AM_CONDITIONAL([WARN], [test "$enable_warnings" = yes])

AC_ARG_ENABLE([gucukey],
              [AS_HELP_STRING([--enable-gucukey],
                              [Install the gucukey script])])
AM_CONDITIONAL([GUCUKEY], [test "$enable_gucukey" = yes])


##################
# Check for header files
AC_CHECK_HEADERS([wchar.h curses.h menu.h form.h panel.h process.h pty.h])
AC_CHECK_HEADERS([util.h])

##################
# Check for types, structures, compiler characteristics
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([chtype], ,[#include <curses.h>])
AC_CHECK_SIZEOF([attr_t], ,[#include <curses.h>])
AC_CHECK_SIZEOF([wchar_t], ,[#include <stddef.h>])
AC_CHECK_SIZEOF([wint_t], ,[#include <wchar.h>])

# AC_PROG_CC_C99

##################
# Check for C library functions
AC_CHECK_FUNCS([cfmakeraw fopencookie])

# Check for ncurses library functions
AC_CHECK_FUNCS([key_defined term_attrs])

# Later something should be done with this
# AC_FUNC_MALLOC

##################
# Check for system services

##################
#  Output
AC_CONFIG_FILES([Makefile])

AC_OUTPUT
