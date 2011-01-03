/*
  type.h

  Copyright 2009, 2010 Free Software Foundation, Inc.

  This file is part of GNU Guile-Ncurses.

  Guile-Ncurses is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  Guile-Ncurses is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with Guile-Ncurses.  If not, see
  <http://www.gnu.org/licenses/>.
*/

#ifndef TYPE_H
#define TYPE_H 1

#include <config.h>

#include <libguile.h>
#include <stddef.h>
#include <stdio.h>
#include <wchar.h>

#if HAVE_CURSES_H
#include <curses.h>
#endif

#if HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#endif

#include "visibility.h"

/*****************************************************************************/
/* CHARACTERS                                                                */

#define GUCU_REPLACEMENT_CHAR ('?')
#define GUCU_REPLACEMENT_CODEPOINT (0xFFFD)
#ifdef __STDC_ISO_10646__
#define GUCU_REPLACEMENT_WCHAR (0xFFFD)
#else
/* This value may be incorrect */
#define GUCU_REPLACEMENT_WCHAR (0xFFFD)
#endif

#define GUCU_PRIVATE_USE_START (0xE000)

GUCU_LOCAL int _scm_is_xchar (SCM x);

#ifdef HAVE_NCURSESW
GUCU_LOCAL cchar_t *_scm_xchar_to_cchar (SCM x);
#endif
GUCU_LOCAL char _scm_schar_to_char (SCM x);
GUCU_LOCAL chtype _scm_xchar_to_chtype (SCM x);
GUCU_LOCAL wchar_t _scm_schar_to_wchar (SCM x);

GUCU_LOCAL SCM _scm_schar_from_char (char c);
GUCU_LOCAL SCM _scm_schar_from_wchar (wchar_t ch);
#ifdef HAVE_NCURSESW
GUCU_LOCAL SCM _scm_xchar_from_cchar (cchar_t * x);
#endif
GUCU_LOCAL SCM _scm_xchar_from_chtype (chtype x);

GUCU_API SCM gucu_schar_from_char (SCM c);
GUCU_API SCM gucu_schar_from_wchar (SCM c);
GUCU_API SCM gucu_schar_to_char (SCM c);
GUCU_API SCM gucu_schar_to_wchar (SCM c);
GUCU_API SCM gucu_xchar_from_chtype (SCM c);
GUCU_API SCM gucu_xchar_to_chtype (SCM c);


/*****************************************************************************/
/* STRINGS                                                                   */

GUCU_LOCAL int _scm_is_xstring (SCM x);

#ifdef HAVE_NCURSESW
GUCU_LOCAL SCM _scm_sstring_from_wint_string (const wint_t * x);
GUCU_LOCAL SCM _scm_sstring_from_wstring (const wchar_t * x);
#endif
GUCU_LOCAL char *_scm_sstring_to_locale_string (SCM x);
GUCU_LOCAL wchar_t *_scm_sstring_to_wstring (SCM x);
GUCU_LOCAL SCM _scm_xstring_from_chstring (const chtype * x);
#ifdef HAVE_NCURSESW
GUCU_LOCAL SCM _scm_xstring_from_cstring (const cchar_t * x);
#endif
GUCU_LOCAL chtype *_scm_xstring_to_chstring (SCM x);
#ifdef HAVE_NCURSESW
GUCU_LOCAL cchar_t *_scm_xstring_to_cstring (SCM x);
#endif

/*****************************************************************************/

GUCU_LOCAL int _scm_is_attr (SCM x);
GUCU_LOCAL attr_t _scm_to_attr (SCM x);
GUCU_LOCAL SCM _scm_from_attr (attr_t x);

GUCU_LOCAL int _scm_is_chtype (SCM x);
GUCU_LOCAL chtype _scm_to_chtype (SCM x);
GUCU_LOCAL SCM _scm_from_chtype (chtype x);

GUCU_LOCAL int _scm_is_chstring (SCM x);
GUCU_LOCAL chtype *_scm_to_chstring (SCM x);
GUCU_LOCAL SCM _scm_from_chstring (chtype * x);

GUCU_LOCAL int _scm_is_file (SCM x);
GUCU_LOCAL FILE *_scm_to_file (SCM x);
GUCU_LOCAL SCM _scm_from_file (FILE * x);

GUCU_LOCAL int _scm_is_mevent (SCM x);
GUCU_LOCAL MEVENT *_scm_to_mevent (SCM x);
GUCU_LOCAL SCM _scm_from_mevent (MEVENT *);

GUCU_API SCM gucu_is_mevent_p (SCM x);

GUCU_LOCAL int _scm_is_screen (SCM x);
GUCU_LOCAL SCREEN *_scm_to_screen (SCM x);
GUCU_LOCAL SCM _scm_from_screen (SCREEN * x);

GUCU_API SCM gucu_is_screen_p (SCM x);

GUCU_LOCAL int _scm_is_window (SCM x);
GUCU_LOCAL WINDOW *_scm_to_window (SCM x);
GUCU_LOCAL SCM _scm_from_window (WINDOW * x);

GUCU_API SCM gucu_is_window_p (SCM x);

GUCU_LOCAL void gucu_init_type (void);

#endif /* not TYPE_H */
