/*
curs_spec.c

Copyright 2009, 2010 Free Software Foundation, Inc.

This file is part of GNU Guile-Ncurses.

Guile-Ncurses is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

Guile-Ncurses is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with Guile-Ncurses.  If not, see
<http://www.gnu.org/licenses/>.
*/

#include <config.h>

#define _XOPEN_SOURCE
#include <libguile.h>
#include <libintl.h>
#include <stdlib.h>
#include <termios.h>

#if HAVE_CURSES_H
#include <curses.h>
#endif

#if HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#endif

/* Work around unistring bug */
#ifndef _UNUSED_PARAMETER_
#define _UNUSED_PARAMETER_
#endif

#ifdef GUILE_CHARS_ARE_UCS4
#include <unistr.h>
#endif

#include "compat.h"
#include "curs_spec.h"
#include "type.h"

#define MAXLEN 1000

#ifdef CYGWIN_CURSES_BUG_FIX
  /* work around bug in Cygwin's ancient NCurses */
extern
NCURSES_EXPORT_VAR (chtype *)
_nc_acs_map (void);
#define acs_map (_nc_acs_map())
#endif

static void curs_bad_state_error (const char *funcname)
{
  scm_misc_error (funcname, "bad curses internal state", SCM_BOOL_F);
}

/* Set the attributes and color pair for a given window */
SCM
gucu_wattr_set_x (SCM win, SCM attrs, SCM pair)
{
  WINDOW *c_win;
  attr_t c_attrs;
  short c_pair;
  int ret;

  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "wattr-set!");
  SCM_ASSERT (_scm_is_attr (attrs), attrs, SCM_ARG2, "wattr-set!");
  SCM_ASSERT (scm_is_integer (pair), pair, SCM_ARG3, "wattr-set!");

  c_win = _scm_to_window (win);
  c_attrs = _scm_to_attr (attrs);
  c_pair = scm_to_short (pair);

  /* wattr_set always returns OK */
  ret = wattr_set (c_win, c_attrs, c_pair, NULL);
  if (ret != OK)
    abort ();

  return SCM_UNSPECIFIED;
}

/* Return a list of three elements containing the RGB of COLOR */
SCM
gucu_color_content (SCM s_color)
{
  int ret;
  short c_red, c_green, c_blue;

  ret = color_content (scm_to_short (s_color), &c_red, &c_green, &c_blue);
  if (ret == OK)
    {
      return scm_list_3 (scm_from_short (c_red),
			 scm_from_short (c_green), scm_from_short (c_blue));
    }
  else
    return SCM_BOOL_F;
}

/* Free the C memory of a window */
SCM
gucu_delwin (SCM win)
{
  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "delwin");

  WINDOW *c_win = _scm_to_window (win);

  SCM_SET_SMOB_DATA (win, NULL);

  if (c_win != NULL)
    {
      int ret = delwin (c_win);
      if (ret == ERR)
	return SCM_BOOL_F;
      else
	return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}


SCM
gucu_getmouse ()
{
  MEVENT me;
  SCM s_me;

  int ret = getmouse (&me);

  if (ret == OK)
    {
      s_me = _scm_from_mevent (&me);
    }
  else
    {
      s_me = SCM_BOOL_F;
    }

  return s_me;
}

#ifdef HAVE_GRANTPT
/* If FD is the file descriptor of a master pseudo-terminal, this
   changes the mode and permissions of the slave pseudo-terminal
   so that it can be used.  */
SCM
gucu_grantpt (SCM fd)
{
  int c_fd;
  int ret;

  SCM_ASSERT (scm_is_integer (fd), fd, SCM_ARG1, "grantpt");
  c_fd = scm_to_int (fd);
  ret = grantpt (c_fd);
  if (ret == -1)
    scm_syserror ("grantpt");

  return SCM_UNSPECIFIED;
}
#endif

/* Write data to terminal-attached printer */
SCM
gucu_mcprint (SCM data)
{
  size_t len;
  char *str = scm_to_locale_stringn (data, &len);
  int ret;
  ret = mcprint (str, len);
  if (ret == ERR)
    return SCM_BOOL_F;
  
  return scm_from_int (ret);
}

SCM
gucu_mousemask (SCM x)
{
  return scm_from_ulong (mousemask (scm_to_ulong (x), NULL));
}

/* Return the foreground and background color numbers of a given PAIR */
SCM
gucu_pair_content (SCM s_pair)
{
  int ret;
  short c_fore, c_back;

  ret = pair_content (scm_to_short (s_pair), &c_fore, &c_back);
  if (ret == OK)
    {
      return scm_list_2 (scm_from_short (c_fore), scm_from_short (c_back));
    }
  else
    return SCM_BOOL_F;
}

#ifdef HAVE_PTSNAME
/* If FD, a file descriptor, is a master pseudo-terminal device, this
   returns a string that contains the name of the slave
   pseudo-terminal device.  */
SCM
gucu_ptsname (SCM fd)
{
  int c_fd;
  char *name;

  SCM_ASSERT (scm_is_integer (fd), fd, SCM_ARG1, "ptsname");

  c_fd = scm_to_int (fd);
  name = ptsname (c_fd);
  if (name == NULL)
    return SCM_BOOL_F;

  return scm_from_locale_string (name);
}
#endif

/* IF FD is a file descriptor of a pseudo-terminal device,
   this sets that pseudoterminal to RAW mode. */
SCM
gucu_ptsmakeraw (SCM fd)
{
  int c_fd;
  int ret;
  struct termios terminal_attributes;

  SCM_ASSERT (scm_is_integer (fd), fd, SCM_ARG1, "ptsmakeraw");

  c_fd = scm_to_int (fd);
  ret = tcgetattr (c_fd, &terminal_attributes);
  if (ret == -1)
    scm_syserror ("ptsmakeraw");
  terminal_attributes.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP
				   | INLCR | IGNCR | ICRNL | IXON);
  terminal_attributes.c_oflag &= ~OPOST;
  terminal_attributes.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
  terminal_attributes.c_cflag &= ~(CSIZE | PARENB);
  terminal_attributes.c_cflag |= CS8;
  ret = tcsetattr (c_fd, TCSANOW, &terminal_attributes);
  if (ret == -1)
    scm_syserror ("ptsmakeraw");
  return SCM_UNDEFINED;
}

#if 0
SCM
gucu_tget (SCM id)
{
  SCM_ASSERT (scm_is_string (id), id, SCM_ARG1, "tget");

  int ret;
  char *c_str;

  c_str = scm_to_locale_string (id);
  ret = tgetnum (id);
  if (id != -1)
    return scm_from_int (ret);

  ret = tgetflag (id);
  if (id == 0)
    return SCM_BOOL_T;
}
#endif


SCM
gucu_ungetmouse (SCM event)
{
  MEVENT *me;
  int ret;

  me = _scm_to_mevent (event);
  ret = ungetmouse (me);
  free (me);
  return (scm_from_int (ret));
}

#ifdef HAVE_UNLOCKPT
/* If FD is the file descriptor of a master pseudo-terminal, this
   changes the mode and permissions of the slave pseudo-terminal
   so that it can be used.  */
SCM
gucu_unlockpt (SCM fd)
{
  int ret;
  SCM_ASSERT (scm_is_integer (fd), fd, SCM_ARG1, "unlockpt");
  ret = unlockpt (scm_to_int (fd));
  if (ret == -1)
    scm_syserror ("unlockpt");

  return SCM_UNSPECIFIED;
}
#endif

/* Get the attributes and color pair number of a window */
SCM
gucu_wattr_get (SCM win)
{
  WINDOW *c_win;
  attr_t c_attrs;
  short c_pair;
  SCM s_list;
  int ret;

  c_win = _scm_to_window (win);

  /* wattr_get appears to be a macro that always returns OK */
  ret = wattr_get (c_win, &c_attrs, &c_pair, NULL);
  if (ret == OK)
    s_list = scm_list_2 (_scm_from_attr (c_attrs), scm_from_short (c_pair));
  else
    abort ();

  return s_list;
}

/* Accept character strings from the curses terminal keyboard */
SCM
gucu_wgetnstr (SCM win, SCM n)
{
  SCM s_str;
  int ret;
  int c_n;

  c_n = scm_to_int (n);
  if (c_n <= 0)
    scm_out_of_range ("%wgetnstr", n);

#ifdef HAVE_NCURSESW
  {
    wint_t *c_wstr = (wint_t *) scm_malloc (sizeof (wint_t) * (c_n + 1));

    ret = wgetn_wstr (_scm_to_window (win), c_wstr, c_n);
    c_wstr[c_n] = 0;
    if (ret == OK)
      {
	s_str = _scm_sstring_from_wint_string (c_wstr);
	free (c_wstr);
      }
    else if (ret == KEY_RESIZE)
      {
	s_str = scm_from_int (KEY_RESIZE);
      }
    else
      abort ();
  }
#else
  {
    char *c_str = (char *) scm_malloc (sizeof (char) * (c_n + 1));

    ret = wgetnstr (_scm_to_window (win), c_str, c_n);
    c_str[c_n] = '\0';
    if (ret == OK)
      {
	s_str = scm_from_locale_string (c_str);
	free (c_str);
      }
    else if (ret == KEY_RESIZE)
      {
	s_str = scm_from_int (KEY_RESIZE);
      }
    else
      abort ();
  }
#endif

  return (s_str);
}

/* Returns a string of N characters and sttributes from the current
   window */
SCM
gucu_winchnstr (SCM win, SCM n)
{
  WINDOW *c_win;
  SCM s_str;
  int c_n;

  c_win = _scm_to_window (win);
  c_n = scm_to_int (n);
  if (c_n == -1)
    c_n = COLS;

#ifdef HAVE_NCURSESW
  {
    int ret;
    cchar_t *c_cstr = (cchar_t *) scm_malloc (sizeof (cchar_t) * (c_n + 1));

    ret = win_wchnstr (c_win, c_cstr, c_n);
    if (ret == ERR)
      return SCM_BOOL_F;
    s_str = _scm_xstring_from_cstring (c_cstr);
    free (c_cstr);
  }
#else
  {
    int ret;
    chtype *c_chstr = (chtype *) scm_malloc (sizeof (chtype) * (c_n + 1));
    ret = winchnstr (_scm_to_window (win), c_chstr, scm_to_int (n));
    if (ret != ERR)
      {
	s_str = _scm_xstring_from_chstring (c_chstr);
	free (c_chstr);
      }
    else
      abort ();
  }
#endif

  return s_str;
}

/* Get a string of characters from a curses window */
SCM
gucu_winnstr (SCM win, SCM n)
{
  SCM s_str;
  int c_n;
  int ret;

  /* -1 indicates that up to an entire line is requested */
  c_n = scm_to_int (n);
  if (c_n == -1)
    {
      int y, x;
      getmaxyx (_scm_to_window (win), y, x);
      c_n = x;
    }
#ifdef HAVE_NCURSESW
  {
    wchar_t *c_wstr;
    c_wstr = (wchar_t *) scm_malloc (sizeof (wchar_t) * (c_n + 1));
    ret = winnwstr (_scm_to_window (win), c_wstr, c_n);
    if (ret != ERR)
      {
	c_wstr[c_n] = 0;
	s_str = _scm_sstring_from_wstring (c_wstr);
      }
    else
      abort ();
  }
#else
  {
    char *c_str;
    c_str = (char *) scm_malloc (sizeof (char) * (c_n + 1));
    ret = winnstr (_scm_to_window (win), c_str, c_n + 1);
    if (ret != ERR)
      {
	c_str[c_n] = 0;
	s_str = scm_from_locale_string (c_str);
	free (c_str);
      }
    else
      abort ();
  }
#endif

  return s_str;
}


/* Convert mouse coordinates to screen coordinates */
SCM
gucu_wmouse_trafo (SCM win, SCM sy, SCM sx, SCM to_screen)
{
  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "mouse-trafo");
  SCM_ASSERT (scm_is_integer (sy), sy, SCM_ARG2, "mouse-trafo");
  SCM_ASSERT (scm_is_integer (sx), sx, SCM_ARG3, "mouse-trafo");
  SCM_ASSERT (scm_is_bool (to_screen), to_screen, SCM_ARG4, "mouse-trafo");

  int x, y, ret;
  x = scm_to_int (sx);
  y = scm_to_int (sy);

  ret = wmouse_trafo (_scm_to_window (win), &y, &x, scm_to_bool (to_screen));
  if (ret)
    {
      return scm_list_2 (scm_from_int (y), scm_from_int (x));
    }
  else
    return SCM_BOOL_F;
}



SCM
gucu_getbegyx (SCM win)
{
  int y, x;

  getbegyx (_scm_to_window (win), y, x);

  return (scm_list_2 (scm_from_int (y), scm_from_int (x)));
}

/* Get the size of the window as a y/x pair*/
SCM
gucu_getmaxyx (SCM win)
{
  int y, x;

  getmaxyx (_scm_to_window (win), y, x);

  return (scm_list_2 (scm_from_int (y), scm_from_int (x)));
}

SCM
gucu_getparyx (SCM win)
{
  int y, x;

  getparyx (_scm_to_window (win), y, x);

  return (scm_list_2 (scm_from_int (y), scm_from_int (x)));
}

/* Return the range of the lines in the scroll region */
SCM
gucu_getscrreg (SCM win)
{
  int top, bottom;

  wgetscrreg (_scm_to_window (win), &top, &bottom);

  return (scm_list_2 (scm_from_int (top), scm_from_int (bottom)));
}


/* Get the location of the virtual screen cursor */
SCM
gucu_getsyx ()
{
  int y = 0, x = 0;

  getsyx (y, x);

  return (scm_list_2 (scm_from_int (y), scm_from_int (x)));
}

SCM
gucu_getyx (SCM win)
{
  int y, x;

  getyx (_scm_to_window (win), y, x);

  return (scm_list_2 (scm_from_int (y), scm_from_int (x)));
}

// The ACS_XXX are supposed to be constants, but in ncurses, they
// are macros that have no value until after initscr().  So here
// they are being bound as scheme thunks.

SCM
gucu_ACS_ULCORNER ()
{
  chtype ret = ACS_ULCORNER;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_LLCORNER ()
{
  chtype ret = ACS_LLCORNER;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_URCORNER ()
{
  chtype ret = ACS_URCORNER;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_LRCORNER ()
{
  chtype ret = ACS_LRCORNER;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_LTEE ()
{
  chtype ret = ACS_LTEE;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_RTEE ()
{
  chtype ret = ACS_RTEE;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_BTEE ()
{
  chtype ret = ACS_BTEE;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_TTEE ()
{
  chtype ret = ACS_TTEE;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_HLINE ()
{
  chtype ret = ACS_HLINE;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_VLINE ()
{
  chtype ret = ACS_VLINE;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_PLUS ()
{
  chtype ret = ACS_PLUS;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_S1 ()
{
  chtype ret = ACS_S1;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_S3 ()
{
  chtype ret = ACS_S3;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_S7 ()
{
  chtype ret = ACS_S7;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_S9 ()
{
  chtype ret = ACS_S9;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_DIAMOND ()
{
  chtype ret = ACS_DIAMOND;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_CKBOARD ()
{
  chtype ret = ACS_CKBOARD;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_DEGREE ()
{
  chtype ret = ACS_DEGREE;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_PLMINUS ()
{
  chtype ret = ACS_PLMINUS;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_BULLET ()
{
  chtype ret = ACS_BULLET;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_LARROW ()
{
  chtype ret = ACS_LARROW;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_RARROW ()
{
  chtype ret = ACS_RARROW;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_DARROW ()
{
  chtype ret = ACS_DARROW;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_UARROW ()
{
  chtype ret = ACS_UARROW;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_BOARD ()
{
  chtype ret = ACS_BOARD;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_LANTERN ()
{
  chtype ret = ACS_LANTERN;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_BLOCK ()
{
  chtype ret = ACS_BLOCK;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_GEQUAL ()
{
  chtype ret = ACS_GEQUAL;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_NEQUAL ()
{
  chtype ret = ACS_NEQUAL;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_LEQUAL ()
{
  chtype ret = ACS_LEQUAL;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_PI ()
{
  chtype ret = ACS_PI;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}

SCM
gucu_ACS_STERLING ()
{
  chtype ret = ACS_STERLING;
  SCM s_ret = _scm_xchar_from_chtype (ret);

  return s_ret;
}


SCM
gucu_LINES ()
{
  int ret = LINES;
  SCM s_ret = scm_from_int (ret);

  return s_ret;
}

SCM
gucu_COLS ()
{
  int ret = COLS;
  SCM s_ret = scm_from_int (ret);

  return s_ret;
}

SCM
gucu_COLORS ()
{
  int ret = COLORS;
  SCM s_ret = scm_from_int (ret);

  return s_ret;
}

SCM
gucu_COLOR_PAIRS ()
{
  int ret = COLOR_PAIRS;
  SCM s_ret = scm_from_int (ret);

  return s_ret;
}

SCM
gucu_TABSIZE ()
{
  int ret = TABSIZE;
  SCM s_ret = scm_from_int (ret);

  return s_ret;
}

#ifdef HAVE_SET_TABSIZE
SCM
gucu_set_TABSIZE (SCM t)
{
  SCM_ASSERT (scm_is_integer (t), t, SCM_ARG1, "set-tabsize!");

  set_tabsize (scm_to_int (t));

  return SCM_UNSPECIFIED;
}
#endif

SCM
gucu_stdscr ()
{
  WINDOW *ret = stdscr;

  if (ret == NULL)
    curs_bad_state_error ("stdscr");

  SCM s_ret = _scm_from_window (ret);

  return s_ret;
}

SCM
gucu_curscr ()
{
  WINDOW *ret = curscr;
  SCM s_ret = _scm_from_window (ret);

  return s_ret;
}

SCM
gucu_getparent (SCM win)
{
  WINDOW *parent = wgetparent (_scm_to_window (win));
  if (parent != (WINDOW *) NULL)
    return _scm_from_win (parent);
  else
    return SCM_BOOL_F;
}

void
gucu_init_special ()
{
  scm_c_define_gsubr ("%wattr-set!", 3, 0, 0, gucu_wattr_set_x);
  scm_c_define_gsubr ("%color-content", 1, 0, 0, gucu_color_content);
  scm_c_define_gsubr ("delwin", 1, 0, 0, gucu_delwin);
  scm_c_define_gsubr ("%getmouse", 0, 0, 0, gucu_getmouse);
#ifdef HAVE_GRANTPT
  scm_c_define_gsubr ("grantpt", 1, 0, 0, gucu_grantpt);
#endif
  scm_c_define_gsubr ("%mcprint", 1, 0, 0, gucu_mcprint);
  scm_c_define_gsubr ("%mousemask", 1, 0, 0, gucu_mousemask);
#ifdef HAVE_PTSNAME
  scm_c_define_gsubr ("ptsname", 1, 0, 0, gucu_ptsname);
#endif
  scm_c_define_gsubr ("ptsmakeraw", 1, 0, 0, gucu_ptsmakeraw);
  scm_c_define_gsubr ("%pair-content", 1, 0, 0, gucu_pair_content);
  scm_c_define_gsubr ("ungetmouse", 1, 0, 0, gucu_ungetmouse);
#ifdef HAVE_UNLOCKPT
  scm_c_define_gsubr ("unlockpt", 1, 0, 0, gucu_unlockpt);
#endif
  scm_c_define_gsubr ("%wattr-get", 1, 0, 0, gucu_wattr_get);
  scm_c_define_gsubr ("%wgetnstr", 2, 0, 0, gucu_wgetnstr);
  scm_c_define_gsubr ("%winchnstr", 2, 0, 0, gucu_winchnstr);
  scm_c_define_gsubr ("%winnstr", 2, 0, 0, gucu_winnstr);
  scm_c_define_gsubr ("mouse-trafo", 4, 0, 0, gucu_wmouse_trafo);
  scm_c_define_gsubr ("%getbegyx", 1, 0, 0, gucu_getbegyx);
  scm_c_define_gsubr ("%getmaxyx", 1, 0, 0, gucu_getmaxyx);
  scm_c_define_gsubr ("%getparyx", 1, 0, 0, gucu_getparyx);
  scm_c_define_gsubr ("%getscrreg", 1, 0, 0, gucu_getscrreg);
  scm_c_define_gsubr ("%getsyx", 0, 0, 0, gucu_getsyx);
  scm_c_define_gsubr ("%getyx", 1, 0, 0, gucu_getyx);
  scm_c_define_gsubr ("%acs-block", 0, 0, 0, gucu_ACS_BLOCK);
  scm_c_define_gsubr ("%acs-board", 0, 0, 0, gucu_ACS_BOARD);
  scm_c_define_gsubr ("%acs-btee", 0, 0, 0, gucu_ACS_BTEE);
  scm_c_define_gsubr ("%acs-bullet", 0, 0, 0, gucu_ACS_BULLET);
  scm_c_define_gsubr ("%acs-ckboard", 0, 0, 0, gucu_ACS_CKBOARD);
  scm_c_define_gsubr ("%acs-darrow", 0, 0, 0, gucu_ACS_DARROW);
  scm_c_define_gsubr ("%acs-degree", 0, 0, 0, gucu_ACS_DEGREE);
  scm_c_define_gsubr ("%acs-diamond", 0, 0, 0, gucu_ACS_DIAMOND);
  scm_c_define_gsubr ("%acs-gequal", 0, 0, 0, gucu_ACS_GEQUAL);
  scm_c_define_gsubr ("%acs-hline", 0, 0, 0, gucu_ACS_HLINE);
  scm_c_define_gsubr ("%acs-lantern", 0, 0, 0, gucu_ACS_LANTERN);
  scm_c_define_gsubr ("%acs-larrow", 0, 0, 0, gucu_ACS_LARROW);
  scm_c_define_gsubr ("%acs-lequal", 0, 0, 0, gucu_ACS_LEQUAL);
  scm_c_define_gsubr ("%acs-llcorner", 0, 0, 0, gucu_ACS_LLCORNER);
  scm_c_define_gsubr ("%acs-lrcorner", 0, 0, 0, gucu_ACS_LRCORNER);
  scm_c_define_gsubr ("%acs-ltee", 0, 0, 0, gucu_ACS_LTEE);
  scm_c_define_gsubr ("%acs-nequal", 0, 0, 0, gucu_ACS_NEQUAL);
  scm_c_define_gsubr ("%acs-pi", 0, 0, 0, gucu_ACS_PI);
  scm_c_define_gsubr ("%acs-plminus", 0, 0, 0, gucu_ACS_PLMINUS);
  scm_c_define_gsubr ("%acs-plus", 0, 0, 0, gucu_ACS_PLUS);
  scm_c_define_gsubr ("%acs-rarrow", 0, 0, 0, gucu_ACS_RARROW);
  scm_c_define_gsubr ("%acs-rtee", 0, 0, 0, gucu_ACS_RTEE);
  scm_c_define_gsubr ("%acs-s1", 0, 0, 0, gucu_ACS_S1);
  scm_c_define_gsubr ("%acs-s3", 0, 0, 0, gucu_ACS_S3);
  scm_c_define_gsubr ("%acs-s7", 0, 0, 0, gucu_ACS_S7);
  scm_c_define_gsubr ("%acs-s9", 0, 0, 0, gucu_ACS_S9);
  scm_c_define_gsubr ("%acs-sterling", 0, 0, 0, gucu_ACS_STERLING);
  scm_c_define_gsubr ("%acs-ttee", 0, 0, 0, gucu_ACS_TTEE);
  scm_c_define_gsubr ("%acs-uarrow", 0, 0, 0, gucu_ACS_UARROW);
  scm_c_define_gsubr ("%acs-ulcorner", 0, 0, 0, gucu_ACS_ULCORNER);
  scm_c_define_gsubr ("%acs-urcorner", 0, 0, 0, gucu_ACS_URCORNER);
  scm_c_define_gsubr ("%acs-vline", 0, 0, 0, gucu_ACS_VLINE);
  scm_c_define_gsubr ("lines", 0, 0, 0, gucu_LINES);
  scm_c_define_gsubr ("cols", 0, 0, 0, gucu_COLS);
  scm_c_define_gsubr ("colors", 0, 0, 0, gucu_COLORS);
  scm_c_define_gsubr ("color-pairs", 0, 0, 0, gucu_COLOR_PAIRS);
  scm_c_define_gsubr ("tabsize", 0, 0, 0, gucu_TABSIZE);
#ifdef HAVE_SET_TABSIZE
  scm_c_define_gsubr ("set-tabsize!", 1, 0, 0, gucu_set_TABSIZE);
#endif
  scm_c_define_gsubr ("stdscr", 0, 0, 0, gucu_stdscr);
  scm_c_define_gsubr ("curscr", 0, 0, 0, gucu_curscr);
  scm_c_define_gsubr ("%getparent", 1, 0, 0, gucu_getparent);
}
