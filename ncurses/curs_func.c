/*
  curs_func.c

  Copyright 2009, 2010, 2011, 2014 Free Software Foundation, Inc.

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

#include <config.h>

#include <libguile.h>
#include <string.h>

#if HAVE_CURSES_H
#include <curses.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#elif HAVE_NCURSESW_CURSES_H
#include <ncursesw/curses.h>
#else
#error "No curses.h file included"
#endif

#include "compat.h"
#include "curs_func.h"
#include "type.h"
#include "unicode.h"

#define RETURNTF(x)                             \
  if(x==ERR)                                    \
    return SCM_BOOL_F;                          \
  else                                          \
    return SCM_BOOL_T

static void
curs_bad_state_error (const char *funcname)
{
  scm_misc_error (funcname, "bad curses internal state", SCM_BOOL_F);
}

/* Assign the colors to color pair zero */
SCM
gucu_assume_default_colors (SCM fg, SCM bg)
{
  int c_fg, c_bg, ret;

  SCM_ASSERT (scm_is_integer (fg), fg, SCM_ARG1, "assume-default-colors");
  SCM_ASSERT (scm_is_integer (bg), bg, SCM_ARG2, "assume-default-colors");

  c_fg = scm_to_int (fg);
  c_bg = scm_to_int (bg);

  ret = assume_default_colors (c_fg, c_bg);
  RETURNTF (ret);
}

/* Turn attributes off on a given window */
SCM
gucu_attr_off_x (SCM win, SCM attrs)
{
  WINDOW *c_win;
  attr_t c_attrs;
  int ret;

  c_win = _scm_to_window (win);
  c_attrs = _scm_to_attr (attrs);

  ret = wattr_off (c_win, c_attrs, (void *) 0);
  /* wattr_off only fails if win is null  */
  RETURNTF (ret);
}

/* Turn on window attributes */
SCM
gucu_attr_on_x (SCM win, SCM attrs)
{
  WINDOW *c_win;
  attr_t c_attrs;
  int ret;

  c_win = _scm_to_window (win);
  c_attrs = _scm_to_attr (attrs);

  ret = wattr_on (c_win, c_attrs, (void *) 0);
  /* wattr_on apparently only fails if c_win is null  */
  RETURNTF (ret);
}

/* Return the output speed of the terminal as an integer */
SCM
gucu_baudrate ()
{
  int ret = baudrate ();
  /* only returns ERR if the screen is not valid  */
  if (ret == ERR)
    return SCM_BOOL_F;

  return scm_from_int (baudrate ());
}

/* Beep! */
SCM
gucu_beep ()
{
  RETURNTF (beep ());
}

/* Set the background property and apply it to every possible location */
SCM
gucu_bkgd (SCM win, SCM ch)
{
  WINDOW *c_win;
  int ret;

  c_win = _scm_to_window (win);

#ifdef HAVE_NCURSESW
  {
    cchar_t *c_ch = _scm_xchar_to_cchar (ch);
    ret = wbkgrnd (c_win, c_ch);
    free (c_ch);
  }
#else
  {
    chtype c_ch = _scm_xchar_to_chtype (ch);

    /* Always returns OK */
    ret = wbkgd (c_win, c_ch);
  }
#endif

  RETURNTF (ret);
}

/* Change the new background character to the given rendered char */
SCM
gucu_bkgdset_x (SCM win, SCM ch)
{
  WINDOW *c_win;

  c_win = _scm_to_window (win);

#ifdef HAVE_NCURSESW
  {
    cchar_t *c_ch = _scm_xchar_to_cchar (ch);
    wbkgrndset (c_win, c_ch);
    free (c_ch);
  }

#else
  {
    chtype c_ch;
    c_ch = _scm_xchar_to_chtype (ch);

    wbkgdset (c_win, c_ch);
  }
#endif

  return SCM_UNSPECIFIED;
}

SCM
gucu_border (SCM win, SCM left, SCM right, SCM top, SCM bottom,
	     SCM topleft, SCM topright, SCM bottomleft, SCM bottomright)
{
  WINDOW *c_win;
  int ret;

  c_win = _scm_to_window (win);

#ifdef HAVE_NCURSESW
  {
    cchar_t *c_left, *c_right, *c_top, *c_bottom;
    cchar_t *c_topleft, *c_topright, *c_bottomleft, *c_bottomright;
    c_left = _scm_xchar_to_cchar (left);
    c_right = _scm_xchar_to_cchar (right);
    c_top = _scm_xchar_to_cchar (top);
    c_bottom = _scm_xchar_to_cchar (bottom);
    c_topleft = _scm_xchar_to_cchar (topleft);
    c_topright = _scm_xchar_to_cchar (topright);
    c_bottomleft = _scm_xchar_to_cchar (bottomleft);
    c_bottomright = _scm_xchar_to_cchar (bottomright);

    ret = wborder_set (c_win, c_left, c_right, c_top, c_bottom,
		       c_topleft, c_topright, c_bottomleft, c_bottomright);

    free (c_left);
    free (c_right);
    free (c_top);
    free (c_bottom);
    free (c_topleft);
    free (c_topright);
    free (c_bottomleft);
    free (c_bottomright);
  }
#else
  {
    chtype c_left, c_right, c_top, c_bottom;
    chtype c_topleft, c_topright, c_bottomleft, c_bottomright;

    c_left = _scm_xchar_to_chtype (left);
    c_right = _scm_xchar_to_chtype (right);
    c_top = _scm_xchar_to_chtype (top);
    c_bottom = _scm_xchar_to_chtype (bottom);
    c_topleft = _scm_xchar_to_chtype (topleft);
    c_topright = _scm_xchar_to_chtype (topright);
    c_bottomleft = _scm_xchar_to_chtype (bottomleft);
    c_bottomright = _scm_xchar_to_chtype (bottomright);

    ret = wborder (c_win, c_left, c_right, c_top, c_bottom,
		   c_topleft, c_topright, c_bottomleft, c_bottomright);
  }
#endif

  RETURNTF (ret);
}


/* Check if the colors can be changed. */
SCM
gucu_can_change_color_p ()
{
  bool ret;
  SCM s_ret;

  ret = can_change_color ();
  s_ret = scm_from_bool (ret);

  return s_ret;
}

/* Disable line buffering and erase/kill character processing */
SCM
gucu_cbreak ()
{
  RETURNTF (cbreak ());
}

/* Copy blanks to entire window */
SCM
gucu_clear (SCM win)
{
  WINDOW *c_win;
  int ret;

  c_win = _scm_to_window (win);

  ret = wclear (c_win);
  /* Returns ERR if window is NULL */
  RETURNTF (ret);
}

/* Enable or disable if the next call to wrefresh will completely
   clear and redraw the screen from scratch */
SCM
gucu_clearok (SCM win, SCM bf)
{
  WINDOW *c_win;
  bool c_bf;

  c_win = _scm_to_window (win);
  c_bf = scm_to_bool (bf);

  clearok (c_win, c_bf);

  return SCM_UNSPECIFIED;
}

/* Clear to bottom of screen.  */
SCM
gucu_clrtobot (SCM win)
{
  WINDOW *c_win;
  int ret;

  c_win = _scm_to_window (win);

  ret = wclrtobot (c_win);
  /* wclrtobot only returns ERR when the window is null */
  RETURNTF (ret);
}

/* Clear to end of line */
SCM
gucu_clrtoeol (SCM win)
{
  WINDOW *c_win;
  int ret;

  c_win = _scm_to_window (win);

  ret = wclrtoeol (c_win);

  /* wclrtoeol fails on window == NULL or when position is outside
     window */
  RETURNTF (ret);
}

/* Returns the attribute that corresponds to a given color pair */
SCM
gucu_COLOR_PAIR (SCM pair1)
{
  int c_pair1, ret;

  SCM_ASSERT (scm_is_integer (pair1), pair1, SCM_ARG1, "color-pair");

  c_pair1 = scm_to_int (pair1);

  ret = COLOR_PAIR (c_pair1);

  return scm_from_int (ret);
}

/* Set the color pair of a window */
SCM
gucu_color_set (SCM win, SCM pair)
{
  WINDOW *c_win;
  short c_pair;
  int ret;

  c_win = _scm_to_window (win);
  c_pair = scm_to_short (pair);

  ret = wcolor_set (c_win, c_pair, 0);
  /* wcolor_set returns an error if pair is not 0 to COLOR_PAIRS -1 */
  RETURNTF (ret);
}

/* Copy part of srcwin onto part of destwin */
SCM
gucu_copywin (SCM srcwin, SCM dstwin, SCM sminrow, SCM smincol, SCM dminrow,
	      SCM dmincol, SCM dmaxrow, SCM dmaxcol, SCM _overlay)
{
  WINDOW *c_srcwin, *c_dstwin;
  int c_sminrow, c_smincol, c_dminrow, c_dmincol, c_dmaxrow, c_dmaxcol;
  int ret;

  c_srcwin = _scm_to_window (srcwin);
  c_dstwin = _scm_to_window (dstwin);
  c_sminrow = scm_to_int (sminrow);
  c_smincol = scm_to_int (smincol);
  c_dminrow = scm_to_int (dminrow);
  c_dmincol = scm_to_int (dmincol);
  c_dmaxrow = scm_to_int (dmaxrow);
  c_dmaxcol = scm_to_int (dmaxcol);

  if (scm_is_true (_overlay))
    ret = copywin ((const WINDOW *)c_srcwin, c_dstwin, c_sminrow, c_smincol,
		   c_dminrow, c_dmincol, c_dmaxrow, c_dmaxcol, 1);
  else
    ret = copywin ((const WINDOW *)c_srcwin, c_dstwin, c_sminrow, c_smincol,
		   c_dminrow, c_dmincol, c_dmaxrow, c_dmaxcol, 0);

  /* copywin fails if the windows are NULL, the rectangle doesn't fit
     in the source, or it doesn't fit in the destination */
  RETURNTF (ret);
}

/* Curses is visible or invisible */
SCM
gucu_curs_set (SCM visibility)
{
  int c_visibility, ret;

  c_visibility = scm_to_int (visibility);
  ret = curs_set (c_visibility);
  /* Return values is the previous visibility setting, or ERR if the
     requested visibility is not supported.  */
  if (ret == ERR)
    return SCM_BOOL_F;

  return scm_from_int (ret);
}

/* Return a string giving the version number of the underlying ncurses
   library */
SCM
gucu_curses_version ()
{
  return scm_from_locale_string (curses_version ());
}

/* Save the current terminal mode, so that it can be returned later by
   reset_prog_mode */
SCM
gucu_def_prog_mode ()
{
  /* def_prog_mode can return ERR, but, I don't think it can return an
     error the way it is used in this library  */
  RETURNTF (def_prog_mode ());
}

/* Save the current terminal mode, so that it can be returned later by
   reset_shell_mode */
SCM
gucu_def_shell_mode ()
{
  RETURNTF (def_shell_mode ());
}

/* Define a keycode and its corresponding control string */
SCM
gucu_define_key (SCM defn, SCM keycode)
{
  char *c_defn;
  int c_keycode, ret;

  SCM_ASSERT (scm_is_string (defn), defn, SCM_ARG1, "define-key");
  SCM_ASSERT (scm_is_integer (keycode), keycode, SCM_ARG2, "define-key");

  c_keycode = scm_to_int (keycode);

  if (scm_c_string_length (defn) == 0)
    {
      ret = define_key ((const char *) NULL, c_keycode);
    }
  else
    {
      c_defn = scm_to_locale_string (defn);
      ret = define_key ((const char *) c_defn, c_keycode);
      free (c_defn);
    }

  RETURNTF (ret);
}

/* Inserts a pause in between outputs, possibly with padding
   characters */
SCM
gucu_delay_output (SCM ms)
{
  int c_ms;

  SCM_ASSERT (scm_is_integer (ms), ms, SCM_ARG1, "delay-output");

  c_ms = scm_to_int (ms);
  if (c_ms < 0)
    scm_out_of_range ("delay-output", ms);

  delay_output (c_ms);
  /* delay_output returns false only when the screen or tinfo doesn't
     exist.  */

  return SCM_UNSPECIFIED;
}

/* Free a screen created by newterm.  Should be called after endwin */
SCM
gucu_delscreen (SCM scr)
{
  if (!isendwin ())
    /* terminal was freed while still in curses mode */
    return SCM_BOOL_F;

  _scm_free_screen (scr);

  return SCM_BOOL_T;
}

/* Create a pointer to a window enclosed by and sharing memory with
   another window */
SCM
gucu_derwin (SCM orig, SCM nlines, SCM ncols, SCM begin_y, SCM begin_x)
{
  int c_nlines, c_ncols, c_begin_y, c_begin_x;
  WINDOW *c_orig, *ret;

  c_orig = _scm_to_window (orig);
  c_nlines = scm_to_int (nlines);
  c_ncols = scm_to_int (ncols);
  c_begin_y = scm_to_int (begin_y);
  c_begin_x = scm_to_int (begin_x);

  ret = derwin (c_orig, c_nlines, c_ncols, c_begin_y, c_begin_x);
  if (ret == 0)
    return SCM_BOOL_F;

  return _scm_from_window (ret);
}

/* Send to the physical terminal all the changes committed by calls to
   wnoutrefresh */
SCM
gucu_doupdate ()
{
  doupdate ();
  /* doupdate probably only fails when the screen is NULL */

  return SCM_UNSPECIFIED;
}

/* Create an exact duplicate of a window */
SCM
gucu_dupwin (SCM win)
{
  WINDOW *c_win, *ret;

  c_win = _scm_to_window (win);

  ret = dupwin (c_win);
  /* dupwin only fails if window is NULL, which should never happen */
  if (ret == NULL)
    return SCM_BOOL_F;

  return _scm_from_window (ret);
}

/* Enable the echoing of user-typed characters */
SCM
gucu_echo ()
{
  echo ();
  /* echo always returns true */
  return SCM_UNSPECIFIED;
}

/* Exit curses mode for this screen */
SCM
gucu_endwin ()
{
  int ret = endwin ();
  /* returns true if screen exists and reset_shell_mode succeeds */
  RETURNTF (ret);
}

/* Copy blanks into every position in the window */
SCM
gucu_erase (SCM win)
{
  WINDOW *c_win;
  int ret;

  c_win = _scm_to_window (win);

  ret = werase (c_win);
  RETURNTF (ret);
}

/* Return the current character used to erase */
SCM
gucu_erasechar ()
{
#ifdef HAVE_NCURSESW
  wchar_t ch;
  int ret = erasewchar (&ch);
  if (ret == ERR)
    return SCM_BOOL_F;
  else
    return _scm_schar_from_wchar (ch);
#else
  char ch;
  ch = erasechar ();
  if (ch == -1)
    return SCM_BOOL_F;
  else
    return _scm_schar_from_char (ch);
#endif
}

/* Do curses only on one line */
SCM
gucu_filter ()
{
  filter ();
  return SCM_UNSPECIFIED;
}

/* Flash the screen */
SCM
gucu_flash ()
{
  RETURNTF (flash ());
}

/* Throw away any typeahead */
SCM
gucu_flushinp ()
{
  int ret = flushinp ();

  if (ret == ERR)
    curs_bad_state_error ("flushinp");

  return SCM_UNSPECIFIED;

}

/* Return the background rendered character of the given window */
SCM
gucu_getbkgd (SCM win)
{
  WINDOW *c_win;

  c_win = _scm_to_window (win);
#ifdef HAVE_NCURSESW
  {
    cchar_t ch;
    int ret = wgetbkgrnd (c_win, &ch);
    if (ret == ERR)
      return SCM_BOOL_F;
    return _scm_xchar_from_cchar (&ch);
  }
#else
  {
    chtype ret = getbkgd (c_win);
    return _scm_xchar_from_chtype (ret);
  }
#endif
}

/* Set halfdelay. */
SCM
gucu_halfdelay (SCM tenths)
{
  int c_tenths;

  c_tenths = scm_to_int (tenths);

  halfdelay (c_tenths);

  return SCM_UNSPECIFIED;
}

/* Check if this screen has colors */
SCM
gucu_has_colors_p ()
{
  bool ret;
  SCM s_ret;

  ret = has_colors ();
  s_ret = scm_from_bool (ret);

  return s_ret;
}

/* True if the terminal has insert and delete character capabilities */
SCM
gucu_has_ic_p ()
{
  return (scm_from_bool (has_ic ()));
}

/* True if the terminal and insert- and delete-line capabilities */
SCM
gucu_has_il_p ()
{
  bool ret;

  ret = has_il ();
  return (scm_from_bool (ret));
}

/* Return #t/#f if the terminal type recognizes a key of that value */
SCM
gucu_has_key_p (SCM key)
{
  int c_key;

  c_key = scm_to_int (key);

  RETURNTF (has_key (c_key));
}

/* Return #t if the mouse driver has been successfully initialized. */
SCM
gucu_has_mouse_p (void)
{
  return (scm_from_bool (has_mouse ()));
}

/* When true, try to use a terminal's hardware to clear characters */
SCM
gucu_idcok_x (SCM win, SCM bf)
{
  WINDOW *c_win;
  bool c_bf;

  c_win = _scm_to_window (win);
  c_bf = scm_to_bool (bf);

  idcok (c_win, c_bf);

  return SCM_UNSPECIFIED;
}

/* When true, try to use a terminal's hardware to clear lines */
SCM
gucu_idlok_x (SCM win, SCM bf)
{
  WINDOW *c_win;
  bool c_bf;

  c_win = _scm_to_window (win);
  c_bf = scm_to_bool (bf);

  idlok (c_win, c_bf);

  return SCM_UNSPECIFIED;
}

/* When true, any change to the window will cause an immediate refresh */
SCM
gucu_immedok_x (SCM win, SCM bf)
{
  WINDOW *c_win;
  bool c_bf;

  c_win = _scm_to_window (win);
  c_bf = scm_to_bool (bf);

  immedok (c_win, c_bf);

  return SCM_UNSPECIFIED;
}

/* Initialize curses and return stdscr */
SCM
gucu_initscr ()
{
  WINDOW *ret;

  ret = initscr ();
  /* Initscr only fails when something really bad happens. */
  if (ret == NULL)
    return SCM_BOOL_F;

  return _scm_from_window (ret);
}

/* Initialize COLOR as an RGB triple. */
SCM
gucu_init_color (SCM color, SCM r, SCM g, SCM b)
{
  short c_color, c_r, c_g, c_b;
  int ret;

  c_color = scm_to_short (color);
  c_r = scm_to_short (r);
  c_g = scm_to_short (g);
  c_b = scm_to_short (b);
  /* Necessary but insufficient range checks */
  if (c_color < 0 || c_color > COLORS)
    scm_out_of_range ("init-color!", color);
  if (c_r < 0 || c_r > 1000)
    scm_out_of_range ("init-color!", r);
  if (c_g < 0 || c_g > 1000)
    scm_out_of_range ("init-color!", g);
  if (c_b < 0 || c_b > 1000)
    scm_out_of_range ("init-color!", b);

  ret = init_color (c_color, c_r, c_g, c_b);
  RETURNTF (ret);
}

/* Initalize a color pair */
SCM
gucu_init_pair (SCM pair, SCM fore, SCM back)
{
  int ret;
  short c_pair, c_fore, c_back;

  c_pair = scm_to_short (pair);
  c_fore = scm_to_short (fore);
  c_back = scm_to_short (back);

  ret = init_pair (c_pair, c_fore, c_back);
  RETURNTF (ret);
}

SCM
gucu_intrflush (SCM bf)
{
  bool c_bf;
  int ret;

  c_bf = scm_to_bool (bf);

  ret = intrflush (stdscr, c_bf);
  /* If ret is ERR, this is a bad state */
  RETURNTF (ret);
}

SCM
gucu_is_cleared_p (SCM win)
{
  return scm_from_bool (is_cleared (_scm_to_window (win)));
}

SCM
gucu_is_idcok_p (SCM win)
{
  return scm_from_bool (is_idcok (_scm_to_window (win)));
}

SCM
gucu_is_idlok_p (SCM win)
{
  return scm_from_bool (is_idlok (_scm_to_window (win)));
}

SCM
gucu_is_immedok_p (SCM win)
{
  return scm_from_bool (is_immedok (_scm_to_window (win)));
}

SCM
gucu_is_keypad_p (SCM win)
{
  return scm_from_bool (is_keypad (_scm_to_window (win)));
}

SCM
gucu_is_leaveok_p (SCM win)
{
  return scm_from_bool (is_leaveok (_scm_to_window (win)));
}

SCM
gucu_is_nodelay_p (SCM win)
{
  return scm_from_bool (is_nodelay (_scm_to_window (win)));
}

SCM
gucu_is_notimeout_p (SCM win)
{
  return scm_from_bool (is_notimeout (_scm_to_window (win)));
}

#if defined(HAVE_IS_PAD) || ! defined(NCURSES_OPAQUE)
SCM
gucu_is_pad_p (SCM win)
{
  WINDOW *c_pad;

  c_pad = _scm_to_window (win);

#ifdef HAVE_IS_PAD
  return scm_from_bool (is_pad (c_pad));
#else
  return scm_from_bool (c_pad->_flags & _ISPAD);
#endif
}
#endif

SCM
gucu_is_scrollok_p (SCM win)
{
  return scm_from_bool (is_scrollok (_scm_to_window (win)));
}

#if defined(HAVE_IS_SUBWIN) || ! defined(NCURSES_OPAQUE)
SCM
gucu_is_subwin_p (SCM win)
{
  WINDOW *c_subwin;

  c_subwin = _scm_to_window (win);

#ifdef HAVE_IS_SUBWIN
  return scm_from_bool (is_subwin (c_subwin));
#else
  return scm_from_bool (c_subwin->_flags & _SUBWIN);
#endif
}
#endif

SCM
gucu_is_syncok_p (SCM win)
{
  return scm_from_bool (is_syncok (_scm_to_window (win)));
}

/* Return true if we are not in curses mode (endwin has been called) */
SCM
gucu_isendwin_p ()
{
  SCM s_ret;
  bool ret;

  ret = isendwin ();
  s_ret = scm_from_bool (ret);

  return s_ret;
}

/* Return #t if line has been altered since last refresh */
SCM
gucu_is_linetouched_p (SCM win, SCM line)
{
  WINDOW *c_win;
  int c_line;
  int ret;

  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "is-linetouched?");
  SCM_ASSERT (scm_is_integer (line), line, SCM_ARG2, "is-linetouched?");

  c_win = _scm_to_window (win);
  c_line = scm_to_int (line);

  ret = is_linetouched (c_win, c_line);
  if (ret == ERR)
    curs_bad_state_error ("is-linetouched?");

  return scm_from_bool (ret);
}

/* Return #t if window has been altered since last refresh */
SCM
gucu_is_wintouched_p (SCM win)
{
  WINDOW *c_win;
  int ret;

  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "is-wintouched?");

  c_win = _scm_to_window (win);

  ret = is_wintouched (c_win);
  if (ret == ERR)
    curs_bad_state_error ("is-wintouched?");

  return scm_from_bool (ret);
}

/* Return the function key value that corresponds to a function key */
SCM
gucu_KEY_F (SCM n)
{
  int c_n, ret;

  SCM_ASSERT (scm_is_integer (n), n, SCM_ARG1, "key-f");

  c_n = scm_to_int (n);

  ret = KEY_F (c_n);

  return scm_from_int (ret);
}

#ifdef HAVE_KEY_DEFINED
/* Returns the keycode bound to STR, or #f on failure */
SCM
gucu_key_defined (SCM str)
{
  SCM_ASSERT (scm_is_string (str), str, SCM_ARG1, "key-defined");

  char *c_str = scm_to_locale_string (str);
  int key = key_defined (c_str);
  free (c_str);
  if (key == 0)
    return SCM_BOOL_F;

  return scm_from_int (key);
}
#endif

/* Returns a character string corresponding to the short key name */
SCM
gucu_keyname (SCM ch)
{
  int c_key;
  char *ret;

  SCM_ASSERT (scm_is_integer (ch) || SCM_CHARP (ch), ch, SCM_ARG1, "keyname");

  if (scm_is_integer (ch))
    {
      c_key = scm_to_int (ch);
      ret = keyname (c_key);
    }
#ifdef HAVE_NCURSESW
  else
    {
      wchar_t c_ch = _scm_schar_to_wchar (ch);
      ret = key_name (c_ch);
    }
#else
  else
    {
      int c_ch;
      c_ch = _scm_schar_to_char (ch);
      ret = keyname (c_ch);
    }
#endif
  if (ret == NULL)
    return scm_from_locale_string ("(unknown)");

  return scm_from_locale_string (ret);
}

/* Enable function key processing */
SCM
gucu_keypad_x (SCM win, SCM bf)
{
  WINDOW *c_win;
  bool c_bf;
  int ret;

  c_win = _scm_to_window (win);
  c_bf = scm_to_bool (bf);

  ret = keypad (c_win, c_bf);
  if (ret == ERR)
    curs_bad_state_error ("keypad!");

  return SCM_UNSPECIFIED;
}

/* Return the current line kill character */
SCM
gucu_killchar ()
{
#ifdef HAVE_NCURSESW
  wchar_t ch;
  int ret = killwchar (&ch);
  if (ret == ERR)
    return SCM_BOOL_F;
  else
    return _scm_schar_from_wchar (ch);
#else
  char ch;
  ch = killchar ();
  if (ch == -1)
    return SCM_BOOL_F;
  else
    return _scm_schar_from_char (ch);
#endif
}

/* If false, curses will try to put the hardware cursor where the
   curses cursor is located.  If true, it will be sloppy, perhaps
   saving time */
SCM
gucu_leaveok_x (SCM win, SCM bf)
{
  WINDOW *c_win;
  bool c_bf;
  int ret;

  c_win = _scm_to_window (win);
  c_bf = scm_to_bool (bf);

  ret = leaveok (c_win, c_bf);
  /* ERR is bad state */
  RETURNTF (ret);
}

/* Return a verbose description of the current terminal */
SCM
gucu_longname ()
{
  char *ret;
  SCM s_ret;

  ret = longname ();

  /* Longname can return an empty string. */
  if (strlen (ret) == 0)
    return gucu_termname ();

  s_ret = scm_from_locale_string (ret);

  /* No need to free static string returned by longname */

  return s_ret;
}

/* Set the terminal to 7 or 8 bit mode */
SCM
gucu_meta_x (SCM bf)
{
  bool c_bf;
  int ret;

  c_bf = scm_to_bool (bf);

  ret = meta ((WINDOW *) 0, c_bf);
  /* ret is ERR when state is bad */
  RETURNTF (ret);
}

/* Set the maximum time between click and release for something to register
   as a button press */
SCM
gucu_mouseinterval (SCM thousandths)
{
  int c_thousandths, ret;

  c_thousandths = scm_to_int (thousandths);

  ret = mouseinterval (c_thousandths);

  return scm_from_int (ret);
}

/* Low-level cursor function: move from old to new position.  Works
   immediately. DEPRECATED?  Who would ever use this?*/
SCM
gucu_mvcur (SCM oldy, SCM oldx, SCM newy, SCM newx)
{
  int c_oldy, c_oldx, c_newy, c_newx, ret;

  SCM_ASSERT (scm_is_integer (oldy), oldy, SCM_ARG1, "mvcur");
  SCM_ASSERT (scm_is_integer (oldx), oldx, SCM_ARG2, "mvcur");
  SCM_ASSERT (scm_is_integer (newy), newy, SCM_ARG3, "mvcur");
  SCM_ASSERT (scm_is_integer (newx), newx, SCM_ARG4, "mvcur");

  c_oldy = scm_to_int (oldy);
  c_oldx = scm_to_int (oldx);
  c_newy = scm_to_int (newy);
  c_newx = scm_to_int (newx);

  ret = mvcur (c_oldy, c_oldx, c_newy, c_newx);
  return scm_from_int (ret);
}

/* Move a derived window inside its parent window */
SCM
gucu_mvderwin (SCM win, SCM par_y, SCM par_x)
{
  WINDOW *c_win;
  int c_par_y, c_par_x, ret;

  c_win = _scm_to_window (win);
  c_par_y = scm_to_int (par_y);
  c_par_x = scm_to_int (par_x);

  ret = mvderwin (c_win, c_par_y, c_par_x);
  if (ret == ERR)
    return SCM_BOOL_F;

  return SCM_UNSPECIFIED;
}

/* Move the window so that the upper left is at y,x */
SCM
gucu_mvwin (SCM win, SCM y, SCM x)
{
  WINDOW *c_win;
  int c_y, c_x, ret;

  c_win = _scm_to_window (win);
  c_y = scm_to_int (y);
  c_x = scm_to_int (x);

  ret = mvwin (c_win, c_y, c_x);
  if (ret == ERR)
    return SCM_BOOL_F;

  return SCM_UNSPECIFIED;
}

/* Pause for some number of milliseconds */
SCM
gucu_napms (SCM ms)
{
  int c_ms;

  c_ms = scm_to_int (ms);

  napms (c_ms);

  return SCM_UNSPECIFIED;
}

SCM
gucu_newpad (SCM nlines, SCM ncols)
{
  int c_nlines, c_ncols;
  WINDOW *ret;

  c_nlines = scm_to_int (nlines);
  c_ncols = scm_to_int (ncols);
  if (c_nlines <= 0)
    scm_out_of_range ("newpad", nlines);
  if (c_ncols <= 0)
    scm_out_of_range ("newpad", ncols);

  ret = newpad (c_nlines, c_ncols);
  if (ret == NULL)
    return SCM_BOOL_F;

  return _scm_from_window (ret);
}

/* Creates and returns a pointer to a new window */
SCM
gucu_newwin (SCM nlines, SCM ncols, SCM begin_y, SCM begin_x)
{
  int c_nlines, c_ncols, c_begin_y, c_begin_x;
  WINDOW *c_win;

  c_nlines = scm_to_int (nlines);
  c_ncols = scm_to_int (ncols);
  c_begin_y = scm_to_int (begin_y);
  c_begin_x = scm_to_int (begin_x);

  c_win = newwin (c_nlines, c_ncols, c_begin_y, c_begin_x);
  if (c_win == NULL)
    return SCM_BOOL_F;

  return _scm_from_window (c_win);
}

/* Translate return to newline on input */
SCM
gucu_nl ()
{
  nl ();

  return SCM_UNSPECIFIED;
}

/* Enable line buffering and erase/kill character processing */
SCM
gucu_nocbreak ()
{
  RETURNTF (nocbreak ());
}

/* Causes getch() to be non-blocking, so that it returns #f if no
   character is ready */
SCM
gucu_nodelay_x (SCM win, SCM bf)
{
  WINDOW *c_win;
  bool c_bf;
  int ret;

  c_win = _scm_to_window (win);
  c_bf = scm_to_bool (bf);

  ret = nodelay (c_win, c_bf);
  RETURNTF (ret);
}

/* Disable echoing of user-type characters */
SCM
gucu_noecho ()
{
  noecho ();

  return SCM_UNSPECIFIED;
}

/* Do not translate the return key to newline on input */
SCM
gucu_nonl ()
{
  nonl ();

  return SCM_UNSPECIFIED;
}

/* Do no flush i/o queues on interrupts */
SCM
gucu_noqiflush ()
{
  noqiflush ();
  return SCM_UNSPECIFIED;
}

/* Allow control characters to generate signals. */
SCM
gucu_noraw ()
{
  RETURNTF (noraw ());
}

/* Do not set an timer for escape sequence processing */
SCM
gucu_notimeout_x (SCM win, SCM bf)
{
  WINDOW *c_win;
  bool c_bf;
  int ret;

  c_win = _scm_to_window (win);
  c_bf = scm_to_bool (bf);

  ret = notimeout (c_win, c_bf);
  RETURNTF (ret);
}

SCM
gucu_noutrefresh (SCM win)
{
  WINDOW *c_win;
  int ret;

  c_win = _scm_to_window (win);

  ret = wnoutrefresh (c_win);
  RETURNTF (ret);
}

/* Overlay srcwin on top of destwin, ignoring blanks */
SCM
gucu_overlay (SCM srcwin, SCM destwin)
{
  WINDOW *c_srcwin, *c_destwin;
  int ret;

  c_srcwin = _scm_to_window (srcwin);
  c_destwin = _scm_to_window (destwin);

  ret = overlay (c_srcwin, c_destwin);
  RETURNTF (ret);
}

/* Overlay srcwin  on top of destwin, including blanks */
SCM
gucu_overwrite (SCM srcwin, SCM destwin)
{
  WINDOW *c_srcwin, *c_destwin;
  int ret;

  c_srcwin = _scm_to_window (srcwin);
  c_destwin = _scm_to_window (destwin);

  ret = overwrite (c_srcwin, c_destwin);
  RETURNTF (ret);
}

/* Return the pair number associated with a color pair attribute */
SCM
gucu_PAIR_NUMBER (SCM attr)
{
  int c_attr, ret;

  SCM_ASSERT (scm_is_integer (attr), attr, SCM_ARG1, "pair-number");

  c_attr = scm_to_int (attr);

  ret = PAIR_NUMBER (c_attr);
  return scm_from_int (ret);
}

SCM
gucu_pechochar (SCM pad, SCM ch)
{
  WINDOW *c_pad;
  int ret;

  c_pad = _scm_to_window (pad);

#ifndef NCURSES_OPAQUE
  if (!(c_pad->_flags & _ISPAD))
    scm_misc_error ("%pechochar", "not a pad", scm_list_1 (pad));
#endif

#ifdef HAVE_NCURSESW
  {
    cchar_t *c_ch = _scm_xchar_to_cchar (ch);

    ret = pecho_wchar (c_pad, c_ch);
    free (c_ch);
  }
#else
  {
    chtype c_ch = _scm_xchar_to_chtype (ch);

    ret = pechochar (c_pad, c_ch);
  }
#endif
  if (ret == ERR)
    curs_bad_state_error ("%pechochar");

  return SCM_UNSPECIFIED;
}

SCM
gucu_pnoutrefresh (SCM win, SCM pminrow, SCM pmincol, SCM sminrow,
		   SCM smincol, SCM smaxrow, SCM smaxcol)
{
  WINDOW *c_win;
  int c_pminrow, c_pmincol, c_sminrow, c_smincol, c_smaxrow, c_smaxcol, ret;

  c_win = _scm_to_window (win);
  c_pminrow = scm_to_int (pminrow);
  c_pmincol = scm_to_int (pmincol);
  c_sminrow = scm_to_int (sminrow);
  c_smincol = scm_to_int (smincol);
  c_smaxrow = scm_to_int (smaxrow);
  c_smaxcol = scm_to_int (smaxcol);

  ret = pnoutrefresh (c_win, c_pminrow, c_pmincol, c_sminrow, c_smincol,
		      c_smaxrow, c_smaxcol);
  RETURNTF (ret);
}

SCM
gucu_prefresh (SCM win, SCM pminrow, SCM pmincol, SCM sminrow, SCM smincol,
	       SCM smaxrow, SCM smaxcol)
{
  WINDOW *c_win;
  int c_pminrow, c_pmincol, c_sminrow, c_smincol, c_smaxrow, c_smaxcol, ret;

  c_win = _scm_to_window (win);
  c_pminrow = scm_to_int (pminrow);
  c_pmincol = scm_to_int (pmincol);
  c_sminrow = scm_to_int (sminrow);
  c_smincol = scm_to_int (smincol);
  c_smaxrow = scm_to_int (smaxrow);
  c_smaxcol = scm_to_int (smaxcol);

  ret = prefresh (c_win, c_pminrow, c_pmincol, c_sminrow, c_smincol,
		  c_smaxrow, c_smaxcol);
  RETURNTF (ret);
}

/* Flush queues on interupts */
SCM
gucu_qiflush ()
{
  qiflush ();

  return SCM_UNSPECIFIED;
}

/* Pass control characters instead of generating a signal */
SCM
gucu_raw ()
{
  RETURNTF (raw ());
}

/* Indicate that this window should be redrawn in its entirity */
SCM
gucu_redrawwin (SCM win)
{
  WINDOW *c_win;
  int ret;

  c_win = _scm_to_window (win);

  ret = redrawwin (c_win);
  RETURNTF (ret);
}

/* Update the physical terminal to reflect the changes in the window */
SCM
gucu_refresh (SCM win)
{
  WINDOW *c_win;
  int ret;

  if (stdscr == NULL)
    return SCM_BOOL_F;

  c_win = _scm_to_window (win);

  ret = wrefresh (c_win);
  RETURNTF (ret);
}

SCM
gucu_reset_prog_mode ()
{
  RETURNTF (reset_prog_mode ());
}

SCM
gucu_reset_shell_mode ()
{
  RETURNTF (reset_shell_mode ());
}

/* Restore the state of the terminal modes */
SCM
gucu_resetty ()
{
  RETURNTF (resetty ());
}

/* Resize the standard and current windows. */
SCM
gucu_resizeterm(SCM lines, SCM columns)
{
  int c_lines, c_columns, ret;

  SCM_ASSERT (scm_is_integer (lines), lines, SCM_ARG1, "resizeterm");
  SCM_ASSERT (scm_is_integer (columns), columns, SCM_ARG2, "resizeterm");
  c_lines = scm_to_int (lines);
  c_columns = scm_to_int (columns);
  ret = resizeterm (c_lines, c_columns);

  RETURNTF(ret);
}

/* Save the state of the terminal modes */
SCM
gucu_savetty ()
{
  RETURNTF (savetty ());
}

/* Write a curses screen to a file */
SCM
gucu_scr_dump (SCM fname)
{
  char *c_fname;
  int ret;

  c_fname = scm_to_locale_string (fname);
  ret = scr_dump (c_fname);
  free (c_fname);

  RETURNTF (ret);
}

/* Given a file, use it to initialize the curses data structures */
SCM
gucu_scr_init (SCM fname)
{
  char *c_fname;
  int ret;

  c_fname = scm_to_locale_string (fname);
  ret = scr_init (c_fname);
  free (c_fname);

  RETURNTF (ret);
}

/* Sets the virtual screen to the contents of filename */
SCM
gucu_scr_restore (SCM fname)
{
  char *c_fname;
  int ret;

  c_fname = scm_to_locale_string (fname);
  ret = scr_restore (c_fname);
  free (c_fname);

  RETURNTF (ret);
}

/* sets the screen to the contents of filename */
SCM
gucu_scr_set (SCM fname)
{
  char *c_fname;
  int ret;

  c_fname = scm_to_locale_string (fname);
  ret = scr_set (c_fname);
  free (c_fname);

  RETURNTF (ret);
}

/* Scroll the window N lines.  Positive N == up. Negative N == down */
SCM
gucu_scrl (SCM win, SCM n)
{
  WINDOW *c_win;
  int c_n, ret;

  c_win = _scm_to_window (win);
  c_n = scm_to_int (n);

  ret = wscrl (c_win, c_n);
  RETURNTF (ret);
}


/* If true, the window will scroll when the cursor touches the end of
   the screen */
SCM
gucu_scrollok_x (SCM win, SCM bf)
{
  WINDOW *c_win;
  bool c_bf;
  int ret;

  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "scrollok!");
  SCM_ASSERT (scm_is_bool (bf), bf, SCM_ARG2, "scrollok!");

  c_win = _scm_to_window (win);
  c_bf = scm_to_bool (bf);

  ret = scrollok (c_win, c_bf);
  if (ret == ERR)
    curs_bad_state_error ("scrollok!");

  return SCM_UNSPECIFIED;
}

/* set the scroll region for the window to be between TOP and BOT */
SCM
gucu_setscrreg_x (SCM win, SCM top, SCM bot)
{
  WINDOW *c_win;
  int c_top, c_bot, ret;

  c_win = _scm_to_window (win);
  c_top = scm_to_int (top);
  c_bot = scm_to_int (bot);

  ret = wsetscrreg (c_win, c_top, c_bot);
  RETURNTF (ret);
}

/* Set the virtual screen cursor to y, x.  This is an obscure and
   pointless feature. */
SCM
gucu_setsyx (SCM y, SCM x)
{
  int c_y, c_x;

  c_y = scm_to_int (y);
  c_x = scm_to_int (x);

  setsyx (c_y, c_x);

  return SCM_UNSPECIFIED;
}

/* Switch to a new terminal. */
SCM
gucu_set_term (SCM newterminal)
{
  SCREEN *c_newterminal;

  c_newterminal = _scm_to_screen (newterminal);

  set_term (c_newterminal);

  return SCM_UNSPECIFIED;
}

/* Initialize the color subsystem. */
SCM
gucu_start_color ()
{
  int ret = start_color ();
  RETURNTF (ret);
}

SCM
gucu_subpad (SCM orig, SCM nlines, SCM ncols, SCM begin_y, SCM begin_x)
{
  int c_nlines, c_ncols, c_begin_y, c_begin_x;
  WINDOW *c_orig, *ret;

  c_orig = _scm_to_window (orig);
  c_nlines = scm_to_int (nlines);
  c_ncols = scm_to_int (ncols);
  c_begin_y = scm_to_int (begin_y);
  c_begin_x = scm_to_int (begin_x);

  ret = subpad (c_orig, c_nlines, c_ncols, c_begin_y, c_begin_x);
  if (ret == NULL)
    return SCM_BOOL_F;

  return _scm_from_window (ret);
}

/* Return a pointer to a new window that is within and shares memory with
   and enclosing window */
SCM
gucu_subwin (SCM orig, SCM nlines, SCM ncols, SCM begin_y, SCM begin_x)
{
  int c_nlines, c_ncols, c_begin_y, c_begin_x;
  WINDOW *c_orig, *ret;

  c_orig = _scm_to_window (orig);
  c_nlines = scm_to_int (nlines);
  c_ncols = scm_to_int (ncols);
  c_begin_y = scm_to_int (begin_y);
  c_begin_x = scm_to_int (begin_x);

  ret = subwin (c_orig, c_nlines, c_ncols, c_begin_y, c_begin_x);
  if (ret == NULL)
    return SCM_BOOL_F;

  return _scm_from_window (ret);
}

/* When called with true, wsyncup is called automatically whenever the
   window is changed */
SCM
gucu_syncok_x (SCM win, SCM bf)
{
  WINDOW *c_win;
  bool c_bf;
  int ret;

  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "syncok!");
  SCM_ASSERT (scm_is_bool (bf), bf, SCM_ARG2, "syncok!");

  c_win = _scm_to_window (win);
  c_bf = scm_to_bool (bf);

  ret = syncok (c_win, c_bf);
  if (ret == ERR)
    curs_bad_state_error ("syncok!");

  return SCM_UNSPECIFIED;
}

/* Return all the video attributes supported by the terminal */
SCM
gucu_term_attrs ()
{
#ifdef HAVE_TERM_ATTRS
  attr_t ret;
  SCM s_ret;

  ret = term_attrs ();
  s_ret = _scm_from_attr (ret);
  return s_ret;
#else
  chtype ret;
  SCM s_ret;

  ret = termattrs ();
  if (SIZEOF_CHTYPE == SIZEOF_LONG)
    s_ret = scm_from_ulong ((unsigned long) ret);
  else
    s_ret = scm_from_uint ((unsigned int) ret);

  return s_ret;
#endif
}

/* Return the short name of the terminal */
SCM
gucu_termname ()
{
  char *ret;
  SCM s_ret;

  ret = termname ();
  s_ret = scm_from_locale_string (ret);

  /* No need to free static string returned by termname() */

  return s_ret;
}

/* Set the blocking read timer */
SCM
gucu_timeout_x (SCM win, SCM delay)
{
  WINDOW *c_win;
  int c_delay;

  c_win = _scm_to_window (win);
  c_delay = scm_to_int (delay);

  wtimeout (c_win, c_delay);

  return SCM_UNSPECIFIED;
}

/* Given a file descriptor, se special optimizations looking for line breaks */
SCM
gucu_typeahead_x (SCM fd)
{
  int c_fd, ret;

  SCM_ASSERT (scm_is_integer (fd), fd, SCM_ARG1, "%typeahead");

  c_fd = scm_to_int (fd);

  ret = typeahead (c_fd);
  RETURNTF (ret);
}

/* Returns a character string which is a printable representation of the
   character, ignoring attributes. */
SCM
gucu_unctrl (SCM ch)
{
  SCM_ASSERT (_scm_is_xchar (ch), ch, SCM_ARG1, "%unctrl");
#ifdef HAVE_NCURSESW
  {
    cchar_t *c_ch = _scm_xchar_to_cchar (ch);
    wchar_t *c_wname;
    c_wname = wunctrl (c_ch);
    free (c_ch);
    return _scm_sstring_from_wstring (c_wname);
  }
#else
  {
    chtype c_ch = _scm_xchar_to_chtype (ch);
    return scm_from_locale_string (unctrl (c_ch));
  }
#endif
}

/* Places a char back on the input queue */
SCM
gucu_ungetch (SCM ch)
{
  int ret;

#ifdef HAVE_NCURSESW
  {
    if (SCM_CHARP (ch))
      {
	wchar_t c_ch = _scm_schar_to_wchar (ch);
	ret = unget_wch (c_ch);
      }
    else if (scm_is_integer (ch))
      {
	ret = 0;
      }

  }
#else
  {
    if (SCM_CHARP (ch))
      {
	char c_ch = _scm_schar_to_char (ch);
	ret = ungetch (c_ch);
      }
    else if (scm_is_integer (ch))
      {
	ret = ungetch (scm_to_uint (ch));
      }
    else
      {
	ret = 0;
      }
  }
#endif
  RETURNTF (ret);
}

/* Assign the terminal default colors to the special color number -1 */
SCM
gucu_use_default_colors ()
{
  RETURNTF (use_default_colors ());
}

/* If set to #f before the call to initscr or newwin, LINES and COLUMNS will
   be gathered from terminfo instead of env */
SCM
gucu_use_env (SCM bf)
{
  bool c_bf;

  SCM_ASSERT (scm_is_bool (bf), bf, SCM_ARG1, "use-env");

  c_bf = scm_to_bool (bf);

  use_env (c_bf);

  return SCM_UNSPECIFIED;
}

/* When false, disable extended names in the terminfo description */
SCM
gucu_use_extended_names (SCM bf)
{
  bool c_bf;

  c_bf = scm_to_bool (bf);

  RETURNTF (use_extended_names (c_bf));
}

/* Put the character ch into the window at its current position, which
   is then advanced. */
SCM
gucu_waddch (SCM win, SCM ch)
{
  WINDOW *c_win;
  int ret;

  c_win = _scm_to_window (win);
#ifdef HAVE_NCURSESW
  {
    cchar_t *c_ch = _scm_xchar_to_cchar (ch);
    ret = wadd_wch (c_win, (const cchar_t *) c_ch);
    free (c_ch);
  }
#else
  {
    chtype c_ch = _scm_xchar_to_chtype (ch);
    ret = waddch (c_win, c_ch);
  }
#endif

  RETURNTF (ret);
}

/* Copy a rendered string in to a window at the current cursor
   positions.  If n is -1, copy until the end of the buffer.
   Otherwise copy N characters. */
SCM
gucu_waddchnstr (SCM win, SCM chstr, SCM n)
{
  WINDOW *c_win;
  int c_n, ret;

  c_win = _scm_to_window (win);
  c_n = scm_to_int (n);

#ifdef HAVE_NCURSESW
  {
    cchar_t *c_str = _scm_xstring_to_cstring (chstr);
    ret = wadd_wchnstr (c_win, c_str, c_n);
    free (c_str);
  }
#else
  {
    chtype *c_chstr = _scm_xstring_to_chstring (chstr);
    ret = waddchnstr (c_win, c_chstr, c_n);
    free (c_chstr);
  }
#endif

  RETURNTF (ret);
}

/* Write the characters of STR to the window. */
SCM
gucu_waddnstr (SCM win, SCM str, SCM n)
{
  WINDOW *c_win;
  int c_n, ret;

  c_win = _scm_to_window (win);
  c_n = scm_to_int (n);
#ifdef HAVE_NCURSESW
  {
    wchar_t *c_str = _scm_sstring_to_wstring (str);
    ret = waddnwstr (c_win, c_str, c_n);
    free (c_str);
  }
#else
  {
    char *c_str = _scm_sstring_to_locale_string (str);
    ret = waddnstr (c_win, c_str, c_n);
    free (c_str);
  }
#endif

  RETURNTF (ret);
}


/* Change the attributes for N characters at the current position */
SCM
gucu_wchgat (SCM win, SCM n, SCM attr, SCM color)
{
  WINDOW *c_win;
  int c_n;
  attr_t c_attr;
  short c_color;
  int ret;

  c_win = _scm_to_window (win);
  c_n = scm_to_int (n);
  c_attr = _scm_to_attr (attr);
  c_color = scm_to_short (color);

  ret = wchgat (c_win, c_n, c_attr, c_color, 0);
  /* param or bad state error */
  RETURNTF (ret);
}


/* Sync the position of the cursor between all the ancestors of win */
SCM
gucu_wcursyncup (SCM win)
{
  WINDOW *c_win;

  c_win = _scm_to_window (win);

  wcursyncup (c_win);
  /* wcursyncup is void */

  return SCM_UNSPECIFIED;
}


/* Delete the character under the cursor */
SCM
gucu_wdelch (SCM win)
{
  WINDOW *c_win;
  int ret;

  c_win = _scm_to_window (win);

  ret = wdelch (c_win);
  RETURNTF (ret);
}

/* Write ch to the given window without advancing the curses.  Refresh
   that character immediately */
SCM
gucu_wechochar (SCM win, SCM ch)
{
  WINDOW *c_win;
  int ret;

  c_win = _scm_to_window (win);
#ifdef HAVE_NCURSESW
  {
    cchar_t *c_ch;
    c_ch = _scm_xchar_to_cchar (ch);
    ret = wecho_wchar (c_win, c_ch);
    free (c_ch);
  }
#else
  {
    chtype c_ch;
    c_ch = _scm_xchar_to_chtype (ch);
    ret = wechochar (c_win, c_ch);
  }
#endif
  /* these could return false if the add goes past the end of a line
     or scrolls a screen that doesn't scroll.  */
  RETURNTF (ret);
}

/* return #t if y,x is enclosed by win */
SCM
gucu_wenclose_p (SCM win, SCM y, SCM x)
{
  WINDOW *c_win;
  int c_y, c_x;
  bool ret;

  c_win = _scm_to_window (win);
  c_y = scm_to_int (y);
  c_x = scm_to_int (x);

  ret = wenclose (c_win, c_y, c_x);

  return scm_from_bool (ret);
}


/* Get a keypress and apply it to the window. Return the keycode. */
SCM
gucu_wgetch (SCM win)
{
  WINDOW *c_win;
  int ret;

  c_win = _scm_to_window (win);

#ifdef HAVE_NCURSESW
  {
    wint_t wch = 0;
    ret = wget_wch (_scm_to_window (win), &wch);

    if (ret == OK)
      {
        return _scm_schar_from_wchar (wch);
      }
    else if (ret == KEY_CODE_YES)
      {
        return scm_from_unsigned_integer (wch);
      }
    else
      {
        return SCM_BOOL_F;
      }
  }
#else
  ret = wgetch (c_win);

  if (ret == ERR)
    return SCM_BOOL_F;
  else if (ret >= KEY_MIN)
    return scm_from_int (ret);
  else
    return _scm_schar_from_char (ret);
#endif
}

/* Draw a horizontal line using the given character that is N
   characters long. */
SCM
gucu_whline (SCM win, SCM ch, SCM n)
{
  WINDOW *c_win;
  int c_n, ret;

  c_win = _scm_to_window (win);
  c_n = scm_to_int (n);

#ifdef HAVE_NCURSESW
  {
    cchar_t *c_ch;
    c_ch = _scm_xchar_to_cchar (ch);
    ret = whline_set (c_win, c_ch, c_n);
    free (c_ch);
  }
#else
  {
    chtype c_ch;
    c_ch = _scm_xchar_to_chtype (ch);
    ret = whline (c_win, c_ch, c_n);
  }
#endif
  if (ret == ERR)
    return SCM_BOOL_F;

  return SCM_UNSPECIFIED;
}

/* Return the character at the cursor position in the given window */
SCM
gucu_winch (SCM win)
{
  WINDOW *c_win;
  SCM ch;

  c_win = _scm_to_window (win);
#ifdef HAVE_NCURSESW
  {
    int ret;
    cchar_t c_ch;
    ret = win_wch (c_win, &c_ch);
    if (ret == ERR)
      return SCM_BOOL_F;
    ch = _scm_xchar_from_cchar (&c_ch);
  }
#else
  {
    chtype c_ch;
    c_ch = winch (c_win);
    ch = _scm_xchar_from_chtype (c_ch);
  }
#endif
  return ch;
}

/* Insert a character before the cursor at a given window */
SCM
gucu_winsch (SCM win, SCM ch)
{
  WINDOW *c_win;
  int ret;

  c_win = _scm_to_window (win);
#ifdef HAVE_NCURSESW
  {
    cchar_t *c_ch = _scm_xchar_to_cchar (ch);
    ret = wins_wch (c_win, c_ch);
    free (c_ch);
  }
#else
  {
    chtype c_ch = _scm_xchar_to_chtype (ch);
    ret = winsch (c_win, c_ch);
  }
#endif
  if (ret == ERR)
    return SCM_BOOL_F;

  return SCM_UNSPECIFIED;
}

/* Insert or delete N lines.  Positive n -> insert.  Negative n -> delete */
SCM
gucu_winsdelln (SCM win, SCM n)
{
  WINDOW *c_win;
  int c_n, ret;

  c_win = _scm_to_window (win);
  c_n = scm_to_int (n);

  ret = winsdelln (c_win, c_n);
  /* winsdelln returns ERR when window == NULL */
  RETURNTF (ret);
}

SCM
gucu_winsnstr (SCM win, SCM str, SCM n)
{
  WINDOW *c_win;
  int c_n, ret;

  c_win = _scm_to_window (win);
  c_n = scm_to_int (n);

#ifdef HAVE_NCURSESW
  {
    wchar_t *c_str = _scm_sstring_to_wstring (str);
    ret = wins_nwstr (c_win, c_str, c_n);
    free (c_str);
  }
#else
  {
    char *c_str = _scm_sstring_to_locale_string (str);
    ret = winsnstr (c_win, c_str, c_n);
    free (c_str);
  }
#endif
  if (ret == ERR)
    return SCM_BOOL_F;

  return SCM_UNSPECIFIED;
}

/* Move the cursor to y, x */
SCM
gucu_wmove (SCM win, SCM y, SCM x)
{
  WINDOW *c_win;
  int c_y, c_x, ret;

  c_win = _scm_to_window (win);
  c_y = scm_to_int (y);
  c_x = scm_to_int (x);

  ret = wmove (c_win, c_y, c_x);
  RETURNTF (ret);
}


/* Indicate that the lines between beg and end should be redrawn */
SCM
gucu_wredrawln (SCM win, SCM beg, SCM end)
{
  WINDOW *c_win;
  int c_beg, c_end, ret;

  c_win = _scm_to_window (win);
  c_beg = scm_to_int (beg);
  c_end = scm_to_int (end);

  ret = wredrawln (c_win, c_beg, c_end);
  RETURNTF (ret);
}


/* Touch all locations in ancestors of a window that are touched in a
   window */
SCM
gucu_wsyncup (SCM win)
{
  WINDOW *c_win;

  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "wsyncup");

  c_win = _scm_to_window (win);

  wsyncup (c_win);

  return SCM_UNSPECIFIED;
}

/* Touch all locations in win that were touched in ancestors of win */
SCM
gucu_wsyncdown (SCM win)
{
  WINDOW *c_win;

  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "wsyncdown");

  c_win = _scm_to_window (win);

  wsyncdown (c_win);

  return SCM_UNSPECIFIED;
}

/* Set the lines from y to y+s as either changed (#t) or unchanged (#f) */
SCM
gucu_wtouchln (SCM win, SCM y, SCM n, SCM touched)
{
  WINDOW *c_win;
  int c_y, c_n, ret;
  bool c_touched;

  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "%wtouchln");
  SCM_ASSERT (scm_is_integer (y), y, SCM_ARG2, "%wtouchln");
  SCM_ASSERT (scm_is_integer (n), n, SCM_ARG3, "%wtouchln");
  SCM_ASSERT (scm_is_bool (touched), touched, SCM_ARG4, "%wtouchln");

  c_win = _scm_to_window (win);
  c_y = scm_to_int (y);
  c_n = scm_to_int (n);
  c_touched = scm_to_bool (touched);

  if (c_n < 0)
    scm_out_of_range ("%wtouchln", n);

#ifndef NCURSES_OPAQUE
  if (c_y < 0 || c_y > c_win->_maxy)
    scm_out_of_range ("%wtouchln", y);
#endif

  ret = wtouchln (c_win, c_y, c_n, c_touched);
  if (ret == ERR)
    curs_bad_state_error ("%wtouchln");

  return SCM_UNSPECIFIED;
}

/* Draw a vertical line with the given character that is N characters
   long */
SCM
gucu_wvline (SCM win, SCM ch, SCM n)
{
  WINDOW *c_win;
  int c_n, ret;

  c_win = _scm_to_window (win);
  c_n = scm_to_int (n);

#ifdef HAVE_NCURSESW
  {
    cchar_t *c_ch;
    c_ch = _scm_xchar_to_cchar (ch);
    ret = wvline_set (c_win, c_ch, c_n);
    free (c_ch);
  }
#else
  {
    chtype c_ch;
    c_ch = _scm_xchar_to_chtype (ch);

    ret = wvline (c_win, c_ch, c_n);
  }
#endif
  if (ret == ERR)
    return SCM_BOOL_F;

  return SCM_UNSPECIFIED;
}

void
gucu_init_function ()
{
  scm_c_define_gsubr ("assume-default-colors", 2, 0, 0,
		      gucu_assume_default_colors);
  scm_c_define_gsubr ("%attr-off!", 2, 0, 0, gucu_attr_off_x);
  scm_c_define_gsubr ("%attr-on!", 2, 0, 0, gucu_attr_on_x);
  scm_c_define_gsubr ("%baudrate", 0, 0, 0, gucu_baudrate);
  scm_c_define_gsubr ("%beep", 0, 0, 0, gucu_beep);
  scm_c_define_gsubr ("%bkgd", 2, 0, 0, gucu_bkgd);
  scm_c_define_gsubr ("%bkgdset!", 2, 0, 0, gucu_bkgdset_x);
  scm_c_define_gsubr ("%border", 9, 0, 0, gucu_border);
  scm_c_define_gsubr ("%can-change-color?", 0, 0, 0, gucu_can_change_color_p);
  scm_c_define_gsubr ("%cbreak!", 0, 0, 0, gucu_cbreak);
  scm_c_define_gsubr ("%clear", 1, 0, 0, gucu_clear);
  scm_c_define_gsubr ("%clearok!", 2, 0, 0, gucu_clearok);
  scm_c_define_gsubr ("%clrtobot", 1, 0, 0, gucu_clrtobot);
  scm_c_define_gsubr ("%clrtoeol", 1, 0, 0, gucu_clrtoeol);
  scm_c_define_gsubr ("color-pair", 1, 0, 0, gucu_COLOR_PAIR);
  scm_c_define_gsubr ("%color-set!", 2, 0, 0, gucu_color_set);
  scm_c_define_gsubr ("%copywin", 9, 0, 0, gucu_copywin);
  scm_c_define_gsubr ("%curs-set", 1, 0, 0, gucu_curs_set);
  scm_c_define_gsubr ("%curses-version", 0, 0, 0, gucu_curses_version);
  scm_c_define_gsubr ("%def-prog-mode", 0, 0, 0, gucu_def_prog_mode);
  scm_c_define_gsubr ("%def-shell-mode", 0, 0, 0, gucu_def_shell_mode);
  scm_c_define_gsubr ("define-key", 2, 0, 0, gucu_define_key);
  scm_c_define_gsubr ("delay-output", 1, 0, 0, gucu_delay_output);
  scm_c_define_gsubr ("%delscreen", 1, 0, 0, gucu_delscreen);
  scm_c_define_gsubr ("%derwin", 5, 0, 0, gucu_derwin);
  scm_c_define_gsubr ("%doupdate", 0, 0, 0, gucu_doupdate);
  scm_c_define_gsubr ("%dupwin", 1, 0, 0, gucu_dupwin);
  scm_c_define_gsubr ("%echo!", 0, 0, 0, gucu_echo);
  scm_c_define_gsubr ("%endwin", 0, 0, 0, gucu_endwin);
  scm_c_define_gsubr ("%erase", 1, 0, 0, gucu_erase);
  scm_c_define_gsubr ("%erasechar", 0, 0, 0, gucu_erasechar);
  scm_c_define_gsubr ("%filter", 0, 0, 0, gucu_filter);
  scm_c_define_gsubr ("%flash", 0, 0, 0, gucu_flash);
  scm_c_define_gsubr ("flushinp", 0, 0, 0, gucu_flushinp);
  scm_c_define_gsubr ("%getbkgd", 1, 0, 0, gucu_getbkgd);
  scm_c_define_gsubr ("%halfdelay!", 1, 0, 0, gucu_halfdelay);
  scm_c_define_gsubr ("%has-colors?", 0, 0, 0, gucu_has_colors_p);
  scm_c_define_gsubr ("%has-ic?", 0, 0, 0, gucu_has_ic_p);
  scm_c_define_gsubr ("%has-il?", 0, 0, 0, gucu_has_il_p);
  scm_c_define_gsubr ("%has-key?", 1, 0, 0, gucu_has_key_p);
  scm_c_define_gsubr ("%has-mouse?", 0, 0, 0, gucu_has_mouse_p);
  scm_c_define_gsubr ("%idcok!", 2, 0, 0, gucu_idcok_x);
  scm_c_define_gsubr ("%idlok!", 2, 0, 0, gucu_idlok_x);
  scm_c_define_gsubr ("%immedok!", 2, 0, 0, gucu_immedok_x);
  scm_c_define_gsubr ("%initscr", 0, 0, 0, gucu_initscr);
  scm_c_define_gsubr ("init-color!", 4, 0, 0, gucu_init_color);
  scm_c_define_gsubr ("%init-pair!", 3, 0, 0, gucu_init_pair);
  scm_c_define_gsubr ("%intrflush!", 1, 0, 0, gucu_intrflush);
  scm_c_define_gsubr ("%isendwin?", 0, 0, 0, gucu_isendwin_p);
  scm_c_define_gsubr ("%is-cleared?", 1, 0, 0, gucu_is_cleared_p);
  scm_c_define_gsubr ("%is-idcok?", 1, 0, 0, gucu_is_idcok_p);
  scm_c_define_gsubr ("%is-idlok?", 1, 0, 0, gucu_is_idlok_p);
  scm_c_define_gsubr ("%is-immedok?", 1, 0, 0, gucu_is_immedok_p);
  scm_c_define_gsubr ("%is-keypad?", 1, 0, 0, gucu_is_keypad_p);
  scm_c_define_gsubr ("%is-leaveok?", 1, 0, 0, gucu_is_leaveok_p);
  scm_c_define_gsubr ("%is-nodelay?", 1, 0, 0, gucu_is_nodelay_p);
  scm_c_define_gsubr ("%is-notimeout?", 1, 0, 0, gucu_is_notimeout_p);
#if defined(HAVE_IS_PAD) || ! defined(NCURSES_OPAQUE)
  scm_c_define_gsubr ("%is-pad?", 1, 0, 0, gucu_is_pad_p);
#endif
  scm_c_define_gsubr ("%is-scrollok?", 1, 0, 0, gucu_is_scrollok_p);
#if defined(HAVE_IS_SUBWIN) || ! defined(NCURSES_OPAQUE)
  scm_c_define_gsubr ("%is-subwin?", 1, 0, 0, gucu_is_subwin_p);
#endif
  scm_c_define_gsubr ("%is-syncok?", 1, 0, 0, gucu_is_syncok_p);
  scm_c_define_gsubr ("is-linetouched?", 2, 0, 0, gucu_is_linetouched_p);
  scm_c_define_gsubr ("is-wintouched?", 1, 0, 0, gucu_is_wintouched_p);
  scm_c_define_gsubr ("key-f", 1, 0, 0, gucu_KEY_F);
#ifdef HAVE_KEY_DEFINED
  scm_c_define_gsubr ("key-defined", 1, 0, 0, gucu_key_defined);
#endif
  scm_c_define_gsubr ("keyname", 1, 0, 0, gucu_keyname);
  scm_c_define_gsubr ("keypad!", 2, 0, 0, gucu_keypad_x);
  scm_c_define_gsubr ("%killchar", 0, 0, 0, gucu_killchar);
  scm_c_define_gsubr ("%leaveok!", 2, 0, 0, gucu_leaveok_x);
  scm_c_define_gsubr ("%longname", 0, 0, 0, gucu_longname);
  scm_c_define_gsubr ("%meta!", 1, 0, 0, gucu_meta_x);
  scm_c_define_gsubr ("mouseinterval", 1, 0, 0, gucu_mouseinterval);
  scm_c_define_gsubr ("mvcur", 4, 0, 0, gucu_mvcur);
  scm_c_define_gsubr ("%mvderwin", 3, 0, 0, gucu_mvderwin);
  scm_c_define_gsubr ("%mvwin", 3, 0, 0, gucu_mvwin);
  scm_c_define_gsubr ("%napms", 1, 0, 0, gucu_napms);
  scm_c_define_gsubr ("%newpad", 2, 0, 0, gucu_newpad);
  scm_c_define_gsubr ("%newwin", 4, 0, 0, gucu_newwin);
  scm_c_define_gsubr ("%nl!", 0, 0, 0, gucu_nl);
  scm_c_define_gsubr ("%nocbreak!", 0, 0, 0, gucu_nocbreak);
  scm_c_define_gsubr ("%nodelay!", 2, 0, 0, gucu_nodelay_x);
  scm_c_define_gsubr ("%noecho!", 0, 0, 0, gucu_noecho);
  scm_c_define_gsubr ("%nonl!", 0, 0, 0, gucu_nonl);
  scm_c_define_gsubr ("%noqiflush!", 0, 0, 0, gucu_noqiflush);
  scm_c_define_gsubr ("%noraw!", 0, 0, 0, gucu_noraw);
  scm_c_define_gsubr ("%notimeout!", 2, 0, 0, gucu_notimeout_x);
  scm_c_define_gsubr ("%noutrefresh", 1, 0, 0, gucu_noutrefresh);
  scm_c_define_gsubr ("%overlay", 2, 0, 0, gucu_overlay);
  scm_c_define_gsubr ("%overwrite", 2, 0, 0, gucu_overwrite);
  scm_c_define_gsubr ("pair-number", 1, 0, 0, gucu_PAIR_NUMBER);
  scm_c_define_gsubr ("%pechochar", 2, 0, 0, gucu_pechochar);
  scm_c_define_gsubr ("%pnoutrefresh", 7, 0, 0, gucu_pnoutrefresh);
  scm_c_define_gsubr ("%prefresh", 7, 0, 0, gucu_prefresh);
  scm_c_define_gsubr ("%qiflush!", 0, 0, 0, gucu_qiflush);
  scm_c_define_gsubr ("%raw!", 0, 0, 0, gucu_raw);
  scm_c_define_gsubr ("%redrawwin", 1, 0, 0, gucu_redrawwin);
  scm_c_define_gsubr ("%refresh", 1, 0, 0, gucu_refresh);
  scm_c_define_gsubr ("%reset-prog-mode", 0, 0, 0, gucu_reset_prog_mode);
  scm_c_define_gsubr ("%reset-shell-mode", 0, 0, 0, gucu_reset_shell_mode);
  scm_c_define_gsubr ("%resetty", 0, 0, 0, gucu_resetty);
  scm_c_define_gsubr ("resizeterm", 2, 0, 0, gucu_resizeterm);
  scm_c_define_gsubr ("%savetty", 0, 0, 0, gucu_savetty);
  scm_c_define_gsubr ("%scr-dump", 1, 0, 0, gucu_scr_dump);
  scm_c_define_gsubr ("%scr-init", 1, 0, 0, gucu_scr_init);
  scm_c_define_gsubr ("scrollok!", 2, 0, 0, gucu_scrollok_x);
  scm_c_define_gsubr ("%scr-restore", 1, 0, 0, gucu_scr_restore);
  scm_c_define_gsubr ("%scr-set", 1, 0, 0, gucu_scr_set);
  scm_c_define_gsubr ("%scrl", 2, 0, 0, gucu_scrl);
  scm_c_define_gsubr ("%set-term", 1, 0, 0, gucu_set_term);
  scm_c_define_gsubr ("%setscrreg!", 3, 0, 0, gucu_setscrreg_x);
  scm_c_define_gsubr ("%setsyx", 2, 0, 0, gucu_setsyx);
  scm_c_define_gsubr ("%start-color!", 0, 0, 0, gucu_start_color);
  scm_c_define_gsubr ("%subpad", 5, 0, 0, gucu_subpad);
  scm_c_define_gsubr ("%subwin", 5, 0, 0, gucu_subwin);
  scm_c_define_gsubr ("syncok!", 2, 0, 0, gucu_syncok_x);
  scm_c_define_gsubr ("%term-attrs", 0, 0, 0, gucu_term_attrs);
  scm_c_define_gsubr ("%termname", 0, 0, 0, gucu_termname);
  scm_c_define_gsubr ("%timeout!", 2, 0, 0, gucu_timeout_x);
  scm_c_define_gsubr ("%typeahead!", 1, 0, 0, gucu_typeahead_x);
  scm_c_define_gsubr ("%unctrl", 1, 0, 0, gucu_unctrl);
  scm_c_define_gsubr ("%ungetch", 1, 0, 0, gucu_ungetch);
  scm_c_define_gsubr ("use-default-colors", 0, 0, 0, gucu_use_default_colors);
  scm_c_define_gsubr ("use-env", 1, 0, 0, gucu_use_env);
  scm_c_define_gsubr ("%use-extended-names", 1, 0, 0, gucu_use_extended_names);
  scm_c_define_gsubr ("%waddch", 2, 0, 0, gucu_waddch);
  scm_c_define_gsubr ("%waddchnstr", 3, 0, 0, gucu_waddchnstr);
  scm_c_define_gsubr ("%waddnstr", 3, 0, 0, gucu_waddnstr);
  scm_c_define_gsubr ("%wchgat", 4, 0, 0, gucu_wchgat);
  scm_c_define_gsubr ("%wcursyncup", 1, 0, 0, gucu_wcursyncup);
  scm_c_define_gsubr ("%wdelch", 1, 0, 0, gucu_wdelch);
  scm_c_define_gsubr ("%wechochar", 2, 0, 0, gucu_wechochar);
  scm_c_define_gsubr ("%wenclose?", 3, 0, 0, gucu_wenclose_p);
  scm_c_define_gsubr ("%wgetch", 1, 0, 0, gucu_wgetch);
  scm_c_define_gsubr ("%whline", 3, 0, 0, gucu_whline);
  scm_c_define_gsubr ("%winch", 1, 0, 0, gucu_winch);
  scm_c_define_gsubr ("%winsch", 2, 0, 0, gucu_winsch);
  scm_c_define_gsubr ("%winsdelln", 2, 0, 0, gucu_winsdelln);
  scm_c_define_gsubr ("%winsnstr", 3, 0, 0, gucu_winsnstr);
  scm_c_define_gsubr ("%wmove", 3, 0, 0, gucu_wmove);
  scm_c_define_gsubr ("%wredrawln", 3, 0, 0, gucu_wredrawln);
  scm_c_define_gsubr ("wsyncup", 1, 0, 0, gucu_wsyncup);
  scm_c_define_gsubr ("wsyncdown", 1, 0, 0, gucu_wsyncdown);
  scm_c_define_gsubr ("%wtouchln", 4, 0, 0, gucu_wtouchln);
  scm_c_define_gsubr ("%wvline", 3, 0, 0, gucu_wvline);
}
