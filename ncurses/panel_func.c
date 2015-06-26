/*
  panel_func.c

  Copyright 2009, 2010, 2014 Free Software Foundation, Inc.

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

#include <assert.h>
#include <libguile.h>

#if HAVE_CURSES_H
#include <curses.h>
#include <panel.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#include <ncurses/panel.h>
#elif HAVE_NCURSESW_CURSES_H
#include <ncursesw/curses.h>
#include <ncursesw/panel.h>
#else
#error "No panel.h file included"
#endif

#include "compat.h"
#include "panel_func.h"
#include "panel_type.h"
#include "type.h"

static SCM
gucu_xxx_panel (SCM p, int (*func) (PANEL *), const char *funcname)
{
  PANEL *c_p;
  int ret;

  SCM_ASSERT (_scm_is_panel (p), p, SCM_ARG1, funcname);

  c_p = _scm_to_panel (p);
  ret = func (c_p);

  /* This should never fail */
  assert (ret == OK);

  return SCM_UNSPECIFIED;
}

/* Puts panel P at the bottom of all panels */
SCM
gucu_bottom_panel (SCM p)
{
  return gucu_xxx_panel (p, bottom_panel, "bottom-panel");
}

/* Puts panel P at the top of all panels */
SCM
gucu_top_panel (SCM p)
{
  return gucu_xxx_panel (p, top_panel, "top-panel");
}

/* Show a hidden panel by putting it on top */
SCM
gucu_show_panel (SCM p)
{
  return gucu_xxx_panel (p, show_panel, "show-panel");
}

/* Refresh the virtual screen */
SCM
gucu_update_panels ()
{
  update_panels ();

  return SCM_UNSPECIFIED;
}

/* Remove the panel from the stack */
SCM
gucu_hide_panel (SCM p)
{
  return gucu_xxx_panel (p, hide_panel, "hide-panel");
}

/* Return the underlying window of the panel */
SCM
gucu_panel_window (SCM x)
{
  struct gucu_panel *gp;

  scm_assert_smob_type (panel_tag, x);

  gp = (struct gucu_panel *) SCM_SMOB_DATA (x);

  return gp->window;
}

/* Replaces the current window of a panel with a new window */
SCM
gucu_replace_panel (SCM pan, SCM win)
{
  struct gucu_panel *gp;

  SCM_ASSERT (_scm_is_panel (pan), pan, SCM_ARG1, "replace-panel!");
  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG2, "replace-panel!");

  gp = (struct gucu_panel *) SCM_SMOB_DATA (pan);

  /* Release the guardian on the old window */
  while (scm_is_true (scm_call_0 (gp->win_guard)))
    ;

  /* Guard the new window */
  scm_call_1 (gp->win_guard, win);

  /* Copy the new window into the SMOB */
  gp->window = win;

  /* Update the curses structure */
  replace_panel (gp->panel, _scm_to_window (gp->window));

  return SCM_UNSPECIFIED;
}

SCM
gucu_move_panel (SCM panel, SCM starty, SCM startx)
{
  SCM_ASSERT (_scm_is_panel (panel), panel, SCM_ARG1, "move-panel");
  SCM_ASSERT (scm_is_integer (starty), starty, SCM_ARG2, "move-panel");
  SCM_ASSERT (scm_is_integer (startx), startx, SCM_ARG3, "move-panel");

  PANEL *c_panel = _scm_to_panel (panel);
  int c_starty = scm_to_int (starty);
  int c_startx = scm_to_int (startx);

  int ret = move_panel (c_panel, c_starty, c_startx);
  SCM s_ret = scm_from_int (ret);

  return s_ret;
}

SCM
gucu_panel_hidden_p (SCM panel)
{
  SCM_ASSERT (_scm_is_panel (panel), panel, SCM_ARG1, "panel-hidden?");

  PANEL *c_panel = _scm_to_panel (panel);

  int ret = panel_hidden (c_panel);

  if (ret == TRUE)
    return SCM_BOOL_T;
  else if (ret == FALSE)
    return SCM_BOOL_F;
  else
    scm_misc_error ("panel-hidden?", "bad panel", SCM_BOOL_F);
}


void
gucu_panel_init_function ()
{
  scm_c_define_gsubr ("bottom-panel", 1, 0, 0, gucu_bottom_panel);
  scm_c_define_gsubr ("top-panel", 1, 0, 0, gucu_top_panel);
  scm_c_define_gsubr ("show-panel", 1, 0, 0, gucu_show_panel);
  scm_c_define_gsubr ("update-panels", 0, 0, 0, gucu_update_panels);
  scm_c_define_gsubr ("hide-panel", 1, 0, 0, gucu_hide_panel);
  scm_c_define_gsubr ("panel-window", 1, 0, 0, gucu_panel_window);
  scm_c_define_gsubr ("replace-panel!", 2, 0, 0, gucu_replace_panel);
  scm_c_define_gsubr ("move-panel", 3, 0, 0, gucu_move_panel);
  scm_c_define_gsubr ("panel-hidden?", 1, 0, 0, gucu_panel_hidden_p);
  scm_c_define_gsubr ("del-panel", 1, 0, 0, gucu_del_panel);
}
