/*
  panel_type.c

  Copyright 2009, 2010 Free Software Foundation, Inc.

  This file is part of Guile-Ncurses.

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

#define _GNU_SOURCE
#include <assert.h>
#include <curses.h>
#include <libguile.h>
#include <libintl.h>
#include <panel.h>
#include <stdio.h>
#include <string.h>

#include "compat.h"
#include "panel_type.h"
#include "type.h"

scm_t_bits panel_tag;

SCM equalp_panel (SCM x1, SCM x2);
size_t gc_free_panel (SCM x);
SCM mark_panel (SCM x);
int print_panel (SCM x, SCM port, scm_print_state * pstate);


/* panel -- in C, a PANEL.  In Scheme, a smob that contains the pointer */

int
_scm_is_panel (SCM x)
{
  if (SCM_SMOB_PREDICATE (panel_tag, x))
    {
      if (SCM_SMOB_DATA (x) == 0)
	return 0;
      else
	return 1;
    }
  else
    return 0;
}

PANEL *
_scm_to_panel (SCM x)
{
  struct gucu_panel *gp;

  scm_assert_smob_type (panel_tag, x);

  gp = (struct gucu_panel *) SCM_SMOB_DATA (x);

  return (PANEL *) gp->panel;
}

/* Since a panel needs to carry around a guardian of the window used
   to create it, it is not possible to have an scm_from_panel without
   some extra lookup table */
#if 0
SCM
_scm_from_panel (PANEL * x)
{
  SCM s_panel;

  assert (x != NULL);

  SCM_NEWSMOB (s_panel, panel_tag, x);

  assert (x == (PANEL *) SCM_SMOB_DATA (s_panel));

  if (0)
    {
      fprintf (stderr, gettext ("Making smob from panel based on WINDOW * %p\n"),
	       x->win);
    }

  return (s_panel);
}
#endif

// Panels are equal if they point to the same C structure
SCM
equalp_panel (SCM x1, SCM x2)
{
  PANEL *panel1 = (PANEL *) _scm_to_panel (x1);
  PANEL *panel2 = (PANEL *) _scm_to_panel (x2);

  if ((panel1 == NULL) || (panel2 == NULL))
    return SCM_BOOL_F;
  else if ((panel1 != panel2))
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

SCM
mark_panel (SCM x)
{
  struct gucu_panel *gp;

  scm_assert_smob_type (panel_tag, x);

  gp = (struct gucu_panel *) SCM_SMOB_DATA (x);

  scm_gc_mark (gp->window);

  return gp->win_guard;
}

/* The curses primitive that frees memory is called del_panel. Note
   that del_panel doesn't free the underlying window. */
size_t
gc_free_panel (SCM x)
{
  struct gucu_panel *gp;
  int retval;

  scm_assert_smob_type (panel_tag, x);

  gp = (struct gucu_panel *) SCM_SMOB_DATA (x);

  assert (gp != NULL);

  retval = del_panel (gp->panel);
  if (retval != OK)
    {
      scm_error_scm (SCM_BOOL_F,
		     scm_from_locale_string ( gettext ("garbage collection of panel")),
		     scm_from_locale_string ( gettext ("bad argument")),
		     SCM_BOOL_F, SCM_BOOL_F);
    }

  /* Release scheme objects from the guardians */
  while (scm_is_true (scm_call_0 (gp->win_guard)))
    ;

  SCM_SET_SMOB_DATA (x, NULL);

  return 0;
}

SCM
gucu_del_panel (SCM x)
{
  SCM_ASSERT (_scm_is_panel (x), x, SCM_ARG1, "del-panel");
  gc_free_panel (x);

  return SCM_UNSPECIFIED;
}

int
print_panel (SCM x, SCM port, scm_print_state * pstate UNUSED)
{
  PANEL *pnl = (PANEL *) _scm_to_panel (x);
  char *str;

  scm_puts ("#<panel ", port);

  if (pnl == NULL)
    scm_puts ( gettext ("(freed)"), port);
  else
    {
      if (asprintf (&str, "%p", (void *) pnl) < 0)
	scm_puts ("???", port);
      else
	scm_puts (str, port);
    }

  scm_puts (">", port);

  // non-zero means success
  return 1;
}

SCM
gucu_is_panel_p (SCM x)
{
  return scm_from_bool (_scm_is_panel (x));
}


SCM
gucu_new_panel (SCM win)
{
  struct gucu_panel *gp;
  SCM smob;

  /* Step 0: Check input list */
  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "new-panel");

  /* Step 1: Allocate memory */
  gp = scm_gc_malloc (sizeof (struct gucu_panel), "gucu_panel");

  /* Step 2: initialize it with C code */

  /* Step 3: create the smob */
  SCM_NEWSMOB (smob, panel_tag, gp);

  /* Step 4: finish the initialization */
  gp->panel = new_panel (_scm_to_window (win));
  if (gp->panel == NULL)
    {
      scm_misc_error ("new-panel", gettext ("bad window"), SCM_BOOL_F);
    }
  gp->window = win;

#ifndef GUILE_1_POINT_6
  gp->win_guard = scm_make_guardian ();
#else
  gp->win_guard = scm_make_guardian (SCM_BOOL_F);
#endif

  scm_call_1 (gp->win_guard, win);

  return smob;
}


void
gucu_panel_init_type ()
{
  panel_tag = scm_make_smob_type ("panel", sizeof (PANEL *));
  scm_set_smob_mark (panel_tag, mark_panel);
  scm_set_smob_free (panel_tag, gc_free_panel);
  scm_set_smob_print (panel_tag, print_panel);
  scm_set_smob_equalp (panel_tag, equalp_panel);
  scm_c_define_gsubr ("panel?", 1, 0, 0, gucu_is_panel_p);

  scm_c_define_gsubr ("new-panel", 1, 0, 0, gucu_new_panel);
  scm_c_define_gsubr ("del-panel", 1, 0, 0, gucu_del_panel);
}
