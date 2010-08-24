/*
  panel_type.h

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
#ifndef PANEL_TYPE_H
#define PANEL_TYPE_H

#include <libguile.h>
#include <panel.h>
#include "visibility.h"

struct gucu_panel
{
  /* Pointer to the panel */
  PANEL *panel;

  /* The window from which the panel was generated */
  SCM window;

  /* Guardians to hold the window SCM objects */
  SCM win_guard;
};

GUCU_LOCAL extern scm_t_bits panel_tag;

GUCU_LOCAL int _scm_is_panel (SCM x);
GUCU_LOCAL PANEL *_scm_to_panel (SCM x);
GUCU_LOCAL SCM _scm_from_panel (PANEL * x);

GUCU_API SCM gucu_is_panel_p (SCM x);
GUCU_API SCM gucu_new_panel (SCM arg1);
GUCU_API SCM gucu_del_panel (SCM arg1);

GUCU_LOCAL void gucu_panel_init_type (void);

#endif
