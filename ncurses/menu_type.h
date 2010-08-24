/*
  menu_type.h

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

#ifndef MENU_TYPE_H
#define MENU_TYPE_H

#include <libguile.h>
#include <menu.h>
#include "visibility.h"

GUCU_LOCAL int _scm_is_item (SCM x);
GUCU_LOCAL ITEM *_scm_to_item (SCM x);
GUCU_LOCAL SCM _scm_from_item (ITEM * x);

GUCU_API SCM gucu_is_item_p (SCM x);
GUCU_API SCM gucu_new_item (SCM x, SCM y);

struct gucu_menu
{
  // Pointer to the menu
  MENU *menu;

  /* Guardians to hold the items, window, and subwindow SCM objects */
  SCM items_guard;
  SCM win_guard;
  SCM subwin_guard;
};

GUCU_LOCAL int _scm_is_menu (SCM x);
GUCU_LOCAL MENU *_scm_to_menu (SCM x);
GUCU_LOCAL SCM _scm_from_menu (MENU * x);

GUCU_API SCM gucu_is_menu_p (SCM x);
GUCU_API SCM gucu_new_menu (SCM x);

GUCU_LOCAL void gucu_menu_init_type (void);

#endif
