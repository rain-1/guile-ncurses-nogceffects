/*
  menu_spec.h

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

#ifndef MENU_SPEC_H
#define MENU_SPEC_H

#include <libguile.h>
#include "visibility.h"

GUCU_API SCM gucu_scale_menu (SCM menu);
GUCU_API SCM gucu_menu_spacing (SCM menu);
GUCU_API SCM gucu_menu_itemlen (SCM menu);
GUCU_API SCM gucu_menu_format (SCM menu);

GUCU_LOCAL void gucu_menu_init_special (void);

#endif
