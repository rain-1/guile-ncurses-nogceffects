/*
  slk_func.c

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

#ifndef FUNCTION_H
#define FUNCTION_H

#include <libguile.h>
#include "visibility.h"

GUCU_API SCM gucu_slk_attr (void);
GUCU_API SCM gucu_slk_attr_off_x (SCM arg1);
GUCU_API SCM gucu_slk_attr_on_x (SCM arg1);
GUCU_API SCM gucu_slk_attr_set_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_slk_attrset (SCM arg1);
GUCU_API SCM gucu_slk_clear (void);
GUCU_API SCM gucu_slk_color (SCM arg1);
GUCU_API SCM gucu_slk_color_x (SCM color_pair_number);
GUCU_API SCM gucu_slk_init (SCM arg1);
GUCU_API SCM gucu_slk_label (SCM arg1);
GUCU_API SCM gucu_slk_noutrefresh (void);
GUCU_API SCM gucu_slk_refresh (void);
GUCU_API SCM gucu_slk_restore (void);
GUCU_API SCM gucu_slk_set (SCM arg1, SCM arg2, SCM arg3);
GUCU_API SCM gucu_slk_touch (void);

GUCU_API void gucu_slk_init_function (void);
#endif
