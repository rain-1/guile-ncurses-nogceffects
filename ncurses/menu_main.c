/*
  menu_main.c

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

#include <config.h>
#include "menu_func.h"
#include "menu_spec.h"
#include "menu_const.h"
#include "menu_type.h"
#include "visibility.h"

GUCU_API void gucu_menu_init (void);

void
gucu_menu_init ()
{
  gucu_menu_init_type ();
  gucu_menu_init_function ();
  gucu_menu_init_special ();
  gucu_menu_init_constant ();
}
