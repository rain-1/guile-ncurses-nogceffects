/*
  termios_main.c

  Copyright 2010 Free Software Foundation, Inc.

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

#include "termios_func.h"
#include "termios_type.h"
#include "termios_const.h"
#include "visibility.h"

GUCU_API void gucu_termios_init (void);

void
gucu_termios_init ()
{
  gucu_termios_init_type ();
  gucu_termios_init_const ();
  gucu_termios_init_function ();
}
