/*
curs_main.c

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

#include <libguile.h>
#include <libintl.h>

#if HAVE_CURSES_H
#include <curses.h>
#endif

#if HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#endif

#include "curs_const.h"
#include "curs_func.h"
#include "curs_port.h"
#include "curs_spec.h"
#include "type.h"
#include "visibility.h"

GUCU_API void gucu_init (void);

void
gucu_init ()
{
  gucu_init_function ();
  gucu_init_special ();
  gucu_init_constant ();
  gucu_init_port ();
  gucu_init_type ();
}
