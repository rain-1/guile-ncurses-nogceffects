/*
  panel_func.h

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

#ifndef PANEL_FUNC_H
#define PANEL_FUNC_H

#include <libguile.h>
#include "visibility.h"

GUCU_API SCM gucu_bottom_panel (SCM arg1);
GUCU_API SCM gucu_top_panel (SCM arg1);
GUCU_API SCM gucu_show_panel (SCM arg1);
GUCU_API SCM gucu_update_panels (void);
GUCU_API SCM gucu_hide_panel (SCM arg1);
GUCU_API SCM gucu_panel_window (SCM arg1);
GUCU_API SCM gucu_replace_panel (SCM arg1, SCM arg2);
GUCU_API SCM gucu_move_panel (SCM arg1, SCM arg2, SCM arg3);
GUCU_API SCM gucu_panel_hidden_p (SCM arg1);

GUCU_LOCAL void gucu_panel_init_function (void);
#endif
