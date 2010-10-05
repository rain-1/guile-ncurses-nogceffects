/*
  extra_type.h

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
#ifndef EXTRA_TYPE_H
#define EXTRA_TYPE_H

#include <libguile.h>
#include "visibility.h"
#include <termios.h>

GUCU_LOCAL extern scm_t_bits termios_tag;

GUCU_LOCAL int _scm_is_termios (SCM x);
GUCU_LOCAL struct termios *_scm_to_termios (SCM x);
GUCU_LOCAL SCM _scm_from_termios (struct termios *x);

GUCU_API SCM gucu_is_termios_p (SCM x);
GUCU_API SCM gucu_new_termios (void);
GUCU_API SCM gucu_del_termios (SCM arg1);

GUCU_LOCAL void gucu_extra_init_type (void);

#endif
