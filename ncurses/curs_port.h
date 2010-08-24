/*
curs_port.h

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

#ifndef CURS_PORT_H
#define CURS_PORT_H 1

#include <config.h>

#include <libguile.h>

#include "visibility.h"

#ifdef HAVE_FOPENCOOKIE
GUCU_API SCM gucu_newterm (SCM a, SCM b, SCM c);
#endif
GUCU_API SCM gucu_getwin (SCM port);
GUCU_API SCM gucu_putwin (SCM win, SCM port);

GUCU_LOCAL void gucu_init_port (void);

#endif /* not CURS_PORT_H */
