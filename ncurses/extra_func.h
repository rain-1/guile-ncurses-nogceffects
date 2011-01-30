/*
  extra_func.h

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

#ifndef EXTRA_FUNC_H
#define EXTRA_FUNC_H

#include <libguile.h>
#include "visibility.h"

GUCU_API SCM gucu_cfgetispeed (SCM termios);
GUCU_API SCM gucu_cfgetospeed (SCM termios);
GUCU_API SCM gucu_cfmakeraw_x (SCM termios);
GUCU_API SCM gucu_cfsetispeed_x (SCM termios, SCM speed);
GUCU_API SCM gucu_cfsetospeed_x (SCM termios, SCM speed);
GUCU_API SCM gucu_cfsetspeed_x (SCM termios, SCM speed);
#ifdef HAVE_GRANTPT
GUCU_API SCM gucu_grantpt (SCM fd);
#endif
#ifdef HAVE_PTSNAME
GUCU_API SCM gucu_ptsname (SCM fd);
#endif
GUCU_API SCM gucu_ptsmakeraw (SCM fd);
GUCU_API SCM gucu_tcdrain (SCM fd);
GUCU_API SCM gucu_tcflow (SCM fd, SCM action);
GUCU_API SCM gucu_tcflush (SCM fd, SCM queue);
GUCU_API SCM gucu_tcgetattr (SCM fd);
GUCU_API SCM gucu_tcgetsid (SCM fd);
GUCU_API SCM gucu_tcsendbreak (SCM fd, SCM duration);
GUCU_API SCM gucu_tcsetattr_x (SCM fd, SCM opt, SCM termios);
GUCU_API SCM gucu_strwidth (SCM x);
GUCU_API SCM gucu_termios_iflag (SCM termios);
GUCU_API SCM gucu_termios_oflag (SCM termios);
GUCU_API SCM gucu_termios_cflag (SCM termios);
GUCU_API SCM gucu_termios_lflag (SCM termios);
GUCU_API SCM gucu_termios_line (SCM termios);
GUCU_API SCM gucu_termios_cc (SCM termios, SCM c);
GUCU_API SCM gucu_termios_iflag_set_x (SCM termios, SCM val);
GUCU_API SCM gucu_termios_oflag_set_x (SCM termios, SCM val);
GUCU_API SCM gucu_termios_cflag_set_x (SCM termios, SCM val);
GUCU_API SCM gucu_termios_lflag_set_x (SCM termios, SCM val);
GUCU_API SCM gucu_termios_cc_set_x (SCM termios, SCM pos, SCM cc);
#ifdef HAVE_UNLOCKPT
GUCU_API SCM gucu_unlockpt (SCM fd);
#endif

GUCU_LOCAL void gucu_extra_init_function (void);
#endif
