/*
  gucuconfig.h

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
#ifndef GUCUCONFIG_H
#define GUCUCONFIG_H

#include <config.h>

#include <libguile.h>

/* Macros that describe the features of this build of
   Guile-Ncurses.  */
#ifdef HAVE___ATTRIBUTE__
#define UNUSED __attribute__ ((unused))
#else
#define UNUSED
#endif

/* For 1.6.x, some compatibility functions are required */
#if SCM_MAJOR_VERSION == 1 && SCM_MINOR_VERSION == 6
#define GUILE_1_POINT_6
#else
#undef GUILE_1_POINT_6
#endif

/* After 1.9.4, characters are codepoints.  */
#if SCM_MAJOR_VERSION > 1				 \
  || (SCM_MAJOR_VERSION == 1 && SCM_MINOR_VERSION > 9)	 \
  || (SCM_MAJOR_VERSION == 1 && SCM_MINOR_VERSION == 9 && SCM_MICRO_VERSION >= 4)
#define GUILE_CHARS_ARE_UCS4
#else
#undef GUILE_CHARS_ARE_UCS4
#endif

#endif
