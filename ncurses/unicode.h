/*
  unicode.h

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

#ifndef UNICODE_H
#define UNICODE_H

#include "gucuconfig.h"

#ifdef GUILE_CHARS_ARE_UCS4
#include <stddef.h>
#include <stdint.h>
#include "visibility.h"

GUCU_LOCAL int locale_char_to_codepoint (char c, uint32_t * p_codepoint);
GUCU_LOCAL int wchar_to_codepoint (wchar_t c, uint32_t * p_codepoint);
GUCU_LOCAL int codepoint_to_locale_char (uint32_t codepoint, char *p_c);
GUCU_LOCAL int codepoint_to_wchar (uint32_t codepoint, wchar_t * p_c);
#endif /* GUILE_CHARS_ARE_UCS4 */

#endif
