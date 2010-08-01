/*
Copyright 2010 Free Software Foundation, Inc.

This file is part of Guile-Ncurses.

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
#include <string.h>

#include "compat.h"
#include "gucuconfig.h"

#ifdef GUILE_1_POINT_6

void
scm_assert_smob_type (scm_t_bits tag, SCM val)
{
  SCM_ASSERT (SCM_SMOB_PREDICATE (tag, val), val, 0, "scm_assert_smob_type");
}

int
scm_c_string_length (SCM x)
{
  return SCM_STRING_LENGTH (x);
}

SCM
scm_c_string_ref (SCM x, size_t k)
{
  return scm_string_ref (x, scm_int2num (k));
}

SCM
scm_from_bool (int x)
{
  return SCM_BOOL (x);
}

SCM
scm_from_int (int x)
{
  return scm_int2num (x);
}

SCM
scm_from_locale_string (const char *str)
{
  return scm_makfrom0str (str);
}

SCM
scm_from_locale_symbol (const char *sym)
{
  return scm_string_to_symbol (scm_makfrom0str (sym));
}

SCM
scm_from_short (short x)
{
  return scm_short2num (x);
}

SCM
scm_from_uint (unsigned int x)
{
  return scm_uint2num (x);
}

SCM
scm_from_ulong (unsigned long x)
{
  return scm_ulong2num (x);
}

void *
scm_gc_malloc (size_t size, const char *what)
{
  return scm_must_malloc (size, what);
}

int
scm_is_bool (SCM x)
{
  return SCM_BOOLP (x);
}

int
scm_is_integer (SCM x)
{
  return SCM_NFALSEP (scm_integer_p (x));
}

int
scm_is_number (SCM x)
{
  return SCM_NUMBERP (x);
}

int
scm_is_string (SCM x)
{
  return SCM_STRINGP (x);
}

int
scm_is_symbol (SCM x)
{
  return SCM_SYMBOLP (x);
}

int
scm_is_true (SCM x)
{
  return SCM_NFALSEP (x);
}

void *
scm_malloc (size_t x)
{
  return scm_must_malloc (x, "memory");
}

int
scm_to_bool (SCM x)
{
  return SCM_NFALSEP (x);
}

int
scm_to_int (SCM x)
{
  return scm_num2int (x, 0, "scm_to_int");
}

char *
scm_to_locale_string (SCM x)
{
  size_t len;
  char *str;

  len = SCM_STRING_LENGTH (x);
  str = (char *) malloc (len + 1);
  if (str == NULL)
    return NULL;
  memcpy (str, SCM_VELTS (x), len);
  str[len] = '\0';
  return str;
}

short
scm_to_short (SCM x)
{
  return scm_num2short (x, 0, "scm_to_short");
}

size_t
scm_to_size_t (SCM x)
{
  return scm_num2size (x, 0, "scm_to_size_t");
}

unsigned int
scm_to_uint (SCM x)
{
  return scm_num2uint (x, 0, "scm_to_uint");
}

unsigned int
scm_to_uint8 (SCM x)
{
  unsigned int val = scm_num2uint (x, 0, "scm_to_uint");
  if (val > 0xFF)
    scm_out_of_range ("scm_to_uint8", x);
  return val;
}

unsigned long
scm_to_ulong (SCM x)
{
  return scm_num2ulong (x, 0, "scm_to_ulong");
}

#endif
