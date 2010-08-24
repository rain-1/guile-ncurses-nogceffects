/*
  form_spec.h

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
#ifndef FORM_SPEC_H
#define FORM_SPEC_H

#include <libguile.h>
#include "visibility.h"

GUCU_API SCM gucu_dup_field (SCM field, SCM toprow, SCM leftcol);
GUCU_API SCM gucu_dynamic_field_info (SCM field);
GUCU_API SCM gucu_field_buffer (SCM field, SCM buffer);
GUCU_API SCM gucu_field_info (SCM field);
GUCU_API SCM gucu_field_type (SCM field);
GUCU_API SCM gucu_link_field (SCM field, SCM x, SCM y);
GUCU_API SCM gucu_scale_form (SCM form);
GUCU_API SCM gucu_set_field_type_x (SCM field, SCM type, SCM a, SCM b, SCM c);

GUCU_LOCAL void gucu_form_init_special (void);

#endif
