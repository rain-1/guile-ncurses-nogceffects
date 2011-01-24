/*
  form_type.h

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
#ifndef FORM_TYPE_H
#define FORM_TYPE_H

#include <libguile.h>
#include "visibility.h"

#if HAVE_CURSES_H
#include <form.h>
#endif

#if HAVE_NCURSES_CURSES_H
#include <ncurses/form.h>
#endif

struct gucu_form
{
  // Pointer to the FORM structure
  FORM *form;
  // A scheme list containing FIELD SCMs
  FIELD **c_fields;
  size_t n_fields;
  SCM fields;
  SCM fields_guard;
  // The windows
  SCM win;
  SCM sub;
  // Guardians for the windows
  SCM win_guard;
  SCM sub_guard;
};


GUCU_LOCAL int _scm_is_field (SCM x);
GUCU_LOCAL FIELD *_scm_to_field (SCM x);
GUCU_LOCAL SCM _scm_from_field (FIELD * x);
GUCU_LOCAL int _scm_is_form (SCM x);
GUCU_LOCAL FORM *_scm_to_form (SCM x);

GUCU_API SCM gucu_new_field (SCM height, SCM width, SCM top, SCM left,
			     SCM offscreen, SCM nbuffers);
GUCU_API SCM gucu_is_field_p (SCM x);
GUCU_API SCM gucu_is_form_p (SCM x);
GUCU_API SCM gucu_new_form (SCM fields);
GUCU_API SCM gucu_form_fields (SCM form);
GUCU_API SCM gucu_set_form_fields_x (SCM form, SCM fields);

GUCU_LOCAL void gucu_form_init_type (void);

#endif
