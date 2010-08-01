/*
  form_func.h

  Copyright 2009, 2010 Free Software Foundation, Inc.

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

#ifndef FORM_FUNC_H
#define FORM_FUNC_H

#include <libguile.h>
#include "visibility.h"

GUCU_LOCAL void form_bad_state_error (const char *funcname);
GUCU_LOCAL void form_connected_error (const char *funcname);
GUCU_LOCAL void form_current_field_error (const char *funcname);
GUCU_LOCAL void form_invalid_field_error (const char *funcname);
GUCU_LOCAL void form_no_room_error (const char *funcname);
GUCU_LOCAL void form_not_connected_error (const char *funcname);
GUCU_LOCAL void form_not_posted_error (const char *funcname);
GUCU_LOCAL void form_posted_error (const char *funcname);

GUCU_API SCM gucu_data_ahead_p (SCM form);
GUCU_API SCM gucu_data_behind_p (SCM form);
GUCU_API SCM gucu_field_back (SCM arg1);
GUCU_API SCM gucu_field_count (SCM arg1);
GUCU_API SCM gucu_field_fore (SCM arg1);
GUCU_API SCM gucu_field_index (SCM arg1);
GUCU_API SCM gucu_field_just (SCM arg1);
GUCU_API SCM gucu_field_opts (SCM arg1);
GUCU_API SCM gucu_field_opts_off_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_field_opts_on_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_field_pad (SCM arg1);
GUCU_API SCM gucu_field_status_p (SCM arg1);
GUCU_API SCM gucu_form_driver (SCM arg1, SCM arg2);
GUCU_API SCM gucu_form_opts (SCM arg1);
GUCU_API SCM gucu_form_opts_on_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_form_opts_off_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_form_page (SCM arg1);
GUCU_API SCM gucu_form_request_by_name (SCM arg1);
GUCU_API SCM gucu_form_request_name (SCM arg1);
GUCU_API SCM gucu_form_sub (SCM arg1);
GUCU_API SCM gucu_form_win (SCM arg1);
GUCU_API SCM gucu_free_field (SCM arg1);
GUCU_API SCM gucu_free_form (SCM arg1);
GUCU_API SCM gucu_move_field (SCM arg1, SCM arg2, SCM arg3);
GUCU_API SCM gucu_new_page_p (SCM arg1);
GUCU_API SCM gucu_pos_form_cursor (SCM arg1);
GUCU_API SCM gucu_post_form (SCM arg1);
GUCU_API SCM gucu_set_current_field_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_field_back_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_field_buffer_x (SCM arg1, SCM arg2, SCM arg3);
GUCU_API SCM gucu_set_field_fore_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_field_just_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_field_opts_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_field_pad_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_field_status_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_form_opts_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_form_page_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_form_sub_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_form_win_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_max_field_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_new_page_x (SCM arg1, SCM arg2);
GUCU_API SCM gucu_unpost_form (SCM arg1);

GUCU_LOCAL void gucu_form_init_function (void);
#endif
