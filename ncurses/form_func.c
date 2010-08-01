/*
  form_func.c

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

#include <config.h>
#include <curses.h>
#include <form.h>
#include <libguile.h>
#include <libintl.h>

#include "compat.h"
#include "form_func.h"
#include "form_type.h"
#include "type.h"

extern scm_t_bits form_tag;
extern scm_t_bits field_tag;

/* Errors */

void
form_bad_state_error (const char *funcname)
{
  scm_misc_error (funcname, gettext ("bad state"), SCM_BOOL_F);
}

void
form_connected_error (const char *funcname)
{
  scm_misc_error (funcname, gettext ("field is already connected to a form"),
		  SCM_BOOL_F);
}

void
form_current_field_error (const char *funcname)
{
  scm_misc_error (funcname, gettext ("the field is the current field"), SCM_BOOL_F);
}

void
form_invalid_field_error (const char *funcname)
{
  scm_misc_error (funcname, gettext ("the field is invalid"), SCM_BOOL_F);
}

void
form_no_room_error (const char *funcname)
{
  scm_misc_error (funcname, gettext ("the form or field can not fit on this screen"),
		  SCM_BOOL_F);
}

void
form_not_connected_error (const char *funcname)
{
  scm_misc_error (funcname, gettext ("the field is not connected to a form"),
		  SCM_BOOL_F);
}

void
form_not_posted_error (const char *funcname)
{
  scm_misc_error (funcname, gettext ("the form is not posted"), SCM_BOOL_F);
}

void
form_posted_error (const char *funcname)
{
  scm_misc_error (funcname, gettext ("the form is posted"), SCM_BOOL_F);
}

/* Tests for off-screen data ahead on the form */
SCM
gucu_data_ahead_p (SCM form)
{
  SCM_ASSERT (_scm_is_form (form), form, SCM_ARG1, "data-ahead?");

  const FORM *c_form = _scm_to_form (form);

  return scm_from_bool (data_ahead (c_form));
}

/* Tests for off-screen data behind on the form */
SCM
gucu_data_behind_p (SCM form)
{
  SCM_ASSERT (_scm_is_form (form), form, SCM_ARG1, "data-behind?");

  const FORM *c_form = _scm_to_form (form);

  return scm_from_bool (data_behind (c_form));
}

/* Returns the background attribute of FIELD */
SCM
gucu_field_back (SCM field)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "field-back");

  const FIELD *c_field = _scm_to_field (field);
  chtype ret = field_back (c_field);
  return _scm_from_chtype (ret);
}

/* Returns the count of fields in FORM */
SCM
gucu_field_count (SCM form)
{
  SCM_ASSERT (_scm_is_form (form), form, SCM_ARG1, "field-count");

  const FORM *c_form = _scm_to_form (form);
  int ret = field_count (c_form);

  return scm_from_int (ret);
}

/* Returns the foreground attribute of FIELD */
SCM
gucu_field_fore (SCM field)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "field-fore");

  const FIELD *c_field = _scm_to_field (field);
  chtype ret = field_fore (c_field);

  return _scm_from_chtype (ret);
}

/* Return the index of the field in the field array of the form to
   which it is connected */
SCM
gucu_field_index (SCM field)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "field-index");

  const FIELD *c_field = _scm_to_field (field);

  int ret = field_index (c_field);
  if (ret == ERR)
    form_not_connected_error ("field-index");

  SCM s_ret = scm_from_int (ret);

  return s_ret;
}

/* Returns the justification attribute of the field */
SCM
gucu_field_just (SCM field)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "field-just");

  const FIELD *c_field = _scm_to_field (field);

  int ret = field_just (c_field);
  SCM s_ret = scm_from_int (ret);

  return s_ret;
}

/* Returns the field's current option bits */
SCM
gucu_field_opts (SCM field)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "field-opts");

  const FIELD *c_field = _scm_to_field (field);

  Field_Options ret = field_opts (c_field);
  SCM s_ret = scm_from_int (ret);

  return s_ret;
}

/* Turn off the given option bits, leaving others alone */
SCM
gucu_field_opts_off_x (SCM field, SCM opts)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "field-opts-off!");
  SCM_ASSERT (scm_is_integer (opts), opts, SCM_ARG2, "field-opts-off!");

  FIELD *c_field = _scm_to_field (field);
  Field_Options c_opts = scm_to_int (opts);

  int ret = field_opts_off (c_field, c_opts);

  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("field-opts-off!", opts);
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("field-opts-off!");
  else if (ret == E_CURRENT)
    form_current_field_error ("field-opts-off!");

  return SCM_UNSPECIFIED;
}

/* Turn on the given option bits, leaving others alone */
SCM
gucu_field_opts_on_x (SCM field, SCM opts)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "field-opts-on!");
  SCM_ASSERT (scm_is_integer (opts), opts, SCM_ARG2, "field-opts-on!");

  FIELD *c_field = _scm_to_field (field);
  int c_opts = scm_to_int (opts);

  int ret = field_opts_on (c_field, c_opts);

  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("field-opts-on!", opts);
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("field-opts-on!");
  else if (ret == E_CURRENT)
    form_current_field_error ("field-opts-on!");

  return SCM_UNSPECIFIED;
}

/* Returns the pad character of the form */
SCM
gucu_field_pad (SCM field)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "field-pad");

  const FIELD *c_field = _scm_to_field (field);

  int ret = field_pad (c_field);
  SCM s_ret = _scm_schar_from_char ((unsigned char) ret);

  return s_ret;
}

/* True if the field has changed */
SCM
gucu_field_status_p (SCM field)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "field-status?");

  const FIELD *c_field = _scm_to_field (field);

  bool ret = field_status (c_field);
  SCM s_ret = scm_from_bool (ret);

  return s_ret;
}

/* Operate on character or event C */
SCM
gucu_form_driver (SCM form, SCM c)
{
  SCM_ASSERT (_scm_is_form (form), form, SCM_ARG1, "form-driver");
  SCM_ASSERT (SCM_CHARP (c) || scm_is_integer (c), c, SCM_ARG2,
	      "form-driver");

  int c_c;
  FORM *c_form = _scm_to_form (form);
  if (SCM_CHARP (c))
    c_c = (unsigned char) _scm_schar_to_char (c);
  else
    c_c = scm_to_int (c);

  int ret = form_driver (c_form, c_c);

  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("form-driver", c);
  else if (ret == E_BAD_STATE)
    form_bad_state_error ("form-driver");
  else if (ret == E_NOT_POSTED)
    form_not_posted_error ("form-driver");
  else if (ret == E_INVALID_FIELD)
    form_invalid_field_error ("form-driver");
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("form-driver");
  else if (ret == E_CURRENT)
    form_current_field_error ("form-driver");

  /* Return E_OK, E_REQUEST_DENIED (mouse error), or
     E_UNKNOWN_COMMAND */
  SCM s_ret = scm_from_int (ret);

  return s_ret;
}

/* Retrieve current form option flags */
SCM
gucu_form_opts (SCM arg1)
{
  SCM_ASSERT (_scm_is_form (arg1), arg1, SCM_ARG1, "form-opts");

  const FORM *c_arg1 = _scm_to_form (arg1);

  int ret = form_opts (c_arg1);
  SCM s_ret = scm_from_int (ret);

  return s_ret;
}

/* Turns on the named options */
SCM
gucu_form_opts_on_x (SCM form, SCM opts)
{
  SCM_ASSERT (_scm_is_form (form), form, SCM_ARG1, "form-opts-on!");
  SCM_ASSERT (scm_is_integer (opts), opts, SCM_ARG2, "form-opts-on!");

  FORM *c_form = _scm_to_form (form);
  Form_Options c_opts = scm_to_int (opts);

  int ret = form_opts_on (c_form, c_opts);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("form-opts-on!", opts);

  return SCM_UNSPECIFIED;
}

/* Turn off the named options */
SCM
gucu_form_opts_off_x (SCM form, SCM opts)
{
  SCM_ASSERT (_scm_is_form (form), form, SCM_ARG1, "form-opts-off!");
  SCM_ASSERT (scm_is_integer (opts), opts, SCM_ARG2, "form-opts-off!");

  FORM *c_form = _scm_to_form (form);
  int c_opts = scm_to_int (opts);

  int ret = form_opts_off (c_form, c_opts);

  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("form-opts-off!", opts);

  return SCM_UNSPECIFIED;
}

/* Return a form's current page number */
SCM
gucu_form_page (SCM form)
{
  SCM_ASSERT (_scm_is_form (form), form, SCM_ARG1, "form-page");

  const FORM *c_form = _scm_to_form (form);

  int ret = form_page (c_form);

  /* This error should never occur, because the _scm_is_form check
     should take care of this error condition.  "Bad State" really
     isn't the right error name for this, but, I don't want to invent
     a new error type. */
  if (ret < 0)
    form_bad_state_error ("form-page");

  SCM s_ret = scm_from_int (ret);

  return s_ret;
}

/* Search for a request with the name.  Return integer on success or
   #f on failure */
SCM
gucu_form_request_by_name (SCM str)
{
  SCM_ASSERT (scm_is_string (str), str, SCM_ARG1, "form-request-by-name");

  char *c_str = scm_to_locale_string (str);

  int ret = form_request_by_name (c_str);

  free (c_str);

  if (ret == E_NO_MATCH)
    return SCM_BOOL_F;

  return scm_from_int (ret);
}

/* Get the external name of a form request */
SCM
gucu_form_request_name (SCM req)
{
  SCM_ASSERT (scm_is_integer (req), req, SCM_ARG1, "form-request-name");

  int c_req = scm_to_int (req);

  const char *name = form_request_name (c_req);

  if (name == NULL)
    scm_out_of_range ("form-request-name", req);

  SCM s_ret = scm_from_locale_string (name);

  return s_ret;
}

/* Return the subwindow of this form */
SCM
gucu_form_sub (SCM form)
{
  struct gucu_form *gf;

  scm_assert_smob_type (form_tag, form);

  gf = (struct gucu_form *) SCM_SMOB_DATA (form);

  /* Return the subwindow, if one has already been assigned */
  if (gf->sub)
    return gf->sub;

  /* Otherwise, the subwindow is stdscr */
  return SCM_BOOL_F;
}

/* Return the main window of this form */
SCM
gucu_form_win (SCM form)
{
  struct gucu_form *gf;

  scm_assert_smob_type (form_tag, form);

  gf = (struct gucu_form *) SCM_SMOB_DATA (form);

  /* Return the window, if one has already been assigned */
  if (gf->win)
    return gf->win;

  /* Otherwise, the subwindow is stdscr */
  return SCM_BOOL_F;
}

/* Manually free the storage associated with the field */
SCM
gucu_free_field (SCM fld)
{
  SCM_ASSERT (_scm_is_field (fld), fld, SCM_ARG1, "free-field");

  FIELD *c_fld = _scm_to_field (fld);

  int ret = free_field (c_fld);
  SCM s_ret = scm_from_int (ret);

  return s_ret;
}

/* Manually free the memory associated with the form */
SCM
gucu_free_form (SCM frm)
{
  struct gucu_form *gf;

  scm_assert_smob_type (form_tag, frm);

  gf = (struct gucu_form *) SCM_SMOB_DATA (frm);

  int ret = free_form (gf->form);
  if (ret == E_POSTED)
    form_posted_error ("free-form");
  else if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("free-form", frm);

  gf->form = NULL;

  /* Detach the fields */
  if (gf->fields)
    {
      while (scm_is_true (scm_call_0 (gf->fields_guard)))
	;
      gf->fields = NULL;
    }
  if (gf->win != NULL)
    {
      while (scm_is_true (scm_call_0 (gf->win_guard)))
	;
      gf->win = NULL;
    }
  if (gf->sub != NULL)
    {
      while (scm_is_true (scm_call_0 (gf->sub_guard)))
	;
      gf->sub = NULL;
    }

  return SCM_UNSPECIFIED;
}

/* Moves a disconnected field to a new location */
SCM
gucu_move_field (SCM fld, SCM y, SCM x)
{
  SCM_ASSERT (_scm_is_field (fld), fld, SCM_ARG1, "move-field");
  SCM_ASSERT (scm_is_integer (y), y, SCM_ARG2, "move-field");
  SCM_ASSERT (scm_is_integer (x), x, SCM_ARG3, "move-field");

  FIELD *c_fld = _scm_to_field (fld);
  int c_y = scm_to_int (y);
  int c_x = scm_to_int (x);

  int ret = move_field (c_fld, c_y, c_x);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("move-field", scm_list_2 (y, x));
  else if (ret == E_CONNECTED)
    form_connected_error ("move-field");
  else if (ret == E_POSTED)
    form_posted_error ("move-field");
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("move-field");

  return SCM_UNSPECIFIED;
}

/* True if the given field marks the beginning of a page */
SCM
gucu_new_page_p (SCM fld)
{
  SCM_ASSERT (_scm_is_field (fld), fld, SCM_ARG1, "new-page?");

  const FIELD *c_fld = _scm_to_field (fld);

  bool ret = new_page (c_fld);
  SCM s_ret = scm_from_bool (ret);

  return s_ret;
}

/* Restore the cursor to the position required by the forms driver */
SCM
gucu_pos_form_cursor (SCM frm)
{
  SCM_ASSERT (_scm_is_form (frm), frm, SCM_ARG1, "pos-form-cursor");

  FORM *c_frm = _scm_to_form (frm);

  int ret = pos_form_cursor (c_frm);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("pos-form-cursor", frm);
  else if (ret == E_NOT_POSTED)
    form_not_posted_error ("pos-form-cursor");
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("pos-form-cursor");

  return SCM_UNSPECIFIED;
}

/* Display a form to its subwindow */
SCM
gucu_post_form (SCM frm)
{
  SCM_ASSERT (_scm_is_form (frm), frm, SCM_ARG1, "post-form");

  FORM *c_frm = _scm_to_form (frm);

  int ret = post_form (c_frm);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("post-form", frm);
  else if (ret == E_BAD_STATE)
    form_bad_state_error ("post-form");
  else if (ret == E_NOT_POSTED)
    form_not_posted_error ("post-form");
  else if (ret == E_NOT_CONNECTED)
    form_not_connected_error ("post-form");
  else if (ret == E_NO_ROOM)
    form_no_room_error ("post-form");
  else if (ret == E_POSTED)
    form_posted_error ("post-form");
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("post-form");

  return SCM_UNSPECIFIED;
}

/* Sets the current field of the given form */
SCM
gucu_set_current_field_x (SCM frm, SCM fld)
{
  SCM_ASSERT (_scm_is_form (frm), frm, SCM_ARG1, "set-current-field!");
  SCM_ASSERT (_scm_is_field (fld), fld, SCM_ARG2, "set-current-field!");

  FORM *c_frm = _scm_to_form (frm);
  FIELD *c_fld = _scm_to_field (fld);

  int ret = set_current_field (c_frm, c_fld);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-current-field!", frm);
  else if (ret == E_BAD_STATE)
    form_bad_state_error ("set-current-field!");
  else if (ret == E_INVALID_FIELD)
    form_invalid_field_error ("set-current-field!");
  else if (ret == E_NOT_CONNECTED)
    form_not_connected_error ("set-current-field!");
  else if (ret == E_NO_ROOM)
    form_no_room_error ("set-current-field!");
  else if (ret == E_POSTED)
    form_posted_error ("set-current-field!");
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-current-field!");

  return SCM_UNSPECIFIED;
}

/* Set the background attribute of the field */
SCM
gucu_set_field_back_x (SCM fld, SCM attr)
{
  SCM_ASSERT (_scm_is_field (fld), fld, SCM_ARG1, "set-field-back!");
  SCM_ASSERT (_scm_is_chtype (attr), attr, SCM_ARG2, "set-field-back!");

  FIELD *c_fld = _scm_to_field (fld);
  chtype c_attr = _scm_to_chtype (attr);

  int ret = set_field_back (c_fld, c_attr);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-field-back!", attr);
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-field-back!");

  return SCM_UNSPECIFIED;
}

/* Set the number of the buffer of the field to contain the given string */
SCM
gucu_set_field_buffer_x (SCM field, SCM n, SCM str)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "set-field-buffer!");
  SCM_ASSERT (scm_is_integer (n), n, SCM_ARG2, "set-field-buffer!");
  SCM_ASSERT (scm_is_string (str), str, SCM_ARG3, "set-field-buffer!");

  FIELD *c_field = _scm_to_field (field);
  int c_n = scm_to_int (n);
  char *c_str = scm_to_locale_string (str);

  int ret = set_field_buffer (c_field, c_n, c_str);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-field-buffer!", n);
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-field-buffer!");

  free (c_str);

  return SCM_UNSPECIFIED;
}

/* Set the foreground attribute of the field */
SCM
gucu_set_field_fore_x (SCM field, SCM attr)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "set-field-fore!");
  SCM_ASSERT (_scm_is_chtype (attr), attr, SCM_ARG2, "set-field-fore!");

  FIELD *c_field = _scm_to_field (field);
  chtype c_attr = _scm_to_chtype (attr);

  int ret = set_field_fore (c_field, c_attr);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-field-fore!", attr);
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-field-fore!");

  return SCM_UNSPECIFIED;
}

/* Sets the justification attribute of a field */
SCM
gucu_set_field_just_x (SCM field, SCM just)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "set-field-just!");
  SCM_ASSERT (scm_is_integer (just), just, SCM_ARG2, "set-field-just!");

  FIELD *c_field = _scm_to_field (field);
  int c_just = scm_to_int (just);

  int ret = set_field_just (c_field, c_just);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-field-just!", just);
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-field-just!");

  return SCM_UNSPECIFIED;
}

/* Set field opts */
SCM
gucu_set_field_opts_x (SCM field, SCM opts)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "set-field-opts!");
  SCM_ASSERT (scm_is_integer (opts), opts, SCM_ARG2, "set-field-opts!");

  FIELD *c_field = _scm_to_field (field);
  Field_Options c_opts = scm_to_int (opts);

  int ret = set_field_opts (c_field, c_opts);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-field-opts!", opts);
  else if (ret == E_CURRENT)
    form_current_field_error ("set-field-opts!");
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-field-opts!");

  return SCM_UNSPECIFIED;
}

SCM
gucu_set_field_pad_x (SCM field, SCM pad)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "set-field-pad!");
  SCM_ASSERT (SCM_CHARP (pad), pad, SCM_ARG2, "set-field-pad!");

  FIELD *c_field = _scm_to_field (field);
  char c_pad = _scm_schar_to_char (pad);

  int ret = set_field_pad (c_field, (unsigned char) c_pad);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-field-pad!", pad);
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-field-pad!");

  return SCM_UNSPECIFIED;
}

/* Set the status of the field as if the field had changed */
SCM
gucu_set_field_status_x (SCM field, SCM status)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "set-field-status!");
  SCM_ASSERT (scm_is_bool (status), status, SCM_ARG2, "set-field-status!");

  FIELD *c_field = _scm_to_field (field);
  bool c_status = scm_to_bool (status);

  int ret = set_field_status (c_field, c_status);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-field-status!", status);
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-field-status!");

  return SCM_UNSPECIFIED;
}

/* Set all the given form's option bits */
SCM
gucu_set_form_opts_x (SCM form, SCM opts)
{
  SCM_ASSERT (_scm_is_form (form), form, SCM_ARG1, "set-form-opts!");
  SCM_ASSERT (scm_is_integer (opts), opts, SCM_ARG2, "set-form-opts!");

  FORM *c_form = _scm_to_form (form);
  Field_Options c_opts = scm_to_int (opts);

  int ret = set_form_opts (c_form, c_opts);
  if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-form-opts!");

  return SCM_UNSPECIFIED;
}

/* The page number of the form */
SCM
gucu_set_form_page_x (SCM form, SCM n)
{
  SCM_ASSERT (_scm_is_form (form), form, SCM_ARG1, "set-form-page!");
  SCM_ASSERT (scm_is_integer (n), n, SCM_ARG2, "set-form-page!");

  FORM *c_form = _scm_to_form (form);
  int c_n = scm_to_int (n);

  int ret = set_form_page (c_form, c_n);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-form-page!", n);
  else if (ret == E_BAD_STATE)
    form_bad_state_error ("set-form-page!");
  else if (ret == E_INVALID_FIELD)
    form_invalid_field_error ("set-form-page!");
  else if (ret == E_NOT_CONNECTED)
    form_not_connected_error ("set-form-page!");
  else if (ret == E_NO_ROOM)
    form_no_room_error ("set-form-page!");
  else if (ret == E_POSTED)
    form_posted_error ("set-form-page!");
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-form-page!");

  return SCM_UNSPECIFIED;
}

/* Set the subwindow for the current form */
SCM
gucu_set_form_sub_x (SCM form, SCM win)
{
  struct gucu_form *gf;

  scm_assert_smob_type (form_tag, form);

  gf = (struct gucu_form *) SCM_SMOB_DATA (form);

  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG2, "set-form-sub!");

  WINDOW *c_win = _scm_to_window (win);

  int ret = set_form_sub (gf->form, c_win);

  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-form-sub!", win);
  else if (ret == E_POSTED)
    form_posted_error ("set-form-sub!");
  else if (ret == E_NOT_CONNECTED)
    form_not_connected_error ("set-form-sub!");
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-form-sub!");

  /* Release whatever subwindow is current */
  if (gf->sub != NULL)
    {
      while (scm_is_true (scm_call_0 (gf->sub_guard)))
	;
      gf->sub = NULL;
    }

  /* If this is stdscr, we shouldn't store it as SUB, because it could
     be returned by form_sub and then possibly freed */
  if (c_win != stdscr)
    {
      gf->sub = win;
      scm_call_1 (gf->sub_guard, win);
    }

  return SCM_UNSPECIFIED;
}

/* Set the window for the current form */
SCM
gucu_set_form_win_x (SCM form, SCM win)
{
  struct gucu_form *gf;

  scm_assert_smob_type (form_tag, form);

  gf = (struct gucu_form *) SCM_SMOB_DATA (form);

  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG2, "set-form-win!");

  WINDOW *c_win = _scm_to_window (win);

  int ret = set_form_win (gf->form, c_win);

  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-form-win!", win);
  else if (ret == E_POSTED)
    form_posted_error ("set-form-win!");
  else if (ret == E_NOT_CONNECTED)
    form_not_connected_error ("set-form-win!");
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-form-win!");

  /* Release whatever window is current */
  if (gf->win != NULL)
    {
      while (scm_is_true (scm_call_0 (gf->win_guard)))
	;
      gf->win = NULL;
    }

  /* If this is stdscr, we shouldn't store it as WIN, because it could
     be returned by form_win and then possibly freed */
  if (c_win != stdscr)
    {
      gf->win = win;
      scm_call_1 (gf->win_guard, win);
    }

  return SCM_UNSPECIFIED;
}

/* Sets the maximum size for a dynamic field */
SCM
gucu_set_max_field_x (SCM field, SCM n)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "set-max-field!");
  SCM_ASSERT (scm_is_integer (n), n, SCM_ARG2, "set-max-field!");

  FIELD *c_field = _scm_to_field (field);
  int c_n = scm_to_int (n);

  int ret = set_max_field (c_field, c_n);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-max-field!", n);
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-max-field!");
  else if (ret != E_OK)
    abort ();

  return SCM_UNSPECIFIED;
}

SCM
gucu_set_new_page_x (SCM field, SCM flag)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "set-new-page!");
  SCM_ASSERT (scm_is_bool (flag), flag, SCM_ARG2, "set-new-page!");

  FIELD *c_field = _scm_to_field (field);
  bool c_flag = scm_to_bool (flag);

  int ret = set_new_page (c_field, c_flag);
  if (ret == E_CONNECTED)
    form_connected_error ("set-new-page!");
  if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-new-page!");
  else if (ret != E_OK)
    abort ();

  return SCM_UNSPECIFIED;
}

/* Erase the form from its associated subwindow */
SCM
gucu_unpost_form (SCM frm)
{
  SCM_ASSERT (_scm_is_form (frm), frm, SCM_ARG1, "unpost-form");

  FORM *c_frm = _scm_to_form (frm);

  int ret = unpost_form (c_frm);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("unpost-form", frm);
  else if (ret == E_BAD_STATE)
    form_bad_state_error ("unpost-form");
  else if (ret == E_NOT_POSTED)
    form_not_posted_error ("unpost-form");
  else if (ret == E_NOT_CONNECTED)
    form_not_connected_error ("unpost-form");
  else if (ret == E_NO_ROOM)
    form_no_room_error ("unpost-form");
  else if (ret == E_POSTED)
    form_posted_error ("unpost-form");
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("unpost-form");

  return SCM_UNSPECIFIED;
}


void
gucu_form_init_function ()
{
  scm_c_define_gsubr ("data-ahead?", 1, 0, 0, gucu_data_ahead_p);
  scm_c_define_gsubr ("data-behind?", 1, 0, 0, gucu_data_behind_p);
  scm_c_define_gsubr ("field-back", 1, 0, 0, gucu_field_back);
  scm_c_define_gsubr ("field-count", 1, 0, 0, gucu_field_count);
  scm_c_define_gsubr ("field-fore", 1, 0, 0, gucu_field_fore);
  scm_c_define_gsubr ("field-index", 1, 0, 0, gucu_field_index);
  scm_c_define_gsubr ("field-just", 1, 0, 0, gucu_field_just);
  scm_c_define_gsubr ("field-opts", 1, 0, 0, gucu_field_opts);
  scm_c_define_gsubr ("field-opts-off!", 2, 0, 0, gucu_field_opts_off_x);
  scm_c_define_gsubr ("field-opts-on!", 2, 0, 0, gucu_field_opts_on_x);
  scm_c_define_gsubr ("field-pad", 1, 0, 0, gucu_field_pad);
  scm_c_define_gsubr ("field-status?", 1, 0, 0, gucu_field_status_p);
  scm_c_define_gsubr ("form-driver", 2, 0, 0, gucu_form_driver);
  scm_c_define_gsubr ("form-opts", 1, 0, 0, gucu_form_opts);
  scm_c_define_gsubr ("form-opts-on!", 2, 0, 0, gucu_form_opts_on_x);
  scm_c_define_gsubr ("form-opts-off!", 2, 0, 0, gucu_form_opts_off_x);
  scm_c_define_gsubr ("form-page", 1, 0, 0, gucu_form_page);
  scm_c_define_gsubr ("form-request-by-name", 1, 0, 0,
		      gucu_form_request_by_name);
  scm_c_define_gsubr ("form-request-name", 1, 0, 0, gucu_form_request_name);
  scm_c_define_gsubr ("form-sub", 1, 0, 0, gucu_form_sub);
  scm_c_define_gsubr ("form-win", 1, 0, 0, gucu_form_win);
  scm_c_define_gsubr ("free-field", 1, 0, 0, gucu_free_field);
  scm_c_define_gsubr ("free-form", 1, 0, 0, gucu_free_form);
  scm_c_define_gsubr ("move-field", 3, 0, 0, gucu_move_field);
  scm_c_define_gsubr ("new-page?", 1, 0, 0, gucu_new_page_p);
  scm_c_define_gsubr ("pos-form-cursor", 1, 0, 0, gucu_pos_form_cursor);
  scm_c_define_gsubr ("post-form", 1, 0, 0, gucu_post_form);
  scm_c_define_gsubr ("set-current-field!", 2, 0, 0,
		      gucu_set_current_field_x);
  scm_c_define_gsubr ("set-field-back!", 2, 0, 0, gucu_set_field_back_x);
  scm_c_define_gsubr ("set-field-buffer!", 3, 0, 0, gucu_set_field_buffer_x);
  scm_c_define_gsubr ("set-field-fore!", 2, 0, 0, gucu_set_field_fore_x);
  scm_c_define_gsubr ("set-field-just!", 2, 0, 0, gucu_set_field_just_x);
  scm_c_define_gsubr ("set-field-opts!", 2, 0, 0, gucu_set_field_opts_x);
  scm_c_define_gsubr ("set-field-pad!", 2, 0, 0, gucu_set_field_pad_x);
  scm_c_define_gsubr ("set-field-status!", 2, 0, 0, gucu_set_field_status_x);
  scm_c_define_gsubr ("set-form-opts!", 2, 0, 0, gucu_set_form_opts_x);
  scm_c_define_gsubr ("set-form-page!", 2, 0, 0, gucu_set_form_page_x);
  scm_c_define_gsubr ("set-form-sub!", 2, 0, 0, gucu_set_form_sub_x);
  scm_c_define_gsubr ("set-form-win!", 2, 0, 0, gucu_set_form_win_x);
  scm_c_define_gsubr ("set-max-field!", 2, 0, 0, gucu_set_max_field_x);
  scm_c_define_gsubr ("set-new-page!", 2, 0, 0, gucu_set_new_page_x);
  scm_c_define_gsubr ("unpost-form", 1, 0, 0, gucu_unpost_form);
}
