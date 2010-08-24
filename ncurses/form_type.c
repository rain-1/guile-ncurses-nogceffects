/*
  form_type.c

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
#include <config.h>

#define _GNU_SOURCE
#include <assert.h>
#include <curses.h>
#include <errno.h>
#include <form.h>
#include <libguile.h>
#include <libintl.h>
#include <stdio.h>
#include <string.h>

#include "compat.h"
#include "form_func.h"
#include "form_type.h"
#include "gucuconfig.h"
#include "type.h"

scm_t_bits form_tag;
scm_t_bits field_tag;

SCM equalp_field (SCM x1, SCM x2);
size_t gc_free_field (SCM x);
SCM mark_field (SCM x);
int print_field (SCM x, SCM port, scm_print_state * pstate);


SCM equalp_form (SCM x1, SCM x2);
size_t gc_free_form (SCM x);
SCM mark_form (SCM x);
int print_form (SCM x, SCM port, scm_print_state * pstate);


// field -- in C, a FIELD.  In Scheme, a smob that contains the pointer

SCM
gucu_new_field (SCM height, SCM width, SCM top, SCM left, SCM offscreen,
		SCM nbuffers)
{
  SCM_ASSERT (scm_is_integer (height), height, SCM_ARG1, "new-field");
  SCM_ASSERT (scm_is_integer (width), width, SCM_ARG2, "new-field");
  SCM_ASSERT (scm_is_integer (top), top, SCM_ARG3, "new-field");
  SCM_ASSERT (scm_is_integer (left), left, SCM_ARG4, "new-field");
  SCM_ASSERT (scm_is_integer (offscreen), offscreen, SCM_ARG5, "new-field");
  SCM_ASSERT (scm_is_integer (nbuffers), nbuffers, SCM_ARG6, "new-field");

  int c_height = scm_to_int (height);
  int c_width = scm_to_int (width);
  int c_top = scm_to_int (top);
  int c_left = scm_to_int (left);
  int c_offscreen = scm_to_int (offscreen);
  int c_nbuffers = scm_to_int (nbuffers);

  FIELD *f =
    new_field (c_height, c_width, c_top, c_left, c_offscreen, c_nbuffers);
  if (f == NULL)
    {
      if (errno == E_BAD_ARGUMENT)
	{
	  scm_error_scm (SCM_BOOL_F,
			 scm_from_locale_string ("new-field"),
			 scm_from_locale_string ( gettext ("bad argument")),
			 SCM_BOOL_F, SCM_BOOL_F);
	}
      else if (errno == E_SYSTEM_ERROR)
	{
	  scm_error_scm (SCM_BOOL_F,
			 scm_from_locale_string ("new-field"),
			 scm_from_locale_string ( gettext ("system error")),
			 SCM_BOOL_F, SCM_BOOL_F);
	}
      else
	abort ();
    }

  SCM ret = _scm_from_field (f);

  return ret;
}

int
_scm_is_field (SCM x)
{
  if (SCM_SMOB_PREDICATE (field_tag, x))
    {
      if (SCM_SMOB_DATA (x) != 0)
	return 1;
      else
	return 0;
    }
  else
    return 0;
}

FIELD *
_scm_to_field (SCM x)
{
  return (FIELD *) SCM_SMOB_DATA (x);
}

SCM
_scm_from_field (FIELD * x)
{
  SCM s_field;

  assert (x != NULL);

  SCM_NEWSMOB (s_field, field_tag, x);

  assert (x == (FIELD *) SCM_SMOB_DATA (s_field));

  if (0)
    {
      fprintf (stderr, gettext ("Making <#field> smob from FIELD * %p\n"), (void *) x);
    }

  return (s_field);
}

// Fields are equal if they point to the same C structure
SCM
equalp_field (SCM x1, SCM x2)
{
  FIELD *field1 = (FIELD *) SCM_SMOB_DATA (x1);
  FIELD *field2 = (FIELD *) SCM_SMOB_DATA (x2);

  if ((field1 == NULL) || (field2 == NULL))
    return SCM_BOOL_F;
  else if ((field1 != field2))
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

SCM
mark_field (SCM x UNUSED)
{
  // No SCMs in the field type: nothing to do here.
  return (SCM_BOOL_F);
}

/* The name is free_field.  The curses primitive that frees memory is
   called del_field. Note that del_field doesn't free the underlying
   window. */
size_t
gc_free_field (SCM field)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "free-field");

  FIELD *f = _scm_to_field (field);

  if (f != NULL)
    {
      int ret = free_field (f);
      if (ret != E_OK)
	{
	  /* ??? --- what should happen when free fails? */
	}
      SCM_SET_SMOB_DATA (field, NULL);
    }

  return 0;
}

int
print_field (SCM x, SCM port, scm_print_state * pstate UNUSED)
{
  FIELD *fld = (FIELD *) SCM_SMOB_DATA (x);
  char *str;

  scm_puts ("#<field ", port);

  if (fld == 0)
    scm_puts ( gettext ("(freed)"), port);
  else
    {
      if (asprintf (&str, "%p", (void *) fld) < 0)
	scm_puts ("???", port);
      else
	scm_puts (str, port);
    }

  scm_puts (">", port);

  // non-zero means success
  return 1;
}

SCM
gucu_is_field_p (SCM x)
{
  return scm_from_bool (_scm_is_field (x));
}

// form -- in C, a FORM *.  In Scheme, a smob that contains the pointer
// to a form along with a list that contains the SCM of the fields

// N.B.: form->field must point to a C array containing the FIELD *
// contained in the SCM fields.

int
_scm_is_form (SCM x)
{
  return SCM_SMOB_PREDICATE (form_tag, x);
}

FORM *
_scm_to_form (SCM x)
{
  struct gucu_form *gf;

  scm_assert_smob_type (form_tag, x);

  gf = (struct gucu_form *) SCM_SMOB_DATA (x);

  return (FORM *) gf->form;
}

#if 0
SCM
_scm_from_form (FORM * x)
{
  SCM s_form;

  assert (x != NULL);

  SCM_NEWSMOB (s_form, form_tag, x);

  assert (x == (FORM *) SCM_SMOB_DATA (s_form));

  if (0)
    {
      fprintf (stderr, gettext ("Making smob from form based on WINDOW * %p\n"),
	       x->win);
    }

  return (s_form);
}
#endif

// Forms are equal if they point to the same C structure
SCM
equalp_form (SCM x1, SCM x2)
{
  FORM *form1 = (FORM *) SCM_SMOB_DATA (x1);
  FORM *form2 = (FORM *) SCM_SMOB_DATA (x2);

  if ((form1 == NULL) || (form2 == NULL))
    return SCM_BOOL_F;
  else if ((form1 != form2))
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

SCM
mark_form (SCM x)
{
  struct gucu_form *gf;

  scm_assert_smob_type (form_tag, x);

  gf = (struct gucu_form *) SCM_SMOB_DATA (x);

  return (gf->fields);
}

size_t
gc_free_form (SCM x)
{
  struct gucu_form *form;
  int retval;

  scm_assert_smob_type (form_tag, x);

  form = (struct gucu_form *) SCM_SMOB_DATA (x);

  assert (form != NULL);

  retval = free_form (form->form);

  if (retval == E_BAD_ARGUMENT)
    {
      scm_error_scm (SCM_BOOL_F,
		     scm_from_locale_string ( gettext ("garbage collection of form")),
		     scm_from_locale_string ( gettext ("bad argument")),
		     SCM_BOOL_F, SCM_BOOL_F);
    }
  else if (retval == E_POSTED)
    {
      scm_error_scm (SCM_BOOL_F,
		     scm_from_locale_string ( gettext ("garbage collection of form")),
		     scm_from_locale_string ( gettext ("posted")),
		     SCM_BOOL_F, SCM_BOOL_F);
    }

  /* Release scheme objects from the guardians */
  /* Detach the fields */
  if (form->fields)
    {
      while (scm_is_true (scm_call_0 (form->fields_guard)))
	;
      form->fields = NULL;
    }
  if (form->win != NULL)
    {
      while (scm_is_true (scm_call_0 (form->win_guard)))
	;
      form->win = NULL;
    }
  if (form->sub != NULL)
    {
      while (scm_is_true (scm_call_0 (form->sub_guard)))
	;
      form->sub = NULL;
    }

  SCM_SET_SMOB_DATA (x, NULL);

  return 0;
}

int
print_form (SCM x, SCM port, scm_print_state * pstate UNUSED)
{
  struct gucu_form *frm = (struct gucu_form *) SCM_SMOB_DATA (x);
  char *str;

  assert (frm != NULL);

  scm_puts ("#<form ", port);

  if (asprintf (&str, "%p", (void *) frm->form) < 0)
    scm_puts ("???", port);
  else
    scm_puts (str, port);

  scm_puts (">", port);

  // non-zero means success
  return 1;
}

SCM
gucu_is_form_p (SCM x)
{
  return scm_from_bool (_scm_is_form (x));
}


SCM
gucu_new_form (SCM fields)
{
  SCM_ASSERT (scm_is_true (scm_list_p (fields)), fields, SCM_ARG1,
	      "new-form");

  struct gucu_form *gf;
  size_t len;
  FIELD **c_fields;
  SCM smob;
  SCM entry;
  size_t i;

  // Step 1: allocate memory
  gf = scm_gc_malloc (sizeof (struct gucu_form), "gucu_form");

  len = scm_to_size_t (scm_length (fields));
  if (len == 0)
    {
      scm_wrong_type_arg ("new-form", SCM_ARG1, fields);

      // Shouldn't get here
      return SCM_UNSPECIFIED;
    }
  c_fields = scm_gc_malloc (sizeof (FIELD *) * (len + 1), "gucu_form");

  // Step 2: initialize it with C code

  // Step 3: Create the smob
  SCM_NEWSMOB (smob, form_tag, gf);

  // Step 4: Finish the initialization
  for (i = 0; i < len; i++)
    {
      entry = scm_list_ref (fields, scm_from_int (i));
      c_fields[i] = _scm_to_field (entry);
    }

  c_fields[len] = (FIELD *) NULL;

  gf->form = new_form (c_fields);

  if (gf->form == NULL)
    {
      free (c_fields);
      if (errno == E_BAD_ARGUMENT)
	{
	  scm_error_scm (SCM_BOOL_F,
			 scm_from_locale_string ("new-form"),
			 scm_from_locale_string ( gettext ("bad argument")),
			 fields, SCM_BOOL_F);
	}
      else if (errno == E_CONNECTED)
	{
	  scm_error_scm (SCM_BOOL_F,
			 scm_from_locale_string ("new-form"),
			 scm_from_locale_string ( gettext ("connected")),
			 SCM_BOOL_F, SCM_BOOL_F);
	}
      else if (errno == E_SYSTEM_ERROR)
	{
	  scm_error_scm (SCM_BOOL_F,
			 scm_from_locale_string ("new-form"),
			 scm_from_locale_string ( gettext ("system error")),
			 SCM_BOOL_F, SCM_BOOL_F);
	}
      else
	abort ();
    }

  gf->fields = fields;
  gf->win = SCM_BOOL_F;
  gf->sub = SCM_BOOL_F;

#ifndef GUILE_1_POINT_6
  gf->fields_guard = scm_make_guardian ();
  gf->win_guard = scm_make_guardian ();
  gf->sub_guard = scm_make_guardian ();
#else
  gf->fields_guard = scm_make_guardian (SCM_BOOL_F);
  gf->win_guard = scm_make_guardian (SCM_BOOL_F);
  gf->sub_guard = scm_make_guardian (SCM_BOOL_F);
#endif

  scm_call_1 (gf->fields_guard, fields);

  return smob;
}

// Return the fields on which the form depends
SCM
gucu_form_fields (SCM form)
{
  struct gucu_form *gf;

  scm_assert_smob_type (form_tag, form);

  gf = (struct gucu_form *) SCM_SMOB_DATA (form);

  // NOTE: the FIELDS is a list of FIELD smobs that were used to
  // initialize the form.  They are held here so that they are not
  // GC'd.

  return gf->fields;
}

SCM
gucu_set_form_fields_x (SCM form, SCM fields)
{
  SCM_ASSERT (_scm_is_form (form), form, SCM_ARG1, "set-form-fields!");
  /* FIXME: This isn't a complete type check */
  SCM_ASSERT (scm_is_true (scm_list_p (fields)), fields, SCM_ARG2,
	      "set-form-fields");

  struct gucu_form *gf;
  size_t len;
  FIELD **c_fields;
  SCM entry;
  size_t i;
  int ret;

  gf = (struct gucu_form *) SCM_SMOB_DATA (form);

  /* Release the current fields, if any */
  if (gf->fields)
    {
      while (scm_is_true (scm_call_0 (gf->fields_guard)))
	;
      gf->fields = NULL;
    }

  len = scm_to_size_t (scm_length (fields));
  c_fields = scm_gc_malloc (sizeof (FIELD *) * (len + 1), "set-form-fields");

  for (i = 0; i < len; i++)
    {
      entry = scm_list_ref (fields, scm_from_int (i));
      c_fields[i] = _scm_to_field (entry);
    }
  c_fields[len] = (FIELD *) NULL;

  ret = set_form_fields (gf->form, c_fields);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-form-fields!", fields);
  else if (ret == E_CONNECTED)
    form_connected_error ("set-form-fields!");
  else if (ret == E_POSTED)
    form_posted_error ("set-form-fields!");
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("set-form-fields!");

  gf->fields = fields;
  scm_call_1 (gf->fields_guard, fields);

  return SCM_UNSPECIFIED;
}


void
gucu_form_init_type ()
{
  field_tag = scm_make_smob_type ("field", sizeof (FIELD *));
  scm_set_smob_mark (field_tag, mark_field);
  scm_set_smob_free (field_tag, gc_free_field);
  scm_set_smob_print (field_tag, print_field);
  scm_set_smob_equalp (field_tag, equalp_field);
  scm_c_define_gsubr ("field?", 1, 0, 0, gucu_is_field_p);

  form_tag = scm_make_smob_type ("form", sizeof (FORM *));
  scm_set_smob_mark (form_tag, mark_form);
  scm_set_smob_free (form_tag, gc_free_form);
  scm_set_smob_print (form_tag, print_form);
  scm_set_smob_equalp (form_tag, equalp_form);
  scm_c_define_gsubr ("form?", 1, 0, 0, gucu_is_form_p);

  scm_c_define_gsubr ("new-field", 6, 0, 0, gucu_new_field);
  scm_c_define_gsubr ("new-form", 1, 0, 0, gucu_new_form);
  scm_c_define_gsubr ("form-fields", 1, 0, 0, gucu_form_fields);
  scm_c_define_gsubr ("set-form-fields!", 2, 0, 0, gucu_set_form_fields_x);
}
