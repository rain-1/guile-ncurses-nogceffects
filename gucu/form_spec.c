#include <curses.h>
#include <libguile.h>
#include <config.h>
#include <errno.h>
#include <form.h>

#include "form_type.h"
#include "form_func.h"
#include "form_spec.h"
#include "compat.h"

// Create a copy of FIELD with a different TOPROW and LEFTCOL
SCM
gucu_dup_field (SCM field, SCM toprow, SCM leftcol)
{
  SCM_ASSERT (_scm_is_field(field), field, SCM_ARG1, "dup-field");
  SCM_ASSERT (scm_is_integer (toprow), toprow, SCM_ARG2, "dup-field");
  SCM_ASSERT (scm_is_integer (leftcol), leftcol, SCM_ARG3, "dup-field");

  int c_toprow;
  int c_leftcol;
  FIELD *c_field;
  FIELD *c_dup;

  c_toprow = scm_to_int (toprow);
  c_leftcol = scm_to_int (leftcol);
  c_field = _scm_to_field (field);
  c_dup = dup_field (c_field, c_toprow, c_leftcol);

  if (c_dup == (FIELD *) NULL) 
    {
      if (errno == E_BAD_ARGUMENT)
	scm_out_of_range ("dup-field", scm_list_2 (toprow, leftcol));
      else if (errno == E_SYSTEM_ERROR)
	scm_syserror ("dup-field");
      else
	abort ();
    }

  return _scm_from_field (c_dup);
}
      
// Returns the rows, columns and max size of FIELD
SCM
gucu_dynamic_field_info (SCM field)    
{
  SCM_ASSERT (_scm_is_field(field), field, SCM_ARG1, "dynamic-field-info");

  FIELD *c_field;
  int c_rows, c_cols, c_max;
  int ret;

  c_field = _scm_to_field (field);
  ret = dynamic_field_info (c_field, &c_rows, &c_cols, &c_max);
  
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("dynamic-field-info", field);
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("dynamic-field-info");
  else if (ret != E_OK)
    abort ();

  return scm_list_3 (scm_from_int (c_rows),
		     scm_from_int (c_cols),
		     scm_from_int (c_max));
}

// Returns the contents of the field buffer BUFFER.  Only buffer 0 is
// the on screen buffer, but, for some reasone, other invisible
// buffers can be allocated.
SCM
gucu_field_buffer (SCM field, SCM buffer)
{
  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "field-buffer");
  SCM_ASSERT (SCM_UNBNDP (buffer) || scm_is_integer (buffer) , buffer, SCM_ARG2, "field-buffer");

  FIELD *c_field = _scm_to_field (field);
  int c_buffer;
  if (SCM_UNBNDP (buffer))
    {
      c_buffer = 0;
    }
  else
    {
      c_buffer = scm_to_int (buffer);
    }
  char *buf = field_buffer (c_field, c_buffer);
  
  if (buf == NULL) 
    {
      if (errno == E_BAD_ARGUMENT)
	scm_out_of_range ("field-buffer", field);
    }
      
  return (scm_from_locale_string (buf));
}

// Returns the constants passed to a field at its creation time.
SCM
gucu_field_info (SCM field)
{
  SCM_ASSERT (_scm_is_field(field), field, SCM_ARG1, "field-info");

  FIELD *c_field;
  int c_rows, c_cols, c_frow, c_fcol, c_nrow, c_nbuf;
  int ret;

  c_field = _scm_to_field (field);
  ret = field_info (c_field, &c_rows, &c_cols, 
		    &c_frow, &c_fcol, &c_nrow, &c_nbuf);
  
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("field-info", field);
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("field-info");
  else if (ret != E_OK)
    abort ();

  return scm_list_n (scm_from_int (c_rows),
		     scm_from_int (c_cols),
		     scm_from_int (c_frow),
		     scm_from_int (c_fcol),
		     scm_from_int (c_nrow),
		     scm_from_int (c_nbuf),
		     SCM_UNDEFINED);

}

#if 0
SCM
gucu_set_field_init (SCM form, SCM func)
{
}
#endif

// Returns the type of field that FIELD is
SCM
gucu_field_type (SCM field)
{
  FIELD *c_field;
  FIELDTYPE *ft;

  SCM_ASSERT (_scm_is_field(field), field, SCM_ARG1, "field-type");
  
  c_field = _scm_to_field (field);
  
  ft = field_type (c_field);
  if (ft == TYPE_ALNUM)
    return scm_from_locale_symbol ("TYPE_ALNUM");
  else if (ft == TYPE_ALPHA)
    return scm_from_locale_symbol ("TYPE_ALPHA");
  else if (ft == TYPE_ENUM)
    return scm_from_locale_symbol ("TYPE_ENUM");
  else if (ft == TYPE_INTEGER)
    return scm_from_locale_symbol ("TYPE_INTEGER");
  else if (ft == TYPE_NUMERIC)
    return scm_from_locale_symbol ("TYPE_NUMERIC");
  else if (ft == TYPE_REGEXP)
    return scm_from_locale_symbol ("TYPE_REGEXP");
  else if (ft == TYPE_IPV4)
    return scm_from_locale_symbol ("TYPE_IPV4");
  else if (ft == NULL)
    return SCM_BOOL_F;
  else
    abort ();
}
    
// Create a copy of FIELD with a different TOPROW and LEFTCOL
SCM
gucu_link_field (SCM field, SCM toprow, SCM leftcol)
{
  SCM_ASSERT (_scm_is_field(field), field, SCM_ARG1, "link-field");
  SCM_ASSERT (scm_is_integer (toprow), toprow, SCM_ARG2, "link-field");
  SCM_ASSERT (scm_is_integer (leftcol), leftcol, SCM_ARG3, "link-field");

  int c_toprow;
  int c_leftcol;
  FIELD *c_field;
  FIELD *c_link;

  c_toprow = scm_to_int (toprow);
  c_leftcol = scm_to_int (leftcol);
  c_field = _scm_to_field (field);
  c_link = link_field (c_field, c_toprow, c_leftcol);

  if (c_link == (FIELD *) NULL) 
    {
      if (errno == E_BAD_ARGUMENT)
	scm_out_of_range ("link-field", scm_list_2 (toprow, leftcol));
      else if (errno == E_SYSTEM_ERROR)
	scm_syserror ("link-field");
      else
	abort ();
    }

  return _scm_from_field (c_link);
}

// Returns the rows, columns and max size of FIELD
SCM
gucu_scale_form (SCM form)    
{
  SCM_ASSERT (_scm_is_form (form), form, SCM_ARG1, "scale-form");

  FORM *c_form;
  int c_rows, c_cols;
  int ret;

  c_form = _scm_to_form (form);
  ret = scale_form (c_form, &c_rows, &c_cols);
  
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("scale-form", form);
  else if (ret == E_SYSTEM_ERROR)
    scm_syserror ("scale-form");
  else if (ret == E_POSTED)
    form_posted_error ("scale-form");
  else if (ret == E_NOT_CONNECTED)
    form_not_connected_error ("scale-form");
  else if (ret != E_OK)
    abort ();

  return scm_list_2 (scm_from_int (c_rows),
		     scm_from_int (c_cols));
}

SCM
gucu_set_field_type (SCM field, SCM type, SCM a, SCM b, SCM c)
{
  FIELD *c_field;
  int ret;

  SCM_ASSERT (_scm_is_field (field), field, SCM_ARG1, "set-field-type");
  SCM_ASSERT (scm_is_symbol (type), type, SCM_ARG2, "set-field-type");

  c_field = _scm_to_field (field);
  
  if (scm_is_true (scm_eq_p (type, scm_from_locale_symbol ("TYPE_ALNUM"))))
    {
      int c_width;

      SCM_ASSERT (scm_is_integer (a), a, SCM_ARG3, "set-field-type");
      c_width = scm_to_int (a);
      ret = set_field_type (c_field, TYPE_ALNUM, c_width);
    }
  else if (scm_is_true (scm_eq_p (type, scm_from_locale_symbol ("TYPE_ALPHA"))))
    {
      int c_width;

      SCM_ASSERT (scm_is_integer (a), a, SCM_ARG3, "set-field-type");
      c_width = scm_to_int (a);
      ret = set_field_type (c_field, TYPE_ALPHA, c_width); 
    }
  else if (scm_is_true (scm_eq_p (type, scm_from_locale_symbol ("TYPE_ENUM"))))
    {
      char **c_str_list;
      int c_case_sensitivity;
      int c_partial;
      size_t len;
      size_t i;

      SCM_ASSERT (scm_is_true (scm_list_p (a)), a, SCM_ARG3, "set-field-type");
      SCM_ASSERT (scm_is_integer (b), b, SCM_ARG4, "set-field-type");
      SCM_ASSERT (scm_is_integer (c), c, SCM_ARG5, "set-field-type");

      len = scm_to_size_t (scm_length (a));
      c_str_list = scm_gc_malloc (sizeof (char *) * (len+1), "set-field-type");
      for (i=0; i<len; i++) 
	{
	  c_str_list[i] = (scm_to_locale_string 
			   (scm_list_ref (a, scm_from_int (i))));
	}
      c_str_list[len-1] = (char *) 0;
      c_case_sensitivity = scm_to_int (b);
      c_partial = scm_to_int (c);
      
      ret = set_field_type (c_field, TYPE_ENUM, c_str_list, c_case_sensitivity,
			    c_partial);

      for (i=0; i<len; i++)
	{
	  free (c_str_list[i]);
	}
      free (c_str_list);
    }
  else if (scm_is_true (scm_eq_p (type, 
				  scm_from_locale_symbol ("TYPE_NUMERIC"))))
    {
      int c_precision;
      double c_min;
      double c_max;

      SCM_ASSERT (scm_is_integer (a), a, SCM_ARG3, "set-field-type");
      SCM_ASSERT (scm_is_number (b), b, SCM_ARG4, "set-field-type");
      SCM_ASSERT (scm_is_number (c), c, SCM_ARG5, "set-field-type");
      
      c_precision = scm_to_int (a);
      c_min = scm_to_double (b);
      c_max = scm_to_double (c);

      ret = set_field_type (c_field, TYPE_NUMERIC, c_precision, c_min, c_max);
    }
  else if (scm_is_true (scm_eq_p (type, scm_from_locale_symbol ("TYPE_REGEXP"))))
    {
      char *c_reg;

      SCM_ASSERT (scm_is_string (a), a, SCM_ARG3, "set-field-type");

      c_reg = scm_to_locale_string (a);
      
      ret = set_field_type (c_field, TYPE_REGEXP, c_reg);

      free (c_reg);
    }
  else if (scm_is_true (scm_eq_p (type, scm_from_locale_symbol ("TYPE_IPV4"))))
    {
      ret = set_field_type (c_field, TYPE_IPV4);
    }
  else
    {
      scm_out_of_range ("set-field-type", type);
    }
  
  return (scm_from_int (ret));
}
      

void
gucu_form_init_special ()
{
  scm_c_define_gsubr ("dup-field", 3, 0, 0, gucu_dup_field);
  scm_c_define_gsubr ("dynamic-field-info", 1, 0, 0, gucu_dynamic_field_info);
  scm_c_define_gsubr ("field-buffer", 1, 1, 0, gucu_field_buffer);
  scm_c_define_gsubr ("field-info", 1, 0, 0, gucu_field_info);
  scm_c_define_gsubr ("field-type", 1, 0, 0, gucu_field_type);
  scm_c_define_gsubr ("link-field", 3, 0, 0, gucu_dup_field);
  scm_c_define_gsubr ("scale-form", 1, 0, 0, gucu_scale_form);
  scm_c_define_gsubr ("set-field-type", 2, 3, 0, gucu_set_field_type);
}
