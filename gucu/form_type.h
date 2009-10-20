#ifndef FORM_TYPE_H
#define FORM_TYPE_H

#include <libguile.h>
#include <form.h>

#include <config.h>

#ifdef DLL_EXPORT
#define API __attribute__ ((dllexport, cdecl))
#else
#define API
#endif

struct gucu_form {
  // Pointer to the FORM structure
  FORM *form;
  // A scheme list containing FIELD SCMs
  SCM fields;
  SCM fields_guard;
  // The windows
  SCM win;
  SCM sub;
  // Guardians for the windows
  SCM win_guard;
  SCM sub_guard;
};



int _scm_is_field (SCM x);
FIELD *_scm_to_field (SCM x);
SCM _scm_from_field (FIELD *x);
SCM gucu_new_field (SCM height, SCM width, SCM top, SCM left, SCM offscreen, 
		    SCM nbuffers) API;

int _scm_is_form (SCM x);
FORM *_scm_to_form (SCM x);

SCM gucu_is_field_p (SCM x);
SCM gucu_is_form_p (SCM x);

SCM gucu_new_form (SCM fields) API;
SCM gucu_form_fields (SCM form) API;
SCM gucu_set_form_fields_x (SCM form, SCM fields) API;

void gucu_form_init_type (void);

#endif
