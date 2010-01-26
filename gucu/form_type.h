#ifndef FORM_TYPE_H
#define FORM_TYPE_H

#include <libguile.h>
#include <form.h>
#include "visibility.h"

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


GUCU_LOCAL int _scm_is_field (SCM x);
GUCU_LOCAL FIELD *_scm_to_field (SCM x);
GUCU_LOCAL SCM _scm_from_field (FIELD *x);
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
