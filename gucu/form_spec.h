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
GUCU_API SCM gucu_set_field_type (SCM field, SCM type, SCM a, SCM b, SCM c);

GUCU_LOCAL void gucu_form_init_special (void);

#endif
