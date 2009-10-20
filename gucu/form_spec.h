#ifndef FORM_SPEC_H
#define FORM_SPEC_H

#include <libguile.h>
#include <config.h>

#ifdef DLL_EXPORT
#define API __attribute__ ((dllexport, cdecl))
#else
#define API
#endif

SCM gucu_dup_field (SCM field, SCM toprow, SCM leftcol) API;
SCM gucu_dynamic_field_info (SCM field) API;
SCM gucu_field_buffer (SCM field, SCM buffer) API;
SCM gucu_field_info (SCM field) API;
SCM gucu_field_type (SCM field) API;
SCM gucu_link_field (SCM field, SCM x, SCM y) API;
SCM gucu_scale_form (SCM form) API;
SCM gucu_set_field_type (SCM field, SCM type, SCM a, SCM b, SCM c) API;

void gucu_form_init_special (void);

#endif
