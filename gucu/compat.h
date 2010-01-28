#ifndef COMPAT_H
#define COMPAT_H

#include <libguile.h>
#include "gucuconfig.h"
#include "visibility.h"

#ifdef GUILE_1_POINT_6

GUCU_LOCAL void scm_assert_smob_type (scm_t_bits tag, SCM val);
GUCU_LOCAL int scm_c_string_length (SCM x);
GUCU_LOCAL SCM scm_c_string_ref (SCM x, size_t k);
GUCU_LOCAL SCM scm_from_bool (int x);
GUCU_LOCAL SCM scm_from_int (int x);
GUCU_LOCAL SCM scm_from_locale_string (const char *str);
GUCU_LOCAL SCM scm_from_locale_symbol (const char *sym);
GUCU_LOCAL SCM scm_from_short (short x);
GUCU_LOCAL SCM scm_from_uint (unsigned int x);
GUCU_LOCAL SCM scm_from_ulong (unsigned long x);
GUCU_LOCAL void *scm_gc_malloc (size_t size, const char *what);
GUCU_LOCAL int scm_is_bool (SCM x);
GUCU_LOCAL int scm_is_integer (SCM x);
GUCU_LOCAL int scm_is_number (SCM x);
GUCU_LOCAL int scm_is_string (SCM x);
GUCU_LOCAL int scm_is_symbol (SCM x);
GUCU_LOCAL int scm_is_true (SCM x);
GUCU_LOCAL void *scm_malloc (size_t x);
GUCU_LOCAL int scm_to_bool (SCM x);
GUCU_LOCAL int scm_to_int (SCM x);
GUCU_LOCAL char *scm_to_locale_string (SCM x);
GUCU_LOCAL short scm_to_short (SCM x);
GUCU_LOCAL size_t scm_to_size_t (SCM x);
GUCU_LOCAL unsigned int scm_to_uint (SCM x);
GUCU_LOCAL unsigned int scm_to_uint8 (SCM x);
GUCU_LOCAL unsigned long scm_to_ulong (SCM x);

#endif /* GUILE_1_POINT_6 */

#endif /* COMPAT_H */
