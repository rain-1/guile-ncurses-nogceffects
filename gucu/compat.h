#ifndef COMPAT_H
#define COMPAT_H

#include <libguile.h>
#include "features.h"

#ifdef GUILE_1_POINT_6

void scm_assert_smob_type (scm_t_bits tag, SCM val);
int scm_c_string_length (SCM x);
SCM scm_c_string_ref (SCM x, size_t k);
SCM scm_from_bool (int x);
SCM scm_from_int (int x);
SCM scm_from_locale_string (const char *str);
SCM scm_from_locale_symbol (const char *sym);
SCM scm_from_short (short x);
SCM scm_from_uint (unsigned int x);
SCM scm_from_ulong (unsigned long x);
void *scm_gc_malloc (size_t size, const char *what);
int scm_is_bool (SCM x);
int scm_is_integer (SCM x);
int scm_is_number (SCM x);
int scm_is_string (SCM x);
int scm_is_symbol (SCM x);
int scm_is_true (SCM x);
void *scm_malloc (size_t x);
int scm_to_bool (SCM x);
int scm_to_int (SCM x);
char *scm_to_locale_string (SCM x);
short scm_to_short (SCM x);
size_t scm_to_size_t (SCM x);
unsigned int scm_to_uint (SCM x);
unsigned int scm_to_uint8 (SCM x);
unsigned long scm_to_ulong (SCM x);

#endif /* GUILE_1_POINT_6 */

#endif /* COMPAT_H */
