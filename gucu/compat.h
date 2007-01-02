#ifndef BACKPORT_H
#define BACKPORT_H

#include <config.h>
#include <libguile.h>

#ifndef HAVE_SCM_ASSERT_SMOB_TYPE
void scm_assert_smob_type (scm_t_bits tag, SCM val);
#endif

#ifndef HAVE_SCM_C_STRING_LENGTH
int scm_c_string_length (SCM x);
#endif

#ifndef HAVE_SCM_C_STRING_REF
SCM scm_c_string_ref (SCM x, size_t k);
#endif

#ifndef HAVE_SCM_FROM_BOOL
SCM scm_from_bool (int x);
#endif

#ifndef HAVE_SCM_FROM_INT
SCM scm_from_int (int x);
#endif

#ifndef HAVE_SCM_FROM_LOCALE_STRING
SCM scm_from_locale_string (const char *str);
#endif

#ifndef HAVE_SCM_FROM_LOCALE_SYMBOL
SCM scm_from_locale_symbol (const char *sym);
#endif

#ifndef HAVE_SCM_FROM_SHORT
SCM scm_from_short (short x);
#endif

#ifndef HAVE_SCM_FROM_UINT
SCM scm_from_uint (unsigned int x);
#endif

#ifndef HAVE_SCM_FROM_ULONG
SCM scm_from_ulong (unsigned long x);
#endif

#ifndef HAVE_SCM_GC_MALLOC
void *scm_gc_malloc (size_t size, const char *what);
#endif 

#ifndef HAVE_SCM_IS_BOOL
int scm_is_bool (SCM x);
#endif

#ifndef HAVE_SCM_IS_INTEGER
int scm_is_integer (SCM x);
#endif

#ifndef HAVE_SCM_IS_NUMBER
int scm_is_number (SCM x);
#endif

#ifndef HAVE_SCM_IS_STRING
int scm_is_string (SCM x);
#endif

#ifndef HAVE_SCM_IS_SYMBOL
int scm_is_symbol (SCM x);
#endif

#ifndef HAVE_SCM_IS_TRUE
int scm_is_true (SCM x);
#endif

#ifndef HAVE_SCM_MALLOC
void *scm_malloc (size_t x);
#endif

#ifndef HAVE_SCM_TO_BOOL
int scm_to_bool (SCM x);
#endif

#ifndef HAVE_SCM_TO_INT
int scm_to_int (SCM x);
#endif

#ifndef HAVE_SCM_TO_LOCALE_STRING
char *scm_to_locale_string (SCM x);
#endif

#ifndef HAVE_SCM_TO_SHORT
short scm_to_short (SCM x);
#endif

#ifndef HAVE_SCM_TO_SIZE_T
size_t scm_to_size_t (SCM x);
#endif

#ifndef HAVE_SCM_TO_UINT
unsigned int scm_to_uint (SCM x);
#endif

#ifndef HAVE_SCM_TO_UINT8
unsigned int scm_to_uint8 (SCM x);
#endif

#ifndef HAVE_SCM_TO_ULONG
unsigned long scm_to_ulong (SCM x);
#endif

#endif
