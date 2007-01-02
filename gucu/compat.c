#include <config.h>
#include <libguile.h>
#include <string.h>

#include "compat.h"

#ifndef HAVE_SCM_ASSERT_SMOB_TYPE
void scm_assert_smob_type (scm_t_bits tag, SCM val)
{
  SCM_ASSERT (SCM_SMOB_PREDICATE(tag, val), val, 0, "scm_assert_smob_type");
}
#endif

#ifndef HAVE_SCM_C_STRING_LENGTH
int scm_c_string_length (SCM x)
{
  return SCM_STRING_LENGTH(x);
}
#endif

#ifndef HAVE_SCM_C_STRING_REF
SCM scm_c_string_ref (SCM x, size_t k)
{
  return scm_string_ref (x, scm_int2num (k));
}
#endif

#ifndef HAVE_SCM_FROM_BOOL
SCM scm_from_bool (int x)
{
  return SCM_BOOL(x);
}
#endif

#ifndef HAVE_SCM_FROM_INT
SCM scm_from_int (int x)
{
  return scm_int2num (x);
}
#endif

#ifndef HAVE_SCM_FROM_LOCALE_STRING
SCM scm_from_locale_string (const char *str)
{  
  return scm_makfrom0str (str);
}
#endif

#ifndef HAVE_SCM_FROM_LOCALE_SYMBOL
SCM scm_from_locale_symbol (const char *sym)
{
  return scm_string_to_symbol (scm_makfrom0str (sym));
}
#endif

#ifndef HAVE_SCM_FROM_SHORT
SCM scm_from_short (short x)
{
  return scm_short2num (x);
}
#endif

#ifndef HAVE_SCM_FROM_UINT
SCM scm_from_uint (unsigned int x)
{
  return scm_uint2num (x);
}
#endif

#ifndef HAVE_SCM_FROM_ULONG
SCM scm_from_ulong (unsigned long x)
{
  return scm_ulong2num (x);
}
#endif

#ifndef HAVE_SCM_GC_MALLOC
void *scm_gc_malloc (size_t size, const char *what)
{
  return scm_must_malloc (size, what);
}
#endif

#ifndef HAVE_SCM_IS_BOOL
int scm_is_bool (SCM x)
{
  return SCM_BOOLP (x);
}
#endif

#ifndef HAVE_SCM_IS_INTEGER
int scm_is_integer (SCM x)
{
  return SCM_NFALSEP (scm_integer_p (x));
}
#endif

#ifndef HAVE_SCM_IS_NUMBER
int scm_is_number (SCM x)
{
  return SCM_NUMBERP (x);
}
#endif

#ifndef HAVE_SCM_IS_STRING
int scm_is_string (SCM x)
{
  return SCM_STRINGP (x);
}
#endif

#ifndef HAVE_SCM_IS_SYMBOL
int scm_is_symbol (SCM x)
{
  return SCM_SYMBOLP (x);
}
#endif

#ifndef HAVE_SCM_IS_TRUE
int scm_is_true (SCM x)
{
  return SCM_NFALSEP (x);
}
#endif

#ifndef HAVE_SCM_MALLOC
void *scm_malloc (size_t x)
{
  return scm_must_malloc (x, "memory");
}
#endif

#ifndef HAVE_SCM_TO_BOOL
int scm_to_bool (SCM x)
{
  return SCM_NFALSEP (x);
}
#endif

#ifndef HAVE_SCM_TO_INT
int scm_to_int (SCM x)
{
  return scm_num2int (x, 0, "scm_to_int");
}
#endif

#ifndef HAVE_SCM_TO_LOCALE_STRING
char *scm_to_locale_string (SCM x)
{
  size_t len;
  char *str;

  len = SCM_STRING_LENGTH (x);
  str = (char *) malloc (len + 1);
  if (str == NULL)
    return NULL;
  memcpy (str, SCM_VELTS (x), len);
  str[len] = '\0';  
  return str;
}
#endif

#ifndef HAVE_SCM_TO_SHORT
short scm_to_short (SCM x)
{
  return scm_num2short (x, 0, "scm_to_short");
}
#endif

#ifndef HAVE_SCM_TO_SIZE_T
size_t scm_to_size_t (SCM x)
{
  return scm_num2size (x, 0, "scm_to_size_t");
}
#endif

#ifndef HAVE_SCM_TO_UINT
unsigned int scm_to_uint (SCM x)
{
  return scm_num2uint (x, 0, "scm_to_uint");
}
#endif

#ifndef HAVE_SCM_TO_UINT8
unsigned int scm_to_uint8 (SCM x)
{
  unsigned int val = scm_num2uint (x, 0, "scm_to_uint");
  if (val > 0xFF)
    scm_out_of_range ("scm_to_uint8", x);
  return val;
}
#endif

#ifndef HAVE_SCM_TO_ULONG
unsigned long scm_to_ulong (SCM x)
{
  return scm_num2ulong (x, 0, "scm_to_ulong");
}
#endif

