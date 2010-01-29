#ifndef GUCUCONFIG_H
#define GUCUCONFIG_H

#include <libguile.h>
#include "config.h"

/* Macros that describe the features of this build of GuCu.  */
#ifdef HAVE___ATTRIBUTE__
#define UNUSED __attribute__ ((unused))
#else
#define UNUSED
#endif

/* For 1.6.x, some compatibility functions are required */
#if SCM_MAJOR_VERSION == 1 && SCM_MINOR_VERSION == 6
#define GUILE_1_POINT_6
#else
#undef GUILE_1_POINT_6
#endif

/* After 1.9.4, characters are codepoints.  */
#if SCM_MAJOR_VERSION > 1				 \
  || (SCM_MAJOR_VERSION == 1 && SCM_MINOR_VERSION > 9)	 \
  || (SCM_MAJOR_VERSION == 1 && SCM_MINOR_VERSION == 9 && SCM_MICRO_VERSION >= 4)
#define GUILE_CHARS_ARE_UCS4
#else
#undef GUILE_CHARS_ARE_UCS4
#endif

#endif
