#include <config.h>
#include "eti_const.h"

#ifdef DLL_EXPORT
#define API __attribute__ ((dllexport, cdecl))
#else
#define API
#endif

void gucu_eti_init (void) API;

void
gucu_eti_init ()
{
  gucu_eti_init_constant ();
}
