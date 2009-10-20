#include <config.h>
#include "form_func.h"
#include "form_spec.h"
#include "form_const.h"
#include "form_type.h"

#ifdef DLL_EXPORT
#define API __attribute__ ((dllexport, cdecl))
#else
#define API
#endif

void gucu_form_init (void) API;

void
gucu_form_init ()
{
  gucu_form_init_type ();
  gucu_form_init_function ();
  gucu_form_init_special ();
  gucu_form_init_constant ();
}
