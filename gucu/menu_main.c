#include <config.h>
#include "menu_func.h"
#include "menu_spec.h"
#include "menu_const.h"
#include "menu_type.h"

#ifdef DLL_EXPORT
#define API __attribute__ ((dllexport, cdecl))
#else
#define API
#endif

void gucu_menu_init (void) API;

void
gucu_menu_init ()
{
  gucu_menu_init_type ();
  gucu_menu_init_function ();
  gucu_menu_init_special ();
  gucu_menu_init_constant ();
}
