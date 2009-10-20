#include <config.h>
#include "panel_func.h"
#include "panel_spec.h"
// #include "panel_const.h"
#include "panel_type.h"

#ifdef DLL_EXPORT
#define API __attribute__ ((dllexport, cdecl))
#else
#define API
#endif

void gucu_panel_init (void) API;

void
gucu_panel_init ()
{
  gucu_panel_init_type ();
  gucu_panel_init_function ();
  gucu_panel_init_special ();
  // gucu_panel_init_constant ();
}
