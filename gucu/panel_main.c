#include <config.h>
#include "panel_func.h"
#include "panel_spec.h"
#include "panel_type.h"
#include "visibility.h"

GUCU_API void gucu_panel_init (void);

void
gucu_panel_init ()
{
  gucu_panel_init_type ();
  gucu_panel_init_function ();
  gucu_panel_init_special ();
}
