#include <config.h>
#include "form_func.h"
#include "form_spec.h"
#include "form_const.h"
#include "form_type.h"
#include "visibility.h"

GUCU_API void gucu_form_init (void);

void
gucu_form_init ()
{
  gucu_form_init_type ();
  gucu_form_init_function ();
  gucu_form_init_special ();
  gucu_form_init_constant ();
}
