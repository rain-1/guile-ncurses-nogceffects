#include <config.h>
#include "eti_const.h"
#include "visibility.h"

GUCU_API void gucu_eti_init (void);

void
gucu_eti_init ()
{
  gucu_eti_init_constant ();
}
