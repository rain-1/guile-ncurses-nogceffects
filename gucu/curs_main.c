#include <libguile.h>
#include <curses.h>
#include <config.h>
#include "curs_const.h"
#include "curs_func.h"
#include "curs_port.h"
#include "curs_spec.h"
#include "type.h"
#include "visibility.h"

GUCU_API void gucu_init (void);

void
gucu_init ()
{
  gucu_init_function ();
  gucu_init_special ();
  gucu_init_constant ();
  gucu_init_port ();
  gucu_init_type ();
}
