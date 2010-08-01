#ifndef MENU_SPEC_H
#define MENU_SPEC_H

#include <libguile.h>
#include "visibility.h"

GUCU_API SCM gucu_scale_menu (SCM menu);
GUCU_API SCM gucu_menu_spacing (SCM menu);
GUCU_API SCM gucu_menu_itemlen (SCM menu);
GUCU_API SCM gucu_menu_format (SCM menu);

GUCU_LOCAL void gucu_menu_init_special (void);

#endif
