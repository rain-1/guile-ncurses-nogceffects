
#ifndef PANEL_FUNC_H
#define PANEL_FUNC_H

#include <libguile.h>
#include "visibility.h"

GUCU_API SCM gucu_bottom_panel (SCM arg1);
GUCU_API SCM gucu_top_panel (SCM arg1);
GUCU_API SCM gucu_show_panel (SCM arg1);
GUCU_API SCM gucu_update_panels (void);
GUCU_API SCM gucu_hide_panel (SCM arg1);
GUCU_API SCM gucu_panel_window (SCM arg1);
GUCU_API SCM gucu_replace_panel (SCM arg1, SCM arg2);
GUCU_API SCM gucu_move_panel (SCM arg1, SCM arg2, SCM arg3);
GUCU_API SCM gucu_panel_hidden_p (SCM arg1);

GUCU_LOCAL void gucu_panel_init_function (void);
#endif

