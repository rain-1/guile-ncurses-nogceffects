
#ifndef MENU_FUNC_H
#define MENU_FUNC_H

#include <libguile.h>
#include "visibility.h"

GUCU_API SCM gucu_set_menu_fore (SCM arg1, SCM arg2);
GUCU_API SCM gucu_menu_fore (SCM arg1);
GUCU_API SCM gucu_set_menu_back (SCM arg1, SCM arg2);
GUCU_API SCM gucu_menu_back (SCM arg1);
GUCU_API SCM gucu_set_menu_grey (SCM arg1, SCM arg2);
GUCU_API SCM gucu_menu_grey (SCM arg1);
GUCU_API SCM gucu_set_menu_pad (SCM arg1, SCM arg2);
GUCU_API SCM gucu_menu_pad (SCM arg1);
GUCU_API SCM gucu_pos_menu_cursor (SCM arg1);
GUCU_API SCM gucu_menu_driver (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_menu_format (SCM arg1, SCM arg2, SCM arg3);
GUCU_API SCM gucu_item_count (SCM arg1);
GUCU_API SCM gucu_set_menu_mark (SCM arg1, SCM arg2);
GUCU_API SCM gucu_menu_mark (SCM arg1);
GUCU_API SCM gucu_set_menu_opts (SCM arg1, SCM arg2);
GUCU_API SCM gucu_menu_opts_off (SCM arg1, SCM arg2);
GUCU_API SCM gucu_menu_opts_on (SCM arg1, SCM arg2);
GUCU_API SCM gucu_menu_opts (SCM arg1);
GUCU_API SCM gucu_set_menu_pattern (SCM arg1, SCM arg2);
GUCU_API SCM gucu_menu_pattern (SCM arg1);
GUCU_API SCM gucu_post_menu (SCM arg1);
GUCU_API SCM gucu_unpost_menu (SCM arg1);
GUCU_API SCM gucu_menu_request_name (SCM arg1);
GUCU_API SCM gucu_menu_request_by_name (SCM arg1);
GUCU_API SCM gucu_set_menu_spacing (SCM arg1, SCM arg2, SCM arg3, SCM arg4);
GUCU_API SCM gucu_set_menu_win (SCM arg1, SCM arg2);
GUCU_API SCM gucu_set_menu_sub (SCM arg1, SCM arg2);
GUCU_API SCM gucu_menu_win (SCM arg1);
GUCU_API SCM gucu_menu_sub (SCM arg1);
GUCU_API SCM gucu_set_current_item (SCM arg1, SCM arg2);
GUCU_API SCM gucu_current_item (SCM arg1);
GUCU_API SCM gucu_set_top_row (SCM arg1, SCM arg2);
GUCU_API SCM gucu_top_row (SCM arg1);
GUCU_API SCM gucu_item_index (SCM arg1);
GUCU_API SCM gucu_item_name (SCM arg1);
GUCU_API SCM gucu_item_description (SCM arg1);
GUCU_API SCM gucu_set_item_opts (SCM arg1, SCM arg2);
GUCU_API SCM gucu_item_opts_on (SCM arg1, SCM arg2);
GUCU_API SCM gucu_item_opts_off (SCM arg1, SCM arg2);
GUCU_API SCM gucu_item_opts (SCM arg1);
GUCU_API SCM gucu_set_item_value (SCM arg1, SCM arg2);
GUCU_API SCM gucu_item_value (SCM arg1);
GUCU_API SCM gucu_item_visible_p (SCM arg1);

GUCU_LOCAL void gucu_menu_init_function (void);
#endif
