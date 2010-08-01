#ifndef MENU_TYPE_H
#define MENU_TYPE_H

#include <libguile.h>
#include <menu.h>
#include "visibility.h"

GUCU_LOCAL int _scm_is_item (SCM x);
GUCU_LOCAL ITEM *_scm_to_item (SCM x);
GUCU_LOCAL SCM _scm_from_item (ITEM * x);

GUCU_API SCM gucu_is_item_p (SCM x);
GUCU_API SCM gucu_new_item (SCM x, SCM y);

struct gucu_menu
{
  // Pointer to the menu
  MENU *menu;

  /* Guardians to hold the items, window, and subwindow SCM objects */
  SCM items_guard;
  SCM win_guard;
  SCM subwin_guard;
};

GUCU_LOCAL int _scm_is_menu (SCM x);
GUCU_LOCAL MENU *_scm_to_menu (SCM x);
GUCU_LOCAL SCM _scm_from_menu (MENU * x);

GUCU_API SCM gucu_is_menu_p (SCM x);
GUCU_API SCM gucu_new_menu (SCM x);

GUCU_LOCAL void gucu_menu_init_type (void);

#endif
