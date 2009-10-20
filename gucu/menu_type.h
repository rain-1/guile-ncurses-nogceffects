#ifndef MENU_TYPE_H
#define MENU_TYPE_H

#include <libguile.h>
#include <menu.h>

extern scm_t_bits item_tag;

int _scm_is_item (SCM x);
ITEM *_scm_to_item (SCM x);
SCM _scm_from_item (ITEM *x);

SCM gucu_is_item_p (SCM x) API;
void gucu_item_init_type (void);

SCM gucu_new_item (SCM x, SCM y) API;

extern scm_t_bits menu_tag;

struct gucu_menu {
  // Pointer to the menu
  MENU *menu;

  /* Guardians to hold the items, window, and subwindow SCM objects */
  SCM items_guard;
  SCM win_guard;
  SCM subwin_guard;
};



int _scm_is_menu (SCM x);
MENU *_scm_to_menu (SCM x);
SCM _scm_from_menu (MENU *x);

SCM gucu_is_menu_p (SCM x) API;
void gucu_menu_init_type (void);

SCM gucu_new_menu (SCM x) API;

#endif
