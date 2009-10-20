#ifndef PANEL_TYPE_H
#define PANEL_TYPE_H

#include <libguile.h>
#include <panel.h>
#include <panel.h>

extern scm_t_bits panel_tag;

struct gucu_panel {
  /* Pointer to the panel */
  PANEL *panel;

  /* The window from which the panel was generated */
  SCM window;

  /* Guardians to hold the window SCM objects */
  SCM win_guard;
};


int _scm_is_panel (SCM x);
PANEL *_scm_to_panel (SCM x);
SCM _scm_from_panel (PANEL *x);

SCM gucu_is_panel_p (SCM x) API;
SCM gucu_new_panel (SCM arg1) API;
SCM gucu_del_panel (SCM arg1) API;

void gucu_panel_init_type (void);

#endif
