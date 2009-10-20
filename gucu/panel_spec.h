#ifndef PANEL_SPEC_H
#define PANEL_SPEC_H

#include <libguile.h>

#if 0
SCM gucu_panel_above (SCM pan);
SCM gucu_panel_below (SCM pan);
SCM gucu_set_panel_userdata (SCM pan, SCM data);
SCM gucu_panel_userdata (SCM pan);
#endif

void gucu_panel_init_special (void);

#endif
