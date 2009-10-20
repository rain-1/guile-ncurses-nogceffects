#ifndef MENU_SPEC_H
#define MENU_SPEC_H

#ifdef DLL_EXPORT
#define API __attribute__ ((dllexport, cdecl))
#else
#define API
#endif

#include <libguile.h>

SCM gucu_scale_menu (SCM menu) API;
SCM gucu_menu_spacing (SCM menu) API;
SCM gucu_menu_itemlen (SCM menu) API;
SCM gucu_menu_format (SCM menu) API;


void gucu_menu_init_special (void);

#endif
