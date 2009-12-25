#ifndef FUNCTION_H
#define FUNCTION_H

#include <libguile.h>
#include <config.h>
#ifdef DLL_EXPORT
#define API __attribute__ ((dllexport, cdecl))
#else
#define API
#endif

SCM gucu_slk_attr (void) API;
SCM gucu_slk_attr_off_x (SCM arg1) API;
SCM gucu_slk_attr_on_x (SCM arg1) API;
SCM gucu_slk_attr_set_x (SCM arg1, SCM arg2) API;
SCM gucu_slk_attrset (SCM arg1) API;
SCM gucu_slk_clear (void) API;
SCM gucu_slk_color (SCM arg1) API;
SCM gucu_slk_init (SCM arg1) API;
SCM gucu_slk_label (SCM arg1) API;
SCM gucu_slk_noutrefresh (void) API;
SCM gucu_slk_refresh (void) API;
SCM gucu_slk_restore (void) API;
SCM gucu_slk_set (SCM arg1, SCM arg2, SCM arg3) API;
SCM gucu_slk_touch (void) API;

void gucu_slk_init_function (void) API;

#endif
