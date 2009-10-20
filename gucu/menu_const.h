
#ifndef MENU_CONST_H
#define MENU_CONST_H

#include <config.h>
#include <libguile.h>
#ifdef DLL_EXPORT
#define API __attribute__ ((dllexport, cdecl))
#else
#define API
#endif

void gucu_menu_init_constant (void) API;
#endif /* not CONSTANT_H */
