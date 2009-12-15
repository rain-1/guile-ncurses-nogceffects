#ifndef TYPE_H
#define TYPE_H 1

#include <curses.h>
#include <libguile.h>
#include <stddef.h>
#include <stdio.h>
#include <wchar.h>

#include <config.h>

#ifdef DLL_EXPORT
#define API __attribute__ ((dllexport, cdecl))
#else
#define API
#endif

/*****************************************************************************/
/* CHARACTERS                                                                */

#define GUCU_REPLACEMENT_CHAR ('?')
#define GUCU_REPLACEMENT_CODEPOINT (0xFFFD)
#ifdef __STDC_ISO_10646__
#define GUCU_REPLACEMENT_WCHAR (0xFFFD)
#else
/* This value may be incorrect */
#define GUCU_REPLACEMENT_WCHAR (0xFFFD)
#endif

#define GUCU_PRIVATE_USE_START (0xE000)

int _scm_is_xchar (SCM x);

#ifdef HAVE_LIBNCURSESW
SCM _scm_xchar_from_cchar (cchar_t *x);
#endif
SCM _scm_xchar_from_chtype (chtype x);
SCM _scm_schar_from_char (char c);
SCM _scm_schar_from_wchar (wchar_t ch);
SCM gucu_schar_from_char (SCM c) API;
SCM gucu_schar_from_wchar (SCM c) API;
SCM gucu_schar_to_char (SCM c) API;
SCM gucu_scm_to_wchar (SCM c) API;

#ifdef HAVE_LIBNCURSESW
cchar_t *_scm_xchar_to_cchar (SCM x);
#endif
chtype _scm_xchar_to_chtype (SCM x);
wchar_t _scm_schar_to_wchar (SCM x);
char _scm_schar_to_char (SCM x);


/*****************************************************************************/
/* STRINGS                                                                   */

int _scm_is_xstring (SCM x);

#ifdef HAVE_LIBNCURSESW
SCM _scm_sstring_from_wint_string (const wint_t *x);
SCM _scm_sstring_from_wstring (const wchar_t *x);
#endif
char *_scm_sstring_to_locale_string (SCM x);
wchar_t *_scm_sstring_to_wstring (SCM x);
SCM _scm_xstring_from_chstring (const chtype *x);
#ifdef HAVE_LIBNCURSESW
SCM _scm_xstring_from_cstring (const cchar_t *x);
#endif
chtype *_scm_xstring_to_chstring (SCM x);  
#ifdef HAVE_LIBNCURSESW
cchar_t *_scm_xstring_to_cstring (SCM x);
#endif

/*****************************************************************************/

int _scm_is_attr (SCM x);
attr_t _scm_to_attr (SCM x) ;
SCM _scm_from_attr (attr_t x);

int _scm_is_chtype (SCM x);
chtype _scm_to_chtype (SCM x);
SCM _scm_from_chtype (chtype x);

int _scm_is_chstring (SCM x);
chtype *_scm_to_chstring (SCM x);
SCM _scm_from_chstring (chtype *x);

int  _scm_is_file (SCM x);
FILE *_scm_to_file (SCM x);
SCM _scm_from_file (FILE *x);

int _scm_is_mevent (SCM x);
MEVENT *_scm_to_mevent (SCM x);
SCM _scm_from_mevent (MEVENT *);

int  _scm_is_screen (SCM x);
SCREEN *_scm_to_screen (SCM x);
SCM _scm_from_screen (SCREEN *x);
SCM gucu_is_screen_p (SCM x) API;

int _scm_is_window (SCM x);
WINDOW *_scm_to_window (SCM x);
SCM _scm_from_window (WINDOW *x);

SCM gucu_is_window_p (SCM x) API;

void gucu_init_type (void) API;

#endif /* not TYPE_H */
