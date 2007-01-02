#ifndef UNICODE_H
#define UNICODE_H

#define GUCU_REPLACEMENT_CHAR ('?')
#define GUCU_REPLACEMENT_CODEPOINT (0xFFFD)
#ifdef __STDC_ISO_10646__
#define GUCU_REPLACEMENT_WCHAR (0xFFFD)
#else
/* This value may be incorrect */
#define GUCU_REPLACEMENT_WCHAR (0xFFFD)
#endif

#define GUCU_PRIVATE_USE_START (0xE000)
#define GUCU_PRIVATE_USE_END (0xE0FF)

#ifdef HAVE_LIBUNISTRING
int locale_char_to_codepoint (char c, uint32_t *p_codepoint);
int wchar_to_codepoint (wchar_t c, uint32_t *p_codepoint);
int codepoint_to_locale_char (uint32_t codepoint, char *p_c);
int codepoint_to_wchar (uint32_t codepoint, wchar_t *p_c);
#endif /* GUILE_CHARS_ARE_UCS4 */

#endif
