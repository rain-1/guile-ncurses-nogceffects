#ifndef UNICODE_H
#define UNICODE_H

#ifdef GUILE_CHARS_ARE_UCS4
#include <stddef.h>
#include <stdint.h>
#include "features.h"

int locale_char_to_codepoint (char c, uint32_t *p_codepoint);
int wchar_to_codepoint (wchar_t c, uint32_t *p_codepoint);
int codepoint_to_locale_char (uint32_t codepoint, char *p_c);
int codepoint_to_wchar (uint32_t codepoint, wchar_t *p_c);
#endif /* GUILE_CHARS_ARE_UCS4 */

#endif
