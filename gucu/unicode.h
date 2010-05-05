#ifndef UNICODE_H
#define UNICODE_H

#include "gucuconfig.h"

#ifdef GUILE_CHARS_ARE_UCS4
#include <stddef.h>
#include <stdint.h>
#include "visibility.h"

GUCU_LOCAL int locale_char_to_codepoint (char c, uint32_t * p_codepoint);
GUCU_LOCAL int wchar_to_codepoint (wchar_t c, uint32_t * p_codepoint);
GUCU_LOCAL int codepoint_to_locale_char (uint32_t codepoint, char *p_c);
GUCU_LOCAL int codepoint_to_wchar (uint32_t codepoint, wchar_t * p_c);
#endif /* GUILE_CHARS_ARE_UCS4 */

#endif
