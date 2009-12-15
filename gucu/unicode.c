#include <config.h>
#include "unicode.h"
#include "features.h"

#ifdef GUILE_CHARS_ARE_UCS4
#include <assert.h>
#include <langinfo.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <uniconv.h>

/* Work around bug in libunistring 0.9.1.1 by
   defining _UNUSED_PARAMETER_ */
#define _UNUSED_PARAMETER_
#include <unistr.h>

#ifdef __STDC_ISO_10646__
static const int stdc_iso_10646 = 1;
#else
static const int stdc_iso_10646 = 0;
#endif

#ifdef DEBUG_UNICODE
static const int debug_unicode = 1;
#else
static const int debug_unicode = 0;
#endif

int
locale_char_to_codepoint (char c, uint32_t *p_codepoint)
{
  char locale_str[2];
  uint32_t *u32_str;

  if (debug_unicode)
    printf ("Entering locale_char_to_codepoint c = %c\n", c);

  assert (p_codepoint != (uint32_t *) NULL);
  
  if (c == '\0')
    {
      if (debug_unicode)
	printf ("Exit (success) codepoint = zero\n");

      *p_codepoint = 0;
      return 1;
    }
  locale_str[0] = c;
  locale_str[1] = '\0';
  u32_str = u32_strconv_from_locale (locale_str);
  if (u32_str == NULL)
    {
      if (debug_unicode)
	printf ("Exit (failure, u32str is null)\n");

      return 0;
    }
  if (u32_strlen (u32_str) != 1)
    {
      if (debug_unicode)
	printf ("Exit (failure, u32str has %d chars)\n", u32_strlen(u32_str));

      *p_codepoint = '\0';
      free (u32_str);
      return 0;
    }
  *p_codepoint = u32_str[0];
  free (u32_str);

  if (debug_unicode)
    printf ("Exit (success) codepoint is %d\n", *p_codepoint);

  return 1;
}

int
wchar_to_codepoint (wchar_t c, uint32_t *p_codepoint)
{
  wchar_t wchar_str[2];
  uint32_t *u32_str;
  size_t u32_len = 0;

  assert (p_codepoint != (uint32_t *) NULL);

  if (stdc_iso_10646)
    {
      *p_codepoint = c;
      return 1;
    }

  wchar_str[0] = c;
  wchar_str[1] = (wchar_t) 0;
  u32_str = u32_conv_from_encoding ("WCHAR_T", 
                                    iconveh_error,
                                    (const char *) wchar_str, 
                                    sizeof(wchar_t),
                                    NULL,
                                    NULL,
                                    &u32_len);
  if (u32_str == NULL)
    {
      *p_codepoint = 0;
      return 0;
    }
  if (u32_len != 1)
    {
      *p_codepoint = 0;
      free (u32_str);
      return 0;
    }
  *p_codepoint = u32_str[0];
  free (u32_str);
  return 1;
}

int
codepoint_to_locale_char (uint32_t codepoint, char *p_c)
{
  uint32_t u32_str[2];
  char *str;
  size_t str_len = 0;
  char *enc;

  assert (p_c != (char *)NULL);

  if (debug_unicode)
    printf ("Entering codepoint_to_locale_char: cp = %u\n", codepoint);

  if (codepoint == 0)
    {
      if (debug_unicode)
	printf ("Exit (success) c = zero\n");

      *p_c = '\0';
      return 1;
    }
  u32_str[0] = codepoint;
  u32_str[1] = 0;
  enc = nl_langinfo (CODESET);
  str = u32_conv_to_encoding (enc,
                              iconveh_error,
                              u32_str,
                              1,
                              NULL,
                              NULL,
                              &str_len);
  if (str == NULL)
    {
      if (debug_unicode)
	printf ("Exit (failure, str is null)\n");

      return 0;
    }
  if (str_len != 1)
    {
      if (debug_unicode)
	printf ("Exit (failure, str has %d chars)\n", str_len);

      *p_c = '\0';
      free (str);
      return 0;
    }

  *p_c = str[0];
  if (debug_unicode)
    printf ("Exit (success) char is %d\n", *p_c);

  free (str);
  return 1;
}

int
codepoint_to_wchar (uint32_t codepoint, wchar_t *p_c)
{
  uint32_t u32_str[2];
  wchar_t *wchar_str;
  size_t wchar_len = 0;
  
  assert (p_c != (wchar_t *) NULL);
  assert (codepoint <= 0x10FFFF);

  if (codepoint == 0)
    {
      *p_c = 0;
      return 1;
    }

  if (stdc_iso_10646)
    {
      if ((SIZEOF_WCHAR_T == 4)
	  || 
	  (SIZEOF_WCHAR_T == 2 && codepoint <= 0xFFFF))
	{
          *p_c = codepoint;
          return 1;
        }
      else
        {
          *p_c = 0;
          return 0;
        }
    }

  u32_str[0] = codepoint;
  u32_str[1] = 0;
  wchar_str = (wchar_t *) u32_conv_to_encoding ("WCHAR_T",
                                                iconveh_error,
                                                u32_str,
                                                1,
                                                NULL,
                                                NULL,
                                                &wchar_len);
  if (wchar_str == NULL)
    {
      *p_c = 0;
      return 0;
    }
  if (wchar_len != SIZEOF_WCHAR_T)
    {
      /* Fail, because we didn't receive exactly one char. */
      *p_c = 0;
      free (wchar_str);
      return 0;
    }
  *p_c = wchar_str[0];
  free (wchar_str);
  return 1;
}

#endif
