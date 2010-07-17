## Process this file with automake to produce Makefile.in

### libguile-gucu.la ###

lib_LTLIBRARIES = libguile-gucu.la
libguile_gucu_la_SOURCES = compat.c compat.h curs_const.c curs_const.h	\
	curs_func.c curs_func.h curs_port.c curs_port.h curs_spec.c	\
	curs_spec.h curs_main.c eti_const.c eti_const.h eti_main.c	\
	form_const.c form_const.h form_func.c form_func.h form_spec.c	\
	form_spec.h form_type.c form_type.h form_main.c gucuconfig.h	\
	menu_const.c menu_const.h menu_func.c menu_func.h menu_main.c	\
	menu_spec.c menu_spec.h menu_type.c menu_type.h panel_func.c	\
	panel_func.h panel_main.c panel_spec.c panel_spec.h		\
	panel_type.c panel_type.h slk_func.c slk_func.h type.c type.h	\
	unicode.c unicode.h visibility.h

libguile_gucu_la_CPPFLAGS = -I $(top_builddir)/lib $(GUILE_CFLAGS)

if WARN
libguile_gucu_la_CPPFLAGS += -Wall -Wextra -Wstrict-prototypes		\
-Wmissing-prototypes -Wpointer-arith -Wreturn-type -Wcast-qual		\
-Wswitch -Wshadow -Wcast-align -Wwrite-strings -Wchar-subscripts	\
-Winline -Wnested-externs -Wredundant-decls -Wunused -Wformat=2
endif

if WIDE
libguile_gucu_la_CPPFLAGS += -D_XOPEN_SOURCE_EXTENDED
endif

if BUILDING_DLL
libguile_gucu_la_CPPFLAGS += -DGUCU_DLL -DGUCU_DLL_EXPORTS
endif

libguile_gucu_la_LDFLAGS = -module -no-undefined \
-version-info $(LIBGUILE_GUCU_CURSES_INTERFACE) $(GUILE_LDFLAGS)

libguile_gucu_la_LIBADD = $(CURSES_LIB) $(PANEL_LIB) $(MENU_LIB) $(FORM_LIB)

if WIDE
libguile_gucu_la_LIBADD += $(UNISTRING_LIB)
endif


### scheme files ###
guilesitedir = ${datadir}/guile/site
pkgguilesitedir = $(guilesitedir)/$(PACKAGE)
dist_pkgguilesite_DATA = curses.scm menu.scm panel.scm form.scm	\
	eti.scm slk.scm lib.scm
ETAGS_ARGS = curses.scm menu.scm panel.scm form.scm eti.scm slk.scm	\
	lib.scm
