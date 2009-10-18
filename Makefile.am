## Process this file with automake to produce Makefile.in
ACLOCAL_AMFLAGS = -I m4 --install

# Distcheck flags for GNU/Linux
# DISTCHECK_CONFIGURE_FLAGS = 

# Distcheck flags for Cygwin
# DISTCHECK_CONFIGURE_FLAGS = --without-ncursesw \
--with-cygwin-curses-acs-bug-fix

### FLAGS ###

if WARN
WARN_FLAGS = -std=c99 -pedantic -Werror -Wall -Wextra			\
-Wstrict-prototypes -Wmissing-prototypes -Wpointer-arith -Wreturn-type	\
-Wcast-qual -Wswitch -Wshadow -Wcast-align -Wwrite-strings		\
-Wchar-subscripts -Winline -Wnested-externs -Wredundant-decls -Wunused	\
-Wformat=2
else
WARN_FLAGS = 
endif

gucu_libguile_gucu_la_CPPFLAGS = \
	$(WARN_FLAGS) \
	$(GUILE_CFLAGS)

if WIDE
gucu_libguile_gucu_la_CPPFLAGS += \
	-D_XOPEN_SOURCE_EXTENDED
endif

# Set version flags and indicate that this is meant to be dlopened
gucu_libguile_gucu_la_LDFLAGS = \
	-module \
	-no-undefined \
	-version-info @LIBGUILE_GUCU_CURSES_INTERFACE@

### DIRECTORIES ###

guilesitedir = ${datadir}/guile/site
pkgguilesitedir = $(guilesitedir)/$(PACKAGE)

### MAIN TARGETS ###

lib_LTLIBRARIES = \
	gucu/libguile-gucu.la
bin_PROGRAMS = \
	tools/gucushell
if GUCUKEY
bin_SCRIPTS = \
	tools/gucukey
endif
dist_pkgguilesite_DATA = \
	gucu/curses.scm \
	gucu/menu.scm \
	gucu/panel.scm \
	gucu/form.scm \
	gucu/eti.scm \
	gucu/slk.scm

if WIDE
dist_pkgguilesite_DATA += \
	gucu/curses-wide.scm
endif

### SOURCES ###

gucu_libguile_gucu_la_SOURCES = \
	gucu/curs_const.c \
	gucu/curs_const.h \
	gucu/curs_func.c \
	gucu/curs_func.h \
	gucu/curs_port.c \
	gucu/curs_port.h \
	gucu/type.c \
	gucu/type.h \
	gucu/curs_spec.c \
	gucu/curs_spec.h \
	gucu/curs_main.c \
	gucu/eti_const.c \
	gucu/eti_const.h \
	gucu/eti_main.c \
	gucu/slk_func.c \
	gucu/slk_func.h \
	gucu/unicode.c \
	gucu/unicode.h

# 	gucu/menu_const.c \
# 	gucu/menu_const.h \
# 	gucu/menu_func.c \
# 	gucu/menu_func.h \
# 	gucu/menu_type.c \
# 	gucu/menu_type.h \
# 	gucu/menu_spec.c \
# 	gucu/menu_spec.h \
# 	gucu/menu_main.c \
# 	gucu/panel_func.c \
# 	gucu/panel_func.h \
# 	gucu/panel_type.c \
# 	gucu/panel_type.h \
# 	gucu/panel_spec.c \
# 	gucu/panel_spec.h \
# 	gucu/panel_main.c \
# 	gucu/form_const.c \
# 	gucu/form_const.h \
# 	gucu/form_func.c \
# 	gucu/form_func.h \
# 	gucu/form_type.c \
# 	gucu/form_type.h \
# 	gucu/form_spec.c \
# 	gucu/form_spec.h \
# 	gucu/form_main.c 

if WIDE
# gucu_libguile_gucu_la_SOURCES += \
# 	gucu/cursw_const.c \
# 	gucu/cursw_const.h \
# 	gucu/cursw_func.c \
# 	gucu/cursw_func.h \
# 	gucu/typew.c \
# 	gucu/typew.h \
# 	gucu/cursw_spec.c \
# 	gucu/cursw_spec.h \
# 	gucu/cursw_main.c
endif

tools_gucushell_SOURCES = \
	tools/shell.c

### LINKER ###

gucu_libguile_gucu_la_LIBADD = \
	@LIBMENU@ @LIBPANEL@ @LIBFORM@ @LIBNCURSES@ $(GUILE_LIBS)

tools_gucushell_LDADD = \
	@LIBNCURSES@ -lutil -ltinfo $(GUILE_LIBS)

### OTHER RULES ###

### CHECK ###

TESTLIB = \
	test/lib.scm \
	test/lib2.scm

TESTS_ENVIRONMENT = ./libtool -dlopen gucu/libguile-gucu.la \
	--mode=execute \
	guile -L $(abs_top_srcdir) -s 

NARROWTESTS = \
	test/r000_test.test \
	test/r000_initscr.test \
	test/r001_color.test \
	test/r002_characters.test \
	test/r003_addch.test \
	test/r004_addchstr.test \
	test/r005_addstr.test \
	test/r006_attr.test \
	test/r007_beep.test \
	test/r008_bkgd.test \
	test/r009_border.test \
	test/r010_clear.test \
	test/r011_delch.test \
	test/r012_deleteln.test \
	test/r013_getch.test \
	test/r014_getstr.test \
	test/r015_getyx.test \
	test/r016_inch.test \
	test/r017_inchstr.test \
	test/r018_inopts.test \
	test/r019_insch.test \
	test/r020_insstr.test \
	test/r021_instr.test \
	test/r022_kernel.test \
	test/r023_move.test \
	test/r024_outputs.test \
	test/r025_overlay.test \
	test/r026_pad.test \
	test/r027_refresh.test \
	test/r028_scr_dump.test \
	test/r029_scroll.test \
	test/r030_termattrs.test \
	test/r031_touch.test \
	test/r032_util.test \
	test/r037_border_set.test \
	test/r038_get_wch.test \
	test/r039_get_wstr.test \
	test/r040_term_attrs.test \
	test/r041_ports.test \
	test/r042_mouse.test \
	test/p001_panel.test \
	test/f001_new_form.test \
	test/f002_data.test \
	test/f003_field_attributes.test \
	test/f004_form_field.test \
	test/f005_form_page.test \
	test/f006_form_field_just.test \
	test/f007_form_field_opts.test \
	test/f008_form_field_buffer.test \
	test/m001_new_menu.test \
	test/m002_mitem_current.test \
	test/m003_mitem_name.test \
	test/m004_mitem_opts.test \
	test/m005_mitem_value.test \
	test/m006_mitem_visible.test \
	test/m007_menu_attributes.test \
	test/m008_menu_cursor.test \
	test/m009_menu_format.test \
	test/m010_menu_mark.test \
	test/m011_menu_options.test

# WIDETESTS = \
# 	test/r033_add_wch.test \
# 	test/r034_add_wchstr.test \
# 	test/r035_addwstr.test \
# 	test/r036_bkgrnd.test \
# 	test/r037_border_set.test \
# 	test/r038_get_wch.test \
# 	test/r039_get_wstr.test \
# 	test/r040_term_attrs.test

TESTS = $(NARROWTESTS)
if WIDE
# TESTS += $(WIDETESTS)
endif

### DOCUMENTATION ###

# Cygwin's texi2dvi can try to call etex, resulting in this error:
#   "texinfo.tex appears to be broken, quitting"
# The problem lies in Cygwin's etex.fmt file
# Forcing TEX to tex works around this problem
# This should not harm other platforms
# TEX=tex
# TEXI2DVI = TEX=${TEX} texi2dvi

info_TEXINFOS = \
	doc/gucu.texi
doc_gucu_TEXINFOS = \
	doc/ex_attrib.scm \
	doc/ex_blink.scm \
	doc/ex_centered_screen.scm \
	doc/ex_color.scm \
	doc/ex_getnstr.scm \
	doc/ex_hello_world.scm \
	doc/ex_hline.scm \
	doc/ex_init_func_usage.scm \
	doc/ex_menu.scm \
	doc/ex_menu2.scm \
	doc/ex_menu3.scm \
	doc/ex_panel.scm \
	doc/ex_window.scm \
	doc/ex_form.scm \
	doc/ex_form2.scm

### DISTRIBUTION ###

# Note that the "wide" .scm.in file needs to appear in EXTRA_DIST.  It
# is part of the distribution regardless of whether it is used.
EXTRA_DIST = \
	AUTHORS \
	INSTALL \
	NEWS \
	README \
	COPYING \
	ChangeLog \
	tools/gucukey \
	tools/pre-inst-gucushell \
	tools/pre-inst-gucukey \
	$(TESTLIB) \
	$(TESTS)

# We don't distcheck this script, because it won't run in the build
# environment without being prepended by
# "libtool -dlopen ../gucu/libguile-gucu.la --mode=execute"

DISTCHECK_CONFIGURE_FLAGS = --disable-gucukey

### INSTALLATION ###

