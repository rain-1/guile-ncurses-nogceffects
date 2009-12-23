## Process this file with automake to produce Makefile.in
ACLOCAL_AMFLAGS = -I m4

# Distcheck flags for GNU/Linux
# DISTCHECK_CONFIGURE_FLAGS = 

# Distcheck flags for Cygwin
# DISTCHECK_CONFIGURE_FLAGS = --without-ncursesw \
--with-cygwin-curses-acs-bug-fix
### FLAGS ###

# -Wextra depends on the gcc version

if WARN
WARN_FLAGS = -std=c99 -pedantic -Wall                                   \
-Wstrict-prototypes -Wmissing-prototypes -Wpointer-arith -Wreturn-type	\
-Wcast-qual -Wswitch -Wshadow -Wcast-align -Wwrite-strings		\
-Wchar-subscripts -Winline -Wnested-externs -Wredundant-decls -Wunused	\
-Wformat=2
else
WARN_FLAGS = 
endif

gucu_libguile_gucu_la_CPPFLAGS = \
	$(WARN_FLAGS) 

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
if DEMO_INSTALL
bin_SCRIPTS = \
	tools/gucu-keyboard-test \
	tools/gucu-vi
else
noinst_SCRIPTS =
	tools/gucu-keyboard-test \
	tools/gucu-vi
endif
dist_pkgguilesite_DATA = \
	gucu/curses.scm \
	gucu/menu.scm \
	gucu/panel.scm \
	gucu/form.scm \
	gucu/eti.scm \
	gucu/slk.scm \
	gucu/lib.scm

### SOURCES ###

gucu_libguile_gucu_la_SOURCES = \
	gucu/compat.c \
	gucu/compat.h \
	gucu/curs_const.c \
	gucu/curs_const.h \
	gucu/curs_func.c \
	gucu/curs_func.h \
	gucu/curs_main.c \
	gucu/curs_port.c \
	gucu/curs_port.h \
	gucu/curs_spec.c \
	gucu/curs_spec.h \
	gucu/eti_const.c \
	gucu/eti_const.h \
	gucu/eti_main.c \
	gucu/features.h \
	gucu/form_const.c \
	gucu/form_const.h \
	gucu/form_func.c \
	gucu/form_func.h \
	gucu/form_main.c \
	gucu/form_spec.c \
	gucu/form_spec.h \
	gucu/form_type.c \
	gucu/form_type.h \
	gucu/menu_const.c \
	gucu/menu_const.h \
	gucu/menu_func.c \
	gucu/menu_func.h \
	gucu/menu_main.c \
	gucu/menu_spec.c \
	gucu/menu_spec.h \
	gucu/menu_type.c \
	gucu/menu_type.h \
	gucu/panel_func.c \
	gucu/panel_func.h \
	gucu/panel_main.c \
	gucu/panel_spec.c \
	gucu/panel_spec.h \
	gucu/panel_type.c \
	gucu/panel_type.h \
	gucu/slk_func.c \
	gucu/slk_func.h \
	gucu/type.c \
	gucu/type.h \
	gucu/unicode.c \
	gucu/unicode.h

tools_gucushell_SOURCES = \
	tools/shell.c

### LINKER ###

gucu_libguile_gucu_la_LIBADD = 

tools_gucushell_LDADD = -lutil

### OTHER RULES ###

### CHECK ###

TESTLIB = \
	test/lib.scm \
	test/lib2.scm

TESTS_ENVIRONMENT = GUILE_AUTO_COMPILE=0 \
	./libtool -dlopen gucu/libguile-gucu.la \
	--mode=execute \
	guile -L $(abs_top_srcdir) -s 

NARROWTESTS = \
	test/curses_000_test_setup.test \
	test/curses_001_char_conversion.test \
	test/curses_002_initscr.test \
	test/curses_003_color.test \
	test/curses_004_characters.test \
	test/curses_005_addch.test \
	test/curses_006_addchstr.test \
	test/curses_007_addstr.test \
	test/curses_008_attr.test \
	test/curses_009_beep.test \
	test/curses_010_bkgd.test \
	test/curses_011_border.test \
	test/curses_012_clear.test \
	test/curses_013_delch.test \
	test/curses_014_deleteln.test \
	test/curses_015_getch.test \
	test/curses_016_getstr.test \
	test/curses_017_getyx.test \
	test/curses_018_inch.test \
	test/curses_019_inchstr.test \
	test/curses_020_inopts.test \
	test/curses_021_insch.test \
	test/curses_022_insstr.test \
	test/curses_023_instr.test \
	test/curses_024_kernel.test \
	test/curses_025_move.test \
	test/curses_026_outputs.test \
	test/curses_027_overlay.test \
	test/curses_028_pad.test \
	test/curses_029_refresh.test \
	test/curses_030_scr_dump.test \
	test/curses_031_scroll.test \
	test/curses_032_termattrs.test \
	test/curses_033_touch.test \
	test/curses_034_util.test \
	test/curses_035_attributes.test \
	test/curses_036_ports.test \
	test/curses_037_mouse.test \
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
	test/m011_menu_options.test \
	test/slk_001_init.test \
	test/slk_002_init.test \
	test/slk_003_init.test \
	test/slk_004_init.test \
	test/slk_005_demo.test

WIDETESTS = \
	test/r037_border_set.test \
	test/r038_get_wch.test \
	test/r039_get_wstr.test \
	test/r040_term_attrs.test

TESTS = $(NARROWTESTS)
if WIDE
#  TESTS += $(WIDETESTS)
endif

### DOCUMENTATION ###

# Cygwin's texi2dvi can try to call etex, resulting in this error:
#   "texinfo.tex appears to be broken, quitting"
# The problem lies in Cygwin's etex.fmt file
# Forcing TEX to tex works around this problem
# This should not harm other platforms
if FORCE_TEX_TO_TEX
TEX=tex
TEXI2DVI= TEX=${TEX} texi2dvi
endif

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
	tools/uninstalled-env.in \
	tools/gucu-keyboard-test \
	tools/gucu-vi \
	$(TESTLIB) \
	$(TESTS)

### INSTALLATION ###

