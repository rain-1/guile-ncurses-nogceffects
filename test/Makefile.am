# Makefile.am for guile-ncurses/test.
#
# Copyright 2009, 2010 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

TESTLIB = lib.scm lib2.scm

TESTS_ENVIRONMENT = GUILE_AUTO_COMPILE=0 ${top_builddir}/libtool	\
-dlopen ${top_builddir}/ncurses/libguile-ncurses.la 			\
--mode=execute guile -L $(abs_top_srcdir) -s

NARROWTESTS = curses_000_test_setup.test				\
curses_001_char_conversion.test curses_002_initscr.test		\
curses_003_color.test curses_004_characters.test curses_005_addch.test	\
curses_006_addchstr.test curses_007_addstr.test curses_008_attr.test	\
curses_009_beep.test curses_010_bkgd.test curses_011_border.test	\
curses_012_clear.test curses_013_delch.test curses_014_deleteln.test	\
curses_015_getch.test curses_016_getstr.test curses_017_getyx.test	\
curses_018_inch.test curses_019_inchstr.test curses_020_inopts.test	\
curses_021_insch.test curses_022_insstr.test curses_023_instr.test	\
curses_024_kernel.test curses_025_move.test curses_026_outputs.test	\
curses_027_overlay.test curses_028_pad.test curses_029_refresh.test	\
curses_030_scr_dump.test curses_031_scroll.test			\
curses_032_termattrs.test curses_033_touch.test curses_034_util.test	\
curses_035_attributes.test curses_036_ports.test curses_037_mouse.test	\
p001_panel.test f001_new_form.test f002_data.test			\
f003_field_attributes.test f004_form_field.test f005_form_page.test	\
f006_form_field_just.test f007_form_field_opts.test			\
f008_form_field_buffer.test m001_new_menu.test m002_mitem_current.test	\
m003_mitem_name.test m004_mitem_opts.test m005_mitem_value.test	\
m006_mitem_visible.test m007_menu_attributes.test			\
m008_menu_cursor.test m009_menu_format.test m010_menu_mark.test	\
m011_menu_options.test slk_001_init.test slk_002_init.test		\
slk_003_init.test slk_004_init.test slk_005_demo.test			\
slk_006_attributes.test

WIDETESTS = r037_border_set.test r038_get_wch.test r039_get_wstr.test	\
r040_term_attrs.test

TESTS = $(NARROWTESTS)
if WIDE
TESTS += $(WIDETESTS)
endif

EXTRA_DIST = $(TESTLIB) $(TESTS)
