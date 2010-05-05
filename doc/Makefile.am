# Cygwin's texi2dvi can try to call etex, resulting in this error:
#   "texinfo.tex appears to be broken, quitting"
# The problem lies in Cygwin's etex.fmt file
# Forcing TEX to tex works around this problem
# This should not harm other platforms

#if FORCE_TEX_TO_TEX
#TEX=tex
#TEXI2DVI= TEX=${TEX} texi2dvi
#endif

info_TEXINFOS = gucu.texi
gucu_TEXINFOS = ex_attrib.scm ex_blink.scm ex_centered_screen.scm	\
	ex_color.scm ex_getnstr.scm ex_hello_world.scm ex_hline.scm	\
	ex_init_func_usage.scm ex_menu.scm ex_menu2.scm ex_menu3.scm	\
	ex_panel.scm ex_window.scm ex_form.scm ex_form2.scm
