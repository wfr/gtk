AM_CPPFLAGS = \
	-I$(top_srcdir)                 \
	-I$(top_srcdir)/gdk             \
	-I$(top_builddir)/gdk           \
	$(GTK_DEBUG_FLAGS)              \
	$(GTK_DEP_CFLAGS)

LDADD = \
	$(top_builddir)/gtk/libgtk-3.la \
	$(top_builddir)/gdk/libgdk-3.la \
	$(GTK_DEP_LIBS)

bin_PROGRAMS = gtk3-font-features

gtk3_font_features_SOURCES = \
	font-features.c \
	resources.c

BUILT_SOURCES = \
	resources.c

resources.c: font-features.gresource.xml $(shell $(GLIB_COMPILE_RESOURCES) --sourcedir=$(srcdir) --generate-dependencies $(srcdir)/font-features.gresource.xml)
	$(AM_V_GEN) $(GLIB_COMPILE_RESOURCES) $(srcdir)/font-features.gresource.xml \
		--target=$@ --sourcedir=$(srcdir) --generate-source

EXTRA_DIST = \
	font-features.gresource.xml \
	font-features.ui

-include $(top_srcdir)/git.mk
