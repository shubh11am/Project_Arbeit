#
# Targets to build
#

.SILENT:


BUILT_SOURCES			= tcasmlex.cc \
				  tcasmyacc.cc

CLEANFILES			= $(BUILT_SOURCES) \
				  tcasmyacc.hh \
				  location.hh \
				  position.hh \
				  stack.hh

EXTRA_DIST			= tcasmlex.ll tcasmlex.h tcasmyacc.yy


#
# Specific rules
#

tcasmlex.cc: $(srcdir)/tcasmlex.ll
	@LEX@ -o $@ $<

tcasmyacc.cc : $(srcdir)/tcasmyacc.yy
	@BISON@ -d -o $@ $<

dist-hook:
	rm -f $(distdir)/tcasmlex.cc $(distdir)/tcasmyacc.cc
