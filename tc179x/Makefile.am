#
# Target to build
#

.SILENT:


#
# Source files of the target
#

BUILT_SOURCES			= tpm.olive tpm.cc tpm.h

CLEANFILES			= $(BUILT_SOURCES)

EXTRA_DIST			= abs_rules.m4 \
				  abs_prolog.m4 \
				  abs_decl.m4 \
				  assignment_rules.m4 \
				  assignment_prolog.m4 \
				  assignment_decl.m4 \
				  casting_rules.m4 \
				  casting_prolog.m4 \
				  casting_decl.m4 \
				  comma_rules.m4 \
				  comma_prolog.m4 \
				  comma_decl.m4 \
				  composed_rules.m4 \
				  composed_prolog.m4 \
				  composed_decl.m4 \
				  composed_initlist_rules.m4 \
				  composed_initlist_prolog.m4 \
				  composed_initlist_decl.m4 \
				  composed_struct_rules.m4 \
				  composed_struct_prolog.m4 \
				  composed_struct_decl.m4 \
				  composed_struct_bitfield_rules.m4 \
				  composed_struct_bitfield_prolog.m4 \
				  composed_struct_bitfield_decl.m4 \
				  longlong_rules.m4 \
				  longlong_prolog.m4 \
				  longlong_decl.m4 \
				  mac_rules.m4 \
				  mac_prolog.m4 \
				  mac_decl.m4 \
				  minmax_rules.m4 \
				  minmax_prolog.m4 \
				  minmax_decl.m4 \
				  relation_rules.m4 \
				  relation_prolog.m4 \
				  relation_decl.m4 \
				  tc_rules.m4 \
				  tc_prolog.m4 \
				  tc_decl.m4 \
				  tpm.m4


#
# Specific rules
#

OLIVE_DEPS			= tpm.m4 tc_rules.m4 tc_prolog.m4 tc_decl.m4 \
				  casting_rules.m4 casting_prolog.m4 \
				  casting_decl.m4 composed_rules.m4 \
				  composed_prolog.m4 composed_decl.m4 \
				  composed_initlist_rules.m4 \
				  composed_initlist_prolog.m4 \
				  composed_initlist_decl.m4 \
				  composed_struct_rules.m4 \
				  composed_struct_prolog.m4 \
				  composed_struct_decl.m4 \
				  composed_struct_bitfield_rules.m4 \
				  composed_struct_bitfield_prolog.m4 \
				  composed_struct_bitfield_decl.m4 \
				  mac_rules.m4 mac_prolog.m4 mac_decl.m4 \
				  longlong_rules.m4 longlong_prolog.m4 \
				  longlong_decl.m4 assignment_rules.m4 \
				  assignment_prolog.m4 assignment_decl.m4 \
				  relation_rules.m4 relation_prolog.m4 \
				  relation_decl.m4 comma_rules.m4 \
				  comma_prolog.m4 comma_decl.m4 \
				  minmax_rules.m4 minmax_prolog.m4 \
				  minmax_decl.m4 abs_rules.m4 abs_prolog.m4 \
				  abs_decl.m4

if WITH_CFES
tpm.olive:\
	$(OLIVE_DEPS)
	@M4@ -s -I/opt/local/cfes/include/tpm -I$(srcdir) tpm.m4 > $@
else
tpm.olive:\
	$(OLIVE_DEPS)
	@M4@ -s -I$(top_builddir)/ICD-C/tpm -I$(srcdir) tpm.m4 > $@
endif

tpm.cc:\
	tpm.olive
	@TPM@ @TPMFLAGS@ -p TC179x_ -o $@ $<

tpm.h:\
	tpm.cc
	touch $@

dist-hook:
	rm -f $(distdir)/tpm.cc $(distdir)/tpm.h
