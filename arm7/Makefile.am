#
# Target to build
#

.SILENT:


#
# Source files of the target
#

BUILT_SOURCES			= tpm.olive tpm.cc tpm.h

CLEANFILES			= $(BUILT_SOURCES)

EXTRA_DIST			= arm_rules.m4\
				  tpm.m4\
				  assignment_rules.m4\
				  assignment_prolog.m4\
				  assignment_decl.m4\
				  composed_rules.m4\
				  composed_decl.m4\
				  composed_prolog.m4\
				  composed_struct_bitfield_rules.m4\
				  composed_struct_bitfield_decl.m4\
				  composed_struct_bitfield_prolog.m4\
				  casting_rules.m4\
				  casting_prolog.m4\
				  casting_decl.m4\
				  jump_rules.m4\
				  jump_prolog.m4\
				  jump_decl.m4\
				  pointer_rules.m4\
				  pointer_decl.m4\
				  arm_decl.m4\
				  arm_prolog.m4



#
# Specific rules
#

OLIVE_DEPS			= arm_rules.m4\
				  tpm.m4\
				  assignment_rules.m4\
				  assignment_prolog.m4\
				  assignment_decl.m4\
				  composed_rules.m4\
				  composed_decl.m4\
				  composed_prolog.m4\
				  composed_struct_bitfield_rules.m4\
				  composed_struct_bitfield_decl.m4\
				  composed_struct_bitfield_prolog.m4\
				  casting_rules.m4\
				  casting_prolog.m4\
				  casting_decl.m4\
				  jump_rules.m4\
				  jump_prolog.m4\
				  jump_decl.m4\
				  pointer_rules.m4\
				  pointer_decl.m4\
				  arm_decl.m4\
				  arm_prolog.m4

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
	@TPM@ @TPMFLAGS@ -p ARM7_ -o $@ $<

tpm.h:\
	tpm.cc
	touch $@

dist-hook:
	rm -f $(distdir)/tpm.cc $(distdir)/tpm.h
