#
# Target to build
#

.SILENT:


#
# Source files of the target
#

BUILT_SOURCES			= tpm.olive tpm.cc tpm.h

CLEANFILES			= $(BUILT_SOURCES)

EXTRA_DIST			= rv32_assignment_decl.m4 \
				  rv32_assignment_prolog.m4 \
				  rv32_assignment_rules.m4 \
				  rv32_relation_decl.m4 \
				  rv32_relation_prolog.m4 \
				  rv32_relation_rules.m4 \
				  rv32_decl.m4 \
				  rv32_prolog.m4 \
				  rv32_rules.m4 \
				  rv32_pointer_rules.m4 \
				  rv32_pointer_decl.m4 \
				  rv32_casting_rules.m4 \
				  rv32_casting_decl.m4 \
				  rv32_composed_rules.m4 \
				  tpm.m4


#
# Specific rules
#

OLIVE_DEPS			= rv32_assignment_decl.m4 \
				  rv32_assignment_prolog.m4 \
				  rv32_assignment_rules.m4 \
				  rv32_relation_decl.m4 \
				  rv32_relation_prolog.m4 \
				  rv32_relation_rules.m4 \
				  rv32_decl.m4 \
				  rv32_prolog.m4 \
				  rv32_rules.m4 \
				  rv32_pointer_rules.m4 \
				  rv32_pointer_decl.m4 \
				  rv32_casting_rules.m4 \
				  rv32_casting_decl.m4 \
				  tpm.m4

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
	@TPM@ @TPMFLAGS@ -p RV32_ -o $@ $<

tpm.h:\
	tpm.cc
	touch $@

dist-hook:
	rm -f $(distdir)/tpm.cc $(distdir)/tpm.h
