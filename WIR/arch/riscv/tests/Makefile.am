#
# Targets to build
#

#.SILENT:

check_PROGRAMS		= rv32BitADD01.bin rv32BitADDI01.bin rv32BitAND01.bin \
			  rv32BitANDI01.bin rv32BitBEQ01.bin rv32BitBGE01.bin \
			  rv32BitBGEU01.bin rv32BitBLT01.bin rv32BitBLTU01.bin \
			  rv32BitBNE01.bin rv32BitCADD01.bin \
			  rv32BitCADDI01.bin rv32BitCAND01.bin \
			  rv32BitCANDI01.bin rv32BitCBEQZ01.bin \
			  rv32BitCBNEZ01.bin rv32BitCLI01.bin \
			  rv32BitCLUI01.bin rv32BitCMV01.bin rv32BitCOR01.bin \
			  rv32BitCSLLI01.bin rv32BitCSRAI01.bin \
			  rv32BitCSRLI01.bin rv32BitCSUB01.bin \
			  rv32BitCXOR01.bin rv32BitLUI01.bin \
			  rv32BitLW-LH-LHU-LB-LBU01.bin rv32BitMOV01.bin \
			  rv32BitOR01.bin rv32BitORI01.bin rv32BitSLL01.bin \
			  rv32BitSLLI01.bin rv32BitSLT01.bin rv32BitSLTI01.bin \
			  rv32BitSLTIU01.bin rv32BitSLTU01.bin \
			  rv32BitSRA01.bin rv32BitSRAI01.bin rv32BitSRL01.bin \
			  rv32BitSRLI01.bin rv32BitSUB01.bin \
			  rv32BitSW-SH-SB01.bin rv32BitXOR01.bin \
			  rv32BitXORI01.bin \
			  rv32iOperations1.bin rv32iOperations2.bin \
			  rv32icOperations1.bin rv32icOperations2.bin \
			  rv32imOperations1.bin rv32imOperations2.bin \
			  rv32imcOperations1.bin rv32imcOperations2.bin \
			  rvConst5Unsigned1.bin rvConst5Unsigned2.bin \
			  rvConst6Signed1.bin rvConst6Signed2.bin \
			  rvConst6Signed3.bin \
			  rvConst6Unsigned1.bin rvConst6Unsigned2.bin \
			  rvConst8Unsigned1.bin rvConst8Unsigned2.bin \
			  rvConst12Signed1.bin rvConst12Signed2.bin \
			  rvConst12Signed3.bin \
			  rvConst20Unsigned1.bin rvConst20Unsigned2.bin \
			  rvPhRegs1.bin \
			  rvReg1.bin

failscripts		= rvConst5Unsigned2.sh \
			  rvConst6Signed2.sh rvConst6Signed3.sh \
			  rvConst6Unsigned2.sh \
			  rvConst8Unsigned2.sh \
			  rvConst12Signed2.sh rvConst12Signed3.sh \
			  rvConst20Unsigned2.sh

passscripts		= rv32iOperations1.sh \
			  rv32icOperations1.sh \
			  rv32imOperations1.sh \
			  rv32imcOperations1.sh \
			  rv32BitADD01.sh rv32BitADDI01.sh rv32BitAND01.sh \
			  rv32BitANDI01.sh rv32BitBEQ01.sh rv32BitBGE01.sh \
			  rv32BitBGEU01.sh rv32BitBLT01.sh rv32BitBLTU01.sh \
			  rv32BitBNE01.sh rv32BitCADD01.sh rv32BitCADDI01.sh \
			  rv32BitCAND01.sh rv32BitCANDI01.sh rv32BitCBEQZ01.sh \
			  rv32BitCBNEZ01.sh rv32BitCLI01.sh rv32BitCLUI01.sh \
			  rv32BitCMV01.sh rv32BitCOR01.sh rv32BitCSLLI01.sh \
			  rv32BitCSRAI01.sh rv32BitCSRLI01.sh rv32BitCSUB01.sh \
			  rv32BitCXOR01.sh rv32BitLUI01.sh \
			  rv32BitLW-LH-LHU-LB-LBU01.sh rv32BitMOV01.sh \
			  rv32BitOR01.sh rv32BitORI01.sh rv32BitSLL01.sh \
			  rv32BitSLLI01.sh rv32BitSLT01.sh rv32BitSLTI01.sh \
			  rv32BitSLTIU01.sh rv32BitSLTU01.sh rv32BitSRA01.sh \
			  rv32BitSRAI01.sh rv32BitSRL01.sh rv32BitSRLI01.sh \
			  rv32BitSUB01.sh rv32BitSW-SH-SB01.sh rv32BitXOR01.sh \
			  rv32BitXORI01.sh \
			  rvConst5Unsigned1.sh \
			  rvConst6Signed1.sh \
			  rvConst6Unsigned1.sh \
			  rvConst8Unsigned1.sh \
			  rvConst12Signed1.sh \
			  rvConst20Unsigned1.sh \
			  rvPhRegs1.sh \
			  rvReg1.sh

aspassscripts		= rv32iOperations2.sh \
			  rv32icOperations2.sh \
			  rv32imOperations2.sh \
			  rv32imcOperations2.sh

check_SCRIPTS		= $(failscripts) $(passscripts)
check_ASSCRIPTS		= $(aspassscripts)

XFAIL_TESTS		= $(failscripts)
TESTS			= $(passscripts) $(aspassscripts) $(failscripts)


#
# Source files of the targets
#

CLEANFILES		= $(check_PROGRAMS) $(check_SCRIPTS) \
			  $(check_ASSCRIPTS) a.out core
AM_DEFAULT_SOURCE_EXT   = .cc


#
# Compiler and linker flags
#

LDADD			= $(top_builddir)/libwir.la \
			  @LIBUSEFUL_PREFIX@/libuseful/libuseful.la


#
# Specific rules
#

$(check_SCRIPTS): \
	%.sh: %.bin
	@echo "#!"$(BASH) > $@;
	@echo $(builddir)"/$< > /dev/null 2>&1" >> $@;
	@echo "exit ""$$""?" >> $@;
	@$(CHMOD) ugo+x $@

$(check_ASSCRIPTS): \
	%.sh: %.bin
	@echo "#!"$(BASH) > $@;
	@echo 'TMPFILE=/tmp/`basename $$0`.$$$$.log' >> $@;
	@echo $(builddir)"/$< > "'$$TMPFILE' >> $@;
	@echo 'ret=$$?' >> $@;
	@echo 'if [ $$ret != 0 ]; then' >> $@;
	@echo '  rm $$TMPFILE' >> $@;
	@echo '  exit $$ret' >> $@;
	@echo 'fi' >> $@;
	@echo $(RISCVAS)' -march=rv32imc -mabi=ilp32 $$TMPFILE' >> $@;
	@echo 'ret=$$?' >> $@;
	@echo 'rm $$TMPFILE' >> $@;
	@echo 'exit $$ret' >> $@;
	@$(CHMOD) ugo+x $@
