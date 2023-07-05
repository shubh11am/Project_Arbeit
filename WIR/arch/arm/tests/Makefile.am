#
# Targets to build
#

#.SILENT:

check_PROGRAMS		= arm7tdmi_1core.bin arm7tdmi_2core.bin \
			  arm7tdmi_4core.bin arm7tdmi_8core.bin \
			  armBasicBlock1.bin \
			  armCompilationUnit1.bin \
			  armConditionFieldParameter1.bin \
			  armConst3CoprocessorOpcode1.bin \
			  armConst3CoprocessorOpcode2.bin \
			  armConst4CoprocessorOpcode1.bin \
			  armConst4CoprocessorOpcode2.bin \
			  armConst5RotateAmount1.bin armConst5RotateAmount2.bin \
			  armConst5SatPos1.bin armConst5SatPos2.bin \
			  armConst5Unsigned0_1.bin armConst5Unsigned0_2.bin \
			  armConst5Unsigned0_3.bin \
			  armConst5Unsigned1.bin armConst5Unsigned2.bin \
			  armConst6Unsigned0_1.bin armConst6Unsigned0_2.bin \
			  armConst6Unsigned0_3.bin \
			  armConst7Unsigned1.bin armConst7Unsigned2.bin \
			  armConst7Unsigned3.bin \
			  armConst8Unsigned1.bin armConst8Unsigned2.bin \
			  armConst9Unsigned1.bin armConst9Unsigned2.bin \
			  armConst9Unsigned3.bin armConst9Unsigned4.bin \
			  armConst10CoprocessorOffset1.bin \
			  armConst10CoprocessorOffset2.bin \
			  armConst10CoprocessorOffset3.bin \
			  armConst12Unsigned1.bin armConst12Unsigned2.bin \
			  armConst16Unsigned1.bin armConst16Unsigned2.bin \
			  armConst24Unsigned1.bin armConst24Unsigned2.bin \
			  armFunction1.bin \
			  armOperations1.bin armOperations2.bin \
			  armOperations3.bin armOperations4.bin \
			  armOperations5.bin armOperations6.bin \
			  armPhRegs1.bin \
			  armVirtualRegister1.bin \
			  armv4tOperations1.bin armv4tOperations2.bin \
			  armv4tOperations3.bin armv4tOperations4.bin \
			  armv5tOperations1.bin armv5tOperations2.bin \
			  armv5teOperations1.bin armv5teOperations2.bin \
			  armv5tejOperations1.bin armv5tejOperations2.bin \
			  armv6Operations1.bin armv6Operations2.bin \
			  lpc2880.bin \
			  lpc2880MemoryLayout1.bin

failscripts		= armConst3CoprocessorOpcode2.sh \
			  armConst4CoprocessorOpcode2.sh \
			  armConst5RotateAmount2.sh \
			  armConst5SatPos2.sh \
			  armConst5Unsigned0_2.sh armConst5Unsigned0_3.sh \
			  armConst5Unsigned2.sh \
			  armConst6Unsigned0_2.sh armConst6Unsigned0_3.sh \
			  armConst7Unsigned2.sh armConst7Unsigned3.sh \
			  armConst8Unsigned2.sh \
			  armConst9Unsigned2.sh armConst9Unsigned3.sh \
			  armConst9Unsigned4.sh \
			  armConst10CoprocessorOffset2.sh \
			  armConst10CoprocessorOffset3.sh \
			  armConst12Unsigned2.sh \
			  armConst16Unsigned2.sh \
			  armConst24Unsigned2.sh \
			  armOperations3.sh armOperations4.sh \
			  armOperations5.sh armOperations6.sh \
			  armv4tOperations3.sh armv4tOperations4.sh

passscripts		= arm7tdmi_1core.sh arm7tdmi_2core.sh \
			  arm7tdmi_4core.sh arm7tdmi_8core.sh \
			  armConditionFieldParameter1.sh \
			  armConst3CoprocessorOpcode1.sh \
			  armConst4CoprocessorOpcode1.sh \
			  armConst5RotateAmount1.sh \
			  armConst5SatPos1.sh \
			  armConst5Unsigned0_1.sh \
			  armConst5Unsigned1.sh \
			  armConst6Unsigned0_1.sh \
			  armConst7Unsigned1.sh \
			  armConst8Unsigned1.sh \
			  armConst9Unsigned1.sh \
			  armConst10CoprocessorOffset1.sh \
			  armConst12Unsigned1.sh \
			  armConst16Unsigned1.sh \
			  armConst24Unsigned1.sh \
			  armOperations1.sh \
			  armPhRegs1.sh \
			  armVirtualRegister1.sh \
			  armv4tOperations1.sh \
			  armv5tOperations1.sh \
			  armv5teOperations1.sh \
			  armv5tejOperations1.sh \
			  armv6Operations1.sh \
			  lpc2880.sh

aspassscripts		= armBasicBlock1.sh \
			  armCompilationUnit1.sh \
			  armFunction1.sh \
			  armOperations2.sh \
			  armv4tOperations2.sh \
			  armv5tOperations2.sh \
			  armv5teOperations2.sh \
			  armv5tejOperations2.sh \
			  armv6Operations2.sh \
			  lpc2880MemoryLayout1.sh

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
	@echo $(ARMAS)' -W $$TMPFILE' >> $@;
	@echo 'ret=$$?' >> $@;
	@echo 'rm $$TMPFILE' >> $@;
	@echo 'exit $$ret' >> $@;
	@$(CHMOD) ugo+x $@
