#
# Targets to build
#

#.SILENT:

check_PROGRAMS		= genericmips.bin \
			  mipsImmediate5Shamt1.bin \
			  mipsImmediate5Shamt2.bin \
			  mipsImmediate16Signed1.bin \
			  mipsImmediate16Signed2.bin \
			  mipsImmediate16Signed3.bin \
			  mipsImmediate16Unsigned1.bin \
			  mipsImmediate16Unsigned2.bin \
			  mipsOperations1.bin \
			  mipsVirtualRegister1.bin

failscripts		= mipsImmediate5Shamt2.sh \
			  mipsImmediate16Signed2.sh \
			  mipsImmediate16Signed3.sh \
			  mipsImmediate16Unsigned2.sh

passscripts		= genericmips.sh \
			  mipsImmediate5Shamt1.sh \
			  mipsImmediate16Signed1.sh \
			  mipsImmediate16Unsigned1.sh \
			  mipsOperations1.sh \
			  mipsVirtualRegister1.sh

check_SCRIPTS		= $(failscripts) $(passscripts)

XFAIL_TESTS		= $(failscripts)
TESTS			= $(passscripts) $(failscripts)


#
# Source files of the targets
#

CLEANFILES		= $(check_PROGRAMS) $(check_SCRIPTS) core
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
	@echo $(builddir)"/$< > /dev/null 2>&1" >> $@
	@echo "exit ""$$""?" >> $@
	@$(CHMOD) ugo+x $@
