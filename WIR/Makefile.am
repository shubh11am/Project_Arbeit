SUBDIRS			= tests arch . doc


#
# Target to build
#

.SILENT:

if INSTALL_LIBRARIES
  wirlibdir		= $(libdir)
else
  wirlibdir		= @LIB_NOINSTALL_DIR@
endif

wirlib_LTLIBRARIES	= libwir.la

EXTRA_DIST		= Releases


#
# Source files of the target
#

libwir_la_SOURCES	=

include analyses/Makefile.src
include arch/Makefile.src
include containers/Makefile.src
include flowfacts/Makefile.src
include optimizations/Makefile.src
include wir/Makefile.src


#
# Compiler and linker flags
#

libwir_la_LDFLAGS	= -avoid-version
libwir_la_LIBADD	= @LIBUSEFUL_PREFIX@/libuseful/libuseful.la
