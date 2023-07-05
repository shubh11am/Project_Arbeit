#
# Target to build
#

.SILENT:


#
# Source files of the target
#

BUILT_SOURCES		=
EXTRA_DIST		= doxygen.conf.in Footer.html

if WITH_DOXYGEN_DOC

  BUILT_SOURCES		+= html/index.html

endif


#
# Specific rules
#

if WITH_DOXYGEN_DOC

html/index.html: \
	$(top_builddir)/libwir.la doxygen.conf.in $(srcdir)/Footer.html
	@DOXYGEN@ doxygen.conf

endif

mostlyclean-generic:
	if [ -d html ]; then \
	  rm -r html; \
	fi
