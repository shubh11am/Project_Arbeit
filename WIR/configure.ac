#
#
#   This script belongs to the
#
#            Hamburg University of Technology (TUHH)
#              WCC Intermediate Representation Framework
#
#   and is property of its respective copyright holder. It must neither be used
#   nor published even in parts without explicit written permission.
#
#   Copyright 2015 - 2022, Heiko Falk.
#
#   Hamburg University of Technology (TUHH)
#   Institute of Embedded Systems
#   21071 Hamburg
#   Germany
#
#   http://www.tuhh.de/es/esd/research/wcc
#
#

#
# Default initialization stuff
#

AC_INIT([wir], [1.4], [[wcc@tuhh.de]])

AC_CANONICAL_TARGET

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADERS(config_wir.h)
AM_INIT_AUTOMAKE([foreign parallel-tests subdir-objects])

# Allow for very compact build output
#AM_SILENT_RULES


#
# Determine some important paths about source and build tree
#

current_configure=$0
relative_srcdir=`dirname $0`
absolute_srcdir=`cd $relative_srcdir; pwd`

ac_cv_cppflags="-I${absolute_srcdir}"
ac_cv_cxxflags=""
ac_cv_cflags=""
ac_cv_ldflags=""

AC_DEFINE_UNQUOTED([ARCHDIR], ["$absolute_srcdir/arch"], [Defines the absolute path to the architectures within the source tree directory.])
AC_DEFINE_UNQUOTED([SYSCONFDIR], ["$prefix/etc"], [Defines the path to etc within the install tree directory.])


#
# Define special command line parameters
#

# CXXFLAGS and Release-Quality
AC_ARG_WITH(release-quality,
        AS_HELP_STRING([--with-release-quality],
               [set compiler flags to highest optimization level (default=no)]),
                ac_cv_release_quality="$withval", ac_cv_release_quality=no)

AX_CHECK_COMPILE_FLAG([-Og], [og="yes"],[og="no"])

if test "$ac_cv_release_quality" = yes; then
  if test -z "${ac_cv_cxxflags}"; then
    ac_cv_cxxflags="-O3"
  else
    ac_cv_cxxflags="-O3 ${ac_cv_cxxflags}"
  fi
  if test -z "${ac_cv_cflags}"; then
    ac_cv_cflags="-O3"
  else
    ac_cv_cflags="-O3 ${ac_cv_cflags}"
  fi
else
  if test -z "${ac_cv_cxxflags}"; then
    ac_cv_cxxflags="-g -Wall -Wextra -Wno-parentheses"
  else
    ac_cv_cxxflags="-g -Wall -Wextra -Wno-parentheses ${ac_cv_cxxflags}"
  fi
  if test -z "${ac_cv_cflags}"; then
    ac_cv_cflags="-g -Wall -Wextra"
  else
    ac_cv_cflags="-g -Wall -Wextra ${ac_cv_cflags}"
  fi
  if [[ "$og" == "yes" ]]; then
    ac_cv_cxxflags="-Og ${ac_cv_cxxflags}"
    ac_cv_cflags="-Og ${ac_cv_cflags}"
  else
    ac_cv_cxxflags="-O0 ${ac_cv_cxxflags}"
    ac_cv_cflags="-O0 ${ac_cv_cflags}"
  fi
fi


# Check if 64-bit build should be enabled.
# TODO: Remove this!!
AC_ARG_ENABLE(x64,
        AS_HELP_STRING([--enable-x64],
               [enable x64 build (default=yes)]),
                ac_cv_x64="$enableval", ac_cv_x64=yes)

if test "$ac_cv_x64" = yes; then
  if test `uname` = Linux -a `uname -m` = "x86_64"; then
    if test -z "${CXXFLAGS}"; then
      CXXFLAGS="-m64"
    else
      CXXFLAGS="-m64 ${CXXFLAGS}"
    fi
    if test -z "${CFLAGS}"; then
      CFLAGS="-m64"
    else
      CFLAGS="-m64 ${CFLAGS}"
    fi
  else
    if test -z "${CXXFLAGS}"; then
      CXXFLAGS="-m32"
    else
      CXXFLAGS="-m32 ${CXXFLAGS}"
    fi
    if test -z "${CFLAGS}"; then
      CFLAGS="-m32"
    else
      CFLAGS="-m32 ${CFLAGS}"
    fi
    ac_cv_x64=no
  fi
else
  if test -z "${CXXFLAGS}"; then
    CXXFLAGS="-m32"
  else
    CXXFLAGS="-m32 ${CXXFLAGS}"
  fi
  if test -z "${CFLAGS}"; then
    CFLAGS="-m32"
  else
    CFLAGS="-m32 ${CFLAGS}"
  fi
fi


# Boost
AC_ARG_WITH([boost],
        AS_HELP_STRING([--with-boost],
               [specify your own path to boost]),
                [BOOST="$withval"], [BOOST=no])

if test "$BOOST" = no; then
   if test -n "$BOOST_ROOT"; then
     BOOST="$BOOST_ROOT"
   fi
fi

if test ! "$BOOST" = no; then
  if test -z "${ac_cv_ldflags}"; then
    ac_cv_ldflags="-L${BOOST}/lib -Wl,-R${BOOST}/lib"
  else
    ac_cv_ldflags="-L${BOOST}/lib -Wl,-R${BOOST}/lib ${ac_cv_ldflags}"
  fi
  if test -z "${ac_cv_cppflags}"; then
    ac_cv_cppflags="-I${BOOST}/include"
  else
    ac_cv_cppflags="-I${BOOST}/include ${ac_cv_cppflags}"
  fi
fi


# Enabling use of debug macros
AC_ARG_ENABLE(wir-debugmacros,
        AS_HELP_STRING([--enable-wir-debugmacros],
               [enable use of debug macros within WIR (default=no)]),
                ac_cv_debugmacros="$enableval", ac_cv_debugmacros=no)

if test "$ac_cv_debugmacros" = yes; then
  AC_DEFINE(DEBUGMACROS, 1, [Define to 1 if you want to produce debug output via debug macros.])

  if test -z "${ac_cv_cppflags}"; then
    ac_cv_cppflags="-DDEBUGMACROS"
  else
    ac_cv_cppflags="-DDEBUGMACROS ${ac_cv_cppflags}"
  fi
fi


# Enabling memory debugging mode
AC_ARG_ENABLE(memory_debugging,
          AS_HELP_STRING([--enable-memory-debugging],
                 [enable GCC stack protection and libstdc++ debug mode and address sanitizer (default=no)]),
          ac_cv_memory_debugging="$enableval", ac_cv_memory_debugging=no)

if test "$ac_cv_memory_debugging" = "yes"; then
  if test -z "${ac_cv_cxxflags}"; then
    ac_cv_cxxflags="-fstack-protector-all -fsanitize=address -Wstack-protector -D_GLIBCXX_DEBUG"
  else
    ac_cv_cxxflags="-fstack-protector-all -fsanitize=address -Wstack-protector -D_GLIBCXX_DEBUG ${ac_cv_cxxflags}"
  fi
  if test -z "${ac_cv_cflags}"; then
    ac_cv_cflags="-fstack-protector-all -fsanitize=address -Wstack-protector -D_GLIBCXX_DEBUG"
    else
    ac_cv_cflags="-fstack-protector-all -fsanitize=address -Wstack-protector -D_GLIBCXX_DEBUG ${ac_cv_cflags}"
  fi
fi
 # Enabling memory protector mode
AC_ARG_ENABLE(memory_protection,
        AS_HELP_STRING([--enable-memory-protection],
               [enable GCC stack protection and libstdc++ debug mode (default=no)]),
                ac_cv_memory_protection="$enableval", ac_cv_memory_protection=no)

if test "$ac_cv_memory_protection" = "yes"; then
  if test -z "${ac_cv_cxxflags}"; then
    ac_cv_cxxflags="-fstack-protector-all -Wstack-protector -D_GLIBCXX_DEBUG"
  else
    ac_cv_cxxflags="-fstack-protector-all -Wstack-protector -D_GLIBCXX_DEBUG ${ac_cv_cxxflags}"
  fi
  if test -z "${ac_cv_cflags}"; then
    ac_cv_cflags="-fstack-protector-all -Wstack-protector -D_GLIBCXX_DEBUG"
  else
    ac_cv_cflags="-fstack-protector-all -Wstack-protector -D_GLIBCXX_DEBUG ${ac_cv_cflags}"
  fi
fi


#
# Enabling of various processor models
#

procs="generic"

AC_ARG_ENABLE(arm,
        AS_HELP_STRING([--enable-arm],
               [enable ARM processor model (default=yes)]),
                ac_cv_arm="$enableval", ac_cv_arm=yes)

if test "$ac_cv_arm" = yes; then
  AC_DEFINE(HAVE_ARM, 1, [Define to 1 if you have the ARM processor model.])
  procs=${procs}" arm"

  AC_PATH_PROG(armas, arm-elf-as, arm-elf-as)
  AC_SUBST(ARMAS,[${armas}])

  AC_SUBST(DOC_ARM, [${absolute_srcdir}/arch/arm])
fi
AM_CONDITIONAL(BUILD_ARM, test $ac_cv_arm = yes)

AC_ARG_ENABLE(leon,
        AS_HELP_STRING([--enable-leon],
               [enable LEON processor model (default=yes)]),
                ac_cv_leon="$enableval", ac_cv_leon=yes)

if test "$ac_cv_leon" = yes; then
  AC_DEFINE(HAVE_LEON, 1, [Define to 1 if you have the LEON processor model.])
  procs=${procs}" leon"

  AC_SUBST(DOC_LEON, [${absolute_srcdir}/arch/leon])
fi
AM_CONDITIONAL(BUILD_LEON, test $ac_cv_leon = yes)

AC_ARG_ENABLE(riscv,
        AS_HELP_STRING([--enable-riscv],
               [enable RISC-V processor model (default=yes)]),
                ac_cv_riscv="$enableval", ac_cv_riscv=yes)

if test "$ac_cv_riscv" = yes; then
  AC_DEFINE(HAVE_RISCV, 1, [Define to 1 if you have the RISC-V processor model.])
  procs=${procs}" riscv"

  AC_PATH_PROG(riscvas, riscv32-unknown-elf-as, riscv32-unknown-elf-as)
  AC_SUBST(RISCVAS,[${riscvas}])

  AC_SUBST(DOC_RISCV, [${absolute_srcdir}/arch/riscv])
fi
AM_CONDITIONAL(BUILD_RISCV, test $ac_cv_riscv = yes)

AC_ARG_ENABLE(tricore,
        AS_HELP_STRING([--enable-tricore],
               [enable TriCore processor model (default=yes)]),
                ac_cv_tricore="$enableval", ac_cv_tricore=yes)

if test "$ac_cv_tricore" = yes; then
  AC_DEFINE(HAVE_TRICORE, 1, [Define to 1 if you have the TRICORE processor model.])
  procs=${procs}" tricore"

  AC_PATH_PROG(trias, tricore-as, tricore-as)
  AC_SUBST(TRIAS,[${trias}])

  AC_SUBST(DOC_TRICORE, [${absolute_srcdir}/arch/tricore])
fi
AM_CONDITIONAL(BUILD_TRICORE, test $ac_cv_tricore = yes)

AC_DEFINE_UNQUOTED([ARCHS], ["$procs"], [Defines the names of the enabled architectures.])


# Enabling use of assertion-based "failsafe mode"
AC_ARG_ENABLE(failsafe,
        AS_HELP_STRING([--enable-failsafe],
               [enable failsafe mode (default=no)]),
                ac_cv_failsafe="$enableval", ac_cv_failsafe=no)

if test "$ac_cv_failsafe" = yes; then
  AC_DEFINE(FAILSAFEMODE, 1, [Define to 1 if you want to check assertions at run-time whenever possible.])
fi

current_dir=`pwd`

# Prefix to debug configuration
AC_ARG_WITH(debugmacro-configuration,
        AS_HELP_STRING([--with-debugmacro-configuration=PATH],
               [Prefix where the debug macro configuration files reside]),
                debugmacro_prefix="$withval", debugmacro_prefix="${absolute_srcdir}/DEBUGMACRO_CONF")

AC_DEFINE_UNQUOTED([DEBUGMACRO_CONF], ["$debugmacro_prefix"], [Defines the path to the debug conf files.])

# Prefix to LIBUSEFUL
AC_ARG_WITH(libuseful,
        AS_HELP_STRING([--with-libuseful=PFX],
               [Prefix where libuseful resides]),
                libuseful_prefix="$withval", libuseful_prefix="")

if test "$libuseful_prefix" = ""; then
  AC_MSG_ERROR([*** Prefix to LIBUSEFUL not specified ***])
fi
AC_SUBST(LIBUSEFUL_PREFIX,[${libuseful_prefix}])

AC_ARG_WITH(libuseful-headers,
        AS_HELP_STRING([--with-libuseful-headers=PATH],
               [Prefix where the libuseful headers reside.]),
                libuseful_headers="$withval", libuseful_headers=none)

if test "$libuseful_headers" != "none"; then
  if test -z "${ac_cv_cppflags}"; then
    ac_cv_cppflags="-I${libuseful_headers}"
  else
    ac_cv_cppflags="-I${libuseful_headers} ${ac_cv_cppflags}"
  fi
fi

# Configuration for installation of WIR
AC_ARG_WITH(restricted-install,
        AS_HELP_STRING([--with-restricted-install],
               [Install only shared libraries. (default=no)]),
                ac_cv_restricted_install="$withval", ac_cv_restricted_install=no)

if test "$ac_cv_restricted_install" = yes; then
  if test "$enable_shared" = no -a "$enable_static" = yes; then
    install_libraries=no
  else
    install_libraries=yes
  fi
else
  install_libraries=yes
fi
AC_SUBST(LIB_NOINSTALL_DIR,[$current_dir/noinst])


# Configuration for WIR within WCC
AC_ARG_WITH(wcc,
        AS_HELP_STRING([--with-wcc],
               [Configure WIR for use within WCC compiler (default=no)]),
                ac_cv_wcc="$withval", ac_cv_wcc=no)

if test "$ac_cv_wcc" = yes; then
  AC_DEFINE(HAVE_WCC, 1, [Define to 1 if you are building inside of WCC.])

  if test "$enable_shared" = no -a "$enable_static" = yes; then
    prefix=`echo $PWD`/wir/.libs
  else
    libdir=${prefix}/lib
    prefix=`echo $PWD`/wir/.libs
  fi
fi
AM_CONDITIONAL(HAVE_WCC, test $ac_cv_wcc = yes)


# Activate building of DOXYGEN documentation
AC_ARG_WITH(doxygen-doc,
        AS_HELP_STRING([--with-doxygen-doc],
               [build doxygen documentation (default=yes)]),
                ac_cv_doxygen_doc="$withval", ac_cv_doxygen_doc=yes)

if test ! -f ${absolute_srcdir}/doc/doxygen.conf.in; then
  ac_cv_doxygen_doc=no
fi


#
# Checking for header files
#

OLD_CPPFLAGS="${CPPFLAGS}"
OLD_CXXFLAGS="${CXXFLAGS}"
CPPFLAGS="-I${BOOST}/include ${CPPFLAGS}"
CXXFLAGS="-L${BOOST}/lib -Wl,-R${BOOST}/lib ${CXXFLAGS}"

AC_LANG_PUSH(C++)

ac_cv_have_boost=yes
AC_CHECK_HEADER([boost/current_function.hpp], [], [ac_cv_have_boost=no])

if test "$ac_cv_have_boost" = no; then
  AC_MSG_ERROR([*** boost library not found - please install first ***])
fi

AC_LANG_POP([C++])

CPPFLAGS="${OLD_CPPFLAGS}"
CXXFLAGS="${OLD_CXXFLAGS}"


#
# Checking the existence of build programs
#

# C++ compiler
AC_PROG_CXX
AC_PROG_CC
AC_LANG(C++)

# Set correct language standard - we use C++17
AX_CHECK_COMPILE_FLAG([-std=c++17], [ac_cv_cxxflags="${ac_cv_cxxflags} -std=c++17"], [AC_MSG_ERROR([Compiler does not support C++17 mode])] )


# Libtool
AC_DISABLE_STATIC
LT_INIT

# Install
AC_PROG_INSTALL

# Bash
AC_PATH_PROGS(BASH, bash)

# Bash
AC_PATH_PROGS(CHMOD, chmod)

# Make
AC_CHECK_PROG(HAVE_GMAKE, gmake, gmake, notfound)
if test "$HAVE_GMAKE" = notfound; then
  AC_CHECK_PROG(HAVE_MAKE, make, make, notfound)
  if test "$HAVE_MAKE" = notfound; then
    AC_MSG_ERROR([*** neither gmake nor make found - please install first ***])
  else
    ac_cv_make=$HAVE_MAKE
  fi
else
  ac_cv_make=$HAVE_GMAKE
fi

AC_SUBST(INSTALLED_MAKE,[${ac_cv_make}])

# lex and yacc (for assembly code parsers)
AC_PROG_LEX(noyywrap)

AC_CHECK_PROG(BISON, bison, bison, notfound)
if test "$BISON" = notfound; then
  AC_MSG_ERROR([*** bison not found - please install first ***])
fi


#
# Static linking stuff.
#

if test "$enable_shared" = no -a "$enable_static" = yes; then
  if test `uname` = "SunOS" -a `uname -r` = "5.10" -a ${CXX} = "g++"; then
    THEGCC=`which $CXX`
    GCCBINDIR=`dirname $THEGCC`
    GCCLIBDIR=`dirname $GCCBINDIR`/lib

    if test ! -d ${PWD}/__lib; then
      mkdir ${PWD}/__lib
    fi
    if test ! -h ${PWD}/__lib/libstdc++.a; then
      $LN_S ${GCCLIBDIR}/libstdc++.a ${PWD}/__lib
    fi

    if test -z "${ac_cv_ldflags}"; then
      ac_cv_ldflags="-static -static-libgcc -L${PWD}/__lib"
    else
      ac_cv_ldflags="${ac_cv_ldflags} -static -static-libgcc -L${PWD}/__lib"
    fi
  else
    if test -z "${ac_cv_ldflags}"; then
      ac_cv_ldflags="-all-static"
    else
      ac_cv_ldflags="${ac_cv_ldflags} -all-static"
    fi
  fi

  AC_DEFINE(STATIC_LINKING, 1, [Define to 1 if you do a static build.])
fi


#
# Checking the existence of miscellaneous programs
#

AC_CHECK_PROG(DATE, date, date, notfound)
if test "$DATE" = notfound; then
  AC_MSG_ERROR([*** date not found - please install first ***])
fi

AC_CHECK_PROG(DIRNAME, dirname, dirname, notfound)
if test "$DIRNAME" = notfound; then
  AC_MSG_ERROR([*** dirname not found - please install first ***])
fi

if test $ac_cv_doxygen_doc = yes; then
  AC_CHECK_PROG(DOT, dot, dot, notfound)
  if test $DOT = notfound; then
    HAVE_DOT=NO
  else
    HAVE_DOT=YES
  fi
  AC_SUBST(DOT,[${HAVE_DOT}])

  AC_CHECK_PROG(DOXYGEN, doxygen, doxygen, notfound)
  AM_CONDITIONAL(HAVE_DOXYGEN, test ! $DOXYGEN = notfound)
else
  HAVE_DOT=NO
  AC_SUBST(DOT,[${HAVE_DOT}])

  DOXYGEN=notfound
  AM_CONDITIONAL(HAVE_DOXYGEN, test ! $DOXYGEN = notfound)
fi
AM_CONDITIONAL(HAVE_DOT, test $HAVE_DOT = YES)

AC_CHECK_PROG(UNAME, uname, uname, notfound)
if test "$UNAME" = notfound; then
  AC_MSG_ERROR([*** uname not found - please install first ***])
fi

# Extract some release numbers for doxygen
WIR_RELEASE="$(sed -n '/^release/h;$x;$p' ${absolute_srcdir}/Releases)"
AC_SUBST(WIR_RELEASE, [${WIR_RELEASE}])
AC_DEFINE_UNQUOTED([WIR_VERSION], ["${WIR_RELEASE}"], [Defines version of WIR module.])
AC_DEFINE([WIR_MODULE], "wir", [Defines name of WIR module.])


#
# Propagate configured parameters into Makefiles
#

AM_CONDITIONAL(WITH_RESTRICTED_INSTALL, test $ac_cv_restricted_install = yes)
AM_CONDITIONAL(INSTALL_LIBRARIES, test $install_libraries = yes)
AM_CONDITIONAL(WITH_DOXYGEN_DOC, test $ac_cv_doxygen_doc = yes)


#
# Prepare output
#

AC_SUBST(AM_CPPFLAGS, [${ac_cv_cppflags}])
AC_SUBST(AM_CFLAGS, [${ac_cv_cflags}])
AC_SUBST(AM_CXXFLAGS, [${ac_cv_cxxflags}])
AC_SUBST(AM_LDFLAGS, [${ac_cv_ldflags}])

AC_CONFIG_FILES([Makefile
                 doc/Makefile
                 tests/Makefile
                 arch/Makefile
                 arch/generic/Makefile
                 arch/generic/tests/Makefile
                 arch/arm/Makefile
                 arch/arm/tests/Makefile
                 arch/leon/Makefile
                 arch/riscv/Makefile
                 arch/riscv/tests/Makefile
                 arch/tricore/Makefile
                 arch/tricore/asmparser/Makefile
                 arch/tricore/tests/Makefile])

if test $ac_cv_doxygen_doc = yes; then
  if test ! ${DOXYGEN} = notfound; then
    AC_CONFIG_FILES([doc/doxygen.conf])
  fi
fi


AC_OUTPUT


#
# Generate status output
#

echo ""
echo "WIR Configuration:"
echo "  Install path:                 ${prefix}"
echo "  C++ Compiler flags:           ${ac_cv_cxxflags} ${CXXFLAGS}"
echo "  C Compiler flags:             ${ac_cv_cflags} ${CFLAGS}"
echo "  C Preprocessor flags:         ${ac_cv_cppflags} ${CPPFLAGS}"
echo "  Linker flags:                 ${ac_cv_ldflags} ${LDFLAGS}"
echo "  Build shared libraries:       ${enable_shared}"
echo "  Build static libraries:       ${enable_static}"
echo ""
echo "  Enabled processor models:     ${procs}"

echo "  Enabling failsafe mode:       ${ac_cv_failsafe}"
echo "  Enabling debug macros:        ${ac_cv_debugmacros}"
echo "  Build DOXYGEN documentation:  ${ac_cv_doxygen_doc}"

echo "  Use LIBUSEFUL in:             ${libuseful_prefix}"

echo ""
