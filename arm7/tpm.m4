%{
/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2010 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include standard headers
#include <cerrno>
#include <cstdlib>
#include <deque>
#include <limits>
#include <numeric>
#include <sstream>
#include <stack>

// Include WIR headers
#include <wir/wir.h>
#include <arch/arm/armv6.h>

// Include ICD headers
#include <icd-c.h>
#include <llir3/llir3.h>
#include <llir3optimize/flowfactupdater/llirflowfactupdater.h>
#include <tpm/irtreeelem.h>
#include <tpm/termlist.h>

// Include LIBUSEFUL headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include misc headers
#include <misc/misc.h>

// Include local headers
#include <codesel/addresswithoffset.h>
#include <arm7/arm_incl.h>
#include <arm7/armaddressmodification.h>
#include <arm7/armlvalue.h>
#include <arm7/cast.h>
#include <arm7/casting_incl.h>
#include <arm7/registrar.h>
#include <arm7/stack.h>
#include <arm7/assignment_incl.h>
#include <arm7/composed_struct_bitfield_incl.h>
#include <arm7/composed_incl.h>

include(arm_prolog.m4)
include(assignment_prolog.m4)
include(composed_prolog.m4)
include(composed_struct_bitfield_prolog.m4)
include(casting_prolog.m4)
include(jump_prolog.m4)

%}

# This directive includes the set of standard C terminals that are predefined by
# the IR tree pattern matcher library.
include(irterminals-icdcg.m4)

include(arm_decl.m4)
include(assignment_decl.m4)
include(composed_decl.m4)
include(casting_decl.m4)
include(jump_decl.m4)
include(pointer_decl.m4)

%%

include(arm_rules.m4)
include(assignment_rules.m4)
include(composed_rules.m4)
include(casting_rules.m4)
include(jump_rules.m4)
include(pointer_rules.m4)

%%
