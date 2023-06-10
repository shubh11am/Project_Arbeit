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
#include <algorithm>
#include <cerrno>
#include <cstdlib>
#include <deque>
#include <functional>
#include <limits>
#include <list>
#include <memory>
#include <numeric>
#include <sstream>
#include <stack>
#include <string>
#include <utility>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>
#include <arch/tricore/asmparser/tcasmaddress.h>
#include <arch/tricore/asmparser/tcasmargument.h>
#include <arch/tricore/asmparser/tcasmconstant.h>
#include <arch/tricore/asmparser/tcasmregister.h>
#include <arch/tricore/asmparser/tcasmtemplateregister.h>

// Include ICD headers
#include <icd-c.h>
#include <icdint/icdint.h>
#include <llir3/llir3.h>
#include <llir3optimize/flowfactupdater/llirflowfactupdater.h>
#include <objectives/llirflowfactref.h>

// Include assembly parser headers
#include <arch/IO/PARSER/address.h>
#include <arch/IO/PARSER/argument.h>
#include <arch/IO/PARSER/constant.h>
#include <arch/IO/PARSER/register.h>
#include <arch/IO/PARSER/templateregister.h>

// Include back-annotation headers
#include <backannotation/backannotation.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include misc headers
#include <misc/misc.h>

// Include local headers
#include <tc179x/abs_incl.h>
#include <tc179x/asmregisterinitializer.h>
#include <tc179x/assignment_incl.h>
#include <tc179x/auxfuncs.h>
#include <tc179x/cast.h>
#include <tc179x/casting_incl.h>
#include <tc179x/composed_incl.h>
#include <tc179x/generateinputregister.h>
#include <tc179x/mac_incl.h>
#include <tc179x/minmax_incl.h>
#include <tc179x/registrar.h>
#include <tc179x/stack.h>
#include <tc179x/tc_incl.h>

include(tc_prolog.m4)
include(casting_prolog.m4)
include(composed_prolog.m4)
include(mac_prolog.m4)
include(longlong_prolog.m4)
include(assignment_prolog.m4)
include(relation_prolog.m4)
include(comma_prolog.m4)
include(minmax_prolog.m4)
include(abs_prolog.m4)

%}

# This directive includes the set of standard C terminals that are predefined by
# the IR tree pattern matcher library.
include(irterminals-icdcg.m4)

include(tc_decl.m4)
include(casting_decl.m4)
include(composed_decl.m4)
include(mac_decl.m4)
include(longlong_decl.m4)
include(assignment_decl.m4)
include(relation_decl.m4)
include(comma_decl.m4)
include(minmax_decl.m4)
include(abs_decl.m4)

%%

include(tc_rules.m4)
include(casting_rules.m4)
include(composed_rules.m4)
include(mac_rules.m4)
include(longlong_rules.m4)
include(assignment_rules.m4)
include(relation_rules.m4)
include(comma_rules.m4)
include(minmax_rules.m4)
include(abs_rules.m4)

%%
