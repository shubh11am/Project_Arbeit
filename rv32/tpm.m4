%{
/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

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
#include <arch/riscv/rv32imc.h>

// Include ICD headers
#include <icd-c.h>
#include <icdint/icdint.h>

// Include back-annotation headers
#include <backannotation/backannotation.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include misc headers
#include <misc/misc.h>

// Include local headers
#include <rv32/rv32addressmodification.h>
#include <rv32/rv32addresswithoffset.h>
#include <rv32/rv32cast.h>
#include <rv32/rv32lvalue.h>
#include <rv32/rv32registrar.h>
#include <rv32/rv32stack.h>
#include <rv32/rv32symbolinfo.h>
#include <rv32/rv32_assignment_incl.h>
#include <rv32/rv32_relation_incl.h>
#include <rv32/rv32_incl.h>

include(rv32_prolog.m4)
include(rv32_assignment_prolog.m4)
include(rv32_relation_prolog.m4)

%}

# This directive includes the set of standard C terminals that are predefined by
# the IR tree pattern matcher library.
include(irterminals-icdcg.m4)

include(rv32_decl.m4)
include(rv32_assignment_decl.m4)
include(rv32_relation_decl.m4)
include(rv32_pointer_decl.m4)
include(rv32_casting_decl.m4)

%%

include(rv32_rules.m4)
include(rv32_assignment_rules.m4)
include(rv32_relation_rules.m4)
include(rv32_pointer_rules.m4)
include(rv32_casting_rules.m4)

				  

%%
