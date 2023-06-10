/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2009 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _COMPOSED_INCL_H_
#define _COMPOSED_INCL_H_


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>

// Include local headers
#include <tc179x/auxfuncs.h>
#include "composed_initlist_incl.h"
#include "composed_struct_incl.h"


//
// Class forward declarations
//

class IR_ComposedType;
class IR_Symbol;

namespace WIR {
class WIR_BasicBlock;
class WIR_Instruction;
class TC_ARegV;
}

class LLIR_BB;
class LLIR_Function;
class LLIR_Instruction;
class LLIR_Register;


//
// Header section
//

/*!
  @brief copyComposedTypeCost computes the cost of instructions copying a
         composed type object from one memory location to another one.

  @param[in] t A const reference to the IR composed type to be copied.
  @return The costs for the required copy instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
COST copyComposedTypeCost( const IR_ComposedType &t );


/*!
  @brief copyComposedType copies a composed type object from one memory location
         to another one.

  @param[in] t A const reference to the IR composed type to be copied.
  @param[in] source A pointer to the source LLIR address register.
  @param[in] src A const reference to the source virtual address register.
  @param[in] source_offset An integer denoting the source offset.
  @param[in] target A pointer to the target LLIR address register.
  @param[in] tgt A const reference to the target virtual address register.
  @param[in] target_offset An integer denoting the target offset.
  @param[in,out] targetBB A reference to the LLIR basic block that will contain
                          the generated copy code.
  @param[in] insertAfter A pointer to an LLIR instruction within targetBB after
                         which the copy code will be inserted (use nullptr to
                         insert the copy code at the beginning of targetBB).
  @param[in,out] b A reference to the WIR basic block that will contain the
                   generated copy code.
  @param[in] pos An iterator denoting the position within b before that the copy
                 code will be inserted.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void copyComposedType( const IR_ComposedType &t,
                       LLIR_Register *source, const WIR::TC_ARegV &src,
                       int source_offset,
                       LLIR_Register *target, const WIR::TC_ARegV &tgt,
                       int target_offset,
                       LLIR_BB &targetBB, LLIR_Instruction *insertAfter,
                       WIR::WIR_BasicBlock &b,
                       std::list<std::reference_wrapper<WIR::WIR_Instruction>>::const_iterator pos );


/*!
  @brief copyComposedOnStackCost computes the cost of instructions copying a
         composed type onto the stack.

  @param[in] composed A const reference to the composed IR symbol to be copied.
  @return The costs for the required copy instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
COST copyComposedOnStackCost( const IR_Symbol &composed );


/*!
  @brief copyComposedOnStack copies an entire composed type onto the stack.

  @param[in] composed A const reference to the composed IR symbol to be copied.
  @param[in,out] f A reference to a LLIR function having symbol 'composed' as an
                   argument. The copy instructions are inserted at the front of
                   the very first basic block in this function.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void copyComposedOnStack( const IR_Symbol &composed, LLIR_Function &f );

#endif
