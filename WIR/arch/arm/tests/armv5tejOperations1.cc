/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2015 - 2022, Heiko Falk.

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


//
// Include section
//

// Include standard headers
#include <vector>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/arm/armv5tej.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  ARMv5TEJ p;
  const ARM_HiRegP &r8 = p.R8();

  const vector<ARM_Base::Condition> conditions {
    ARM_Base::Condition::eq,
    ARM_Base::Condition::ne,
    ARM_Base::Condition::hs,
    ARM_Base::Condition::lo,
    ARM_Base::Condition::mi,
    ARM_Base::Condition::pl,
    ARM_Base::Condition::vs,
    ARM_Base::Condition::vc,
    ARM_Base::Condition::hi,
    ARM_Base::Condition::ls,
    ARM_Base::Condition::ge,
    ARM_Base::Condition::lt,
    ARM_Base::Condition::gt,
    ARM_Base::Condition::le,
    ARM_Base::Condition::al };

  // The following operations must be accepted for the ARMv5TEJ ISA.
  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARMv5TEJ::OpCode::BXJ, ARMv4T::OperationFormat::CR_3,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::use ) );

    ufAssert( o.getSize() == 4 );
    ufAssert(
      !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
      !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
      o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
      o.isIndirectJump() );
  }

  return( 0 );
}
