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

// Include WIR headers
#include <wir/wir.h>
#include <arch/arm/armv5te.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  ARMv5TE p;
  const ARM_LoRegP &r4 = p.R4();
  const ARMv5TE_PRegP &p6 = p.P6();
  WIR_BasicBlock b;
  WIR_Instruction i;

  // The following operations must be accepted for the ARMv5TE ISA.
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
  const vector<ARM_Base::AddressingMode> memoryAddSubModes {
    ARM_Base::AddressingMode::plus,
    ARM_Base::AddressingMode::minus };

  for ( auto &cond : conditions )
    for ( auto &addsub : memoryAddSubModes ) {
      i.pushBackOperation(
        WIR_Operation(
          ARMv5TE::OpCode::LDRD, ARMv5TE::OperationFormat::CPRAC8_1,
          WIR_ConditionFieldParameter( cond ),
          WIR_RegisterParameter( p6, WIR_Usage::def ),
          WIR_RegisterParameter( r4, WIR_Usage::use ),
          WIR_AddressingModeParameter( addsub ),
          ARM_Const8_Unsigned( 255 ) ) );
      b.pushBackInstruction( move( i ) );
    }

  cout << arm << b;

  return( 0 );
}
