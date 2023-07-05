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

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/arm/armbase.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  ARM_RegV r1;
  ARM_LoRegV r2;
  ARM_HiRegV r3;

  // The following must assert since operation format CRRRAC60_1 only accepts
  // addressing modes lsr and asr.
  WIR_Operation o(
    ARM_Base::OpCode::ADD, ARM_Base::OperationFormat::CRRRAC60_1, WIR_ConditionFieldParameter( ARM_Base::Condition::eq ),
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_AddressingModeParameter( ARM_Base::AddressingMode::lsl ),
    ARM_Const6_Unsigned0( 1 ) );

  // Fallback for disabled failsafe mode.
  auto it = o.end();
  --it;
  --it;
  auto &regP = dynamic_cast<WIR_AddressingModeParameter &>( it->get() );
  ufAssertT(
    regP.getAddressingMode().isCompatible(
      ARM_Base::AddressingMode::lsr, ARM_Base::OperationFormat::CRRRAC60_1,
      ARM_Base::OpCode::ADD ),
    "Parameters incompatible with operation format '" <<
    o.getOperationFormat().getName() << "'." );

  return( 0 );
}
