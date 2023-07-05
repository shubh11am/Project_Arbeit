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
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  WIR_Instruction i;
  MIPS_RegV r1, r2;

  // Insert operations in DESCENDING order of IDs.
  i.pushBackOperation(
    WIR_Operation(
      MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
      WIR_RegisterParameter( r1, WIR_Usage::def ),
      MIPS_Immediate16_Signed( -138 ),
      WIR_RegisterParameter( r2, WIR_Usage::use ) ) );
  i.pushFrontOperation(
    WIR_Operation(
      MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
      WIR_RegisterParameter( r1, WIR_Usage::def ),
      WIR_RegisterParameter( r2, WIR_Usage::use ),
      MIPS_Immediate16_Signed( 1 ) ) );
  i.pushFrontOperation(
    WIR_Operation(
      MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
      WIR_RegisterParameter( r1, WIR_Usage::def ),
      WIR_RegisterParameter( r2, WIR_Usage::use ),
      MIPS_Immediate16_Signed( -4 ) ) );

  // Copy instruction. The operations in i1 should also appear in DESCENDING ID
  // order.
  WIR_Instruction i1( i );

  ufAssert( i.getOperations().size() == i1.getOperations().size() );

  // Check descending order of operation IDs.
  WIR_id_t prev = nullid;
  for ( WIR_Operation &o : i1 ) {
    if ( prev == nullid )
      ufAssert( o.getID() > prev );
    else
      ufAssert( o.getID() < prev );
    prev = o.getID();
  }

  return( 0 );
}
