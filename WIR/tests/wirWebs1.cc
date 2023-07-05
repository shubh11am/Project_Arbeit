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
#include <set>
#include <string>

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

  MIPS p;
  WIR_Function f( "foo" );
  MIPS_RegV &x =
    static_cast<MIPS_RegV &>( f.pushBackVirtualRegister( MIPS_RegV() ) );
  MIPS_RegV &y =
    static_cast<MIPS_RegV &>( f.pushBackVirtualRegister( MIPS_RegV() ) );
  MIPS_RegV &z =
    static_cast<MIPS_RegV &>( f.pushBackVirtualRegister( MIPS_RegV() ) );
  MIPS_RegV &t1 =
    static_cast<MIPS_RegV &>( f.pushBackVirtualRegister( MIPS_RegV() ) );
  MIPS_RegV &t2 =
    static_cast<MIPS_RegV &>( f.pushBackVirtualRegister( MIPS_RegV() ) );
  WIR_id_t xID = x.getID();

  // For this test CFG, we need 7 basic blocks.
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b3 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b5 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b4 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b6 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b7 = f.pushBackBasicBlock( {} );

  // Create a CFG with 2 webs, cf. Muchnick page 252, Fig. 8.17.
  b1.pushBackInstruction(
    { { MIPS::OpCode::SLTI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( t1, WIR_Usage::def ),
        WIR_RegisterParameter( z, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 2 ) } } );
  b1.pushBackInstruction(
    { { MIPS::OpCode::BEQ, MIPS::OperationFormat::RRL,
        WIR_RegisterParameter( t1, WIR_Usage::use ),
        WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
        WIR_LabelParameter( b2 ) } } );

  WIR_Instruction i1;
  WIR_Operation &o1 = i1.pushBackOperation(
    { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
      WIR_RegisterParameter( x, WIR_Usage::def ),
      WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
      MIPS_Immediate16_Signed( 2 ) } );
  b3.pushBackInstruction( move( i1 ) );
  b3.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( b5 ) } } );

  WIR_Instruction i2;
  WIR_Operation &o2 = i2.pushBackOperation(
    { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
      WIR_RegisterParameter( x, WIR_Usage::def ),
      WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
      MIPS_Immediate16_Signed( 1 ) } );
  b2.pushBackInstruction( move( i2 ) );
  b2.pushBackInstruction(
    { { MIPS::OpCode::SLTI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( t2, WIR_Usage::def ),
        WIR_RegisterParameter( z, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 3 ) } } );
  b2.pushBackInstruction(
    { { MIPS::OpCode::BEQ, MIPS::OperationFormat::RRL,
        WIR_RegisterParameter( t2, WIR_Usage::use ),
        WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
        WIR_LabelParameter( b4 ) } } );

  WIR_Instruction i3;
  WIR_Operation &o3 = i3.pushBackOperation(
    { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
      WIR_RegisterParameter( z, WIR_Usage::def ),
      WIR_RegisterParameter( x, WIR_Usage::use ),
      MIPS_Immediate16_Signed( -3 ) } );
  b5.pushBackInstruction( move( i3 ) );
  WIR_Instruction i4;
  WIR_Operation &o4 = i4.pushBackOperation(
    { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
      WIR_RegisterParameter( x, WIR_Usage::def ),
      WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
      MIPS_Immediate16_Signed( 4 ) } );
  b5.pushBackInstruction( move( i4 ) );
  b5.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( b6 ) } } );

  WIR_Instruction i5;
  WIR_Operation &o5 = i5.pushBackOperation(
    { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
      WIR_RegisterParameter( y, WIR_Usage::def ),
      WIR_RegisterParameter( x, WIR_Usage::use ),
      MIPS_Immediate16_Signed( 1 ) } );
  b4.pushBackInstruction( move( i5 ) );
  b4.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( b7 ) } } );

  WIR_Instruction i6;
  WIR_Operation &o6 = i6.pushBackOperation(
    { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
      WIR_RegisterParameter( z, WIR_Usage::def ),
      WIR_RegisterParameter( x, WIR_Usage::use ),
      MIPS_Immediate16_Signed( 7 ) } );
  b6.pushBackInstruction( move( i6 ) );

  b7.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( p.r31(), WIR_Usage::use ) } } );

//   cout << functionregisters << defuse << f;

  // Now, replace all webs in f.
  // Virtual register x must be split in 2 webs: one containing the definitions
  // in b2 and b3 and the uses in b4 and b5, and the other web containing the
  // definition of x in b5 and its use in b6.
  // Furthermore, the degenerated use of z leads to 2 new webs, one for the
  // definition of z in b5, and one for the definition of z in b6.
  // Register x must be removed since it is unused afterwards.
  WIR_Webs a( f );
  a.optimize();

//   cout << f;

  ufAssert( f.getVirtualRegisters().size() == 8 );
  for ( WIR_VirtualRegister &r : f.getVirtualRegisters() )
    ufAssert( r.getID() != xID );

  // Web 1.
  WIR_RegisterParameter &w1p1 =
    dynamic_cast<WIR_RegisterParameter &>( o1.getParameters().front().get() );
  WIR_RegisterParameter &w1p2 =
    dynamic_cast<WIR_RegisterParameter &>( o2.getParameters().front().get() );
  WIR_RegisterParameter &w1p3 =
    dynamic_cast<WIR_RegisterParameter &>(
      (++(o3.getParameters().begin()))->get() );
  WIR_RegisterParameter &w1p4 =
    dynamic_cast<WIR_RegisterParameter &>(
      (++(o5.getParameters().begin()))->get() );

  // Web 2.
  WIR_RegisterParameter &w2p1 =
    dynamic_cast<WIR_RegisterParameter &>( o4.getParameters().front().get() );
  WIR_RegisterParameter &w2p2 =
    dynamic_cast<WIR_RegisterParameter &>(
      (++(o6.getParameters().begin()))->get() );

  // Web 3.
  WIR_RegisterParameter &w3p1 =
    dynamic_cast<WIR_RegisterParameter &>( o3.getParameters().front().get() );

  // Web 4.
  WIR_RegisterParameter &w4p1 =
    dynamic_cast<WIR_RegisterParameter &>( o6.getParameters().front().get() );

  ufAssert( w1p1.getRegister() == w1p2.getRegister() );
  ufAssert( w1p1.getRegister() == w1p3.getRegister() );
  ufAssert( w1p1.getRegister() == w1p4.getRegister() );

  ufAssert( w2p1.getRegister() == w2p2.getRegister() );

  ufAssert( w1p1.getRegister() != w2p1.getRegister() );
  ufAssert( w1p1.getRegister() != w3p1.getRegister() );
  ufAssert( w1p1.getRegister() != w4p1.getRegister() );

  ufAssert( w2p1.getRegister() != w3p1.getRegister() );
  ufAssert( w2p1.getRegister() != w4p1.getRegister() );

  ufAssert( w3p1.getRegister() != w4p1.getRegister() );

  ufAssert( w3p1.getRegister() != z );
  ufAssert( w4p1.getRegister() != z );

  return( 0 );
}
