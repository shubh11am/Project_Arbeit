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
#include <iterator>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>
#include <arch/tricore/analyses/bit/tcbitdfa.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  // This test case tests the bit-true top-down data flow analysis for TriCore
  // operations AND & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &dExt = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dFF0000EF = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp0 = f.pushBackVirtualRegister( TC_DRegV() );

  // This lambda serves for generating and inserting a TriCore operation.
  auto tcop = [&]( WIR_Operation &&o ) -> WIR_Operation & {
    auto &i = b1.pushBackInstruction( WIR_Instruction { WIR_Operation { o } } );
    return( i.begin()->get() );
  };

  // This lambda serves to retrieve an operation's outgoing down value.
  auto dval = []( const WIR_Operation &o,
                  unsigned int pos ) -> const WIR_UpDownValue & {
    auto it = o.begin();
    std::advance( it, pos );
    auto &c = it->get().getContainers<WIR_BitValues>().begin()->get();
    return( c.getOutValues().begin()->downVal );
  };

  // This lambda serves to retrieve an operation's incoming up value.
  auto uval = []( const WIR_Operation &o,
                  unsigned int pos ) -> const WIR_UpDownValue & {
    auto it = o.begin();
    std::advance( it, pos );
    auto &c = it->get().getContainers<WIR_BitValues>().begin()->get();
    return( c.getInValues().begin()->upVal );
  };

  // Create WIR code.

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( dFF0000EF, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFF00 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( dFF0000EF, WIR_Usage::def ),
      new WIR_RegisterParameter( dFF0000EF, WIR_Usage::use ),
      new TC_Const16_Signed( 0x00EF ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dMi123, WIR_Usage::def ),
      new TC_Const16_Signed( -123 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dPl64, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  // AND
  auto &and1 = tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dFF0000EF, WIR_Usage::use ),
      new TC_Const9_Unsigned( 123 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // AND.AND.T
  auto &andandt1 = tcop(
    { TC131::OpCode::AND_AND_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andandt2 = tcop(
    { TC131::OpCode::AND_AND_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // AND.ANDN.T
  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 1 ) } );

  auto &andandnt1 = tcop(
    { TC131::OpCode::AND_ANDN_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andandnt2 = tcop(
    { TC131::OpCode::AND_ANDN_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // AND.NOR.T
  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 1 ) } );

  auto &andnort1 = tcop(
    { TC131::OpCode::AND_NOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andnort2 = tcop(
    { TC131::OpCode::AND_NOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 2 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // AND.OR.T
  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 1 ) } );

  auto &andort1 = tcop(
    { TC131::OpCode::AND_OR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andort2 = tcop(
    { TC131::OpCode::AND_OR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // AND.EQ
  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 1 ) } );

  auto &andeq1 = tcop(
    { TC131::OpCode::AND_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andeq2 = tcop(
    { TC131::OpCode::AND_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 68 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andeq3 = tcop(
    { TC131::OpCode::AND_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 1 ) } );

  auto &andeq4 = tcop(
    { TC131::OpCode::AND_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( 68 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andeq5 = tcop(
    { TC131::OpCode::AND_EQ, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( dPl64, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  // AND.GE
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 1 ) } );

  auto &andge1 = tcop(
    { TC131::OpCode::AND_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andge2 = tcop(
    { TC131::OpCode::AND_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const9_Signed( -64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 1 ) } );

  auto &andge3 = tcop(
    { TC131::OpCode::AND_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( -64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andge4 = tcop(
    { TC131::OpCode::AND_GE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( dPl64, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  // AND.GE.U
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 1 ) } );

  auto &andgeu1 = tcop(
    { TC131::OpCode::AND_GE_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andgeu2 = tcop(
    { TC131::OpCode::AND_GE_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Unsigned( 65 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // AND.LT
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 1 ) } );

  auto &andlt1 = tcop(
    { TC131::OpCode::AND_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 1 ) } );

  auto &andlt2 = tcop(
    { TC131::OpCode::AND_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const9_Signed( -64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 1 ) } );

  auto &andlt3 = tcop(
    { TC131::OpCode::AND_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( -64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andlt4 = tcop(
    { TC131::OpCode::AND_LT, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // AND.LT.U
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 1 ) } );

  auto &andltu1 = tcop(
    { TC131::OpCode::AND_LT_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 1 ) } );

  auto &andltu2 = tcop(
    { TC131::OpCode::AND_LT_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Unsigned( 65 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // AND.NE
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 1 ) } );

  auto &andne1 = tcop(
    { TC131::OpCode::AND_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 68 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andne2 = tcop(
    { TC131::OpCode::AND_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 1 ) } );

  auto &andne3 = tcop(
    { TC131::OpCode::AND_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( 68 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andne4 = tcop(
    { TC131::OpCode::AND_NE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // AND.T
  auto &andt1 = tcop(
    { TC131::OpCode::AND_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andt2 = tcop(
    { TC131::OpCode::AND_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // ANDN
  auto &andn1 = tcop(
    { TC131::OpCode::ANDN, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dFF0000EF, WIR_Usage::use ),
      new TC_Const9_Unsigned( 123 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andn2 = tcop(
    { TC131::OpCode::ANDN, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // ANDN.T
  auto &andnt1 = tcop(
    { TC131::OpCode::ANDN_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &andnt2 = tcop(
    { TC131::OpCode::ANDN_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( and1, 0 ).getSignedValue() == 107 );
  ufAssert( uval( and1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( and1, 1 ).at( 2 ) == WIR_L4::bX );
  ufAssert( uval( and1, 2 ).at( 4 ) == WIR_L4::bX );

  ufAssert( dval( andandt1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andandt1, 0 ).isBinaryInteger() );
  ufAssert(
    uval( andandt1, 1 ).extract( 0, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andandt1, 1 ).at( 25 ) == WIR_L4::b1 );
  ufAssert(
    uval( andandt1, 1 ).extract( 26, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert(
    uval( andandt1, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andandt1, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert(
    uval( andandt1, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( andandt2, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( andandt2, 0 ).extract( 1, 31 ).isBinaryInteger() );
  ufAssert( uval( andandt2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( andandt2, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert(
    uval( andandt2, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andandt2, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert(
    uval( andandt2, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( andandnt1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andandnt1, 0 ).isBinaryInteger() );
  ufAssert(
    uval( andandnt1, 1 ).extract( 0, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andandnt1, 1 ).at( 25 ) == WIR_L4::b1 );
  ufAssert(
    uval( andandnt1, 1 ).extract( 26, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert(
    uval( andandnt1, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andandnt1, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert(
    uval( andandnt1, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( andandnt2, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( andandnt2, 0 ).extract( 1, 31 ).isBinaryInteger() );
  ufAssert( uval( andandnt2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( andandnt2, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert(
    uval( andandnt2, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andandnt2, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert(
    uval( andandnt2, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( andnort1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andnort1, 0 ).isBinaryInteger() );
  ufAssert( uval( andnort1, 1 ).extract( 0, 3 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andnort1, 1 ).at( 3 ) == WIR_L4::b0 );
  ufAssert(
    uval( andnort1, 1 ).extract( 4, 28 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andnort1, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andnort1, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert(
    uval( andnort1, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( andnort2, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( andnort2, 0 ).extract( 1, 31 ).isBinaryInteger() );
  ufAssert( uval( andnort2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( andnort2, 1 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andnort2, 1 ).at( 2 ) == WIR_L4::b1 );
  ufAssert(
    uval( andnort2, 1 ).extract( 3, 29 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andnort2, 3 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( andort1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andort1, 0 ).isBinaryInteger() );
  ufAssert( uval( andort1, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andort1, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andort1, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( andort1, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( andort2, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( andort2, 0 ).extract( 1, 31 ).isBinaryInteger() );
  ufAssert( uval( andort2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( andort2, 1 ).extract( 0, 3 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andort2, 1 ).at( 3 ) == WIR_L4::b0 );
  ufAssert(
    uval( andort2, 1 ).extract( 4, 28 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andort2, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andort2, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( andort2, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( andeq1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andeq1, 0 ).isBinaryInteger() );
  ufAssert( uval( andeq1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andeq1, 1 ).isBinaryInteger() );
  ufAssert( uval( andeq1, 1 ).getSignedValue() == 64 );
  ufAssert( uval( andeq1, 2 ).isBinaryInteger() );
  ufAssert( uval( andeq1, 2 ).getSignedValue() == 64 );

  ufAssert( dval( andeq2, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( andeq2, 0 ).extract( 1, 31 ).isBinaryInteger() );
  ufAssert( uval( andeq2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( andeq2, 1 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andeq2, 1 ).at( 2 ) == WIR_L4::b0 );
  ufAssert( uval( andeq2, 1 ).extract( 3, 29 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andeq2, 2 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andeq2, 2 ).at( 2 ) == WIR_L4::b1 );
  ufAssert( uval( andeq2, 2 ).extract( 3, 6 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( uval( andeq3, 0 ).isBinaryInteger() );
  ufAssert( uval( andeq3, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( andeq3, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andeq3, 2 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( andeq4, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( uval( andeq4, 0 ).isBinaryInteger() );
  ufAssert( uval( andeq4, 1 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( andeq4, 2 ).isBinaryInteger() );

  ufAssert( dval( andeq5, 0 ).getSignedValue() == 64 );

  ufAssert( dval( andge1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andge1, 0 ).isBinaryInteger() );
  ufAssert( uval( andge1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andge1, 1 ).isBinaryInteger() );
  ufAssert( uval( andge1, 1 ).getSignedValue() == 64 );
  ufAssert( uval( andge1, 2 ).isBinaryInteger() );
  ufAssert( uval( andge1, 2 ).getSignedValue() == 64 );

  ufAssert( dval( andge2, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( andge2, 0 ).extract( 1, 31 ).isBinaryInteger() );
  ufAssert( uval( andge2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( andge2, 1 ).isBinaryInteger() );
  ufAssert( uval( andge2, 1 ).getSignedValue() == -123 );
  ufAssert( uval( andge2, 2 ).isBinaryInteger() );
  ufAssert( uval( andge2, 2 ).getSignedValue() == -64 );

  ufAssert( dval( andge3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( uval( andge3, 0 ).isBinaryInteger() );
  ufAssert( uval( andge3, 1 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( andge3, 2 ).isBinaryInteger() );

  ufAssert( dval( andge4, 0 ).getSignedValue() == 64 );

  ufAssert( dval( andgeu1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andgeu1, 0 ).isBinaryInteger() );
  ufAssert( uval( andgeu1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andgeu1, 1 ).isBinaryInteger() );
  ufAssert( uval( andgeu1, 1 ).getSignedValue() == 64 );
  ufAssert( uval( andgeu1, 2 ).isBinaryInteger() );
  ufAssert( uval( andgeu1, 2 ).getSignedValue() == 64 );

  ufAssert( dval( andgeu2, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( andgeu2, 0 ).extract( 1, 31 ).isBinaryInteger() );
  ufAssert( uval( andgeu2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( andgeu2, 1 ).isBinaryInteger() );
  ufAssert( uval( andgeu2, 1 ).getSignedValue() == 64 );
  ufAssert( uval( andgeu2, 2 ).isBinaryInteger() );
  ufAssert( uval( andgeu2, 2 ).getSignedValue() == 65 );

  ufAssert( dval( andlt1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( andlt1, 0 ).extract( 1, 31 ).isBinaryInteger() );
  ufAssert( uval( andlt1, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( andlt1, 1 ).isBinaryInteger() );
  ufAssert( uval( andlt1, 1 ).getSignedValue() == 64 );
  ufAssert( uval( andlt1, 2 ).isBinaryInteger() );
  ufAssert( uval( andlt1, 2 ).getSignedValue() == 64 );

  ufAssert( dval( andlt2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andlt2, 0 ).isBinaryInteger() );
  ufAssert( uval( andlt2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andlt2, 1 ).isBinaryInteger() );
  ufAssert( uval( andlt2, 1 ).getSignedValue() == -123 );
  ufAssert( uval( andlt2, 2 ).isBinaryInteger() );
  ufAssert( uval( andlt2, 2 ).getSignedValue() == -64 );

  ufAssert( dval( andlt3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( uval( andlt3, 0 ).isBinaryInteger() );
  ufAssert( uval( andlt3, 1 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( andlt3, 2 ).isBinaryInteger() );

  ufAssert( dval( andlt4, 0 ).at( 0 ) == WIR_L4::b0 );

  ufAssert( dval( andltu1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( andltu1, 0 ).extract( 1, 31 ).isBinaryInteger() );
  ufAssert( uval( andltu1, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( andltu1, 1 ).isBinaryInteger() );
  ufAssert( uval( andltu1, 1 ).getSignedValue() == 64 );
  ufAssert( uval( andltu1, 2 ).isBinaryInteger() );
  ufAssert( uval( andltu1, 2 ).getSignedValue() == 64 );

  ufAssert( dval( andltu2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andltu2, 0 ).isBinaryInteger() );
  ufAssert( uval( andltu2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andltu2, 1 ).isBinaryInteger() );
  ufAssert( uval( andltu2, 1 ).getSignedValue() == 64 );
  ufAssert( uval( andltu2, 2 ).isBinaryInteger() );
  ufAssert( uval( andltu2, 2 ).getSignedValue() == 65 );

  ufAssert( dval( andne1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andne1, 0 ).isBinaryInteger() );
  ufAssert( uval( andne1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andne1, 1 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andne1, 1 ).at( 2 ) == WIR_L4::b0 );
  ufAssert( uval( andne1, 1 ).extract( 3, 29 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andne1, 2 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andne1, 2 ).at( 2 ) == WIR_L4::b1 );
  ufAssert( uval( andne1, 2 ).extract( 3, 6 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( andne2, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( andne2, 0 ).extract( 1, 31 ).isBinaryInteger() );
  ufAssert( uval( andne2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( andne2, 1 ).isBinaryInteger() );
  ufAssert( uval( andne2, 1 ).getSignedValue() == 64 );
  ufAssert( uval( andne2, 2 ).isBinaryInteger() );
  ufAssert( uval( andne2, 2 ).getSignedValue() == 64 );

  ufAssert( dval( andne3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( uval( andne3, 0 ).isBinaryInteger() );
  ufAssert( uval( andne3, 1 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( andne3, 2 ).isBinaryInteger() );

  ufAssert( dval( andne4, 0 ).at( 0 ) == WIR_L4::b0 );

  ufAssert( dval( andt1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andt1, 1 ).extract( 0, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andt1, 1 ).at( 25 ) == WIR_L4::b1 );
  ufAssert( uval( andt1, 1 ).extract( 26, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andt1, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andt1, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( andt1, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( andt2, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( andt2, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andt2, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andt2, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( andt2, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( andn1, 0 ).getSignedValue() == 0xFF000084 );
  ufAssert( uval( andn1, 1 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andn1, 1 ).at( 2 ) == WIR_L4::b1 );
  ufAssert( uval( andn1, 1 ).extract( 3, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andn1, 1 ).extract( 7, 25 ).isBinaryInteger() );
  ufAssert( uval( andn1, 2 ).extract( 0, 8 ).isBinaryInteger() );
  ufAssert( uval( andn1, 2 ).at( 8 ) == WIR_L4::bX );

  ufAssert( dval( andn2, 0 ).getSignedValue() == 0 );

  ufAssert( dval( andnt1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( andnt1, 1 ).extract( 0, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andnt1, 1 ).at( 25 ) == WIR_L4::b1 );
  ufAssert( uval( andnt1, 1 ).extract( 26, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andnt1, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andnt1, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( andnt1, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( andnt2, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( andnt2, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andnt2, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( andnt2, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( andnt2, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  return( 0 );
}
