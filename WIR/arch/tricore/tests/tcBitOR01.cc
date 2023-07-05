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
  // operations OR & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &dExt = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dFF0000EE = f.pushBackVirtualRegister( TC_DRegV() );
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
      new WIR_RegisterParameter( dFF0000EE, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFF00 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( dFF0000EE, WIR_Usage::def ),
      new WIR_RegisterParameter( dFF0000EE, WIR_Usage::use ),
      new TC_Const16_Signed( 0x00EE ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dMi123, WIR_Usage::def ),
      new TC_Const16_Signed( -123 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dPl64, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  // OR
  auto &or1 = tcop(
    { TC131::OpCode::OR, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dFF0000EE, WIR_Usage::use ),
      new TC_Const9_Unsigned( 122 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // OR.AND.T
  auto &orandt1 = tcop(
    { TC131::OpCode::OR_AND_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &orandt2 = tcop(
    { TC131::OpCode::OR_AND_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 7 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // OR.ANDN.T
  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 1 ) } );

  auto &orandnt1 = tcop(
    { TC131::OpCode::OR_ANDN_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 7 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &orandnt2 = tcop(
    { TC131::OpCode::OR_ANDN_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 7 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // OR.NOR.T
  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 1 ) } );

  auto &ornort1 = tcop(
    { TC131::OpCode::OR_NOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 7 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &ornort2 = tcop(
    { TC131::OpCode::OR_NOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // OR.OR.T
  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 1 ) } );

  auto &orort1 = tcop(
    { TC131::OpCode::OR_OR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &orort2 = tcop(
    { TC131::OpCode::OR_OR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 7 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // OR.EQ
  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 1 ) } );

  auto &oreq1 = tcop(
    { TC131::OpCode::OR_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 68 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &oreq2 = tcop(
    { TC131::OpCode::OR_EQ, TC131::OperationFormat::DDC9_3,
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

  auto &oreq3 = tcop(
    { TC131::OpCode::OR_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( 68 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &oreq4 = tcop(
    { TC131::OpCode::OR_EQ, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // OR.GE
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &orge1 = tcop(
    { TC131::OpCode::OR_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const9_Signed( -64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &orge2 = tcop(
    { TC131::OpCode::OR_GE, TC131::OperationFormat::DDC9_3,
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

  auto &orge3 = tcop(
    { TC131::OpCode::OR_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( -64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &orge4 = tcop(
    { TC131::OpCode::OR_GE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // OR.GE.U
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &orgeu1 = tcop(
    { TC131::OpCode::OR_GE_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Unsigned( 65 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &orgeu2 = tcop(
    { TC131::OpCode::OR_GE_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const9_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // OR.LT
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &orlt1 = tcop(
    { TC131::OpCode::OR_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &orlt2 = tcop(
    { TC131::OpCode::OR_LT, TC131::OperationFormat::DDC9_3,
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
      new TC_Const16_Signed( 0 ) } );

  auto &orlt3 = tcop(
    { TC131::OpCode::OR_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( -64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &orlt4 = tcop(
    { TC131::OpCode::OR_LT, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( dPl64, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  // OR.LT.U
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &orltu1 = tcop(
    { TC131::OpCode::OR_LT_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const9_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &orltu2 = tcop(
    { TC131::OpCode::OR_LT_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Unsigned( 65 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // OR.NE
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &orne1 = tcop(
    { TC131::OpCode::OR_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &orne2 = tcop(
    { TC131::OpCode::OR_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 68 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &orne3 = tcop(
    { TC131::OpCode::OR_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( 68 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &orne4 = tcop(
    { TC131::OpCode::OR_NE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( dPl64, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  // OR.T
  auto &ort1 = tcop(
    { TC131::OpCode::OR_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &ort2 = tcop(
    { TC131::OpCode::OR_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 7 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // ORN
  auto &orn1 = tcop(
    { TC131::OpCode::ORN, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dFF0000EE, WIR_Usage::use ),
      new TC_Const9_Unsigned( 123 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &orn2 = tcop(
    { TC131::OpCode::ORN, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // ORN.T
  auto &ornt1 = tcop(
    { TC131::OpCode::ORN_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &ornt2 = tcop(
    { TC131::OpCode::ORN_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( or1, 0 ).getSignedValue() == 0xFF0000FE );
  ufAssert( uval( or1, 1 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( or1, 1 ).at( 1 ) == WIR_L4::bX );
  ufAssert( uval( or1, 1 ).at( 2 ) == WIR_L4::b1 );
  ufAssert( uval( or1, 1 ).extract( 3, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( or1, 1 ).extract( 7, 25 ).isBinaryInteger() );
  ufAssert( uval( or1, 2 ).extract( 0, 2 ).isBinaryInteger() );
  ufAssert( uval( or1, 2 ).at( 2 ) == WIR_L4::bX );
  ufAssert( uval( or1, 2 ).extract( 3, 4 ).containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( uval( or1, 2 ).at( 7 ) == WIR_L4::bX );
  ufAssert( uval( or1, 2 ).at( 8 ) == WIR_L4::b0 );

  ufAssert( dval( orandt1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( orandt1, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( orandt1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orandt1, 1 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( orandt1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orandt1, 3 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( orandt2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( orandt2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( orandt2, 1 ).extract( 0, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orandt2, 1 ).at( 7 ) == WIR_L4::b1 );
  ufAssert( uval( orandt2, 1 ).extract( 8, 24 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orandt2, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orandt2, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( orandt2, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( orandnt1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( orandnt1, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( orandnt1, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orandnt1, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orandnt1, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert(
    uval( orandnt1, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( orandnt2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( orandnt2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( orandnt2, 1 ).extract( 0, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orandnt2, 1 ).at( 7 ) == WIR_L4::b1 );
  ufAssert(
    uval( orandnt2, 1 ).extract( 8, 24 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orandnt2, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orandnt2, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert(
    uval( orandnt2, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( ornort1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( ornort1, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( ornort1, 1 ).extract( 0, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ornort1, 1 ).at( 7 ) == WIR_L4::b1 );
  ufAssert( uval( ornort1, 1 ).extract( 8, 24 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ornort1, 3 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( ornort2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( ornort2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( ornort2, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ornort2, 1 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( ornort2, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ornort2, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ornort2, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( ornort2, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( orort1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( orort1, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( orort1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orort1, 1 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( orort1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orort1, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orort1, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( orort1, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( orort2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( orort2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( orort2, 1 ).extract( 0, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orort2, 1 ).at( 7 ) == WIR_L4::b1 );
  ufAssert( uval( orort2, 1 ).extract( 8, 24 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orort2, 3 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( oreq1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( oreq1, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( oreq1, 1 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( oreq1, 1 ).at( 2 ) == WIR_L4::b0 );
  ufAssert( uval( oreq1, 1 ).extract( 3, 29 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( oreq1, 2 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( oreq1, 2 ).at( 2 ) == WIR_L4::b1 );
  ufAssert( uval( oreq1, 2 ).extract( 3, 6 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( oreq2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( oreq2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( oreq2, 1 ).isBinaryInteger() );
  ufAssert( uval( oreq2, 2 ).isBinaryInteger() );

  ufAssert( dval( oreq3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( uval( oreq3, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( oreq3, 1 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( oreq3, 2 ).isBinaryInteger() );

  ufAssert( dval( oreq4, 0 ).at( 0 ) == WIR_L4::b1 );

  ufAssert( dval( orge1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( orge1, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( orge1, 1 ).isBinaryInteger() );
  ufAssert( uval( orge1, 2 ).isBinaryInteger() );

  ufAssert( dval( orge2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( orge2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( orge2, 1 ).isBinaryInteger() );
  ufAssert( uval( orge2, 2 ).isBinaryInteger() );

  ufAssert( dval( orge3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( uval( orge3, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( orge3, 1 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( orge3, 2 ).isBinaryInteger() );

  ufAssert( dval( orge4, 0 ).at( 0 ) == WIR_L4::b1 );

  ufAssert( dval( orgeu1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( orgeu1, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( orgeu1, 1 ).isBinaryInteger() );
  ufAssert( uval( orgeu1, 2 ).isBinaryInteger() );

  ufAssert( dval( orgeu2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( orgeu2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( orgeu2, 1 ).isBinaryInteger() );
  ufAssert( uval( orgeu2, 2 ).isBinaryInteger() );

  ufAssert( dval( orlt1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( orlt1, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( orlt1, 1 ).isBinaryInteger() );
  ufAssert( uval( orlt1, 2 ).isBinaryInteger() );

  ufAssert( dval( orlt2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( orlt2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( orlt2, 1 ).isBinaryInteger() );
  ufAssert( uval( orlt2, 2 ).isBinaryInteger() );

  ufAssert( dval( orlt3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( uval( orlt3, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( orlt3, 1 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( orlt3, 2 ).isBinaryInteger() );

  ufAssert( dval( orlt4, 0 ).getSignedValue() == 64 );

  ufAssert( dval( orltu1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( orltu1, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( orltu1, 1 ).isBinaryInteger() );
  ufAssert( uval( orltu1, 2 ).isBinaryInteger() );

  ufAssert( dval( orltu2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( orltu2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( orltu2, 1 ).isBinaryInteger() );
  ufAssert( uval( orltu2, 2 ).isBinaryInteger() );

  ufAssert( dval( orne1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( orne1, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( orne1, 1 ).isBinaryInteger() );
  ufAssert( uval( orne1, 2 ).isBinaryInteger() );

  ufAssert( dval( orne2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( orne2, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( orne2, 1 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orne2, 1 ).at( 2 ) == WIR_L4::b0 );
  ufAssert( uval( orne2, 1 ).extract( 3, 29 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orne2, 2 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orne2, 2 ).at( 2 ) == WIR_L4::b1 );
  ufAssert( uval( orne2, 2 ).extract( 3, 6 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( orne3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( uval( orne3, 0 ).at( 0 ) != WIR_L4::bX );
  ufAssert( uval( orne3, 1 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( orne3, 2 ).isBinaryInteger() );

  ufAssert( dval( orne4, 0 ).getSignedValue() == 64 );

  ufAssert( dval( ort1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( ort1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ort1, 1 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( ort1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ort1, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ort1, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( ort1, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( ort2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( ort2, 1 ).extract( 0, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ort2, 1 ).at( 7 ) == WIR_L4::b1 );
  ufAssert( uval( ort2, 1 ).extract( 8, 24 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ort2, 3 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( orn1, 0 ).getSignedValue() == 0xFFFFFFEE );
  ufAssert( uval( orn1, 1 ).extract( 0, 2 ).isBinaryInteger() );
  ufAssert( uval( orn1, 1 ).at( 2 ) == WIR_L4::bX );
  ufAssert( uval( orn1, 1 ).extract( 3, 4 ).isBinaryInteger() );
  ufAssert( uval( orn1, 1 ).extract( 7, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orn1, 2 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( orn1, 2 ).at( 1 ) == WIR_L4::bX );
  ufAssert( uval( orn1, 2 ).at( 2 ) == WIR_L4::b0 );
  ufAssert( uval( orn1, 2 ).at( 3 ) == WIR_L4::bX );
  ufAssert( uval( orn1, 2 ).at( 4 ) == WIR_L4::b1 );
  ufAssert( uval( orn1, 2 ).extract( 5, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( orn1, 2 ).extract( 7, 2 ).containsOnlyBit( WIR_L4::b0 ) );

  ufAssert( dval( orn2, 0 ).containsOnlyBit( WIR_L4::b1 ) );

  ufAssert( dval( ornt1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( ornt1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ornt1, 1 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( ornt1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ornt1, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ornt1, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( ornt1, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( ornt2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( ornt2, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ornt2, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ornt2, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( ornt2, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  return( 0 );
}
