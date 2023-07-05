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
  // operations EQ & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &dExt = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &aExt = f.pushBackVirtualRegister( TC_ARegV() );
  auto &a0 = f.pushBackVirtualRegister( TC_ARegV() );
  auto &aPl64 = f.pushBackVirtualRegister( TC_ARegV() );
  auto &aPl123 = f.pushBackVirtualRegister( TC_ARegV() );
  auto &tmp0 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp1 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp2 = f.pushBackVirtualRegister( TC_DRegV() );

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
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dMi123, WIR_Usage::def ),
      new TC_Const16_Signed( -123 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dPl64, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AC18ABSA,
      new WIR_RegisterParameter( a0, WIR_Usage::def ),
      new TC_Const18_Unsigned( 0 ) } );

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AC18ABSA,
      new WIR_RegisterParameter( aPl64, WIR_Usage::def ),
      new TC_Const18_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AC18ABSA,
      new WIR_RegisterParameter( aPl123, WIR_Usage::def ),
      new TC_Const18_Unsigned( 123 ) } );

  // EQ
  auto &eq1 = tcop(
    { TC131::OpCode::EQ, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &eq2 = tcop(
    { TC131::OpCode::EQ, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &eq3 = tcop(
    { TC131::OpCode::EQ, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &eq4 = tcop(
    { TC131::OpCode::EQ, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( -192 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &eq5 = tcop(
    { TC131::OpCode::EQ, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // EQ.A
  auto &eqa1 = tcop(
    { TC131::OpCode::EQ_A, TC131::OperationFormat::DAA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( aPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( aPl123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &eqa2 = tcop(
    { TC131::OpCode::EQ_A, TC131::OperationFormat::DAA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( aPl123, WIR_Usage::use ),
      new WIR_RegisterParameter( aPl123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &eqa3 = tcop(
    { TC131::OpCode::EQ_A, TC131::OperationFormat::DAA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( aExt, WIR_Usage::use ),
      new WIR_RegisterParameter( aPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // EQ.B
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 8 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 24 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 8 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 24 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 29 ),
      new TC_Const5_Unsigned( 1 ) } );

  auto &eqb1 = tcop(
    { TC131::OpCode::EQ_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &eqb2 = tcop(
    { TC131::OpCode::EQ_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // EQ.H
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  auto &eqh1 = tcop(
    { TC131::OpCode::EQ_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 29 ),
      new TC_Const5_Unsigned( 1 ) } );

  auto &eqh2 = tcop(
    { TC131::OpCode::EQ_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &eqh3 = tcop(
    { TC131::OpCode::EQ_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // EQ.W
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  auto &eqw1 = tcop(
    { TC131::OpCode::EQ_W, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new TC_Const16_Signed( -123 ) } );

  auto &eqw2 = tcop(
    { TC131::OpCode::EQ_W, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  auto &eqw3 = tcop(
    { TC131::OpCode::EQ_W, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 29 ),
      new TC_Const5_Unsigned( 1 ) } );

  auto &eqw4 = tcop(
    { TC131::OpCode::EQ_W, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &eqw5 = tcop(
    { TC131::OpCode::EQ_W, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // EQANY.B
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 8 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 24 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 8 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 24 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 29 ),
      new TC_Const5_Unsigned( 1 ) } );

  auto &eqanyb1 = tcop(
    { TC131::OpCode::EQANY_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 8 ) } );

  auto &eqanyb2 = tcop(
    { TC131::OpCode::EQANY_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 8 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 24 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 8 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 24 ),
      new TC_Const5_Unsigned( 8 ) } );

  auto &eqanyb3 = tcop(
    { TC131::OpCode::EQANY_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &eqanyb4 = tcop(
    { TC131::OpCode::EQANY_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // EQANY.H
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  auto &eqanyh1 = tcop(
    { TC131::OpCode::EQANY_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 16 ) } );

  auto &eqanyh2 = tcop(
    { TC131::OpCode::EQANY_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 29 ),
      new TC_Const5_Unsigned( 1 ) } );

  auto &eqanyh3 = tcop(
    { TC131::OpCode::EQANY_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &eqanyh4 = tcop(
    { TC131::OpCode::EQANY_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // EQZ.A
  auto &eqza1 = tcop(
    { TC131::OpCode::EQZ_A, TC131::OperationFormat::DA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( aPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &eqza2 = tcop(
    { TC131::OpCode::EQZ_A, TC131::OperationFormat::DA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( a0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &eqza3 = tcop(
    { TC131::OpCode::EQZ_A, TC131::OperationFormat::DA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( aExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( eq1, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( eq1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( eq1, 1 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eq1, 1 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( eq1, 2 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eq1, 2 ).at( 0 ) == WIR_L4::b1 );

  ufAssert( dval( eq2, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( eq2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( eq2, 1 ).isBinaryInteger() );
  ufAssert( uval( eq2, 2 ).isBinaryInteger() );

  ufAssert( dval( eq3, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( eq3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( uval( eq3, 1 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( eq3, 2 ).isBinaryInteger() );

  ufAssert( dval( eq4, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( eq4, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( eq4, 1 ).extract( 9, 23 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eq4, 1 ).at( 8 ) == WIR_L4::b0 );
  ufAssert( uval( eq4, 1 ).extract( 0, 8 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eq4, 2 ).at( 8 ) == WIR_L4::b1 );
  ufAssert( uval( eq4, 2 ).extract( 0, 8 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( eq5, 0 ).getSignedValue() == 1 );

  ufAssert( dval( eqa1, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( eqa1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( eqa1, 1 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqa1, 1 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( eqa1, 2 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqa1, 2 ).at( 0 ) == WIR_L4::b1 );

  ufAssert( dval( eqa2, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( eqa2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( eqa2, 1 ).isBinaryInteger() );
  ufAssert( uval( eqa2, 2 ).isBinaryInteger() );

  ufAssert( dval( eqa3, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( eqa3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( uval( eqa3, 1 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( eqa3, 2 ).isBinaryInteger() );

  ufAssert( dval( eqb1, 0 ).extract( 0, 8 ).containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( uval( eqb1, 1 ).extract( 0, 8 ).isBinaryInteger() );
  ufAssert( uval( eqb1, 2 ).extract( 0, 8 ).isBinaryInteger() );
  ufAssert( dval( eqb1, 0 ).extract( 8, 8 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( eqb1, 1 ).extract( 9, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqb1, 1 ).at( 8 ) == WIR_L4::b1 );
  ufAssert( uval( eqb1, 2 ).extract( 9, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqb1, 2 ).at( 8 ) == WIR_L4::b0 );
  ufAssert( dval( eqb1, 0 ).extract( 16, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( eqb1, 1 ).extract( 16, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( eqb1, 2 ).extract( 16, 8 ).isBinaryInteger() );
  ufAssert( dval( eqb1, 0 ).extract( 24, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( eqb1, 1 ).extract( 24, 8 ).isBinaryInteger() );
  ufAssert( uval( eqb1, 2 ).extract( 24, 5 ).isBinaryInteger() );
  ufAssert( uval( eqb1, 2 ).at( 29 ) == WIR_L4::bL );
  ufAssert( uval( eqb1, 2 ).extract( 30, 2 ).isBinaryInteger() );

  ufAssert( dval( eqb2, 0 ).containsOnlyBit( WIR_L4::b1 ) );

  ufAssert( dval( eqh1, 0 ).extract( 0, 16 ).containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( uval( eqh1, 1 ).extract( 0, 16 ).isBinaryInteger() );
  ufAssert( uval( eqh1, 2 ).extract( 0, 16 ).isBinaryInteger() );
  ufAssert( dval( eqh1, 0 ).extract( 16, 16 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( eqh1, 1 ).extract( 17, 15 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqh1, 1 ).at( 16 ) == WIR_L4::b1 );
  ufAssert( uval( eqh1, 2 ).extract( 17, 15 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqh1, 2 ).at( 16 ) == WIR_L4::b0 );

  ufAssert( dval( eqh2, 0 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( eqh2, 1 ).extract( 0, 16 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( eqh2, 1 ).extract( 16, 16 ).isBinaryInteger() );
  ufAssert( uval( eqh2, 2 ).extract( 0, 29 ).isBinaryInteger() );
  ufAssert( uval( eqh2, 2 ).at( 29 ) == WIR_L4::bL );
  ufAssert( uval( eqh2, 2 ).extract( 30, 2 ).isBinaryInteger() );

  ufAssert( dval( eqh3, 0 ).containsOnlyBit( WIR_L4::b1 ) );

  ufAssert( dval( eqw1, 0 ).containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( uval( eqw1, 1 ).isBinaryInteger() );
  ufAssert( uval( eqw1, 2 ).isBinaryInteger() );

  ufAssert( dval( eqw2, 0 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( eqw2, 1 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( eqw2, 1 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqw2, 2 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( eqw2, 2 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( eqw3, 0 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( eqw3, 1 ).isBinaryInteger() );
  ufAssert( uval( eqw3, 2 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( eqw4, 0 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( eqw4, 1 ).isBinaryInteger() );
  ufAssert( uval( eqw4, 2 ).extract( 0, 29 ).isBinaryInteger() );
  ufAssert( uval( eqw4, 2 ).at( 29 ) == WIR_L4::bL );
  ufAssert( uval( eqw4, 2 ).extract( 30, 2 ).isBinaryInteger() );

  ufAssert( dval( eqw5, 0 ).containsOnlyBit( WIR_L4::b1 ) );

  ufAssert( dval( eqanyb1, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( eqanyb1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert(
    uval( eqanyb1, 1 ).containsOnlyBits(
      { WIR_L4::bL, WIR_L4::b0, WIR_L4::b1 } ) );
  ufAssert(
    uval( eqanyb1, 2 ).containsOnlyBits(
      { WIR_L4::bL, WIR_L4::b0, WIR_L4::b1 } ) );

  ufAssert( dval( eqanyb2, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( eqanyb2, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert(
    uval( eqanyb2, 1 ).containsOnlyBits(
      { WIR_L4::bL, WIR_L4::b0, WIR_L4::b1 } ) );
  ufAssert(
    uval( eqanyb2, 2 ).containsOnlyBits(
      { WIR_L4::bL, WIR_L4::b0, WIR_L4::b1 } ) );

  ufAssert( dval( eqanyb3, 0 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( eqanyb3, 1 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( eqanyb3, 1 ).extract( 1, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqanyb3, 1 ).at( 8 ) == WIR_L4::b0 );
  ufAssert( uval( eqanyb3, 1 ).extract( 9, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqanyb3, 1 ).at( 16 ) == WIR_L4::b0 );
  ufAssert( uval( eqanyb3, 1 ).extract( 17, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqanyb3, 1 ).at( 24 ) == WIR_L4::b0 );
  ufAssert( uval( eqanyb3, 1 ).extract( 25, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqanyb3, 2 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( eqanyb3, 2 ).extract( 1, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqanyb3, 2 ).at( 8 ) == WIR_L4::b1 );
  ufAssert( uval( eqanyb3, 2 ).extract( 9, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqanyb3, 2 ).at( 16 ) == WIR_L4::b1 );
  ufAssert( uval( eqanyb3, 2 ).extract( 17, 7 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqanyb3, 2 ).at( 24 ) == WIR_L4::b1 );
  ufAssert( uval( eqanyb3, 2 ).extract( 25, 7 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( eqanyb4, 0 ).getSignedValue() == 1 );

  ufAssert( dval( eqanyh1, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( eqanyh1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert(
    uval( eqanyh1, 1 ).containsOnlyBits(
      { WIR_L4::bL, WIR_L4::b0, WIR_L4::b1 } ) );
  ufAssert(
    uval( eqanyh1, 2 ).containsOnlyBits(
      { WIR_L4::bL, WIR_L4::b0, WIR_L4::b1 } ) );

  ufAssert( dval( eqanyh2, 0 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( eqanyh2, 1 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( eqanyh2, 1 ).extract( 1, 15 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqanyh2, 1 ).at( 16 ) == WIR_L4::b1 );
  ufAssert( uval( eqanyh2, 1 ).extract( 17, 15 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqanyh2, 2 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( eqanyh2, 2 ).extract( 1, 15 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqanyh2, 2 ).at( 16 ) == WIR_L4::b0 );
  ufAssert( uval( eqanyh2, 2 ).extract( 17, 15 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( eqanyh3, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( eqanyh3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert(
    uval( eqanyh3, 1 ).containsOnlyBits(
      { WIR_L4::bL, WIR_L4::b0, WIR_L4::b1 } ) );
  ufAssert(
    uval( eqanyh3, 2 ).containsOnlyBits(
      { WIR_L4::bL, WIR_L4::b0, WIR_L4::b1 } ) );

  ufAssert( dval( eqanyh4, 0 ).getSignedValue() == 1 );

  ufAssert( dval( eqza1, 0 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( eqza1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( eqza1, 1 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( eqza1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( eqza2, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( eqza2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( eqza2, 1 ).containsOnlyBit( WIR_L4::b0 ) );

  ufAssert( dval( eqza3, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( eqza3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( uval( eqza3, 1 ).containsOnlyBit( WIR_L4::bL ) );

  return( 0 );
}
