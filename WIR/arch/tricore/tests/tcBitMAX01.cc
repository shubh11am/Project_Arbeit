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
  // operations MAX/MIN & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &dExt = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
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

  // MAX
  auto &max1 = tcop(
    { TC131::OpCode::MAX, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &max2 = tcop(
    { TC131::OpCode::MAX, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &max3 = tcop(
    { TC131::OpCode::MAX, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // MAX.U
  auto &maxu1 = tcop(
    { TC131::OpCode::MAX_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &maxu2 = tcop(
    { TC131::OpCode::MAX_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &maxu3 = tcop(
    { TC131::OpCode::MAX_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // MAX.B
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
      new TC_Const16_Signed( -63 ) } );

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

  auto &maxb1 = tcop(
    { TC131::OpCode::MAX_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::EXTR, TC131::OperationFormat::DDC5C5,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 8 ) } );

  // MAX.BU
  auto &maxbu1 = tcop(
    { TC131::OpCode::MAX_BU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // MAX.H
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
      new TC_Const16_Signed( -63 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  auto &maxh1 = tcop(
    { TC131::OpCode::MAX_H, TC131::OperationFormat::DDD_1,
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

  auto &maxh2 = tcop(
    { TC131::OpCode::MAX_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // MAX.HU
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
      new TC_Const16_Signed( -63 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  auto &maxhu1 = tcop(
    { TC131::OpCode::MAX_HU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::EXTR, TC131::OperationFormat::DDC5C5,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  // MIN
  auto &min1 = tcop(
    { TC131::OpCode::MIN, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &min2 = tcop(
    { TC131::OpCode::MIN, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &min3 = tcop(
    { TC131::OpCode::MIN, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // MIN.U
  auto &minu1 = tcop(
    { TC131::OpCode::MIN_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &minu2 = tcop(
    { TC131::OpCode::MIN_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &minu3 = tcop(
    { TC131::OpCode::MIN_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // MIN.B
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
      new TC_Const16_Signed( -63 ) } );

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

  auto &minb1 = tcop(
    { TC131::OpCode::MIN_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // MIN.BU
  auto &minbu1 = tcop(
    { TC131::OpCode::MIN_BU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // MIN.H
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
      new TC_Const16_Signed( -63 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  auto &minh1 = tcop(
    { TC131::OpCode::MIN_H, TC131::OperationFormat::DDD_1,
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

  auto &minh2 = tcop(
    { TC131::OpCode::MIN_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // MIN.HU
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
      new TC_Const16_Signed( -63 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  auto &minhu1 = tcop(
    { TC131::OpCode::MIN_HU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( max1, 0 ).getSignedValue() == 64 );
  ufAssert( dval( max2, 0 ).getSignedValue() == 64 );
  ufAssert( dval( max3, 0 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( maxu1, 0 ).getSignedValue() == 0xFFFFFF85 );
  ufAssert( dval( maxu2, 0 ).getSignedValue() == 0xFFFFFF85 );
  ufAssert( dval( maxu3, 0 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( maxb1, 0 ).extract( 0, 8 ).getSignedValue() == 64 );
  ufAssert( dval( maxb1, 0 ).extract( 8, 8 ).getSignedValue() == 64 );
  ufAssert( dval( maxb1, 0 ).extract( 16, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( maxb1, 0 ).extract( 24, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( maxb1, 1 ).extract( 0, 8 ).isBinaryInteger() );
  ufAssert( uval( maxb1, 1 ).extract( 8, 24 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( maxb1, 2 ).extract( 0, 8 ).isBinaryInteger() );
  ufAssert( uval( maxb1, 2 ).extract( 8, 24 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( maxbu1, 0 ).extract( 0, 8 ).getSignedValue() == 193 );
  ufAssert( dval( maxbu1, 0 ).extract( 8, 8 ).getSignedValue() == 133 );
  ufAssert( dval( maxbu1, 0 ).extract( 16, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( maxbu1, 0 ).extract( 24, 8 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( maxh1, 0 ).extract( 0, 16 ).getSignedValue() == 64 );
  ufAssert( dval( maxh1, 0 ).extract( 16, 16 ).getSignedValue() == 64 );
  ufAssert( dval( maxh2, 0 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( maxhu1, 0 ).extract( 0, 16 ).getSignedValue() == 0xFFC1 );
  ufAssert( dval( maxhu1, 0 ).extract( 16, 16 ).getSignedValue() == 0xFF85 );
  ufAssert( uval( maxhu1, 1 ).extract( 0, 16 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( maxhu1, 1 ).extract( 16, 16 ).isBinaryInteger() );
  ufAssert( uval( maxhu1, 2 ).extract( 0, 16 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( maxhu1, 2 ).extract( 16, 16 ).isBinaryInteger() );

  ufAssert( dval( min1, 0 ).getSignedValue() == -123 );
  ufAssert( dval( min2, 0 ).getSignedValue() == -123 );
  ufAssert( dval( min3, 0 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( minu1, 0 ).getSignedValue() == 64 );
  ufAssert( dval( minu2, 0 ).getSignedValue() == 64 );
  ufAssert( dval( minu3, 0 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( minb1, 0 ).extract( 0, 8 ).getSignedValue() == -63 );
  ufAssert( dval( minb1, 0 ).extract( 8, 8 ).getSignedValue() == -123 );
  ufAssert( dval( minb1, 0 ).extract( 16, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( minb1, 0 ).extract( 24, 8 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( minbu1, 0 ).extract( 0, 8 ).getSignedValue() == 64 );
  ufAssert( dval( minbu1, 0 ).extract( 8, 8 ).getSignedValue() == 64 );
  ufAssert( dval( minbu1, 0 ).extract( 16, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( minbu1, 0 ).extract( 24, 8 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( minh1, 0 ).extract( 0, 16 ).getSignedValue() == -63 );
  ufAssert( dval( minh1, 0 ).extract( 16, 16 ).getSignedValue() == -123 );
  ufAssert( dval( minh2, 0 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( minhu1, 0 ).extract( 0, 16 ).getSignedValue() == 64 );
  ufAssert( dval( minhu1, 0 ).extract( 16, 16 ).getSignedValue() == 64 );

  return( 0 );
}
