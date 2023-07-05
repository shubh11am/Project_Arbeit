/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file rv32ic.cc
  @brief This file implements the specific interface of the RISC-V RV32IC Base
         Integer instruction set plus the C Standard Extension for Compressed
         Instructions, version 2.0.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32ic.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for RV32IC processor architectures.
*/
RV32IC::RV32IC( void ) :
  RV32I {}
{
  DSTART( "RV32IC::RV32IC()" );

  // Specify the processor architecture modeled by this class.
  setISAName( "RV32IC" );
};


/*
  Copy constructor.
*/
RV32IC::RV32IC( const RV32IC &__o ) :
  RV32I { __o }
{
  DSTART( "RV32IC::RV32IC(const RV32IC&)" );
};


/*
  Move constructor.
*/
RV32IC::RV32IC( RV32IC &&__o ) :
  RV32I { move( __o ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
RV32IC::~RV32IC( void )
{
  DSTART( "virtual RV32IC::~RV32IC()" );
};


/*
  Copy-assignment operator.
*/
RV32IC & RV32IC::operator = ( const RV32IC &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  RV32I::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
RV32IC & RV32IC::operator = ( RV32IC &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  RV32IC::operator = ( move( __o ) );

  return( *this );
};


/*
  init performs some global initialization tasks for RV32IC processor
  architectures.

  This includes setting up the assignment of valid operation formats to RV32IC
  opcodes.

  init shall be called globally by WIR_Init(). It shall only perform tasks that
  cannot be expressed as initializations of static class members (since the
  order of static initialization is unspecified in C++) and that thus require
  execution by active code.
*/
void RV32IC::init( void )
{
  DSTART( "static void RV32IC::init()" );

  //
  // RV32IC operation formats.
  //

  WIR_BasicBlock b;
  RV_RegV *regV = new RV_RegV;
  RV_RegP *reg2P = new RV_RegP( "2" );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SL_1,
    { new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SNULL_1, {} );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SR_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SRC5_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::defuse ),
      new RV_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SRC5R_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new RV_Const5_Unsigned( 0 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SRC5R_2,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new RV_Const5_Unsigned( 0 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SRC6_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new RV_Const6_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SRC6_2,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new RV_Const6_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SRC6_3,
    { new WIR_RegisterParameter( *regV, WIR_Usage::defuse ),
      new RV_Const6_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SRC6_4,
    { new WIR_RegisterParameter( *reg2P, WIR_Usage::defuse ),
      new RV_Const6_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SRC6R_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new RV_Const6_Unsigned( 0 ),
      new WIR_RegisterParameter( *reg2P, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SRC6R_2,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new RV_Const6_Unsigned( 0 ),
      new WIR_RegisterParameter( *reg2P, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SRL_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SRR_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SRR_2,
    { new WIR_RegisterParameter( *regV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SRRC8_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *reg2P, WIR_Usage::use ),
      new RV_Const8_Unsigned( 0 ) } );


  //
  // RV32IC opcode to operation format mapping.
  //

  WIR_Registry::registerOpCode( OpCode::CADD, OperationFormat::SRR_2 );
  WIR_Registry::registerOpCode( OpCode::CADDI, OperationFormat::SRC6_3 );
  WIR_Registry::registerOpCode( OpCode::CADDI16SP, OperationFormat::SRC6_4 );
  WIR_Registry::registerOpCode( OpCode::CADDI4SPN, OperationFormat::SRRC8_1 );
  WIR_Registry::registerOpCode( OpCode::CAND, OperationFormat::SRR_2 );
  WIR_Registry::registerOpCode( OpCode::CANDI, OperationFormat::SRC6_3 );
  WIR_Registry::registerOpCode( OpCode::CBEQZ, OperationFormat::SRL_1 );
  WIR_Registry::registerOpCode( OpCode::CBNEZ, OperationFormat::SRL_1 );
  WIR_Registry::registerOpCode( OpCode::CEBREAK, OperationFormat::SNULL_1 );
  WIR_Registry::registerOpCode( OpCode::CJ, OperationFormat::SL_1 );
  WIR_Registry::registerOpCode( OpCode::CJAL, OperationFormat::SL_1 );
  WIR_Registry::registerOpCode( OpCode::CJALR, OperationFormat::SR_1 );
  WIR_Registry::registerOpCode( OpCode::CJR, OperationFormat::SR_1 );
  WIR_Registry::registerOpCode( OpCode::CLI, OperationFormat::SRC6_1 );
  WIR_Registry::registerOpCode( OpCode::CLUI, OperationFormat::SRC6_2 );
  WIR_Registry::registerOpCode( OpCode::CLW, OperationFormat::SRC5R_1 );
  WIR_Registry::registerOpCode( OpCode::CLWSP, OperationFormat::SRC6R_1 );
  WIR_Registry::registerOpCode( OpCode::CMV, OperationFormat::SRR_1 );
  WIR_Registry::registerOpCode( OpCode::CNOP, OperationFormat::SNULL_1);
  WIR_Registry::registerOpCode( OpCode::COR, OperationFormat::SRR_2 );
  WIR_Registry::registerOpCode( OpCode::CSLLI, OperationFormat::SRC5_1);
  WIR_Registry::registerOpCode( OpCode::CSRAI, OperationFormat::SRC5_1 );
  WIR_Registry::registerOpCode( OpCode::CSRLI, OperationFormat::SRC5_1 );
  WIR_Registry::registerOpCode( OpCode::CSUB, OperationFormat::SRR_2 );
  WIR_Registry::registerOpCode( OpCode::CSW, OperationFormat::SRC5R_2 );
  WIR_Registry::registerOpCode( OpCode::CSWSP, OperationFormat::SRC6R_2 );
  WIR_Registry::registerOpCode( OpCode::CXOR, OperationFormat::SRR_2 );


  //
  // Register this current processor model.
  //

  WIR_Registry::registerProcessor( RV32IC() );
};


//
// Private class methods
//

/*
  clone creates a copy of an RV32IC processor.
*/
WIR_BaseProcessor *RV32IC::clone( void ) const
{
  DSTART( "virtual WIR_BaseProcessor* RV32IC::clone() const" );

  return( new RV32IC( *this ) );
};

}       // namespace WIR
