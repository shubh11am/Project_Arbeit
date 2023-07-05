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
  @file rv32im.cc
  @brief This file implements the specific interface of the RISC-V RV32IM Base
         Integer instruction set plus the M Standard Extension for Integer
         Multiplication and Division, version 2.0.

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
#include <arch/riscv/rv32im.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for RV32IM processor architectures.
*/
RV32IM::RV32IM( void ) :
  RV32I {}
{
  DSTART( "RV32IM::RV32IM()" );

  // Specify the processor architecture modeled by this class.
  setISAName( "RV32IM" );
};


/*
  Copy constructor.
*/
RV32IM::RV32IM( const RV32IM &__o ) :
  RV32I { __o }
{
  DSTART( "RV32IM::RV32IM(const RV32IM&)" );
};


/*
  Move constructor.
*/
RV32IM::RV32IM( RV32IM &&__o ) :
  RV32I { move( __o ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
RV32IM::~RV32IM( void )
{
  DSTART( "virtual RV32IM::~RV32IM()" );
};


/*
  Copy-assignment operator.
*/
RV32IM & RV32IM::operator = ( const RV32IM &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  RV32I::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
RV32IM & RV32IM::operator = ( RV32IM &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  RV32I::operator = ( move( __o ) );

  return( *this );
};


/*
  init performs some global initialization tasks for RV32IM processor
  architectures.

  This includes setting up the assignment of valid operation formats to RV32IM
  opcodes.

  init shall be called globally by WIR_Init(). It shall only perform tasks that
  cannot be expressed as initializations of static class members (since the
  order of static initialization is unspecified in C++) and that thus require
  execution by active code.
*/
void RV32IM::init( void )
{
  DSTART( "static void RV32IM::init()" );

  //
  // RV32IM opcode to operation format mapping.
  //

  WIR_Registry::registerOpCode( OpCode::DIV, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::DIVU, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::MUL, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::MULH, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::MULHSU, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::MULHU, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::REM, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::REMU, OperationFormat::RRR_1 );

  //
  // Register this current processor model.
  //

  WIR_Registry::registerProcessor( RV32IM() );
};


//
// Private class methods
//

/*
  clone creates a copy of an RV32IM processor.
*/
WIR_BaseProcessor *RV32IM::clone( void ) const
{
  DSTART( "virtual WIR_BaseProcessor* RV32IM::clone() const" );

  return( new RV32IM( *this ) );
};

}       // namespace WIR
