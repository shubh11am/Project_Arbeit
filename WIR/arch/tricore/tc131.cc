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

/*!
  @file tc131.cc
  @brief This file implements the specific interface of the Infineon TriCore
         V1.3.1 architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
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
#include <arch/tricore/tc131.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for TC131 processor architectures.
*/
TC131::TC131( void ) :
  TC13 {}
{
  DSTART( "TC131::TC131()" );

  // Specify the processor architecture modeled by this class.
  setISAName( "TC1.3.1" );
};


/*
  Copy constructor.
*/
TC131::TC131( const TC131 &__o ) :
  TC13 { __o }
{
  DSTART( "TC131::TC131(const TC131&)" );
};


/*
  Move constructor.
*/
TC131::TC131( TC131 &&__o ) :
  TC13 { move( __o ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
TC131::~TC131( void )
{
  DSTART( "virtual TC131::~TC131()" );
};


/*
  Copy-assignment operator.
*/
TC131 & TC131::operator = ( const TC131 &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  TC13::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
TC131 & TC131::operator = ( TC131 &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  TC13::operator = ( move( __o ) );

  return( *this );
};


/*
  init performs some global initialization tasks for TC131 processor
  architectures.

  This includes setting up the assignment of valid operation formats to TC131
  opcodes.

  init shall be called globally by WIR_Init(). It shall only perform tasks that
  cannot be expressed as initializations of static class members (since the
  order of static initialization is unspecified in C++) and that thus require
  execution by active code.
*/
void TC131::init( void )
{
  DSTART( "static void TC131::init()" );

  //
  // TC131 opcode to operation format mapping.
  //

  WIR_Registry::registerOpCode( OpCode::CACHEI_W, OperationFormat::AC10BOA );
  WIR_Registry::registerOpCode( OpCode::CACHEI_W, OperationFormat::AC10PIA );
  WIR_Registry::registerOpCode( OpCode::CACHEI_WI, OperationFormat::AC10BOA );
  WIR_Registry::registerOpCode( OpCode::CACHEI_WI, OperationFormat::AC10PIA );
  WIR_Registry::registerOpCode( OpCode::FTOIZ, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::FTOQ31Z, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::FTOUZ, OperationFormat::DD );


  //
  // Register this current processor model.
  //

  WIR_Registry::registerProcessor( TC131() );
};


//
// Private class methods
//

/*
  clone creates a copy of a TC131 processor.
*/
WIR_BaseProcessor *TC131::clone( void ) const
{
  DSTART( "virtual WIR_BaseProcessor* TC131::clone() const" );

  return( new TC131( *this ) );
};

}       // namespace WIR
