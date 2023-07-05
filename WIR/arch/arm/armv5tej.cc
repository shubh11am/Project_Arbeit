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
  @file armv5tej.cc
  @brief This file implements the specific interface of the ARMv5TEJ instruction
         set architecture.

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
#include <arch/arm/armv5tej.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for ARMv5TEJ-based processor architectures.
*/
ARMv5TEJ::ARMv5TEJ( void ) :
  ARMv5TE {}
{
  DSTART( "ARMv5TEJ::ARMv5TEJ()" );

  // Specify the processor architecture modeled by this class.
  setISAName( "ARMv5TEJ" );
};


/*
  Copy constructor.
*/
ARMv5TEJ::ARMv5TEJ( const ARMv5TEJ &__o ) :
  ARMv5TE { __o }
{
  DSTART( "ARMv5TEJ::ARMv5TEJ(const ARMv5TEJ&)" );
};


/*
  Move constructor.
*/
ARMv5TEJ::ARMv5TEJ( ARMv5TEJ &&__o ) :
  ARMv5TE { move( __o ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
ARMv5TEJ::~ARMv5TEJ( void )
{
  DSTART( "virtual ARMv5TEJ::~ARMv5TEJ()" );
};


/*
  Copy-assignment operator.
*/
ARMv5TEJ & ARMv5TEJ::operator = ( const ARMv5TEJ &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ARMv5TE::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
ARMv5TEJ & ARMv5TEJ::operator = ( ARMv5TEJ &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ARMv5TE::operator = ( move( __o ) );

  return( *this );
};


/*
  init performs some global initialization tasks for ARMv5TEJ-based processor
  architectures.

  This includes setting up the ARMv5TEJ machine operation formats and the
  assignment of valid operation formats to ARMv5TEJ opcodes.

  init shall be called globally by WIR_Init(). It shall only perform tasks that
  cannot be expressed as initializations of static class members (since the
  order of static initialization is unspecified in C++) and that thus require
  execution by active code.
*/
void ARMv5TEJ::init( void )
{
  DSTART( "static void ARMv5TEJ::init()" );


  //
  // ARMv5TEJ opcode to operation format mapping.
  //

  WIR_Registry::registerOpCode( OpCode::BXJ, OperationFormat::CR_3 );


  //
  // Register this current processor model.
  //

  WIR_Registry::registerProcessor( ARMv5TEJ() );
};


//
// Private class methods
//

/*
  clone creates a copy of a ARMv5TEJ processor.
*/
WIR_BaseProcessor *ARMv5TEJ::clone( void ) const
{
  DSTART( "virtual WIR_BaseProcessor* ARMv5TEJ::clone() const" );

  return( new ARMv5TEJ( *this ) );
};

}       // namespace WIR
