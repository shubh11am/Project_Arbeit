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
  @file rv32imc.cc
  @brief This file implements the specific interface of the RISC-V RV32IMC Base
         Integer instruction set plus the M Standard Extension for Integer
         Multiplication and  and the C Standart Extension for Compressed
         Instructions, version 2.0.

  @author Simon Kopischke <Simon.Kopischke@tuhh.de>
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
#include <arch/riscv/rv32imc.h>


//
// Code Section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for RV32IMC processor architectures.
*/
RV32IMC::RV32IMC( void ) :
  RV32IC {},
  RV32IM {}
{
  DSTART( "RV32IMC::RV32IMC()" );

  // Specify the processor architecture modeled by this class.
  setISAName( "RV32IMC" );
};


/*
  Copy constructor.
*/
RV32IMC::RV32IMC( const RV32IMC &__o ) :
  RV32I { __o },
  RV32IC { __o },
  RV32IM { __o }
{
  DSTART( "RV32IMC::RV32IMC(const RV32IMC&)" );
};


/*
  Move constructor.
*/
RV32IMC::RV32IMC( RV32IMC &&__o ) :
  RV32IC { move( __o ) },
  RV32IM { move( __o ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
RV32IMC::~RV32IMC( void )
{
  DSTART( "virtual RV32IMC::~RV32IMC()" );
};


/*
  Copy-assignment operator.
*/
RV32IMC & RV32IMC::operator = ( const RV32IMC &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  RV32IC::operator = ( __o );
  RV32IM::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
RV32IMC & RV32IMC::operator = ( RV32IMC &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  RV32IC::operator = ( move( __o ) );
  RV32IM::operator = ( move( __o ) );

  return( *this );
};


/*
  init performs some global initialization tasks for RV32IMC processor
  architectures.

  This includes setting up the assignment of valid operation formats to RV32IMC
  opcodes. init, however, only registers the RV32IMC processor, as all OpCodes
  and OperationFormats have already been registerd by its base classes.
*/
void RV32IMC::init( void )
{
  DSTART( "static void RV32IMC::init()" );

  //
  // Register this current processor model.
  //

  WIR_Registry::registerProcessor( RV32IMC() );
};


//
// Private class methods
//

/*
  clone creates a copy of an RV32IMC processor.
*/
WIR_BaseProcessor *RV32IMC::clone( void ) const
{
  DSTART( "virtual WIR_BaseProcessor* RV32IMC::clone() const" );

  return ( new RV32IMC( *this ) );
};

}       // namespace WIR
