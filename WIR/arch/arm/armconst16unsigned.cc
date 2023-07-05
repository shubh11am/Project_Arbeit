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
  @file armconst16unsigned.cc
  @brief This file implements unsigned 16 bits-wide immediate parameters.

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
#include <arch/arm/armv5t.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for unsigned const16 parameters.

  The constructor ensures that i lies in the range of values that can be
  represented with 16 bits (unsigned).
*/
ARM_Const16_Unsigned::ARM_Const16_Unsigned( unsigned long long __i ) :
  WIR_UnsignedImmediateParameter<ARM_Const16_Unsigned> { __i, 16 }
{
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Copy constructor.
*/
ARM_Const16_Unsigned::ARM_Const16_Unsigned( const ARM_Const16_Unsigned &__o ) :
  WIR_UnsignedImmediateParameter<ARM_Const16_Unsigned> { __o }
{
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Move constructor.
*/
ARM_Const16_Unsigned::ARM_Const16_Unsigned( ARM_Const16_Unsigned &&__o ) :
  WIR_UnsignedImmediateParameter<ARM_Const16_Unsigned> { move( __o ) }
{
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
ARM_Const16_Unsigned::~ARM_Const16_Unsigned( void )
{
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Copy-assignment operator.
*/
ARM_Const16_Unsigned & ARM_Const16_Unsigned::operator = ( const ARM_Const16_Unsigned &__o )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_UnsignedImmediateParameter<ARM_Const16_Unsigned>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
ARM_Const16_Unsigned & ARM_Const16_Unsigned::operator = ( ARM_Const16_Unsigned &&__o )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_UnsignedImmediateParameter<ARM_Const16_Unsigned>::operator = (
    move( __o ) );

  return( *this );
};

}       // namespace WIR
