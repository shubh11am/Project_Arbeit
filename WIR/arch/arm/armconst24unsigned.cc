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
  @file armconst24unsigned.cc
  @brief This file implements unsigned 24 bits-wide immediate parameters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/arm/armbase.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for unsigned const24 parameters.

  The constructor ensures that i lies in the range of values that can be
  represented with 24 bits (unsigned).
*/
ARM_Const24_Unsigned::ARM_Const24_Unsigned( unsigned long long __i ) :
  WIR_UnsignedImmediateParameter<ARM_Const24_Unsigned> { __i, 24 }
{
  DSTART(
    "ARM_Const24_Unsigned::ARM_Const24_Unsigned(long long unsigned int)" );
};


/*
  Copy constructor.
*/
ARM_Const24_Unsigned::ARM_Const24_Unsigned( const ARM_Const24_Unsigned &__o ) :
  WIR_UnsignedImmediateParameter<ARM_Const24_Unsigned> { __o }
{
  DSTART(
    "ARM_Const24_Unsigned::ARM_Const24_Unsigned(const ARM_Const24_Unsigned&)" );
};


/*
  Move constructor.
*/
ARM_Const24_Unsigned::ARM_Const24_Unsigned( ARM_Const24_Unsigned &&__o ) :
  WIR_UnsignedImmediateParameter<ARM_Const24_Unsigned> { move( __o ) }
{
  DSTART(
    "ARM_Const24_Unsigned::ARM_Const24_Unsigned(ARM_Const24_Unsigned&&)" );
};


/*
  Destructor.
*/
ARM_Const24_Unsigned::~ARM_Const24_Unsigned( void )
{
  DSTART( "virtual ARM_Const24_Unsigned::~ARM_Const24_Unsigned()" );
};


/*
  Copy-assignment operator.
*/
ARM_Const24_Unsigned & ARM_Const24_Unsigned::operator = ( const ARM_Const24_Unsigned &__o )
{
  DSTART(
    "ARM_Const24_Unsigned& ARM_Const24_Unsigned::operator=(const ARM_Const24_Unsigned&)" );

  WIR_UnsignedImmediateParameter<ARM_Const24_Unsigned>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
ARM_Const24_Unsigned & ARM_Const24_Unsigned::operator = ( ARM_Const24_Unsigned &&__o )
{
  DSTART(
    "ARM_Const24_Unsigned& ARM_Const24_Unsigned::operator=(ARM_Const24_Unsigned&&)" );

  WIR_UnsignedImmediateParameter<ARM_Const24_Unsigned>::operator = (
    move( __o ) );

  return( *this );
};

}       // namespace WIR
