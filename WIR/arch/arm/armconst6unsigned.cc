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
  @file armconst6unsigned.cc
  @brief This file implements unsigned 6 bits-wide immediate parameters.

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
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/arm/armv4t.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for unsigned const6 parameters.

  The constructor ensures that __i lies in the range of values that can be
  represented with 6 bits (unsigned).
*/
ARM_Const6_Unsigned::ARM_Const6_Unsigned( unsigned long long __i ) :
  WIR_UnsignedImmediateParameter<ARM_Const6_Unsigned> { __i, 6 }
{
  DSTART( "ARM_Const6_Unsigned::ARM_Const6_Unsigned(long long unsigned int)" );

  ufAssertT(
    __i % 2 == 0,
    "Only values between 0 and 62 that can be divided by 2 are allowed." );
};


/*
  Copy constructor.
*/
ARM_Const6_Unsigned::ARM_Const6_Unsigned( const ARM_Const6_Unsigned &__o ) :
  WIR_UnsignedImmediateParameter<ARM_Const6_Unsigned> { __o }
{
  DSTART(
    "ARM_Const6_Unsigned::ARM_Const6_Unsigned(const ARM_Const6_Unsigned&)" );
};


/*
  Move constructor.
*/
ARM_Const6_Unsigned::ARM_Const6_Unsigned( ARM_Const6_Unsigned &&__o ) :
  WIR_UnsignedImmediateParameter<ARM_Const6_Unsigned> { move( __o ) }
{
  DSTART( "ARM_Const6_Unsigned::ARM_Const6_Unsigned(ARM_Const6_Unsigned&&)" );
};


/*
  Destructor.
*/
ARM_Const6_Unsigned::~ARM_Const6_Unsigned( void )
{
  DSTART( "virtual ARM_Const6_Unsigned::~ARM_Const6_Unsigned()" );
};


/*
  Copy-assignment operator.
*/
ARM_Const6_Unsigned & ARM_Const6_Unsigned::operator = ( const ARM_Const6_Unsigned &__o )
{
  DSTART(
    "ARM_Const6_Unsigned& ARM_Const6_Unsigned::operator=(const ARM_Const6_Unsigned&)" );

  WIR_UnsignedImmediateParameter<ARM_Const6_Unsigned>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
ARM_Const6_Unsigned & ARM_Const6_Unsigned::operator = ( ARM_Const6_Unsigned &&__o )
{
  DSTART(
    "ARM_Const6_Unsigned& ARM_Const6_Unsigned::operator=(ARM_Const6_Unsigned&&)" );

  WIR_UnsignedImmediateParameter<ARM_Const6_Unsigned>::operator = ( move( __o ) );

  return( *this );
};


/*
  setValue sets an unsigned immediate parameter's actual value.

  setValue ensures that i lies in the range of values that can be represented
  with 6 bits (unsigned).
*/
void ARM_Const6_Unsigned::setValue( unsigned long long i )
{
  DSTART(
    "virtual void ARM_Const6_Unsigned::setValue(long long unsigned int)" );

  ufAssertT(
    i % 2 == 0,
    "Only values between 0 and 62 that can be divided by 2 are allowed." );

  WIR_UnsignedImmediateParameter<ARM_Const6_Unsigned>::setValue( i );
};

}       // namespace WIR
