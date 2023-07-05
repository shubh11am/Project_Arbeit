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
  @file armconst5satpos.cc
  @brief This file provides the interface of unsigned 5 bits-wide immediate
         parameters from the interval [1, 16] used for saturation positions.
  @brief This file implements unsigned 5 bits-wide immediate parameters from the
         interval [1, 16] used for saturation positions.,

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
#include <arch/arm/armv6.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for unsigned const5 parameters.

  The constructor ensures that i lies in the range [1, 16].
*/
ARM_Const5_SatPos::ARM_Const5_SatPos( unsigned long long __i ) :
  WIR_UnsignedImmediateParameter<ARM_Const5_SatPos> { __i, 5 }
{
  DSTART( "ARM_Const5_SatPos::ARM_Const5_SatPos(long long unsigned int)" );

  ufAssert( ( __i > 0 ) && ( __i < 17 ) );
};


/*
  Copy constructor.
*/
ARM_Const5_SatPos::ARM_Const5_SatPos( const ARM_Const5_SatPos &__o ) :
  WIR_UnsignedImmediateParameter<ARM_Const5_SatPos> { __o }
{
  DSTART( "ARM_Const5_SatPos::ARM_Const5_SatPos(const ARM_Const5_SatPos&)" );
};


/*
  Move constructor.
*/
ARM_Const5_SatPos::ARM_Const5_SatPos( ARM_Const5_SatPos &&__o ) :
  WIR_UnsignedImmediateParameter<ARM_Const5_SatPos> { move( __o ) }
{
  DSTART( "ARM_Const5_SatPos::ARM_Const5_SatPos(ARM_Const5_SatPos&&)" );
};


/*
  Destructor.
*/
ARM_Const5_SatPos::~ARM_Const5_SatPos( void )
{
  DSTART( "virtual ARM_Const5_SatPos::~ARM_Const5_SatPos()" );
};


/*
  Copy-assignment operator.
*/
ARM_Const5_SatPos & ARM_Const5_SatPos::operator = ( const ARM_Const5_SatPos &__o )
{
  DSTART(
    "ARM_Const5_SatPos& ARM_Const5_SatPos::operator=(const ARM_Const5_SatPos&)" );

  WIR_UnsignedImmediateParameter<ARM_Const5_SatPos>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
ARM_Const5_SatPos & ARM_Const5_SatPos::operator = ( ARM_Const5_SatPos &&__o )
{
  DSTART(
    "ARM_Const5_SatPos& ARM_Const5_SatPos::operator=(ARM_Const5_SatPos&&)" );

  WIR_UnsignedImmediateParameter<ARM_Const5_SatPos>::operator = ( move( __o ) );

  return( *this );
};


/*
  setValue sets an unsigned immediate parameter's actual value.

  setValue ensures that __i lies in the range [1, 16].
*/
void ARM_Const5_SatPos::setValue( unsigned long long i )
{
  DSTART( "virtual void ARM_Const5_SatPos::setValue(long long unsigned int)" );

  ufAssert( ( i > 0 ) && ( i < 17 ) );

  WIR_UnsignedImmediateParameter<ARM_Const5_SatPos>::setValue( i );
};

}       // namespace WIR
