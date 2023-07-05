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
  @file armconst10coprocessoroffset.cc
  @brief This file implements unsigned 10 bits-wide immediate parameters for
         coprocessor load/store offsets.

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
  Default constructor for unsigned const10 offsets.

  The constructor ensures that __i lies in the range of values that can be
  represented as coprocessor load/store offsets.
*/
ARM_Const10_CoprocessorOffset::ARM_Const10_CoprocessorOffset( unsigned long long __i ) :
  WIR_UnsignedImmediateParameter<ARM_Const10_CoprocessorOffset> { __i, 10 }
{
  DSTART(
    "ARM_Const10_CoprocessorOffset::ARM_Const10_CoprocessorOffset(long long unsigned int)" );

  ufAssertT(
    __i % 4 == 0,
    "Only values between 0 and 1020 that can be divided by 4 are allowed as " <<
    "coprocessor load/store offsets." );
};


/*
  Copy constructor.
*/
ARM_Const10_CoprocessorOffset::ARM_Const10_CoprocessorOffset( const ARM_Const10_CoprocessorOffset &__o ) :
  WIR_UnsignedImmediateParameter<ARM_Const10_CoprocessorOffset> { __o }
{
  DSTART(
    "ARM_Const10_CoprocessorOffset::ARM_Const10_CoprocessorOffset(const ARM_Const10_CoprocessorOffset&)" );
};


/*
  Move constructor.
*/
ARM_Const10_CoprocessorOffset::ARM_Const10_CoprocessorOffset( ARM_Const10_CoprocessorOffset &&__o ) :
  WIR_UnsignedImmediateParameter<ARM_Const10_CoprocessorOffset> { move( __o ) }
{
  DSTART(
    "ARM_Const10_CoprocessorOffset::ARM_Const10_CoprocessorOffset(ARM_Const10_CoprocessorOffset&&)" );
};


/*
  Destructor.
*/
ARM_Const10_CoprocessorOffset::~ARM_Const10_CoprocessorOffset( void )
{
  DSTART(
    "virtual ARM_Const10_CoprocessorOffset::~ARM_Const10_CoprocessorOffset()" );
};


/*
  Copy-assignment operator.
*/
ARM_Const10_CoprocessorOffset & ARM_Const10_CoprocessorOffset::operator = ( const ARM_Const10_CoprocessorOffset &__o )
{
  DSTART(
    "ARM_Const10_CoprocessorOffset& ARM_Const10_CoprocessorOffset::operator=(const ARM_Const10_CoprocessorOffset&)" );

  WIR_UnsignedImmediateParameter<ARM_Const10_CoprocessorOffset>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
ARM_Const10_CoprocessorOffset & ARM_Const10_CoprocessorOffset::operator = ( ARM_Const10_CoprocessorOffset &&__o )
{
  DSTART(
    "ARM_Const10_CoprocessorOffset& ARM_Const10_CoprocessorOffset::operator=(ARM_Const10_CoprocessorOffset&&)" );

  WIR_UnsignedImmediateParameter<ARM_Const10_CoprocessorOffset>::operator = ( move( __o ) );

  return( *this );
};


/*
  setValue sets an unsigned immediate parameter's actual value.

  setValue ensures that i lies in the range of values that can be represented as
  coprocessor load/store offsets.
*/
void ARM_Const10_CoprocessorOffset::setValue( unsigned long long i )
{
  DSTART(
    "virtual void ARM_Const10_CoprocessorOffset::setValue(long long unsigned int)" );

  ufAssertT(
    i % 4 == 0,
    "Only values between 0 and 1020 that can be divided by 4 are allowed as " <<
    "coprocessor load/store offsets." );

  WIR_UnsignedImmediateParameter<ARM_Const10_CoprocessorOffset>::setValue( i );
};

}       // namespace WIR
