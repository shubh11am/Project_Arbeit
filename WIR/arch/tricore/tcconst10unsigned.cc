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
  @file tcconst10unsigned.cc
  @brief This file implements unsigned 10 bits-wide immediate parameters that
         are multiples of 4.

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
#include <arch/tricore/tc13.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for unsigned const10 parameters that are multiples of 4.

  The constructor ensures that __i lies in the range of values that can be
  represented with 10 bits (unsigned) and that the given value is a multiple of
  4.
*/
TC_Const10_Unsigned::TC_Const10_Unsigned( unsigned long long __i ) :
  WIR_UnsignedImmediateParameter<TC_Const10_Unsigned> { __i, 10 }
{
  DSTART(
    "TC_Const10_Unsigned::TC_Const10_Unsigned(long long unsigned int)" );

  ufAssert( __i % 4 == 0 );
};


/*
  Copy constructor.
*/
TC_Const10_Unsigned::TC_Const10_Unsigned( const TC_Const10_Unsigned &__o ) :
  WIR_UnsignedImmediateParameter<TC_Const10_Unsigned> { __o }
{
  DSTART(
    "TC_Const10_Unsigned::TC_Const10_Unsigned(const TC_Const10_Unsigned&)" );
};


/*
  Move constructor.
*/
TC_Const10_Unsigned::TC_Const10_Unsigned( TC_Const10_Unsigned &&__o ) :
  WIR_UnsignedImmediateParameter<TC_Const10_Unsigned> { move( __o ) }
{
  DSTART(
    "TC_Const10_Unsigned::TC_Const10_Unsigned(TC_Const10_Unsigned&&)" );
};


/*
  Destructor.
*/
TC_Const10_Unsigned::~TC_Const10_Unsigned( void )
{
  DSTART( "virtual TC_Const10_Unsigned::~TC_Const10_Unsigned()" );
};


/*
  Copy-assignment operator.
*/
TC_Const10_Unsigned & TC_Const10_Unsigned::operator = ( const TC_Const10_Unsigned &__o )
{
  DSTART(
    "TC_Const10_Unsigned& TC_Const10_Unsigned::operator=(const TC_Const10_Unsigned&)" );

  WIR_UnsignedImmediateParameter<TC_Const10_Unsigned>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
TC_Const10_Unsigned & TC_Const10_Unsigned::operator = ( TC_Const10_Unsigned &&__o )
{
  DSTART(
    "TC_Const10_Unsigned& TC_Const10_Unsigned::operator=(TC_Const10_Unsigned&&)" );

  WIR_UnsignedImmediateParameter<TC_Const10_Unsigned>::operator = ( move( __o ) );

  return( *this );
};


/*
  setValue sets an unsigned immediate parameter's actual value.

  setValue ensures that i lies in the range of values that can be represented by
  the parameter's bit width.
*/
void TC_Const10_Unsigned::setValue( unsigned long long i )
{
  DSTART( "void TC_Const10_Unsigned::setValue(long long unsigned int)" );

  WIR_UnsignedImmediateParameter::setValue( i );

  ufAssert( i % 4 == 0 );
};

}       // namespace WIR
