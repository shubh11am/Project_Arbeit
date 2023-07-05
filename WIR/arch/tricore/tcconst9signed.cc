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
  @file tcconst9signed.cc
  @brief This file implements signed 9 bits-wide immediate parameters.

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
  Default constructor for signed const9 parameters.

  The constructor ensures that i lies in the range of values that can be
  represented with 9 bits, assuming two's-complement as underlying data format.
*/
TC_Const9_Signed::TC_Const9_Signed( signed long long __i ) :
  WIR_SignedImmediateParameter<TC_Const9_Signed> { __i, 9 }
{
  DSTART( "TC_Const9_Signed::TC_Const9_Signed(long long int)" );
};


/*
  Copy constructor.
*/
TC_Const9_Signed::TC_Const9_Signed( const TC_Const9_Signed &__o ) :
  WIR_SignedImmediateParameter<TC_Const9_Signed> { __o }
{
  DSTART( "TC_Const9_Signed::TC_Const9_Signed(const TC_Const9_Signed&)" );
};


/*
  Move constructor.
*/
TC_Const9_Signed::TC_Const9_Signed( TC_Const9_Signed &&__o ) :
  WIR_SignedImmediateParameter<TC_Const9_Signed> { move( __o ) }
{
  DSTART( "TC_Const9_Signed::TC_Const9_Signed(TC_Const9_Signed&&)" );
};


/*
  Destructor.
*/
TC_Const9_Signed::~TC_Const9_Signed( void )
{
  DSTART( "virtual TC_Const9_Signed::~TC_Const9_Signed()" );
};


/*
  Copy-assignment operator.
*/
TC_Const9_Signed & TC_Const9_Signed::operator = ( const TC_Const9_Signed &__o )
{
  DSTART(
    "TC_Const9_Signed& TC_Const9_Signed::operator=(const TC_Const9_Signed&)" );

  WIR_SignedImmediateParameter<TC_Const9_Signed>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
TC_Const9_Signed & TC_Const9_Signed::operator = ( TC_Const9_Signed &&__o )
{
  DSTART(
    "TC_Const9_Signed& TC_Const9_Signed::operator=(TC_Const9_Signed&&)" );

  WIR_SignedImmediateParameter<TC_Const9_Signed>::operator = ( move( __o ) );

  return( *this );
};

}       // namespace WIR
