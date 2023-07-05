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
  @file tcconst4signed.cc
  @brief This file implements signed 4 bits-wide immediate parameters.

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
  Default constructor for signed const4 parameters.

  The constructor ensures that i lies in the range of values that can be
  represented with 4 bits, assuming two's-complement as underlying data format.
*/
TC_Const4_Signed::TC_Const4_Signed( signed long long __i ) :
  WIR_SignedImmediateParameter<TC_Const4_Signed> { __i, 4 }
{
  DSTART( "TC_Const4_Signed::TC_Const4_Signed(long long int)" );
};


/*
  Copy constructor.
*/
TC_Const4_Signed::TC_Const4_Signed( const TC_Const4_Signed &__o ) :
  WIR_SignedImmediateParameter<TC_Const4_Signed> { __o }
{
  DSTART( "TC_Const4_Signed::TC_Const4_Signed(const TC_Const4_Signed&)" );
};


/*
  Move constructor.
*/
TC_Const4_Signed::TC_Const4_Signed( TC_Const4_Signed &&__o ) :
  WIR_SignedImmediateParameter<TC_Const4_Signed> { move( __o ) }
{
  DSTART( "TC_Const4_Signed::TC_Const4_Signed(TC_Const4_Signed&&)" );
};


/*
  Destructor.
*/
TC_Const4_Signed::~TC_Const4_Signed( void )
{
  DSTART( "virtual TC_Const4_Signed::~TC_Const4_Signed()" );
};


/*
  Copy-assignment operator.
*/
TC_Const4_Signed & TC_Const4_Signed::operator = ( const TC_Const4_Signed &__o )
{
  DSTART(
    "TC_Const4_Signed& TC_Const4_Signed::operator=(const TC_Const4_Signed&)" );

  WIR_SignedImmediateParameter<TC_Const4_Signed>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
TC_Const4_Signed & TC_Const4_Signed::operator = ( TC_Const4_Signed &&__o )
{
  DSTART(
    "TC_Const4_Signed& TC_Const4_Signed::operator=(TC_Const4_Signed&&)" );

  WIR_SignedImmediateParameter<TC_Const4_Signed>::operator = ( move( __o ) );

  return( *this );
};

}       // namespace WIR
