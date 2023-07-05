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
  @file tcconst3unsigned.cc
  @brief This file implements unsigned 3 bits-wide immediate parameters.

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
  Default constructor for unsigned const3 parameters.

  The constructor ensures that i lies in the range of values that can be
  represented with 3 bits (unsigned).
*/
TC_Const3_Unsigned::TC_Const3_Unsigned( unsigned long long __i ) :
  WIR_UnsignedImmediateParameter<TC_Const3_Unsigned> { __i, 3 }
{
  DSTART( "TC_Const3_Unsigned::TC_Const3_Unsigned(long long unsigned int)" );
};


/*
  Copy constructor.
*/
TC_Const3_Unsigned::TC_Const3_Unsigned( const TC_Const3_Unsigned &__o ) :
  WIR_UnsignedImmediateParameter<TC_Const3_Unsigned> { __o }
{
  DSTART( "TC_Const3_Unsigned::TC_Const3_Unsigned(const TC_Const3_Unsigned&)" );
};


/*
  Move constructor.
*/
TC_Const3_Unsigned::TC_Const3_Unsigned( TC_Const3_Unsigned &&__o ) :
  WIR_UnsignedImmediateParameter<TC_Const3_Unsigned> { move( __o ) }
{
  DSTART( "TC_Const3_Unsigned::TC_Const3_Unsigned(TC_Const3_Unsigned&&)" );
};


/*
  Destructor.
*/
TC_Const3_Unsigned::~TC_Const3_Unsigned( void )
{
  DSTART( "virtual TC_Const3_Unsigned::~TC_Const3_Unsigned()" );
};


/*
  Copy-assignment operator.
*/
TC_Const3_Unsigned & TC_Const3_Unsigned::operator = ( const TC_Const3_Unsigned &__o )
{
  DSTART(
    "TC_Const3_Unsigned& TC_Const3_Unsigned::operator=(const TC_Const3_Unsigned&)" );

  WIR_UnsignedImmediateParameter<TC_Const3_Unsigned>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
TC_Const3_Unsigned & TC_Const3_Unsigned::operator = ( TC_Const3_Unsigned &&__o )
{
  DSTART(
    "TC_Const3_Unsigned& TC_Const3_Unsigned::operator=(TC_Const3_Unsigned&&)" );

  WIR_UnsignedImmediateParameter<TC_Const3_Unsigned>::operator = ( move( __o ) );

  return( *this );
};

}       // namespace WIR
