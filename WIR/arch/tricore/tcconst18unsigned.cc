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
  @file tcconst18unsigned.cc
  @brief This file implements unsigned 18 bits-wide immediate parameters.

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
  Default constructor for 18 bits-wide absolute address parameters.

  The constructor ensures that __i lies in the range of values that are valid
  according to the TriCore ISA.
*/
TC_Const18_Unsigned::TC_Const18_Unsigned( unsigned long long __i ) :
  WIR_UnsignedImmediateParameter<TC_Const18_Unsigned> { __i, 32 }
{
  DSTART( "TC_Const18_Unsigned::TC_Const18_Unsigned(long long unsigned int)" );

  // Extract bit positions 14:27 from __i.
  unsigned int bits1427 = ( __i >> 14 ) & 0x3FFF;
  ufAssert( bits1427 == 0 );
};


/*
  Copy constructor.
*/
TC_Const18_Unsigned::TC_Const18_Unsigned( const TC_Const18_Unsigned &__o ) :
  WIR_UnsignedImmediateParameter<TC_Const18_Unsigned> { __o }
{
  DSTART(
    "TC_Const18_Unsigned::TC_Const18_Unsigned(const TC_Const18_Unsigned&)" );
};


/*
  Move constructor.
*/
TC_Const18_Unsigned::TC_Const18_Unsigned( TC_Const18_Unsigned &&__o ) :
  WIR_UnsignedImmediateParameter<TC_Const18_Unsigned> { move( __o ) }
{
  DSTART( "TC_Const18_Unsigned::TC_Const18_Unsigned(TC_Const18_Unsigned&&)" );
};


/*
  Destructor.
*/
TC_Const18_Unsigned::~TC_Const18_Unsigned( void )
{
  DSTART( "virtual TC_Const18_Unsigned::~TC_Const18_Unsigned()" );
};


/*
  Copy-assignment operator.
*/
TC_Const18_Unsigned & TC_Const18_Unsigned::operator = ( const TC_Const18_Unsigned &__o )
{
  DSTART(
    "TC_Const18_Unsigned& TC_Const18_Unsigned::operator=(const TC_Const18_Unsigned&)" );

  WIR_UnsignedImmediateParameter<TC_Const18_Unsigned>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
TC_Const18_Unsigned & TC_Const18_Unsigned::operator = ( TC_Const18_Unsigned &&__o )
{
  DSTART(
    "TC_Const18_Unsigned& TC_Const18_Unsigned::operator=(TC_Const18_Unsigned&&)" );

  WIR_UnsignedImmediateParameter<TC_Const18_Unsigned>::operator = ( move( __o ) );

  return( *this );
};


/*
  setValue sets an unsigned immediate parameter's actual value.

  setValue ensures that i lies in the range of values that can be represented by
  the parameter's bit width.
*/
void TC_Const18_Unsigned::setValue( unsigned long long i )
{
  DSTART( "void TC_Const18_Unsigned::setValue(long long unsigned int)" );

  WIR_UnsignedImmediateParameter::setValue( i );

  // Extract bit positions 14:27 from i.
  unsigned int bits1427 = ( i >> 14 ) & 0x3FFF;
  ufAssert( bits1427 == 0 );
};

}       // namespace WIR
