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
  @file mipsimmediate16signed.cc
  @brief This file implements signed 16-bit immediate parameters.

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
#include <arch/generic/mips.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for signed 16-bit immediate parameters.

  The constructor ensures that __i lies in the range of values that can be
  represented with 16 bits, assuming two's-complement as underlying data format.
*/
MIPS_Immediate16_Signed::MIPS_Immediate16_Signed( signed long long __i ) :
  WIR_SignedImmediateParameter<MIPS_Immediate16_Signed> { __i, 16 }
{
  DSTART( "MIPS_Immediate16_Signed::MIPS_Immediate16_Signed(long long int)" );
};


/*
  Copy constructor.
*/
MIPS_Immediate16_Signed::MIPS_Immediate16_Signed( const MIPS_Immediate16_Signed &__o ) :
  WIR_SignedImmediateParameter<MIPS_Immediate16_Signed> { __o }
{
  DSTART(
    "MIPS_Immediate16_Signed::MIPS_Immediate16_Signed(const MIPS_Immediate16_Signed&)" );
};


/*
  Move constructor.
*/
MIPS_Immediate16_Signed::MIPS_Immediate16_Signed( MIPS_Immediate16_Signed &&__o ) :
  WIR_SignedImmediateParameter<MIPS_Immediate16_Signed> { move( __o ) }
{
  DSTART(
    "MIPS_Immediate16_Signed::MIPS_Immediate16_Signed(MIPS_Immediate16_Signed&&)" );
};


/*
  Destructor.
*/
MIPS_Immediate16_Signed::~MIPS_Immediate16_Signed( void )
{
  DSTART( "virtual MIPS_Immediate16_Signed::~MIPS_Immediate16_Signed()" );
};


/*
  Copy-assignment operator.
*/
MIPS_Immediate16_Signed & MIPS_Immediate16_Signed::operator = ( const MIPS_Immediate16_Signed &__o )
{
  DSTART(
    "MIPS_Immediate16_Signed& MIPS_Immediate16_Signed::operator=(const MIPS_Immediate16_Signed&)" );

  WIR_SignedImmediateParameter<MIPS_Immediate16_Signed>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
MIPS_Immediate16_Signed & MIPS_Immediate16_Signed::operator = ( MIPS_Immediate16_Signed &&__o )
{
  DSTART(
    "MIPS_Immediate16_Signed& MIPS_Immediate16_Signed::operator=(MIPS_Immediate16_Signed&&)" );

  WIR_SignedImmediateParameter<MIPS_Immediate16_Signed>::operator = ( move( __o ) );

  return( *this );
};

}       // namespace WIR
