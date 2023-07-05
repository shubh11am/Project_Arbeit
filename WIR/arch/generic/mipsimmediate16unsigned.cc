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
  @file mipsimmediate16unsigned.cc
  @brief This file implements unsigned 16-bit immediate parameters.

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
  Default constructor for unsigned 16-bit immediate parameters.

  The constructor ensures that __i lies in the range of values that can be
  represented with 16 bits (unsigned).
*/
MIPS_Immediate16_Unsigned::MIPS_Immediate16_Unsigned( unsigned long long __i ) :
  WIR_UnsignedImmediateParameter<MIPS_Immediate16_Unsigned> { __i, 16 }
{
  DSTART(
    "MIPS_Immediate16_Unsigned::MIPS_Immediate16_Unsigned(long long unsigned int)" );
};


/*
  Copy constructor.
*/
MIPS_Immediate16_Unsigned::MIPS_Immediate16_Unsigned( const MIPS_Immediate16_Unsigned &__o ) :
  WIR_UnsignedImmediateParameter<MIPS_Immediate16_Unsigned> { __o }
{
  DSTART(
    "MIPS_Immediate16_Unsigned::MIPS_Immediate16_Unsigned(const MIPS_Immediate16_Unsigned&)" );
};


/*
  Move constructor.
*/
MIPS_Immediate16_Unsigned::MIPS_Immediate16_Unsigned( MIPS_Immediate16_Unsigned &&__o ) :
  WIR_UnsignedImmediateParameter<MIPS_Immediate16_Unsigned> { move( __o ) }
{
  DSTART(
    "MIPS_Immediate16_Unsigned::MIPS_Immediate16_Unsigned(MIPS_Immediate16_Unsigned&&)" );
};


/*
  Destructor.
*/
MIPS_Immediate16_Unsigned::~MIPS_Immediate16_Unsigned( void )
{
  DSTART( "virtual MIPS_Immediate16_Unsigned::~MIPS_Immediate16_Unsigned()" );
};


/*
  Copy-assignment operator.
*/
MIPS_Immediate16_Unsigned & MIPS_Immediate16_Unsigned::operator = ( const MIPS_Immediate16_Unsigned &__o )
{
  DSTART(
    "MIPS_Immediate16_Unsigned& MIPS_Immediate16_Unsigned::operator=(const MIPS_Immediate16_Unsigned&)" );

  WIR_UnsignedImmediateParameter<MIPS_Immediate16_Unsigned>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
MIPS_Immediate16_Unsigned & MIPS_Immediate16_Unsigned::operator = ( MIPS_Immediate16_Unsigned &&__o )
{
  DSTART(
    "MIPS_Immediate16_Unsigned& MIPS_Immediate16_Unsigned::operator=(MIPS_Immediate16_Unsigned&&)" );

  WIR_UnsignedImmediateParameter<MIPS_Immediate16_Unsigned>::operator = ( move( __o ) );

  return( *this );
};

}       // namespace WIR
