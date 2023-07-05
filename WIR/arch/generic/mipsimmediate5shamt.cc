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
  @file mipsimmediate5shamt.cc
  @brief This file implements 5-bit shift amount parameters.

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
  Default constructor for 5-bit shift amount parameters.

  The constructor ensures that __i lies in the range of values that can be
  represented with 5 bits (unsigned).
*/
MIPS_Immediate5_Shamt::MIPS_Immediate5_Shamt( unsigned long long __i ) :
  WIR_UnsignedImmediateParameter<MIPS_Immediate5_Shamt> { __i, 5 }
{
  DSTART(
    "MIPS_Immediate5_Shamt::MIPS_Immediate5_Shamt(long long unsigned int)" );
};


/*
  Copy constructor.
*/
MIPS_Immediate5_Shamt::MIPS_Immediate5_Shamt( const MIPS_Immediate5_Shamt &__o ) :
  WIR_UnsignedImmediateParameter<MIPS_Immediate5_Shamt> { __o }
{
  DSTART(
    "MIPS_Immediate5_Shamt::MIPS_Immediate5_Shamt(const MIPS_Immediate5_Shamt&)" );
};


/*
  Move constructor.
*/
MIPS_Immediate5_Shamt::MIPS_Immediate5_Shamt( MIPS_Immediate5_Shamt &&__o ) :
  WIR_UnsignedImmediateParameter<MIPS_Immediate5_Shamt> { move( __o ) }
{
  DSTART(
    "MIPS_Immediate5_Shamt::MIPS_Immediate5_Shamt(MIPS_Immediate5_Shamt&&)" );
};


/*
  Destructor.
*/
MIPS_Immediate5_Shamt::~MIPS_Immediate5_Shamt( void )
{
  DSTART( "virtual MIPS_Immediate5_Shamt::~MIPS_Immediate5_Shamt()" );
};


/*
  Copy-assignment operator.
*/
MIPS_Immediate5_Shamt & MIPS_Immediate5_Shamt::operator = ( const MIPS_Immediate5_Shamt &__o )
{
  DSTART(
    "MIPS_Immediate5_Shamt& MIPS_Immediate5_Shamt::operator=(const MIPS_Immediate5_Shamt&)" );

  WIR_UnsignedImmediateParameter<MIPS_Immediate5_Shamt>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
MIPS_Immediate5_Shamt & MIPS_Immediate5_Shamt::operator = ( MIPS_Immediate5_Shamt &&__o )
{
  DSTART(
    "MIPS_Immediate5_Shamt& MIPS_Immediate5_Shamt::operator=(MIPS_Immediate5_Shamt&&)" );

  WIR_UnsignedImmediateParameter<MIPS_Immediate5_Shamt>::operator = ( move( __o ) );

  return( *this );
};

}       // namespace WIR
