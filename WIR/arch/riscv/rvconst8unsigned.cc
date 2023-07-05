/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file rvconst8unsigned.cc
  @brief This file implements unsigned 8 bits-wide immediate parameters.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
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
#include <arch/riscv/rv32ic.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for unsigned const8 parameters.

  The constructor ensures that i lies in the range of values that can be
  represented with 8 bits (unsigned).
*/
RV_Const8_Unsigned::RV_Const8_Unsigned( unsigned long long __i ) :
  WIR_UnsignedImmediateParameter<RV_Const8_Unsigned> { __i, 8 }
{
  DSTART( "RV_Const8_Unsigned::RV_Const8_Unsigned(long long unsigned int)" );
};


/*
  Copy constructor.
*/
RV_Const8_Unsigned::RV_Const8_Unsigned( const RV_Const8_Unsigned &__o ) :
  WIR_UnsignedImmediateParameter<RV_Const8_Unsigned> { __o }
{
  DSTART( "RV_Const8_Unsigned::RV_Const8_Unsigned(const RV_Const8_Unsigned&)" );
};


/*
  Move constructor.
*/
RV_Const8_Unsigned::RV_Const8_Unsigned( RV_Const8_Unsigned &&__o ) :
  WIR_UnsignedImmediateParameter<RV_Const8_Unsigned> { move( __o ) }
{
  DSTART( "RV_Const5_Unsigned::RV_Const8_Unsigned(RV_Const8_Unsigned&&)" );
};


/*
  Destructor.
*/
RV_Const8_Unsigned::~RV_Const8_Unsigned( void )
{
  DSTART( "virtual RV_Const8_Unsigned::~RV_Const8_Unsigned()" );
};


/*
  Copy-assignment operator.
*/
RV_Const8_Unsigned & RV_Const8_Unsigned::operator = ( const RV_Const8_Unsigned &__o )
{
  DSTART(
    "RV_Const8_Unsigned& RV_Const8_Unsigned::operator=(const"
    " RV_Const8_Unsigned&)" );

  WIR_UnsignedImmediateParameter<RV_Const8_Unsigned>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
RV_Const8_Unsigned & RV_Const8_Unsigned::operator = ( RV_Const8_Unsigned &&__o )
{
  DSTART(
    "RV_Const8_Unsigned& RV_Const8_Unsigned::operator=(RV_Const8_Unsigned&&)" );

  WIR_UnsignedImmediateParameter<RV_Const8_Unsigned>::operator = ( move( __o ) );

  return( *this );
};

}       // namespace WIR
