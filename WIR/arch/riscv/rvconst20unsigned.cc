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
  @file rvconst20unsigned.cc
  @brief This file implements unsigned 20 bits-wide immediate parameters.

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
#include <arch/riscv/rv32i.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for unsigned const20 parameters.

  The constructor ensures that i lies in the range of values that can be
  represented with 20 bits (unsigned).
*/
RV_Const20_Unsigned::RV_Const20_Unsigned( unsigned long long __i ) :
  WIR_UnsignedImmediateParameter<RV_Const20_Unsigned> { __i, 20 }
{
  DSTART( "RV_Const20_Unsigned::RV_Const20_Unsigned(long long unsigned int)" );
};


/*
  Copy constructor.
*/
RV_Const20_Unsigned::RV_Const20_Unsigned( const RV_Const20_Unsigned &__o ) :
  WIR_UnsignedImmediateParameter<RV_Const20_Unsigned> { __o }
{
  DSTART(
    "RV_Const20_Unsigned::RV_Const20_Unsigned(const RV_Const20_Unsigned&)" );
};


/*
  Move constructor.
*/
RV_Const20_Unsigned::RV_Const20_Unsigned( RV_Const20_Unsigned &&__o ) :
  WIR_UnsignedImmediateParameter<RV_Const20_Unsigned> { move( __o ) }
{
  DSTART( "RV_Const20_Unsigned::RV_Const20_Unsigned(RV_Const20_Unsigned&&)" );
};


/*
  Destructor.
*/
RV_Const20_Unsigned::~RV_Const20_Unsigned( void )
{
  DSTART( "virtual RV_Const20_Unsigned::~RV_Const20_Unsigned()" );
};


/*
  Copy-assignment operator.
*/
RV_Const20_Unsigned & RV_Const20_Unsigned::operator = ( const RV_Const20_Unsigned &__o )
{
  DSTART(
    "RV_Const20_Unsigned& RV_Const20_Unsigned::operator=(const"
    " RV_Const20_Unsigned&)" );

  WIR_UnsignedImmediateParameter<RV_Const20_Unsigned>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
RV_Const20_Unsigned & RV_Const20_Unsigned::operator = ( RV_Const20_Unsigned &&__o )
{
  DSTART(
    "RV_Const20_Unsigned& RV_Const20_Unsigned::operator=("
    "RV_Const20_Unsigned&&)" );

  WIR_UnsignedImmediateParameter<RV_Const20_Unsigned>::operator = ( move( __o ) );

  return( *this );
};

}       // namespace WIR
