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
  @file rvconst12signed.cc
  @brief This file implements signed 12 bits-wide immediate parameters.

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
  Default constructor for signed const12 parameters.

  The constructor ensures that i lies in the range of values that can be
  represented with 12 bits, assuming two's-complement as underlying data format.
*/
RV_Const12_Signed::RV_Const12_Signed( signed long long __i ) :
  WIR_SignedImmediateParameter<RV_Const12_Signed> { __i, 12 }
{
  DSTART( "RV_Const12_Signed::RV_Const12_Signed(long long int)" );
};


/*
  Copy constructor.
*/
RV_Const12_Signed::RV_Const12_Signed( const RV_Const12_Signed &__o ) :
  WIR_SignedImmediateParameter<RV_Const12_Signed> { __o }
{
  DSTART( "RV_Const12_Signed::RV_Const12_Signed(const RV_Const12_Signed&)" );
};


/*
  Move constructor.
*/
RV_Const12_Signed::RV_Const12_Signed( RV_Const12_Signed &&__o ) :
  WIR_SignedImmediateParameter<RV_Const12_Signed> { move( __o ) }
{
  DSTART( "RV_Const12_Signed::RV_Const12_Signed(RV_Const12_Signed&&)" );
};


/*
  Destructor.
*/
RV_Const12_Signed::~RV_Const12_Signed( void )
{
  DSTART( "virtual RV_Const12_Signed::~RV_Const12_Signed()" );
};


/*
  Copy-assignment operator.
*/
RV_Const12_Signed & RV_Const12_Signed::operator = ( const RV_Const12_Signed &__o )
{
  DSTART(
    "RV_Const12_Signed& RV_Const12_Signed::operator=(const"
    " RV_Const12_Signed&)" );

  WIR_SignedImmediateParameter<RV_Const12_Signed>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
RV_Const12_Signed & RV_Const12_Signed::operator = ( RV_Const12_Signed &&__o )
{
  DSTART(
    "RV_Const12_Signed& RV_Const12_Signed::operator=(RV_Const12_Signed&&)" );

  WIR_SignedImmediateParameter<RV_Const12_Signed>::operator = ( move( __o ) );

  return( *this );
};

}       // namespace WIR
