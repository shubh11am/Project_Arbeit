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
  @file rvregvirtual.cc
  @brief This file implements virtual RISC-V RV32I registers.

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
  Default constructor for virtual registers.
*/
RV_RegV::RV_RegV( void ) :
  WIR_VirtualRegister { RV32I::RegisterType::reg }
{
  DSTART( "RV_RegV::RV_RegV()" );
};


/*
  Copy constructor.
*/
RV_RegV::RV_RegV( const RV_RegV &__o ) :
  WIR_VirtualRegister { __o }
{
  DSTART( "RV_RegV::RV_RegV(const RV_RegV&)" );
};


/*
  Move constructor.
*/
RV_RegV::RV_RegV( RV_RegV &&__o ) :
  WIR_VirtualRegister { move( __o ) }
{
  DSTART( "RV_RegV::RV_RegV(RV_RegV&&)" );
};


/*
  Destructor.
*/
RV_RegV::~RV_RegV( void )
{
  DSTART( "virtual RV_RegV::~RV_RegV()" );
};


/*
  Copy-assignment operator.
*/
RV_RegV & RV_RegV ::operator = ( const RV_RegV &__o )
{
  DSTART( "RV_RegV& RV_RegV::operator=(const RV_RegV&)" );

  WIR_VirtualRegister::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
RV_RegV & RV_RegV::operator = ( RV_RegV &&__o )
{
  DSTART( "RV_RegV& RV_RegV::operator=(RV_RegV&&)" );

  WIR_VirtualRegister::operator = ( move( __o ) );

  return( *this );
};


//
// Protected class methods
//

/*
  clone creates a copy of a virtual RV32I register.
*/
RV_RegV *RV_RegV::clone( void ) const
{
  DSTART( "virtual RV_RegV* RV_RegV::clone() const" );

  return( new RV_RegV( *this ) );
};

}       // namespace WIR
