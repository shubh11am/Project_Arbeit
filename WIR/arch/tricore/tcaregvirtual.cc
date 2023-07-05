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
  @file tcaregvirtual.cc
  @brief This file implements virtual TriCore address registers.

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
  Default constructor for virtual address registers.
*/
TC_ARegV::TC_ARegV( void ) :
  WIR_VirtualRegister { TC13::RegisterType::aReg }
{
  DSTART( "TC_ARegV::TC_ARegV()" );
};


/*
  Copy constructor.
*/
TC_ARegV::TC_ARegV( const TC_ARegV &__o ) :
  WIR_VirtualRegister { __o }
{
  DSTART( "TC_ARegV::TC_ARegV(const TC_ARegV&)" );
};


/*
  Move constructor.
*/
TC_ARegV::TC_ARegV( TC_ARegV &&__o ) :
  WIR_VirtualRegister { move( __o ) }
{
  DSTART( "TC_ARegV::TC_ARegV(TC_ARegV&&)" );
};


/*
  Destructor.
*/
TC_ARegV::~TC_ARegV( void )
{
  DSTART( "virtual TC_ARegV::~TC_ARegV()" );
};


/*
  Copy-assignment operator.
*/
TC_ARegV & TC_ARegV ::operator = ( const TC_ARegV &__o )
{
  DSTART( "TC_ARegV& TC_ARegV::operator=(const TC_ARegV&)" );

  WIR_VirtualRegister::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
TC_ARegV & TC_ARegV::operator = ( TC_ARegV &&__o )
{
  DSTART( "TC_ARegV& TC_ARegV::operator=(TC_ARegV&&)" );

  WIR_VirtualRegister::operator = ( move( __o ) );

  return( *this );
};


//
// Protected class methods
//

/*
  clone creates a copy of a virtual TriCore address register.
*/
TC_ARegV *TC_ARegV::clone( void ) const
{
  DSTART( "virtual TC_ARegV* TC_ARegV::clone() const" );

  return( new TC_ARegV( *this ) );
};

}       // namespace WIR
