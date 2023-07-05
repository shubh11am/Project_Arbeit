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
  @file tceregvirtual.cc
  @brief This file implements virtual TriCore extended data registers.

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
  Default constructor for virtual extended data registers.
*/
TC_ERegV::TC_ERegV( void ) :
  WIR_VirtualRegister { TC13::RegisterType::eReg }
{
  DSTART( "TC_ERegV::TC_ERegV()" );

  // Create two virtual child data registers.
  pushBackChild( TC_DRegV() );
  pushBackChild( TC_DRegV() );
};


/*
  Copy constructor.
*/
TC_ERegV::TC_ERegV( const TC_ERegV &__o ) :
  WIR_VirtualRegister { __o }
{
  DSTART( "TC_ERegV::TC_ERegV(const TC_ERegV&)" );
};


/*
  Move constructor.
*/
TC_ERegV::TC_ERegV( TC_ERegV &&__o ) :
  WIR_VirtualRegister { move( __o ) }
{
  DSTART( "TC_ERegV::TC_ERegV(TC_ERegV&&)" );
};


/*
  Destructor.
*/
TC_ERegV::~TC_ERegV( void )
{
  DSTART( "virtual TC_ERegV::~TC_ERegV()" );
};


/*
  Copy-assignment operator.
*/
TC_ERegV & TC_ERegV::operator = ( const TC_ERegV &__o )
{
  DSTART( "TC_ERegV& TC_ERegV::operator=(const TC_ERegV&)" );

  WIR_VirtualRegister::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
TC_ERegV & TC_ERegV::operator = ( TC_ERegV &&__o )
{
  DSTART( "TC_ERegV& TC_ERegV::operator=(TC_ERegV&&)" );

  WIR_VirtualRegister::operator = ( move( __o ) );

  return( *this );
};


//
// Protected class methods
//

/*
  clone creates a copy of a virtual TriCore extended data register.
*/
TC_ERegV *TC_ERegV::clone( void ) const
{
  DSTART( "virtual TC_ERegV* TC_ERegV::clone() const" );

  return( new TC_ERegV( *this ) );
};

}       // namespace WIR
