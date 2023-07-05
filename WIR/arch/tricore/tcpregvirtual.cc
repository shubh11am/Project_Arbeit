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
  @file tcpregvirtual.cc
  @brief This file implements virtual TriCore extended address registers.

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
  Default constructor for virtual extended address registers.
*/
TC_PRegV::TC_PRegV( void ) :
  WIR_VirtualRegister { TC13::RegisterType::pReg }
{
  DSTART( "TC_PRegV::TC_PRegV()" );

  // Create two virtual child address registers.
  pushBackChild( TC_ARegV() );
  pushBackChild( TC_ARegV() );
};


/*
  Copy constructor.
*/
TC_PRegV::TC_PRegV( const TC_PRegV &__o ) :
  WIR_VirtualRegister { __o }
{
  DSTART( "TC_PRegV::TC_PRegV(const TC_PRegV&)" );
};


/*
  Move constructor.
*/
TC_PRegV::TC_PRegV( TC_PRegV &&__o ) :
  WIR_VirtualRegister { move( __o ) }
{
  DSTART( "TC_PRegV::TC_PRegV(TC_PRegV&&)" );
};


/*
  Destructor.
*/
TC_PRegV::~TC_PRegV( void )
{
  DSTART( "virtual TC_PRegV::~TC_PRegV()" );
};


/*
  Copy-assignment operator.
*/
TC_PRegV & TC_PRegV::operator = ( const TC_PRegV &__o )
{
  DSTART( "TC_PRegV& TC_PRegV::operator=(const TC_PRegV&)" );

  WIR_VirtualRegister::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
TC_PRegV & TC_PRegV::operator = ( TC_PRegV &&__o )
{
  DSTART( "TC_PRegV& TC_PRegV::operator=(TC_PRegV&&)" );

  WIR_VirtualRegister::operator = ( move( __o ) );

  return( *this );
};


//
// Protected class methods
//

/*
  clone creates a copy of a virtual TriCore extended address register.
*/
TC_PRegV *TC_PRegV::clone( void ) const
{
  DSTART( "virtual TC_PRegV* TC_PRegV::clone() const" );

  return( new TC_PRegV( *this ) );
};

}       // namespace WIR
