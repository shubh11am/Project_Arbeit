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
  @file armv5tepregvirtual.cc
  @brief This file implements virtual ARMv5TE register pairs.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/arm/armv5te.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for virtual register pairs.
*/
ARMv5TE_PRegV::ARMv5TE_PRegV( void ) :
  WIR_VirtualRegister { ARMv5TE::RegisterType::pReg }
{
  DSTART( "ARMv5TE_PRegV::ARMv5TE_PRegV()" );

  // Create two virtual child registers.
  pushBackChild( ARM_RegV() );
  pushBackChild( ARM_RegV() );
};


/*
  Copy constructor.
*/
ARMv5TE_PRegV::ARMv5TE_PRegV( const ARMv5TE_PRegV &__o ) :
  WIR_VirtualRegister { __o }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Move constructor.
*/
ARMv5TE_PRegV::ARMv5TE_PRegV( ARMv5TE_PRegV &&__o ) :
  WIR_VirtualRegister { move( __o ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
ARMv5TE_PRegV::~ARMv5TE_PRegV( void )
{
  DSTART( "virtual ARMv5TE_PRegV::~ARMv5TE_PRegV()" );
};


/*
  Copy-assignment operator.
*/
ARMv5TE_PRegV & ARMv5TE_PRegV::operator = ( const ARMv5TE_PRegV &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_VirtualRegister::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
ARMv5TE_PRegV & ARMv5TE_PRegV::operator = ( ARMv5TE_PRegV &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_VirtualRegister::operator = ( move( __o ) );

  return( *this );
};


//
// Protected class methods
//

/*
  clone creates a copy of a virtual ARMv5TE register pair.
*/
ARMv5TE_PRegV *ARMv5TE_PRegV::clone( void ) const
{
  DSTART( "virtual ARMv5TE_PRegV* ARMv5TE_PRegV::clone() const" );

  return( new ARMv5TE_PRegV( *this ) );
};

}       // namespace WIR
