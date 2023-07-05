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
  @file armloregvirtual.cc
  @brief This file implements low virtual ARM registers.

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
#include <arch/arm/armbase.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for low virtual registers.
*/
ARM_LoRegV::ARM_LoRegV( void ) :
  WIR_VirtualRegister { ARM_Base::RegisterType::lo }
{
  DSTART( "ARM_LoRegV::ARM_LoRegV()" );
};


/*
  Copy constructor.
*/
ARM_LoRegV::ARM_LoRegV( const ARM_LoRegV &__o ) :
  WIR_VirtualRegister { __o }
{
  DSTART( "ARM_LoRegV::ARM_LoRegV(const ARM_LoRegV&)" );
};


/*
  Move constructor.
*/
ARM_LoRegV::ARM_LoRegV( ARM_LoRegV &&__o ) :
  WIR_VirtualRegister { move( __o ) }
{
  DSTART( "ARM_LoRegV::ARM_LoRegV(ARM_LoRegV&&)" );
};


/*
  Destructor.
*/
ARM_LoRegV::~ARM_LoRegV( void )
{
  DSTART( "virtual ARM_LoRegV::~ARM_LoRegV()" );
};


/*
  Copy-assignment operator.
*/
ARM_LoRegV & ARM_LoRegV::operator = ( const ARM_LoRegV &__o )
{
  DSTART( "ARM_LoRegV& ARM_LoRegV::operator=(const ARM_LoRegV&)" );

  WIR_VirtualRegister::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
ARM_LoRegV & ARM_LoRegV::operator = ( ARM_LoRegV &&__o )
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
  clone creates a copy of a low virtual ARM register.
*/
ARM_LoRegV *ARM_LoRegV::clone( void ) const
{
  DSTART( "virtual ARM_LoRegV* ARM_LoRegV::clone() const" );

  return( new ARM_LoRegV( *this ) );
};

}       // namespace WIR
