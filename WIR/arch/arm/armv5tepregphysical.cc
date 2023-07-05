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
  @file armv5tepregphysical.cc
  @brief This file implements physical ARMv5TE register pairs.

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
#include <libuseful/io.h>

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
  Destructor.
*/
ARMv5TE_PRegP::~ARMv5TE_PRegP( void )
{
  DSTART( "virtual ARMv5TE_PRegP::~ARMv5TE_PRegP()" );
};


//
// Private class methods
//

/*
  Default constructor for physical register pairs.
*/
ARMv5TE_PRegP::ARMv5TE_PRegP( const std::string &__s, bool __sp ) :
  WIR_PhysicalRegister { ARMv5TE::RegisterType::pReg, __s, __sp }
{
  DSTART( "ARMv5TE_PRegP::ARMv5TE_PRegP(const string&, bool)" );
};


/*
  Copy constructor.
*/
ARMv5TE_PRegP::ARMv5TE_PRegP( const ARMv5TE_PRegP &__o ) :
  WIR_PhysicalRegister { __o }
{
  DSTART( "ARMv5TE_PRegP::ARMv5TE_PRegP(const ARMv5TE_PRegP&)" );
};


/*
  Copy-assignment operator.
*/
ARMv5TE_PRegP & ARMv5TE_PRegP::operator = ( const ARMv5TE_PRegP &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_PhysicalRegister::operator = ( __o );

  return( *this );
};


/*
  Upon creation of ARM physical register pairs, addChilds adds the two specified
  physical registers as childs.
*/
void ARMv5TE_PRegP::addChilds( const WIR_PhysicalRegister &c1,
                               const WIR_PhysicalRegister &c2 )
{
  DSTART(
    "void ARMv5TE_PRegP::addChilds(const WIR_PhysicalRegister&, const WIR_PhysicalRegister&)" );

  pushBackChild( const_cast<WIR_PhysicalRegister &>( c1 ) );
  pushBackChild( const_cast<WIR_PhysicalRegister &>( c2 ) );
};


/*
  clone creates a copy of a physical ARMv5TE register pair.
*/
ARMv5TE_PRegP *ARMv5TE_PRegP::clone( void ) const
{
  DSTART( "virtual ARMv5TE_PRegP* ARMv5TE_PRegP::clone() const" );

  return( new ARMv5TE_PRegP( *this ) );
};

}       // namespace WIR
