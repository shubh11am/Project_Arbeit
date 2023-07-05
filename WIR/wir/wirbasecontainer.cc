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
  @file wirbasecontainer.cc
  @brief This file implements basic generic %WIR containers that can freely be
         attached to the core classes of the %WIR library.

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


//
// Code section
//

namespace WIR {


using namespace std;


WIR_id_t WIR_BaseContainer::mTypeID = 0;


//
// Public class methods
//

/*
  Default constructor creating an empty container.
*/
WIR_BaseContainer::WIR_BaseContainer( void ) :
  WIR_ID_API {}
{
  DSTART( "WIR_BaseContainer::WIR_BaseContainer()" );
};


/*
  Copy constructor.
*/
WIR_BaseContainer::WIR_BaseContainer( const WIR_BaseContainer &__o ) :
  WIR_ID_API { __o }
{
  DSTART( "WIR_BaseContainer::WIR_BaseContainer(const WIR_BaseContainer&)" );
};


/*
  Move constructor.
*/
WIR_BaseContainer::WIR_BaseContainer( WIR_BaseContainer &&__o ) :
  WIR_ID_API { move( __o ) }
{
  DSTART( "WIR_BaseContainer::WIR_BaseContainer(WIR_BaseContainer&&)" );
};


/*
  Destructor.
*/
WIR_BaseContainer::~WIR_BaseContainer( void )
{
  DSTART( "virtual WIR_BaseContainer::~WIR_BaseContainer()" );
};


/*
  Copy-assignment operator.
*/
WIR_BaseContainer & WIR_BaseContainer::operator = ( const WIR_BaseContainer &__o )
{
  DSTART(
    "WIR_BaseContainer& WIR_BaseContainer::operator=(const WIR_BaseContainer&)" );

  (void) __o;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_BaseContainer & WIR_BaseContainer::operator = ( WIR_BaseContainer &&__o )
{
  DSTART(
    "WIR_BaseContainer& WIR_BaseContainer::operator=(WIR_BaseContainer&&)" );

  (void) __o;

  return( *this );
};


//
// Protected class methods
//

/*
  Default constructor registering a new container type.
*/
WIR_BaseContainer::Registrator::Registrator( WIR_id_t &id )
{
  DSTART( "WIR_BaseContainer::Registrator::Registrator(WIR_id_t&)" );

  registerNewContainerType( id );
};


/*
  touch is a dummy method that just serves to activate the initialization of
  static data members.

  Objects of class 'Registrator' are used as static initializers for class
  WIR_Container. Since WIR_Container is a templated class, instantiation of its
  static data members does not occur until these are explicitly referenced. For
  this purpose, this method is provided: WIR_Container can 'touch' its static
  data member so that it will get initialized.
*/
void WIR_BaseContainer::Registrator::touch( void )
{
  DSTART( "void WIR_BaseContainer::Registrator::touch()" );
};


//
// Private class methods
//

/*
  registerNewContainerType registers a new container type.
*/
void WIR_BaseContainer::registerNewContainerType( WIR_id_t &id )
{
  DSTART(
    "static void WIR_BaseContainer::registerNewContainerType(WIR_id_t&)" );

  id = mTypeID++;
};

}       // namespace WIR
