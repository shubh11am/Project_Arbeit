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
  @file wircontainerapi.cc
  @brief This file implements a base class for managing names for managing %WIR
         containers in derived classes.
  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <algorithm>
#include <iostream>
#include <vector>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  insertContainer attaches a new container.

  The content of c is copied into the WIR_Container_API class. If a unique
  container is inserted and there are already containers of the same type
  attached before, all the old containers are erased before.
*/
void WIR_Container_API::insertContainer( const WIR_BaseContainer &c )
{
  DSTART( "void WIR_Container_API::insertContainer(const WIR_BaseContainer&)" );

  if ( c.isUnique() )
    eraseContainers( c.getContainerType() );

  auto *p = c.clone();
  mContainerPointers[ c.getContainerType() ].insert(
    unique_ptr<WIR_BaseContainer>( p ) );
};


/*
  insertContainer attaches a new container.

  The content of c is copied into the WIR_Container_API class. If a unique
  container is inserted and there are already containers of the same type
  attached before, all the old containers are erased before.
*/
void WIR_Container_API::insertContainer( WIR_BaseContainer &&c )
{
  DSTART( "void WIR_Container_API::insertContainer(WIR_BaseContainer&&)" );

  if ( c.isUnique() )
    eraseContainers( c.getContainerType() );

  auto *p = c.clone();
  mContainerPointers[ c.getContainerType() ].insert(
    unique_ptr<WIR_BaseContainer>( p ) );
};


/*
  insertContainer attaches a new container.

  If a unique container is inserted and there are already containers of the same
  type attached before, all the old containers are erased before.

  Class WIR_Container_API takes over full control over the ownership of the
  given pointer! In particular, WIR_Container_API automatically destroys the
  object pointed to. Users of this variant of insertContainer are strongly
  discouraged of continuing to use this pointer afterwards. This variant of
  insertContainer is more efficient than the previous ones since it completely
  avoids any (polymorphic) copy operations. Thus, it should only be used if
  large amounts of containers shall be created/added highly efficiently.
*/
void WIR_Container_API::insertContainer( WIR_BaseContainer *p )
{
  DSTART( "void WIR_Container_API::insertContainer(WIR_BaseContainer*)" );

  if ( p->isUnique() )
    eraseContainers( p->getContainerType() );

  mContainerPointers[ p->getContainerType() ].insert(
    unique_ptr<WIR_BaseContainer>( p ) );
};


/*
  eraseContainers removes all containers of the specified container type.

  This destroys all removed containers.
*/
void WIR_Container_API::eraseContainers( WIR_id_t id, bool recursive )
{
  DSTART( "void WIR_Container_API::eraseContainers(WIR_id_t, bool)" );

  mContainerPointers.erase( id );

  if ( recursive ) {
    if ( auto *ptr = dynamic_cast<WIR_System *>( this ) ) {
      for ( WIR_CompilationUnit &c : ptr->getCompilationUnits() )
        c.eraseContainers( id, recursive );
    } else

    if ( auto *ptr = dynamic_cast<WIR_CompilationUnit *>( this ) ) {
      for ( WIR_Function &f : ptr->getFunctions() )
        f.eraseContainers( id, recursive );
    } else

    if ( auto *ptr = dynamic_cast<WIR_Function *>( this ) ) {
      for ( WIR_BasicBlock &b : ptr->getBasicBlocks() )
        b.eraseContainers( id, recursive );
    } else

    if ( auto *ptr = dynamic_cast<WIR_BasicBlock *>( this ) ) {
      for ( WIR_Instruction &i : ptr->getInstructions() )
        i.eraseContainers( id, recursive );
    } else

    if ( auto *ptr = dynamic_cast<WIR_Instruction *>( this ) ) {
      for ( WIR_Operation &o : ptr->getOperations() )
        o.eraseContainers( id, recursive );
    } else

    if ( auto *ptr = dynamic_cast<WIR_Operation *>( this ) ) {
      for ( WIR_Parameter &p : ptr->getParameters() )
        p.eraseContainers( id, recursive );
    }
  };
};


/*
  eraseContainer removes the specified container.

  This destroys the removed container.
*/
void WIR_Container_API::eraseContainer( WIR_BaseContainer &c )
{
  DSTART( "void WIR_Container_API::eraseContainer(WIR_BaseContainer&)" );

  if ( containsContainer( c ) ) {
    WIR_id_t t = c.getContainerType();

    auto &containers = mContainerPointers[ t ];
    auto it = containers.begin();
    while ( (*it)->getID() != c.getID() )
      ++it;
    containers.erase( it );

    if ( containers.empty() )
      mContainerPointers.erase( t );
  }
};


/*
  clearContainers removes all containers.

  This destroys all removed containers.
*/
void WIR_Container_API::clearContainers( void )
{
  DSTART( "void WIR_Container_API::clearContainers()" );

  mContainerPointers.clear();
};


/*
  getContainerTypes determines all types of containers that are currently
  attached.
*/
set<WIR_id_t> WIR_Container_API::getContainerTypes( void ) const
{
  DSTART( "set<WIR_id_t> WIR_Container_API::getContainerTypes() const" );

  set<WIR_id_t> res;

  for ( auto it = mContainerPointers.begin(); it != mContainerPointers.end();
        ++it )
    res.insert( (*it).first );

  return( res );
};


/*
  This variant of getContainers returns a set of all containers that are
  currently attached at all.

  Since the containers can be of different type, the returned set contains
  generic references of type WIR_BaseContainer.
*/
const WIR_ContainerSet WIR_Container_API::getContainers( void ) const
{
  DSTART( "const WIR_ContainerSet WIR_Container_API::getContainers() const" );

  WIR_ContainerSet res;

  for ( auto it = mContainerPointers.begin(); it != mContainerPointers.end();
        ++it )
    for ( auto it1 = (*it).second.begin(); it1 != (*it).second.end(); ++it1 )
      res.insert( *( it1->get() ) );

  return( res );
};


/*
  containsContainers returns whether some containers are attached at all.
*/
bool WIR_Container_API::containsContainers( void ) const
{
  DSTART( "bool WIR_Container_API::containsContainers() const" );

  return( !mContainerPointers.empty() );
};


/*
  containsContainers returns whether containers of the specified type are
  attached.
*/
bool WIR_Container_API::containsContainers( WIR_id_t id ) const
{
  DSTART( "bool WIR_Container_API::containsContainers(WIR_id_t) const" );

  return( mContainerPointers.count( id ) != 0 );
};


/*
  containsContainer returns whether the specified container is attached.
*/
bool WIR_Container_API::containsContainer( const WIR_BaseContainer &c ) const
{
  DSTART(
    "bool WIR_Container_API::containsContainer(const WIR_BaseContainer&) const" );

  WIR_id_t t = c.getContainerType();
  auto it = mContainerPointers.find( t );

  if ( it != mContainerPointers.end() ) {
    auto &containers = it->second;

    for ( auto &con : containers )
      if ( *con == c )
        return( true );
  }

  return( false );
};


//
// Protected class methods
//

/*
  Default constructor.
*/
WIR_Container_API::WIR_Container_API( void )
{
  DSTART( "WIR_Container_API::WIR_Container_API()" );
};


/*
  Copy constructor.
*/
WIR_Container_API::WIR_Container_API( const WIR_Container_API &__o )
{
  DSTART( "WIR_Container_API::WIR_Container_API(const WIR_Container_API&)" );

  copyContainers( __o );
};


/*
  Move constructor.
*/
WIR_Container_API::WIR_Container_API( WIR_Container_API &&__o ) :
  mContainerPointers { move( __o.mContainerPointers ) }
{
  DSTART( "WIR_Container_API::WIR_Container_API(WIR_Container_API&&)" );

  __o.mContainerPointers.clear();
};


/*
  Destructor.
*/
WIR_Container_API::~WIR_Container_API( void )
{
  DSTART( "virtual WIR_Container_API::~WIR_Container_API()" );

  mContainerPointers.clear();
};


/*
  Copy-assignment operator.
*/
WIR_Container_API & WIR_Container_API::operator = ( const WIR_Container_API &__o )
{
  DSTART(
    "WIR_Container_API& WIR_Container_API::operator=(const WIR_Container_API&)" );

  copyContainers( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_Container_API & WIR_Container_API::operator = ( WIR_Container_API &&__o )
{
  DSTART(
    "WIR_Container_API& WIR_Container_API::operator=(WIR_Container_API&&)" );

  mContainerPointers = move( __o.mContainerPointers );
  __o.mContainerPointers.clear();

  return( *this );
};


//
// Private class methods
//

/*
  copyContainers performs actions common to the copy constructor and copy
  assignment operator of WIR_Container_API.
*/
void WIR_Container_API::copyContainers( const WIR_Container_API &__o )
{
  DSTART( "void WIR_Container_API::copyContainers(const WIR_Container_API&)" );

  // While copying containers, we create the copies of the containers in an
  // order that matches the order of the original containers' IDs.

  // Sort the IDs of containers from __o.
  vector<WIR_id_t> originalIDs;
  WIR_ContainerSet originalContainers = __o.getContainers();

  for ( WIR_BaseContainer &c : originalContainers )
    originalIDs.push_back( c.getID() );
  sort( originalIDs.begin(), originalIDs.end() );

  // Copy and attach containers according to the sorted order.
  clearContainers();
  for ( auto id : originalIDs ) {
    auto it = originalContainers.begin();
    while ( (*it).get().getID() != id )
      ++it;

    insertContainer( (*it).get() );
  }
};

}       // namespace WIR
