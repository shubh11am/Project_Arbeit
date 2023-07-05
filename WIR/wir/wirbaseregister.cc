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
  @file wirbaseregister.cc
  @brief This file implements the basic interface of generic %WIR registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <iostream>
#include <list>
#include <sstream>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

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
  Default constructor for registers.
*/
WIR_BaseRegister::WIR_BaseRegister( const WIR_BaseProcessor::RegisterType &__r ) :
  WIR_ID_API {},
  mParent { nullptr },
  mType { const_cast<WIR_BaseProcessor::RegisterType *>( &__r ) }
{
  DSTART(
    "WIR_BaseRegister::WIR_BaseRegister(const WIR_BaseProcessor::RegisterType&)" );

  stringstream str;
  str << getID();
  mName = str.str();
};


/*
  Copy constructor.
*/
WIR_BaseRegister::WIR_BaseRegister( const WIR_BaseRegister &__o ) :
  WIR_ID_API { __o },
  mParent { nullptr },
  mType { __o.mType }
{
  DSTART( "WIR_BaseRegister::WIR_BaseRegister(const WIR_BaseRegister&)" );

  stringstream str;
  str << getID();
  mName = str.str();
};


/*
  Move constructor.
*/
WIR_BaseRegister::WIR_BaseRegister( WIR_BaseRegister &&__o ) :
  WIR_ID_API { __o },
  mParent { __o.mParent },
  mType { __o.mType }
{
  DSTART( "WIR_BaseRegister::WIR_BaseRegister(WIR_BaseRegister&&)" );

  __o.mType = nullptr;

  stringstream str;
  str << getID();
  mName = str.str();
  __o.mName.clear();
};


/*
  Destructor.
*/
WIR_BaseRegister::~WIR_BaseRegister( void )
{
  DSTART( "virtual WIR_BaseRegister::~WIR_BaseRegister()" );
};


/*
  Copy-assignment operator.
*/
WIR_BaseRegister & WIR_BaseRegister::operator = ( const WIR_BaseRegister &__o )
{
  DSTART(
    "WIR_BaseRegister& WIR_BaseRegister::operator=(const WIR_BaseRegister&)" );

  mParent = __o.mParent;
  mType = __o.mType;

  stringstream str;
  str << getID();
  mName = str.str();

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_BaseRegister & WIR_BaseRegister::operator = ( WIR_BaseRegister &&__o )
{
  DSTART(
    "WIR_BaseRegister& WIR_BaseRegister::operator=(WIR_BaseRegister&&)" );

  mParent = __o.mParent;
  mType = move( __o.mType );
  __o.mType = nullptr;

  stringstream str;
  str << getID();
  mName = str.str();
  __o.mName.clear();

  return( *this );
};


/*
  getType gets a register's actual type.
*/
WIR_BaseProcessor::RegisterType &WIR_BaseRegister::getType( void ) const
{
  DSTART(
    "WIR_BaseProcessor::RegisterType& WIR_BaseRegister::getType() const" );

  ufAssertT(
    mType != nullptr,
    "Invalid attempt to get type from a register that has previously " <<
    "been moved." );

  return( *mType );
};


/*
  getBitWidth returns a register's bit width.
*/
unsigned int WIR_BaseRegister::getBitWidth( void ) const
{
  DSTART( "unsigned int WIR_BaseRegister::getBitWidth() const" );

  return( getType().getBitWidth() );
};


/*
  isPhysical returns whether a register is physical.
*/
bool WIR_BaseRegister::isPhysical( void ) const
{
  DSTART( "bool WIR_BaseRegister::isPhysical() const" );

  return( !isVirtual() );
};


/*
  getName returns a register's specific name.
*/
string WIR_BaseRegister::getName( void ) const
{
  DSTART( "string WIR_BaseRegister::getName() const" );

  auto &t = getType();
  return(
    t.getPrefixes()[ isVirtual() ] + mName + t.getSuffixes()[ isVirtual() ] );
};


/*
  isChild returns whether a register is child of some parent register, i.e.,
  whether it is part of a register hierarchy.
*/
bool WIR_BaseRegister::isChild( void ) const
{
  DSTART( "bool WIR_BaseRegister::isChild() const" );

  return( ( mParent == nullptr ) ? false : true );
};


/*
  isChild returns whether this register is a child of the specified register in
  the register hierarchy.
*/
bool WIR_BaseRegister::isChildOf( const WIR_BaseRegister &r ) const
{
  DSTART( "bool WIR_BaseRegister::isChildOf(const WIR_BaseRegister&) const" );

  if ( mParent == nullptr )
    return( false );

  return( ( *mParent == r ) ? true : mParent->isChildOf( r ) );
};


/*
  isHierarchical returns whether a register is part of a register hierarchy,
  i.e., whether it has some childs or is a child.
*/
bool WIR_BaseRegister::isHierarchical( void ) const
{
  DSTART( "bool WIR_BaseRegister::isHierarchical() const" );

  return( isChild() || hasChilds() );
};


/*
  getParent returns a register's parent register.
*/
WIR_BaseRegister &WIR_BaseRegister::getParent( void ) const
{
  DSTART( "WIR_BaseRegister& WIR_BaseRegister::getParent() const" );

  if ( mParent == nullptr )
    return( const_cast<WIR_BaseRegister &>( *this ) );
  else
    return( *mParent );
};


/*
  getRoot returns the root of a complex register hierarchy that this register is
  part of.
*/
WIR_BaseRegister &WIR_BaseRegister::getRoot( void ) const
{
  DSTART( "WIR_BaseRegister& WIR_BaseRegister::getRoot() const" );

  if ( mParent == nullptr )
    return( const_cast<WIR_BaseRegister &>( *this ) );
   else
    return( getParent().getRoot() );
};


/*
  getLeafs returns all leafs of the register hierarchy rooted by this object,
  i.e., all registers that have no childs.

  Leaf registers are returned in little-endian order. I.e., the
  least-significant leaf register is the first element of the returned vector.
*/
const std::vector<std::reference_wrapper<WIR_BaseRegister>> WIR_BaseRegister::getLeafs( void ) const
{
  DSTART(
    "const vector<reference_wrapper<WIR_BaseRegister> > WIR_BaseRegister::getLeafs() const" );

  vector<reference_wrapper<WIR_BaseRegister>> res;
  list<WIR_BaseRegister *> q;
  q.push_back( const_cast<WIR_BaseRegister *>( this ) );

  while ( !q.empty() ) {
    auto *r = q.front();
    q.pop_front();

    if ( !( r->hasChilds() ) )
      // We found a leaf register, so we add it to res.
      res.push_back( *r );
    else {
      // The current register is not a leaf so that we insert its child into
      // the ready queue.
      if ( r->isVirtual() )
        for ( WIR_BaseRegister &c :
                dynamic_cast<WIR_VirtualRegister *>( r )->getChilds() )
          q.push_back( &c );
      else
        for ( WIR_BaseRegister &c :
                dynamic_cast<WIR_PhysicalRegister *>( r )->getChilds() )
          q.push_back( &c );
    }
  }

  return( res );
};

}       // namespace WIR
