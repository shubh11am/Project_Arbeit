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
  @file wirreachability.cc
  @brief This file implements a %WIR container representing reachability control
         flow information.

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


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor.
*/
WIR_Reachability::WIR_Reachability( const WIR_BasicBlock &b ) :
  WIR_Container<WIR_Reachability> {},
  mBB { const_cast<WIR_BasicBlock *>( &b ) }
{
  DSTART( "WIR_Reachability::WIR_Reachability(const WIR_BasicBlock&)" );
};


/*
  Copy constructor.
*/
WIR_Reachability::WIR_Reachability( const WIR_Reachability &__o ) :
  WIR_Container<WIR_Reachability> { __o },
  mBB { __o.mBB },
  mReachableBlocks { __o.mReachableBlocks },
  mReachableBlockIDs { __o.mReachableBlockIDs },
  mReachableBlocksWOBackEdges { __o.mReachableBlocksWOBackEdges },
  mReachableBlockIDsWOBackEdges { __o.mReachableBlockIDsWOBackEdges }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Move constructor.
*/
WIR_Reachability::WIR_Reachability( WIR_Reachability &&__o ) :
  WIR_Container<WIR_Reachability> { move( __o ) },
  mBB { __o.mBB },
  mReachableBlocks { move( __o.mReachableBlocks ) },
  mReachableBlockIDs { move( __o.mReachableBlockIDs ) },
  mReachableBlocksWOBackEdges { move( __o.mReachableBlocksWOBackEdges ) },
  mReachableBlockIDsWOBackEdges { move( __o.mReachableBlockIDsWOBackEdges ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  __o.mBB = nullptr;
  __o.mReachableBlocks.clear();
  __o.mReachableBlockIDs.clear();
  __o.mReachableBlocksWOBackEdges.clear();
  __o.mReachableBlockIDsWOBackEdges.clear();
};


/*
  Destructor.
*/
WIR_Reachability::~WIR_Reachability( void )
{
  DSTART( "virtual WIR_Reachability::~WIR_Reachability()" );
};


/*
  Copy-assignment operator.
*/
WIR_Reachability & WIR_Reachability::operator = ( const WIR_Reachability &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Container<WIR_Reachability>::operator = ( __o );

  mBB = __o.mBB;
  mReachableBlocks = __o.mReachableBlocks;
  mReachableBlockIDs = __o.mReachableBlockIDs;
  mReachableBlocksWOBackEdges = __o.mReachableBlocksWOBackEdges;
  mReachableBlockIDsWOBackEdges = __o.mReachableBlockIDsWOBackEdges;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_Reachability & WIR_Reachability::operator = ( WIR_Reachability &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Container<WIR_Reachability>::operator = ( move( __o ) );

  mBB = __o.mBB;
  __o.mBB = nullptr;
  mReachableBlocks = move( __o.mReachableBlocks );
  __o.mReachableBlocks.clear();
  mReachableBlockIDs = move( __o.mReachableBlockIDs );
  __o.mReachableBlockIDs.clear();
  mReachableBlocksWOBackEdges = move( __o.mReachableBlocksWOBackEdges );
  __o.mReachableBlocksWOBackEdges.clear();
  mReachableBlockIDsWOBackEdges = move( __o.mReachableBlockIDsWOBackEdges );
  __o.mReachableBlockIDsWOBackEdges.clear();

  return( *this );
};


/*
  isUnique returns whether reachability information is unique, i.e., whether at
  most one instance of this container type can be attached to a WIR class.
*/
bool WIR_Reachability::isUnique( void ) const
{
  DSTART( "virtual bool WIR_Reachability::isUnique() const" );

  return( true );
};


/*
  addReachableBlock adds the specified basic block to the set of reachable
  blocks.
*/
void WIR_Reachability::addReachableBlock( WIR_BasicBlock &b, bool be )
{
  DSTART( "void WIR_Reachability::addReachableBlock(WIR_BasicBlock&, bool)" );

  if ( be ) {
    mReachableBlocks.insert( b );
    mReachableBlockIDs.insert( b.getID() );
  } else {
    mReachableBlocksWOBackEdges.insert( b );
    mReachableBlockIDsWOBackEdges.insert( b.getID() );
  }
};


/*
  getReachableBlocks returns the set of reachable basic blocks.
*/
const WIR_BasicBlockSet &WIR_Reachability::getReachableBlocks( bool be ) const
{
  DSTART(
    "const WIR_BasicBlockSet& WIR_Reachability::getReachableBlocks(bool) const" );

  if ( be )
    return( mReachableBlocks );
  else
    return( mReachableBlocksWOBackEdges );
};


/*
  isReachable determines whether the given basic block is reachable via a
  control flow path starting in that basic block to which this container is
  attached.
*/
bool WIR_Reachability::isReachable( const WIR_BasicBlock &b, bool be ) const
{
  DSTART(
    "bool WIR_Reachability::isReachable(const WIR_BasicBlock&, bool) const" );

  auto &reachableBlockIDs =
    be ? mReachableBlockIDs : mReachableBlockIDsWOBackEdges;

  bool res =
    ( reachableBlockIDs.find( b.getID() ) != reachableBlockIDs.end() );

  DOUT(
    "'" << b.getName() << "' is " << string( res ? "" : "not " ) <<
    "reachable from '" << mBB->getName() << "' (" <<
    string( be ? "" : "not " ) << "including back edges)." << endl );

  return( res );
};


/*
  This static variant of isReachable determines whether the two given
  instructions are reachable via a control flow path starting in i1 and ending
  in i2.
*/
bool WIR_Reachability::isReachable( const WIR_Instruction &i1,
                                    const WIR_Instruction &i2, bool be )
{
  DSTART(
    "static bool WIR_Reachability::isReachable(const WIR_Instruction&, const WIR_Instruction&, bool)" );

  auto &b1 = i1.getBasicBlock();
  auto &b2 = i2.getBasicBlock();

  // If both instructions are in the same basic block and i1 comes first, then
  // i2 is reachable.
  if ( b1 == b2 ) {
    bool i1ComesFirst = true;

    for ( WIR_Instruction &i : b1 )
      if ( i == i1 )
        break;
      else

      if ( i == i2 ) {
        i1ComesFirst = false;
        break;
      }

    if ( i1ComesFirst && ( i1 != i2 ) ) {
      DOUT(
        "isReachable( i1, i2, " << string( be ? "true" : "false" ) <<
        " ) = true" << endl );
      return( true );
    }
  }

  // Otherwise, check reachability between both involved basic blocks.
  auto &c1 = b1.getContainers<WIR_Reachability>().begin()->get();
  return( c1.isReachable( b2, be ) );
};

}       // namespace WIR
