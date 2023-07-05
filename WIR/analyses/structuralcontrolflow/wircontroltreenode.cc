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
  @file wircontroltreenode.cc
  @brief This file implements control tree nodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <sstream>

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
WIR_ControlTreeNode::WIR_ControlTreeNode( void ) :
  WIR_ID_API {},
  mParent { nullptr }
{
  DSTART( "WIR_ControlTreeNode::WIR_ControlTreeNode()" );
};


/*
  Destructor.
*/
WIR_ControlTreeNode::~WIR_ControlTreeNode( void )
{
  DSTART( "virtual WIR_ControlTreeNode::~WIR_ControlTreeNode()" );
};


/*
  isAcyclic returns whether a WIR control tree node is acyclic or not.
*/
bool WIR_ControlTreeNode::isAcyclic( void ) const
{
  DSTART( "bool WIR_ControlTreeNode::isAcyclic() const" );

  return( !isCyclic() );
};


/*
  isEntry returns whether a control tree node is the entry node of a potential
  parent or not.
*/
bool WIR_ControlTreeNode::isEntry( void ) const
{
  DSTART( "bool WIR_ControlTreeNode::isEntry() const" );

  if ( mParent && ( mParent->getEntry().getID() == getID() ) )
    return( true );

  return( false );
};


/*
  getChilds returns the set of all control tree child nodes.
*/
const WIR_ControlTreeNodeSet &WIR_ControlTreeNode::getChilds( void ) const
{
  DSTART(
    "const WIR_ControlTreeNodeSet& WIR_ControlTreeNode::getChilds() const" );

  return( mChildReferences );
};


/*
  begin returns an iterator to the first child node.
*/
WIR_ControlTreeNodeSet::const_iterator WIR_ControlTreeNode::begin( void ) const
{
  DSTART(
    "WIR_ControlTreeNodeSet::const_iterator WIR_ControlTreeNode::begin() const" );

  return( mChildReferences.begin() );
};


/*
  end returns an iterator to the end of the child node set.
*/
WIR_ControlTreeNodeSet::const_iterator WIR_ControlTreeNode::end( void ) const
{
  DSTART(
    "WIR_ControlTreeNodeSet::const_iterator WIR_ControlTreeNode::end() const" );

  return( mChildReferences.end() );
};


/*
  getParent returns a control tree node's parent node.
*/
WIR_ControlTreeNode &WIR_ControlTreeNode::getParent( void ) const
{
  DSTART( "WIR_ControlTreeNode& WIR_ControlTreeNode::getParent() const" );

  if ( mParent == nullptr )
    return( const_cast<WIR_ControlTreeNode &>( *this ) );
  else
    return( *mParent );
};

/*
  getRoot returns the root of the entire control tree that this node is part of.
*/
WIR_ControlTreeNode &WIR_ControlTreeNode::getRoot( void ) const
{
  DSTART( "WIR_ControlTreeNode& WIR_ControlTreeNode::getRoot() const" );

  if ( mParent == nullptr )
    return( const_cast<WIR_ControlTreeNode &>( *this ) );
   else
    return( getParent().getRoot() );
};


/*
  getBasicBlocks determines the set of actual WIR basic blocks hierarchically
  included in a control tree node.
*/
WIR_BasicBlockSet WIR_ControlTreeNode::getBasicBlocks( void ) const
{
  DSTART( "WIR_BasicBlockSet WIR_ControlTreeNode::getBasicBlocks() const" );

  WIR_BasicBlockSet res;

  if ( getType() == WIR_CTNodeType::bb ) {
    auto &bbNode = dynamic_cast<const WIR_BasicBlockTreeNode &>( *this );
    res.insert( bbNode.getBasicBlock() );
  } else
    for ( WIR_ControlTreeNode &c : getChilds() ) {
      auto bbs = c.getBasicBlocks();
      res.insert( bbs.begin(), bbs.end() );
    }

  return( res );
};


/*
  getBackEdges returns the set of back-edges of an improper region.
*/
const WIR_ControlTreeNode::WIR_ControlTreeEdgeSet &WIR_ControlTreeNode::getBackEdges( void ) const
{
  DSTART(
    "const WIR_ControlTreeEdgeSet& WIR_ControlTreeNode::getBackEdges() const" );

  return( mBackEdges );
};


//
// Protected class methods
//

/*
  buildNodeName returns a string denoting the specified node's name.
*/
string WIR_ControlTreeNode::buildNodeName( void ) const
{
  DSTART( "string WIR_ControlTreeNode::buildNodeName() const" );

  ostringstream str;

  switch ( getType() ) {
    case WIR_CTNodeType::bb: {
      str << "Basic Block";
      break;
    }

    case WIR_CTNodeType::block: {
      str << "Block Region";
      break;
    }

    case WIR_CTNodeType::ifthen: {
      str << "If-Then Region";
      break;
    }

    case WIR_CTNodeType::ifthenelse: {
      str << "If-Then-Else Region";
      break;
    }

    case WIR_CTNodeType::switchcase: {
      str << "Switch-Case Region";
      break;
    }

    case WIR_CTNodeType::proper: {
      str << "Proper Region";
      break;
    }

    case WIR_CTNodeType::selfloop: {
      str << "Self-Loop";
      break;
    }

    case WIR_CTNodeType::whileloop: {
      str << "While-Do Loop";
      break;
    }

    case WIR_CTNodeType::naturalloop: {
      str << "Natural Loop";
      break;
    }

    case WIR_CTNodeType::improper: {
      str << "Improper Region";
      break;
    }
  }

  str << " (ID = " << getID() << ")";

  return( str.str() );
};


//
// Private class methods
//

/*
  insertChild inserts a child into a control tree node's hierarchy.

  The control tree node into which a child is inserted takes over control of the
  specified pointer. In particular, the parent node will automatically destroy a
  child node. Manual destruction of child nodes which are inserted into a
  hierarchical control tree is thus strongly discouraged!
*/
void WIR_ControlTreeNode::insertChild( WIR_ControlTreeNode *c )
{
  DSTART( "void WIR_ControlTreeNode::insertChild(WIR_ControlTreeNode*)" );

  c->mParent = this;
  mChildPointers.push_back( unique_ptr<WIR_ControlTreeNode>( c ) );
  mChildReferences.insert( *c );
};

}       // namespace WIR
