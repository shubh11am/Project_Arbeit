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
  @file wirpropertreenode.cc
  @brief This file implements proper acyclic tree nodes.

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


//
// Public class methods
//

/*
  Default constructor.
*/
WIR_ProperTreeNode::WIR_ProperTreeNode( const WIR_ControlTreeNode &n ) :
  WIR_ControlTreeNode {},
  mEntry { n }
{
  DSTART(
    "WIR_ProperTreeNode::WIR_ProperTreeNode(const WIR_ControlTreeNode&)" );
};


/*
  Destructor.
*/
WIR_ProperTreeNode::~WIR_ProperTreeNode( void )
{
  DSTART( "virtual WIR_ProperTreeNode::~WIR_ProperTreeNode()" );
};


/*
  getType returns the type of a proper region node.
*/
WIR_CTNodeType WIR_ProperTreeNode::getType( void ) const
{
  DSTART( "virtual WIR_CTNodeType WIR_ProperTreeNode::getType() const" );

  return( WIR_CTNodeType::proper );
};


/*
  isCyclic returns whether a proper region is cyclic or not.
*/
bool WIR_ProperTreeNode::isCyclic( void ) const
{
  DSTART( "virtual bool WIR_ProperTreeNode::isCyclic() const" );

  return( false );
};


/*
  getEntry returns a proper region's unique entry child node.
*/
const WIR_ControlTreeNode &WIR_ProperTreeNode::getEntry( void ) const
{
  DSTART(
    "virtual const WIR_ControlTreeNode& WIR_ProperTreeNode::getEntry() const" );

  return( mEntry );
};


/*
  getNodes returns the set of all stored nodes.
*/
const WIR_ControlTreeNodeSet &WIR_ProperTreeNode::getNodes( void ) const
{
  DSTART(
    "const WIR_ControlTreeNodeSet& WIR_ProperTreeNode::getNodes() const" );

  return( mNodes );
};


//
// Protected class methods
//

/*
  visualize dumps a proper region node into a given DOT file.
*/
void WIR_ProperTreeNode::visualize( std::fstream &dotFile ) const
{
  DSTART( "virtual void WIR_ProperTreeNode::visualize(fstream&) const" );

  dotFile << "subgraph cluster" << getID() << " {" << endl;

  dotFile << "  label=\"" << buildNodeName() << "\";" << endl;
  dotFile << "  color=blue;" << endl;

  // Recursively visualize all included hierarchical child nodes.
  for ( WIR_ControlTreeNode &c : mNodes )
    c.visualize( dotFile );

  dotFile << "}" << endl;
};


//
// Private class methods
//

/*
  insertNode adds a new node to a proper region.
*/
void WIR_ProperTreeNode::insertNode( WIR_ControlTreeNode &n )
{
  DSTART( "void WIR_ProperTreeNode::insertNode(WIR_ControlTreeNode&)" );

  mNodes.insert( ref( n ) );
};


/*
  insertEdge adds a new edge between two nodes to a proper region.
*/
void WIR_ProperTreeNode::insertEdge( const WIR_ControlTreeNode &s,
                                     const WIR_ControlTreeNode &t )
{
  DSTART(
    "void WIR_ProperTreeNode::insertEdge(const WIR_ControlTreeNode&, const WIR_ControlTreeNode&)" );

  mEdges.insert(
    make_pair(
      const_cast<WIR_ControlTreeNode *>( &s ),
      const_cast<WIR_ControlTreeNode *>( &t ) ) );
};

}       // namespace WIR
