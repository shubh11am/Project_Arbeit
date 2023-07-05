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
  @file wirselflooptreenode.cc
  @brief This file implements self-loop tree nodes.

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
WIR_SelfLoopTreeNode::WIR_SelfLoopTreeNode( const WIR_ControlTreeNode &n ) :
  WIR_ControlTreeNode {},
  mEntry { n }
{
  DSTART(
    "WIR_SelfLoopTreeNode::WIR_SelfLoopTreeNode(const WIR_ControlTreeNode&)" );

  mBackEdges.insert(
    make_pair(
      const_cast<WIR_ControlTreeNode *>( &n ),
      const_cast<WIR_ControlTreeNode *>( &n ) ) );
};


/*
  Destructor.
*/
WIR_SelfLoopTreeNode::~WIR_SelfLoopTreeNode( void )
{
  DSTART( "virtual WIR_SelfLoopTreeNode::~WIR_SelfLoopTreeNode()" );
};


/*
  getType returns the type of a self-loop region node.
*/
WIR_CTNodeType WIR_SelfLoopTreeNode::getType( void ) const
{
  DSTART( "virtual WIR_CTNodeType WIR_SelfLoopTreeNode::getType() const" );

  return( WIR_CTNodeType::selfloop );
};


/*
  isCyclic returns whether a self-loop region is cyclic or not.
*/
bool WIR_SelfLoopTreeNode::isCyclic( void ) const
{
  DSTART( "virtual bool WIR_SelfLoopTreeNode::isCyclic() const" );

  return( true );
};


/*
  getEntry returns a self-loop's unique entry child node.
*/
const WIR_ControlTreeNode& WIR_SelfLoopTreeNode::getEntry( void ) const
{
  DSTART( "const WIR_ControlTreeNode& WIR_SelfLoopTreeNode::getEntry() const" );

  return( mEntry );
};


//
// Protected class methods
//

/*
  visualize dumps a self-loop region node into a given DOT file.
*/
void WIR_SelfLoopTreeNode::visualize( std::fstream &dotFile ) const
{
  DSTART( "virtual void WIR_SelfLoopTreeNode::visualize(fstream&) const" );

  dotFile << "subgraph cluster" << getID() << " {" << endl;

  dotFile << "  label=\"" << buildNodeName() << "\";" << endl;
  dotFile << "  color=blue;" << endl;

  // Recursively visualize all included hierarchical child nodes.
  mEntry.visualize( dotFile );

  dotFile << "}" << endl;
};

}       // namespace WIR
