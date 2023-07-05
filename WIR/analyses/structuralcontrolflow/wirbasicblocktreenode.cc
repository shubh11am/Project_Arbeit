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
  @file wirbasicblocktreenode.cc
  @brief This file implements tree nodes representing individual basic blocks.

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
WIR_BasicBlockTreeNode::WIR_BasicBlockTreeNode( WIR_BasicBlock &b ) :
  WIR_ControlTreeNode {},
  mBasicBlock { b }
{
  DSTART( "WIR_BasicBlockTreeNode::WIR_BasicBlockTreeNode(WIR_BasicBlock&)" );
};


/*
  Destructor.
*/
WIR_BasicBlockTreeNode::~WIR_BasicBlockTreeNode( void )
{
  DSTART( "virtual WIR_BasicBlockTreeNode::~WIR_BasicBlockTreeNode()" );
};


/*
  getType returns the type of a block region node.
*/
WIR_CTNodeType WIR_BasicBlockTreeNode::getType( void ) const
{
  DSTART(
    "virtual WIR_CTNodeType WIR::WIR_BasicBlockTreeNode::getType() const" );

  return( WIR_CTNodeType::bb );
};


/*
  isCyclic returns whether a basic block node is cyclic or not.
*/
bool WIR_BasicBlockTreeNode::isCyclic( void ) const
{
  DSTART( "virtual bool WIR_BasicBlockTreeNode::isCyclic() const" );

  return( false );
};


/*
  getEntry returns a basic block node's unique entry child node.
*/
const WIR_ControlTreeNode &WIR_BasicBlockTreeNode::getEntry( void ) const
{
  DSTART(
    "virtual const WIR_ControlTreeNode& WIR_BasicBlockTreeNode::getEntry() const" );

  return( *this );
};


/*
  getBasicBlockBlock returns the list of stored child nodes in their sequential
  order.
*/
WIR_BasicBlock &WIR_BasicBlockTreeNode::getBasicBlock( void ) const
{
  DSTART( "WIR_BasicBlock& WIR_BasicBlockTreeNode::getBasicBlock() const" );

  return( mBasicBlock );
};


//
// Protected class methods
//

/*
  visualize dumps a basic block node into a given DOT file.
*/
void WIR_BasicBlockTreeNode::visualize( std::fstream &dotFile ) const
{
  DSTART( "virtual void WIR_BasicBlockTreeNode::visualize(fstream&) const" );

  dotFile << "  " << getBasicBlock().getID() << "[label=\""
          << getBasicBlock().getName() << "\"";
  if ( isEntry() )
    dotFile << ",style=bold";
  dotFile << "]" << endl << ";" << endl;
};

}       // namespace WIR
