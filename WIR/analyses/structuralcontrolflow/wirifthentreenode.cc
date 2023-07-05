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
  @file wirifthentreenode.cc
  @brief This file implements if-then tree nodes.

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
WIR_IfThenTreeNode::WIR_IfThenTreeNode( const WIR_ControlTreeNode &c,
                                        const WIR_ControlTreeNode &b ) :
  WIR_ControlTreeNode {},
  mCondition { c },
  mBranch { b }
{
  DSTART(
    "WIR_IfThenTreeNode::WIR_IfThenTreeNode(const WIR_ControlTreeNode&, const WIR_ControlTreeNode&)");
};


/*
  Destructor.
*/
WIR_IfThenTreeNode::~WIR_IfThenTreeNode( void )
{
  DSTART( "virtual WIR_IfThenTreeNode::~WIR_IfThenTreeNode()" );
};


/*
  getType returns the type of an if-then region node.
*/
WIR_CTNodeType WIR_IfThenTreeNode::getType( void ) const
{
  DSTART( "virtual WIR_CTNodeType WIR_IfThenTreeNode::getType() const" );

  return( WIR_CTNodeType::ifthen );
};


/*
  isCyclic returns whether an if-then region is cyclic or not.
*/
bool WIR_IfThenTreeNode::isCyclic( void ) const
{
  DSTART( "virtual bool WIR_IfThenTreeNode::isCyclic() const" );

  return( false );
};


/*
  getEntry returns an if-then region's unique entry child node.
*/
const WIR_ControlTreeNode &WIR_IfThenTreeNode::getEntry( void ) const
{
  DSTART(
    "virtual const WIR_ControlTreeNode& WIR_IfThenTreeNode::getEntry() const" );

  return( mCondition );
};


/*
  getCondition returns an if-then-else's condition node.
*/
const WIR_ControlTreeNode& WIR_IfThenTreeNode::getCondition( void ) const
{
  DSTART(
    "const WIR_ControlTreeNode& WIR_IfThenTreeNode::getCondition() const" );

  return( mCondition );
};


/*
  getBranch returns the control tree node representing the then-branch of an
  if-then.
*/
const WIR_ControlTreeNode& WIR_IfThenTreeNode::getBranch( void ) const
{
  DSTART( "const WIR_ControlTreeNode& WIR_IfThenTreeNode::getBranch() const" );

  return( mBranch );
};


//
// Protected class methods
//

/*
  visualize dumps an if-then-else region node into a given DOT file.
*/
void WIR_IfThenTreeNode::visualize( std::fstream &dotFile ) const
{
  DSTART( "virtual void WIR_IfThenTreeNode::visualize(std::fstream&) const" );

  dotFile << "subgraph cluster" << getID() << " {" << endl;

  dotFile << "  label=\"" << buildNodeName() << "\";" << endl;
  dotFile << "  color=blue;" << endl;

  // Recursively visualize all included hierarchical child nodes.
  mCondition.visualize( dotFile );
  mBranch.visualize( dotFile );

  dotFile << "}" << endl;
};

}       // namespace WIR
