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
  @file wirswitchcasetreenode.cc
  @brief This file implements switch-case tree nodes.

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
WIR_SwitchCaseTreeNode::WIR_SwitchCaseTreeNode( const WIR_ControlTreeNode &c ) :
  WIR_ControlTreeNode {},
  mCondition { c }
{
  DSTART(
    "WIR_SwitchCaseTreeNode::WIR_SwitchCaseTreeNode(const WIR_ControlTreeNode&)" );
};


/*
  Destructor.
*/
WIR_SwitchCaseTreeNode::~WIR_SwitchCaseTreeNode( void )
{
  DSTART( "virtual WIR_SwitchCaseTreeNode::~WIR_SwitchCaseTreeNode()" );
};


/*
  getType returns the type of a switch-case region node.
*/
WIR_CTNodeType WIR_SwitchCaseTreeNode::getType( void ) const
{
  DSTART( "virtual WIR_CTNodeType WIR_SwitchCaseTreeNode::getType() const" );

  return( WIR_CTNodeType::switchcase );
};


/*
  isCyclic returns whether a switch-case region is cyclic or not.
*/
bool WIR_SwitchCaseTreeNode::isCyclic( void ) const
{
  DSTART( "virtual bool WIR_SwitchCaseTreeNode::isCyclic() const" );

  return( false );
};


/*
  getEntry returns a switch-case region's unique entry child node.
*/
const WIR_ControlTreeNode &WIR_SwitchCaseTreeNode::getEntry( void ) const
{
  DSTART(
    "virtual const WIR_ControlTreeNode& WIR_SwitchCaseTreeNode::getEntry() const" );

  return( mCondition );
};


/*
  getCondition returns an switch-case's condition node.
*/
const WIR_ControlTreeNode& WIR_SwitchCaseTreeNode::getCondition( void ) const
{
  DSTART(
    "const WIR_ControlTreeNode& WIR_SwitchCaseTreeNode::getCondition() const" );

  return( mCondition );
};


/*
  getCases returns the list of stored case nodes in their sequential order.
*/
const list<reference_wrapper<WIR_ControlTreeNode>> &WIR_SwitchCaseTreeNode::getCases( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_ControlTreeNode> >& WIR_SwitchCaseTreeNode::getCases() const" );

  return( mCaseList );
};


/*
  getFallthroughCases returns a list of fall-through case nodes in their
  sequential order.
*/
const list<list<reference_wrapper<WIR_ControlTreeNode>>::const_iterator> WIR_SwitchCaseTreeNode::getFallthroughCases( void ) const
{
  DSTART(
    "const list<list<reference_wrapper<WIR_ControlTreeNode> >::const_iterator > WIR_SwitchCaseTreeNode::getFallthroughCases() const" );

  list<list<reference_wrapper<WIR_ControlTreeNode>>::const_iterator> res;

  for ( auto it = mCaseList.begin(); it != mCaseList.end(); ++it )
    if ( mFallthroughCases.count( it->get().getID() ) )
      res.push_back( it );

  return( res );
};


//
// Protected class methods
//

/*
  visualize dumps a switch-case region node into a given DOT file.
*/
void WIR_SwitchCaseTreeNode::visualize( std::fstream &dotFile ) const
{
  DSTART( "virtual void WIR_SwitchCaseTreeNode::visualize(fstream&) const" );

  dotFile << "subgraph cluster" << getID() << " {" << endl;

  dotFile << "  label=\"" << buildNodeName() << "\";" << endl;
  dotFile << "  color=blue;" << endl;

  // Recursively visualize all included hierarchical child nodes.
  mCondition.visualize( dotFile );

  for ( WIR_ControlTreeNode &c : mCaseList )
    c.visualize( dotFile );

  dotFile << "}" << endl;
};


//
// Private class methods
//

/*
  pushBackCaseNode adds a new node to a switch-case region at the end of list
  mCaseList.
*/
void WIR_SwitchCaseTreeNode::pushBackCaseNode( WIR_ControlTreeNode &n )
{
  DSTART(
    "void WIR_SwitchCaseTreeNode::pushBackCaseNode(WIR_ControlTreeNode&)" );

  mCaseList.push_back( ref( n ) );
};


/*
  pushBackFallthroughCaseNode adds a new node to a switch-case region at the end
  of list mCaseList and marks it as fall-through.
*/
void WIR_SwitchCaseTreeNode::pushBackFallthroughCaseNode( WIR_ControlTreeNode &n )
{
  DSTART(
    "void WIR_SwitchCaseTreeNode::pushBackFallthroughCaseNode(WIR_ControlTreeNode&)" );

  mCaseList.push_back( ref( n ) );
  mFallthroughCases.insert( n.getID() );
};

}       // namespace WIR
