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
  @file wirreachabilityanalysis.cc
  @brief This file implements the %WIR reachability control flow analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include boost headers
#include <boost/graph/iteration_macros.hpp>

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
  Default constructor for function-level analysis.
*/
WIR_ReachabilityAnalysis::WIR_ReachabilityAnalysis( WIR_Function &f ) :
  WIR_ControlFlowAnalysis { f }
{
  DSTART( "WIR_ReachabilityAnalysis::WIR_ReachabilityAnalysis(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_ReachabilityAnalysis::~WIR_ReachabilityAnalysis( void )
{
  DSTART( "virtual WIR_ReachabilityAnalysis::~WIR_ReachabilityAnalysis()" );
};


//
// Protected class methods
//

/*
  runAnalysis performs reachability control flow analysis of the given function.
*/
void WIR_ReachabilityAnalysis::runAnalysis( WIR_Function &f )
{
  DSTART( "virtual void WIR_ReachabilityAnalysis::runAnalysis(WIR_Function&)" );

  // Initialize parameter-level data structures.
  init( f );

  // Compute reachability by depth-first searches starting from each graph node.
  BGL_FORALL_VERTICES( v, mCGraph, CGraph ) {
    // Determine basic block and its associated reachability container.
    WIR_BasicBlock &b = mCGraph[ v ].getBasicBlock();
    WIR_Reachability &reachContainer =
      b.getContainers<WIR_Reachability>().begin()->get();

    // Do DFS visit of CFG starting with the current node.
    CGraphVertex start = v;
    ReachabilityVisitor vis { reachContainer, true, start };
    vector<boost::default_color_type> color_map( boost::num_vertices( mCGraph ) );

    DOUT(
      "Doing reachability analysis from start node '" << b.getName() <<
      "' (including back edges)." << endl );

    boost::depth_first_visit(
      mCGraph, v, vis,
      boost::make_iterator_property_map(
        color_map.begin(), boost::get( boost::vertex_index, mCGraph ),
        color_map[ 0 ] ) );

    // Check for self-loops of the current basic block which can only be a back
    // edge.
    for ( auto p : mWIRBackEdges )
      if ( ( p.first.get() == p.second.get() ) &&
           ( mNodeByID[ p.first.get().getID() ] == v ) )
        // Detected a self-loop for the current CFG node.
        reachContainer.addReachableBlock( p.first.get() );
  }

  // Remove all back edges from the CFG.
  for ( auto e : mBackEdges )
    remove_edge( e, mCGraph );

  // Compute reachability by a second series of depth-first searches, but now
  // without using back edges.
  BGL_FORALL_VERTICES( v, mCGraph, CGraph ) {
    // Determine basic block and its associated reachability container.
    WIR_BasicBlock &b = mCGraph[ v ].getBasicBlock();
    WIR_Reachability &reachContainer =
      b.getContainers<WIR_Reachability>().begin()->get();

    // Do DFS visit of CFG starting with the current node.
    CGraphVertex start = v;
    ReachabilityVisitor vis { reachContainer, false, start };
    vector<boost::default_color_type> color_map( boost::num_vertices( mCGraph ) );

    DOUT(
      "Doing reachability analysis from start node '" << b.getName() <<
      "' (not including back edges)." << endl );

    boost::depth_first_visit(
      mCGraph, v, vis,
      boost::make_iterator_property_map(
        color_map.begin(), boost::get( boost::vertex_index, mCGraph ),
        color_map[ 0 ] ) );
  }

  // Rebuild the CFG due to the previous modifications.
  rebuild();
};


//
// Private class methods
//

/*
  init initializes data structures by attaching fresh containers to basic
  blocks.
*/
void WIR_ReachabilityAnalysis::init( WIR_Function &f )
{
  DSTART( "void WIR_ReachabilityAnalysis::init(WIR_Function&)" );

  // Clear previous analysis results by attaching fresh containers.
  for ( WIR_BasicBlock &b : f )
    b.insertContainer( new WIR_Reachability( b ) );
};


/*
  Default constructor for reachability visitors.
*/
WIR_ReachabilityAnalysis::ReachabilityVisitor::ReachabilityVisitor( WIR_Reachability &c,
                                                                    bool be,
                                                                    CGraphVertex &s ) :
  mContainer { c },
  mWithBackEdges { be },
  mStart { s }
{
  DSTART(
    "WIR_ReachabilityAnalysis::ReachabilityVisitor::ReachabilityVisitor(WIR_Reachability&, bool, WIR_CFG::CGraphVertex&)" );
};


/*
  discover_vertex visits a new reachable CFG node.
*/
void WIR_ReachabilityAnalysis::ReachabilityVisitor::discover_vertex( CGraphVertex v,
                                                                     const CGraph &g ) const
{
  DSTART(
    "void WIR_ReachabilityAnalysis::ReachabilityVisitor::discover_vertex(WIR_CFG::CGraphVertex, const CGraph&) const" );

  if ( v != mStart ) {
    DOUT(
      "'" << g[ v ].getBasicBlock().getName() << "' is reachable from '" <<
      g[ mStart ].getBasicBlock().getName() << "' (" <<
      string( mWithBackEdges ? "" : "not " ) << "including back edges)." <<
      endl );
    mContainer.addReachableBlock( g[ v ].getBasicBlock(), mWithBackEdges );
  }
};


/*
  back_edge marks an identified back edge.
*/
void WIR_ReachabilityAnalysis::ReachabilityVisitor::back_edge( CGraphEdge e,
                                                               const CGraph &g ) const
{
  DSTART(
    "void WIR_ReachabilityAnalysis::ReachabilityVisitor::back_edge(WIR_CFG::CGraphEdge, const CGraph&) const" );

  CGraphVertex tgt = target( e, g );

  if ( tgt == mStart ) {
    DOUT(
      "'" << g[ tgt ].getBasicBlock().getName() << "' is reachable from '" <<
      g[ mStart ].getBasicBlock().getName() << "' (" <<
      string( mWithBackEdges ? "" : "not " ) << "including back edges)." <<
      endl );

    // The current start node can reach itself via a back edge.
    mContainer.addReachableBlock( g[ tgt ].getBasicBlock(), mWithBackEdges );
  }
};

}       // namespace WIR
