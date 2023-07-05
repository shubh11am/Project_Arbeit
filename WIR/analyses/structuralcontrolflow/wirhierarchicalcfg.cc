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
  @file wirhierarchicalcfg.cc
  @brief This file implements hierarchical control flow graphs.

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
#include <stdlib.h>
#include <string>
#include <unistd.h>
#include <vector>

// Include boost headers
#include <boost/graph/iteration_macros.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace boost;
using namespace std;


//
// Public class methods
//

/*
  WIR_Compare_HCFGNode is a comparator class that is used to sort sets of
  pointers to hierarchical control flow graph nodes.
*/
bool WIR_HierarchicalCFG::WIR_Compare_HCFGNode::operator()( const WIR_ControlTreeNode *lhs,
                                                            const WIR_ControlTreeNode *rhs ) const
{
  return( lhs->getID() < rhs->getID() );
};


/*
  Default constructor for a given WIR function.
*/
WIR_HierarchicalCFG::WIR_HierarchicalCFG( WIR_Function &f, bool verbosity,
                                          bool keepTmpFiles ) :
  mFunction { f },
  mVerbosity { verbosity },
  mKeepTmpFiles { keepTmpFiles }
{
  DSTART(
    "WIR_HierarchicalCFG::WIR_HierarchicalCFG(WIR_Function&, bool, bool)" );

  // Remove empty and unreachable basic blocks first.
  WIR_EmptyBlocks optEB { f };
  optEB.optimize();
  WIR_UnreachableBlocks optUB { f };
  optUB.optimize();

  addNodes();
  addEdges();
};


/*
  Destructor.
*/
WIR_HierarchicalCFG::~WIR_HierarchicalCFG( void )
{
  DSTART( "WIR_HierarchicalCFG::~WIR_HierarchicalCFG()" );
};


/*
  dfs traverses the CFG in depth-first order and builds the postorder traversal
  and the set of start nodes.
*/
void WIR_HierarchicalCFG::dfs( void )
{
  DSTART( "void WIR_HierarchicalCFG::dfs()" );

  if ( mFunction.getBasicBlocks().empty() )
    return;

  mStartNodes.clear();
  mBackEdges.clear();
  mPostOrder.clear();

  // Do depth-first traversal of CFG starting at the current function's very
  // first basic block.
  CFGVisitor vis { mStartNodes, mPostOrder, mBackEdges };
  vector<boost::default_color_type> color_map( boost::num_vertices( mCGraph ) );

  boost::depth_first_search(
    mCGraph,
    vis,
    boost::make_iterator_property_map(
      color_map.begin(), boost::get( boost::vertex_index, mCGraph ),
      color_map[ 0 ] ),
    mNodeByID[ mFunction.begin()->get().getID() ] );

  BGL_FORALL_EDGES( e, mCGraph, CGraph )
    mCGraph[ e ].isBackEdge = ( mBackEdges.count( e ) ) ? true : false;
};


/*
  dfsDryRun traverses the CFG in depth-first order and prints the visited nodes
  and edges for debugging purposes.
*/
void WIR_HierarchicalCFG::dfsDryRun( void )
{
  DSTART( "void WIR_HierarchicalCFG::dfsDryRun()" );

  if ( mFunction.getBasicBlocks().empty() )
    return;

  // Do depth-first traversal of CFG starting at the current function's very
  // first basic block.
  CFGVisitor vis { mStartNodes, mPostOrder, mBackEdges, true };
  vector<boost::default_color_type> color_map( boost::num_vertices( mCGraph ) );

  boost::depth_first_search(
    mCGraph,
    vis,
    boost::make_iterator_property_map(
      color_map.begin(), boost::get( boost::vertex_index, mCGraph ),
      color_map[ 0 ] ),
    mNodeByID[ mFunction.begin()->get().getID() ] );
};


/*
  getPostOrder returns the list of all hierarchical control flow graph nodes in
  their DFS postorder.
*/
const std::list<WIR_ControlTreeNode *> &WIR_HierarchicalCFG::getPostOrder( void ) const
{
  DSTART(
    "const list<WIR_ControlTreeNode*>& WIR_HierarchicalCFG::getPostOrder() const" );

  return( mPostOrder );
};


/*
  getStartNodes returns all start nodes of the current CFG.
*/
const WIR_HierarchicalCFG::WIR_HCFGNodeSet &WIR_HierarchicalCFG::getStartNodes( void ) const
{
  DSTART( "const WIR_HCFGNodeSet& WIR_HierarchicalCFG::getStartNodes() const" );

  return( mStartNodes );
};


/*
  getNumberOfBackEdges returns the number of back edges in the current CFG.
*/
unsigned int WIR_HierarchicalCFG::getNumberOfBackEdges( void ) const
{
  DSTART( "unsigned int WIR_HierarchicalCFG::getNumberOfBackEdges() const" );

  return( mBackEdges.size() );
};


/*
  visualize dumps the current control flow graph into a DOT file and invokes
  xdot on it.
*/
void WIR_HierarchicalCFG::visualize( void ) const
{
  DSTART( "void WIR_HierarchicalCFG::visualize() const" );

  char mask[12] = "/tmp/XXXXXX";
  int fileDescriptor = mkstemp( mask );

  if ( fileDescriptor == -1 )
    throw ufFatalError( "Failed to allocate temporary file." );
  else
    close( fileDescriptor );

  string fileName( mask );
  fstream dotFile( fileName, ios_base::out );

  // Write dot graph header.
  dotFile << "digraph G {" << endl << "compound=true;" << endl
          << "newrank=true;" << endl
          << "graph [rotate=90]" << endl;

  // Write dot graph nodes.
  visualizeNodes( dotFile );

  // Write dot graph edges.
  visualizeEdges( dotFile );

  // Write dot graph closing.
  dotFile << "}" << endl;
  dotFile.close();

  // Call xdot.
  ostringstream sstr;
  sstr << "xdot " << fileName << " > /dev/null 2>&1";
  if ( mVerbosity )
    ufNoteMsg << ufFile() << sstr.str() << endl;
  if ( system( sstr.str().c_str() ) != 0 )
    throw ufFatalError( "Failed to execute xdot." );

  if ( !mKeepTmpFiles )
    unlink( fileName.c_str() );
};


//
// Private class methods
//

/*
  addNodes adds CFG nodes for all basic blocks in the specified WIR function.
*/
void WIR_HierarchicalCFG::addNodes( void )
{
  DSTART( "void WIR_HierarchicalCFG::addNodes()" );

  // Traverse the current WIR function.
  for ( WIR_BasicBlock &b : mFunction ) {
    auto *n = new WIR_BasicBlockTreeNode( b );
    CGraphVertex v = add_vertex( n, mCGraph );
    mNodeByID[ b.getID() ] = v;
    mNodeByID[ n->getID() ] = v;

    DOUT(
      "Adding node with ID " << n->getID() << " for basic block '" <<
      b.getName() << "'." << endl );
  }
};


/*
  addEdges adds edges between all control flow-dependent CFG nodes.
*/
void WIR_HierarchicalCFG::addEdges( void )
{
  DSTART( "void WIR_HierarchicalCFG::addEdges()" );

  // Traverse the current WIR function.
  for ( WIR_BasicBlock &b : mFunction ) {
    CGraphVertex v1 = mNodeByID[ b.getID() ];

    for ( WIR_BasicBlock &succ : b.getSuccessors() ) {
      CGraphVertex v2 = mNodeByID[ succ.getID() ];

      DOUT(
        "Adding edge " << mCGraph[ v1 ]->getID() << " ('" << b.getName() <<
        "') -> " << mCGraph[ v2 ]->getID() << " ('" << succ.getName() <<
        "')." << endl );
      auto p = add_edge( v1, v2, mCGraph );

      // Check whether the edge b -> succ is marked as a regular loop exit or a
      // break.
      bool isLoopExit = false;
      bool isBreak = false;

      for ( auto rIt = b.getInstructions().rbegin();
            rIt != b.getInstructions().rend(); ++rIt ) {
        for ( WIR_Operation &o : rIt->get() ) {
          if ( o.isJump() &&
               o.containsContainers( WIR_LoopExit::getContainerTypeID() ) ) {
            auto &c = o.getContainers<WIR_LoopExit>().begin()->get();

            if ( c.explicitSuccessorIsExit() ) {
              // The loop exit is explicit target of the current jump o. So
              // let's check whether node succ is in o's jump targets.
              for ( WIR_BasicBlock &t : o.getJumpTargets() ) {
                if ( t == succ ) {
                  isLoopExit = true;
                  break;
                }
              }
            } else {
              // The loop exit is implicit successor of the current jump o. So
              // let's ensure that node succ is not in o's jump targets.
              bool isSuccImplicit = true;

              for ( WIR_BasicBlock &t : o.getJumpTargets() )
                if ( t == succ ) {
                  isSuccImplicit = false;
                  break;
                }

              if ( isSuccImplicit )
                isLoopExit = true;
            }
          }

          if ( o.isJump() &&
               o.containsContainers( WIR_Break::getContainerTypeID() ) )
            for ( WIR_BasicBlock &t : o.getJumpTargets() )
              if ( t == succ ) {
                isBreak = true;
                break;
              }

          if ( isLoopExit || isBreak )
            break;
        }

        if ( isLoopExit || isBreak )
          break;
      }

      mCGraph[ p.first ].isLoopExitEdge = isLoopExit;
      mCGraph[ p.first ].isBreakEdge = isBreak;

      DACTION(
        if ( isLoopExit ) {
          DOUT(
            "Edge " << mCGraph[ v1 ]->getID() << " ('" << b.getName() <<
            "') -> " << mCGraph[ v2 ]->getID() << " ('" << succ.getName() <<
            "') is a regular loop exit." << endl );
        }

        if ( isBreak ) {
          DOUT(
            "Edge " << mCGraph[ v1 ]->getID() << " ('" << b.getName() <<
            "') -> " << mCGraph[ v2 ]->getID() << " ('" << succ.getName() <<
            "') is a break." << endl ); } );
    }
  }
};


/*
  visualizeNodes dumps the control flow graph nodes into a given DOT file.
*/
void WIR_HierarchicalCFG::visualizeNodes( std::fstream &dotFile ) const
{
  DSTART( "void WIR_HierarchicalCFG::visualizeNodes(fstream&) const" );

  // Produce a legend in the visualization.
  dotFile << "subgraph clusterLegend {" << endl;

  dotFile << "legendBB[label=\"Basic Block\",shape=oval]" << endl << ";"
          << endl;
  dotFile << "legendBBEntry[label=\"Entry Node\",shape=oval,style=bold]"
          << endl << ";" << endl;

  dotFile << "legendRegion[label=\"Control Region\",shape=box,color=blue]"
          << endl << ";" << endl;

  dotFile << "legend1[shape=point]" << endl << ";" << endl;
  dotFile << "legend2[shape=point]" << endl << ";" << endl;
  dotFile << "legend1->legend2"
          << "[label=\"Back edge\",dir=forward,arrowhead=vee,arrowtail=none,"
          << "color=chartreuse4,style=bold]" << endl << ";" << endl;

  dotFile << "legend3[shape=point]" << endl << ";" << endl;
  dotFile << "legend4[shape=point]" << endl << ";" << endl;
  dotFile << "legend3->legend4"
          << "[label=\"Loop exit edge\",dir=forward,arrowhead=vee,arrowtail=none,"
          << "color=orange,style=bold]" << endl << ";" << endl;

  dotFile << "legend5[shape=point]" << endl << ";" << endl;
  dotFile << "legend6[shape=point]" << endl << ";" << endl;
  dotFile << "legend5->legend6"
          << "[label=\"Break edge\",dir=forward,arrowhead=vee,arrowtail=none,"
          << "color=red,style=bold]" << endl << ";" << endl;

  dotFile << "legendFun[label=\"";
  if ( mFunction.isInserted() )
    dotFile << mFunction.getCompilationUnit().getName() << "\\n";
  dotFile << mFunction.getName() << "\",color=white]" << endl << ";" << endl;

  dotFile << "}" << endl;

  // Visualize all graph nodes.
  BGL_FORALL_VERTICES( v, mCGraph, CGraph )
    mCGraph[ v ]->visualize( dotFile );
};


/*
  visualizeEdges dumps the control flow graph edges into a given DOT file.
*/
void WIR_HierarchicalCFG::visualizeEdges( std::fstream &dotFile ) const
{
  DSTART( "void WIR_HierarchicalCFG::visualizeEdges(fstream&) const" );

  // A small lambda to determine the CFG's basic block node containing WIR basic
  // block b.
  auto findBBNode = [&]( const WIR_BasicBlock &b ) {
    WIR_ControlTreeNode *res = nullptr;
    list<WIR_ControlTreeNode *> workList;
    workList.push_back( mCGraph[ mNodeByID.at( b.getID() ) ] );

    do {
      auto *n = workList.front();
      workList.pop_front();

      if ( ( n->getType() == WIR_CTNodeType::bb ) &&
           ( dynamic_cast<WIR_BasicBlockTreeNode *>( n )->getBasicBlock() ==
               b ) ) {
        res = n;
        workList.clear();
      } else
        for ( auto &c : n->getChilds() )
          workList.push_back( &(c.get()) );
    } while ( !workList.empty() );

    return( res );
  };

  // Traverse the current WIR function.
  for ( WIR_BasicBlock &b : mFunction ) {
    for ( WIR_BasicBlock &succ : b.getSuccessors() ) {
      DOUT(
        "Visualizing edge '" << b.getName() << "' (ID " << b.getID() <<
        ") -> '" << succ.getName() << "' (ID " << succ.getID() << ")." <<
        endl );

      dotFile << b.getID() << "->" << succ.getID()
              << "[dir=forward,arrowhead=vee,arrowtail=none,style=bold";

      // Determine the CFG basic block nodes containing b and succ.
      auto *src = findBBNode( b );
      auto *tgt = findBBNode( succ );

      // Check whether the edge b -> succ is marked as a regular loop exit or a
      // break.
      bool isLoopExit = false;
      bool isBreak = false;

      for ( auto rIt = b.getInstructions().rbegin();
            rIt != b.getInstructions().rend(); ++rIt ) {
        for ( WIR_Operation &o : rIt->get() ) {
          if ( o.isJump() &&
               o.containsContainers( WIR_LoopExit::getContainerTypeID() ) ) {
            auto &c = o.getContainers<WIR_LoopExit>().begin()->get();

            if ( c.explicitSuccessorIsExit() ) {
              // The loop exit is explicit target of the current jump o. So
              // let's check whether node succ is in o's jump targets.
              for ( WIR_BasicBlock &t : o.getJumpTargets() )
                if ( t == succ ) {
                  isLoopExit = true;
                  break;
                }
            } else {
              // The loop exit is implicit successor of the current jump o. So
              // let's ensure that node succ is not in o's jump targets.
              bool isSuccImplicit = true;

              for ( WIR_BasicBlock &t : o.getJumpTargets() )
                if ( t == succ ) {
                  isSuccImplicit = false;
                  break;
                }

              if ( isSuccImplicit )
                isLoopExit = true;
            }
          }

          if ( o.isJump() &&
               o.containsContainers( WIR_Break::getContainerTypeID() ) )
            for ( WIR_BasicBlock &t : o.getJumpTargets() )
              if ( t == succ ) {
                isBreak = true;
                break;
              }

          if ( isLoopExit || isBreak )
            break;
        }

        if ( isLoopExit || isBreak )
          break;
      }

      if ( isLoopExit ) {
        DOUT( "  Identified regular loop exit edge." << endl );
        dotFile << ",color=orange,style=bold]" << endl << ";" << endl;
        break;
      } else

      if ( isBreak ) {
        DOUT( "  Identified break edge." << endl );
        dotFile << ",color=red,style=bold]" << endl << ";" << endl;
        break;
      }

      // Determine common parent of b and succ in the hierarchical CFG, if any.
      WIR_ControlTreeNode *commonParent = nullptr;

      set<WIR_id_t> parents;
      auto *parent = src;
      parents.insert( parent->getID() );

      while ( parent->getID() != parent->getRoot().getID() ) {
        parent = &(parent->getParent());
        parents.insert( parent->getID() );
      }

      parent = tgt;
      while ( parent->getID() != parent->getRoot().getID() ) {
        if ( parents.count( parent->getID() ) ) {
          commonParent = parent;
          break;
        }
        parent = &(parent->getParent());
      }

      // Finally, determine back edges in a cyclic common parent and visualize
      // them differently.
      if ( commonParent && commonParent->isCyclic() ) {
        DOUT(
          "  Found cyclic common parent " << commonParent->getID() <<
          " for current edge." << endl );

        auto *c1 = src;
        while ( c1->getParent().getID() != commonParent->getID() )
          c1 = &(c1->getParent());

        auto *c2 = tgt;
        while ( c2->getParent().getID() != commonParent->getID() )
          c2 = &(c2->getParent());

        for ( auto &e : commonParent->getBackEdges() )
          if ( ( *(e.first) == *c1 ) && ( *(e.second) == *c2 ) ) {
            DOUT( "  Identified back edge." << endl );
            dotFile << ",color=chartreuse4,style=bold";
            break;
          }
      } else {
        // If there is no structured information present in the hierarchical
        // CFG, let's check the Boost graph itself.
        CGraphVertex v1 = mNodeByID.at( b.getID() );
        CGraphVertex v2 = mNodeByID.at( succ.getID() );

        auto p = boost::edge( v1, v2, mCGraph );

        if ( p.second && mCGraph[ p.first ].isBackEdge ) {
          // There is an edge present in the Boost graph, and it's a back edge.
          DOUT( "  Identified back edge." << endl );
          dotFile << ",color=chartreuse4,style=bold";
        }
      }

      dotFile << "]" << endl << ";" << endl;
    }
  }
};


/*
  Default constructor for CFG visitors.
*/
WIR_HierarchicalCFG::CFGVisitor::CFGVisitor( WIR_HCFGNodeSet &s,
                                             std::list<WIR_ControlTreeNode *> &p,
                                             std::set<CGraphEdge> &b,
                                             bool dryRun ) :
  mStartNodes { s },
  mPostOrder { p },
  mBackEdges { b },
  mDryRun { dryRun }
{
  DSTART(
    "WIR_HierarchicalCFG::CFGVisitor::CFGVisitor(WIR_HierarchicalCFG::WIR_HCFGNodeSet&, list<WIR_ControlTreeNode*>&, set<CGraphEdge>&)" );
};


void WIR_HierarchicalCFG::CFGVisitor::start_vertex( CGraphVertex v,
                                                    const CGraph &g ) const
{
  DSTART(
    "void WIR_HierarchicalCFG::CFGVisitor::start_vertex(WIR_HierarchicalCFG::CGraphVertex, const CGraph&) const" );

  DACTION(
    DOUT( "Start node with ID " << g[ v ]->getID() );
    if ( g[ v ]->getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" <<
        dynamic_cast<const WIR_BasicBlockTreeNode *>(
          g[ v ] )->getBasicBlock().getName() << ")" );
    DOUT( " detected." << endl ); );

  if ( !mDryRun )
    mStartNodes.insert( g[ v ] );
};


/*
  finish_vertex finishes a visited CFG node.
*/
void WIR_HierarchicalCFG::CFGVisitor::finish_vertex( CGraphVertex v,
                                                     const CGraph &g ) const
{
  DSTART(
    "void WIR_HierarchicalCFG::CFGVisitor::finish_vertex(WIR_HierarchicalCFG::CGraphVertex, const CGraph&) const" );

  DACTION(
    DOUT( "Finishing node with ID " << g[ v ]->getID() );
    if ( g[ v ]->getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" <<
        dynamic_cast<const WIR_BasicBlockTreeNode *>(
          g[ v ] )->getBasicBlock().getName() << ")" );
    DOUT( "." << endl ); );

  if ( !mDryRun )
    mPostOrder.push_back( g[ v ] );
};


/*
  examine_edge prints a currently visited edge.
*/
void WIR_HierarchicalCFG::CFGVisitor::examine_edge( CGraphEdge e,
                                                    const CGraph &g ) const
{
  (void) e;
  (void) g;

  DSTART(
    "void WIR_HierarchicalCFG::CFGVisitor::examine_edge(WIR_HierarchicalCFG::CGraphEdge, const CGraph&) const" );

  DACTION(
    CGraphVertex src = source( e, g );
    CGraphVertex tgt = target( e, g );

    DOUT( "Examining edge " << e << ": '" << g[ src ]->getID() << "' " );
    if ( g[ src ]->getType() == WIR_CTNodeType::bb )
      DOUT(
        "(" <<
        dynamic_cast<const WIR_BasicBlockTreeNode *>(
          g[ src ] )->getBasicBlock().getName() << ") " );

    DOUT( "-> '" << g[ tgt ]->getID() << "' " );
    if ( g[ tgt ]->getType() == WIR_CTNodeType::bb )
      DOUT(
        "(" <<
        dynamic_cast<const WIR_BasicBlockTreeNode *>(
          g[ tgt ] )->getBasicBlock().getName() << ")" );
    DOUT( "." << endl ); );
};


/*
  back_edge marks an identified back edge.
*/
void WIR_HierarchicalCFG::CFGVisitor::back_edge( CGraphEdge e,
                                                 const CGraph &g ) const
{
  (void) g;

  DSTART(
    "void WIR_HierarchicalCFG::CFGVisitor::back_edge(WIR_HierarchicalCFG::CGraphEdge, const CGraph&) const" );

  DACTION(
    CGraphVertex src = source( e, g );
    CGraphVertex tgt = target( e, g );

    DOUT( "Examining back edge '" << g[ src ]->getID() << "' " );
    if ( g[ src ]->getType() == WIR_CTNodeType::bb )
      DOUT(
        "(" <<
        dynamic_cast<const WIR_BasicBlockTreeNode *>(
          g[ src ] )->getBasicBlock().getName() << ") " );

    DOUT( "-> '" << g[ tgt ]->getID() << "' " );
    if ( g[ tgt ]->getType() == WIR_CTNodeType::bb )
      DOUT(
        "(" <<
        dynamic_cast<const WIR_BasicBlockTreeNode *>(
          g[ tgt ] )->getBasicBlock().getName() << ") " );
    DOUT( "(back)." << endl ); );

  if ( !mDryRun )
    mBackEdges.insert( e );
};

}       // namespace WIR
