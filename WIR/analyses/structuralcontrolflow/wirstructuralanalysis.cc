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
  @file wirstructuralanalysis.cc
  @brief This file implements the %WIR structural control flow analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <functional>
#include <list>
#include <set>

// Include boost headers
#include <boost/optional.hpp>
#include <boost/graph/iteration_macros.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>


//
// Preprocessor macros
//

#define DEBUGFUNCTION ""


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
WIR_StructuralAnalysis::WIR_StructuralAnalysis( WIR_Function &f, bool verbosity,
                                                bool keepTmpFiles ) :
  WIR_Analysis { f },
  WIR_HierarchicalCFG { f, verbosity, keepTmpFiles }
{
  DSTART(
    "WIR_StructuralAnalysis::WIR_StructuralAnalysis(WIR_Function&, bool, bool)" );
};


/*
  Destructor.
*/
WIR_StructuralAnalysis::~WIR_StructuralAnalysis( void )
{
  DSTART( "virtual WIR_StructuralAnalysis::~WIR_StructuralAnalysis()" );
};


//
// Protected class methods
//

/*
  runAnalysis performs structural control flow analysis of the given function.

  This method implements procedure Structural_Analysis() from S. S. Muchnick,
  page 206.
*/
void WIR_StructuralAnalysis::runAnalysis( WIR_Function &f )
{
  DSTART( "virtual void WIR_StructuralAnalysis::runAnalysis(WIR_Function&)" );

  (void) f;

  DOUT( "Processing function '" << f.getName() << "'." << endl );

  // Initialize data structures.
  init();
  dfs();

  {
    DSTART(
      "virtual void WIR_StructuralAnalysis::runAnalysis(WIR_Function&).init.visualize" );
    DACTION(
      if ( string( DEBUGFUNCTION ).empty() || f.getName() == DEBUGFUNCTION )
        visualize(); );
  }

  // Iteratively reduce the CFG to the start nodes only.
  while ( num_vertices( mCGraph ) > getStartNodes().size() ) {

    // Do DFS traversal.
    dfs();
    bool reductionFoundInCurrentTraversal = false;

    #ifdef FAILSAFEMODE
    auto numNodes = num_vertices( mCGraph );
    auto numStartNodes = getStartNodes().size();
    auto numBackEdges = getNumberOfBackEdges();
    #endif

    // Iterate the CFG nodes in post-order.
    auto it = getPostOrder().begin();

    while ( ( num_vertices( mCGraph ) > getStartNodes().size() ) &&
            ( it != getPostOrder().end() ) ) {
      auto &n = *(*it);
      WIR_HCFGNodeSet nodeSet;
      WIR_ControlTreeNode *newNode = nullptr;

      DACTION(
        DOUT( "  Analyzing CFG node " << n.getID() );
        if ( n.getType() == WIR_CTNodeType::bb )
          DOUT(
            " (" <<
            dynamic_cast<WIR_BasicBlockTreeNode &>( n ).getBasicBlock().getName() << ")" );
        DOUT( "." << endl ); );

      // Locate an acyclic region, if present.
      newNode = findAcyclicRegion( n, nodeSet );

      if ( newNode != nullptr )
        reduce( *newNode, nodeSet );
      else {
        // Locate a cyclic region, if present.
        WIR_HCFGNodeSet reachUnder;

        // Find the smallest cyclic node set starting at n.
        for ( auto e : mBackEdges ) {
          // Skip back edge e if it does not end in the currently considered
          // node n.
          if ( *(mCGraph[ target( e, mCGraph ) ]) != n )
            continue;

          WIR_HCFGNodeSet tmpSet { &n };

          // Consider all nodes m such that n can be reached from m via back
          // edge e without passing through n.
          BGL_FORALL_VERTICES( m, mCGraph, CGraph )
            if ( pathBack( m, mNodeByID.at( n.getID() ), e ) )
              tmpSet.insert( mCGraph[ m ] );

          DACTION(
            DOUT( "Current tmpSet = {" );
            for ( auto *p : tmpSet ) {
              if ( p != *(tmpSet.begin()) )
                DOUT( "," );
              DOUT( " " << p->getID() );
              if ( p->getType() == WIR_CTNodeType::bb )
                DOUT(
                  " (" <<
                  dynamic_cast<WIR_BasicBlockTreeNode *>(
                    p )->getBasicBlock().getName() << ")" );
            }
            DOUT( " }" << endl ); );

          // If tmpSet contains both source and target of back edge e and is
          // smaller than the current reachUnder set, keep it.
          if ( tmpSet.count( mCGraph[ source( e, mCGraph ) ] ) &&
               ( reachUnder.empty() || ( tmpSet.size() < reachUnder.size() ) ) )
            reachUnder = tmpSet;
        }

        newNode = findCyclicRegion( n, reachUnder );

        if ( newNode != nullptr )
          reduce( *newNode, reachUnder );
      }

      if ( newNode != nullptr ) {
        reductionFoundInCurrentTraversal = true;
        mCheckProperRegions = false;
        mIgnoreBreakEdges = false;

        // The control tree was modified. So, find the correct position in the
        // DFS post-order where to continue.
        dfs();

        it = getPostOrder().begin();
        while ( *it != newNode )
          ++it;
      }

      ++it;
    }

    if ( !reductionFoundInCurrentTraversal && !mCheckProperRegions &&
         !mIgnoreBreakEdges ) {
      // No reduction was found during the current postorder traversal. So,
      // let's check for acyclic proper regions only in the next traversal.
      mCheckProperRegions = true;
      continue;
    }

    if ( !reductionFoundInCurrentTraversal && mCheckProperRegions &&
         !mIgnoreBreakEdges ) {
      // No reduction was found during the current postorder traversal. So,
      // let's ignore break edges only in the next traversal.
      mCheckProperRegions = false;
      mIgnoreBreakEdges = true;
      continue;
    }

    {
      DSTART(
        "virtual void WIR_StructuralAnalysis::runAnalysis(WIR_Function&).visualize" );
      DACTION(
        if ( string( DEBUGFUNCTION ).empty() || f.getName() == DEBUGFUNCTION )
          visualize(); );
    }

    #ifdef FAILSAFEMODE
    ufAssertT(
      ( num_vertices( mCGraph ) == getStartNodes().size() ) ||
      ( num_vertices( mCGraph ) < numNodes ),
      "Failed to reduce control flow graph of function '" + f.getName() +
      "'." );
    ufAssertT(
      getStartNodes().size() == numStartNodes,
      "Illegal change of the number of CFG start nodes detected: previously " <<
      numStartNodes << ", now " << getStartNodes().size() << "." );
    ufAssertT(
      getNumberOfBackEdges() <= numBackEdges,
      "Illegal increase of the number of CFG back edges detected: " <<
      "previously " << numBackEdges << ", now " << getNumberOfBackEdges() <<
      "." );
    #endif
  }

  {
    DSTART(
      "virtual void WIR_StructuralAnalysis::runAnalysis(WIR_Function&).final.visualize" );
    DACTION(
      if ( string( DEBUGFUNCTION ).empty() || f.getName() == DEBUGFUNCTION )
        visualize(); );
  }

  createContainers();
};


//
// Private class methods
//

/*
  init initializes internal data structures by collecting information about
  predecessor/successor relations between basic blocks.
*/
void WIR_StructuralAnalysis::init( void )
{
  DSTART( "void WIR_StructuralAnalysis::init()" );

  for ( WIR_BasicBlock &b : mFunction )
    // Clear previous analysis results.
    b.eraseContainers( WIR_ControlTree::getContainerTypeID() );
  mFunction.eraseContainers( WIR_ControlTree::getContainerTypeID() );

  mStartNodes.clear();
  mIDomAnalysisDone = false;
  mCheckProperRegions = false;
  mIgnoreBreakEdges = false;
};


/*
  findAcyclicRegion determines whether the specified CFG node forms an acyclic
  control region.

  This method implements procedure Acyclic_Region_Type() from S. S. Muchnick,
  page 208.
*/
WIR_ControlTreeNode *WIR_StructuralAnalysis::findAcyclicRegion( const WIR_ControlTreeNode &node,
                                                                WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet )
{
  DSTART(
    "WIR_ControlTreeNode* WIR_StructuralAnalysis::findAcyclicRegion(const WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&)" );

  WIR_ControlTreeNode *res = nullptr;

  if ( !mCheckProperRegions ) {
    // Check for a Block containing 'node'.
    res = checkBlock( node, nSet );
    if ( res != nullptr )
      return( res );

    // Check for an If-then-else containing 'node'.
    res = checkIfThenElse( node, nSet );
    if ( res != nullptr )
      return( res );

    // Check for a cyclic If-then-else containing 'node'.
    res = checkIfElseSelfLoop( node, nSet );
    if ( res != nullptr )
      return( res );

    // Check for an If-then containing 'node'.
    res = checkIfThen( node, nSet );
    if ( res != nullptr )
      return( res );

    // Check for a Switch-case containing 'node'.
    res = checkSwitchCase( node, nSet );
    if ( res != nullptr )
      return( res );
  } else {
    // Check for a Proper region starting at 'node'.
    res = checkProper( node, nSet );
    if ( res != nullptr )
      return( res );
  }

  return( nullptr );
};


/*
  findCyclicRegion determines whether the specified CFG node forms a cyclic
  control region.

  This method implements procedure Cyclic_Region_Type() from S. S. Muchnick,
  page 208ff.
*/
WIR_ControlTreeNode *WIR_StructuralAnalysis::findCyclicRegion( const WIR_ControlTreeNode &node,
                                                               WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet )
{
  DSTART(
    "WIR_ControlTreeNode* WIR_StructuralAnalysis::findCyclicRegion(const WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&)" );

  // Check for a self-loop containing 'node'.
  WIR_ControlTreeNode *res = checkSelfLoop( node, nSet );
  if ( res != nullptr )
    return( res );

  // Check for an improper cyclic region containing 'node'.
  res = checkImproper( node, nSet );
  if ( res != nullptr )
    return( res );

  // Check for a regular for- or while-do loop containing 'node'.
  res = checkWhileLoop( node, nSet );
  if ( res != nullptr )
    return( res );

  // Check for a natural do-while loop containing 'node'.
  res = checkNaturalLoop( node, nSet );
  if ( res != nullptr )
    return( res );

  // Check for a natural loop containing 'node' resulting from tail recursion
  // elimination.
  res = checkRecursionLoop( node, nSet );
  if ( res != nullptr )
    return( res );

  return( nullptr );
};


/*
  checkBlock checks whether the specified CFG node forms a block region
  (S. S. Muchnick, Fig. 7.35, page 203).
*/
WIR_BlockTreeNode *WIR_StructuralAnalysis::checkBlock( const WIR_ControlTreeNode &node,
                                                       WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet ) const
{
  DSTART(
    "WIR_BlockTreeNode* WIR_StructuralAnalysis::checkBlock(const WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&) const" );

  // Try to extend a potential block region at its tail.
  CGraphVertex n = mNodeByID.at( node.getID() );
  nSet.clear();

  while ( true ) {
    CGraph::in_edge_iterator ieIt, ieEnd;
    CGraph::out_edge_iterator oeIt, oeEnd;
    bool nAdded = false;

    if ( n == mNodeByID.at( node.getID() ) ) {
      bool selfLoop = false;
      for ( boost::tie( oeIt, oeEnd ) = out_edges( n, mCGraph ); oeIt != oeEnd;
            ++oeIt )
        if ( target( *oeIt, mCGraph ) == n ) {
          selfLoop = true;
          break;
        }

      if ( !selfLoop ) {
        nSet.insert( mCGraph[ n ] );
        nAdded = true;

        DACTION(
          DOUT(
            "Extending block region at tail by node ID " <<
            mCGraph[ n ]->getID() );
          if ( mCGraph[ n ]->getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<WIR_BasicBlockTreeNode *>(
                mCGraph[ n ] )->getBasicBlock().getName() << ")" );
          DOUT( "." << endl ); );
      }
    } else

    if ( in_degree( n, mCGraph ) == 1 ) {
      bool isLoop = false;
      for ( boost::tie( oeIt, oeEnd ) = out_edges( n, mCGraph ); oeIt != oeEnd;
            ++oeIt )
        if ( nSet.count( mCGraph[ target( *oeIt, mCGraph ) ] ) ||
             mCGraph[ *oeIt ].isBackEdge ) {
          isLoop = true;
          break;
        }

      bool isBreak = false;
      for ( boost::tie( ieIt, ieEnd ) = in_edges( n, mCGraph ); ieIt != ieEnd;
            ++ieIt ) {
        if ( mCGraph[ *ieIt ].isBreakEdge ) {
          isBreak = true;
          break;
        }
      }

      if ( !isLoop &&
           ( !isBreak || mCGraph[ n ]->isAcyclic() || mIgnoreBreakEdges ) ) {
        nSet.insert( mCGraph[ n ] );
        nAdded = true;

        DACTION(
          DOUT(
            "Extending block region at tail by node ID " <<
            mCGraph[ n ]->getID() );
          if ( mCGraph[ n ]->getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<WIR_BasicBlockTreeNode *>(
                mCGraph[ n ] )->getBasicBlock().getName() << ")" );
          DOUT( "." << endl ); );
      }
    }

    if ( nAdded && ( out_degree( n, mCGraph ) == 1 ) &&
         !mCGraph[ *( out_edges( n, mCGraph ).first ) ].isBackEdge ) {
      auto it = out_edges( n, mCGraph ).first;
      n = target( *it, mCGraph );

      DACTION(
        DOUT(
          "Advancing n to node ID " << mCGraph[ n ]->getID() );
        if ( mCGraph[ n ]->getType() == WIR_CTNodeType::bb )
          DOUT(
            " (" <<
            dynamic_cast<WIR_BasicBlockTreeNode *>(
              mCGraph[ n ] )->getBasicBlock().getName() << ")" );
        DOUT( "." << endl ); );
    } else
      break;
  }

  // Try to extend a potential block region at its head.
  n = mNodeByID.at( node.getID() );
  WIR_ControlTreeNode *head = const_cast<WIR_ControlTreeNode *>( &node );

  while ( true ) {
    CGraph::in_edge_iterator ieIt, ieEnd;
    CGraph::out_edge_iterator oeIt, oeEnd;
    bool nAdded = false;

    if ( n == mNodeByID.at( node.getID() ) ) {
      bool selfLoop = false;
      for ( boost::tie( ieIt, ieEnd ) = in_edges( n, mCGraph ); ieIt != ieEnd;
            ++ieIt )
        if ( source( *ieIt, mCGraph ) == n ) {
          selfLoop = true;
          break;
        }

      if ( !selfLoop ) {
        nSet.insert( mCGraph[ n ] );
        nAdded = true;
        head = mCGraph[ n ];

        DACTION(
          DOUT(
            "Extending block region at head by node ID " <<
            mCGraph[ n ]->getID() );
          if ( mCGraph[ n ]->getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<WIR_BasicBlockTreeNode *>(
                mCGraph[ n ] )->getBasicBlock().getName() << ")" );
          DOUT( "." << endl ); );
      }
    } else

    if ( out_degree( n, mCGraph ) == 1 ) {
      bool isLoop = false;
      for ( boost::tie( ieIt, ieEnd ) = in_edges( n, mCGraph ); ieIt != ieEnd;
            ++ieIt )
        if ( nSet.count( mCGraph[ source( *ieIt, mCGraph ) ] ) ||
             mCGraph[ *ieIt ].isBackEdge ) {
          isLoop = true;
          break;
        }

      bool isBreak = false;
      for ( boost::tie( oeIt, oeEnd ) = out_edges( n, mCGraph ); oeIt != oeEnd;
            ++oeIt )
        if ( mCGraph[ *oeIt ].isBreakEdge ) {
          isBreak = true;
          break;
        }

      if ( !isLoop &&
           ( !isBreak || mCGraph[ n ]->isAcyclic() || mIgnoreBreakEdges ) ) {
        nSet.insert( mCGraph[ n ] );
        nAdded = true;
        head = mCGraph[ n ];

        DACTION(
          DOUT(
            "Extending block region at head by node ID " <<
            mCGraph[ n ]->getID() );
          if ( mCGraph[ n ]->getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<WIR_BasicBlockTreeNode *>(
                mCGraph[ n ] )->getBasicBlock().getName() << ")" );
          DOUT( "." << endl ); );
      }
    }

    if ( nAdded && ( in_degree( n, mCGraph ) == 1 ) &&
         !mCGraph[ *(in_edges( n, mCGraph ).first) ].isBackEdge ) {
      auto it = in_edges( n, mCGraph ).first;
      n = source( *it, mCGraph );

      DACTION(
        DOUT(
          "Advancing n to node ID " << mCGraph[ n ]->getID() );
        if ( mCGraph[ n ]->getType() == WIR_CTNodeType::bb )
          DOUT(
            " (" <<
            dynamic_cast<WIR_BasicBlockTreeNode *>(
              mCGraph[ n ] )->getBasicBlock().getName() << ")" );
        DOUT( "." << endl ); );
    } else
      break;
  }

  if ( nSet.size() > 1 ) {
    DOUT( "Found a block region." << endl );

    auto *newNode = new WIR_BlockTreeNode();

    for ( unsigned int i = 0; i < nSet.size(); ++i ) {
      if ( head->getType() == WIR_CTNodeType::block ) {
        // An existing block region shall be inserted into a new block region.
        // Merge them both.
        auto *b = dynamic_cast<WIR_BlockTreeNode *>( head );

        DOUT(
          "Merging existing block region " << b->getID() <<
          " into new block region " << newNode->getID() << "." << endl );

        // Shift all internal nodes from b to newNode.
        for ( WIR_ControlTreeNode &c : b->getBlockList() ) {
          DACTION(
            DOUT( "Moving node " << c.getID() );
            if ( c.getType() == WIR_CTNodeType::bb )
              DOUT(
                " (" <<
                dynamic_cast<const WIR_BasicBlockTreeNode &>(
                  c ).getBasicBlock().getName() << ")" );
            DOUT( " from old to new block region." << endl ); );
          newNode->pushBackBlockNode( c );
        }

        // Shift control tree hierarchy from b to newNode.
        for ( auto &ptr : b->mChildPointers ) {
          auto *p = ptr.release();
          newNode->insertChild( p );
        }
        b->mChildReferences.clear();
        b->mChildPointers.clear();
      } else
        newNode->pushBackBlockNode( *head );

      n = mNodeByID.at( head->getID() );
      auto it = out_edges( n, mCGraph ).first;
      n = target( *it, mCGraph );
      head = mCGraph[ n ];
    }

    return( newNode );
  } else
    return( nullptr );
};


/*
  checkIfThenElse checks whether the specified CFG node forms an if-then-else
  region (S. S. Muchnick, Fig. 7.35, page 203).
*/
WIR_IfThenElseTreeNode *WIR_StructuralAnalysis::checkIfThenElse( const WIR_ControlTreeNode &node,
                                                                 WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet ) const
{
  DSTART(
    "WIR_IfThenElseTreeNode* WIR_StructuralAnalysis::checkIfThenElse(const WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&) const" );

  CGraphVertex n = mNodeByID.at( node.getID() );
  nSet.clear();

  if ( out_degree( n, mCGraph ) == 2 ) {
    auto it = out_edges( n, mCGraph ).first;
    CGraphVertex m1 = target( *it, mCGraph );

    ++it;
    CGraphVertex m2 = target( *it, mCGraph );

    // Check for a classical if-then-else where both branches m1 and m2 lead to
    // one and the same successor.
    if ( ( out_degree( m1, mCGraph ) == 1 ) &&
         ( out_degree( m2, mCGraph ) == 1 ) &&
         ( in_degree( m1, mCGraph ) == 1 ) &&
         ( in_degree( m2, mCGraph ) == 1 ) ) {
      auto e1 = *(out_edges( m1, mCGraph ).first);
      CGraphVertex succm1 = target( e1, mCGraph );
      auto e2 = *(out_edges( m2, mCGraph ).first);
      CGraphVertex succm2 = target( e2, mCGraph );

      if ( ( *(mCGraph[ succm1 ]) == *(mCGraph[ succm2 ]) ) &&
           ( *(mCGraph[ succm1 ]) != node ) ) {
        // We don't cover if-then-elses here where the then- and else-branch
        // jump back to the entry node n. Such structures are cyclic by
        // construction and will be matched by an if-then-else node included in
        // a self-loop node within method checkIfElseSelfLoop.
        DACTION(
          DOUT(
            "Found an if-then-else region with condition node ID " <<
            node.getID() );
          if ( node.getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<const WIR_BasicBlockTreeNode &>(
                node ).getBasicBlock().getName() << ")" );
          DOUT( " and branches with IDs " << mCGraph[ m1 ]->getID() );
          if ( mCGraph[ m1 ]->getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<WIR_BasicBlockTreeNode *>(
                mCGraph[ m1 ] )->getBasicBlock().getName() << ")" );
          DOUT( " and " << mCGraph[ m2 ]->getID() );
          if ( mCGraph[ m2 ]->getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<WIR_BasicBlockTreeNode *>(
                mCGraph[ m2 ] )->getBasicBlock().getName() << ")" );
          DOUT( "." << endl ); );

        auto *newNode =
          new WIR_IfThenElseTreeNode(
            node, *(mCGraph[ m1 ]), *(mCGraph[ m2 ]) );

        nSet =
          { &(const_cast<WIR_ControlTreeNode &>( node ) ), mCGraph[ m1 ],
            mCGraph[ m2 ] };

        return( newNode );
      }
    } else

    // Check for a classical if-then-else where one branch m1 or m2 is a sink in
    // the CFG. In this case, we need to test if n -> m1 or n -> m2 are loop
    // exit or break edges, or whether there are edges m1 -> n or m2 -> n. In
    // any such case, this is not an if-then-else structure.
    if ( ( ( out_degree( m1, mCGraph ) == 0 ) ||
           ( out_degree( m2, mCGraph ) == 0 ) ) &&
         ( in_degree( m1, mCGraph ) == 1 ) &&
         ( in_degree( m2, mCGraph ) == 1 ) ) {
      CGraph::out_edge_iterator oeIt, oeEnd;

      bool regionSpansLoopExit = false;
      bool regionSpansLoopBreak = false;
      for ( boost::tie( oeIt, oeEnd ) = out_edges( n, mCGraph ); oeIt != oeEnd;
            ++oeIt ) {
        if ( mCGraph[ *oeIt ].isLoopExitEdge )
          regionSpansLoopExit = true;
        if ( mCGraph[ *oeIt ].isBreakEdge )
          regionSpansLoopBreak = true;
      }

      bool regionSpansBackEdge = false;
      for ( boost::tie( oeIt, oeEnd ) = out_edges( m1, mCGraph ); oeIt != oeEnd;
            ++oeIt )
        if ( target( *oeIt, mCGraph ) == n )
          regionSpansBackEdge = true;
      for ( boost::tie( oeIt, oeEnd ) = out_edges( m2, mCGraph ); oeIt != oeEnd;
            ++oeIt )
        if ( target( *oeIt, mCGraph ) == n )
          regionSpansBackEdge = true;

      if ( regionSpansLoopExit || regionSpansLoopBreak || regionSpansBackEdge )
        return( nullptr );

      DACTION(
        DOUT(
          "Found an if-then-else region with condition node ID " <<
          node.getID() );
        if ( node.getType() == WIR_CTNodeType::bb )
          DOUT(
            " (" <<
            dynamic_cast<const WIR_BasicBlockTreeNode &>(
              node ).getBasicBlock().getName() << ")" );
        DOUT( " and branches with IDs " << mCGraph[ m1 ]->getID() );
        if ( mCGraph[ m1 ]->getType() == WIR_CTNodeType::bb )
          DOUT(
            " (" <<
            dynamic_cast<WIR_BasicBlockTreeNode *>(
              mCGraph[ m1 ] )->getBasicBlock().getName() << ")" );
        DOUT( " and " << mCGraph[ m2 ]->getID() );
        if ( mCGraph[ m2 ]->getType() == WIR_CTNodeType::bb )
          DOUT(
            " (" <<
            dynamic_cast<WIR_BasicBlockTreeNode *>(
              mCGraph[ m2 ] )->getBasicBlock().getName() << ")" );
        DOUT( "." << endl ); );

      auto *newNode =
        new WIR_IfThenElseTreeNode( node, *(mCGraph[ m1 ]), *(mCGraph[ m2 ]) );

      nSet =
        { &(const_cast<WIR_ControlTreeNode &>( node ) ), mCGraph[ m1 ],
          mCGraph[ m2 ] };

      return( newNode );
    }
  }

  return( nullptr );
};


/*
  brief checkIfElseSelfLoop checks whether the specified CFG node forms an
  if-then-else region simultaneously being a self-loop.
*/
WIR_SelfLoopTreeNode *WIR_StructuralAnalysis::checkIfElseSelfLoop( const WIR_ControlTreeNode &node,
                                                                   WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet )
{
  DSTART(
    "WIR_SelfLoopTreeNode* WIR_StructuralAnalysis::checkIfElseSelfLoop(const WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&)" );

  CGraphVertex n = mNodeByID.at( node.getID() );
  nSet.clear();

  if ( out_degree( n, mCGraph ) == 2 ) {
    auto it = out_edges( n, mCGraph ).first;
    CGraphVertex m1 = target( *it, mCGraph );

    ++it;
    CGraphVertex m2 = target( *it, mCGraph );

    // Check for a classical if-then-else where both branches m1 and m2 lead to
    // one and the same successor.
    if ( ( out_degree( m1, mCGraph ) == 1 ) &&
         ( out_degree( m2, mCGraph ) == 1 ) &&
         ( in_degree( m1, mCGraph ) == 1 ) &&
         ( in_degree( m2, mCGraph ) == 1 ) ) {
      auto e1 = *(out_edges( m1, mCGraph ).first);
      CGraphVertex succm1 = target( e1, mCGraph );
      auto e2 = *(out_edges( m2, mCGraph ).first);
      CGraphVertex succm2 = target( e2, mCGraph );

      if ( ( *(mCGraph[ succm1 ]) == *(mCGraph[ succm2 ]) ) &&
           ( *(mCGraph[ succm1 ]) == node ) ) {
        DACTION(
          DOUT(
            "Found a cyclic if-then-else region with condition node ID " <<
            node.getID() );
          if ( node.getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<const WIR_BasicBlockTreeNode &>(
                node ).getBasicBlock().getName() << ")" );
          DOUT( " and branches with IDs " << mCGraph[ m1 ]->getID() );
          if ( mCGraph[ m1 ]->getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<WIR_BasicBlockTreeNode *>(
                mCGraph[ m1 ] )->getBasicBlock().getName() << ")" );
          DOUT( " and " << mCGraph[ m2 ]->getID() );
          if ( mCGraph[ m2 ]->getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<WIR_BasicBlockTreeNode *>(
                mCGraph[ m2 ] )->getBasicBlock().getName() << ")" );
          DOUT( "." << endl ); );

        auto *iteNode =
          new WIR_IfThenElseTreeNode(
            node, *(mCGraph[ m1 ]), *(mCGraph[ m2 ]) );

        nSet =
          { &(const_cast<WIR_ControlTreeNode &>( node ) ), mCGraph[ m1 ],
            mCGraph[ m2 ] };

        // Reduce the found if-then-else first.
        auto id = reduce( *iteNode, nSet );
        auto v = mNodeByID[ id ];

        nSet.clear();
        nSet = { mCGraph[ v ] };

        return( new WIR_SelfLoopTreeNode( *mCGraph[ v ]) );
      }
    }
  }

  return( nullptr );
};


/*
  checkIfThen checks whether the specified CFG node forms an if-then region
  (S. S. Muchnick, Fig. 7.35, page 203).
*/
WIR_IfThenTreeNode *WIR_StructuralAnalysis::checkIfThen( const WIR_ControlTreeNode &node,
                                                         WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet ) const
{
  DSTART(
    "WIR_IfThenTreeNode* WIR_StructuralAnalysis::checkIfThen(const WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&) const" );

  CGraphVertex n = mNodeByID.at( node.getID() );
  nSet.clear();

  if ( out_degree( n, mCGraph ) == 2 ) {
    auto it = out_edges( n, mCGraph ).first;
    auto succn1 = target( *it, mCGraph );
    ++it;
    auto succn2 = target( *it, mCGraph );

    auto thenNode = n;
    auto exitNode = n;

    if ( ( out_degree( succn1, mCGraph ) == 1 ) &&
         ( in_degree( succn1, mCGraph ) == 1 ) &&
         ( in_degree( succn2, mCGraph ) >= 2 ) ) {
      thenNode = succn1;
      exitNode = succn2;
    } else

    if ( ( out_degree( succn2, mCGraph ) == 1 ) &&
         ( in_degree( succn2, mCGraph ) == 1 ) &&
         ( in_degree( succn1, mCGraph ) >= 2 ) ) {
      thenNode = succn2;
      exitNode = succn1;
    }

    if ( ( thenNode == n ) || ( exitNode == n ) )
      return( nullptr );

    it = out_edges( thenNode, mCGraph ).first;
    auto succThenNode = target( *it, mCGraph );

    if ( succThenNode == exitNode ) {
      DACTION(
        DOUT(
          "Found an if-then region with condition node ID " << node.getID() );
        if ( node.getType() == WIR_CTNodeType::bb )
          DOUT(
            " (" <<
            dynamic_cast<const WIR_BasicBlockTreeNode &>( node ).getBasicBlock().getName() << ")" );
        DOUT( " and then-branch with ID " << mCGraph[ thenNode ]->getID() );
        if ( mCGraph[ thenNode ]->getType() == WIR_CTNodeType::bb )
          DOUT(
            " (" <<
            dynamic_cast<WIR_BasicBlockTreeNode *>( mCGraph[ thenNode ] )->getBasicBlock().getName() << ")" );
        DOUT( "." << endl ); );

      auto *newNode =
        new WIR_IfThenTreeNode( node, *(mCGraph[ thenNode ]) );

      nSet =
        { &(const_cast<WIR_ControlTreeNode &>( node ) ), mCGraph[ thenNode ] };

      return( newNode );
    }
  }

  return( nullptr );
};


/*
  checkSwitchCase checks whether the specified CFG node forms a switch-case
  region (S. S. Muchnick, Fig. 7.35, page 203).
*/
WIR_SwitchCaseTreeNode *WIR_StructuralAnalysis::checkSwitchCase( const WIR_ControlTreeNode &node,
                                                                 WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet ) const
{
  DSTART(
    "WIR_SwitchCaseTreeNode* WIR_StructuralAnalysis::checkSwitchCase(const WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&) const" );

  CGraphVertex n = mNodeByID.at( node.getID() );
  nSet.clear();

  if ( out_degree( n, mCGraph ) <= 1 )
    return( nullptr );

  // Collect all successors of the current node, i.e., all cases of a switch.
  set<CGraphVertex> caseNodes;

  CGraph::out_edge_iterator oeIt, oeEnd;
  for ( boost::tie( oeIt, oeEnd ) = out_edges( n, mCGraph ); oeIt != oeEnd;
        ++oeIt )
    if ( mCGraph[ *oeIt ].isBackEdge )
      // A switch must not contain back edges.
      return( nullptr );
    else
      caseNodes.insert( target( *oeIt, mCGraph ) );

  // Check CFG structure of all case nodes.
  set<CGraphVertex> exitNodes;
  set<CGraphVertex> fallthroughNodes;

  for ( auto v : caseNodes ) {
    // All case nodes must have n as only predecessor, or some other case node
    // if there is a fall-through.
    if ( in_degree( v, mCGraph ) > 2 )
      return( nullptr );
    if ( in_degree( v, mCGraph ) == 2 ) {
      CGraph::in_edge_iterator ieIt, ieEnd;
      for ( boost::tie( ieIt, ieEnd ) = in_edges( v, mCGraph ); ieIt != ieEnd;
            ++ieIt ) {
        auto pred = source( *ieIt, mCGraph );
        if ( ( pred != n ) && !caseNodes.count( pred ) )
          return( nullptr );
      }
    }

    // All case nodes must have a single unique successor, or some other case
    // node if there is a fall-through. Also, a node without any successor (i.e.
    // a function's exit) cannot be a proper case node.
    if ( ( out_degree( v, mCGraph ) > 2 ) || ( out_degree( v, mCGraph ) == 0 ) )
      return( nullptr );

    for ( boost::tie( oeIt, oeEnd ) = out_edges( v, mCGraph ); oeIt != oeEnd;
          ++oeIt ) {
      if ( mCGraph[ *oeIt ].isBackEdge )
        // A switch must not contain back edges.
        return( nullptr );

      auto succ = target( *oeIt, mCGraph );
      if ( caseNodes.count( succ ) )
        fallthroughNodes.insert( v );
      else
        exitNodes.insert( succ );

      if ( exitNodes.size() > 1 )
        return( nullptr );
    }
  }

  // Determine the physical order of the case nodes according to the order of
  // their basic blocks in the WIR code.
  map<WIR_id_t, CGraphVertex> bbToCaseNode;
  for ( auto v : caseNodes )
    for ( WIR_BasicBlock &b : mCGraph[ v ]->getBasicBlocks() )
      bbToCaseNode[ b.getID() ] = v;

  list<CGraphVertex> orderedCaseNodes;
  boost::optional<CGraphVertex> currentCase;
  for ( WIR_BasicBlock &b : mFunction ) {
    if ( !bbToCaseNode.count( b.getID() ) )
      currentCase = boost::none;
    else {
      if ( ( currentCase == boost::none ) ||
           ( currentCase.get() != bbToCaseNode[ b.getID() ] ) ) {
        currentCase = bbToCaseNode[ b.getID() ];

        auto lastCaseIt = orderedCaseNodes.rbegin();
        if ( ( lastCaseIt == orderedCaseNodes.rend() ) ||
             ( *lastCaseIt != currentCase.get() ) )
          orderedCaseNodes.push_back( currentCase.get() );
      }
    }
  }

  // For all fall-through cases ensure that their basic blocks are really
  // physically following each other in the WIR code.
  for ( auto it = orderedCaseNodes.begin(); it != orderedCaseNodes.end();
        ++it ) {
    auto v = *it;

    if ( fallthroughNodes.count( v ) ) {
      // Determine the case to that a fall-through was determined.
      for ( boost::tie( oeIt, oeEnd ) = out_edges( v, mCGraph ); oeIt != oeEnd;
            ++oeIt ) {
        auto succ = target( *oeIt, mCGraph );
        if ( caseNodes.count( succ ) ) {
          auto it1 = it;
          ++it1;

          if ( ( it1 == orderedCaseNodes.end() ) || ( *it1 != succ ) )
            return( nullptr );
        }
      }
    }
  }

  // All checks are passed, create a new switch-case node.
  DACTION(
    DOUT(
      "Found a switch-case region with condition node ID " << node.getID() );
    if ( node.getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" <<
        dynamic_cast<const WIR_BasicBlockTreeNode &>( node ).getBasicBlock().getName() << ")" );
    DOUT( "." << endl ); );

  auto *newNode =
    new WIR_SwitchCaseTreeNode( node );
  nSet = { &(const_cast<WIR_ControlTreeNode &>( node ) ) };

  for ( auto v : orderedCaseNodes ) {
    if ( fallthroughNodes.count ( v ) )
      newNode->pushBackFallthroughCaseNode( *(mCGraph[ v ]) );
    else
      newNode->pushBackCaseNode( *(mCGraph[ v ] ) );

    nSet.insert( mCGraph[ v ] );
  }

  return( newNode );
};


/*
  checkProper checks whether the specified CFG node forms a proper acyclic
  region (S. S. Muchnick, Fig. 7.37, page 204).
*/
WIR_ProperTreeNode *WIR_StructuralAnalysis::checkProper( const WIR_ControlTreeNode &node,
                                                         WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet )
{
  DSTART(
    "WIR_ProperTreeNode* WIR_StructuralAnalysis::checkProper(const WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&)" );

  nSet.clear();
  bool properRegionFound = testProper( node, nSet );

  if ( !properRegionFound )
    return( nullptr );

  DACTION(
    DOUT(
      "Found a proper acyclic region with start node ID " << node.getID() );
    if ( node.getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" <<
        dynamic_cast<const WIR_BasicBlockTreeNode &>(
          node ).getBasicBlock().getName() << ")" );
    DOUT( "." << endl ); );

  auto *newNode =
    ( node.getType() != WIR_CTNodeType::proper ) ?
        new WIR_ProperTreeNode( node ) :
        new WIR_ProperTreeNode( node.getEntry() );

  for ( auto *v : nSet )
    if ( v->getType() == WIR_CTNodeType::proper ) {
      // An existing proper region shall be inserted into a new proper region.
      // Merge them both.
      WIR_ProperTreeNode *pr = dynamic_cast<WIR_ProperTreeNode *>( v );

      DOUT(
        "Merging existing proper region " << pr->getID() <<
        " into new proper region " << newNode->getID() << "." << endl );

      // Shift all internal nodes and edges from v to newNode.
      for ( WIR_ControlTreeNode &c : pr->getNodes() ) {
        DACTION(
          DOUT( "Moving node " << c.getID() );
          if ( c.getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<const WIR_BasicBlockTreeNode &>(
                c ).getBasicBlock().getName() << ")" );
          DOUT( " from old to new proper region." << endl ); );
        newNode->insertNode( c );
      }

      for ( auto &e : pr->mEdges ) {
        DACTION(
          DOUT( "Moving edge " << (*(e.first)).getID() );
          if ( (*(e.first)).getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<const WIR_BasicBlockTreeNode &>(
                (*(e.first)) ).getBasicBlock().getName() << ")" );
          DOUT( " -> " << (*(e.second)).getID() );
          if ( (*(e.second)).getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<const WIR_BasicBlockTreeNode &>(
                (*(e.second)) ).getBasicBlock().getName() << ")" );
          DOUT( " from old to new proper region." << endl ); );
        newNode->insertEdge( *(e.first), *(e.second) );
      }

      // Insert edges from nSet to v into newNode as (n -> v.getEntry() ).
      auto t = mNodeByID.at( v->getID() );
      CGraph::in_edge_iterator ieIt, ieEnd;
      for ( boost::tie( ieIt, ieEnd ) = in_edges( t, mCGraph );
            ieIt != ieEnd; ++ieIt )
        if ( nSet.count( mCGraph[ source( *ieIt, mCGraph ) ] ) ) {
          auto *src = mCGraph[ source( *ieIt, mCGraph ) ];
          DACTION(
            DOUT( "Inserting edge " << src->getID() );
            if ( src->getType() == WIR_CTNodeType::bb )
              DOUT(
                " (" <<
                dynamic_cast<const WIR_BasicBlockTreeNode *>(
                  src )->getBasicBlock().getName() << ")" );
            DOUT( " -> " << pr->getEntry().getID() );
            if ( pr->getEntry().getType() == WIR_CTNodeType::bb )
              DOUT(
                " (" <<
                dynamic_cast<const WIR_BasicBlockTreeNode &>(
                  pr->getEntry() ).getBasicBlock().getName() << ")" );
            DOUT( " to new proper region." << endl ); );
          newNode->insertEdge( *src, pr->getEntry() );
        }

      // Insert edges from childs of v to nSet into newNode.
      for ( WIR_BasicBlock &b : pr->getBasicBlocks() ) {
        for ( WIR_BasicBlock &succ : b.getSuccessors() ) {
          CGraphVertex succNode = mNodeByID.at( succ.getID() );
          auto *succCTNode = mCGraph[ succNode ];

          if ( nSet.count( succCTNode ) && ( *succCTNode != *v ) ) {
            for ( WIR_ControlTreeNode &c : pr->getNodes() )
              if ( c.getBasicBlocks().count( b ) ) {
                DACTION(
                  DOUT( "Inserting edge " << c.getID() );
                  if ( c.getType() == WIR_CTNodeType::bb )
                    DOUT(
                      " (" <<
                      dynamic_cast<const WIR_BasicBlockTreeNode &>(
                        c ).getBasicBlock().getName() << ")" );
                  DOUT( " -> " << succCTNode->getEntry().getID() );
                  if ( succCTNode->getEntry().getType() ==
                          WIR_CTNodeType::bb )
                    DOUT(
                      " (" <<
                      dynamic_cast<const WIR_BasicBlockTreeNode &>(
                        succCTNode->getEntry() ).getBasicBlock().getName() <<
                        ")" );
                  DOUT( " to new proper region." << endl ); );
                newNode->insertEdge( c, succCTNode->getEntry() );
              }
          }
        }
      }

      // Shift control tree hierarchy from v to newNode.
      for ( auto &ptr : pr->mChildPointers ) {
        DACTION(
          DOUT( "Moving hierarchical child " << ptr.get()->getID() );
          if ( ptr.get()->getType() == WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<const WIR_BasicBlockTreeNode *>(
                ptr.get() )->getBasicBlock().getName() << ")" );
          DOUT( " from old to new proper region." << endl ); );
        auto *p = ptr.release();
        newNode->insertChild( p );
      }
      pr->mChildReferences.clear();
      pr->mChildPointers.clear();
    } else {
      newNode->insertNode( *v );

      auto s = mNodeByID.at( v->getID() );
      CGraph::out_edge_iterator oeIt, oeEnd;
      for ( boost::tie( oeIt, oeEnd ) = out_edges( s, mCGraph );
            oeIt != oeEnd; ++oeIt )
        if ( nSet.count( mCGraph[ target( *oeIt, mCGraph ) ] ) &&
              ( mCGraph[ target( *oeIt, mCGraph ) ]->getType() !=
                  WIR_CTNodeType::proper ) ) {
          DACTION(
            DOUT( "Inserting edge " << v->getID() );
            if ( v->getType() == WIR_CTNodeType::bb )
              DOUT(
                " (" <<
                dynamic_cast<const WIR_BasicBlockTreeNode *>(
                  v )->getBasicBlock().getName() << ")" );
            DOUT( " -> " << mCGraph[ target( *oeIt, mCGraph ) ]->getID() );
            if ( mCGraph[ target( *oeIt, mCGraph ) ]->getType() ==
                    WIR_CTNodeType::bb )
              DOUT(
                " (" <<
                dynamic_cast<const WIR_BasicBlockTreeNode *>(
                  mCGraph[ target( *oeIt, mCGraph ) ] )->getBasicBlock().getName() <<
                  ")" );
            DOUT( " to new proper region." << endl ); );
          newNode->insertEdge( *v, *(mCGraph[ target( *oeIt, mCGraph ) ]) );
        }
    }

  return( newNode );
};


/*
  checkSelfLoop checks whether the specified CFG node forms a self-loop (S. S.
  Muchnick, Fig. 7.36, page 203).
*/
WIR_SelfLoopTreeNode *WIR_StructuralAnalysis::checkSelfLoop( const WIR_ControlTreeNode &node,
                                                             WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet ) const
{
  DSTART(
    "WIR_SelfLoopTreeNode* WIR_StructuralAnalysis::checkSelfLoop(const WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&) const" );

  if ( nSet.size() != 1 )
    return( nullptr );

  CGraphVertex n = mNodeByID.at( node.getID() );
  bool selfLoop = false;
  CGraph::out_edge_iterator oeIt, oeEnd;

  for ( boost::tie( oeIt, oeEnd ) = out_edges( n, mCGraph ); oeIt != oeEnd;
        ++oeIt )
    if ( target( *oeIt, mCGraph ) == n )
      selfLoop = true;

  if ( !selfLoop )
    return( nullptr );

  DACTION(
    DOUT(
      "Found a self-loop with start node ID " << node.getID() );
    if ( node.getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" <<
        dynamic_cast<const WIR_BasicBlockTreeNode &>( node ).getBasicBlock().getName() << ")" );
    DOUT( "." << endl ); );

  return( new WIR_SelfLoopTreeNode( node ) );
};


/*
  checkImproper checks whether the specified CFG node forms an improper cyclic
  region (S. S. Muchnick, Fig. 7.36, page 203).
*/
WIR_ImproperTreeNode *WIR_StructuralAnalysis::checkImproper( const WIR_ControlTreeNode &node,
                                                             WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet )
{
  DSTART(
    "WIR_ImproperTreeNode* WIR_StructuralAnalysis::checkImproper(const WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&)" );

  // If nSet contains a non-descendant m of node, then the region is improper
  // (cf. S. S. Muchnick, page 205).

  // Determine all descendants of node.
  set<WIR_id_t> descendants;
  set<CGraphVertex> workSet { mNodeByID.at( node.getID() ) };

  while ( !workSet.empty() ) {
    auto v = *(workSet.begin());
    workSet.erase( workSet.begin() );
    descendants.insert( mCGraph[ v ]->getID() );

    CGraph::out_edge_iterator oeIt, oeEnd;
    for ( boost::tie( oeIt, oeEnd ) = out_edges( v, mCGraph ); oeIt != oeEnd;
          ++oeIt )
      if ( !descendants.count( mCGraph[ target( *oeIt, mCGraph ) ]->getID() ) )
        workSet.insert( target( *oeIt, mCGraph ) );
  }

  // Check for a non-descendant of node.
  bool nonDescendantFound = false;
  for ( auto *m : nSet ) {
    if ( !descendants.count( m->getID() ) ) {
      nonDescendantFound = true;
      break;
    }
  }

  if ( !nonDescendantFound )
    return( nullptr );

  auto &entry = minimizeImproper( node, nSet );

  DACTION(
    DOUT(
      "Found an improper cyclic region with start node ID " << entry.getID() );
    if ( entry.getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" <<
        dynamic_cast<const WIR_BasicBlockTreeNode &>(
          entry ).getBasicBlock().getName() << ")" );
    DOUT( "." << endl ); );

  auto *newNode = new WIR_ImproperTreeNode( entry );

  for ( auto *v : nSet ) {
    newNode->insertNode( *v );

    auto s = mNodeByID.at( v->getID() );
    CGraph::out_edge_iterator oeIt, oeEnd;
    for ( boost::tie( oeIt, oeEnd ) = out_edges( s, mCGraph ); oeIt != oeEnd;
          ++oeIt )
      if ( nSet.count( mCGraph[ target( *oeIt, mCGraph ) ] ) ) {
        if ( mCGraph[ *oeIt ].isBackEdge )
          newNode->insertBackEdge( *v, *(mCGraph[ target( *oeIt, mCGraph ) ]) );
        else
          newNode->insertEdge( *v, *(mCGraph[ target( *oeIt, mCGraph ) ]) );
      }
  }

  return( newNode );
};


/*
  checkWhileLoop checks whether the specified CFG node forms a regular for- or
  while-do loop (S. S. Muchnick, Fig. 7.36, page 203).
*/
WIR_WhileLoopTreeNode *WIR_StructuralAnalysis::checkWhileLoop( const WIR_ControlTreeNode &node,
                                                               WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet )
{
  DSTART(
    "WIR_WhileLoopTreeNode* WIR_StructuralAnalysis::checkWhileLoop(const WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&)" );

  auto n = mNodeByID.at( node.getID() );
  auto m = n;

  DACTION(
    DOUT( "Current node n = " << mCGraph[ n ]->getID() );
    if ( mCGraph[ n ]->getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" << dynamic_cast<WIR_BasicBlockTreeNode *>(
          mCGraph[ n ] )->getBasicBlock().getName() << ")" );
    DOUT( endl << "Current nSet = {" );
    for ( auto *p : nSet ) {
      if ( p != *(nSet.begin()) )
        DOUT( "," );
      DOUT( " " << p->getID() );
      if ( p->getType() == WIR_CTNodeType::bb )
        DOUT(
          " (" <<
          dynamic_cast<WIR_BasicBlockTreeNode *>(
            p )->getBasicBlock().getName() << ")" );
    }
    DOUT( " }" << endl ); );

  // Determine the back edge leading from m to n.
  unsigned int nIncomingBackEdges = 0;

  CGraph::in_edge_iterator ieIt, ieEnd;
  for ( boost::tie( ieIt, ieEnd ) = in_edges( n, mCGraph ); ieIt != ieEnd;
        ++ieIt )
    if ( mCGraph[ *ieIt ].isBackEdge ) {
      ++nIncomingBackEdges;
      auto src = source( *ieIt, mCGraph );

      if ( nSet.count( mCGraph[ src ] ) )
        m = src;
    }

  if ( m == n )
    // No back edge found.
    return( nullptr );

  DACTION(
    DOUT( "Found node that is target of a back-edge." << endl );
    DOUT( "Start of back-edge m = " << mCGraph[ m ]->getID() );
    if ( mCGraph[ m ]->getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" << dynamic_cast<WIR_BasicBlockTreeNode *>(
          mCGraph[ m ] )->getBasicBlock().getName() << ")" );
    DOUT( endl ); );

  // Determine the number of back and break edges leaving the potential loop
  // body m.
  unsigned int mOutgoingBackEdges = 0;
  unsigned int mOutgoingBreakEdges = 0;

  CGraph::out_edge_iterator oeIt, oeEnd;
  for ( boost::tie( oeIt, oeEnd ) = out_edges( m, mCGraph ); oeIt != oeEnd;
        ++oeIt ) {
    if ( mCGraph[ *oeIt ].isBackEdge )
      ++mOutgoingBackEdges;
    if ( mCGraph[ *oeIt ].isBreakEdge )
      ++mOutgoingBreakEdges;
  }

  DOUT(
    "Out-degree of m = " << out_degree( m, mCGraph ) << endl <<
    "Back edges leaving m = " << mOutgoingBackEdges << endl <<
    "Break edges leaving m = " << mOutgoingBreakEdges << endl );

  if ( ( out_degree( n, mCGraph ) != 2 ) || ( nIncomingBackEdges != 1 ) ||
       ( mOutgoingBackEdges != 1 ) || ( in_degree( m, mCGraph ) != 1 ) ||
       ( nSet.size() != 2 ) ||
       ( out_degree( m, mCGraph ) !=
           mOutgoingBackEdges + mOutgoingBreakEdges ) )
    // Not a regular for- or while-do loop.
    return( nullptr );

  // If nSet includes back edges that do not lead to n, it's again not a natural
  // loop.
  for ( auto *v : nSet ) {
    auto s = mNodeByID.at( v->getID() );
    for ( boost::tie( oeIt, oeEnd ) = out_edges( s, mCGraph ); oeIt != oeEnd;
          ++oeIt )
      if ( nSet.count( mCGraph[ target( *oeIt, mCGraph ) ] ) &&
           mCGraph[ *oeIt ].isBackEdge &&
           ( *(mCGraph[ target( *oeIt, mCGraph ) ]) != node ) )
        return( nullptr );
  }

  DACTION(
    DOUT( "Found a for/while-loop with start node ID " << node.getID() );
    if ( node.getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" <<
        dynamic_cast<const WIR_BasicBlockTreeNode &>(
          node ).getBasicBlock().getName() << ")" );
    DOUT( "." << endl ); );

  auto *newNode = new WIR_WhileLoopTreeNode( node );

  for ( auto *v : nSet ) {
    newNode->insertNode( *v );

    auto s = mNodeByID.at( v->getID() );
    CGraph::out_edge_iterator oeIt, oeEnd;
    for ( boost::tie( oeIt, oeEnd ) = out_edges( s, mCGraph ); oeIt != oeEnd;
          ++oeIt )
      if ( nSet.count( mCGraph[ target( *oeIt, mCGraph ) ] ) ) {
        if ( mCGraph[ *oeIt ].isBackEdge )
          newNode->insertBackEdge( *v, *(mCGraph[ target( *oeIt, mCGraph ) ]) );
        else
          newNode->insertEdge( *v, *(mCGraph[ target( *oeIt, mCGraph ) ]) );
      }
  }

  return( newNode );
};


/*
  checkNaturalLoop checks whether the specified CFG node forms a natural
  do-while loop (S. S. Muchnick, Fig. 7.36, page 203).
*/
WIR_NaturalLoopTreeNode *WIR_StructuralAnalysis::checkNaturalLoop( const WIR_ControlTreeNode &node,
                                                                   WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet )
{
  DSTART(
    "WIR_NaturalLoopTreeNode* WIR_StructuralAnalysis::checkNaturalLoop(const WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&)" );

  auto n = mNodeByID.at( node.getID() );
  auto m = n;

  DACTION(
    DOUT( "Current node n = " << mCGraph[ n ]->getID() );
    if ( mCGraph[ n ]->getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" << dynamic_cast<WIR_BasicBlockTreeNode *>(
          mCGraph[ n ] )->getBasicBlock().getName() << ")" );
    DOUT( endl << "Current nSet = {" );
    for ( auto *p : nSet ) {
      if ( p != *(nSet.begin()) )
        DOUT( "," );
      DOUT( " " << p->getID() );
      if ( p->getType() == WIR_CTNodeType::bb )
        DOUT(
          " (" <<
          dynamic_cast<WIR_BasicBlockTreeNode *>(
            p )->getBasicBlock().getName() << ")" );
    }
    DOUT( " }" << endl ); );

  // Determine the back edge leading from m to n.
  CGraph::in_edge_iterator ieIt, ieEnd;
  for ( boost::tie( ieIt, ieEnd ) = in_edges( n, mCGraph ); ieIt != ieEnd;
        ++ieIt )
    if ( mCGraph[ *ieIt ].isBackEdge ) {
      m = source( *ieIt, mCGraph );
      break;
    }

  if ( m == n )
    // No back edge found.
    return( nullptr );

  DACTION(
    DOUT( "Found node that is target of a back-edge." << endl );
    DOUT( "Start of back-edge m = " << mCGraph[ m ]->getID() );
    if ( mCGraph[ m ]->getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" << dynamic_cast<WIR_BasicBlockTreeNode *>(
          mCGraph[ m ] )->getBasicBlock().getName() << ")" );
    DOUT( endl ); );

  // Determine the number of back and break edges leaving the potential loop
  // exit block m.
  unsigned int backEdges = 0;
  unsigned int breakEdges = 0;

  CGraph::out_edge_iterator oeIt, oeEnd;
  for ( boost::tie( oeIt, oeEnd ) = out_edges( m, mCGraph ); oeIt != oeEnd;
        ++oeIt ) {
    if ( mCGraph[ *oeIt ].isBackEdge )
      ++backEdges;
    if ( mCGraph[ *oeIt ].isBreakEdge )
      ++breakEdges;
  }

  DOUT(
    "Out-degree of m = " << out_degree( m, mCGraph ) << endl <<
    "Back edges leaving m = " << backEdges << endl <<
    "Break edges leaving m = " << breakEdges << endl );

  if ( ( out_degree( m, mCGraph ) != 2 ) || ( backEdges != 1 ) ||
       ( breakEdges != 0 ) || ( nSet.size() != 2 ) )
    // Not a natural do-while loop.
    return( nullptr );

  // If nSet includes back edges that do not lead to n, it's again not a natural
  // loop.
  for ( auto *v : nSet ) {
    auto s = mNodeByID.at( v->getID() );
    CGraph::out_edge_iterator oeIt, oeEnd;
    for ( boost::tie( oeIt, oeEnd ) = out_edges( s, mCGraph ); oeIt != oeEnd;
          ++oeIt )
      if ( nSet.count( mCGraph[ target( *oeIt, mCGraph ) ] ) &&
           mCGraph[ *oeIt ].isBackEdge &&
           ( *(mCGraph[ target( *oeIt, mCGraph ) ]) != node ) )
        return( nullptr );
  }

  DACTION(
    DOUT( "Found a natural do-while-loop with start node ID " << node.getID() );
    if ( node.getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" <<
        dynamic_cast<const WIR_BasicBlockTreeNode &>(
          node ).getBasicBlock().getName() << ")" );
    DOUT( "." << endl ); );

  auto *newNode = new WIR_NaturalLoopTreeNode( node );

  for ( auto *v : nSet ) {
    newNode->insertNode( *v );

    auto s = mNodeByID.at( v->getID() );
    CGraph::out_edge_iterator oeIt, oeEnd;
    for ( boost::tie( oeIt, oeEnd ) = out_edges( s, mCGraph ); oeIt != oeEnd;
          ++oeIt )
      if ( nSet.count( mCGraph[ target( *oeIt, mCGraph ) ] ) ) {
        if ( mCGraph[ *oeIt ].isBackEdge )
          newNode->insertBackEdge( *v, *(mCGraph[ target( *oeIt, mCGraph ) ]) );
        else
          newNode->insertEdge( *v, *(mCGraph[ target( *oeIt, mCGraph ) ]) );
      }
  }

  return( newNode );
};



/*
  checkRecursionLoop checks whether the specified CFG node forms a cyclic
  structure stemming from tail recursion elimination.
*/
WIR_NaturalLoopTreeNode *WIR_StructuralAnalysis::checkRecursionLoop( const WIR_ControlTreeNode &node,
                                                                     WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet )
{
  DSTART(
    "WIR_NaturalLoopTreeNode* WIR_StructuralAnalysis::checkRecursionLoop(const "
    "WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&)" );

  auto n = mNodeByID.at( node.getID() );
  auto m = n;

  DACTION(
    DOUT( "Current node n = " << mCGraph[ n ]->getID() );
    if ( mCGraph[ n ]->getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" << dynamic_cast<WIR_BasicBlockTreeNode *>(
          mCGraph[ n ] )->getBasicBlock().getName() << ")" );
    DOUT( endl << "Current nSet = {" );
    for ( auto *p : nSet ) {
      if ( p != *(nSet.begin()) )
        DOUT( "," );
      DOUT( " " << p->getID() );
      if ( p->getType() == WIR_CTNodeType::bb )
        DOUT(
          " (" <<
          dynamic_cast<WIR_BasicBlockTreeNode *>(
            p )->getBasicBlock().getName() << ")" );
    }
    DOUT( " }" << endl ); );

  // Determine the back edge leading from m to n.
  unsigned int nIncomingBackEdges = 0;

  CGraph::in_edge_iterator ieIt, ieEnd;
  for ( boost::tie( ieIt, ieEnd ) = in_edges( n, mCGraph ); ieIt != ieEnd;
        ++ieIt )
    if ( mCGraph[ *ieIt ].isBackEdge ) {
      ++nIncomingBackEdges;
      auto src = source( *ieIt, mCGraph );

      if ( nSet.count( mCGraph[ src ] ) )
        m = src;
    }

  if ( m == n )
    // No back edge found.
    return( nullptr );

  DACTION(
    DOUT( "Found node that is target of a back-edge." << endl );
    DOUT( "Start of back-edge m = " << mCGraph[ m ]->getID() );
    if ( mCGraph[ m ]->getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" << dynamic_cast<WIR_BasicBlockTreeNode *>(
          mCGraph[ m ] )->getBasicBlock().getName() << ")" );
    DOUT( endl ); );

  // Determine the number of back and break edges leaving the potential loop
  // body m.
  unsigned int mOutgoingBackEdges = 0;
  unsigned int mOutgoingBreakEdges = 0;

  CGraph::out_edge_iterator oeIt, oeEnd;
  for ( boost::tie( oeIt, oeEnd ) = out_edges( m, mCGraph ); oeIt != oeEnd;
        ++oeIt ) {
    if ( mCGraph[ *oeIt ].isBackEdge )
      ++mOutgoingBackEdges;
    if ( mCGraph[ *oeIt ].isBreakEdge )
      ++mOutgoingBreakEdges;
  }

  DOUT(
    "Out-degree of m = " << out_degree( m, mCGraph ) << endl <<
    "Back edges leaving m = " << mOutgoingBackEdges << endl <<
    "Break edges leaving m = " << mOutgoingBreakEdges << endl );

  if ( ( out_degree( n, mCGraph ) != in_degree( m, mCGraph ) ) ||
       ( nIncomingBackEdges != 1 ) || ( mOutgoingBackEdges != 1 ) ||
       ( nSet.size() != 2 ) || ( out_degree( m, mCGraph ) != 1 ) )
    // Not a while-do loop stemming from tail-recursion elimination.
    return( nullptr );

  // If nSet includes back edges that do not lead to n, it's again not a natural
  // loop.
  for ( auto *v : nSet ) {
    auto s = mNodeByID.at( v->getID() );
    for ( boost::tie( oeIt, oeEnd ) = out_edges( s, mCGraph ); oeIt != oeEnd;
          ++oeIt )
      if ( nSet.count( mCGraph[ target( *oeIt, mCGraph ) ] ) &&
           mCGraph[ *oeIt ].isBackEdge &&
           ( *(mCGraph[ target( *oeIt, mCGraph ) ]) != node ) )
        return( nullptr );
  }

  DACTION(
    DOUT( "Found a natural do-while-loop with start node ID " << node.getID() );
    if ( node.getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" <<
        dynamic_cast<const WIR_BasicBlockTreeNode &>(
          node ).getBasicBlock().getName() << ")" );
    DOUT( "." << endl ); );

  auto *newNode = new WIR_NaturalLoopTreeNode( node );

  for ( auto *v : nSet ) {
    newNode->insertNode( *v );

    auto s = mNodeByID.at( v->getID() );
    CGraph::out_edge_iterator oeIt, oeEnd;
    for ( boost::tie( oeIt, oeEnd ) = out_edges( s, mCGraph ); oeIt != oeEnd;
          ++oeIt )
      if ( nSet.count( mCGraph[ target( *oeIt, mCGraph ) ] ) ) {
        if ( mCGraph[ *oeIt ].isBackEdge )
          newNode->insertBackEdge( *v, *(mCGraph[ target( *oeIt, mCGraph ) ]) );
        else
          newNode->insertEdge( *v, *(mCGraph[ target( *oeIt, mCGraph ) ]) );
      }
  }

  return( newNode );
};


/*
  testProper tests recursively whether the specified CFG node forms a proper
  acyclic region (S. S. Muchnick, Fig. 7.37, page 204).
*/
bool WIR_StructuralAnalysis::testProper( const WIR_ControlTreeNode &node,
                                         WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet ) const
{
  DSTART(
    "bool WIR_StructuralAnalysis::testProper(const WIR_ControlTreeNode&, "
    "WIR_HierarchicalCFG::WIR_HCFGNodeSet&) const" );

  auto testSet = nSet;
  CGraphVertex n = mNodeByID.at( node.getID() );

  // Check whether node is a self-loop or whether it is source of a back-edge if
  // it's the first node of a hypothetical proper region, or if including node
  // makes the region span over a loop's back edge. If so, this is not a proper
  // region.
  CGraph::out_edge_iterator oeIt, oeEnd;
  for ( boost::tie( oeIt, oeEnd ) = out_edges( n, mCGraph ); oeIt != oeEnd;
        ++oeIt ) {
    if ( target( *oeIt, mCGraph ) == n )
//          ( nSet.empty() && mCGraph[ *oeIt ].isBackEdge ) )
      return( false );

    if ( mCGraph[ *oeIt ].isBackEdge &&
         testSet.count( mCGraph[ target( *oeIt, mCGraph ) ] ) )
      return( false );
  }

  // Check whether node is target of a back-edge or of a loop exit edge if it's
  // not the first node of a hypothetical proper region. If so, this is not a
  // proper region.
  CGraph::in_edge_iterator ieIt, ieEnd;
  for ( boost::tie( ieIt, ieEnd ) = in_edges( n, mCGraph ); ieIt != ieEnd;
        ++ieIt )
    if ( ( mCGraph[ *ieIt ].isBackEdge || mCGraph[ *ieIt ].isLoopExitEdge ) &&
         !nSet.empty() )
      return( false );

  // Add node to the test set.
  testSet.insert( const_cast<WIR_ControlTreeNode *>( &node ) );
  DACTION(
    DOUT(
      "Checking to extend proper region by node ID " << node.getID() );
    if ( node.getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" <<
        dynamic_cast<const WIR_BasicBlockTreeNode &>( node ).getBasicBlock().getName() << ")" );
    DOUT( "." << endl << "Current testSet = {" );
    for ( auto *p : testSet ) {
      if ( p != *(testSet.begin()) )
        DOUT( "," );
      DOUT( " " << p->getID() );
      if ( p->getType() == WIR_CTNodeType::bb )
        DOUT(
          " (" <<
          dynamic_cast<WIR_BasicBlockTreeNode *>(
            p )->getBasicBlock().getName() << ")" );
    }
    DOUT( " }" << endl ); );

  // Compute set of entry and exit nodes for the current region.
  set<CGraphVertex> entries;
  set<CGraphVertex> exits;

  for ( auto &v : testSet ) {
    auto m = mNodeByID.at( v->getID() );

    for ( boost::tie( oeIt, oeEnd ) = out_edges( m, mCGraph ); oeIt != oeEnd;
          ++oeIt )
      if ( !testSet.count( mCGraph[ target( *oeIt, mCGraph ) ] ) )
        exits.insert( m );
    if ( out_degree( m, mCGraph ) == 0 )
      exits.insert( m );

    for ( boost::tie( ieIt, ieEnd ) = in_edges( m, mCGraph ); ieIt != ieEnd;
          ++ieIt )
      if ( !testSet.count( mCGraph[ source( *ieIt, mCGraph ) ] ) )
        entries.insert( m );
    if ( in_degree( m, mCGraph ) == 0 )
      entries.insert( m );
  }

  // Check whether the current node set is a proper region, i.e., whether
  // |entries| == 1 && |testSet| > 1 holds.
  bool properRegionFound = false;

  if ( ( entries.size() == 1 ) && ( testSet.size() > 1 ) ) {
    // We have found a proper region, replace nSet by the identified testSet and
    // exit.
    nSet = testSet;
    DOUT( "  Extension succeeded, proper region found (1)." << endl );
    return( true );
  } else {
    // Recursively test all successors of node as long as they are not reached
    // via back or break edges. If any proper region was found this way, select
    // the smallest one.
    CGraphVertex n = mNodeByID.at( node.getID() );

    for ( boost::tie( oeIt, oeEnd ) = out_edges( n, mCGraph ); oeIt != oeEnd;
          ++oeIt )
      if ( !mCGraph[ *oeIt ].isBackEdge && !mCGraph[ *oeIt ].isBreakEdge &&
            !testSet.count( mCGraph[ target( *oeIt, mCGraph ) ] ) ) {
        auto successorSet = testSet;
        bool found =
          testProper( *mCGraph[ target( *oeIt, mCGraph) ], successorSet );

        if ( found &&
             ( ( successorSet.size() < testSet.size() ) ||
               !properRegionFound ) ) {
          properRegionFound = true;
          testSet = successorSet;
        }
      }
  }

  if ( properRegionFound ) {
    nSet = testSet;
    DOUT( "  Extension succeeded, proper region found (2)." << endl );
    return( true );
  }

  DOUT( "  Extension failed, no proper region found." << endl );
  return( false );
};


/*
  reduce replaces the given node set by a new abstract control region.

  This method implements procedure Reduce() from S. S. Muchnick, page 209 and
  procedure Replace() from S. S. Muchnick, page 210.
*/
WIR_id_t WIR_StructuralAnalysis::reduce( WIR_ControlTreeNode &node,
                                         WIR_HierarchicalCFG::WIR_HCFGNodeSet &nodeSet )
{
  DSTART(
    "WIR_id_t WIR_StructuralAnalysis::reduce(WIR_ControlTreeNode&, "
    "WIR_HierarchicalCFG::WIR_HCFGNodeSet&)" );

  // Build completely new hierarchical control flow graph from scratch.
  CGraph newCGraph;
  map<CGraphVertex, CGraphVertex> newVertexOf;

  BGL_FORALL_VERTICES( v, mCGraph, CGraph ) {
    // Duplicate all vertices not being in nodeSet.
    if ( !nodeSet.count( mCGraph[ v ] ) ) {
      auto newVertex = add_vertex( mCGraph[ v ], newCGraph );
      newVertexOf[ v ] = newVertex;

      DACTION(
        auto *n = newCGraph[ newVertex ];
        DOUT(
          "Created duplicate node '" << newVertex << "' (ID " << n->getID() );
        if ( n->getType() == WIR_CTNodeType::bb )
          DOUT(
            ", " <<
            dynamic_cast<const WIR_BasicBlockTreeNode *>(
              n )->getBasicBlock().getName() );
        DOUT( ")." << endl ); );
    }
  }

  // Add new vertex for node.
  auto newNodeVertex = add_vertex( &node, newCGraph );
  DOUT(
    "Created new node '" << newNodeVertex << "' (ID " <<
    newCGraph[ newNodeVertex ]->getID() << ")." << endl );

  // Duplicate all edges.
  BGL_FORALL_EDGES( e, mCGraph, CGraph ) {
    auto src = source( e, mCGraph );
    auto tgt = target( e, mCGraph );

    DACTION(
      DOUT( "Processing edge '" << src << "' (ID " << mCGraph[ src ]->getID() );
      if ( mCGraph[ src ]->getType() == WIR_CTNodeType::bb )
        DOUT(
          ", " <<
          dynamic_cast<const WIR_BasicBlockTreeNode *>(
            mCGraph[ src ] )->getBasicBlock().getName() );
      DOUT( ") -> '" << tgt << "' (ID " << mCGraph[ tgt ]->getID() );
      if ( mCGraph[ tgt ]->getType() == WIR_CTNodeType::bb )
        DOUT(
          ", " <<
          dynamic_cast<const WIR_BasicBlockTreeNode *>(
            mCGraph[ tgt ] )->getBasicBlock().getName() );
      DOUT( ")." << endl ); );

    if ( !nodeSet.count( mCGraph[ src ] ) &&
         !nodeSet.count( mCGraph[ tgt ] ) ) {
      // Duplicate edges that are not incident with nodeSet.
      auto newEdge =
        add_edge( newVertexOf[ src ], newVertexOf[ tgt ], newCGraph ).first;

      newCGraph[ newEdge ].isBackEdge = mCGraph[ e ].isBackEdge;
      newCGraph[ newEdge ].isBreakEdge = mCGraph[ e ].isBreakEdge;
      newCGraph[ newEdge ].isLoopExitEdge = mCGraph[ e ].isLoopExitEdge;

      DACTION(
        auto newSrc = source( newEdge, newCGraph );
        auto newTgt = target( newEdge, newCGraph );
        DOUT(
          "Created duplicate edge '" << newSrc << "' (ID " <<
          newCGraph[ newSrc ]->getID() );
        if ( newCGraph[ newSrc ]->getType() == WIR_CTNodeType::bb )
          DOUT(
            ", " <<
            dynamic_cast<const WIR_BasicBlockTreeNode *>(
              newCGraph[ newSrc ] )->getBasicBlock().getName() );
        DOUT( ") -> '" << newTgt << "' (ID " << newCGraph[ newTgt ]->getID() );
        if ( newCGraph[ newTgt ]->getType() == WIR_CTNodeType::bb )
          DOUT(
            ", " <<
            dynamic_cast<const WIR_BasicBlockTreeNode *>(
              newCGraph[ newTgt ] )->getBasicBlock().getName() );
        DOUT( ")." << endl ); );
    } else

    if ( !nodeSet.count( mCGraph[ src ] ) ) {
      // Create new edge starting outside nodeSet and ending in nodeSet.
      // if ( e@1 \in N - nodeSet + node ) && ( e@1 != node )
      auto p = add_edge( newVertexOf[ src ], newNodeVertex, newCGraph );
      auto newEdge = p.first;

      if ( p.second ) {
        newCGraph[ newEdge ].isBackEdge = mCGraph[ e ].isBackEdge;
        newCGraph[ newEdge ].isBreakEdge = mCGraph[ e ].isBreakEdge;
        newCGraph[ newEdge ].isLoopExitEdge = mCGraph[ e ].isLoopExitEdge;
      } else {
        newCGraph[ newEdge ].isBackEdge |= mCGraph[ e ].isBackEdge;
        newCGraph[ newEdge ].isBreakEdge &= mCGraph[ e ].isBreakEdge;
        newCGraph[ newEdge ].isLoopExitEdge |= mCGraph[ e ].isLoopExitEdge;
      }

      DACTION(
        auto newSrc = source( newEdge, newCGraph );
        auto newTgt = target( newEdge, newCGraph );
        DOUT(
          "Adding edge '" << newSrc << "' (ID " <<
          newCGraph[ newSrc ]->getID() );
        if ( newCGraph[ newSrc ]->getType() == WIR_CTNodeType::bb )
          DOUT(
            ", " <<
            dynamic_cast<const WIR_BasicBlockTreeNode *>(
              newCGraph[ newSrc ] )->getBasicBlock().getName() );
        DOUT(
          ") -> '" << newTgt << "' (ID " << newCGraph[ newTgt ]->getID() <<
          ")." << endl ); );
    } else

    if ( !nodeSet.count( mCGraph[ tgt ] ) ) {
      // Create new edge starting inside nodeSet and ending outside nodeSet.
      // if ( e@2 \in N - nodeSet + node ) && ( e@2 != node )
      auto p = add_edge( newNodeVertex, newVertexOf[ tgt ], newCGraph );
      auto newEdge = p.first;

      if ( p.second ) {
        newCGraph[ newEdge ].isBackEdge = mCGraph[ e ].isBackEdge;
        newCGraph[ newEdge ].isBreakEdge = mCGraph[ e ].isBreakEdge;
        newCGraph[ newEdge ].isLoopExitEdge = mCGraph[ e ].isLoopExitEdge;
      } else {
        newCGraph[ newEdge ].isBackEdge |= mCGraph[ e ].isBackEdge;
        newCGraph[ newEdge ].isBreakEdge &= mCGraph[ e ].isBreakEdge;
        newCGraph[ newEdge ].isLoopExitEdge |= mCGraph[ e ].isLoopExitEdge;
      }

      DACTION(
        auto newSrc = source( newEdge, newCGraph );
        auto newTgt = target( newEdge, newCGraph );
        DOUT(
          "Adding edge '" << newSrc << "' (ID " <<
          newCGraph[ newSrc ]->getID() << ") -> '" << newTgt << "' (ID " <<
          newCGraph[ newTgt ]->getID() );
        if ( newCGraph[ newTgt ]->getType() == WIR_CTNodeType::bb )
          DOUT(
            ", " <<
            dynamic_cast<const WIR_BasicBlockTreeNode *>(
              newCGraph[ newTgt ] )->getBasicBlock().getName() );
        DOUT( ")." << endl ); );
    }
  }

  // Merge all nodes to be replaced into the new hierarchical node.
  while ( !nodeSet.empty() ) {
    auto *n = *(nodeSet.begin());
    nodeSet.erase( nodeSet.begin() );

    if ( ( ( node.getType() == WIR_CTNodeType::proper ) &&
           ( n->getType() == WIR_CTNodeType::proper ) ) ||
         ( ( node.getType() == WIR_CTNodeType::block ) &&
            ( n->getType() == WIR_CTNodeType::block ) ) ) {
      DOUT(
        "Deleting hierarchical proper region " << n->getID() << "." << endl );
      delete( n );
    } else {
      node.insertChild( n );
      DACTION(
        DOUT( "Merging hierarchical node " << n->getID() );
        if ( n->getType() == WIR_CTNodeType::bb )
          DOUT(
            " (" <<
            dynamic_cast<const WIR_BasicBlockTreeNode *>(
              n )->getBasicBlock().getName() << ")" );
        DOUT( " into new node " << node.getID() << "." << endl ); );
    }
  }

  // Switch the graphs.
  mCGraph = newCGraph;

  // Rebuild map mNodeByID.
  mNodeByID.clear();

  BGL_FORALL_VERTICES( v, mCGraph, CGraph ) {
    list<WIR_ControlTreeNode *> workList;

    workList.push_back( mCGraph[ v ] );
    while ( !workList.empty() ) {
      auto *n = workList.front();
      workList.pop_front();

      mNodeByID[ n->getID() ] = v;

      if ( n->getType() == WIR_CTNodeType::bb ) {
        auto *bbNode = dynamic_cast<WIR_BasicBlockTreeNode *>( n );
        mNodeByID[ bbNode->getBasicBlock().getID() ] = v;
      }
      for ( WIR_ControlTreeNode &c : n->getChilds() )
        workList.push_back( &c );
    }
  }

  return( newCGraph[ newNodeVertex ]->getID() );
};


/*
  pathBack checks whether there is a node k such that there is a (possibly
  empty) path from m to k that does not pass through n and an edge k -> n that
  is a back edge.

  This method implements procedure Path_Back(m, n) from S. S. Muchnick, page
  207.
*/
bool WIR_StructuralAnalysis::pathBack( CGraphVertex m, CGraphVertex n,
                                       CGraphEdge b ) const
{
  DSTART(
    "bool WIR_StructuralAnalysis::pathBack(WIR_HierarchicalCFG::CGraphVertex, "
    "WIR_HierarchicalCFG::CGraphVertex, "
    "WIR_HierarchicalCFG::CGraphEdge) const" );

  // Determine the set of all nodes that are reachable starting from m, but
  // without passing through n.
  set<CGraphVertex> reachable;
  set<CGraphVertex> workSet { m };

  while ( !workSet.empty() ) {
    auto v = *(workSet.begin());
    workSet.erase( workSet.begin() );
    reachable.insert( v );

    DACTION(
      DOUT( "Examining node " << mCGraph[ v ]->getID() );
      if ( mCGraph[ v ]->getType() == WIR_CTNodeType::bb )
        DOUT(
          " (" <<
          dynamic_cast<WIR_BasicBlockTreeNode *>(
            mCGraph[ v ] )->getBasicBlock().getName() << ")" );
      DOUT( "." << endl ); );

    CGraph::out_edge_iterator oeIt, oeEnd;
    for ( boost::tie( oeIt, oeEnd ) = out_edges( v, mCGraph ); oeIt != oeEnd;
          ++oeIt )
      if ( ( target( *oeIt, mCGraph ) != n ) &&
           !reachable.count( target( *oeIt, mCGraph ) ) ) {
        workSet.insert( target( *oeIt, mCGraph ) );
        DACTION(
          DOUT(
            "Adding node " << mCGraph[ target( *oeIt, mCGraph ) ]->getID() );
          if ( mCGraph[ target( *oeIt, mCGraph ) ]->getType() ==
                 WIR_CTNodeType::bb )
            DOUT(
              " (" <<
              dynamic_cast<WIR_BasicBlockTreeNode *>(
                mCGraph[ target( *oeIt, mCGraph ) ] )->getBasicBlock().getName() << ")" );
          DOUT( " to workSet." << endl ); );
      }
  }

  DACTION(
    DOUT( "reachable[ " << mCGraph[ m ]->getID() );
    if ( mCGraph[ m ]->getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" <<
        dynamic_cast<WIR_BasicBlockTreeNode *>(
          mCGraph[ m ] )->getBasicBlock().getName() << ")" );
    DOUT( " ] = { " );
    for ( auto v : reachable ) {
      if ( v != *(reachable.begin() ) )
        DOUT( ", " );
      DOUT( mCGraph[ v ]->getID() );
      if ( mCGraph[ v ]->getType() == WIR_CTNodeType::bb )
        DOUT(
          " (" <<
          dynamic_cast<WIR_BasicBlockTreeNode *>(
            mCGraph[ v ] )->getBasicBlock().getName() << ")" );
    }
    DOUT( " }" << endl ); );

  // Check the back edge b = k -> n whether it is reachable.
  if ( ( target( b, mCGraph ) == n ) &&
       reachable.count( source( b, mCGraph ) ) )
    return( true );

  return( false );
};


/*
  minimizeImproper minimizes an improper interval containing a given node.

  This method implements procedure Minimize_Improper(m, n) from S. S. Muchnick,
  Fig. 7.45, page 207.
*/
WIR_ControlTreeNode &WIR_StructuralAnalysis::minimizeImproper( const WIR_ControlTreeNode &node,
                                                               WIR_HierarchicalCFG::WIR_HCFGNodeSet &nSet )
{
  DSTART(
    "WIR_ControlTreeNode& WIR_StructuralAnalysis::minimizeImproper(const WIR_ControlTreeNode&, WIR_HierarchicalCFG::WIR_HCFGNodeSet&)" );

  // Determine the set of entry nodes within nSet of the smallest multiple-
  // entry cycle of which node is one of the entries.
  set<CGraphVertex> minCycle;
  set<CGraphVertex> I;

  set<CGraphVertex> visited;
  set<CGraphVertex> currentPath;

  DACTION(
    DOUT( "Current start node: " << node.getID() );
    if ( node.getType() == WIR_CTNodeType::bb )
      DOUT(
        " (" <<
        dynamic_cast<const WIR_BasicBlockTreeNode &>(
          node ).getBasicBlock().getName() << ")" );
    DOUT( "." << endl );
    DOUT( "nSet = {" );
    for ( auto *n : nSet ) {
      if ( *n != *(*(nSet.begin())) )
        DOUT( "," );
      DOUT( " " << n->getID() );
      if ( n->getType() == WIR_CTNodeType::bb )
        DOUT(
          " (" <<
          dynamic_cast<const WIR_BasicBlockTreeNode *>(
            n )->getBasicBlock().getName() << ")" );
    }
    DOUT( " }" << endl ); );

  // A small lambda to do DFS-style detection of smallest multi-entry cycles.
  // This basically implements procedure MEC_Entries from S. S. Muchnick, page
  // 211.
  // It is, however, very likely that this algorithm does not find the
  // definitely minimal cycle. Suppose a CFG like
  //
  //   node <--- n1 <--- n2
  //    | |      ^       ^
  //    | +------+       |
  //    +----------------+
  //
  // Depending on the DFS visiting order, the algorithm below might first of all
  // detect node->n2->n1->node and mark n1 as visited, so that the smaller cycle
  // node->n1->node is not re-visited. I did not find any efficient algorithm to
  // find all cycles in a graph, and all standard graph algorithms dealing with
  // cycles (e.g., Tarjan's algorithm for strongly-connected components) base on
  // DFS and are thus sensitive to the DFS visiting order.
  //
  // In practice, the non-optimality of the algorithm below should not be a
  // problem, since improper regions are rare, and improper regions featuring
  // multiple cycles are even more rare. If, however, this turns out to be a
  // problem, then the optimal computation of minimal cycles can be done easily
  // (and probably very efficiently) using ILP. The basic idea of this ILP is:
  //
  // For each directed edge e_m = (v_i -> v_j) in mCGraph, insert a binary
  // decision variable edge_m to the ILP that will be set to 1 iff e_m belongs
  // to an identified cycle.
  // If e_m belongs to a cycle, its incident nodes v_i and v_j are also part of
  // the cycle. Thus, insert binary variables node_i to the ILP and add
  // constraints (1) for each node v_i:
  //   node_i >= \sum_{over all edges e_m incident with v_i} edge_m          (1)
  // The edges selectes this way form a cycle iff the number of selected edges
  // is equal to the number of selected nodes, i.e., another constraint (2) is
  // added:
  //   \sum_{over all edges e_m} edge_m = \sum_{over all nodes n_i} node_i   (2)
  // Since we only want cycles including node 'node', we enforce that this node
  // is always selected:
  //   node_node = 1                                                         (3)
  // We are interested in minimal cycles so that the ILP's objective function
  // is:
  //   min \sum_{over all edges e_m} edge_m                                  (4)
  // Within a cycle, each selected node has exactly one selected outgoing edge
  // and one selected incoming edge. A node is an entry of a cycle if it has at
  // least one more non-selected in coming edge. In other words, a node v_i is
  // an entry if it is selected for a cycle and if its in-degree is greater than
  // 1. We thus add other binary variables entry_i per node v_i that are set to
  // one iff v_i is an entry:
  //   entry_i = node_i && ( in_degree_i > 1 )                               (5)
  // where the latter term relating to the in_degree can be computed statically
  // prior to generating the ILP.
  // Finally, we enforce that the identified cycles have at least two entries:
  //   \sum_{over all nodes n_i} entry_i >= 2                                (6)
  std::function<void(CGraphVertex)> dfsCycle = [&]( CGraphVertex v ) {
    // Visit the current node.
    visited.insert( v );
    currentPath.insert( v );

    // Visit descendants if they are in nSet.
    CGraph::out_edge_iterator oeIt, oeEnd;
    for ( boost::tie( oeIt, oeEnd ) = out_edges( v, mCGraph ); oeIt != oeEnd;
          ++oeIt ) {
      auto tgt = target( *oeIt, mCGraph );

      if ( tgt == mNodeByID.at( node.getID() ) ) {
        // Detected a cycle starting and ending in node. Determine its entries.
        set<CGraphVertex> entries;
        for ( auto w : currentPath ) {
          CGraph::in_edge_iterator ieIt, ieEnd;
          for ( boost::tie( ieIt, ieEnd ) = in_edges( w, mCGraph );
                ieIt != ieEnd; ++ieIt ) {
            auto src = source( *ieIt, mCGraph );
            if ( !currentPath.count( src ) )
              entries.insert( w );
          }
        }

        if ( ( entries.size() > 1 ) &&
             ( minCycle.empty() ||
               ( currentPath.size() < minCycle.size() ) ) ) {
          // Found a multi-entry cycle that is smaller than the currently
          // smallest such cycle.
          minCycle = currentPath;
          I = entries;
        }
      } else

      if ( !visited.count( tgt ) && nSet.count( mCGraph[ tgt ] ) )
        dfsCycle( tgt );
    }

    // Finish the current node.
    currentPath.erase( v );
  };

  dfsCycle( mNodeByID.at( node.getID() ) );
  ufAssert( I.size() > 1 );
  DACTION(
    DOUT( "Identified entry nodes of multi-entry cycle: {" );
    for ( auto v : I ) {
      auto *n = mCGraph[ v ];
      if ( v != *(I.begin()) )
        DOUT( "," );
      DOUT( " " << n->getID() );
      if ( n->getType() == WIR_CTNodeType::bb )
        DOUT(
          " (" <<
          dynamic_cast<WIR_BasicBlockTreeNode *>(
            n )->getBasicBlock().getName() << ")" );
    }
    DOUT( " }" << endl ); );

  // Determine the nearest common dominator of all entry nodes in I.
  // This basically implements procedure NC_Domin from S. S. Muchnick, page 211.

  // If this has not yet been done, do an immediate domination analysis now.
  if ( !mIDomAnalysisDone ) {
    WIR_ImmediateDominationAnalysis a { mFunction };
    a.analyze();
    mIDomAnalysisDone = true;
  }

  // Determine the set of unique entry basic blocks for each node in I.
  WIR_BasicBlockSet entryBBs;
  for ( auto &v : I ) {
    WIR_ControlTreeNode *n = mCGraph[ v ];
    while ( n->getType() != WIR_CTNodeType::bb )
      n = const_cast<WIR_ControlTreeNode *>( &(n->getEntry()) );

    entryBBs.insert(
      dynamic_cast<WIR_BasicBlockTreeNode *>( n )->getBasicBlock() );
  }

  // Arrange the paths from the entry basic blocks in the dominator tree to its
  // root in lists.
  map<WIR_id_t, list<reference_wrapper<WIR_BasicBlock>>> dominatorPaths;
  unsigned int minLength = 0;
  for ( WIR_BasicBlock &entry : entryBBs ) {
    // Get b's immediate domination container.
    WIR_BasicBlock *b = &entry;
    WIR_ImmediateDomination *cont =
      &(b->getContainers<WIR_ImmediateDomination>().begin()->get());

    while ( cont->getImmediateDominator() != *b ) {
      dominatorPaths[ entry.getID() ].push_front(
        cont->getImmediateDominator() );

      b = &(cont->getImmediateDominator());
      cont = &(b->getContainers<WIR_ImmediateDomination>().begin()->get());
    }
    dominatorPaths[ entry.getID() ].push_front( *b );
    if ( ( minLength == 0 ) ||
         ( dominatorPaths[ entry.getID() ].size() < minLength ) )
      minLength = dominatorPaths[ entry.getID() ].size();
  }

  WIR_BasicBlock *ncdBB = nullptr;
  for ( unsigned int i = 0; i < minLength; ++i ) {
    bool commonDominatorFound = true;
    WIR_BasicBlock *currentDomin = nullptr;
    for ( auto &p : dominatorPaths ) {
      if ( currentDomin == nullptr )
        currentDomin = &(p.second.front().get());
      else

      if ( p.second.front().get() != *currentDomin ) {
        commonDominatorFound = false;
        break;
      }

      p.second.pop_front();
    }

    if ( commonDominatorFound )
      ncdBB = currentDomin;
  }

  ufAssert( ncdBB != nullptr );
  CGraphVertex ncd = mNodeByID.at( ncdBB->getID() );

  // Determine all descendants of ncd.
  set<CGraphVertex> descendantsNCD;
  set<CGraphVertex> workSet { ncd };

  while ( !workSet.empty() ) {
    auto v = *(workSet.begin());
    workSet.erase( workSet.begin() );
    descendantsNCD.insert( v );

    CGraph::out_edge_iterator oeIt, oeEnd;
    for ( boost::tie( oeIt, oeEnd ) = out_edges( v, mCGraph ); oeIt != oeEnd;
          ++oeIt )
      if ( !descendantsNCD.count( target( *oeIt, mCGraph ) ) )
        workSet.insert( target( *oeIt, mCGraph ) );
  }

  BGL_FORALL_VERTICES( n, mCGraph, CGraph )
    // Check all nodes n \in mCGraph\{ncd} where a path ncd ~> n exists.
    if ( ( n != ncd ) && descendantsNCD.count( n ) ) {
      // Determine all descendants of n without ncd.
      bool mFound = false;
      set<CGraphVertex> descendantsN;
      workSet = { n };

      while ( !workSet.empty() ) {
        auto v = *(workSet.begin());
        workSet.erase( workSet.begin() );
        descendantsN.insert( v );

        if ( I.count( v ) ) {
          // Found a node m in I where a path n ~> m exists.
          mFound = true;
          break;
        }

        CGraph::out_edge_iterator oeIt, oeEnd;
        for ( boost::tie( oeIt, oeEnd ) = out_edges( v, mCGraph );
              oeIt != oeEnd; ++oeIt )
          if ( !descendantsN.count( target( *oeIt, mCGraph ) ) &&
               ( target( *oeIt, mCGraph ) != ncd ) )
            workSet.insert( target( *oeIt, mCGraph ) );
      }

      if ( mFound )
        I.insert( n );
    }

  // Finally add ncd to I and return that via nSet.
  I.insert( ncd );

  nSet.clear();
  for ( auto v : I )
    nSet.insert( mCGraph[ v ] );

  return( *(mCGraph[ ncd ]) );
};


/*
  createContainers takes the leaf nodes of the created control trees and
  attaches them persistently to the current WIR function's basic blocks using
  control tree containers.
*/
void WIR_StructuralAnalysis::createContainers( void )
{
  DSTART( "void WIR_StructuralAnalysis::createContainers()" );

  // First, attach a function-level container just for internal memory
  // management.
  mFunction.insertContainer( new WIR_ControlTree() );
  WIR_ControlTree &fRes =
    mFunction.getContainers<WIR_ControlTree>().begin()->get();

  BGL_FORALL_VERTICES( v, mCGraph, CGraph ) {
    list<WIR_ControlTreeNode *> workList;

    fRes.pushBackRootNode( *(mCGraph[ v ]) );

    workList.push_back( mCGraph[ v ] );
    while ( !workList.empty() ) {
      auto *n = workList.front();
      workList.pop_front();

      if ( n->getType() == WIR_CTNodeType::bb ) {
        auto &leaf = *(dynamic_cast<WIR_BasicBlockTreeNode *>( n ));
        auto &b = leaf.getBasicBlock();

        // Attach a fresh container for control tree leafs to the current basic
        // block.
        b.insertContainer( new WIR_ControlTree( leaf ) );
      }

      for ( WIR_ControlTreeNode &c : n->getChilds() )
        workList.push_back( &c );
    }
  }

  // Free some no longer needed memory.
  if ( !mIDomAnalysisDone ) {
    for ( WIR_BasicBlock &b : mFunction )
      b.eraseContainers( WIR_ImmediateDomination::getContainerTypeID() );
    mIDomAnalysisDone = false;
  }
};

}       // namespace WIR
