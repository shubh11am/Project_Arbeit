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
  @file wirloopinvariantcm.cc
  @brief This file implements a loop-invariant code motion optimization.

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
#include <sstream>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/stringtools.h>

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
  Default constructor for system-level optimization.
*/
WIR_LoopInvariantCM::WIR_LoopInvariantCM( WIR_System &s ) :
  WIR_Optimization { s },
  mEntry { nullptr },
  mIsForLoop { false }
{
  DSTART( "WIR_LoopInvariantCM::WIR_LoopInvariantCM(WIR_System&)" );
};


/*
  Default constructor for compilation unit-level optimization.
*/
WIR_LoopInvariantCM::WIR_LoopInvariantCM( WIR_CompilationUnit &c ) :
  WIR_Optimization { c },
  mEntry { nullptr },
  mIsForLoop { false }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
WIR_LoopInvariantCM::WIR_LoopInvariantCM( WIR_Function &f ) :
  WIR_Optimization { f },
  mEntry { nullptr },
  mIsForLoop { false }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
WIR_LoopInvariantCM::~WIR_LoopInvariantCM( void )
{
  DSTART( "virtual WIR_LoopInvariantCM::~WIR_LoopInvariantCM()" );
};


//
// Protected class methods
//

/*
  runOptimization moves loop-invariant code in the given system.
*/
void WIR_LoopInvariantCM::runOptimization( WIR_System &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_CompilationUnit &c : s )
    runOptimization( c );
};


/*
  runOptimization moves loop-invariant code in the given compilation unit.
*/
void WIR_LoopInvariantCM::runOptimization( WIR_CompilationUnit &c )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_Function &f : c )
    runOptimization( f );
};


/*
  runOptimization moves loop-invariant code in the given function.
*/
void WIR_LoopInvariantCM::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void WIR_LoopInvariantCM::runOptimization(WIR_Function&)" );

  DOUT( "Processing function '" << f.getName() << "'." << endl );

  // Compute control tree for function f.
  WIR_StructuralAnalysis scfa( f );
  scfa.analyze();

  // Do a def-use/use-def chain analysis next.
  WIR_DUUDChainAnalysis duChain( f );
  duChain.analyze();

  // Finally, do a domination analysis.
  WIR_DominationAnalysis dom( f );
  dom.analyze();

  // Identify all loops in the current function in top-down sense.
  identifyLoops( f );

  // Identify and move loop-invariant code in each loop.
  for ( mCurrentLoop = mLoops.begin(); mCurrentLoop != mLoops.end();
        ++mCurrentLoop ) {
    identifyLIC( **mCurrentLoop );
    moveLIC();
  }

  // Free some no longer needed memory.
  f.eraseContainers( WIR_Domination::getContainerTypeID(), true );
  f.eraseContainers( WIR_DUUDChain::getContainerTypeID(), true );
  f.eraseContainers( WIR_ControlTree::getContainerTypeID() );

  mLoops.clear();
  mPreHeader.clear();
  mNewEntry.clear();
};


//
// Protected class methods
//

/*
  isLoopInvariant determines whether an unstructured string parameter is
  loop-invariant or not.

  Since this task is processor-specific and might or might not be necessary for
  some actual processor, this method is virtual and can be overloaded if
  required.
*/
bool WIR_LoopInvariantCM::isLoopInvariant( const WIR_StringParameter &s ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  (void) s;

  return( false );
}


//
// Private class methods
//

/*
  identifyLoops identifies all cyclic control structures in a given WIR
  function.
*/
void WIR_LoopInvariantCM::identifyLoops( const WIR_Function &f )
{
  DSTART( "void WIR_LoopInvariantCM::identifyLoops(const WIR_Function&)" );

  mEntry = nullptr;
  set<WIR_id_t> visitedNodes;
  WIR_ControlTreeNodeSet workSet;

  for ( WIR_BasicBlock &b : f ) {
    auto &c = b.getContainers<WIR_ControlTree>().begin()->get();
    auto &n = c.getBasicBlockTreeNode();

    visitedNodes.insert( n.getID() );

    if ( n.getParent() != n )
      workSet.insert( n.getParent() );
  }

  while ( !workSet.empty() )
    for ( auto it = workSet.begin(); it != workSet.end(); ) {
      auto &n = it->get();

      // Check whether all childs of the current node have been visited.
      bool allChildsVisited = true;
      for ( WIR_ControlTreeNode &ch : n.getChilds() )
        if ( !visitedNodes.count( ch.getID() ) ) {
          allChildsVisited = false;
          break;
        }

      if ( allChildsVisited ) {
        // Mark current node as visited.
        visitedNodes.insert( n.getID() );

        // Add current node to mLoops if so.
        if ( n.isCyclic() ) {
          mLoops.push_back( &n );
          DACTION(
            auto *entry = &(n.getEntry());
            while ( entry->getType() != WIR_CTNodeType::bb )
              entry = &(entry->getEntry());
            DOUT(
              "Identified loop with entry block '" <<
              dynamic_cast<const WIR_BasicBlockTreeNode *>(
                entry )->getBasicBlock().getName() << "' (ID " << n.getID() <<
                ")." << endl ); );
        }

        // Add parent node to work set if we didn't reach the control tree's
        // root yet.
        if ( n.getParent() != n )
          workSet.insert( n.getParent() );

        // Remove current node from the work set.
        it = workSet.erase( it );
      } else
        ++it;
    }
};


/*
  identifyLIC identifies loop-invariant code in a given loop.

  This method implements procedure Mark_Invar() from S. S. Muchnick, figure
  13.17, page 398.
*/
void WIR_LoopInvariantCM::identifyLIC( const WIR_ControlTreeNode &l )
{
  DSTART( "void WIR_LoopInvariantCM::identifyLIC(const WIR_ControlTreeNode&)" );

  // Determine the loop's basic blocks.
  mBSet = l.getBasicBlocks();

  // Determine whether the current loop l is an outer loop that directly
  // surrounds some already processed inner loop.
  list<WIR_ControlTreeNode *>::reverse_iterator processedLoop = mLoops.rbegin();
  while ( (*processedLoop)->getID() != l.getID() )
    ++processedLoop;
  ++processedLoop;

  WIR_BasicBlock *processedEntry = nullptr;
  while ( processedLoop != mLoops.rend() ) {
    // Determine the entry basic block of the previously processed loop.
    auto *entry = &((*processedLoop)->getEntry());
    while ( entry->getType() != WIR_CTNodeType::bb )
      entry = &(entry->getEntry());
    auto *b =
      &(dynamic_cast<const WIR_BasicBlockTreeNode *>( entry )->getBasicBlock());

    // Check whether the entry of the previously processed loop is also part of
    // mBSet.
    if ( mBSet.count( *b ) ) {
      processedEntry = b;
      break;
    } else
      ++processedLoop;
  }

  // Collect some data about register definitions, and patch some dominators for
  // the basic blocks of the currently processed loop.
  for ( WIR_BasicBlock &b : mBSet ) {
    for ( WIR_Instruction &i : b )
      for ( WIR_Operation &o : i )
        for ( WIR_Parameter &p : o )
          if ( p.getType() == WIR_ParameterType::reg ) {
            auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );

            if ( rp.isDefined() || rp.isDefUsed() ) {
              auto &r = rp.getRegister();

              for ( WIR_BaseRegister &l : r.getLeafs() ) {
                ++(mRegisterDefinitions[ l.getID() ]);

                if ( l.isVirtual() ) {
                  auto &vReg = dynamic_cast<WIR_VirtualRegister &>( l );
                  if ( vReg.isPrecolored() )
                    ++(mRegisterDefinitions[ vReg.getPrecolor().getID() ]);
                }
              }
            }
          }

    // In the case of nested loops, add the inner pre-header as dominator to all
    // relevant blocks of the current loop.
    if ( ( processedEntry != nullptr ) &&
         mPreHeader.count( (*processedLoop)->getID() ) ) {
      auto &doms = b.getContainers<WIR_Domination>().begin()->get();

      if ( doms.getDominatorBlocks().count( *processedEntry ) )
        doms.insertDominator( *(mPreHeader.at( (*processedLoop)->getID() )) );
    }
  }

  mIsForLoop = ( l.getType() == WIR_CTNodeType::whileloop );

  // Determine the entry block of the current loop.
  if ( mNewEntry.count( l.getID() ) )
    mEntry = mNewEntry[ l.getID() ];
  else {
    auto *entry = &(l.getEntry());
    while ( entry->getType() != WIR_CTNodeType::bb )
      entry = &(entry->getEntry());
    mEntry =
      &(dynamic_cast<const WIR_BasicBlockTreeNode *>( entry )->getBasicBlock());
  }

  DACTION(
    DOUT(
      "Processing loop with entry block '" << mEntry->getName() <<
      "', mBSet = {" );
    for ( WIR_BasicBlock &b : mBSet )
      DOUT( " " << b.getName() );
    DOUT( " }." << endl ); );

  // Determine all back-edges and exits in the current loop.
  set<WIR_id_t> visited;
  map<WIR_id_t, unsigned int> pre, post;
  unsigned int i = 1, j = 1;

  // A small lambda to perform a depth-first traversal of a loop's basic blocks.
  // This lambda implements procedure Depth_First_Search_PP() from S. S.
  // Muchnick, figure 7.12, page 180.
  function<void( WIR_BasicBlock & )> dfs = [&]( WIR_BasicBlock &b ) {
    visited.insert( b.getID() );
    pre[ b.getID() ] = j++;

    for ( WIR_BasicBlock &s : b.getSuccessors() )
      if ( mBSet.count( s ) ) {
        if ( !visited.count( s.getID() ) )
          dfs( s );
        else

        if ( ( pre[ b.getID() ] >= pre[ s.getID() ] ) &&
             ( !post.count( s.getID() ) ) ) {
          // s is a visited but unfinished successor, (b -> s) is a back-edge.
          mBackEdges.insert( { b, s } );
          DOUT(
            "Identified back-edge ('" << b.getName() << "' -> '" << s.getName()
            << "')." << endl );
        }
      } else
        mExits.insert( b );

    post[ b.getID() ] = i++;
  };
  dfs( *mEntry );

  // A small lambda to obtain the breadth-first order of a loop's basic blocks.
  // This lambda implements procedure Breadth_First() from S. S. Muchnick,
  // figure 7.13, page 181.
  list<WIR_BasicBlock *> bfsOrder { mEntry };
  visited.clear();
  visited.insert( mEntry->getID() );

  function<void( WIR_BasicBlock & )> bfs = [&]( const WIR_BasicBlock &b ) {
    list<WIR_BasicBlock *> T;

    for ( WIR_BasicBlock &t : b.getSuccessors() )
      if ( mBSet.count( t ) )
        if ( !visited.count( t.getID() ) ) {
          bfsOrder.push_back( &t );
          visited.insert( t.getID() );
          T.push_back( &t );
        }

    for ( WIR_BasicBlock *t : T )
      bfs( *t );
  };

  // Process the loop's basic blocks in breadth-first order.
  bfs( *mEntry );
  bool change;

  do {
    change = false;

    for ( auto *b : bfsOrder )
      change |= identifyLIC( *b );
  } while ( change );

  mBackEdges.clear();
  mRegisterDefinitions.clear();
  mInvariantInstrIDs.clear();
  mInvariantOpIDs.clear();
  mInvariantParamIDs.clear();
};


/*
  identifyLIC identifies loop-invariant code in a given basic block.

  This method implements procedure Mark_Block() from S. S. Muchnick, figure
  13.17, page 399.
*/
bool WIR_LoopInvariantCM::identifyLIC( const WIR_BasicBlock &b )
{
  DSTART( "bool WIR_LoopInvariantCM::identifyLIC(const WIR_BasicBlock&)" );

  DOUT( "Processing basic block '" << b.getName() << "'." << endl );

  bool change = false;

  // A small lambda to check whether i's basic block dominates all uses of some
  // register defined by by i within the loop (this implements criterion
  // Dom_Uses from S. S. Muchnick, page 403).
  auto domUses = [&]( const WIR_Instruction &i ) -> bool {
    for ( WIR_Operation &o : i )
      for ( WIR_Parameter &p : o )
        if ( p.getType() == WIR_ParameterType::reg ) {
          auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );

          if ( rp.isDefined() || rp.isDefUsed() ) {
            // Get the current definition's DU chain.
            auto &du = rp.getContainers<WIR_DUUDChain>().begin()->get();
            auto &duChain = du.getDUChains();

            for ( WIR_RegisterParameter &use : duChain ) {
              auto &b = use.getOperation().getInstruction().getBasicBlock();
              auto &doms = b.getContainers<WIR_Domination>().begin()->get();

              if ( mBSet.count( b ) &&
                    !doms.getDominatorBlocks().count( i.getBasicBlock() ) )
                return( false );
            }
          }
        }

    return( true );
  };

  // A small lambda to check whether i's basic block dominates all exits of the
  // loop (this implements criterion Dom_Exits from S. S. Muchnick, page 403).
  auto domExits = [&]( const WIR_Instruction &i ) -> bool {
    for ( WIR_BasicBlock &ex : mExits ) {
      auto &doms = ex.getContainers<WIR_Domination>().begin()->get();

      if ( !doms.getDominatorBlocks().count( i.getBasicBlock() ) )
        return( false );
    }

    return( true );
  };

  // Muchnick's domExits criterion above is very restrictive - it actually
  // prevents loop-invariant code motion for all kinds of regular for-/while-do
  // loops, since an instruction in the loop never dominates the loop exit (cf.
  // the discussion of this issue in S. S. Muchnick, bottom of page 403). Thus
  // the following lambda is an alternative check just for pre-checked
  // for-loops.
  auto domForLoop = [&]( const WIR_Instruction &i ) -> bool {
    // We allow code motion of i out of its loop if
    // 1) i's loop is a for-/while-do loop, and
    // 2) i dominates all exits of the loop except the header, and
    // 3) i dominates all source nodes of back-edges, and
    // 4a) no register defined by i is used outside the loop, or
    // 4b) the loop is executed at least once.

    // Criterion 1) above.
    if ( !mIsForLoop )
      return( false );

    // Criterion 2) above.
    for ( WIR_BasicBlock &ex : mExits )
      if ( ex != *mEntry ) {
        auto &doms = ex.getContainers<WIR_Domination>().begin()->get();

        if ( !doms.getDominatorBlocks().count( i.getBasicBlock() ) )
          return( false );
      }

    // Criterion 3) above.
    for ( auto &be : mBackEdges )
      if ( be.second.get() == *mEntry ) {
        auto &doms =
          be.first.get().getContainers<WIR_Domination>().begin()->get();

        if ( !doms.getDominatorBlocks().count( i.getBasicBlock() ) )
          return( false );
      }

    // Criterion 4a) above.
    bool allUsesInLoop = true;
    for ( WIR_Operation &o : i )
      for ( WIR_Parameter &p : o )
        if ( p.getType() == WIR_ParameterType::reg ) {
          auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );

          if ( rp.isDefined() || rp.isDefUsed() ) {
            // Get the current definition's DU chain.
            auto &du = rp.getContainers<WIR_DUUDChain>().begin()->get();
            auto &duChain = du.getDUChains();

            for ( WIR_RegisterParameter &use : duChain ) {
              auto &b = use.getOperation().getInstruction().getBasicBlock();

              if ( !mBSet.count( b ) )
                allUsesInLoop = false;
            }
          }
        }

    // Criterion 4b) above.
    bool loopExecutedOnce = false;
    // TODO: To be added based on, e.g., flow facts.

    return( allUsesInLoop || loopExecutedOnce );
  };

  for ( WIR_Instruction &i : b ) {
    if ( i.getDontOptimize() )
      continue;

    if ( mInvariantInstrIDs.count( i.getID() ) )
      // The instruction was already identified as loop-invariant.
      continue;

    // For each instruction i of b, check whether it has loop-constant operands
    // and appropriate reaching definitions. If so, mark it as loop-invariant.
    for ( WIR_Operation &o : i )
      checkLoopInvariant( o );

    bool isInvariant = true;
    for ( WIR_Operation &o : i )
      if ( !mInvariantOpIDs.count( o.getID() ) ) {
        isInvariant = false;
        break;
      }

    if ( isInvariant ) {
      // Identified a loop-invariant instruction.
      mInvariantInstrIDs.insert( i.getID() );

      if ( domUses( i ) && ( domExits( i ) || domForLoop( i ) ) ) {
        mInvariantInstrs.push_back( &i );

        DACTION(
          stringstream str;
          str << i;
          auto s = trim( str.str() );

          DOUT(
            "Found loop-invariant instruction '" <<
            s.substr( 0, s.length() - 1 ) << "' (ID " << i.getID() << ")." <<
            endl ); );

        change = true;
      } else
        // The dominators of instruction i don't match. We thus have to remove
        // all operations of i from map mInvariantOpIDs.
        for ( WIR_Operation &o : i )
          mInvariantOpIDs.erase( o.getID() );
    }
  }

  return( change );
};


/*
  moveLIC moves loop-invariant code out of a given loop.

  This method implements procedure Move_Invar() from S. S. Muchnick, figure
  13.32, page 404.
*/
void WIR_LoopInvariantCM::moveLIC( void )
{
  DSTART( "void WIR_LoopInvariantCM::moveLIC()" );

  if ( mInvariantInstrs.empty() ) {
    mBSet.clear();
    mExits.clear();
    return;
  }

  DACTION(
    DOUT(
      "Processing loop with entry block '" << mEntry->getName() <<
      "', mBSet = {" );
    for ( WIR_BasicBlock &b : mBSet )
      DOUT( " " << b.getName() );
    DOUT( " }." << endl ); );

  // Insert pre-header.
  auto &preHeader = insertPreHeader();

  // Iterate all loop-invariant instructions.
  while ( !mInvariantInstrs.empty() ) {
    auto &i = *(mInvariantInstrs.front());
    mInvariantInstrs.pop_front();

    DACTION(
      stringstream str;
      str << i;
      auto s = trim( str.str() );

      DOUT(
        "Moving instruction '" <<
        s.substr( 0, s.length() - 1 ) << "' (ID " << i.getID() <<
        ") from basic block '" << i.getBasicBlock().getName() << "' to '" <<
        preHeader.getName() << "'." << endl ); );

    // Move loop-invariant instructions to pre-header.
    preHeader.moveInstruction( i );
  }

  mBSet.clear();
  mExits.clear();
};


/*
  checkLoopInvariant checks whether a WIR operation is loop-invariant.

  If the operation is found to be loop-invariant, its ID is added to set
  mInvariantOpIDs.

  An operation is loop-invariant if, for each of its parameters (cf. S. S.
  Muchnick, page 397):
  1. the parameter is constant, or
  2. all definitions that reach this use of the parameter are located outside
     the current loop, or
  3. there is exactly one definition of the parameter that reaches the operation
     and that definition is an operation inside the loop that is itself
     loop-invariant.
  Calls, returns or jumps are never loop-invariant.
*/
void WIR_LoopInvariantCM::checkLoopInvariant( const WIR_Operation &o )
{
  DSTART(
    "void WIR_LoopInvariantCM::checkLoopInvariant(const WIR_Operation&)" );

  DOUT( "Processing operation '" << o << "'." << endl );

  if ( o.isMemoryAccess() || o.isMemoryLoad() || o.isMemoryStore() ||
       o.isCall() || o.isIndirectCall() || o.isReturn() ||
       o.isJump() || o.hasSideEffects() || o.getDontOptimize() )
    // The operation is "dangerous" and thus cannot be moved.
    return;

  if ( mInvariantOpIDs.count( o.getID() ) )
    // The operation was already identified as loop-invariant.
    return;

  // According to Andrew W. Appel, Modern Compiler Implementation in C (section
  // 18.2, page 418), we have to check that any register defined by o is only
  // defined once within the entire loop, i.e., by o only.
  for ( WIR_Parameter &p : o )
    if ( p.getType() == WIR_ParameterType::reg ) {
      auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );

      if ( rp.isDefined() || rp.isDefUsed() ) {
        auto &r = rp.getRegister();

        for ( WIR_BaseRegister &l : r.getLeafs() ) {
          if ( mRegisterDefinitions[ l.getID() ] != 1 )
            return;

          if ( l.isVirtual() ) {
            auto &vReg = dynamic_cast<WIR_VirtualRegister &>( l );
            if ( vReg.isPrecolored() &&
                 ( mRegisterDefinitions[ vReg.getPrecolor().getID() ] != 1 ) )
              return;
          }
        }
      }
    }

  for ( WIR_Parameter &p : o )
    checkLoopInvariant( p );

  bool isInvariant = true;
  for ( WIR_Parameter &p : o )
    if ( !mInvariantParamIDs.count( p.getID() ) ) {
      isInvariant = false;
      break;
    }

  if ( isInvariant ) {
    // Identified a loop-invariant operation.
    mInvariantOpIDs.insert( o.getID() );
    DOUT(
      "Operation is loop-invariant, added ID " << o.getID() <<
      " to mInvariantOpIDs." << endl );
  }
};


/*
  checkLoopInvariant checks whether a WIR parameter is loop-invariant.

  If the parameter is found to be loop-invariant, its ID is added to set
  mInvariantParamIDs.

  A parameter is loop-invariant if (cf. S. S. Muchnick, page 397):
  1. it is constant, or
  2. all definitions that reach this use of the parameter are located outside
     the current loop, or
  3. there is exactly one definition of the parameter that reaches the parameter
     and that definition is an operation inside the loop that is itself
     loop-invariant.
*/
void WIR_LoopInvariantCM::checkLoopInvariant( const WIR_Parameter &p )
{
  DSTART(
    "void WIR_LoopInvariantCM::checkLoopInvariant(const WIR_Parameter&)" );

  if ( mInvariantParamIDs.count( p.getID() ) )
    // The parameter was already identified as loop-invariant.
    return;

  DOUT( "Parameter '" << p << "' is " );

  // Handle unstructured string parameters in a generic fashion first.
  if ( p.getType() == WIR_ParameterType::str ) {
    if ( isLoopInvariant( dynamic_cast<const WIR_StringParameter &>( p ) ) )
      mInvariantParamIDs.insert( p.getID() );
    else {
      DOUT( "not " );
    }

    DOUT( "loop-invariant." << endl );
    return;
  }

  // Check if the parameter is constant (criterion 1 above). Effectively, all
  // parameters except register parameters are constant.
  if ( p.getType() != WIR_ParameterType::reg ) {
    // Identified a loop-invariant parameter.
    mInvariantParamIDs.insert( p.getID() );

    DOUT( "loop-invariant (criterion 1)." << endl );
    return;
  }

  // Check register parameters.
  auto &rp = dynamic_cast<const WIR_RegisterParameter &>( p );

  if ( rp.isDefined() ) {
    // Pure register definitions don't care here.
    mInvariantParamIDs.insert( p.getID() );

    DOUT( "loop-invariant (register definition)." << endl );
    return;
  }

  // Get the used parameter's UD chain.
  auto &ud = rp.getContainers<WIR_DUUDChain>().begin()->get();
  auto &udChain = ud.getUDChains();

  // Check if all reaching definitions of p are located outside the current
  // loop (criterion 2 above).
  bool allOutside = true;
  for ( WIR_RegisterParameter &def : udChain )
    if ( mBSet.count( def.getOperation().getInstruction().getBasicBlock() ) ) {
      allOutside = false;
      break;
    }

  if ( allOutside ) {
    // Identified a loop-invariant parameter.
    mInvariantParamIDs.insert( p.getID() );

    DOUT( "loop-invariant (criterion 2)." << endl );
    return;
  }

  // Check if there is exactly one definition of the parameter that reaches it
  // and that that definition is an operation inside the loop that is itself
  // loop-invariant (criterion 3 above).
  bool isInvariant = false;

  if ( udChain.size() == 1 ) {
    auto &defOp = udChain.begin()->get().getOperation();
    auto &defBlock = defOp.getInstruction().getBasicBlock();

    if ( mBSet.count( defBlock ) && mInvariantOpIDs.count( defOp.getID() ) ) {
      // We also have to check that defOp is executed before the current
      // parameter's operation (cf. S. S. Muchnick, Reach_Defs_In, page 400).
      if ( isExecutedBefore( defOp, p.getOperation() ) ) {
        // Finally, check that there are no uses of results produced by p's
        // operation in the loop before p's operation (cf. S. S. Muchnick,
        // Reach_Defs_In, page 400).
        bool allUsesOK = true;

        for ( WIR_Parameter &p1 : p.getOperation() )
          if ( p1.getType() == WIR_ParameterType::reg ) {
            auto &def = dynamic_cast<WIR_RegisterParameter &>( p1 );
            if ( def.isDefined() || def.isDefUsed() ) {
              // Get the current definition's DU chain.
              auto &du = rp.getContainers<WIR_DUUDChain>().begin()->get();
              auto &duChain = du.getDUChains();

              for ( WIR_RegisterParameter &use : duChain )
                // Ensure that no use is executed before the def in the loop.
                if ( mBSet.count(
                       use.getOperation().getInstruction().getBasicBlock() ) &&
                     isExecutedBefore(
                      use.getOperation(), p.getOperation() ) ) {
                  allUsesOK = false;
                  break;
                }
            }
          }

        if ( allUsesOK )
          isInvariant = true;
      }
    }
  }

  if ( isInvariant )
    // Identified a loop-invariant parameter.
    mInvariantParamIDs.insert( p.getID() );
  else {
    DOUT( "not " );
  }

  DOUT( "loop-invariant (criterion 3)." << endl );
};


/*
  isExecutedBefore determines whether one operation is always executed before
  another operation in the very same loop.
*/
bool WIR_LoopInvariantCM::isExecutedBefore( const WIR_Operation &o1,
                                            const WIR_Operation &o2 ) const
{
  DSTART(
    "bool WIR_LoopInvariantCM::isExecutedBefore(const WIR_Operation&, const WIR_Operation&) const" );

  auto &i1 = o1.getInstruction();
  auto &i2 = o2.getInstruction();

  auto &b1 = i1.getBasicBlock();
  auto &b2 = i2.getBasicBlock();

  if ( i1 == i2 )
    // Both operations lie in the very same instruction so that they are
    // executed in parallel.
    return( false );

  if ( b1 == b2 ) {
    // Both operations lie in the very same basic block so that we have to check
    // whether i1 comes before i2 in that block.
    for ( WIR_Instruction &i: b1 ) {
      if ( i == i1 )
        return ( true );
      else if ( i == i2 )
        return ( false );
    }
  }

  // A small lambda to perform a depth-first traversal of a loop's basic blocks.
  set<WIR_id_t> visited;
  function<void( WIR_BasicBlock & )> dfs = [&]( WIR_BasicBlock &b ) {
    visited.insert( b.getID() );

    for ( WIR_BasicBlock &s : b.getSuccessors() )
      if ( mBSet.count( s ) )
        if ( !visited.count( s.getID() ) &&
             !mBackEdges.count( { b, s } ) )
          // Successor s has not yet been visited, (b -> s) is a not back-edge.
          dfs( s );
  };

  // Determine all loop blocks reachable from b1 without using back-edges.
  dfs( b1 );

  if ( visited.count( b2.getID()  ) )
    return( true );
  else
    return( false );
};


/*
  insertPreHeader inserts a new pre-header basic block into which loop-invariant
  code will be moved.
*/
WIR_BasicBlock &WIR_LoopInvariantCM::insertPreHeader( void )
{
  DSTART( "WIR_BasicBlock& WIR_LoopInvariantCM::insertPreHeader()" );

  // Determine the entry's predecessors outside the current loop.
  auto &en = *mEntry;
  auto preds = en.getPredecessors();

  for ( auto it = preds.begin(); it != preds.end(); )
    if ( mBSet.count( it->get() ) )
      it = preds.erase( it );
    else
      ++it;

  // Create the new pre-header.
  auto &preHeader =
    en.getFunction().insertBasicBlock(
      en.getFunction().findBasicBlock( en ), {} )->get();
  mPreHeader[ (*mCurrentLoop)->getID() ] = &preHeader;

  DOUT(
    "Inserted pre-header '" + preHeader.getName() + "' in front of '" <<
    en.getName() << "'." << endl );

  // Copy dominators from the entry to the pre-header.
  auto &domEn = en.getContainers<WIR_Domination>().begin()->get();
  domEn.insertDominator( preHeader );
  preHeader.insertContainer( new WIR_Domination( domEn ) );

  // For all blocks of the current loop, add the pre-header to their dominators.
  for ( WIR_BasicBlock &b : mBSet ) {
    auto &dom = b.getContainers<WIR_Domination>().begin()->get();
    if ( dom.getDominatorBlocks().count( *mEntry ) )
      dom.insertDominator( preHeader );
  }

  // Determine the next loop surrounding the currently processed one, if any.
  auto *nextLoop = *mCurrentLoop;
  do
    nextLoop = &(nextLoop->getParent());
  while ( nextLoop->isAcyclic() && ( nextLoop->getParent() != *nextLoop ) );

  if ( nextLoop->isCyclic() && ( *nextLoop != **mCurrentLoop ) ) {
    // Determine the entry basic block of this next loop.
    auto *entry = &(nextLoop->getEntry());
    while ( entry->getType() != WIR_CTNodeType::bb )
      entry = &(entry->getEntry());
    WIR_BasicBlock *nextEntry =
      &(dynamic_cast<const WIR_BasicBlockTreeNode *>( entry )->getBasicBlock());

    // Add the current loop's new pre-header to the outer surrounding loop so
    // that we'll also consider the pre-header while optimizing the outer loop
    // later.
    nextLoop->insertChild( new WIR_BasicBlockTreeNode( preHeader ) );
    DOUT(
      "Inserted pre-header '" + preHeader.getName() + "' as new block node " <<
      "to outer loop with entry block '" + nextEntry->getName() + "'." <<
      endl );

    // Check whether the current and the outer loop share the very same basic
    // block as entries. If so, the outer loop needs to get a new entry block,
    // i.e., the newly generated pre-header of the current loop.
    if ( *nextEntry == en ) {
      mNewEntry[ nextLoop->getID() ] = &preHeader;
      DOUT(
        "Setting pre-header '" + preHeader.getName() + "' as new entry of " <<
        "outer loop with original entry block '" + nextEntry->getName() +
        "'." << endl );
    }
  }

  // Update control flow from the predecessors to the pre-header.
  for ( WIR_BasicBlock &pred : preds ) {
    DOUT(
      "Checking control flow edge '" << pred.getName() << "' -> '" <<
      en.getName() << "'." << endl );
    // From a predecessor, the loop can be reached via three different ways in
    // WIR code:
    // 1. An operation carries explicit jump target information.
    // 2. There is a jump operation explicitly branching to the loop.
    // 3. Neither of the above cases holds so that there is an implicit jump
    //    from the predecessor to the loop. This case requires no special
    //    handling below.
    for ( WIR_Instruction &i : pred ) {
      for ( WIR_Operation &o : i ) {
        // Handle criterion 1 above.
        if ( o.jumpTargetsAdded() ) {
          auto targets = o.getJumpTargets();
          for ( WIR_BasicBlock &tgt : targets )
            if ( tgt == en ) {
              o.eraseJumpTarget( en );
              o.addJumpTarget( preHeader );
              DOUT( "Changing jump target of '" << o << "'." << endl );
            }
        }

        // Handle criterion 2 above.
        if ( o.isJump() )
          // Change all label parameters from en to preHeader.
          for ( auto pIt = o.begin(); pIt != o.end(); ) {
            if ( pIt->get().getType() == WIR_ParameterType::label ) {
              auto &lp = dynamic_cast<WIR_LabelParameter &>( pIt->get() );

              if ( ( lp.getLabelType() == WIR_SymbolType::block ) &&
                   ( lp.getBasicBlock() == en ) ) {
                DOUT( "Changing jump target of '" << o << "'." << endl );
                pIt =
                  o.replaceParameter( pIt, WIR_LabelParameter { preHeader } );
              }
            }

            ++pIt;
          }
      }
    }
  }

  return( preHeader );
};

}       // namespace WIR
