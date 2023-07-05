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
  @file tcjumpcorrection.cc
  @brief This file implements a TriCore-specific optimimzation detecting and
         correcting jump instructions with too large displacements.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <algorithm>
#include <functional>
#include <list>
#include <sstream>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <analyses/lifeness/wirlifenessanalysis.h>
#include <arch/tricore/tc13.h>

// Include local headers
#include "tcjumpcorrection.h"


//
// Preprocessor macros
//

// #define ENABLE_INVARIANTS

#ifdef ENABLE_INVARIANTS

// This invariant verifies that the memory addresses as determined by the full
// WIR memory layout and by the internal light-weight data structure mMemLayout
// are the same for a WIR basic block.
#define DEBUG_MEMORYLAYOUT_INVARIANT( __b )                                    \
  {                                                                            \
    DSTART( "TC_JumpCorrection.invariants" );                                  \
    DACTION(                                                                   \
      DOUT(                                                                    \
        "Checking memory layout invariant for block '" << __b.getName() <<     \
        "' (0x" << hex << mBBPosition.at( __b.getID() )->address << ")." <<    \
        endl );                                                                \
      auto &__sym = mSystem.findSymbol( __b );                                 \
      bool __memLayoutInvariant =                                              \
        ( __sym.getBaseAddress().getContent() ==                               \
            mBBPosition.at( __b.getID() )->address );                          \
      if ( !__memLayoutInvariant )                                             \
        DOUT(                                                                  \
          "Memory Layout Invariant failed for basic block '" <<                \
          __b.getName() << "': memory layout = 0x" << hex <<                   \
          __sym.getBaseAddress().getContent() << ", mMemLayout = 0x" <<        \
          mBBPosition.at( __b.getID() )->address << endl );                    \
      ufAssert( __memLayoutInvariant );                                        \
    );                                                                         \
  }

#else

#define DEBUG_MEMORYLAYOUT_INVARIANT(...)

#endif


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
TC_JumpCorrection::TC_JumpCorrection( WIR_System &s, bool warn ) :
  WIR_JumpCorrection { s, warn },
  mFreeSpillAddress { 0 }
{
  DSTART( "TC_JumpCorrection::TC_JumpCorrection(WIR_System&, bool)" );
};


/*
  Destructor.
*/
TC_JumpCorrection::~TC_JumpCorrection( void )
{
  DSTART( "virtual TC_JumpCorrection::~TC_JumpCorrection()" );
};


/*
  getJumpCostsSize computes the size costs of a jump correction.

  This method computes the amount of bytes that are introduced to a basic block
  due to jump correction code if a jump from basic block s to t is corrected.
  Depending on addToSrc, the number of additional bytes for block s or t is
  computed, resp.
*/
int TC_JumpCorrection::getJumpCostsSize( const WIR_BasicBlock &s,
                                         const WIR_BasicBlock &t,
                                         bool addToSrc )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  // Avoid compiler warnings.
  (void) s;
  (void) t;

  if ( addToSrc )
    // According to generateLongJump, a movh.a, lea and ji operation are created
    // in the source basic block.
    return(
      TC13::OperationFormat::AL_1.getSize() +
      TC13::OperationFormat::AALC16BOA.getSize() +
      TC13::OperationFormat::SA.getSize() );

  return( 0 );
};


/*
  getJumpCostsCycles computes the timing costs of a jump correction.

  This method computes the amount of clock cycles that are introduced to a basic
  block due to jump correction code if a jump from basic block s to t is
  corrected. Depending on addToSrc, the number of additional clock cycles for
  block s or t is computed, resp.
*/
int TC_JumpCorrection::getJumpCostsCycles( const WIR_BasicBlock &s,
                                           const WIR_BasicBlock &t,
                                           bool addToSrc )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  // Avoid compiler warnings.
  (void) t;

  if ( addToSrc ) {
    // Determine ELF section of source basic block.
    auto &srcSection = mSystem.findSymbol( s ).getSection();
    if ( srcSection.getName() == ".text_spm" )
      return( 16 );
    else
      return( 18 );
  }

  return( 0 );
};


//
// Protected class methods
//

/*
  runOptimization performs jump correction in the given system.
*/
void TC_JumpCorrection::runOptimization( WIR_System &s )
{
  DSTART( "virtual void TC_JumpCorrection::runOptimization(WIR_System&)" );

  initializeMemoryLayout();

  do {
    mCorrectedJumps = 0;

    for ( WIR_CompilationUnit &c : s )
      WIR_JumpCorrection::runOptimization( c );
  } while ( ( mCorrectedJumps != 0 ) && !mSinglePass );

  mMemLayout.clear();
  mBBPosition.clear();
};


/*
  runOptimization performs jump correction in the given function.
*/
void TC_JumpCorrection::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void TC_JumpCorrection::runOptimization(WIR_Function&)" );

  // Determine the processor core to which f is mapped.
  auto &p = mSystem.findSymbol( f ).getSection().getProcessor();

  if ( dynamic_cast<TC13 *>( &p ) == nullptr )
    // Skip functions that are not assigned to a TriCore processor.
    return;

  // Before correcting a new function, indicate that there is no already
  // allocated but unused stack space for spills.
  mFreeSpillAddress = -1;

  {
    DSTART(
      "virtual void TC_JumpCorrection::runOptimization(WIR_Function&).visualize" );
    DACTION(
      WIR_CFG cfg { f };
      cfg.visualize( true ); );
  }

  auto it = f.begin();
  while ( it != f.end() ) {
    WIR_BasicBlock &b = it->get();

    DDECLARE(
      const unsigned int prevCorrections = mCorrectedJumps;
      const unsigned int prevInstructions = b.getInstructions().size();
      bool blockModified = false; );

    runOpt( b );

    DACTION(
      if ( ( mCorrectedJumps != prevCorrections ) ||
           ( b.getInstructions().size() != prevInstructions ) )
        blockModified = true; );

    if ( b.getInstructions().empty() ) {
      // If the current basic block is empty, e.g., due to removal of a
      // redundant jump or of a trampoline, let's remove it.

      // Determine first non-empty successor basic block (transitive).
      auto succIt = std::next( it );
      while ( ( succIt != f.end() ) &&
              succIt->get().getInstructions().empty() )
        ++succIt;

      if ( succIt != f.end() ) {
        // Found a non-empty successor block.

        // Replace all occurrences of b's label by this successor's label.
        for ( WIR_BasicBlock &pred : b.getPredecessors() ) {
          DOUT(
            "Checking predecessor '" << pred.getName() << "' of '" <<
            b.getName() << "'." << endl );

          for ( auto lastIns = pred.rbegin(); lastIns != pred.rend();
                ++lastIns ) {
            for ( WIR_Operation &o : lastIns->get() )
              for ( auto paramIt = o.begin(); paramIt != o.end(); ++paramIt )
                if ( paramIt->get().getType() == WIR_ParameterType::label ) {
                  WIR_LabelParameter &lp =
                    dynamic_cast<WIR_LabelParameter &>( paramIt->get() );

                  if ( ( lp.getLabelType() == WIR_SymbolType::block ) &&
                       ( lp.getBasicBlock() == b ) ) {
                    DOUT(
                      "Replacing label '" << lp.getBasicBlock().getName() <<
                      "' by '" << succIt->get().getName() << "'." << endl );
                    paramIt =
                      o.replaceParameter(
                        paramIt, WIR_LabelParameter( succIt->get() ) );
                  }
                }

            // Exit from the loop over all instructions, since only the very
            // last instruction of a basic block is allowed to be a jump with
            // labels.
            break;
          }
        }

        // Move containers from b to its non-empty successor.
        for ( WIR_BaseContainer &c : b.getContainers() )
          succIt->get().insertContainer( c );
        b.clearContainers();
      }

      // Update flow facts.

      // Since the successor basic block is executed every time that the empty
      // basic block is excecuted, the former can be replaced by the latter in
      // flow restrictions.
      WIR_FlowFactUpdater::replaceBasicBlock( b, succIt->get() );

      // There should not be any loop bounds attached to an empty basic block,
      // but just in case, we erase them now.
      WIR_FlowFactUpdater::eraseLoopBound( b );

      DOUT( "Removing empty basic block '" << b.getName() << "'." << endl );

      // Update memory layout.
      auto bbPos = mBBPosition[ b.getID() ];
      auto &r = mSystem.findSymbol( b ).getSection().getRegion();
      mMemLayout[ r.getID() ].erase( bbPos );
      mBBPosition.erase( b.getID() );

      it = f.eraseBasicBlock( it );
      DACTION( blockModified = true; );

      verifyMemoryLayout();
    } else
      ++it;

    {
      DSTART(
        "virtual void TC_JumpCorrection::runOptimization(WIR_Function&).visualize" );
      DACTION(
        if ( blockModified ) {
          WIR_CFG cfg { f };
          cfg.visualize( true );
        } );
    }
  }

  // Cleanup the current function, i.e., remove redundant basic blocks.

  // Traverse all basic blocks of f.
  for ( auto it = f.begin(); it != f.end(); ++it ) {
    WIR_BasicBlock &b1 = it->get();

    // Check if b1 ends with a call. If so, we skip b1.
    bool b1HasCall = false;
    if ( !b1.getInstructions().empty() ) {
      WIR_Instruction &i = b1.rbegin()->get();
      for ( WIR_Operation &o : i )
        if ( o.isCall() || o.isIndirectCall() ) {
          b1HasCall = true;
          break;
        }
    }
    if ( b1HasCall )
      continue;

    bool redundantSuccessorRemoved = false;

    do {
      // Determine successor block, if any.
      redundantSuccessorRemoved = false;
      auto it2 = std::next( it );

      if ( it2 != f.getBasicBlocks().end() ) {
        WIR_BasicBlock &b2 = it2->get();

        auto b1Succs = b1.getSuccessors();
        auto b2Preds = b2.getPredecessors();

        // b1 and b2 are redundant if
        // - b1 has exactly one successor, namely b2, AND
        // - b2 has exactly one predecessor, namely b1, AND
        // - b1 and b2 are allocated to the same memory region, AND
        // - b2 is the immediate successor of b1 within the current WIR function.
        if ( ( b1Succs.size() == 1 ) && ( b1Succs.begin()->get() == b2 ) &&
             ( b2Preds.size() == 1 ) && ( b2Preds.begin()->get() == b1 ) ) {
          // Found two redundant basic blocks b1 and b2 to be merged.
          DOUT(
            "Found redundant basic blocks '" + b1.getName() + "' and '" +
            b2.getName() + "'." << endl );

          // Remove potential jump operations from b1's tail.
          if ( !b1.getInstructions().empty() ) {
            WIR_Instruction &i = b1.rbegin()->get();
            for ( auto oit = i.begin(); oit != i.end(); ) {
              WIR_Operation &o = oit->get();
              if ( o.isJump() ) {
                DOUT( "Removing jump operation " << o << endl );
                o.setDontOptimize( false );
                i.setDontOptimize( false );
                oit = i.eraseOperation( oit );
              } else
                ++oit;
            }

            if ( i.getOperations().empty() ) {
              DOUT(
                "Removing empty last instruction of basic block '" +
                b1.getName() + "'." << endl );
              i.setDontOptimize( false );
              b1.eraseInstruction( --(b1.end()) );
            }
          }

          // Move instructions from b2 to b1.
          bool b2HasCall = false;
          while ( !b2.getInstructions().empty() ) {
            for ( WIR_Operation &o : b2.getInstructions().front().get() )
              if ( o.isCall() || o.isIndirectCall() )
                b2HasCall = true;

            b2.getInstructions().front().get().setDontOptimize( false );
            b1.moveInstruction( b2.getInstructions().front() );
          }

          // TODO: Flow Fact Update!

          // Because of their redundancy, both basic blocks are executed with
          // the same frequency. All occurences of b2 in Flow Restrictions can
          // thus be replaced by b1.
          WIR_FlowFactUpdater::replaceBasicBlock( b2, b1 );

          // There should not be any LoopBounds attached to an b2, (it cannot
          // an entry to a loop) but just in case, they are deleted.
          WIR_FlowFactUpdater::eraseLoopBound( b2 );

          // Move containers from b2 to b1.
          for ( WIR_BaseContainer &c : b2.getContainers() )
            b1.insertContainer( c );
          b2.clearContainers();

          // Finally, remove now redundant basic block b2.
          DOUT(
            "Removing redundant basic block '" + b2.getName() + "'." << endl );

          // Update memory layout.
          auto bbPos = mBBPosition[ b2.getID() ];
          auto &r = mSystem.findSymbol( b2 ).getSection().getRegion();
          mMemLayout[ r.getID() ].erase( bbPos );
          mBBPosition.erase( b2.getID() );

          f.eraseBasicBlock( it2 );
          redundantSuccessorRemoved = !b2HasCall;
          updateMemoryLayout( b1 );

          {
            DSTART(
              "virtual void TC_JumpCorrection::runOptimization(WIR_Function&).visualize" );
            DACTION(
              WIR_CFG cfg { f };
              cfg.visualize( true ); );
          }
        }
      }
    } while ( redundantSuccessorRemoved );
  }
};


/*
  runOpt performs jump correction in the given basic block.
*/
void TC_JumpCorrection::runOpt( WIR_BasicBlock &b )
{
  DSTART( "virtual void TC_JumpCorrection::runOpt(WIR_BasicBlock&)" );

  // Skip empty basic blocks.
  if ( b.getInstructions().empty() )
    return;

  DDECLARE( const unsigned int prevCorrections = mCorrectedJumps );
  DOUT( endl << "Checking basic block '" << b.getName() << "'." << endl );

  DEBUG_MEMORYLAYOUT_INVARIANT( b );

  // Determine the block's very last operation.
  WIR_Operation *o = nullptr;
  for ( auto iit = b.rbegin(); iit != b.rend(); ++iit )
    if ( !iit->get().getOperations().empty() ) {
      o = &(iit->get().getOperations().back().get());
      break;
    }

  if ( !o )
    return;

  if ( o->isConditionalJump() ) {
    DACTION(
      stringstream str;
      str << tricore << *o;
      DOUT(
        "Checking conditional jump '" << str.str().substr( 24 ) << "' (" <<
        o->getOperationFormat().getName() << ")." << endl ); );
    mCorrectedJumps += adjustConditionalJump( *o );
  } else

  if ( o->isCall() || o->isIndirectCall() ||
       ( o->isUnconditionalJump() && !isTCReturn( *o ) ) ) {
    if ( o->isCall() || o->isIndirectCall() ) {
      // Function calls return to their call-sites so that an implicit jump
      // needs to be checked here, too.
      DACTION(
        stringstream str;
        str << tricore << *o;
        DOUT(
          "Checking implicit jump of call '" << str.str().substr( 24 ) <<
          "' (" << o->getOperationFormat().getName() << ")." << endl ); );
      mCorrectedJumps += adjustImplicitJump( *o );
    }

    if ( o->isUnconditionalJump() || o->isCall() ) {
      DACTION(
        stringstream str;
        str << tricore << *o;
        DOUT(
          "Checking unconditional jump '" << str.str().substr( 24 ) << "' (" <<
          o->getOperationFormat().getName() << ")." << endl ); );
      mCorrectedJumps += adjustUnconditionalJump( *o );
    }
  } else {
    DACTION(
      stringstream str;
      str << tricore << *o;
      DOUT(
        "Checking implicit jump '" << str.str().substr( 24 ) << "' (" <<
        o->getOperationFormat().getName() << ")." << endl ); );
    mCorrectedJumps += adjustImplicitJump( *o );
  }

  DOUT(
    "Corrected " << mCorrectedJumps - prevCorrections << " invalid jump" <<
    string( mCorrectedJumps - prevCorrections != 1 ? "s" : "" ) << "." <<
    endl );

  if ( eraseTrampoline( b ) ) {
    DOUT( "Removed obsolete trampoline '" << b.getName() << "'." << endl );
  }
};


//
// Private class methods
//

/*
  Default constructor for invalid jump containers.
*/
TC_JumpCorrection::InvalidJump::InvalidJump( const WIR_Operation &o,
                                             const WIR_Symbol &s,
                                             const WIR_Symbol &t,
                                             const WIR_disp_t d,
                                             const bool i ) :
  op { o },
  srcSym { s },
  tgtSym { t },
  disp { d },
  isImplicit { i }
{
  DSTART(
    "TC_JumpCorrection::InvalidJump::InvalidJump(const WIR_Operation&, const WIR_Symbol&, const WIR_Symbol&, WIR_disp_t, bool)" );
};


/*
  adjustConditionalJump corrects invalid conditional jumps.
*/
unsigned int TC_JumpCorrection::adjustConditionalJump( WIR_Operation &o )
{
  DSTART(
    "unsigned int TC_JumpCorrection::adjustConditionalJump(WIR_Operation&)" );

  unsigned int count = 0;

  WIR_Instruction &i = o.getInstruction();
  WIR_BasicBlock &src = i.getBasicBlock();

  auto &srcSym = mSystem.findSymbol( src );
  auto &tgtSym = getJumpTarget( o );
  WIR_disp_t disp = computeDisplacement( src, tgtSym, o );
  WIR_BasicBlock &tgt = tgtSym.getBasicBlock();

  // Skip correction if the source basic block is simply a superfluous
  // trampoline.
  if ( eraseTrampoline( src ) )
    return( 1 );

  // Check if both implicit and explicit successors of the conditional jump are
  // the same basic blocks, i.e., if the number of successor blocks is 1.
  if ( src.getSuccessors().size() == 1 ) {
    DACTION(
      stringstream str;
      str << tricore << o;
      DOUT(
        "Removing obsolete conditional jump '" << str.str().substr( 24 ) <<
        "' in '" << src.getName() << "'." << endl ); );

    i.setDontOptimize( false );
    i.eraseOperation( i.findOperation( o ) );

    if ( i.getOperations().empty() )
      src.eraseInstruction( src.findInstruction( i ) );
    updateMemoryLayout( src );

    return( 1 );
  }

  DACTION(
    stringstream str;
    str << tricore << o;
    DOUT(
      "Correcting invalid conditional jump '" << str.str().substr( 24 ) <<
      "' in '" << src.getName() << "': actual displacement = 0x" << hex <<
      disp << dec << endl ); );

  // If the explicit jump target is physical successor of the jump's source
  // block, all implicit target blocks originally lying between source and
  // target in the WIR code have been moved to some different memory. Thus, the
  // jump's condition has to be inverted.
  if ( isPhysicalSuccessor( src, tgt ) ) {
    InvalidJump j { o, srcSym, tgtSym, disp, false };
    if ( fixInvalidCondition( j ) )
      ++count;
  }

  WIR_Operation &op = i.rbegin()->get();

  // Check implicit successor.
  auto &expTgtSym = getJumpTarget( op );
  WIR_BasicBlock &expTgtBB = expTgtSym.getBasicBlock();
  auto succs = src.getSuccessors();
  ufAssert( succs.size() == 2 );
  WIR_BasicBlock &impTgtBB =
    ( succs.begin()->get() == expTgtBB ) ?
    succs.rbegin()->get() : succs.begin()->get();

  if ( !isPhysicalSuccessor( src, impTgtBB ) ) {
    auto &impTgtSym = mSystem.findSymbol( impTgtBB );
    WIR_disp_t disp = computeDisplacement( src, impTgtSym, op );
    InvalidJump j { op, srcSym, impTgtSym, disp, true };

    DOUT(
      "Displacement of implicit successor = 0x" << hex <<
      getAddress( impTgtBB ) << " (tgt) - ( 0x" << getAddress( src ) <<
      " (src) + " << dec << src.getSize() << " (srcSize) ) = " << disp <<
      endl );

    DACTION(
      stringstream str;
      str << tricore << op;
      DOUT(
        "Correcting implicit jump of '" << str.str().substr( 24 ) << "' in '" <<
        src.getName() << "'." << endl ); );

    // Generate new basic block for the long jump to the implicit successor.
    auto bbIt = src.getFunction().findBasicBlock( src );
    WIR_BasicBlock &newBB =
      src.getFunction().insertBasicBlock( std::next( bbIt ), {} )->get();

    // Assign the new basic block to the same memory section as the source basic
    // block.
    mSystem.findSymbol( newBB ).setSection( srcSym.getSection() );

    DOUT(
      "Inserted new basic block '" << newBB.getName() << "' for long jump " <<
      "to implicit jump target '" << impTgtSym.getName() << "after block '" <<
      src.getName() << "'." << endl );

    // TODO: Update back-annotation!

    generateLongJump( j, newBB );

    // Update memory layout by newly inserted basic block. Consider
    // modifications in the whole WIR function, since generateLongJump might
    // insert spill code and stack adjustments etc.
    auto memPos = std::next( mBBPosition[ src.getID() ] );
    auto &r = srcSym.getSection().getRegion();
    auto newIt = mMemLayout[ r.getID() ].insert( memPos, { newBB, 0 } );
    mBBPosition[ newBB.getID() ] = newIt;
    updateMemoryLayout( src.getFunction() );

    ++count;
  }

  // Check explicit successor.
  disp = computeDisplacement( src, expTgtSym, op );
  // We use getDisplacementWidth + 1 here, since all jump displacements are
  // internally shifted left by 1 bit by the TriCore architecture.
  unsigned int dispWidth = getDisplacementWidth( op ) + 1;
  const bool noSignExt =
    ( ( op.getOperationFormat() == TC13::OperationFormat::SIC4L ) ||
      ( op.getOperationFormat() == TC13::OperationFormat::SIDL ) ||
      ( op.getOperationFormat() == TC13::OperationFormat::SDL ) ||
      ( op.getOperationFormat() == TC13::OperationFormat::SAL_1 ) ||
      ( op.getOperationFormat() == TC13::OperationFormat::SIC5L ) );

  if ( ( noSignExt && ( disp < 0 ) ) ||
       ( disp < TC_Const16_Signed::getMinValue( dispWidth ) ) ||
       ( disp > TC_Const16_Signed::getMaxValue( dispWidth ) ) ) {
    InvalidJump j { op, srcSym, expTgtSym, disp, false };

    DOUT(
      "Displacement of explicit successor = 0x" << hex <<
      getAddress( expTgtBB ) << " (tgt) - ( 0x" << getAddress( src ) <<
      " (src) + " << dec << src.getSize() << " (srcSize) ) = " << disp <<
      ( noSignExt ? " (no sign extension)" : "" ) << endl );

    if ( fixInvalidDisplacement( j ) ) {
      // Try to correct the jump by first using a 32-bit operation.
      DOUT( "Replacing 16-bit jump by 32-bit variant." << endl );
      ++count;
    } else {
      // Correction of the conditional jump.
      DACTION(
        stringstream str;
        str << tricore << op;
        DOUT(
          "Correcting conditional jump '" << str.str().substr( 24 ) <<
          "' in '" << src.getName() << "'." << endl ); );

      // For the correction, we need a new basic block acting as trampoline to
      // reach the explicit jump target. The position of such a new basic block
      // could be either after an unconditional jump or after a return.
      auto srcIt = src.getFunction().findBasicBlock( src );
      auto srcSectionPredecessor = srcIt;
      auto bbIt = std::next( srcIt );
      while ( bbIt != src.getFunction().getBasicBlocks().end() ) {
        auto &bbSym = mSystem.findSymbol( bbIt->get() );

        // Ensure that the potential position is in the same section as the
        // source basic block so that the control flow will definitely be valid.
        if ( bbSym.getSection() == srcSym.getSection() ) {
          // Determine last operation in bbbIt.
          WIR_Operation *o = nullptr;
          for ( auto iit = bbIt->get().rbegin(); iit != bbIt->get().rend();
                ++iit )
            if ( !iit->get().getOperations().empty() ) {
              o = &(iit->get().getOperations().back().get());
              break;
            }

          if ( o && ( o->isUnconditionalJump() || isTCReturn( *o ) ) )
            break;

          srcSectionPredecessor = bbIt;
        }

        ++bbIt;
      }

      // If we didn't find a suitable position after an unconditional jump or a
      // return, we must insert the trampoline directly after src.
      if ( bbIt == src.getFunction().getBasicBlocks().end() ) {
        srcSectionPredecessor = srcIt;
        bbIt = srcIt;
      }

      // Check if the identified trampoline position can be reached from the
      // original conditional jump.
      WIR_disp_t trampDisp =
        getAddress( bbIt->get() ) + bbIt->get().getSize() -
        ( getAddress( src ) + src.getSize() );
      DOUT( "Displacement to the trampoline is " << trampDisp << "." << endl );

      bool fixJumpToTrampoline = false;
      if ( trampDisp < TC_Const16_Signed::getMaxValue( dispWidth ) ) {
        // The trampoline could be reached with exactly that conditional jump
        // that shall actuall be corrected.
        DOUT(
          "Identified trampoline position is reachable by conditional jump." <<
          endl );
      } else

      if ( trampDisp < TC_Const16_Signed::getMaxValue( 16 ) ) {
        // The trampoline could be reached with a 32-bit conditional jump.
        DOUT(
          "Identified trampoline position is reachable by 32-bit variant of " <<
          "conditional jump." << endl );
        fixJumpToTrampoline = true;
      } else {
        // Trampoline position is too far away, we must insert the trampoline
        // directly after src.
        DOUT(
          "Identified trampoline position is not reachable by conditional " <<
          "jump. Inserting trampoline directly after '" << src.getName() <<
          "'." << endl );
        srcSectionPredecessor = srcIt;
        bbIt = srcIt;
      }

      // Now that the trampoline position is clear, let's create it.
      WIR_BasicBlock &newBB =
        src.getFunction().insertBasicBlock( std::next( bbIt ), {} )->get();

      // Assign the new basic block to the same memory section as the
      // source basic block.
      mSystem.findSymbol( newBB ).setSection( srcSym.getSection() );

      DOUT(
        "Inserted new basic block '" << newBB.getName() << "' as trampoline " <<
        "to '" << expTgtSym.getBasicBlock().getName() << "' after block '" <<
        bbIt->get().getName() << "'." << endl );

      // TODO: Update back-annotation!

      generateLongJump( j, newBB );

      // Update memory layout by newly inserted basic block. Consider
      // modifications in the whole WIR function, since generateLongJump might
      // insert spill code and stack adjustments etc.
      auto memPos =
        std::next( mBBPosition[ srcSectionPredecessor->get().getID() ] );
      auto &r = srcSym.getSection().getRegion();
      auto newIt = mMemLayout[ r.getID() ].insert( memPos, { newBB, 0 } );
      mBBPosition[ newBB.getID() ] = newIt;
      updateMemoryLayout( src.getFunction() );

      // Replace the original explicit jump target of the conditional jump by
      // the new trampoline.
      WIR_LabelParameter &p =
        dynamic_cast<WIR_LabelParameter &>(
          op.getExplicitParameters().back().get() );

      DACTION(
        stringstream str;
        str << tricore << op;
        DOUT(
          "Replacing '" << p.getName() << "' by '" << newBB.getName() <<
          "' in '" << str.str().substr( 24 ) << "'." << endl ); );

      p.setDontOptimize( false );
      i.setDontOptimize( false );
      op.replaceParameter( op.findParameter( p ), WIR_LabelParameter( newBB ) );

      // As very last step, we have to re-classify and eventually correct the
      // changed jump instruction.
      auto &newTgtSym = getJumpTarget( op );
      WIR_disp_t newTgtDisp = computeDisplacement( src, newTgtSym, op );
      InvalidJump newJ { op, srcSym, newTgtSym, newTgtDisp, false };

      // Replace 16-bit jump by 32-bit conditional jump if required.
      if ( fixJumpToTrampoline )
        ufAssertT(
          fixInvalidDisplacement( newJ ),
          "Failed to correct conditional jump to inserted trampoline." );

      // If the trampoline was directly inserted after src, the changed
      // conditional jump requires further correction.
      if ( bbIt == srcIt )
        count += adjustConditionalJump( op );

      updateMemoryLayout( src );

      ++count;
    }
  }

  return( count );
};


/*
  adjustImplicitJump corrects invalid implicit jumps.
*/
unsigned int TC_JumpCorrection::adjustImplicitJump( WIR_Operation &o )
{
  DSTART(
    "unsigned int TC_JumpCorrection::adjustImplicitJump(WIR_Operation&)" );

  unsigned int count = 0;

  WIR_BasicBlock &src = o.getInstruction().getBasicBlock();

  if ( src.getSuccessors().empty() )
    return( 0 );

  #ifdef FAILSAFEMODE
  ufAssert( src.getSuccessors().size() == 1 );
  #endif

  WIR_BasicBlock &tgt = src.getSuccessors().begin()->get();

  if ( !isPhysicalSuccessor( src, tgt ) ) {
    auto &srcSym = mSystem.findSymbol( src );
    auto &tgtSym = mSystem.findSymbol( tgt );
    WIR_disp_t disp = computeDisplacement( src, tgtSym, o );
    InvalidJump j { o, srcSym, tgtSym, disp, true };

    DOUT(
      "Displacement = 0x" << hex << getAddress( tgt ) << " (tgt) - ( 0x" <<
      getAddress( src ) << " (src) + " << dec << src.getSize() <<
      " (srcSize) ) = " << disp << endl );

    if ( !o.isConditionalJump() ) {
      if ( o.isCall() || o.isIndirectCall() || o.isUnconditionalJump() ) {
        if ( o.isCall() || o.isIndirectCall() ) {
          // Correction of an implicit jump after a function call.
          DACTION(
            stringstream str;
            str << tricore << o;
            DOUT(
              "Correcting implicit jump after call '" <<
              str.str().substr( 24 ) << "' in '" << src.getName() << "." <<
              endl ); );

          // If the last instruction is a call, we have to generate a new basic
          // block for the implicit long jump.
          auto bbIt = src.getFunction().findBasicBlock( src );
          WIR_BasicBlock &newBB =
            src.getFunction().insertBasicBlock( std::next( bbIt ), {} )->get();

          // Assign the new basic block to the same memory section as the
          // source basic block.
          mSystem.findSymbol( newBB ).setSection( srcSym.getSection() );

          DOUT(
            "Inserted new basic block '" << newBB.getName() << "' for long " <<
            "jump to function '" << src.getFunction().getName() <<
            "' after block '" << src.getName() << "'." << endl );

          // TODO: Update back-annotation!

          generateLongJump( j, newBB );

          // Update memory layout by newly inserted basic block. Consider
          // modifications in the whole WIR function, since generateLongJump
          // might insert spill code and stack adjustments etc.
          auto memPos = std::next( mBBPosition[ src.getID() ] );
          auto &r = srcSym.getSection().getRegion();
          auto newIt = mMemLayout[ r.getID() ].insert( memPos, { newBB, 0 } );
          mBBPosition[ newBB.getID() ] = newIt;
          updateMemoryLayout( src.getFunction() );

          ++count;
        }
      } else {
        // Correction of a purely implicit jump.
        DACTION(
          stringstream str;
          str << tricore << o;
          DOUT(
            "Correcting purely implicit jump '" << str.str().substr( 24 ) <<
            "' in '" << src.getName() << "'." << endl ); );

        generateLongJump( j, src );

        updateMemoryLayout( src.getFunction() );

        ++count;
      }
    }
  }

  return( count );
};


/*
  adjustUnconditionalJump corrects invalid unconditional jumps.
*/
unsigned int TC_JumpCorrection::adjustUnconditionalJump( WIR_Operation &o )
{
  DSTART(
    "unsigned int TC_JumpCorrection::adjustUnconditionalJump(WIR_Operation&)" );

  unsigned int count = 0;

  WIR_BasicBlock &src = o.getInstruction().getBasicBlock();

  auto &srcSym = mSystem.findSymbol( src );
  auto &tgtSym = getJumpTarget( o );
  WIR_disp_t disp =
    ( o.isCall() && tgtSym.isExtern() ) ?
      0 : computeDisplacement( src, tgtSym, o );
  // We use getDisplacementWidth + 1 here, since all jump displacements are
  // internally shifted left by 1 bit by the TriCore architecture.
  unsigned int dispWidth = getDisplacementWidth( o ) + 1;

  DACTION(
    stringstream str;
    str << tricore << o;
    DOUT(
      "Checking unconditional jump '" << str.str().substr( 24 ) << "' in '" <<
      src.getName() << "': displacement witdh = " << dispWidth <<
      ", legal displacements = [0x" << hex <<
      TC_Const16_Signed::getMinValue( dispWidth ) << ", 0x" <<
      TC_Const16_Signed::getMaxValue( dispWidth ) <<
      "], actual displacement = 0x" << disp << dec << "." << endl ); );

  if ( o.isCall() && tgtSym.isExtern() ) {
    // Correction of calls to external symbols.
    InvalidJump j { o, srcSym, tgtSym, disp, false };

    if ( fixInvalidDisplacement( j ) ) {
      DOUT(
        "Replacing 16-bit call to external symbol by 32-bit variant." << endl );
      ++count;
    }
  } else

  if ( o.getOpCode() == TC13::OpCode::CALLA ) {
    auto addr = getAddress( tgtSym.getFunction().begin()->get() );

    if ( addr & 0x0FE00001 ) {
      // Correction of an invalid CALLA by a CALL. A subsequent pass of the jump
      // correction will take care of the correct format of the CALL.
      DACTION(
        stringstream str;
        str << tricore << o;
        DOUT(
          "Correcting invalid absolute call '" << str.str().substr( 24 ) <<
          "' in '" << src.getName() << "'." << endl ); );

      // Generate a regular call in the original source basic block.
      auto &i = src.pushBackInstruction(
        { { TC13::OpCode::CALL,
            m16BitOperations ?
              TC13::OperationFormat::SL : TC13::OperationFormat::L,
            new WIR_LabelParameter( tgtSym.getFunction() ) } } );
      auto &call = i.rbegin()->get();

      // Copy implicit parameters from CALLA to CALL.
      for ( WIR_Parameter &p : o )
        if ( p.isImplicit() )
          call.pushBackParameter( p );

      // Remove the old, invalid call.
      o.getInstruction().setDontOptimize( false );
      src.eraseInstruction( src.findInstruction( o.getInstruction() ) );

      updateMemoryLayout( src.getFunction() );
      ++count;
    }
  } else

  if ( ( disp < TC_Const16_Signed::getMinValue( dispWidth ) ) ||
       ( disp > TC_Const16_Signed::getMaxValue( dispWidth ) ) ) {
    // Correction of an invalid unconditional jump.
    InvalidJump j { o, srcSym, tgtSym, disp, false };

    DACTION(
      stringstream str;
      str << tricore << o;
      DOUT(
        "Correcting invalid unconditional jump '" << str.str().substr( 24 ) <<
        "' in '" << src.getName() << "': legal displacements = [0x" << hex <<
        TC_Const16_Signed::getMinValue( dispWidth ) << ", 0x" <<
        TC_Const16_Signed::getMaxValue( dispWidth ) <<
        "], actual displacement = 0x" << disp << dec << "." << endl ); );

    if ( fixInvalidDisplacement( j ) ) {
      DOUT( "Replacing 16-bit jump by 32-bit variant." << endl );
      ++count;
    } else {
      // Correction of an unconditional jump.
      DACTION(
        stringstream str;
        str << tricore << o;
        DOUT(
          "Correcting unconditional jump '" << str.str().substr( 24 ) <<
          "' in '" << src.getName() << "'." << endl ); );

      // Generate a long jump in the original source basic block.
      generateLongJump( j, src );

      // Remove the old, invalid jump/call.
      o.getInstruction().setDontOptimize( false );
      src.eraseInstruction( src.findInstruction( o.getInstruction() ) );

      updateMemoryLayout( src.getFunction() );
    }

    ++count;
  } else

  if ( !( o.isCall() || o.isIndirectCall() ) &&
       isPhysicalSuccessor( src, tgtSym.getBasicBlock() ) ) {
    // If the jump target is physical successor of the source block, the
    // explicit unconditional jump can be removed. However, calls must not be
    // removed due their side effects.
    DACTION(
      stringstream str;
      str << tricore << o;
      DOUT(
        "Removing obsolete jump '" << str.str().substr( 24 ) << "' in '" <<
        src.getName() << "'." << endl ); );

    if ( o.isIndirectJump() ) {
      // In case of an indirect jump, we perform a def-use/use-def chain
      // analysis first in order to determine the movh.a and lea operations that
      // compute the target of the indirect jump.
      WIR_DUUDChainAnalysis duChain( src.getFunction() );
      duChain.analyze();

      // Determine the movh.a and lea operations defining the address register
      // of the indirect jump:
      //   movh.a %aReg, HI:foo
      //   lea    %aReg, [%aReg] LO:foo
      //   ji     %aReg / calli  %aReg

      auto &rp =
        dynamic_cast<WIR_RegisterParameter &>( o.getExplicitParameter( 1 ) );
      WIR_DUUDChain &c1 = rp.getContainers<WIR_DUUDChain>().begin()->get();
      auto &ud1 = c1.getUDChains();
      ufAssert( ud1.size() == 1 );

      WIR_Operation &lea = ud1.begin()->get().getOperation();
      ufAssert( lea.getOpCode() == TC13::OpCode::LEA );
      ufAssert( lea.getOperationFormat() == TC13::OperationFormat::AALC16BOA );
      auto &leaBlock = lea.getInstruction().getBasicBlock();

      auto &rp1 =
        dynamic_cast<WIR_RegisterParameter &>( lea.getExplicitParameter( 2 ) );
      WIR_DUUDChain &c2 = rp1.getContainers<WIR_DUUDChain>().begin()->get();
      auto &ud2 = c2.getUDChains();
      ufAssert( ud2.size() == 1 );

      WIR_Operation &movha = ud2.begin()->get().getOperation();
      ufAssert( movha.getOpCode() == TC13::OpCode::MOVH_A );
      ufAssert( movha.getOperationFormat() == TC13::OperationFormat::AL_1 );
      auto &movhaBlock = movha.getInstruction().getBasicBlock();

      // After having identified the movh.a and lea operations, check the DU
      // chain of the address register defined by lea. If the size of this DU
      // chain is 1, this definition of the address register reaches only one
      // use, namely our indirect jump o. So, we can remove both movh.a and lea.
      // If the DU chain contains more than 1 uses, we have to keep movh.a and
      // lea.
      auto &rp2 =
        dynamic_cast<WIR_RegisterParameter &>( lea.getExplicitParameter( 1 ) );
      WIR_DUUDChain &c3 = rp2.getContainers<WIR_DUUDChain>().begin()->get();
      auto &du1 = c3.getDUChains();

      if ( du1.size() == 1 ) {
        movhaBlock.eraseInstruction(
          movhaBlock.findInstruction( movha.getInstruction() ) );
        leaBlock.eraseInstruction(
          leaBlock.findInstruction( lea.getInstruction() ) );

        updateMemoryLayout( movhaBlock );
        updateMemoryLayout( leaBlock );
      }

      // Free some no longer needed memory.
      src.getFunction().eraseContainers(
        WIR_DUUDChain::getContainerTypeID(), true );
    }

    WIR_Instruction &i = o.getInstruction();
    i.setDontOptimize( false );
    i.eraseOperation( i.findOperation( o ) );

    if ( i.getOperations().empty() )
      src.eraseInstruction( src.findInstruction( i ) );

    updateMemoryLayout( src );

    ++count;
  }

  return( count );
};


/*
  computeDisplacement determines the displacement between the source basic block
  and the given symbol.
*/
WIR_disp_t TC_JumpCorrection::computeDisplacement( const WIR_BasicBlock &src,
                                                   const WIR_Symbol &tgtSym,
                                                   const WIR_Operation &o ) const
{
  DSTART(
    "WIR_disp_t TC_JumpCorrection::computeDisplacement(const WIR_BasicBlock&, const WIR_Symbol&, const WIR_Operation&) const" );

  auto &tgt =
    tgtSym.getType() == WIR_SymbolType::block ?
      tgtSym.getBasicBlock() : tgtSym.getFunction().begin()->get();

  DOUT(
    "src basic block = " << src.getName() << endl <<
    "tgt = " << tgt.getName() << endl <<
    "getAddress( tgt ) = " << getAddress( tgt ) << endl <<
    "getAddress( src ) = " << getAddress( src ) << endl <<
    "src.getSize() = " << src.getSize() << endl <<
    "o.getSize() = " << o.getSize() << endl );

  // The displacement is the distance between jump target and the current
  // program counter, i.e., the address of jump operation o.
  // o's address is defined by the source basic block's base address plus the
  // block's byte size minus o's byte size itself.
  WIR_disp_t res =
    getAddress( tgt ) - ( getAddress( src ) + src.getSize() - o.getSize() );

  DOUT( "Displacement = " << dec << res << "." << endl );

  return( res );
};


/*
  generateLongJump generates a code fragment for long-distance jumps.
*/
void TC_JumpCorrection::generateLongJump( const InvalidJump &jmp,
                                          WIR_BasicBlock &srcBB )
{
  DSTART(
    "void TC_JumpCorrection::generateLongJump(const TC_JumpCorrection::InvalidJump&, WIR_BasicBlock&)" );

  if ( !jmp.tgtSym.isExtern() ) {
    // If the target symbol is not extern, we may generate more compact code.
    bool shortJmp =
      ( jmp.disp >= TC_Const16_Signed::getMinValue( 25 ) ) &&
      ( jmp.disp <= TC_Const16_Signed::getMaxValue( 25 ) );

    DOUT(
      "Trampoline will be created in '" << srcBB.getName() <<
      "' to jump to '" << jmp.tgtSym.getName() << "' using a " <<
      string( shortJmp ? "short" : "long" ) << " jump instruction." << endl );

    // Handle short jumps.
    if ( ( jmp.disp >= TC_Const16_Signed::getMinValue( 9 ) ) &&
         ( jmp.disp <= TC_Const16_Signed::getMaxValue( 9 ) ) &&
         m16BitOperations ) {
      DOUT(
        "Creating 16-bit jump due to small displacement of " << jmp.disp <<
        "." << endl );

      srcBB.pushBackInstruction(
        { { TC13::OpCode::J, TC13::OperationFormat::SL,
            new WIR_LabelParameter( jmp.tgtSym.getBasicBlock() ) } } );
      return;
    }

    if ( shortJmp ) {
      DOUT(
        "Creating 32-bit jump due to displacement of " << jmp.disp << "." <<
        endl );

      srcBB.pushBackInstruction(
        { { TC13::OpCode::J, TC13::OperationFormat::L,
            new WIR_LabelParameter( jmp.tgtSym.getBasicBlock() ) } } );
      return;
    }

    // We can use JA if the jump targets a segment aligned to 256MB boundaries.
    auto &tgt =
      jmp.tgtSym.getType() == WIR_SymbolType::block ?
        jmp.tgtSym.getBasicBlock() : jmp.tgtSym.getFunction().begin()->get();

    if ( !( getAddress( tgt ) & 0x0FE00001 ) ) {
      if ( ( jmp.op.isCall() || jmp.op.isIndirectCall() ) && !jmp.isImplicit ) {
        DOUT(
          "Creating 32-bit absolute call due to displacement of " << jmp.disp <<
          " and target address of 0x" << hex << getAddress( tgt ) << "." <<
          endl );

        srcBB.pushBackInstruction(
          { { TC13::OpCode::CALLA, TC13::OperationFormat::L,
              new WIR_LabelParameter( jmp.tgtSym.getFunction() ) } } );
      } else {
        DOUT(
          "Creating 32-bit absolute jump due to displacement of " << jmp.disp <<
          " and target address of 0x" << hex << getAddress( tgt ) << "." <<
          endl );

        srcBB.pushBackInstruction(
          { { TC13::OpCode::JA, TC13::OperationFormat::L,
              new WIR_LabelParameter( jmp.tgtSym.getBasicBlock() ) } } );
      }

      return;
    }

  } else
    // If the target symbol is extern, we will generate a full trampoline below.
    // Right here, we only do a little sanity check.
    ufAssertT(
      !jmp.isImplicit,
      "Correction of an implicit jump to an external target is not possible." );

  // The handling of far jumps requires an address register to store the
  // indirect address. Unfortunately, register allocation is usually already
  // done when correcting jump displacements. So we have to add spill code if we
  // do not find any free address register.
  WIR_Instruction &i =
    srcBB.getInstructions().empty() ?
      srcBB.getPredecessors().begin()->get().rbegin()->get() :
      srcBB.rbegin()->get();
  WIR_Function &f = srcBB.getFunction();
  auto aRegInfo = determineAddressRegister( f, i );

  WIR_BaseRegister &aReg = aRegInfo.first.get();
  const bool spill = aRegInfo.second;
  int spillAddress = -1;
  auto &p =
    dynamic_cast<TC13 &>(
      mSystem.findSymbol( srcBB ).getSection().getProcessor() );

  if ( spill ) {
    // No free address register available, we have to spill.
    // Is there some space left from the last spill allocation?
    if ( mFreeSpillAddress > 0 ) {
      spillAddress = mFreeSpillAddress;
      mFreeSpillAddress = -1;
    } else {
      // We need 4 extra bytes on the stack. Since the TriCore stack pointer has
      // to be 8 byte-aligned, we have to allocate 8 bytes here.
      list<reference_wrapper<WIR_Instruction>> dummyList;
      TC13::adjustStack( f, 8, dummyList );

      // The 4 fresh bytes are available at SP + f.getFrameSize().
      // (f.getFrameSize() returns the size of the current function's overflow
      // region, as determined by the TriCore code selector.
      spillAddress = f.getFrameSize();

      // The remaining 4 bytes are available for a next spill allocation.
      mFreeSpillAddress = spillAddress + 4;
    }

    // Insert a spill-store: st.a [%sp]spillAddress, %aReg.
    srcBB.pushBackInstruction(
      { { TC13::OpCode::ST_A, TC13::OperationFormat::AC10ABOA,
          new WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
          new TC_Const10_Signed( spillAddress ),
          new WIR_RegisterParameter( aReg, WIR_Usage::use ) } } );
  }

  // Trampoline code:
  //   movh.a %aReg, HI:foo
  //   lea    %aReg, [%aReg] LO:foo
  //   ji     %aReg / calli  %aReg

  srcBB.pushBackInstruction(
    { { TC13::OpCode::MOVH_A, TC13::OperationFormat::AL_1,
        new WIR_RegisterParameter( aReg, WIR_Usage::def ),
        ( jmp.tgtSym.getType() == WIR_SymbolType::block ) ?
          new WIR_LabelParameter( jmp.tgtSym.getBasicBlock() ) :
          new WIR_LabelParameter( jmp.tgtSym.getFunction() ) } } );
  srcBB.pushBackInstruction(
    { { TC13::OpCode::LEA, TC13::OperationFormat::AALC16BOA,
        new WIR_RegisterParameter( aReg, WIR_Usage::def ),
        new WIR_RegisterParameter( aReg, WIR_Usage::use ),
        ( jmp.tgtSym.getType() == WIR_SymbolType::block ) ?
          new WIR_LabelParameter( jmp.tgtSym.getBasicBlock() ) :
          new WIR_LabelParameter( jmp.tgtSym.getFunction() ),
        new TC_Const16_Signed( 0 ) } } );

  if ( !( jmp.op.isCall() || jmp.op.isIndirectCall() ) || jmp.isImplicit ) {
    // We have to insert an indirect jump to correct the pass-through part of a
    // jump or the implicit jump of a call.
    auto &ji =
      srcBB.pushBackInstruction(
        { { TC13::OpCode::JI,
            m16BitOperations ?
              TC13::OperationFormat::SA : TC13::OperationFormat::A,
            new WIR_RegisterParameter(
              aReg, WIR_Usage::use ) } } ).begin()->get();
    ji.addJumpTarget( jmp.tgtSym.getBasicBlock() );

    if ( spill ) {
      // We need to insert a spill-load such that it always gets executed after
      // the JI generated above. So, the spill-load needs to be inserted into
      // the basic block being the JI's target. However, this is correct only if
      // this target block can be reached only via exactly one predecessor,
      // namely the JI's source block. If the target block can be reached via
      // other predecessors, there currently is no way to find a safe place
      // where to put the spill-load.
      auto &tgtBB = jmp.tgtSym.getBasicBlock();
      auto preds = tgtBB.getPredecessors();
      if ( ( preds.size() == 1 ) && ( preds.begin()->get() == srcBB ) )
        // Insert a spill-load: ld.a %aReg, [%sp]spillAddress.
        tgtBB.pushFrontInstruction(
          { { TC13::OpCode::LD_A, TC13::OperationFormat::AAC16BOA,
              new WIR_RegisterParameter( aReg, WIR_Usage::def ),
              new WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
              new TC_Const16_Signed( spillAddress ) } } );
      else
        throw(
          ufFatalError(
            "Internal error: Unable to insert required spill-load after JI.",
            true ) );
    }
  } else {
    // We have to insert an indirect call to correct a call.
    WIR_Operation &call =
      srcBB.pushBackInstruction(
        { { TC13::OpCode::CALLI, TC13::OperationFormat::A,
            new WIR_RegisterParameter(
              aReg, WIR_Usage::use ) } } ).begin()->get();

    // Copy implicit parameters of the original call.
    WIR_Instruction &i = srcBB.rbegin()->get();
    WIR_Operation &o = i.begin()->get();
    for ( WIR_Parameter &p : o )
      if ( p.isImplicit() ) {
        if ( p.getType() == WIR_ParameterType::addr )
          call.pushBackParameter(
            dynamic_cast<WIR_AddressingModeParameter &>( p ) );
        else
        if ( p.getType() == WIR_ParameterType::cond )
          call.pushBackParameter(
            dynamic_cast<WIR_ConditionFieldParameter &>( p ) );
        else
        if ( p.getType() == WIR_ParameterType::imm )
          call.pushBackParameter(
            dynamic_cast<WIR_BaseImmediateParameter &>( p ) );
        else
        if ( p.getType() == WIR_ParameterType::label )
          call.pushBackParameter( dynamic_cast<WIR_LabelParameter &>( p ) );
        else
        if ( p.getType() == WIR_ParameterType::reg )
          call.pushBackParameter( dynamic_cast<WIR_RegisterParameter &>( p ) );
        else
          call.pushBackParameter( dynamic_cast<WIR_StringParameter &>( p ) );
      }

    if ( spill ) {
      // If we spilled, a spill-load is added after the CALLI. Since the CALLI
      // terminates the current basic block, a new basic block is created just
      // for the spill-load.
      auto bbIt = f.findBasicBlock( srcBB );
      WIR_BasicBlock &newBB =
        f.insertBasicBlock( std::next( bbIt ), {} )->get();

      // Assign the new basic block to the same memory section as the
      // source basic block.
      auto &srcSym = mSystem.findSymbol( srcBB );
      mSystem.findSymbol( newBB ).setSection( srcSym.getSection() );

      // Insert a spill-load: ld.a %aReg, [%sp]spillAddress.
      newBB.pushBackInstruction(
        { { TC13::OpCode::LD_A, TC13::OperationFormat::AAC16BOA,
            new WIR_RegisterParameter( aReg, WIR_Usage::def ),
            new WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
            new TC_Const16_Signed( spillAddress ) } } );
    }
  }
};


/*
  eraseTrampoline checks if the specified basic block is a trampoline (i.e.,
  only contains one single jump operation) that is superfluous and removes it.
*/
bool TC_JumpCorrection::eraseTrampoline( WIR_BasicBlock &b )
{
  DSTART( "bool TC_JumpCorrection::eraseTrampoline(WIR_BasicBlock&)" );

  // Trampolines are basic blocks with only one operation.
  if ( ( b.getInstructions().size() == 0 ) ||
       ( b.getInstructions().size() > 1 ) ||
       ( ( b.getInstructions().size() == 1 ) &&
         ( b.begin()->get().getOperations().size() > 1 ) ) )
    return( false );

  WIR_Instruction &i = b.begin()->get();
  WIR_Operation &o = i.begin()->get();

  // A basic block with an implicit jump cannot be removed.
  if ( !o.isJump() )
    return( false );

  auto succs = b.getSuccessors();
  WIR_BasicBlock &succ = succs.begin()->get();

  // A trampoline featuring a conditional jump can only be removed if both
  // implicit and explicit successors of the conditional jump are the same basic
  // blocks, i.e., if the number of successor blocks is 1.
  if ( o.isConditionalJump() && ( succs.size() != 1 ) )
    return( false );

  // Move flow facts to the next and only succesor.
  // TODO: Flow-Fact Update!

  // Update all predecessors before removing the trampoline.
  bool removeTrampoline = true;
  for ( WIR_BasicBlock &pred : b.getPredecessors() ) {
    DOUT(
      "Checking predecessor '" << pred.getName() << "' of '" << b.getName() <<
      "'." << endl );

    // Determine the predecessor's very last operation.
    WIR_Operation *jmp = nullptr;
    for ( auto iit = pred.rbegin(); iit != pred.rend(); ++iit )
      if ( !iit->get().getOperations().empty() ) {
        jmp = &(iit->get().getOperations().back().get());
        break;
      }

    if ( jmp == nullptr )
      // Skip the predecessor if it is empty.
      continue;

    if ( jmp->isCall() || jmp->isIndirectCall() || !jmp->isJump() ||
         ( jmp->isConditionalJump() && ( getJumpTarget( *jmp ) != b ) ) ) {
      // Skip preceding calls or indirect jumps, nothing needs to be done here.
      removeTrampoline = false;
      continue;
    } else
      redirectJump( *jmp, succ );
  }

  // Finally, remove the jump instruction from the trampoline block. Cleanup of
  // the empty basic block will be done afterwards elsewhere.
  if ( removeTrampoline ) {
    b.clearInstructions();
    updateMemoryLayout( b );
  }

  return( removeTrampoline );
};


/*
  determineAddressRegister tries to determine a free TriCore adddress register
  for indirect jumps.
*/
std::pair<std::reference_wrapper<WIR_BaseRegister>, bool> TC_JumpCorrection::determineAddressRegister( WIR_Function &f,
                                                                                                       const WIR_Instruction &i ) const
{
  DSTART(
    "pair<reference_wrapper<WIR_BaseRegister>, bool> TC_JumpCorrection::determineAddressRegister(WIR_Function&, const WIR_Instruction&) const" );

  if ( mPhysicalWIR ) {
    // Look for a free physical register. Apply lifeness analysis first.
    WIR_LifenessAnalysis lta( f );
    lta.analyze();

    // Determine the actual processor core and its physical registers.
    auto &p =
      dynamic_cast<TC13 &>(
        mSystem.findSymbol( f ).getSection().getProcessor() );

    list<reference_wrapper<TC_ARegP>> candidateRegs
      { const_cast<TC_ARegP &>( p.A2() ), const_cast<TC_ARegP &>( p.A3() ),
        const_cast<TC_ARegP &>( p.A4() ), const_cast<TC_ARegP &>( p.A5() ),
        const_cast<TC_ARegP &>( p.A6() ), const_cast<TC_ARegP &>( p.A7() ),
        const_cast<TC_ARegP &>( p.A12() ), const_cast<TC_ARegP &>( p.A13() ),
        const_cast<TC_ARegP &>( p.A14() ), const_cast<TC_ARegP &>( p.A15() ) };

    // Get live-out set at instruction i.
    auto &ltaContainer = i.getContainers<WIR_LiveOut>().begin()->get();
    WIR_RegisterSet live = ltaContainer.getRegisters();

    // Iterate live-out set to determine an unused register.
    for ( auto it = live.begin();
          ( it != live.end() ) && !candidateRegs.empty(); ++it )
      for ( auto candidateIt = candidateRegs.begin();
            candidateIt != candidateRegs.end(); ++candidateIt )
        if ( candidateIt->get().getName() == it->get().getName() ) {
          DOUT(
            "Physical register '" << it->get().getName() << "' is live." <<
            endl );
          candidateRegs.erase( candidateIt );
          break;
        }

    // Free some no longer needed memory.
    f.eraseContainers( WIR_LiveOut::getContainerTypeID(), true );

    if ( !candidateRegs.empty() )
      // Return a physical address register if there is one left that is unused.
      return( make_pair( candidateRegs.front(), false ) );

    // Return A2 as spilling candidate otherwise.
    return( make_pair( ref( const_cast<TC_ARegP &>( p.A2() ) ), true ) );
  } else
    // Look for a free virtual register. I.e., simply create a new one.
    return(
      make_pair(
        ref(
          static_cast<WIR_BaseRegister &>(
            f.pushBackVirtualRegister( TC_ARegV() ) ) ),
        false ) );
};


/*
  getJumpTarget determines the symbol of the target of the given jump operation.
*/
WIR_Symbol &TC_JumpCorrection::getJumpTarget( const WIR_Operation &o ) const
{
  DSTART(
    "WIR_Symbol& TC_JumpCorrection::getJumpTarget(const WIR_Operation&) const" );

  WIR_Instruction &i = o.getInstruction();
  WIR_BasicBlock &srcBB = i.getBasicBlock();

  if ( !( o.isJump() || o.isCall() || o.isIndirectCall() ) ) {
    // Implicit jumps.
    DOUT(
      "Implicit jump to '" << srcBB.getSuccessors().begin()->get() <<
      "' found." << endl );
    return( mSystem.findSymbol( srcBB.getSuccessors().begin()->get() ) );
  } else

  if ( o.isIndirectCall() || o.isIndirectJump() ) {
    // Indirect jumps/calls.
    // We have to check a preceding mov.h/lea combination that is used to
    // specify the indirect jump's target.
    WIR_Operation &lea =
      ( o == i.getOperations().front() ) ?
        prev( srcBB.findInstruction( i ) )->get().rbegin()->get() :
        prev( i.findOperation( o ) )->get();

    ufAssert(
      ( lea.getOpCode() == TC13::OpCode::LEA ) &&
      ( lea.getOperationFormat() == TC13::OperationFormat::AALC16BOA ) );

    WIR_LabelParameter &p =
      dynamic_cast<WIR_LabelParameter &>( lea.getExplicitParameter( 3 ) );
    DOUT( "Indirect jump to '" << p.getName() << "' found." << endl );
    return(
      ( p.getLabelType() == WIR_SymbolType::block ) ?
        mSystem.findSymbol( p.getBasicBlock() ) :
        mSystem.findSymbol( p.getFunction() ) );
  } else {
    // Any other kind of jump/call.
    // Get the very last explicit parameter which stores the jump/call's label.
    WIR_LabelParameter &p =
      dynamic_cast<WIR_LabelParameter &>(
        o.getExplicitParameters().back().get() );

    DOUT( "Regular jump/call to '" << p.getName() << "' found." << endl );
    return(
      ( p.getLabelType() == WIR_SymbolType::block ) ?
        mSystem.findSymbol( p.getBasicBlock() ) :
        mSystem.findSymbol( p.getFunction() ) );
  }
};


/*
  getDisplacementWidth computes the maximal bit width of jump displacements for
  the given operation and its operation format.
*/
unsigned int TC_JumpCorrection::getDisplacementWidth( const WIR_Operation &o ) const
{
  DSTART(
    "unsigned int TC_JumpCorrection::getDisplacementWidth(const WIR_Operation&) const" );

  if ( !( o.isJump() || o.isCall() || o.isIndirectCall() ) )
    return( 0 );

  if ( ( o.getOperationFormat() == TC13::OperationFormat::A ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::SA ) )
    return( 32 );
  else

  if ( o.getOperationFormat() == TC13::OperationFormat::L ) {
    if ( o.getOpCode() == TC13::OpCode::LOOPU )
      return( 15 );
    else
      return( 24 );
  } else

  if ( ( o.getOperationFormat() == TC13::OperationFormat::DC4L_1 ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::DC4L_2 ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::DC4L_3 ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::DC5L ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::DDL_1 ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::DDL_2 ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::AL_2 ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::AL_3 ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::AAL ) )
    return( 15 );
  else

  if ( ( o.getOperationFormat() == TC13::OperationFormat::SL ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::SIL ) )
    return( 8 );
  else

  if ( ( o.getOperationFormat() == TC13::OperationFormat::SDL ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::SIDL ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::SIC4L ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::SIC5L ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::SAL_1 ) ||
       ( o.getOperationFormat() == TC13::OperationFormat::SAL_2 ) )
    return( 4 );

  return( 0 );
};


/*
  fixInvalidCondition tries to correct the given invalid conditional jump by
  inverting its test condition.
*/
bool TC_JumpCorrection::fixInvalidCondition( const InvalidJump &jmp )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &o = jmp.op;
  ufAssertT(
    o.isConditionalJump(),
    "Invalid attempt to fix an operation not being a conditional jump." );
  ufAssertT(
    jmp.disp == 0,
    "Invalid attempt to fix a conditional jump with non-zero displacement." );

  DACTION(
    stringstream str;
    str << tricore << o;
    DOUT(
      "Attempting to invert condition of jump '" << str.str().substr( 24 ) <<
      "' to target '" << jmp.tgtSym.getName() << "'." << endl ); );

  // Determine implicit successor of the conditional jump, i.e., that successor
  // block that is not the explicit jump target.
  WIR_Instruction &i = o.getInstruction();
  bool dontOptimize = i.getDontOptimize();
  i.setDontOptimize( false );
  WIR_BasicBlock &srcBB = i.getBasicBlock();
  unsigned long long srcBBSize = srcBB.getSize();
  WIR_BasicBlock &expTgtBB = jmp.tgtSym.getBasicBlock();
  auto succs = srcBB.getSuccessors();
  ufAssert( succs.size() == 2 );
  WIR_BasicBlock &impTgtBB =
    ( succs.begin()->get() == expTgtBB ) ?
    succs.rbegin()->get() : succs.begin()->get();

  WIR_RegisterParameter &p1 =
    dynamic_cast<WIR_RegisterParameter &>( o.getExplicitParameter( 1 ) );
  WIR_Parameter &p2 = o.getExplicitParameter( 2 );
  WIR_LabelParameter lp { impTgtBB };

  // Invert the jump's test condition.
  if ( ( o.getOpCode() == TC13::OpCode::JEQ ) ||
       ( o.getOpCode() == TC13::OpCode::JNE ) ||
       ( o.getOpCode() == TC13::OpCode::JEQ_A ) ||
       ( o.getOpCode() == TC13::OpCode::JNE_A ) ) {
    // jeq %Da, const4, <expTgt> -> jne %Da, const4, <impTgt>             resp.
    // jne %Da, const4, <expTgt> -> jeq %Da, const4, <impTgt>             resp.
    // jeq %Da, %Db, <expTgt> -> jne %Da, %Db, <impTgt>                   resp.
    // jne %Da, %Db, <expTgt> -> jeq %Da, %Db, <impTgt>                   resp.
    // jeq %d15, const4, <expTgt> -> jne %d15, const4, <impTgt>           resp.
    // jne %d15, const4, <expTgt> -> jeq %d15, const4, <impTgt>           resp.
    // jeq %d15, %Db, <expTgt> -> jne %d15, %Db, <impTgt>                 resp.
    // jne %d15, %Db, <expTgt> -> jeq %d15, %Db, <impTgt>                 resp.
    // jeq.a %Aa, %Ab, <expTgt> -> jne.a %Aa, %Ab, <impTgt>               resp.
    // jne.a %Aa, %Ab, <expTgt> -> jeq.a %Aa, %Ab, <impTgt>
    auto &oc =
      ( o.getOpCode() == TC13::OpCode::JEQ ) ?
        TC13::OpCode::JNE :
        ( o.getOpCode() == TC13::OpCode::JNE ) ?
          TC13::OpCode::JEQ :
          ( o.getOpCode() == TC13::OpCode::JEQ_A ) ?
            TC13::OpCode::JNE_A : TC13::OpCode::JEQ_A;
    i.pushBackOperation( { oc, o.getOperationFormat(), p1, p2, lp } );
  } else

  if ( ( o.getOpCode() == TC13::OpCode::JGE ) ||
       ( o.getOpCode() == TC13::OpCode::JLT ) ||
       ( o.getOpCode() == TC13::OpCode::JGE_U ) ||
       ( o.getOpCode() == TC13::OpCode::JLT_U ) ) {
    // jge %Da, const4, <expTgt> -> jlt %Da, const4, <impTgt>             resp.
    // jlt %Da, const4, <expTgt> -> jge %Da, const4, <impTgt>             resp.
    // jge %Da, %Db, <expTgt> -> jlt %Da, %Db, <impTgt>                   resp.
    // jlt %Da, %Db, <expTgt> -> jge %Da, %Db, <impTgt>                   resp.
    // jge.u %Da, uconst4, <expTgt> -> jlt.u %Da, uconst4, <impTgt>       resp.
    // jlt.u %Da, uconst4, <expTgt> -> jge.u %Da, uconst4, <impTgt>       resp.
    // jge.u %Da, %Db, <expTgt> -> jlt.u %Da, %Db, <impTgt>               resp.
    // jlt.u %Da, %Db, <expTgt> -> jge.u %Da, %Db, <impTgt>
    auto &oc =
      ( o.getOpCode() == TC13::OpCode::JGE ) ?
        TC13::OpCode::JLT :
        ( o.getOpCode() == TC13::OpCode::JLT ) ?
          TC13::OpCode::JGE :
          ( o.getOpCode() == TC13::OpCode::JGE_U ) ?
          TC13::OpCode::JLT_U : TC13::OpCode::JGE_U;
    i.pushBackOperation( { oc, o.getOperationFormat(), p1, p2, lp } );
  } else

  if ( o.getOpCode() == TC13::OpCode::JGEZ ) {
    // jgez %Da, <expTgt> -> jltz %Da, <impTgt>
    if ( m16BitOperations )
      i.pushBackOperation(
        { TC13::OpCode::JLTZ, TC13::OperationFormat::SDL, p1, lp } );
    else
      i.pushBackOperation(
        { TC13::OpCode::JLT, TC13::OperationFormat::DC4L_1, p1,
          new TC_Const4_Signed( 0 ), lp } );
  } else

  if ( o.getOpCode() == TC13::OpCode::JLTZ ) {
    // jltz %Da, <expTgt> -> jgez %Da, <impTgt>
    if ( m16BitOperations )
      i.pushBackOperation(
        { TC13::OpCode::JGEZ, TC13::OperationFormat::SDL, p1, lp } );
    else
      i.pushBackOperation(
        { TC13::OpCode::JGE, TC13::OperationFormat::DC4L_1, p1,
          new TC_Const4_Signed( 0 ), lp } );
  } else

  if ( o.getOpCode() == TC13::OpCode::JGTZ ) {
    // jgtz %Da, <expTgt> -> jlez %Da, <impTgt>                           resp.
    if ( m16BitOperations )
      i.pushBackOperation(
        { TC13::OpCode::JLEZ, TC13::OperationFormat::SDL, p1, lp } );
    else
      i.pushBackOperation(
        { TC13::OpCode::JLT, TC13::OperationFormat::DC4L_1, p1,
          new TC_Const4_Signed( 1 ), lp } );
  } else

  if ( o.getOpCode() == TC13::OpCode::JLEZ ) {
    // jlez %Da, <expTgt> -> jgtz %Da, <impTgt>
    if ( m16BitOperations )
      i.pushBackOperation(
        { TC13::OpCode::JGTZ, TC13::OperationFormat::SDL, p1, lp } );
    else
      i.pushBackOperation(
        { TC13::OpCode::JGE, TC13::OperationFormat::DC4L_1, p1,
          new TC_Const4_Signed( 1 ), lp } );
  } else

  if ( ( o.getOpCode() == TC13::OpCode::JNED ) ||
       ( o.getOpCode() == TC13::OpCode::JNEI ) ) {
    // srcBB:    jned/jnei %Da, %Db, <expTgt>
    // impTgtBB: <blabla>
    //           ...
    // expTgtBB: <foobar>
    //
    // becomes
    //
    // srcBB:    jeq %Da, %Db, <impTgt>
    // impTgtBB: add %Da, +/-1
    //           <blabla>
    //           ...
    // expTgtBB: add %Da, +/-1
    //           <foobar>
    //
    // Since the decrement/increment operations of JNED/JNEI are executed
    // unconditionally, this transformation is valid.
    if ( o.getOperationFormat() == TC13::OperationFormat::DC4L_3 )
      i.pushBackOperation(
        { TC13::OpCode::JEQ, TC13::OperationFormat::DC4L_1,
          WIR_RegisterParameter( p1.getRegister(), WIR_Usage::use ), p2, lp } );
    else
      i.pushBackOperation(
        { TC13::OpCode::JEQ, TC13::OperationFormat::DDL_1,
          WIR_RegisterParameter( p1.getRegister(), WIR_Usage::use ), p2, lp } );

    if ( m16BitOperations ) {
      expTgtBB.pushFrontInstruction(
        { { TC13::OpCode::ADD, TC13::OperationFormat::SDC4_2, p1,
            TC_Const4_Signed(
              o.getOpCode() == TC13::OpCode::JNED ? -1 : 1 ) } } );
      impTgtBB.pushFrontInstruction(
        { { TC13::OpCode::ADD, TC13::OperationFormat::SDC4_2, p1,
            TC_Const4_Signed(
              o.getOpCode() == TC13::OpCode::JNED ? -1 : 1 ) } } );
    } else {
      expTgtBB.pushFrontInstruction(
        { { TC13::OpCode::ADD, TC13::OperationFormat::DDC9_1,
            WIR_RegisterParameter( p1.getRegister(), WIR_Usage::def ),
            WIR_RegisterParameter( p1.getRegister(), WIR_Usage::use ),
            TC_Const9_Signed(
              o.getOpCode() == TC13::OpCode::JNED ? -1 : 1 ) } } );
      impTgtBB.pushFrontInstruction(
        { { TC13::OpCode::ADD, TC13::OperationFormat::DDC9_1,
            WIR_RegisterParameter( p1.getRegister(), WIR_Usage::def ),
            WIR_RegisterParameter( p1.getRegister(), WIR_Usage::use ),
            TC_Const9_Signed(
              o.getOpCode() == TC13::OpCode::JNED ? -1 : 1 ) } } );
    }
  } else

  if ( ( o.getOpCode() == TC13::OpCode::JNZ ) ||
       ( o.getOpCode() == TC13::OpCode::JZ ) ||
       ( o.getOpCode() == TC13::OpCode::JNZ_A ) ||
       ( o.getOpCode() == TC13::OpCode::JZ_A ) ) {
    // jnz %Da, <expTgt> -> jz %Da, <impTgt>                              resp.
    // jnz %d15, <expTgt> -> jz %d15, <impTgt>                            resp.
    // jz %Da, <expTgt> -> jnz %Da, <impTgt>                              resp.
    // jz %d15, <expTgt> -> jnz %d15, <impTgt>                            resp.
    // jnz.a %Da, <expTgt> -> jz.a %Da, <impTgt>                          resp.
    // jz.a %Da, <expTgt> -> jnz.a %Da, <impTgt>
    auto &oc =
      ( o.getOpCode() == TC13::OpCode::JNZ ) ?
        TC13::OpCode::JZ :
        ( o.getOpCode() == TC13::OpCode::JZ ) ?
          TC13::OpCode::JNZ :
          ( o.getOpCode() == TC13::OpCode::JNZ_A ) ?
            TC13::OpCode::JZ_A : TC13::OpCode::JNZ_A;
    i.pushBackOperation( { oc, o.getOperationFormat(), p1, lp } );
  } else

  if ( ( o.getOpCode() == TC13::OpCode::JNZ_T ) ||
       ( o.getOpCode() == TC13::OpCode::JZ_T ) )
    // jnz.t %Da, n, <expTgt> -> jz.t %Da, n, <impTgt>                    resp.
    // jnz.t %d15, n, <expTgt> -> jz.t %d15, n, <impTgt>                  resp.
    // jz.t %Da, n, <expTgt> -> jnz.t %Da, n, <impTgt>                    resp.
    // jz.t %d15, n, <expTgt> -> jnz.t %d15, n, <impTgt>
    i.pushBackOperation(
      { ( o.getOpCode() == TC13::OpCode::JNZ_T ) ?
          TC13::OpCode::JZ_T : TC13::OpCode::JNZ_T,
        o.getOperationFormat(), p1, p2, lp } );

  if ( i.getOperations().size() > 1 ) {
    DACTION(
      stringstream str1;
      stringstream str2;
      str1 << tricore << o;
      str2 << tricore << i.rbegin()->get();
      DOUT(
        "Replacing '" << str1.str().substr( 24 ) << "' by '" <<
        str2.str().substr( 24 ) << "'." << endl ); );

    // Copy implicit parameters first.
    for ( WIR_Parameter &p : o )
      if ( p.isImplicit() )
        i.getOperations().back().get().pushBackParameter( p );

    // Erase the original operation.
    i.eraseOperation( i.findOperation( o ) );
    i.setDontOptimize( dontOptimize );

    // If the size of the basic block changed, update the memory layout.
    if ( srcBB.getSize() != srcBBSize )
      updateMemoryLayout( srcBB );

    return( true );
  }

  i.setDontOptimize( dontOptimize );

  return( false );
};


/*
  fixInvalidDisplacement tries to correct the given invalid jump by replacing a
  16-bit operation by a 32-bit operation allowing for larger displacements.
*/
bool TC_JumpCorrection::fixInvalidDisplacement( const InvalidJump &jmp )
{
  DSTART(
    "bool TC_JumpCorrection::fixInvalidDisplacement(const TC_JumpCorrection::InvalidJump&)" );

  ufAssertT( !jmp.isImplicit, "Invalid attempt to fix an implicit jump." );
  auto &o = jmp.op;

  DACTION(
    stringstream str;
    str << tricore << o;
    DOUT(
      "Attempting to correct jump '" << str.str().substr( 24 ) <<
      "' to target '" << jmp.tgtSym.getName() << "'." << endl ); );

  if ( isTCReturn( o ) || ( getDisplacementWidth( o ) == 32 ) )
    // Indirect jumps have a displacement width of 32 and can jump freely.
    // Function returns behave like an indirect jump. Thus, nothing needs to be
    // done here.
    return( true );

  if ( ( o.getOpCode() == TC13::OpCode::JA ) ||
       ( o.getOpCode() == TC13::OpCode::JLA ) ||
       ( o.getOpCode() == TC13::OpCode::CALLA ) ) {
    // Check absolute jumps/calls with their TriCore-specific interpretation of
    // 24-bit displacements.
    unsigned int b0 = jmp.disp & 0x1;
    unsigned int b21to27 = jmp.disp & 0xFE00000;

    if ( ( b0 == 0 ) && ( b21to27 == 0 ) )
      // The current displacement is fine, nothing needs to be done here.
      return( true );
    else
      // The current displacement is not good, but we cannot correct it here
      // using some other kind of TriCore operation.
      return( false );
  }

  if ( o.getOpCode() == TC13::OpCode::LOOP ) {
    DOUT( "Unable to fix LOOP instructions." << endl );
    return( false );
  }

  if ( o.isConditionalJump() &&
       ( ( jmp.disp > TC_Const16_Signed::getMaxValue( 16 ) ) ||
         ( jmp.disp < TC_Const16_Signed::getMinValue( 16 ) ) ) ) {
    DOUT(
      "Current displacement 0x" << hex << jmp.disp << dec <<
      " exceeds 15 bits available for conditional jumps." << endl );
    return( false );
  }

  if ( ( o.isUnconditionalJump() || o.isCall() ) &&
       ( ( jmp.disp > TC_Const16_Signed::getMaxValue( 25 ) ) ||
         ( jmp.disp < TC_Const16_Signed::getMinValue( 25 ) ) ) ) {
    DOUT(
      "Current displacement 0x" << hex << jmp.disp << dec <<
      " exceeds 24 bits available for unconditional jumps." << endl );
    return( false );
  }

  WIR_Instruction &i = o.getInstruction();
  bool dontOptimize = i.getDontOptimize();
  i.setDontOptimize( false );
  auto &tgtSym = jmp.tgtSym;

  if ( o.getSize() == 2 ) {
    if ( o.getOpCode() == TC13::OpCode::CALL )
      // call disp8 -> call disp24
      i.pushBackOperation(
        { TC13::OpCode::CALL, TC13::OperationFormat::L,
          new WIR_LabelParameter( tgtSym.getFunction() ) } );
    else

    if ( o.getOpCode() == TC13::OpCode::J )
      // j disp8 -> j disp24
      i.pushBackOperation(
        { TC13::OpCode::J, TC13::OperationFormat::L,
          new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
    else

    if ( o.getOpCode() == TC13::OpCode::JEQ ) {
      if ( o.getOperationFormat() == TC13::OperationFormat::SIC4L )
        // jeq %d15, const4, disp4 -> jeq %d15, const4, disp15
        i.pushBackOperation(
          { TC13::OpCode::JEQ, TC13::OperationFormat::DC4L_1,
            new WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                o.getExplicitParameter( 1 ) ) ),
            new TC_Const4_Signed(
              dynamic_cast<TC_Const4_Signed &>( o.getExplicitParameter( 2 ) ) ),
            new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
      else
        // jeq %d15, %Db, disp4 -> jeq %d15, %Db, disp15
        i.pushBackOperation(
          { TC13::OpCode::JEQ, TC13::OperationFormat::DDL_1,
            new WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                o.getExplicitParameter( 1 ) ) ),
            new WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                o.getExplicitParameter( 2 ) ) ),
            new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
    } else

    if ( o.getOpCode() == TC13::OpCode::JGEZ )
      // jgez %Db, disp4 -> jge %Db, 0, disp15
      i.pushBackOperation(
        { TC13::OpCode::JGE, TC13::OperationFormat::DC4L_1,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ) ),
          new TC_Const4_Signed( 0 ),
          new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
    else

    if ( o.getOpCode() == TC13::OpCode::JGTZ )
      // jgtz %Db, disp4 -> jge %Db, 1, disp15
      i.pushBackOperation(
        { TC13::OpCode::JGE, TC13::OperationFormat::DC4L_1,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ) ),
          new TC_Const4_Signed( 1 ),
          new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
    else

    if ( o.getOpCode() == TC13::OpCode::JLEZ )
      // jlez %Db, disp4 -> jlt %Db, 1, disp15
      i.pushBackOperation(
        { TC13::OpCode::JLT, TC13::OperationFormat::DC4L_1,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ) ),
          new TC_Const4_Signed( 1 ),
          new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
    else

    if ( o.getOpCode() == TC13::OpCode::JLTZ )
      // jltz %Db, disp4 -> jlt %Db, 0, disp15
      i.pushBackOperation(
        { TC13::OpCode::JLT, TC13::OperationFormat::DC4L_1,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ) ),
          new TC_Const4_Signed( 0 ),
          new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
    else

    if ( o.getOpCode() == TC13::OpCode::JNE ) {
      if ( o.getOperationFormat() == TC13::OperationFormat::SIC4L )
        // jne %d15, const4, disp4 -> jne %d15, const4, disp15
        i.pushBackOperation(
          { TC13::OpCode::JNE, TC13::OperationFormat::DC4L_1,
            new WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                o.getExplicitParameter( 1 ) ) ),
            new TC_Const4_Signed(
              dynamic_cast<TC_Const4_Signed &>( o.getExplicitParameter( 2 ) ) ),
            new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
      else
        // jne %d15, %Db, disp4 -> jne %d15, %Db, disp15
        i.pushBackOperation(
          { TC13::OpCode::JNE, TC13::OperationFormat::DDL_1,
            new WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                o.getExplicitParameter( 1 ) ) ),
            new WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                o.getExplicitParameter( 2 ) ) ),
            new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
    } else

    if ( o.getOpCode() == TC13::OpCode::JNZ )
      // jnz %Db, disp4 OR jnz %d15, disp8 -> jne %Db, 0, disp15
      i.pushBackOperation(
        { TC13::OpCode::JNE, TC13::OperationFormat::DC4L_1,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ) ),
          new TC_Const4_Signed( 0 ),
          new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
    else

    if ( o.getOpCode() == TC13::OpCode::JNZ_A )
      // jnz.a %Aa, disp4 -> jnz.a %Aa, disp15
      i.pushBackOperation(
        { TC13::OpCode::JNZ_A, TC13::OperationFormat::AL_2,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ) ),
          new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
    else

    if ( o.getOpCode() == TC13::OpCode::JNZ_T )
      // jnz.t %d15, n, disp4 -> jnz.t %d15, n, disp15
      i.pushBackOperation(
        { TC13::OpCode::JNZ_T, TC13::OperationFormat::DC5L,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ) ),
          new TC_Const5_Unsigned(
            dynamic_cast<TC_Const5_Unsigned &>( o.getExplicitParameter( 2 ) ) ),
          new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
    else

    if ( o.getOpCode() == TC13::OpCode::JZ )
      // jz %Db, disp4 OR jz %d15, disp8 -> jeq %Db, 0, disp15
      i.pushBackOperation(
        { TC13::OpCode::JEQ, TC13::OperationFormat::DC4L_1,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ) ),
          new TC_Const4_Signed( 0 ),
          new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
    else

    if ( o.getOpCode() == TC13::OpCode::JZ_A )
      // jz.a %Aa, disp4 -> jz.a %Aa, disp15
      i.pushBackOperation(
        { TC13::OpCode::JZ_A, TC13::OperationFormat::AL_2,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ) ),
          new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
    else

    if ( o.getOpCode() == TC13::OpCode::JZ_T )
      // jz.t %d15, n, disp4 -> jz.t %d15, n, disp15
      i.pushBackOperation(
        { TC13::OpCode::JZ_T, TC13::OperationFormat::DC5L,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ) ),
          new TC_Const5_Unsigned(
            dynamic_cast<TC_Const5_Unsigned &>( o.getExplicitParameter( 2 ) ) ),
          new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
  } else {
    if ( o.getOpCode() == TC13::OpCode::LOOPU )
      // loopu disp15 -> j disp24
      i.pushBackOperation(
        { TC13::OpCode::J, TC13::OperationFormat::L,
          new WIR_LabelParameter( tgtSym.getBasicBlock() ) } );
  }

  if ( i.getOperations().size() > 1 ) {
    DACTION(
      stringstream str1;
      stringstream str2;
      str1 << tricore << o;
      str2 << tricore << i.rbegin()->get();
      DOUT(
        "Replacing '" << str1.str().substr( 24 ) << "' (" <<
        o.getOperationFormat().getName() << ") by '" <<
        str2.str().substr( 24 ) << "' (" <<
        i.rbegin()->get().getOperationFormat().getName() << ")." << endl ); );

    // Copy implicit parameters first.
    for ( WIR_Parameter &p : o )
      if ( p.isImplicit() )
        i.getOperations().back().get().pushBackParameter( p );

    // Erase the original operation.
    i.eraseOperation( i.findOperation( o ) );
    i.setDontOptimize( dontOptimize );

    updateMemoryLayout( i.getBasicBlock() );

    return( true );
  }

  i.setDontOptimize( dontOptimize );

  return( false );
};


/*
  redirectJump examines the specified operation and if it is a jump, it will be
  redirected to the given basic block.
*/
void TC_JumpCorrection::redirectJump( WIR_Operation &o,
                                      const WIR_BasicBlock &b ) const
{
  DSTART(
    "void TC_JumpCorrection::redirectJump(WIR_Operation&, const WIR_BasicBlock&) const" );

  if ( o.isIndirectJump() ) {
    WIR_Function &f = o.getInstruction().getBasicBlock().getFunction();

    // Do a def-use/use-def chain analysis first.
    WIR_DUUDChainAnalysis duChain( f );
    duChain.analyze();

    // Determine the movh.a and lea operations defining the address register
    // of the indirect jump:
    //   movh.a %aReg, HI:foo
    //   lea    %aReg, [%aReg] LO:foo
    //   ji     %aReg

    auto &rp =
      dynamic_cast<WIR_RegisterParameter &>( o.getExplicitParameter( 1 ) );
    WIR_DUUDChain &c1 = rp.getContainers<WIR_DUUDChain>().begin()->get();
    auto &ud1 = c1.getUDChains();
    ufAssert( ud1.size() == 1 );

    WIR_Operation &lea = ud1.begin()->get().getOperation();
    ufAssert( lea.getOpCode() == TC13::OpCode::LEA );
    ufAssert( lea.getOperationFormat() == TC13::OperationFormat::AALC16BOA );

    auto &rp1 =
      dynamic_cast<WIR_RegisterParameter &>( lea.getExplicitParameter( 2 ) );
    WIR_DUUDChain &c2 = rp1.getContainers<WIR_DUUDChain>().begin()->get();
    auto &ud2 = c2.getUDChains();
    ufAssert( ud2.size() == 1 );

    WIR_Operation &movha = ud2.begin()->get().getOperation();
    ufAssert( movha.getOpCode() == TC13::OpCode::MOVH_A );
    ufAssert( movha.getOperationFormat() == TC13::OperationFormat::AL_1 );

    // Free some no longer needed memory.
    f.eraseContainers( WIR_DUUDChain::getContainerTypeID(), true );

    // Replace the jump target in the identified movh.a and lea operations.
    WIR_BasicBlock &oldTgt =
      dynamic_cast<WIR_LabelParameter &>(
        movha.getExplicitParameters().back().get() ).getBasicBlock();

    DACTION(
      stringstream str1;
      stringstream str2;
      str1 << tricore << movha;
      str2 << tricore << lea;
      DOUT(
        "Replacing '" << oldTgt.getName() << "' by '" << b.getName() <<
        "' in '" << str1.str().substr( 24 ) << "' and '" <<
        str2.str().substr( 24 ) << "'." << endl ); );

    movha.replaceParameter(
      movha.findParameter( movha.getExplicitParameters().back().get() ),
      WIR_LabelParameter( b ) );
    lea.replaceParameter(
      lea.findParameter( lea.getExplicitParameters().back().get() ),
      WIR_LabelParameter( b ) );

    // Correct explicit control flow information of the indirect jump.
    o.eraseJumpTarget( oldTgt );
    o.addJumpTarget( b );
  } else

  if ( o.isConditionalJump() || o.isUnconditionalJump() ) {
    // Get the very last explicit parameter which stores the jump's label.
    WIR_LabelParameter &p =
      dynamic_cast<WIR_LabelParameter &>(
        o.getExplicitParameters().back().get() );

    // Replace the jump target in the jump operation.
    DACTION(
      stringstream str;
      str << tricore << o;
      DOUT(
        "Replacing '" << p.getName() << "' by '" << b.getName() <<
        "' in '" << str.str().substr( 24 ) << "'." << endl ); );

    o.replaceParameter(
      o.findParameter( p ), WIR_LabelParameter( b ) );
  }
};


/*
  isTCReturn checks whether the given operation is a TriCore-specific function
  return.

  isTCReturn checks whether o is classified as return operation or whether it is
  an indirect jump using physical register A11.
*/
bool TC_JumpCorrection::isTCReturn( const WIR_Operation &o ) const
{
  DSTART( "bool TC_JumpCorrection::isTCReturn(const WIR_Operation&) const" );

  if ( o.isReturn() )
    return( true );

  if ( o.isIndirectJump() ) {
    // Determine the indirect jump's address register.
    WIR_BaseRegister &aReg =
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameter( 1 ) ).getRegister();

    if ( TC13::isRA( aReg ) )
      return( true );
  }

  return( false );
};


/*
  getAddress determines the start address of a basic block.
*/
unsigned long long TC_JumpCorrection::getAddress( const WIR_BasicBlock &b ) const
{
  DSTART(
    "long long unsigned int TC_JumpCorrection::getAddress(const WIR_BasicBlock&) const" );

  DEBUG_MEMORYLAYOUT_INVARIANT( b );

  return( mBBPosition.at( b.getID() )->address );
};

}       // namespace WIR
