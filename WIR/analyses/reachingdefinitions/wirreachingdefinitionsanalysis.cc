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
  @file wirreachingdefinitionsanalysis.cc
  @brief This file implements the %WIR reaching definitions analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <iostream>
#include <map>
#include <utility>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <analyses/generic/wirbitvector.h>


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
WIR_ReachingDefinitionsAnalysis::WIR_ReachingDefinitionsAnalysis( WIR_Function &f ) :
  WIR_Analysis { f },
  mVirtualRegistersOnly { false },
  mProblemSize { 0 },
  mRegisterDefinitionsSize { 0 }
{
  DSTART(
    "WIR_ReachingDefinitionsAnalysis::WIR_ReachingDefinitionsAnalysis(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_ReachingDefinitionsAnalysis::~WIR_ReachingDefinitionsAnalysis( void )
{
  DSTART(
    "virtual WIR_ReachingDefinitionsAnalysis::~WIR_ReachingDefinitionsAnalysis()" );
};


/*
  setVirtualRegistersOnly sets whether the analysis should consider only virtual
  registers or whether both virtual and physical registers are analyzed.
*/
void WIR_ReachingDefinitionsAnalysis::setVirtualRegistersOnly( bool b )
{
  DSTART(
    "void WIR_ReachingDefinitionsAnalysis::setVirtualRegistersOnly(bool)" );

  mVirtualRegistersOnly = b;
};


/*
  getVirtualRegistersOnly returns whether only virtual or both virtual and
  physical registers are analyzed.
*/
bool WIR_ReachingDefinitionsAnalysis::getVirtualRegistersOnly( void ) const
{
  DSTART(
    "bool WIR_ReachingDefinitionsAnalysis::getVirtualRegistersOnly() const" );

  return( mVirtualRegistersOnly );
};


//
// Protected class methods
//

/*
  runAnalysis performs reaching definitions analysis by iteration of the given
  function.
*/
void WIR_ReachingDefinitionsAnalysis::runAnalysis( WIR_Function &f )
{
  DSTART(
    "virtual void WIR_ReachingDefinitionsAnalysis::runAnalysis(WIR_Function&)" );

  // Initialize instruction-level data structures.
  init( f );

  // Propagate instruction-level data to basic block-level.
  propagateIns2BB( f );

  // Get reverse postorder list of basic blocks.
  WIR_CFG cfg { f };
  auto &postOrderBBs = cfg.getReversePostOrder();

  // Compute reaching definitions by iteration (Appel, page 388) over basic
  // blocks.
  bool modified = false;

  do {
    modified = false;

    for ( WIR_BasicBlock &b : postOrderBBs )
      if ( mUpdateBlock[ b.getID() ] ) {
        DOUT( "Updating basic block '" << b.getName() << "'." << endl );
        mUpdateBlock[ b.getID() ] = false;

        auto &blockReachingDefsIn = mBlockReachingDefsIn.at( b.getID() );
        auto &blockReachingDefsOut = mBlockReachingDefsOut.at( b.getID() );
        auto &blockKill = mBlockKill.at( b.getID() );

        // Apply data flow equations.
        for ( WIR_BasicBlock &pred : mPredecessors[ b.getID() ] ) {
          DOUT(
            "Checking predecessor block '" << pred.getName() << "'." << endl );

          // mBlockReachingDefsIn[ b ] :=
          //   mBlockReachingDefsIn[ b ] U mBlockReachingDefsOut[ pred ]
          blockReachingDefsIn.set_union(
            mBlockReachingDefsOut.at( pred.getID() ) );

          // According to Appel and Muchnick:
          //   diff := mBlockReachingDefsIn[ b ] - mBlockKill[ b ]
          //   mBlockReachingDefsOut[ b ] := mBlockGen[ b ] U diff
          //
          // However, the data flow equations are monotone from iteration to
          // iteration, i.e., the sets of incoming/outgoing reaching definitions
          // can only increase. Thus, mBlockGen[ b ] in the above equation must
          // always be part of mBlockReachingDefsOut[ b ], it is independent of
          // this current analysis iteration. Hence, the consideration of
          // mBlockGen is already done in method propagateIns2BB below and not
          // here in the innermost loop of the analysis.
          // As a consequence, only those reaching definitions added to set diff
          // above must iteratively be added to mBlockReachingDefsOut[ b ].
          // Exactly this is done in the lines below.
          WIR_BitVector diff { blockReachingDefsIn };
          diff.set_difference( blockKill );
          blockReachingDefsOut.set_union( diff );
        }

        DACTION(
          DOUT( "  mBlockReachingDefsIn[ (" << b.getName() << ") ] = {" );

          for ( size_t j = 0; j < mRegisterDefinitionsSize; ++j )
            if ( blockReachingDefsIn[ j ] ) {
              auto hashIt = mReverseHash.find( j );
              WIR_RegisterParameter &rp = *(hashIt->second);
              unsigned int bbPos1 = 1;
              WIR_Instruction &i1 = rp.getOperation().getInstruction();
              for ( auto it = i1.getBasicBlock().getInstructions().begin();
                    (*it).get() != i1; ++it, ++bbPos1 ) ;
              unsigned int opPos = 1;
              for ( auto it = rp.getOperation().getParameters().begin();
                    it->get() != rp; ++it, ++opPos ) ;

              DOUT(
                " (" << i1.getBasicBlock().getName() << "/" << bbPos1 << "/" <<
                opPos << " " << rp << ")" );
            }
          for ( size_t j = mRegisterDefinitionsSize; j < mProblemSize; ++j )
            if ( blockReachingDefsIn[ j ] ) {
              auto hashIt = mReverseHashInputs.find( j );
              DOUT( " (External Input " << hashIt->second->getName() << ")" );
            }
          DOUT( " }" << endl );

          DOUT( "  mBlockReachingDefsOut[ (" << b.getName() << ") ] = {" );

          for ( size_t j = 0; j < mRegisterDefinitionsSize; ++j )
            if ( blockReachingDefsOut[ j ] ) {
              auto hashIt = mReverseHash.find( j );
              unsigned int bbPos1 = 1;
              WIR_RegisterParameter &rp = *(hashIt->second);
              WIR_Instruction &i1 = rp.getOperation().getInstruction();
              for ( auto it = i1.getBasicBlock().getInstructions().begin();
                    (*it).get() != i1; ++it, ++bbPos1 ) ;
                unsigned int opPos = 1;
                for ( auto it = rp.getOperation().getParameters().begin();
                      it->get() != rp; ++it, ++opPos ) ;

              DOUT(
                " (" << i1.getBasicBlock().getName() << "/" << bbPos1 << "/" <<
                opPos << " " << rp << ")" );
            }
          for ( size_t j = mRegisterDefinitionsSize; j < mProblemSize; ++j )
            if ( blockReachingDefsOut[ j ] ) {
              auto hashIt = mReverseHashInputs.find( j );
              DOUT( " (External Input " << hashIt->second->getName() << ")" );
            }
          DOUT( " }" << endl ); );

        // Check if reaching definition sets were modified in this iteration.
        // Set update flag of successor blocks if necessary.
        if ( blockReachingDefsOut.isModified() ) {
          modified = true;

          for ( WIR_BasicBlock &succ : mSuccessors[ b.getID() ] )
            mUpdateBlock[ succ.getID() ] = true;
        }

        // Check if incoming reaching definition sets were modified in this
        // iteration.
        modified |= blockReachingDefsIn.isModified();
      }
  } while ( modified );

  // Propagate basic block-level analysis data to instruction-level.
  propagateBB2Ins( f );
};


//
// Private class methods
//

/*
  init initializes internal data structures by collecting information about
  register definitions, kill and gen sets and of predecessor/successor relations
  between basic blocks.
*/
void WIR_ReachingDefinitionsAnalysis::init( WIR_Function &f )
{
  DSTART( "void WIR_ReachingDefinitionsAnalysis::init(WIR_Function&)" );

  // Build reverse-map of the function's precolors, i.e., a map mapping a
  // physical register 'pr' to the set of virtual registers that are all
  // precolored with 'pr'.
  map<WIR_id_t, WIR_VirtualRegisterSet> precolRMap;

  for ( WIR_VirtualRegister &vr : f.getVirtualRegisters() )
    for ( WIR_VirtualRegister &leaf : vr.getLeafs() )
      if ( leaf.isPrecolored() ) {
        WIR_PhysicalRegister &pr = leaf.getPrecolor();

        auto &vregSet = precolRMap[ pr.getID() ];
        vregSet.insert( ref( leaf ) );
      }

  // Compute information about the data flow lattice, i.e., the number of
  // involved register definitions and the hash and reverse-hash functions for
  // the used bit vectors.
  mProblemSize = 0;
  mHash.clear();
  mReverseHash.clear();
  mReverseHashInputs.clear();

  set<WIR_id_t> consideredDefinitions;

  // A small lambda to consider a register's definition during analysis.
  auto considerDef = [&]( WIR_RegisterParameter &rp ) {
    if ( !consideredDefinitions.count( rp.getID() ) ) {
      mHash[ rp.getID() ] = mProblemSize;
      consideredDefinitions.insert( rp.getID() );
    }
    mReverseHash.insert( { mProblemSize, &rp } );
    mProblemSize++;
  };

  set<WIR_id_t> tmpIDs;

  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      for ( WIR_Operation &o : i )
        for ( WIR_Parameter &p : o )
          if ( p.getType() == WIR_ParameterType::reg ) {
            auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );

            if ( rp.isDefined() || rp.isDefUsed() ) {
              WIR_BaseRegister &r = rp.getRegister();

              if ( r.isVirtual() )
                for ( WIR_VirtualRegister &leaf :
                        dynamic_cast<WIR_VirtualRegister &>( r ).getLeafs() ) {
                  tmpIDs.insert( leaf.getID() );
                  considerDef( rp );

                  if ( !mVirtualRegistersOnly && leaf.isPrecolored() ) {
                    auto &pr = leaf.getPrecolor();
                    tmpIDs.insert( pr.getID() );

                    auto it = precolRMap.find( pr.getID() );
                    if ( it != precolRMap.end() )
                      for ( WIR_VirtualRegister &vr : it->second )
                        tmpIDs.insert( vr.getID() );
                  }
                }
              else

              if ( !mVirtualRegistersOnly )
                for ( WIR_PhysicalRegister &leaf :
                        dynamic_cast<WIR_PhysicalRegister &>( r ).getLeafs() ) {
                  tmpIDs.insert( leaf.getID() );
                  considerDef( rp );

                  auto it = precolRMap.find( leaf.getID() );
                  if ( it != precolRMap.end() )
                    for ( WIR_VirtualRegister &vr : it->second )
                      tmpIDs.insert( vr.getID() );
                }
            }
          }

  // A small lambda to consider a function's external inputs during analysis.
  auto considerInput = [&]( WIR_PhysicalRegister &p ) {
    mHash[ p.getID() ] = mProblemSize;
    mReverseHashInputs.insert( { mProblemSize, &p } );
    mProblemSize++;
  };

  WIR_PhysicalRegisterSet inputRegs;
  for ( WIR_PhysicalRegister &p : f.getFunctionInputs() )
    for ( WIR_PhysicalRegister &leaf : p.getLeafs() ) {
      inputRegs.insert( leaf );
      tmpIDs.insert( leaf.getID() );
    }

  // Add positions in the bit vectors for each implicit DEF of a function's
  // external input.
  mRegisterDefinitionsSize = mProblemSize;
  for ( WIR_PhysicalRegister &p : inputRegs )
    considerInput( p );

  // defs maps a register's ID to a bit vector of all definitions of this
  // register.
  map<WIR_id_t, WIR_BitVector> defs;

  for ( auto id : tmpIDs )
    defs.insert( { id, WIR_BitVector { mProblemSize } } );
  tmpIDs.clear();

  // definedParams is used to collect all register parameters that define some
  // register.
  WIR_RegisterParameterSet definedParams;

  // Initialize data structures.
  mInstrGen.clear();
  mInstrKill.clear();
  mBlockKill.clear();
  mBlockReachingDefsIn.clear();
  mBlockReachingDefsOut.clear();
  mPredecessors.clear();
  mSuccessors.clear();
  mUpdateBlock.clear();

  // Compute sets of parameters defining a register and gen sets per
  // instruction.
  for ( WIR_BasicBlock &b : f ) {

    for ( WIR_BasicBlock &pred : b.getPredecessors() )
      mPredecessors[ b.getID() ].insert( pred );
    for ( WIR_BasicBlock &succ : b.getSuccessors() )
      mSuccessors[ b.getID() ].insert( succ );
    mUpdateBlock[ b.getID() ] = true;

    for ( WIR_Instruction &i : b ) {
      auto &instrGen =
        mInstrGen.insert(
          { i.getID(), WIR_BitVector { mProblemSize } } ).first->second;
      mInstrKill.insert( { i.getID(), WIR_BitVector { mProblemSize } } );

      // Clear previous analysis results.
      i.eraseContainers( WIR_ReachingDefinitions::getContainerTypeID() );

      for ( WIR_Operation &o : i )
        for ( WIR_Parameter &p : o )
          if ( p.getType() == WIR_ParameterType::reg ) {
            auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );

            if ( rp.isDefined() || rp.isDefUsed() ) {
              definedParams.insert( rp );
              WIR_BaseRegister &r = rp.getRegister();
              unsigned int rpIdx = mHash[ rp.getID() ];

              if ( r.isVirtual() )
                for ( WIR_VirtualRegister &leaf :
                        dynamic_cast<WIR_VirtualRegister &>( r ).getLeafs() ) {
                  // defs[ leaf ] := defs[ leaf ] U { rp }
                  defs.at( leaf.getID() ).setBit( rpIdx );

                  // mInstrGen[ i ] := mInstrGen[ i ] U { rp }
                  instrGen.setBit( rpIdx );

                  // Also consider precolorings if desired.
                  if ( !mVirtualRegistersOnly && leaf.isPrecolored() ) {
                    WIR_PhysicalRegister &pr = leaf.getPrecolor();

                    defs.at( pr.getID() ).setBit( rpIdx );
                    instrGen.setBit( rpIdx );
                  }

                  ++rpIdx;
                }
              else

              if ( !mVirtualRegistersOnly )
                for ( WIR_PhysicalRegister &leaf :
                        dynamic_cast<WIR_PhysicalRegister &>( r ).getLeafs() ) {
                  // defs[ leaf ] := defs[ leaf ] U { rp }
                  defs.at( leaf.getID() ).setBit( rpIdx );

                  // mInstrGen[ i ] := mInstrGen[ i ] U { rp }
                  instrGen.setBit( rpIdx );

                  // Also consider precolorings.
                  auto it = precolRMap.find( leaf.getID() );
                  if ( it != precolRMap.end() )
                    for ( WIR_VirtualRegister &vr : it->second ) {
                      // defs[ vr ] := defs[ vr ] U { rp }
                      defs.at( vr.getID() ).setBit( rpIdx );

                      // mInstrGen[ i ] := mInstrGen[ i ] U { rp }
                      instrGen.setBit( rpIdx );
                    }

                  ++rpIdx;
                }
            }
          }

      // A function's external inputs of course reach the function's very first
      // instruction of the very first basic block.
      if ( ( b == f.begin()->get() ) && ( i == b.begin()->get() ) )
        for ( size_t j = mRegisterDefinitionsSize; j < mProblemSize; ++j )
          instrGen.setBit( j );
    }
  }

  for ( WIR_PhysicalRegister &p : inputRegs )
    defs.at( p.getID() ).setBit( mHash[ p.getID() ] );

  // Compute kill sets per instruction.
  for ( WIR_RegisterParameter &rp : definedParams ) {
    WIR_Instruction &i = rp.getOperation().getInstruction();
    WIR_BaseRegister &r = rp.getRegister();

    auto &instrGen = mInstrGen.at( i.getID() );
    auto &instrKill = mInstrKill.at( i.getID() );

    if ( r.isVirtual() )
      for ( WIR_VirtualRegister &leaf :
              dynamic_cast<WIR_VirtualRegister &>( r ).getLeafs() ) {
        // If a definition of leaf is not in mInstrGen[ i ], do
        // mInstrKill[ i ] := mInstrKill[ i ] U defs[ leaf ]
        WIR_BitVector diff = defs.at( leaf.getID() );
        diff.set_difference( instrGen );
        instrKill.set_union( diff );

        // Also consider kills of precolorings if desired.
        if ( !mVirtualRegistersOnly && leaf.isPrecolored() ) {
          auto &pr = leaf.getPrecolor();

          WIR_BitVector diff = defs.at( pr.getID() );
          diff.set_difference( instrGen );
          instrKill.set_union( diff );

          // A definition of a pre-colored virtual register also kills all those
          // other virtual registers that are pre-colored with the very same
          // physical register.
          auto it = precolRMap.find( pr.getID() );
          if ( it != precolRMap.end() )
            for ( WIR_VirtualRegister &vr : it->second ) {
              WIR_BitVector diff = defs.at( vr.getID() );
              diff.set_difference( instrGen );
              instrKill.set_union( diff );
            }
        }
      }
    else
      for ( WIR_PhysicalRegister &leaf :
              dynamic_cast<WIR_PhysicalRegister &>( r ).getLeafs() ) {
        // If a definition of leaf is not in mInstrGen[ i ], do
        // mInstrKill[ i ] := mInstrKill[ i ] U defs[ leaf ]
        WIR_BitVector diff = defs.at( leaf.getID() );
        diff.set_difference( instrGen );
        instrKill.set_union( diff );

        // Also consider kills of precolorings.
        auto it = precolRMap.find( leaf.getID() );
        if ( it != precolRMap.end() )
          for ( WIR_VirtualRegister &vr : it->second ) {
            WIR_BitVector diff = defs.at( vr.getID() );
            diff.set_difference( instrGen );
            instrKill.set_union( diff );
          }
      }
  }

  DACTION(
    for ( WIR_BasicBlock &b : f ) {
      unsigned int bbPos = 1;

      for ( WIR_Instruction &i : b ) {
        DOUT( "  mInstrGen[ (" << b.getName() << "/" << bbPos << ") ] = {" );
        WIR_BitVector &instrGen = mInstrGen.at( i.getID() );

        for ( size_t j = 0; j < mRegisterDefinitionsSize; ++j )
          if ( instrGen[ j ] ) {
            auto hashIt = mReverseHash.find( j );
            unsigned int bbPos1 = 1;
            WIR_RegisterParameter &rp = *(hashIt->second);
            WIR_Instruction &i1 = rp.getOperation().getInstruction();
            for ( auto it = i1.getBasicBlock().getInstructions().begin();
                  (*it).get() != i1; ++it, ++bbPos1 ) ;
            unsigned int opPos = 1;
            for ( auto it = rp.getOperation().getParameters().begin();
                  it->get() != rp; ++it, ++opPos ) ;

            DOUT(
              " (" << i1.getBasicBlock().getName() << "/" << bbPos1 << "/" <<
              opPos << " " << rp << ")" );
          }
        for ( size_t j = mRegisterDefinitionsSize; j < mProblemSize; ++j )
          if ( instrGen[ j ] ) {
            auto hashIt = mReverseHashInputs.find( j );
            DOUT( " (External Input " << hashIt->second->getName() << ")" );
          }
        DOUT( " }" << endl );

        DOUT( "  mInstrKill[ (" << b.getName() << "/" << bbPos << ") ] = {" );
        WIR_BitVector &instrKill = mInstrKill.at( i.getID() );

        for ( size_t j = 0; j < mRegisterDefinitionsSize; ++j )
          if ( instrKill[ j ] ) {
            auto hashIt = mReverseHash.find( j );
            unsigned int bbPos1 = 1;
            WIR_RegisterParameter &rp = *(hashIt->second);
            WIR_Instruction &i1 = rp.getOperation().getInstruction();
            for ( auto it = i1.getBasicBlock().getInstructions().begin();
                  (*it).get() != i1; ++it, ++bbPos1 ) ;
            unsigned int opPos = 1;
            for ( auto it = rp.getOperation().getParameters().begin();
                  it->get() != rp; ++it, ++opPos ) ;

            DOUT(
              " (" << i1.getBasicBlock().getName() << "/" << bbPos1 << "/" <<
              opPos << " " << rp << ")" );
          }
        for ( size_t j = mRegisterDefinitionsSize; j < mProblemSize; ++j )
          if ( instrKill[ j ] ) {
            auto hashIt = mReverseHashInputs.find( j );
            DOUT( " (External Input " << hashIt->second->getName() << ")" );
          }
        DOUT( " }" << endl );

        ++bbPos;
      }
    }

    for ( WIR_BasicBlock &b : f ) {
      DOUT( "  mPredecessors[ " << b.getName() << " ] = {" );
      for ( WIR_BasicBlock &pred : mPredecessors[ b.getID() ] )
        DOUT( " " << pred.getName() );
      DOUT( " }" << endl );

      DOUT( "  mSuccessors[ " << b.getName() << " ] = {" );
      for ( WIR_BasicBlock &succ : mSuccessors[ b.getID() ] )
        DOUT( " " << succ.getName() );
      DOUT( " }" << endl );
    } );
};


/*
  propagateIns2BB propagates instruction-level gen/kill information to basic
  block-level where reaching-definition analysis is actually done (Appel, pages
  394-395).
*/
void WIR_ReachingDefinitionsAnalysis::propagateIns2BB( WIR_Function &f )
{
  DSTART(
    "void WIR_ReachingDefinitionsAnalysis::propagateIns2BB(WIR_Function&)" );

  // Aggregate all instruction-level gen/kill values for basic blocks.
  for ( WIR_BasicBlock &b : f ) {
    auto &blockReachingDefsOut =
      mBlockReachingDefsOut.insert(
        { b.getID(), WIR_BitVector { mProblemSize } } ).first->second;
    auto &blockKill =
      mBlockKill.insert(
        { b.getID(), WIR_BitVector { mProblemSize } } ).first->second;
    auto &blockReachingDefsIn =
      mBlockReachingDefsIn.insert(
        { b.getID(), WIR_BitVector { mProblemSize } } ).first->second;

    // For all instructions i of b:
    //   mBlockGen[ b ] := mBlockGen[ b ] U (mInstrGen[ i ] - blockKill)
    // Since mBlockGen only serves for the proper initialization of
    // mBlockReachingDefsOut[ b ], we do not compute mBlockGen here explicitly
    // but instead simply initialize mBlockReachingDefsOut accordingly.
    for ( auto it = b.getInstructions().rbegin();
          it != b.getInstructions().rend(); ++it ) {
      WIR_BitVector diff { mInstrGen.at( (*it).get().getID() ) };
      diff.set_difference( blockKill );
      blockReachingDefsOut.set_union( diff );

      // mBlockKill[ b ] := mBlockKill[ b ] U mInstrKill[ i ]
      blockKill.set_union( mInstrKill.at( (*it).get().getID() ) );
    }

    // A function's external inputs of course reach the function's very first
    // basic block.
    if ( b == f.begin()->get() )
      for ( size_t j = mRegisterDefinitionsSize; j < mProblemSize; ++j )
        blockReachingDefsIn.setBit( j );
  }

  DACTION(
    for ( WIR_BasicBlock &b : f ) {
      DOUT( "  mBlockKill[ (" << b.getName() << ") ] = {" );
      WIR_BitVector &blockKill = mBlockKill.at( b.getID() );

      for ( size_t j = 0; j < mRegisterDefinitionsSize; ++j )
        if ( blockKill[ j ] ) {
          auto hashIt = mReverseHash.find( j );
          unsigned int bbPos1 = 1;
          WIR_RegisterParameter &rp = *(hashIt->second);
          WIR_Instruction &i1 = rp.getOperation().getInstruction();
          for ( auto it = i1.getBasicBlock().getInstructions().begin();
                (*it).get() != i1; ++it, ++bbPos1 ) ;
          unsigned int opPos = 1;
          for ( auto it = rp.getOperation().getParameters().begin();
                it->get() != rp; ++it, ++opPos ) ;

          DOUT(
            " (" << i1.getBasicBlock().getName() << "/" << bbPos1 << "/" <<
            opPos << " " << rp << ")" );
        }
      for ( size_t j = mRegisterDefinitionsSize; j < mProblemSize; ++j )
        if ( blockKill[ j ] ) {
          auto hashIt = mReverseHashInputs.find( j );
          DOUT( " (External Input " << hashIt->second->getName() << ")" );
        }
      DOUT( " }" << endl );
    } );
};


/*
  propagateBB2Ins propagates basic block-level analysis results to
  instruction-level.
*/
void WIR_ReachingDefinitionsAnalysis::propagateBB2Ins( WIR_Function &f )
{
  DSTART(
    "void WIR_ReachingDefinitionsAnalysis::propagateBB2Ins(WIR_Function&)" );

  // Free some no longer needed memory.
  mUpdateBlock.clear();
  mPredecessors.clear();
  mSuccessors.clear();
  mBlockKill.clear();
  mBlockReachingDefsOut.clear();

  // Propagate basic block-level analysis results down to the instruction level.
  for ( WIR_BasicBlock &b : f ) {
    // reachingDefsOutPred contains the set of definitions reaching the end of a
    // predecessor instruction of i.
    WIR_BitVector reachingDefsOutPred { mProblemSize };

    for ( WIR_Instruction &i : b ) {

      // Attach a fresh container for incoming reaching-definition sets to the
      // current instruction.
      i.insertContainer( new WIR_ReachingDefinitions() );
      auto &res = i.getContainers<WIR_ReachingDefinitions>().begin()->get();

      // reachingDefsIn contains the set of definitions reaching the beginning
      // of the current instruction n.
      WIR_BitVector reachingDefsIn =
        ( i == *(b.getInstructions().begin()) ) ?
          // For the first instruction of a basic block, its incoming reaching-
          // definitions set is that of the entire block.
          std::move( mBlockReachingDefsIn.at( b.getID() ) ) :
          // reachingDefsIn[ i ] := reachingDefsOut[ pred ]
          std::move( reachingDefsOutPred );

      // Save incoming reaching definitions in their associated container.
      for ( size_t i = 0; i < mRegisterDefinitionsSize; ++i )
        if ( reachingDefsIn[ i ] ) {
          auto hashIt = mReverseHash.find( i );
          res.insertReachingDefinition( *(hashIt->second) );
        }
      for ( size_t i = mRegisterDefinitionsSize; i < mProblemSize; ++i )
        if ( reachingDefsIn[ i ] ) {
          auto hashIt = mReverseHashInputs.find( i );
          res.insertReachingInput( *(hashIt->second) );
        }

      // reachingDefsOut[ i ] :=
      //   mInstrGen[ i ] U ( reachingDefsIn[ i ] - mInstrKill[ i ] )
      // Since the current instruction i will be predecessor instruction in the
      // next iteration of this loop over i, we directly store the results in
      // reachingDefsOutPred.
      reachingDefsOutPred = std::move( mInstrGen.at( i.getID() ) );
      WIR_BitVector diff { reachingDefsIn };
      diff.set_difference( mInstrKill.at( i.getID() ) );
      reachingDefsOutPred.set_union( diff );
    }
  }

  // Free some no longer needed memory.
  mBlockReachingDefsIn.clear();
  mInstrGen.clear();
  mInstrKill.clear();
  mReverseHash.clear();
  mReverseHashInputs.clear();
  mHash.clear();
};

}       // namespace WIR
