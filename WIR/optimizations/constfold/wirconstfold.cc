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
  @file wirconstfold.cc
  @brief This file implements a constant folding optimization.

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
#include <iterator>
#include <list>
#include <sstream>
#include <string>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>

// Include local headers
#include "wirconstfold.h"


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
WIR_ConstFold::WIR_ConstFold( WIR_System &s ) :
  WIR_Optimization { s },
  WIR_BitOpt { s }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
WIR_ConstFold::WIR_ConstFold( WIR_CompilationUnit &c ) :
  WIR_Optimization { c },
  WIR_BitOpt { c }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
WIR_ConstFold::WIR_ConstFold( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_BitOpt { f }
{
  DSTART( "WIR_ConstFold::WIR_ConstFold(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_ConstFold::~WIR_ConstFold( void )
{
  DSTART( "virtual WIR_ConstFold::~WIR_ConstFold()" );
};


//
// Protected class methods
//

/*
  runOptimization folds constants in the given system.
*/
void WIR_ConstFold::runOptimization( WIR_System &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_CompilationUnit &c : s )
    if ( !c.getDontOptimize() )
      runOptimization( c );
};


/*
  runOptimization folds constants in the given compilation unit.
*/
void WIR_ConstFold::runOptimization( WIR_CompilationUnit &c )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_Function &f : c )
    if ( !f.getDontOptimize() )
      runOptimization( f );
};


/*
  runOptimization folds constants in the given function.
*/
void WIR_ConstFold::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void WIR_ConstFold::runOptimization(WIR_Function&)" );

  bool cfgChanged = false;

  do {
    // Perform bit-true data and value flow analysis first.
    if ( ( mRunDFA || cfgChanged ) && ( mBitDFA != nullptr ) )
      mBitDFA->analyze();
    cfgChanged = false;

    DOUT( "Processing function '" << f.getName() << "'." << endl );
    verifyLocations( f );

    for ( WIR_BasicBlock &b : f ) {
      if ( b.getDontOptimize() )
        continue;

      // Check all instructions of b.
      for ( auto iIt = b.begin(); iIt != b.end(); ) {
        // Set up an iterator pointing to the next instruction after the current
        // one.
        auto nextInsIt = next( iIt );

        WIR_Instruction &i = iIt->get();

        if ( i.getDontOptimize() ) {
          iIt = nextInsIt;
          continue;
        }

        bool instructionModified = false;

        // Check all operations of i.
        for ( WIR_Operation &o : i ) {
          bool dontOptimizeParams = false;
          for ( WIR_Parameter &p : o )
            if ( p.getDontOptimize() ) {
              dontOptimizeParams = true;
              break;
            }

          // Fold a constant operation if possible.
          if ( !o.getDontOptimize() && !dontOptimizeParams && constFold( o ) ) {
            // Store the current instruction iterator for efficient later use by
            // addNewInstructions.
            mIterator[ o.getID() ] = iIt;

            instructionModified = true;
          }
        }

        // Add new instructions to the current WIR function.
        if ( instructionModified ) {
          addNewInstructions();

          // Update locations in up/down values if necessary.
          updateLocations( f );

          cfgChanged |= eraseInstructions();
        }

        iIt = nextInsIt;
      }
    }

    if ( cfgChanged ) {
      // Clean up basic block structure after CFG modifications.
      WIR_RedundantBlocks optRedundantBlocks( f );
      optRedundantBlocks.optimize();

      WIR_EmptyBlocks optEmptyBlocks( f );
      optEmptyBlocks.optimize();

      WIR_UnreachableBlocks optUnreachableBlocks( f );
      optUnreachableBlocks.optimize();
    }

    verifyLocations( f );
  } while ( cfgChanged );
};


/*
  replace replaces all 'X' bits in the given up/down value by the specified L4
  value.
*/
signed long long WIR_ConstFold::replace( const WIR_UpDownValue &v,
                                         WIR_L4 b ) const
{
  DSTART(
    "long long int WIR_ConstFold::replace(const WIR_UpDownValue&, WIR_L4) const" );

  auto val = v;

  for ( unsigned int i = 0; i < val.getBitWidth(); ++i )
    if ( val.at( i ) == WIR_L4::bX )
      val.setBit( i, b );

  return( val.getSignedValue() );
};


//
// Private class methods
//

/*
  constFold performs the generic, processor-independent parts of constant
  folding.

  constFold basically combines the possibly several outgoing up/down values per
  parameter into a single up/down value and calls the processor-specific virtual
  methods that check and finally realize folding.
*/
bool WIR_ConstFold::constFold( const WIR_Operation &o )
{
  DSTART( "bool WIR_ConstFold::constFold(const WIR_Operation&)" );

  DACTION(
    stringstream sstr;
    sstr << wir << o;
    DOUT(
      "Checking '" << sstr.str().substr( 8 ) << "' (ID " << o.getID() << ")." <<
      endl; ); );

  // An operation's parameter can, depending on the operation's position in the
  // control flow, be reached via several DFG edges. Before the actual
  // optimization of the current operation, the (possibly several) up values per
  // operation parameter have to be combined into one single up value per
  // parameter.
  map<WIR_id_t, WIR_UpDownValue> outValue;
  map<WIR_id_t, WIR_UpDownValue> inValue;

  // Iterate all explicit parameters of o and combine their outgoing and
  // incoming up values.
  for ( WIR_Parameter &p : o.getExplicitParameters() )
    if ( p.getType() == WIR_ParameterType::reg ) {
      auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );

      combineOutValues( rp, outValue );
      combineInValues( rp, inValue );
    }

  // Determine whether operation o is constant.
  bool outValuesAreConst = !outValue.empty();

  for ( auto &p : outValue ) {
    if ( !p.second.isInteger() ) {
      DOUT(
        "  Case 1: Operation is not const due to out-value '" << p.second <<
        "'." << endl );
      outValuesAreConst = false;
      break;
    }

    if ( p.second.containsOnlyBit( WIR_L4::bX ) ) {
      DOUT(
        "  Case 2: Operation is not const due to out-value '" << p.second <<
        "'." << endl );
      outValuesAreConst = false;
      break;
    }
  }

  if ( outValuesAreConst || !inValue.empty() ) {
    // Do the actual constant folding.
    DOUT( "  Operation might be const." << endl );
    return( doConstFolding( o, outValue, inValue ) );
  }

  return( false );
};


/*
  addNewInstructions processes map mNewInstructions and adds the generated
  constant-folded instructions immediately behind each original constant
  operation.
*/
void WIR_ConstFold::addNewInstructions( void )
{
  DSTART( "void WIR_ConstFold::addNewInstructions()" );

  for ( auto &p : mNewInstructions ) {
    // Retrieve the list of new instructions to be added.
    auto &instrs = p.second;

    // Retrieve an iterator before which the new instructions will be added.
    auto insertPos = std::next( mIterator[ p.first.get().getID() ] );

    // Retrieve the basic block to which the new instructions will be added.
    auto &o = p.first.get();
    auto &i = o.getInstruction();
    auto &b = i.getBasicBlock();
    auto pos = b.findInstruction( i );

    // Insert all new instructions into b at the current position.
    while ( !instrs.empty() ) {
      auto it = b.insertInstruction( insertPos, std::move( instrs.front() ) );
      instrs.pop_front();

      // Copy comments and file-infos.
      for ( WIR_Comment &c : i.getContainers<WIR_Comment>() )
        it->get().insertContainer( c );
      for ( WIR_FileInfo &c : i.getContainers<WIR_FileInfo>() )
        it->get().insertContainer( c );

      // For the very last newly inserted instruction, copy all containers, not
      // only comments and file-infos.
      if ( instrs.empty() )
        copyContainers( it->get(), i );

      DACTION(
        stringstream sstr0;
        sstr0 << wir << it->get();
        stringstream sstr1;
        sstr1 << wir << o;
        DOUT(
          "Adding new instruction '" <<
          sstr0.str().substr( 8, sstr0.str().size() - 9 ) <<
          "' (ID " << it->get().getID() <<
          ") after operation '" << sstr1.str().substr( 8 ) << "' (ID " <<
          o.getID() << ")." << endl; ); );
    }

    // Remove o's parameters properly from all adjacent bit-value containers.
    for ( WIR_Parameter &p : o )
      if ( p.containsContainers( WIR_BitValues::getContainerTypeID() ) ) {
        // Get the parameter's bitValue container.
        auto &c = p.getContainers<WIR_BitValues>().begin()->get();

        // Iterate all outgoing edges of p.
        for ( auto &outEdge : c.getOutValues() ) {
          auto &tgtContainer =
            outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
          tgtContainer.eraseInValues( p );
        }

        // Iterate all incoming edges of p.
        for ( auto &inEdge : c.getInValues() ) {
          auto &srcContainer =
            inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
          srcContainer.eraseOutValues( p );
        }
      }

    // Add o's instruction to the list of instructions to be erased.
    mErasePositions.push_back( pos );
  }

  mNewInstructions.clear();
  mIterator.clear();
};



/*
  eraseInstructions processes list mErasePositions and erases all instructions
  therein.
*/
bool WIR_ConstFold::eraseInstructions( void )
{
  DSTART( "bool WIR_ConstFold::eraseInstructions()" );

  bool cfgChanged = false;

  while ( !mErasePositions.empty() ) {
    auto pos = mErasePositions.front();
    mErasePositions.pop_front();

    WIR_BasicBlock &b = pos->get().getBasicBlock();

    // Store the current number of CFG successors of basic block b.
    unsigned int cfgSuccessors = b.getSuccessors().size();

    // Remove instruction from basic block b.
    DACTION(
      stringstream sstr;
      sstr << wir << pos->get();
      DOUT(
        "Removing original instruction '" <<
        sstr.str().substr( 8, sstr.str().size() - 9 ) << "' (ID " <<
        pos->get().getID() << ")." << endl; ); );
    b.eraseInstruction( pos );

    // Check whether the CFG of basic block b has changed.
    cfgChanged |= ( b.getSuccessors().size() != cfgSuccessors );
  }

  return( cfgChanged );
};

}       // namespace WIR
