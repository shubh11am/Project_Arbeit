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
  @file wirconstprop.cc
  @brief This file implements a constant propagation optimization.

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
#include "wirconstprop.h"


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
WIR_ConstProp::WIR_ConstProp( WIR_System &s ) :
  WIR_BitOpt { s }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
WIR_ConstProp::WIR_ConstProp( WIR_CompilationUnit &c ) :
  WIR_BitOpt { c }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
WIR_ConstProp::WIR_ConstProp( WIR_Function &f ) :
  WIR_BitOpt { f }
{
  DSTART( "WIR_ConstProp::WIR_ConstProp(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_ConstProp::~WIR_ConstProp( void )
{
  DSTART( "virtual WIR_ConstProp::~WIR_ConstProp()" );
};


//
// Protected class methods
//

/*
  runOptimization propagates constants in the given system.
*/
void WIR_ConstProp::runOptimization( WIR_System &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_CompilationUnit &c : s )
    if ( !c.getDontOptimize() )
      runOptimization( c );
};


/*
  runOptimization propagates constants in the given compilation unit.
*/
void WIR_ConstProp::runOptimization( WIR_CompilationUnit &c )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_Function &f : c )
    if ( !f.getDontOptimize() )
      runOptimization( f );
};


/*
  runOptimization propagates constants in the given function.
*/
void WIR_ConstProp::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void WIR_ConstProp::runOptimization(WIR_Function&)" );

  // Perform bit-true data and value flow analysis first.
  if ( mRunDFA && ( mBitDFA != nullptr ) )
    mBitDFA->analyze();

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

        // Propagate constants if possible.
        if ( !o.getDontOptimize() && !dontOptimizeParams && constProp( o ) ) {
          // Store the current instruction iterator for later efficient use by
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

        eraseInstructions();
      }

      iIt = nextInsIt;
    }
  }

  verifyLocations( f );
};


/*
  replace replaces all 'X' bits in the given up/down value by the specified L4
  value.
*/
signed long long WIR_ConstProp::replace( const WIR_UpDownValue &v,
                                         WIR_L4 b, bool s ) const
{
  DSTART(
    "long long int WIR_ConstProp::replace(const WIR_UpDownValue&, WIR_L4, bool) const" );

  auto val = v;
  val.setSignedness( s );

  for ( unsigned int i = 0; i < val.getBitWidth(); ++i )
    if ( val.at( i ) == WIR_L4::bX )
      val.setBit( i, b );

  return( val.getSignedValue() );
};


//
// Private class methods
//

/*
  constProp performs the generic, processor-independent parts of constant
  propagation.

  constProp basically combines the possibly several incoming up/down values per
  parameter into a single up/down value and calls the processor-specific virtual
  methods that check and finally realize propagation.
*/
bool WIR_ConstProp::constProp( const WIR_Operation &o )
{
  DSTART( "bool WIR_ConstProp::constProp(const WIR_Operation&)" );

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
  map<WIR_id_t, WIR_UpDownValue> inValue;

  // Iterate all explicit parameters of o and combine their incoming up values.
  for ( WIR_Parameter &p : o.getExplicitParameters() )
    if ( p.getType() == WIR_ParameterType::reg ) {
      auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );

      combineInValues( rp, inValue );
    }

  // Do the actual constant propagation.
  return( doConstProp( o, inValue ) );
};


/*
  addNewInstructions processes map mNewInstructions and adds the generated
  constant-propagated instructions immediately behind each original constant
  operation.
*/
void WIR_ConstProp::addNewInstructions( void )
{
  DSTART( "void WIR_ConstProp::addNewInstructions()" );

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
          "' after operation '" << sstr1.str().substr( 8 ) << "' (ID " <<
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
void WIR_ConstProp::eraseInstructions( void )
{
  DSTART( "void WIR_ConstProp::eraseInstructions()" );

  while ( !mErasePositions.empty() ) {
    auto pos = mErasePositions.front();
    mErasePositions.pop_front();

    WIR_BasicBlock &b = pos->get().getBasicBlock();

    // Remove instruction from basic block b.
    DACTION(
      stringstream sstr;
      sstr << wir << pos->get();
      DOUT(
        "Removing original instruction '" <<
        sstr.str().substr( 8, sstr.str().size() - 9 ) << "' (ID " <<
        pos->get().getID() << ")." << endl; ); );
    b.eraseInstruction( pos );
  }
};

}       // namespace WIR
