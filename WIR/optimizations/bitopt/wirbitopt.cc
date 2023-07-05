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
  @file wirbitopt.cc
  @brief This file implements a virtual base class for optimizations using
         bit-true data and value flow analysis.

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
#include <sstream>
#include <string>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>

// Include local headers
#include "wirbitopt.h"


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
WIR_BitOpt::WIR_BitOpt( WIR_System &s ) :
  WIR_Optimization { s },
  mBitDFA { nullptr },
  mRunDFA { true }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
WIR_BitOpt::WIR_BitOpt( WIR_CompilationUnit &c ) :
  WIR_Optimization { c },
  mBitDFA { nullptr },
  mRunDFA { true }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
WIR_BitOpt::WIR_BitOpt( WIR_Function &f ) :
  WIR_Optimization { f },
  mBitDFA { nullptr },
  mRunDFA { true }
{
  DSTART( "WIR_BitOpt::WIR_BitOpt(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_BitOpt::~WIR_BitOpt( void )
{
  DSTART( "virtual WIR_BitOpt::~WIR_BitOpt()" );
};


/*
  setRunDFA specifies whether a bit-true data flow analysis shall be carried out
  prior to the actual optimization or not.

  If no data flow analysis shall be done, a bit-true optimization uses bitValue
  containers that are already attached to the WIR by prior analysis runs.
*/
void WIR_BitOpt::setRunDFA( bool f )
{
  DSTART( "void WIR_BitOpt::setRunDFA(bool)" );

  mRunDFA = f;
};


//
// Protected class methods
//

/*
  setDFA sets the processor-specific bit-true data and value flow analyzer.
*/
void WIR_BitOpt::setDFA( WIR_BitDFA &a )
{
  DSTART( "void WIR_BitOpt::setDFA(WIR_BitDFA&)" );

  mBitDFA = &a;
};


/*
  combineOutValues combines the outgoing up values of a register parameter with
  previous up values, if multiple edges with potentially different up values
  refer to the very same parameter of a WIR operation.
*/
map<WIR_id_t, WIR_UpDownValue>::iterator WIR_BitOpt::combineOutValues( const WIR_RegisterParameter &rp,
                                                                       std::map<WIR_id_t, WIR_UpDownValue> &outValue ) const
{
  DSTART(
    "map<long long unsigned int, WIR_UpDownValue>::iterator WIR_BitOpt::combineOutValues(const WIR_RegisterParameter&, map<long long unsigned int, WIR_UpDownValue>&) const" );

  // Get the register parameter's bitValue container.
  auto &rpContainer = rp.getContainers<WIR_BitValues>().begin()->get();

  // initializedOperandBits is used for book-keeping. For hierarchical
  // registers, we need to keep track, which sub-parts of the hierarchical
  // registers have already been initialized explicitly and which other parts
  // not.
  map<WIR_id_t, WIR_UpDownValue> initializedOperandBits;

  // Check whether rp is defined or def-used.
  WIR_RegisterParameter *maxWidth = nullptr;
  if ( rp.isDefined() || rp.isDefUsed() ) {
    // Iterate all outgoing edges of the current parameter.
    for ( auto &e : rpContainer.getOutValues() ) {
      auto &neighbor = dynamic_cast<const WIR_RegisterParameter &>( *(e.rp) );

      if ( ( maxWidth == nullptr ) ||
           ( neighbor.getRegister().getBitWidth() >
               maxWidth->getRegister().getBitWidth() ) )
        maxWidth = const_cast<WIR_RegisterParameter *>( &neighbor );

      // Combine all these up values.
      WIR_BitDFA::combineOutEdge(
        rp, neighbor, e.upVal, outValue, initializedOperandBits );
    }

    // Strip down the bit width of the produced outvalue in case of hierarchical
    // registers.
    auto it = outValue.find( rp.getID() );
    if ( maxWidth && it != outValue.end() ) {
      auto &v = it->second;
      auto &r = rp.getRegister();

      if ( r.getBitWidth() < v.getBitWidth() ) {
        // A wide bit value was produced for a tall register which can only
        // happen for hierarchical registers. Thus, apply an appropriate extract
        // operation.
        unsigned int offset =
          WIR_BitDFA::getHierarchicalRegisterOffset( rp, *maxWidth );
        v = v.extract( offset, r.getBitWidth() );
      }

      #ifdef FAILSAFEMODE
      ufAssert( v.getBitWidth() == r.getBitWidth() );
      #endif
    }

    return( it );
  }

  return( outValue.end() );
};


/*
  combineInValues combines the incoming up values of a register parameter with
  previous up values, if multiple edges with potentially different up values
  refer to the very same parameter of a WIR operation.
*/
void WIR_BitOpt::combineInValues( const WIR_RegisterParameter &rp,
                                  std::map<WIR_id_t, WIR_UpDownValue> &inValue ) const
{
  DSTART(
    "void WIR_BitOpt::combineInValues(const WIR_RegisterParameter&, map<long long unsigned int, WIR_UpDownValue>&) const" );

  // Get the register parameter's bitValue container.
  auto &rpContainer = rp.getContainers<WIR_BitValues>().begin()->get();

  // initializedOperandBits is used for book-keeping. For hierarchical
  // registers, we need to keep track, which sub-parts of the hierarchical
  // registers have already been initialized explicitly and which other parts
  // not.
  map<WIR_id_t, WIR_UpDownValue> initializedOperandBits;

  // Check whether rp is used or def-used.
  WIR_RegisterParameter *maxWidth = nullptr;
  if ( rp.isUsed() || rp.isDefUsed() ) {
    // Iterate all incoming edges of the current parameter.
    for ( auto &e : rpContainer.getInValues() ) {
      auto &neighbor = dynamic_cast<const WIR_RegisterParameter &>( *(e.rp) );

      if ( ( maxWidth == nullptr ) ||
           ( neighbor.getRegister().getBitWidth() >
               maxWidth->getRegister().getBitWidth() ) )
        maxWidth = const_cast<WIR_RegisterParameter *>( &neighbor );

      // Combine all these up values.
      WIR_BitDFA::combineInEdge(
        neighbor, rp, e.upVal, inValue, initializedOperandBits );
    }

    // Strip down the bit width of the produced in-value in case of hierarchical
    // registers.
    auto it = inValue.find( rp.getID() );
    if ( it != inValue.end() ) {
      auto &v = it->second;
      auto &r = rp.getRegister();

      if ( maxWidth && ( r.getBitWidth() < v.getBitWidth() ) ) {
        // A wide bit value was produced for a tall register which can only
        // happen for hierarchical registers. Thus, apply an appropriate extract
        // operation.
        unsigned int offset =
          WIR_BitDFA::getHierarchicalRegisterOffset( rp, *maxWidth );
        v = v.extract( offset, r.getBitWidth() );
      }

      #ifdef FAILSAFEMODE
      ufAssert( v.getBitWidth() == r.getBitWidth() );
      #endif
    } else
      // rp has no incoming edges and thus is undefined. Thus, add a dummy entry
      // to inValues.
      inValue.insert(
        make_pair(
          rp.getID(),
          WIR_UpDownValue( WIR_L4::bU, rp.getRegister().getBitWidth() ) ) );
  }
};


/*
  updateLocations patches all up/down values attached to WIR code of a given
  function such that location bits refering to old register parameters are
  replaced by locations of new parameters according to map mNewLocation.
*/
void WIR_BitOpt::updateLocations( WIR_Function &f )
{
  DSTART( "void WIR_BitOpt::updateLocations(WIR_Function&)" );

  // A small lambda to update locations in an up/down value.
  auto patchValue = [&]( WIR_UpDownValue &v ) {
    for ( unsigned int i = 0; i < v.getBitWidth(); ++i )
      // Check for L or N bits.
      if ( getLevel( v.at( i ) ) == 1 ) {
        auto &location = v.getLocation( i );

        if ( location.isRegisterParameter() ) {
          WIR_id_t oldLocationID = location.getRegisterParameter().getID();
          auto it = mNewLocation.find( oldLocationID );
          if ( it != mNewLocation.end() ) {
            // Replace the old location by the new one.
            v.setBit(
              i, v.at( i ), { it->second.get(), location.getBitPosition() } );
          }
        }
      }
  };

  if ( !mNewLocation.empty() )
    for ( WIR_BasicBlock &b : f )
      for ( WIR_Instruction &i : b )
        for ( WIR_Operation &o : i )
          for ( WIR_Parameter &p : o )
            if ( ( p.getType() == WIR_ParameterType::reg ) &&
                p.containsContainers( WIR_BitValues::getContainerTypeID() ) ) {
              // Get the register parameter's bitValue container.
              auto &cont = p.getContainers<WIR_BitValues>().begin()->get();

              // Iterate all incoming edges of the curent register parameter.
              for ( auto &e : cont.getInValues() ) {
                // Patch the edge's up and down values.
                patchValue( e.downVal );
                patchValue( e.upVal );
              }

              // Iterate all outgoing edges of the curent register parameter.
              for ( auto &e : cont.getOutValues() ) {
                // Patch the edge's up and down values.
                patchValue( e.downVal );
                patchValue( e.upVal );
              }
            }

  mNewLocation.clear();
};


/*
  verifyLocations verifies that location bits attached as bit values to the
  specified function actually refer to correct and existing register parameters.
*/
void WIR_BitOpt::verifyLocations( const WIR_Function &f ) const
{
  DSTART( "void WIR_BitOpt::verifyLocations(const WIR_Function&) const" );

  (void) f;

  DACTION(
    cout << f;

    for ( WIR_BasicBlock &b : f ) {
      unsigned int bbPos = 1;

      for ( WIR_Instruction &i : b ) {
        for ( WIR_Operation &o : i ) {
          stringstream sstr1;
          sstr1 << wir << implicitparams << defuse << o;
          cout << "Checking operation '"
               << sstr1.str().substr( 8, sstr1.str().size() - 9 ) << "' (ID "
               << o.getID() << ", " << b.getName() << "/" << bbPos << ")."
               << endl;

          for ( WIR_Parameter &p : o )
            if ( p.containsContainers( WIR_BitValues::getContainerTypeID() ) ) {
              cout << "  Checking parameter '" << p << "' (ID " << p.getID()
                   << ")." << endl;
              // Get the current parameter's bitValue container.
              auto &c = p.getContainers<WIR_BitValues>().begin()->get();

              // Verify all in-edges.
              for ( auto &e : c.getInValues() ) {
                cout << "    Checking in-edge '" << e.rp << "' (ID "
                     << e.rp->getID() << ") -> '" << p << "' (ID " << p.getID()
                     << ")." << endl;

                cout << "      Down Value = " << e.downVal << endl;
                for ( unsigned int i = 0; i < e.downVal.getBitWidth(); ++i )
                  if ( getLevel( e.downVal.at( i ) ) == 1 ) {
                    cout << "        Bit[ " << i << " ] refers to '";
                    auto &l = e.downVal.getLocation( i );
                    if ( l.isRegisterParameter() )
                      cout << l.getRegisterParameter() << "' (ID "
                           << l.getRegisterParameter().getID() << ")." << endl;
                    else
                      cout << l.getSymbol().getName() << "' (ID "
                           << l.getSymbol().getID() << ")." << endl;
                  }

                cout << "      Up Value = " << e.upVal << endl;
                for ( unsigned int i = 0; i < e.upVal.getBitWidth(); ++i )
                  if ( getLevel( e.upVal.at( i ) ) == 1 ) {
                    cout << "        Bit[ " << i << " ] refers to '";
                    auto &l = e.upVal.getLocation( i );
                    if ( l.isRegisterParameter() )
                      cout << l.getRegisterParameter() << "' (ID "
                           << l.getRegisterParameter().getID() << ")." << endl;
                    else
                      cout << l.getSymbol().getName() << "' (ID "
                           << l.getSymbol().getID() << ")." << endl;
                  }
              }

              // Verify all out-edges.
              for ( auto &e : c.getOutValues() ) {
                cout << "    Checking out-edge '" << p << "' (ID "
                     << p.getID() << ") -> '" << e.rp << "' (ID "
                     << e.rp->getID() << ")." << endl;

                cout << "      Down Value = " << e.downVal << endl;
                for ( unsigned int i = 0; i < e.downVal.getBitWidth(); ++i )
                  if ( getLevel( e.downVal.at( i ) ) == 1 ) {
                    cout << "        Bit[ " << i << " ] refers to '";
                    auto &l = e.downVal.getLocation( i );
                    if ( l.isRegisterParameter() )
                      cout << l.getRegisterParameter() << "' (ID "
                           << l.getRegisterParameter().getID() << ")." << endl;
                    else
                      cout << l.getSymbol().getName() << "' (ID "
                           << l.getSymbol().getID() << ")." << endl;
                  }

                cout << "      Up Value = " << e.upVal << endl;
                for ( unsigned int i = 0; i < e.upVal.getBitWidth(); ++i )
                  if ( getLevel( e.upVal.at( i ) ) == 1 ) {
                    cout << "        Bit[ " << i << " ] refers to '";
                    auto &l = e.upVal.getLocation( i );
                    if ( l.isRegisterParameter() )
                      cout << l.getRegisterParameter() << "' (ID "
                           << l.getRegisterParameter().getID() << ")." << endl;
                    else
                      cout << l.getSymbol().getName() << "' (ID "
                           << l.getSymbol().getID() << ")." << endl;
                  }
              }
          }
        }

        ++bbPos;
      }
    }
  );
};

}       // namespace WIR
