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
  @file wirredundantcode.cc
  @brief This file implements an optimization eliminating redundant code that
         computes bit-wise equivalent results.

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
#include <list>
#include <map>
#include <sstream>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>

// Include local headers
#include "wirredundantcode.h"


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
WIR_RedundantCode::WIR_RedundantCode( WIR_System &s ) :
  WIR_Optimization { s },
  WIR_BitOpt { s }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
WIR_RedundantCode::WIR_RedundantCode( WIR_CompilationUnit &c ) :
  WIR_Optimization { c },
  WIR_BitOpt { c }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
WIR_RedundantCode::WIR_RedundantCode( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_BitOpt { f }
{
  DSTART( "WIR_RedundantCode::WIR_RedundantCode(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_RedundantCode::~WIR_RedundantCode( void )
{
  DSTART( "virtual WIR_RedundantCode::~WIR_RedundantCode()" );
};


//
// Protected class methods
//

/*
  runOptimization eliminates redundant code in the given system.
*/
void WIR_RedundantCode::runOptimization( WIR_System &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_CompilationUnit &c : s )
    if ( !c.getDontOptimize() )
      runOptimization( c );
};


/*
  runOptimization eliminates redundant code in the given compilation unit.
*/
void WIR_RedundantCode::runOptimization( WIR_CompilationUnit &c )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_Function &f : c )
    if ( !f.getDontOptimize() )
      runOptimization( f );
};


/*
  runOptimization eliminates redundant code in the given function.
*/
void WIR_RedundantCode::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void WIR_RedundantCode::runOptimization(WIR_Function&)" );

  // Perform bit-true data and value flow analysis first.
  if ( mRunDFA && ( mBitDFA != nullptr ) )
    mBitDFA->analyze();

  // Perform available definitions analysis next.
  WIR_AvailableDefinitionsAnalysis aDef( f );
  aDef.analyze();

  DOUT( "Processing function '" << f.getName() << "'." << endl );
  verifyLocations( f );

  map<WIR_id_t, WIR_UpDownValue> outValues;

  for ( WIR_BasicBlock &b : f ) {
    if ( b.getDontOptimize() )
      continue;

    // Check all instructions of b.
    for ( WIR_Instruction &i : b ) {
      if ( i.getDontOptimize() )
        continue;

      // Get the instruction's available-definitions container.
      auto &aDefContainer =
        i.getContainers<WIR_AvailableDefinitions>().begin()->get();

      // Check all operations of i.
      for ( WIR_Operation &o2 : i ) {
        if ( o2.getDontOptimize() )
          continue;

        // Check all explicit parameters of o.
        for ( WIR_Parameter &p2 : o2.getExplicitParameters() ) {
          if ( p2.getDontOptimize() ||
               ( p2.getType() != WIR_ParameterType::reg ) )
            continue;

          auto &rp2 = dynamic_cast<WIR_RegisterParameter &>( p2 );

          // Skip all defined parameters, we only consider used register
          // parameters here.
          if ( rp2.isDefined() || rp2.isDefUsed() )
            continue;

          // An operation's parameter can, depending on the operation's position
          // in the control flow, be reached via several DFG edges. The
          // (possibly several) up values per used operation parameter have to
          // be combined into one single up value per parameter.
          map<WIR_id_t, WIR_UpDownValue> inValues;
          combineInValues( rp2, inValues );

          // Get incoming up value for rp2 from inValues map.
          auto &inValue = inValues.at( rp2.getID() );

          DACTION(
            stringstream sstr1;
            sstr1 << wir << implicitparams << defuse << o2;
            DOUT( endl << "Checking USE '" << rp2 );
            if ( rp2.getRegister().isVirtual() ) {
              auto &vreg =
                dynamic_cast<WIR_VirtualRegister &>( rp2.getRegister() );
              if ( vreg.isPrecolored() )
                DOUT( "/" << vreg.getPrecolor().getName() );
            }
            DOUT( "' (ID = " << rp2.getID() <<
              ", inValue = " << inValue << ") from" << endl );
            unsigned int bbPos = 1;
            for ( auto it = b.getInstructions().begin(); (*it).get() != i;
                  ++it, ++bbPos ) ;
            DOUT(
              "  (" << b.getName() << "/" << bbPos << ")  '" <<
              sstr1.str().substr( 8, sstr1.str().size() - 9 ) << "':" <<
              endl ); );

          // Skip parameters being constant or X*, since they are subject to
          // constant propagation/folding and dead code elimination. Also skip
          // parameters with unknowns, since they are, mhmm well, unknown.
          if ( inValue.containsOnlyBit( WIR_L4::bX ) ||
               inValue.containsBit( WIR_L4::bU ) )
            continue;

          // Iterate all definitions alive at the current instruction and check
          // whether the outgoing up values of such a definition match with the
          // current inValue.
          auto &availableDefinitions = aDefContainer.getAvailableDefinitions();
          for ( WIR_RegisterParameter &rp1 : availableDefinitions ) {
            // First of all, check whether the registers involved in the
            // definition and the use are of appropriate type and whether they
            // are inequal.
            if ( !checkRegisters( rp1, rp2 ) )
              continue;

            // Combine the (possibly several) outgoing up values of the current
            // definition into one single up value.
            auto it = outValues.find( rp1.getID() );
            if ( it == outValues.end() )
              it = combineOutValues( rp1, outValues );
            if ( it == outValues.end() )
              continue;

            // Get outgoing up value for rp1 from outValues map.
            auto &outValue = it->second;

            // Check the bit values of the defined/used register parameters.
            bool redundant =
              areRedundant( outValue, inValue ) &&
              areRedundant( rp1, outValue, rp2, inValue );

            DACTION(
              if ( redundant ) {
                stringstream sstr1;
                sstr1 << wir << implicitparams << defuse << rp1.getOperation();
                DOUT( "  Checking available DEF '" << rp1 );
                if ( rp1.getRegister().isVirtual() ) {
                  auto &vreg =
                    dynamic_cast<WIR_VirtualRegister &>( rp1.getRegister() );
                  if ( vreg.isPrecolored() )
                    DOUT( "/" << vreg.getPrecolor().getName() );
                }
                DOUT( "' (ID = " <<
                  rp1.getID() << ", outValue = " << outValue << ") from" <<
                  endl );
                unsigned int bbPos = 1;
                WIR_Instruction &i1 = rp1.getOperation().getInstruction();
                for ( auto it = i1.getBasicBlock().getInstructions().begin();
                      (*it).get() != i1; ++it, ++bbPos ) ;
                DOUT(
                  "    (" << i1.getBasicBlock().getName() << "/" << bbPos <<
                  ")  '" << sstr1.str().substr( 8, sstr1.str().size() - 9 ) <<
                  "': "<< string( redundant ? "" : "not " ) << "redundant." <<
                  endl );
              } );

            if ( redundant ) {
              // Store the actual replacement of USE and DEF parameters.
              mParameterReplacement[ rp2 ] = &rp1;

              // Store the down value produced by the DEF parameter.
              auto &defCont = rp1.getContainers<WIR_BitValues>().begin()->get();
              mDownValue.insert(
                { rp2.getID(), defCont.getOutValues().begin()->downVal } );

              // Store the up value consumed by the USE parameter.
              mUpValue.insert( { rp2.getID(), inValue } );
              break;
            }
          }
        }
      }
    }
  }

  // Replace all identified redundant parameters.
  replaceParameters();

  // Clear previous analysis results.
  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      i.eraseContainers( WIR_AvailableDefinitions::getContainerTypeID() );

  verifyLocations( f );
};


/*
  areRedundant checks whether the outgoing bit-value of a defined register
  parameter and the incoming bit-value of a used one are redundant for some
  actual processor architecture.

  Since this task is processor-specific and might or might not be necessary
  for some actual processor, this method is virtual and can be overloaded if
  required.
*/
bool WIR_RedundantCode::areRedundant( const WIR_RegisterParameter &def,
                                      const WIR_UpDownValue &outValue,
                                      const WIR_RegisterParameter &use,
                                      const WIR_UpDownValue &inValue ) const
{
  DSTART(
    "virtual bool WIR_RedundantCode::areRedundant(const WIR_RegisterParameter&, const WIR_UpDownValue&, const WIR_RegisterParameter&, const WIR_UpDownValue&) const" );

  (void) def;
  (void) outValue;
  (void) use;
  (void) inValue;

  return( true );
};


//
// Private class methods
//

/*
  checkRegisters checks whether the registers involved in a defined and a used
  register parameter are suited for redundant code elimination.

  The involved register are suited for redundant code elimination iff
  - they are both of the same register type, and
  - they are different, where the check of difference of registers also involves
    potential precolorings of virtual registers.
*/
bool WIR_RedundantCode::checkRegisters( const WIR_RegisterParameter &def,
                                        const WIR_RegisterParameter &use ) const
{
  DSTART(
    "bool WIR_RedundantCode::checkRegisters(const WIR_RegisterParameter&, const WIR_RegisterParameter&) const" );

  auto &defReg = def.getRegister();
  auto &useReg = use.getRegister();

  // Check the involved register types.
  if ( defReg.getType() != useReg.getType() )
    return( false );

  // Check the involved registers and their precolorings.
  if ( defReg == useReg )
    return( false );

  if ( defReg.isVirtual() && useReg.isPhysical() ) {
    auto &defVReg = dynamic_cast<WIR_VirtualRegister &>( defReg );
    if ( defVReg.isPrecolored() && ( defVReg.getPrecolor() == useReg ) )
      return( false );
  } else

  if ( defReg.isPhysical() && useReg.isVirtual() ) {
    auto &useVReg = dynamic_cast<WIR_VirtualRegister &>( useReg );
    if ( useVReg.isPrecolored() && ( useVReg.getPrecolor() == defReg ) )
      return( false );
  } else

  if ( defReg.isVirtual() && useReg.isVirtual() ) {
    auto &defVReg = dynamic_cast<WIR_VirtualRegister &>( defReg );
    auto &useVReg = dynamic_cast<WIR_VirtualRegister &>( useReg );
    if ( defVReg.isPrecolored() && useVReg.isPrecolored() &&
         ( defVReg.getPrecolor() == useVReg.getPrecolor() ) )
      return( false );
  }

  return( true );
};


/*
  areRedundant checks whether the outgoing bit-value of a defined register
  parameter and the incoming bit-value of a used one are redundant.
*/
bool WIR_RedundantCode::areRedundant( const WIR_UpDownValue &outValue,
                                      const WIR_UpDownValue &inValue ) const
{
  DSTART(
    "bool WIR_RedundantCode::areRedundant(const WIR_UpDownValue&, const WIR_UpDownValue&) const" );

  // Check equivalence of the outgoing/incoming bit values.
  if ( outValue.getBitWidth() != inValue.getBitWidth() )
    // Bit-widths are incompatible.
    return( false );

  // Handle equivalence for numbers.
  for ( unsigned int i = 0; i < outValue.getBitWidth(); ++i ) {
    WIR_L4 b1 = outValue.at( i ), b2 = inValue.at( i );

    if ( ( b1 == WIR_L4::bU ) || ( b2 == WIR_L4::bU ) )
      // If either of the two currently compared bits are U, we don't know
      // absolutely anything about the bits and thus can't compare them.
      return( false );

    if ( b2 == WIR_L4::bX )
      // If the incoming bit is X, we go on.
      continue;

    if ( getLevel( b1 ) != getLevel( b2 ) )
      // If 0 is compared with L or 1 is compared with N etc, the bits are not
      // equivalent
      return( false );

    // From here on, both bits are from the same L4 level and can thus be
    // compared correctly.
    if ( ( getLevel( b1 ) == 2 ) && ( b1 != b2 ) )
      // The current bits are true 0 and 1, and they are different. Thus, the
      // two up/down values cannot be equal.
      return( false );

    if ( getLevel( b1 ) == 1 ) {
      if ( outValue.getLocation( i ) != inValue.getLocation( i ) )
        // The current bits are refering to different locations so that we can't
        // say anything about the comparison result.
        return( false );

      if ( b1 != b2 )
        // The current bits are refering to the same location, and they are
        // different. Thus, the two up/down values cannot be equal.
        return( false );
    }
  }

  return( true );
};


/*
  replaceParameters processes map mParameterReplacement and replaces all
  redundant register parameters by their equivalent available definition.
*/
void WIR_RedundantCode::replaceParameters( void )
{
  DSTART( "void WIR_RedundantCode::replaceParameters()" );

  // For all redundant, used parameters, srcOfUses stores all those register
  // parameters that define this original use.
  list<set<WIR_id_t>> srcOfUses;

  // operationOfUse stores the operations belonging to a redundant, used
  // parameter.
  list<WIR_Operation *> operationOfUse;

  // First, do the actual replacement of a used parameter by its novel
  // definition, and update the direct data flow edges between def and use.
  for ( auto &p : mParameterReplacement ) {
    WIR_RegisterParameter &def = *(p.second);
    WIR_RegisterParameter &use = p.first.get();
    auto useID = use.getID();

    WIR_Operation &o = use.getOperation();
    operationOfUse.push_back( &o );
    srcOfUses.push_back( {} );

    DACTION(
      stringstream sstr1;
      sstr1 << wir << o;
      DOUT( "Replacing used parameter '" << use );
      if ( use.getRegister().isVirtual() ) {
        auto &vreg = dynamic_cast<WIR_VirtualRegister &>( use.getRegister() );
        if ( vreg.isPrecolored() )
          DOUT( "/" << vreg.getPrecolor().getName() );
      }
      DOUT( "' (ID " << p.first << ") by definition from '" << def );
      if ( def.getRegister().isVirtual() ) {
        auto &vreg = dynamic_cast<WIR_VirtualRegister &>( def.getRegister() );
        if ( vreg.isPrecolored() )
          DOUT( "/" << vreg.getPrecolor().getName() );
      }
      DOUT( "' (ID " << p.second <<
        ") in operation '" << sstr1.str().substr( 8 ) << "' (ID " <<
        o.getID() << ")." << endl ); );

    if ( use.containsContainers( WIR_BitValues::getContainerTypeID() ) ) {
      // Get the used parameter's bitValue container.
      auto &c = use.getContainers<WIR_BitValues>().begin()->get();

      // Iterate all incoming edges of use.
      for ( auto &inEdge : c.getInValues() ) {
        srcOfUses.back().insert( inEdge.rp->getID() );

        auto &srcContainer =
          inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
        srcContainer.eraseOutValues( use );
      }
    }

    auto it = o.replaceParameter(
      o.findParameter( use ),
      WIR_RegisterParameter(
        def.getRegister(), WIR_Usage::use, use.isImplicit() ) );

    // Attach a bit-value container to the newly produced register parameter.
    auto *cont = new WIR_BitValues();
    it->get().insertContainer( cont );

    auto &defCont = def.getContainers<WIR_BitValues>().begin()->get();

    cont->insertInValues(
      def, WIR_UpDownValue( mDownValue.at( useID ) ),
      WIR_UpDownValue( mUpValue.at( useID ) ) );

    defCont.insertOutValues(
      it->get(), WIR_UpDownValue( mDownValue.at( useID ) ),
      WIR_UpDownValue( mUpValue.at( useID ) ) );
  }

  // Next, patch the data flow leaving all optimized operations such that
  // location bits that are simply propagated through an operation are
  // corrected.
  for ( auto &p : mParameterReplacement ) {
    WIR_RegisterParameter &def = *(p.second);
    WIR_Operation &op = *(operationOfUse.front());
    operationOfUse.pop_front();
    set<WIR_id_t> &srcOfUse = srcOfUses.front();

    // A small lambda to update locations in an up/down value.
    auto patchValue = [&]( WIR_UpDownValue &v ) {
      bool res = false;

      for ( unsigned int i = 0; i < v.getBitWidth(); ++i )
        // Check for L or N bits.
        if ( getLevel( v.at( i ) ) == 1 ) {
          auto &location = v.getLocation( i );

          if ( location.isRegisterParameter() &&
               srcOfUse.count( location.getRegisterParameter().getID() ) ) {
            // The current bit refers to a location that relates to the
            // originally used parameter of o. So, we have to patch this
            // location bit. Thus, replace the old location by the new one.
            v.setBit( i, v.at( i ), { def, location.getBitPosition() } );
            res = true;
          }
        }

      return( res );
    };

    WIR_OperationSet workList;
    workList.insert( op );

    while ( !workList.empty() ) {
      WIR_Operation &o = workList.begin()->get();
      workList.erase( workList.begin() );

      // Patch all outgoing edges of the modified operation o.
      //   ADD r1, <something>
      //   MOV r2, r1               # This is the current operation o, its
      //                              parameter r1 is the current 'use' that
      //                              shall be replaced by a completely
      //                              different register 'def', e.g., r3.
      // It can happen (esp. in the case of MOV operations) that the defined
      // parameter r2 of o carries outgoing edges which contain bit-locations
      // that refer to the original definition of parameter 'use' (here: r1 of
      // ADD). If the MOV now gets optimized to 'MOV r2, r3', the bits
      // originally coming from the ADD via r1 no longer flow to/through the
      // optimized operation o and thus cannot show up as locations in some
      // outgoing edge of o. For this purpose, all location bits of outgoing
      // edges of o that refer to the original definition (r1) need to be
      // patched such that they refer to register parameter 'def' afterwards.
      for ( WIR_Parameter &param : o )
        if ( param.getType() == WIR_ParameterType::reg ) {
          auto &rp = dynamic_cast<WIR_RegisterParameter &>( param );

          if ( ( rp.isDefined() || rp.isDefUsed() ) &&
              rp.containsContainers( WIR_BitValues::getContainerTypeID() ) ) {
            auto &c = rp.getContainers<WIR_BitValues>().begin()->get();

            // Iterate all outgoing edges of the current def of o.
            for ( auto &outEdge : c.getOutValues() ) {
              // Patch the edge's up and down values.
              bool patched = patchValue( outEdge.downVal );
              patched |= patchValue( outEdge.upVal );
              if ( patched )
                workList.insert( outEdge.rp->getOperation() );

              // Likewise, patch the in-values of the current out-edge's target.
              auto &targetCont =
                outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
              auto &inEdge = *(targetCont.findInValues( rp ));

              // Patch the edge's up and down values.
              patchValue( inEdge.downVal );
              patchValue( inEdge.upVal );
            }
          }
        }
    }

    srcOfUses.pop_front();
  }

  mParameterReplacement.clear();
};

}       // namespace WIR
