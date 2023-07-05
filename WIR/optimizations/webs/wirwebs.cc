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
  @file wirwebs.cc
  @brief This file implements an optimization replacing occurrences of virtual
         registers by webs.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include boost headers
#include <boost/current_function.hpp>

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
  Default constructor for system-level optimization.
*/
WIR_Webs::WIR_Webs( WIR_System &s ) :
  WIR_Optimization { s }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
WIR_Webs::WIR_Webs( WIR_CompilationUnit &c ) :
  WIR_Optimization { c }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
WIR_Webs::WIR_Webs( WIR_Function &f ) :
  WIR_Optimization { f }
{
  DSTART( "WIR_Webs::WIR_Webs(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_Webs::~WIR_Webs( void )
{
  DSTART( "virtual WIR_Webs::~WIR_Webs()" );
};


/*
  getWebs returns the list of finally created webs.
*/
std::list<struct WIR_Webs::Web> WIR_Webs::getWebs( void ) const
{
  DSTART( "list<WIR_Webs::Web> WIR_Webs::getWebs() const" );

  return( mNewWebs );
};


//
// Protected class methods
//

/*
  runOptimization inserts webs in the given system.
*/
void WIR_Webs::runOptimization( WIR_System &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_CompilationUnit &c : s )
    runOptimization( c );
};


/*
  runOptimization inserts webs in the given compilation unit.
*/
void WIR_Webs::runOptimization( WIR_CompilationUnit &c )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_Function &f : c )
    runOptimization( f );
};


/*
  runOptimization inserts webs in the given function.
*/
void WIR_Webs::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void WIR_Webs::runOptimization(WIR_Function&)" );

  mNewWebs.clear();

  // Do a def-use/use-def chain analysis first.
  WIR_DUUDChainAnalysis duChain( f );
  duChain.analyze();

  // Iterate over all current virtual registers of f.
  unsigned int numberOfRegs = f.getVirtualRegisters().size();
  auto it = f.getVirtualRegisters().begin();
  for ( unsigned int i = 0; i < numberOfRegs; ++i, ++it )
    if ( !it->get().isPrecolored() ) {
      // We do not consider precolored virtual registers here. Precolored
      // virtual registers have the same meaning as a physical register, and
      // generating webs for physical registers makes no sense: How would you
      // split a physical register into several ones...?
      identifyWebs( it->get() );
      createWebs( it->get() );
    }

  // Free some no longer needed memory.
  f.eraseContainers( WIR_DUUDChain::getContainerTypeID(), true );

  // Finally, remove all unused virtual registers.
  WIR_UnusedVRegs unusedVRegs( f );
  unusedVRegs.optimize();
};


//
// Private class methods
//

/*
  identifyWebs identifies webs for a given virtual register.
*/
void WIR_Webs::identifyWebs( const WIR_VirtualRegister &r )
{
  DSTART( "void WIR_Webs::identifyWebs(const WIR_VirtualRegister&)" );

  // Analyze all WIR parameters defining or using r or one of its childs.
  for ( WIR_BasicBlock &b : r.getFunction() )
    for ( WIR_Instruction &i : b )
      for ( WIR_Operation &o : i )
        for ( WIR_Parameter &p : o )
          if ( p.getType() == WIR_ParameterType::reg ) {
            WIR_RegisterParameter &rp =
              dynamic_cast<WIR_RegisterParameter &>( p );

            if ( ( rp.getRegister() == r ) &&
                 ( rp.isDefined() || rp.isDefUsed() ) ) {
              // OK, fine - we found a parameter where the current register r is
              // defined or defused. So, let's extract this parameter's def-use
              // chain and compare it to the webs identified so far.
              WIR_DUUDChain &chainContainer =
                rp.getContainers<WIR_DUUDChain>().begin()->get();
              auto &duChain = chainContainer.getDUChains();

              // Initialize a new web only consisting of the current def-use
              // chain for the moment.
              struct Web newWeb;
              newWeb.defs.insert( rp );
              DOUT( "Processing def-use chain " << defuse << rp << " ->" );
              for ( WIR_RegisterParameter &tmp : duChain ) {
                DOUT( " " << defuse << tmp );
                newWeb.uses.insert( tmp );
              }
              DOUT( endl );

              // Iterate over all webs identified up to now.
              auto it = mWebs.begin();
              while ( it != mWebs.end() ) {
                struct Web &w = *it;

                // Does the current def-use chain intersect with the current
                // web?
                bool usesIntersect = false;
                // uses intersect iff newWeb.uses intersect w.uses != empty
                for ( WIR_RegisterParameter &newp : newWeb.uses )
                  if ( w.uses.count( newp ) ) {
                    usesIntersect = true;
                    break;
                  }
                DOUT(
                  "usesIntersect = " <<
                  string( usesIntersect ? "true" : "false" ) << endl );

                bool defUsesIntersect = false;
                // If the current parameter rp is of defuse type, we have to be
                // careful. In this case, p might be a use appearing in some
                // other def-use chain so that p occurs as use in a completely
                // different web w. If this is the case, we must not generate a
                // new web here for rp and its associated def-use chain.
                // Instead, we have to merge p and its def-use chain with the
                // web w where p occurs as use.
                if ( rp.isDefUsed() )
                  // defUses intersect iff newWeb.defs intersect w.uses != empty
                  for ( WIR_RegisterParameter &newp : newWeb.defs )
                    if ( w.uses.count( newp ) ) {
                      defUsesIntersect = true;
                      break;
                    }
                DOUT(
                  "defUsesIntersect = " <<
                  string( defUsesIntersect ? "true" : "false" ) << endl );

                if ( usesIntersect || defUsesIntersect ) {
                  // Yes, the uses of the current def-use chain intersect with
                  // the uses of the current web, or rp is a defuse parameter
                  // intersecting with the uses of the current web.
                  // Thus, we merge the current web w with the new web and erase
                  // w.

                  // newWeb.defs := newWeb.defs U w.defs
                  for ( WIR_RegisterParameter &wp : w.defs )
                    newWeb.defs.insert( wp );

                  // newWeb.uses := newWeb.uses U w.uses
                  for ( WIR_RegisterParameter &wp : w.uses )
                    newWeb.uses.insert( wp );

                  it = mWebs.erase( it );
                } else
                  ++it;
              }

              // Finally, add the new web to the list of identified webs.
              mWebs.push_back( move( newWeb ) );
            }
          }

  DACTION(
    DOUT( "Identified webs for register " << r.getName() << ":" << endl );
    bool first = true;
    for ( Web &w : mWebs ) {
      if ( !first )
        DOUT( endl );
      first = false;
      DOUT( "  DEFs:" );
      for ( WIR_RegisterParameter &rp : w.defs )
        DOUT( " " << rp.getID() );
      DOUT( endl << "  USEs:" );
      for ( WIR_RegisterParameter &rp : w.uses )
        DOUT( " " << rp.getID() );
      DOUT( endl );
    } );
};


/*
  createWebs does the actual work to create the previously identified webs for
  the given virtual register.
*/
void WIR_Webs::createWebs( const WIR_VirtualRegister &r )
{
  DSTART( "void WIR_Webs::createWebs(const WIR_VirtualRegister&)" );

  // The current virtual register is only split into several ones if more than
  // one web has been found.
  if ( mWebs.size() > 1 ) {
    for ( Web &w : mWebs ) {
      struct Web newWeb;

      // Clone register r for the current web first.
      cloneRegisters( r );

      // Collect all parameters of the current web, be it defined or used.
      WIR_RegisterParameterSet params;

      for ( WIR_RegisterParameter &def : w.defs )
        params.insert( def );
      for ( WIR_RegisterParameter &use : w.uses )
        params.insert( use );

      // For all collected parameters, replace r by its clone.
      for ( auto it = params.begin(); it != params.end(); ++it )
        cloneParameter( *it, newWeb );

      mNewWebs.push_back( move( newWeb ) );
    }
  }

  // Free some no longer needed memory.
  mClonedReg.clear();
  mWebs.clear();
};


/*
  cloneRegisters creates an exact copy of the specified register and its entire
  register hierarchy and inserts it into the WIR function to which r belongs.
*/
void WIR_Webs::cloneRegisters( const WIR_VirtualRegister &r )
{
  DSTART( "void WIR_Webs::cloneRegisters(const WIR_VirtualRegister&)" );

  mClonedReg.clear();

  WIR_VirtualRegister &clone =
    r.getFunction().pushBackVirtualRegister( WIR_VirtualRegister( r ) );

  // Update map of clones.
  list<reference_wrapper<const WIR_VirtualRegister>> worklist;
  list<reference_wrapper<WIR_VirtualRegister>> clonedWorklist;

  worklist.push_back( r );
  clonedWorklist.push_back( clone );

  while ( !worklist.empty() ) {
    auto &reg = worklist.front().get();
    auto &clone = clonedWorklist.front().get();
    worklist.pop_front();
    clonedWorklist.pop_front();

    mClonedReg[ reg.getID() ] = &(clone);
    DOUT(
      "Adding new clone " << clone.getName() << " for register " <<
      reg.getName() << endl );

    for ( WIR_VirtualRegister &child : reg )
      worklist.push_back( child );
    for ( WIR_VirtualRegister &child : clone )
      clonedWorklist.push_back( child );
  }
};


/*
  cloneParameter creates a new %WIR register parameter as a clone for the
  specified parameter and replaces the old parameter by the the new one in its
  owning operation.
*/
void WIR_Webs::cloneParameter( WIR_RegisterParameter &p, struct Web &newWeb )
{
  DSTART(
    "void WIR_Webs::cloneParameter(WIR_RegisterParameter&, WIR_Webs::Web&)" );

  // Clone parameter p.
  WIR_RegisterParameter newParam(
    *(mClonedReg[ p.getRegister().getID() ] ), p.getUsage() );
  newParam.setImplicit( p.isImplicit() );

  // Replace p by newParam in the owning WIR operation.
  WIR_Operation &o = p.getOperation();
  auto it = o.getParameters().begin();
  while ( it->get() != p )
    ++it;

  DOUT(
    "Inserting web for parameter " << p.getID() << endl << "  Before:" <<
    defuse << o.getInstruction() );

  auto newIt = o.replaceParameter( it, move( newParam ) );
  auto &newP = dynamic_cast<WIR_RegisterParameter &>( newIt->get() );

  if ( newP.isDefined() || newP.isDefUsed() )
    newWeb.defs.insert( newP );
  if ( newP.isUsed() || newP.isDefUsed() )
    newWeb.uses.insert( newP );

  DOUT( "  After: " << defuse << o.getInstruction() );
};

}       // namespace WIR
