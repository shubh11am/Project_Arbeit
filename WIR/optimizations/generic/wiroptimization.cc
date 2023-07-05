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
  @file wiroptimization.cc
  @brief This file implements the interface of generic %WIR code optimizations.

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

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>

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
WIR_Optimization::WIR_Optimization( WIR_System &s ) :
  mLevel { WIR_OptimizationLevel::sys },
  m16BitOperations { true },
  mID { s }
{
  DSTART( "WIR_Optimization::WIR_Optimization(WIR_System&)" );
};


/*
  Default constructor for compilation unit-level optimization.
*/
WIR_Optimization::WIR_Optimization( WIR_CompilationUnit &c ) :
  mLevel { WIR_OptimizationLevel::cu },
  m16BitOperations { true },
  mID { c }
{
  DSTART( "WIR_Optimization::WIR_Optimization(WIR_CompilationUnit&)" );
};


/*
  Default constructor for function-level optimization.
*/
WIR_Optimization::WIR_Optimization( WIR_Function &f ) :
  mLevel { WIR_OptimizationLevel::fct },
  m16BitOperations { true },
  mID { f }
{
  DSTART( "WIR_Optimization::WIR_Optimization(WIR_Function&)" );
};


/*
  Default constructor for basic block-level optimization.
*/
WIR_Optimization::WIR_Optimization( WIR_BasicBlock &b ) :
  mLevel { WIR_OptimizationLevel::bb },
  m16BitOperations { true },
  mID { b }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for instruction-level optimization.
*/
WIR_Optimization::WIR_Optimization( WIR_Instruction &i ) :
  mLevel { WIR_OptimizationLevel::ins },
  m16BitOperations { true },
  mID { i }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for operation-level optimization.
*/
WIR_Optimization::WIR_Optimization( WIR_Operation &o ) :
  mLevel { WIR_OptimizationLevel::op },
  m16BitOperations { true },
  mID { o }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
WIR_Optimization::~WIR_Optimization( void )
{
  DSTART( "virtual WIR_Optimization::~WIR_Optimization()" );
};


/*
  optimize performs an optimization.

  Depending on the value of mLevel, the corresponding runOptimization method
  is called.
*/
void WIR_Optimization::optimize( void )
{
  DSTART( "void WIR_Optimization::optimize()" );

  #ifdef FAILSAFEMODE
  // This lambda checks the presence of a 16-bit operation.
  auto checkOperationSize = [&]( const WIR_Operation &o ) {
    if ( o.getSize() == 2 ) {
      stringstream str;
      str << o;

      throw
        ufFatalError(
          "Found 16-bit operation despite them being deactivated: '" +
          str.str().substr( 8 ) + "'.",
          false );
    }
  };
  #endif

  switch ( mLevel ) {

    case WIR_OptimizationLevel::sys: {
      auto &sys = dynamic_cast<WIR_System &>( mID );

      #ifdef FAILSAFEMODE
      if ( !m16BitOperations )
        for ( WIR_CompilationUnit &c : sys )
          for ( WIR_Function &f : c )
            for ( WIR_BasicBlock &b : f )
              for ( WIR_Instruction &i : b )
                for ( WIR_Operation &o : i )
                  checkOperationSize( o );
      #endif

      runOptimization( sys );

      #ifdef FAILSAFEMODE
      if ( !m16BitOperations )
        for ( WIR_CompilationUnit &c : sys )
          for ( WIR_Function &f : c )
            for ( WIR_BasicBlock &b : f )
              for ( WIR_Instruction &i : b )
                for ( WIR_Operation &o : i )
                  checkOperationSize( o );
      #endif

      break;
    };

    case WIR_OptimizationLevel::cu: {
      auto &c = dynamic_cast<WIR_CompilationUnit &>( mID );

      #ifdef FAILSAFEMODE
      if ( !m16BitOperations )
        for ( WIR_Function &f : c )
          for ( WIR_BasicBlock &b : f )
            for ( WIR_Instruction &i : b )
              for ( WIR_Operation &o : i )
                checkOperationSize( o );
      #endif

      runOptimization( c );

      #ifdef FAILSAFEMODE
      if ( !m16BitOperations )
        for ( WIR_Function &f : c )
          for ( WIR_BasicBlock &b : f )
            for ( WIR_Instruction &i : b )
              for ( WIR_Operation &o : i )
                checkOperationSize( o );
      #endif

      break;
    };

    case WIR_OptimizationLevel::fct: {
      auto &f = dynamic_cast<WIR_Function &>( mID );

      #ifdef FAILSAFEMODE
      if ( !m16BitOperations )
        for ( WIR_BasicBlock &b : f )
          for ( WIR_Instruction &i : b )
            for ( WIR_Operation &o : i )
              checkOperationSize( o );
      #endif

      runOptimization( f );

      #ifdef FAILSAFEMODE
      if ( !m16BitOperations )
        for ( WIR_BasicBlock &b : f )
          for ( WIR_Instruction &i : b )
            for ( WIR_Operation &o : i )
              checkOperationSize( o );
      #endif

      break;
    };

    case WIR_OptimizationLevel::bb: {
      auto &b = dynamic_cast<WIR_BasicBlock &>( mID );

      #ifdef FAILSAFEMODE
      if ( !m16BitOperations )
        for ( WIR_Instruction &i : b )
          for ( WIR_Operation &o : i )
            checkOperationSize( o );
      #endif

      runOptimization( b );

      #ifdef FAILSAFEMODE
      if ( !m16BitOperations )
        for ( WIR_Instruction &i : b )
          for ( WIR_Operation &o : i )
            checkOperationSize( o );
      #endif

      break;
    };

    case WIR_OptimizationLevel::ins: {
      auto &i = dynamic_cast<WIR_Instruction &>( mID );

      #ifdef FAILSAFEMODE
      if ( !m16BitOperations )
        for ( WIR_Operation &o : i )
          checkOperationSize( o );
      #endif

      runOptimization( i );

      #ifdef FAILSAFEMODE
      if ( !m16BitOperations )
        for ( WIR_Operation &o : i )
          checkOperationSize( o );
      #endif

      break;
    };

    case WIR_OptimizationLevel::op: {
      auto &o = dynamic_cast<WIR_Operation &>( mID );

      #ifdef FAILSAFEMODE
      if ( !m16BitOperations )
        checkOperationSize( o );
      #endif

      runOptimization( o );

      #ifdef FAILSAFEMODE
      if ( !m16BitOperations )
        checkOperationSize( o );
      #endif

      break;
    };

  }
};


/*
  setGenerate16BitOperations sets whether an optimization shall generate 16 bits
  wide operations or not.
*/
void WIR_Optimization::setGenerate16BitOperations( bool f )
{
  DSTART( "void WIR_Optimization::setGenerate16BitOperations(bool)" );

  m16BitOperations = f;
};


/*
  getGenerate16BitOperations returns whether an optimization shall generate 16
  bits wide operations or not.
*/
bool WIR_Optimization::getGenerate16BitOperations( void ) const
{
  DSTART( "bool WIR_Optimization::getGenerate16BitOperations() const" );

  return( m16BitOperations );
};


//
// Protected class methods
//

/*
  runOptimization performs a system-level optimization.

  For actual system-level optimizations, this method needs to be overloaded in
  derived classes.
*/
void WIR_Optimization::runOptimization( WIR_System &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  (void) s;
};


/*
  runOptimization performs a compilation unit-level optimization.

  For actual compilation unit-level optimizations, this method needs to be
  overloaded in derived classes.
*/
void WIR_Optimization::runOptimization( WIR_CompilationUnit &c )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  (void) c;
};


/*
  runOptimization performs a function-level optimization.

  For actual function-level optimizations, this method needs to be overloaded in
  derived classes.
*/
void WIR_Optimization::runOptimization( WIR_Function &f )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  (void) f;
};


/*
  runAnalysis performs a basic block-level analysis.

  For actual basic block-level optimizations, this method needs to be overloaded
  in derived classes.
*/
void WIR_Optimization::runOptimization( WIR_BasicBlock &b )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  (void) b;
};


/*
  runOptimization performs an instruction-level optimization.

  For actual instruction-level optimizations, this method needs to be overloaded
  in derived classes.
*/
void WIR_Optimization::runOptimization( WIR_Instruction &i )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  (void) i;
};


/*
  runOptimization performs an operation-level optimization.

  For actual operation-level optimizations, this method needs to be overloaded
  in derived classes.
*/
void WIR_Optimization::runOptimization( WIR_Operation &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  (void) o;
};


/*
  copyContainers copies any kind of WIR containers from an original instruction
  to a new instruction stemming from some optimization.
*/
void WIR_Optimization::copyContainers( WIR_Instruction &i,
                                       const WIR_Instruction &iOrig,
                                       bool op ) const
{
  DSTART(
    "void WIR_Optimization::copyContainers(WIR_Instruction&, const WIR_Instruction&, bool) const" );

  // Copy an instruction's containers.
  for ( WIR_BaseContainer &c : iOrig.getContainers() )
    i.insertContainer( c );

  // Also include the instruction's operations.
  if ( op ) {
    auto it1 = i.begin();
    auto it2 = iOrig.begin();

    for ( ; it1 != i.end() && it2 != iOrig.end(); ++it1, ++it2 )
      copyContainers( it1->get(), it2->get() );
  }
};


/*
  copyContainers copies any kind of WIR containers from an original operation to
  a new operation stemming from some optimization.
*/
void WIR_Optimization::copyContainers( WIR_Operation &o,
                                       const WIR_Operation &oOrig ) const
{
  DSTART(
    "void WIR_Optimization::copyContainers(WIR_Operation&, const WIR_Operation&) const" );

  // Copy an operation's containers.
  for ( WIR_BaseContainer &c : oOrig.getContainers() )
    o.insertContainer( c );
};

}       // namespace WIR
