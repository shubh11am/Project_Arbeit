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
  @file wirpeephole.cc
  @brief This file implements a generic peephole optimimzer.

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
WIR_Peephole::WIR_Peephole( WIR_System &s ) :
  WIR_Optimization { s },
  mCrossBBs { false }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
WIR_Peephole::WIR_Peephole( WIR_CompilationUnit &c ) :
  WIR_Optimization { c },
  mCrossBBs { false }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
WIR_Peephole::WIR_Peephole( WIR_Function &f ) :
  WIR_Optimization { f },
  mCrossBBs { false }
{
  DSTART( "WIR_Peephole::WIR_Peephole(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_Peephole::~WIR_Peephole( void )
{
  DSTART( "virtual WIR_Peephole::~WIR_Peephole()" );
};


/*
  addPeepholeSize adds a peephole size.
*/
void WIR_Peephole::addPeepholeSize( unsigned int s )
{
  DSTART( "void WIR_Peephole::addPeepholeSize(unsigned int)" );

  ufAssertT( s > 0, "Peephole size must be greater than 0." );
  mPeepholeSizes.insert( s );
};


/*
  getPeepholeSizes returns the set of supported peephole sizes.
*/
const std::set<unsigned int> &WIR_Peephole::getPeepholeSizes( void ) const
{
  DSTART( "const set<unsigned int>& WIR_Peephole::getPeepholeSizes() const" );

  return( mPeepholeSizes );
};


/*
  setCrossBasicBlocks sets whether peepholes may cross basic block boundaries or
  not.
*/
void WIR_Peephole::setCrossBasicBlocks( bool b )
{
  DSTART( "void WIR_Peephole::setCrossBasicBlocks(bool)" );

  mCrossBBs = b;
};


/*
  getCrossBasicBlocks returns whether peepholes may cross basic block boundaries
  or not.
*/
bool WIR_Peephole::getCrossBasicBlocks( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mCrossBBs );
};


//
// Protected class methods
//

/*
  runOptimization performs peephole optimization in the given system.
*/
void WIR_Peephole::runOptimization( WIR_System &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_CompilationUnit &c : s )
    runOptimization( c );
};


/*
  runOptimization performs peephole optimization in the given compilation unit.
*/
void WIR_Peephole::runOptimization( WIR_CompilationUnit &c )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_Function &f : c )
    runOptimization( f );
};


/*
  runOptimization performs peephole optimization in the given function.
*/
void WIR_Peephole::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void WIR_Peephole::runOptimization(WIR_Function&)" );

  for ( WIR_BasicBlock &b : f )
    for ( auto it = b.begin(); it != b.end(); ++it )
      // Iterate over all existing peepholes.
      for ( auto &p : getPeepholes( it ) )
        if ( matchPeephole( p ) )
          transformPeephole( p );
};


/*
  getPeepholes determines all possible peepholes having the WIR instruction
  pointed to by the iterator as very first element.

  All returned peepholes have one of the sizes specified in mPeepholeSizes. The
  instructions in the returned peepholes may span multiple basic blocks as
  specified by mCrossBBs. The returned list of peepholes may be empty if no
  peepholes of a specified size exist at all from the starting position on.
*/
WIR_Peephole::peepholeList WIR_Peephole::getPeepholes( std::list<std::reference_wrapper<WIR::WIR_Instruction>>::const_iterator it ) const
{
  DSTART(
    "WIR_Peephole::peepholeList WIR_Peephole::getPeepholes(list<reference_wrapper<WIR_Instruction> >::const_iterator) const" );

  peepholeList res, q;
  auto maxLen = *(mPeepholeSizes.rbegin());

  // Apply a worklist algorithm to traverse the CFG.
  q.push_back( { it } );
  while ( !q.empty() ) {
    // Get a peephole from the worklist.
    auto p = q.front();
    q.pop_front();

    // Save peephole if it has a correct size.
    if ( mPeepholeSizes.count( p.size() ) )
      res.push_back( p );

    // Continue extending the current peephole if necessary.
    if ( p.size() < maxLen ) {
      // The current peephole is not yet large enough, try to extend it.
      auto lastPos = p.back();
      WIR_BasicBlock &b = lastPos->get().getBasicBlock();
      ++lastPos;

      if ( lastPos != b.end() ) {
        // The current peephole can be extended within the current basic block.
        p.push_back( lastPos );
        q.push_back( p );
      } else

      if ( mCrossBBs ) {
        // We reached the end of the current basic block. Let's traverse all
        // successors.
        auto succs = b.getSuccessors();
        while ( !succs.empty() ) {
          WIR_BasicBlock &succ = succs.begin()->get();
          succs.erase( succs.begin() );

          if ( !succ.getInstructions().empty() ) {
            // The current successor basic block is non-empty. Let's add a new
            // peephole to the worklist including the successor's very first
            // instruction.
            peephole s = p;
            s.push_back( succ.begin() );
            q.push_back( s );
          } else
            // The current successor basic block is empty. Let's check all
            // successors of the empty successor.
            for ( WIR_BasicBlock &s : succ.getSuccessors() )
              succs.insert( s );
        }
      }
    }
  }

  return( res );
};

}       // namespace WIR
