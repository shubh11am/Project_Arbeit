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
  @file wirsiliconbugs.cc
  @brief This file implements a generic peephole optimimzer for silicon bug
         detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <string>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>

// Include local headers
#include "wirsiliconbugs.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor.
*/
WIR_SiliconBugs::WIR_SiliconBugs( WIR_Function &f, std::string &&n,
                                  unsigned int s, bool b ) :
  WIR_Peephole { f },
  mName { move( n ) }
{
  DSTART(
    "WIR_SiliconBugs::WIR_SiliconBugs(WIR_Function&, string&&, unsigned int, bool)" );

  addPeepholeSize( s );
  setCrossBasicBlocks( b );
};


/*
  Destructor.
*/
WIR_SiliconBugs::~WIR_SiliconBugs( void )
{
  DSTART( "virtual WIR_SiliconBugs::~WIR_SiliconBugs()" );
};


//
// Protected class methods
//

/*
  matchPeephole determines whether the specified peephole matches a silicon bug.
*/
bool WIR_SiliconBugs::matchPeephole( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool WIR_SiliconBugs::matchPeephole(const peephole&)" );

  DACTION(
    DOUT( "Current Peephole:" << endl );
    for ( unsigned int i = 0; i < p.size(); ++i )
      DOUT( p[ i ]->get() );
    DOUT( "matches silicon bug " << mName << ": "; ) );

  auto res = matchSiliconBug( p );

  DOUT( string( res ? "true" : "false" ) << "." << endl );

  return( res );
};


/*
  transformPeephole fixes a silicon bug.

  All WIR instructions of the fixed code sequence are marked as unmodifiable for
  subsequent optimizations.
*/
void WIR_SiliconBugs::transformPeephole( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual void WIR_SiliconBugs::transformPeephole(const peephole&)" );

  auto res = fixSiliconBug( p );

  DOUT( "Fix for silicon bug " << mName << ":" << endl; );

  for ( unsigned int i = 0; i < res.size(); ++i ) {
    res[ i ]->get().setDontOptimize();

    DOUT( res[ i ]->get(); );
  }
};


/*
  markInstruction marks the given instruction with a WIR comment that identifies
  it as fix for a silicon bug.
*/
void WIR_SiliconBugs::markInstruction( WIR_Instruction &i ) const
{
  DSTART( "void WIR_SiliconBugs::markInstruction(WIR_Instruction&) const" );

  i.insertContainer( WIR_Comment( "Fix for silicon bug " + mName + "." ) );
};

}       // namespace WIR
