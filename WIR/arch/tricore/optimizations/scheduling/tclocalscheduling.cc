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
  @file tclocalscheduling.cc
  @brief This file implements a TriCore-specific optimization performing
         instruction scheduling locally within basic blocks.

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
#include <optimizations/scheduling/wirschedulingpriority.h>

// Include local headers
#include "tcblockschedulingregion.h"
#include "tclocalscheduling.h"


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
TC_LocalScheduling::TC_LocalScheduling( WIR_System &s,
                                        const WIR_SchedulingPriority &p,
                                        bool verbosity, bool keepTmpFiles ) :
  WIR_LocalScheduling { s, p, verbosity, keepTmpFiles }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
TC_LocalScheduling::TC_LocalScheduling( WIR_CompilationUnit &c,
                                        const WIR_SchedulingPriority &p,
                                        bool verbosity, bool keepTmpFiles ) :
  WIR_LocalScheduling { c, p, verbosity, keepTmpFiles }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
TC_LocalScheduling::TC_LocalScheduling( WIR_Function &f,
                                        const WIR_SchedulingPriority &p,
                                        bool verbosity, bool keepTmpFiles ) :
  WIR_LocalScheduling { f, p, verbosity, keepTmpFiles }
{
  DSTART(
    "TC_LocalScheduling::TC_LocalScheduling(WIR_Function&, const WIR_SchedulingPriority&, bool, bool)" );
};


/*
  Destructor.
*/
TC_LocalScheduling::~TC_LocalScheduling( void )
{
  DSTART( "virtual TC_LocalScheduling::~TC_LocalScheduling()" );
};


//
// Protected class methods
//

/*!
  generateBlockRegion generates a new TriCore block region for scheduling.

  The pointer returned by generateBlockRegion will automatically be freed by
  the scheduler class TC_LocalScheduling.
*/
WIR_BlockSchedulingRegion *TC_LocalScheduling::generateBlockRegion( WIR_BasicBlock &b ) const
{
  DSTART(
    "virtual WIR::WIR_BlockSchedulingRegion* TC_LocalScheduling::generateBlockRegion(WIR_BasicBlock&) const" );

  auto *res = new TC_BlockSchedulingRegion( b, mVerbosity, mKeepTmpFiles );
  res->buildDG();

  return( res );
};

}       // namespace WIR
