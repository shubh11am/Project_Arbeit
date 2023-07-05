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
  @file wirblockschedulingregion.cc
  @brief This file implements a class representing basic block regions in which
         scheduling is performed.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>

// Include local headers
#include "wirblockschedulingregion.h"


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
WIR_BlockSchedulingRegion::WIR_BlockSchedulingRegion( WIR_BasicBlock &b,
                                                      bool verbosity,
                                                      bool keepTmpFiles ) :
  WIR_SchedulingRegion { b, verbosity, keepTmpFiles }
{
  DSTART(
    "WIR_BlockSchedulingRegion::WIR_BlockSchedulingRegion(WIR_BasicBlock&, bool, bool)" );
};


/*
  Destructor.
*/
WIR_BlockSchedulingRegion::~WIR_BlockSchedulingRegion( void )
{
  DSTART( "virtual WIR_BlockSchedulingRegion::~WIR_BlockSchedulingRegion()" );
};

}       // namespace WIR
