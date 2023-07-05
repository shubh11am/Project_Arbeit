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
  @file wircfganalysis.cc
  @brief This file implements generic CFG-based control flow analyses.

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


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for function-level analysis.
*/
WIR_ControlFlowAnalysis::WIR_ControlFlowAnalysis( WIR_Function &f ) :
  WIR_Analysis { f },
  WIR_CFG { f }
{
  DSTART( "WIR_ControlFlowAnalysis::WIR_ControlFlowAnalysis(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_ControlFlowAnalysis::~WIR_ControlFlowAnalysis( void )
{
  DSTART( "virtual WIR_ControlFlowAnalysis::~WIR_ControlFlowAnalysis()" );
};

}       // namespace WIR
