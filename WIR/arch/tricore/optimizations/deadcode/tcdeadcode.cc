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
  @file tcdeadcode.cc
  @brief This file implements a TriCore-specific dead code elimination.

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
#include <arch/tricore/tc131.h>
#include <arch/tricore/analyses/bit/tcbitdfa.h>

// Include local headers
#include "tcdeadcode.h"


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
TC_DeadCode::TC_DeadCode( WIR_System &s ) :
  WIR_Optimization { s },
  WIR_DeadCode { s }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
TC_DeadCode::TC_DeadCode( WIR_CompilationUnit &c ) :
  WIR_Optimization { c },
  WIR_DeadCode { c }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
TC_DeadCode::TC_DeadCode( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_DeadCode { f }
{
  DSTART( "TC_DeadCode::TC_DeadCode(WIR_Function&)" );
};


/*
  Destructor.
*/
TC_DeadCode::~TC_DeadCode( void )
{
  DSTART( "virtual TC_DeadCode::~TC_DeadCode()" );
};


//
// Protected class methods
//

/*
  runOptimization eliminates dead code in the given function.
*/
void TC_DeadCode::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void TC_DeadCode::runOptimization(WIR_Function&)" );

  TC_BitDFA analyzer { f };
  setDFA( analyzer );

  WIR_DeadCode::runOptimization( f );
};

}       // namespace WIR
