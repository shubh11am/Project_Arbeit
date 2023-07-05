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
  @file wiranalysis.cc
  @brief This file implements the interface of generic %WIR code analyses.

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
  Default constructor for system-level analysis.
*/
WIR_Analysis::WIR_Analysis( WIR_System &o ) :
  mLevel { WIR_AnalysisLevel::sys },
  mID { o }
{
  DSTART( "WIR_Analysis::WIR_Analysis(WIR_System&)" );
};


/*
  Default constructor for compilation unit-level analysis.
*/
WIR_Analysis::WIR_Analysis( WIR_CompilationUnit &o ) :
  mLevel { WIR_AnalysisLevel::cu },
  mID { o }
{
  DSTART( "WIR_Analysis::WIR_Analysis(WIR_CompilationUnit&)" );
};


/*
  Default constructor for function-level analysis.
*/
WIR_Analysis::WIR_Analysis( WIR_Function &o ) :
  mLevel { WIR_AnalysisLevel::fct },
  mID { o }
{
  DSTART( "WIR_Analysis::WIR_Analysis(WIR_Function&)" );
};


/*
  Default constructor for basic block-level analysis.
*/
WIR_Analysis::WIR_Analysis( WIR_BasicBlock &o ) :
  mLevel { WIR_AnalysisLevel::bb },
  mID { o }
{
  DSTART( "WIR_Analysis::WIR_Analysis(WIR_BasicBlock&)" );
};


/*
  Default constructor for instruction-level analysis.
*/
WIR_Analysis::WIR_Analysis( WIR_Instruction &o ) :
  mLevel { WIR_AnalysisLevel::ins },
  mID { o }
{
  DSTART( "WIR_Analysis::WIR_Analysis(WIR_Instruction&)" );
};


/*
  Default constructor for operation-level analysis.
*/
WIR_Analysis::WIR_Analysis( WIR_Operation &o ) :
  mLevel { WIR_AnalysisLevel::op },
  mID { o }
{
  DSTART( "WIR_Analysis::WIR_Analysis(WIR_Operation&)" );
};


/*
  Destructor.
*/
WIR_Analysis::~WIR_Analysis( void )
{
  DSTART( "virtual WIR_Analysis::~WIR_Analysis()" );
};


/*
  analyze performs an analysis.

  Depending on the value of mLevel, the corresponding runAnalysis method is
  called.
*/
void WIR_Analysis::analyze( void )
{
  DSTART( "void WIR_Analysis::analyze()" );

  switch ( mLevel ) {

    case WIR_AnalysisLevel::sys: {
      runAnalysis( dynamic_cast<WIR_System &>( mID ) );
      break;
    };

    case WIR_AnalysisLevel::cu: {
      runAnalysis( dynamic_cast<WIR_CompilationUnit &>( mID ) );
      break;
    };

    case WIR_AnalysisLevel::fct: {
      runAnalysis( dynamic_cast<WIR_Function &>( mID ) );
      break;
    };

    case WIR_AnalysisLevel::bb: {
      runAnalysis( dynamic_cast<WIR_BasicBlock &>( mID ) );
      break;
    };

    case WIR_AnalysisLevel::ins: {
      runAnalysis( dynamic_cast<WIR_Instruction &>( mID ) );
      break;
    };

    case WIR_AnalysisLevel::op: {
      runAnalysis( dynamic_cast<WIR_Operation &>( mID ) );
      break;
    };

  }
};


//
// Protected class methods
//

/*
  runAnalysis performs a system-level analysis.

  For actual system-level analyses, this method needs to be overloaded in
  derived classes.
*/
void WIR_Analysis::runAnalysis( WIR_System &o )
{
  DSTART( "virtual void WIR_Analysis::runAnalysis(WIR_System&)" );

  (void) o;
};


/*
  runAnalysis performs a compilation unit-level analysis.

  For actual compilation unit-level analyses, this method needs to be overloaded
  in derived classes.
*/
void WIR_Analysis::runAnalysis( WIR_CompilationUnit &o )
{
  DSTART( "virtual void WIR_Analysis::runAnalysis(WIR_CompilationUnit&)" );

  (void) o;
};


/*
  runAnalysis performs a function-level analysis.

  For actual function-level analyses, this method needs to be overloaded in
  derived classes.
*/
void WIR_Analysis::runAnalysis( WIR_Function &o )
{
  DSTART( "virtual void WIR_Analysis::runAnalysis(WIR_Function&)" );

  (void) o;
};


/*
  runAnalysis performs a basic block-level analysis.

  For actual basic bloc-level analyses, this method needs to be overloaded in
  derived classes.
*/
void WIR_Analysis::runAnalysis( WIR_BasicBlock &o )
{
  DSTART( "virtual void WIR_Analysis::runAnalysis(WIR_BasicBlock&)" );

  (void) o;
};


/*
  runAnalysis performs a instruction-level analysis.

  For actual instruction-level analyses, this method needs to be overloaded in
  derived classes.
*/
void WIR_Analysis::runAnalysis( WIR_Instruction &o )
{
  DSTART( "virtual void WIR_Analysis::runAnalysis(WIR_Instruction&)" );

  (void) o;
};


/*
  runAnalysis performs an operation-level analysis.

  For actual operation-level analyses, this method needs to be overloaded in
  derived classes.
*/
void WIR_Analysis::runAnalysis( WIR_Operation &o )
{
  DSTART( "virtual void WIR_Analysis::runAnalysis(WIR_Operation&)" );

  (void) o;
};

}       // namespace WIR
