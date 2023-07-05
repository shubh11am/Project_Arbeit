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
  @brief This file implements a TriCore-specific optimization eliminating
         redundant code that computes bit-wise equivalent results.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <set>
#include <string>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>
#include <arch/tricore/analyses/bit/tcbitdfa.h>

// Include local headers
#include "tcredundantcode.h"


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
TC_RedundantCode::TC_RedundantCode( WIR_System &s ) :
  WIR_Optimization { s },
  WIR_RedundantCode { s }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
TC_RedundantCode::TC_RedundantCode( WIR_CompilationUnit &c ) :
  WIR_Optimization { c },
  WIR_RedundantCode { c }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
TC_RedundantCode::TC_RedundantCode( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_RedundantCode { f }
{
  DSTART( "TC_RedundantCode::TC_RedundantCode(WIR_Function&)" );
};


/*
  Destructor.
*/
TC_RedundantCode::~TC_RedundantCode( void )
{
  DSTART( "virtual TC_RedundantCode::~TC_RedundantCode()" );
};


//
// Protected class methods
//

/*
  runOptimization propagates constants in the given function.
*/
void TC_RedundantCode::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void TC_RedundantCode::runOptimization(WIR_Function&)" );

  TC_BitDFA analyzer { f };
  setDFA( analyzer );

  WIR_RedundantCode::runOptimization( f );
};


/*
  areRedundant checks whether the outgoing bit-value of a defined register
  parameter and the incoming bit-value of a used one are redundant for some
  actual processor architecture.

  For the TriCore architecture, areRedundant returns false if the defined
  register parameter is an implicit parameter of a function call, as a
  subsequent optimization would change the lifeness of this particular
  definition such that it could span other function calls which would render
  the optimization invalid.

  Likewise, if the defined register is used for argument passing for a function
  call, both def and use are also not redundant.

  Furthermore, the obeyance of particular TriCore operation formats that require
  the presence of particular physical registers (i.e., A10, A15 and D15) is
  ensured.
*/
bool TC_RedundantCode::areRedundant( const WIR_RegisterParameter &def,
                                     const WIR_UpDownValue &outValue,
                                     const WIR_RegisterParameter &use,
                                     const WIR_UpDownValue &inValue ) const
{
  DSTART(
    "virtual bool TC_RedundantCode::areRedundant(const WIR_RegisterParameter&, const WIR_UpDownValue&, const WIR_RegisterParameter&, const WIR_UpDownValue&) const" );

  (void) outValue;
  (void) inValue;

  // Check implicit defines of TriCore function calls.
  if ( def.isImplicit() &&
     ( def.getOperation().isCall() || def.getOperation().isIndirectCall() ) )
    return( false );

  // Check defines from TriCore moves that serve for argument passing of a
  // function call.
  static const set<string> argumentPassingRegs {
    "a4", "a5", "a6", "a7",
    "d4", "d5", "d6", "d7",
    "e4", "e6" };

  bool defIsArgumentPassingReg = false;
  if ( def.getRegister().isPhysical() &&
       argumentPassingRegs.count( def.getRegister().getName() ) )
    defIsArgumentPassingReg = true;
  if ( def.getRegister().isVirtual() ) {
    auto &vreg = dynamic_cast<WIR_VirtualRegister &>( def.getRegister() );
    if ( vreg.isPrecolored() &&
         argumentPassingRegs.count( vreg.getPrecolor().getName() ) )
      defIsArgumentPassingReg = true;
  }

  auto &oDef = def.getOperation();
  if ( defIsArgumentPassingReg ) {
    // OK, the def refers to a physical register used for function argument
    // passing. So, let's check whether the very last operation of the def's
    // basic block actually is a function call.
    auto &defBB = oDef.getInstruction().getBasicBlock();
    auto &lastOp = defBB.rbegin()->get().begin()->get();

    if ( lastOp.isCall() || lastOp.isIndirectCall() )
      return( false );
  }

  // Check TriCore-specific operation formats.
  auto &oUse = use.getOperation();
  int paramPos = -1;
  int i = 0;
  for ( WIR_Parameter &p : oUse.getExplicitParameters() ) {
    if ( p == use ) {
      paramPos = i;
      break;
    }
    ++i;
  }

  // Operation formats featuring the stack pointer.
  if ( ( ( oUse.getOperationFormat() == TC13::OperationFormat::SISPC10_1 ) &&
         ( paramPos == 1 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SISPC10_2 ) &&
         ( paramPos == 1 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SSPC8 ) &&
         ( paramPos == 0 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SSPC10I_1 ) &&
         ( paramPos == 0 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SSPC10I_2 ) &&
         ( paramPos == 0 ) ) )
    return( TC13::isSP( def.getRegister() ) );

  // Operation formats featuring A15.
  if ( ( ( oUse.getOperationFormat() == TC13::OperationFormat::SAC4I_1 ) &&
         ( paramPos == 2 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SAIC4 ) &&
         ( paramPos == 1 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SDIC4_1 ) &&
         ( paramPos == 1 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SIC4A ) &&
         ( paramPos == 0 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SIC4D ) &&
         ( paramPos == 0 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SSPC10I_1 ) &&
         ( paramPos == 2 ) ) )
    return( TC13::isA15( def.getRegister() ) );

  // Operation formats featuring D15.
  if ( ( ( oUse.getOperationFormat() == TC13::OperationFormat::SAAIC2 ) &&
         ( paramPos == 2 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SAC4I_2 ) &&
         ( paramPos == 2 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SDIC4_2 ) &&
         ( paramPos == 1 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SDIC4_3 ) &&
         ( paramPos == 1 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SDID_1 ) &&
         ( paramPos == 1 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SDID_2 ) &&
         ( paramPos == 1 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SIC4L ) &&
         ( paramPos == 0 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SIC5L ) &&
         ( paramPos == 0 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SIC8_2 ) &&
         ( paramPos == 0 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SIDL ) &&
         ( paramPos == 0 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SIL ) &&
         ( paramPos == 0 ) ) ||
       ( ( oUse.getOperationFormat() == TC13::OperationFormat::SSPC10I_2 ) &&
         ( paramPos == 2 ) ) )
    return( TC13::isD15( def.getRegister() ) );

  return( true );
};

}       // namespace WIR
