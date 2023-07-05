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
  @file peep_extrst.cc
  @brief This file implements a peephole optimizer for
         <TT>EXTR</TT>/<TT>EXTR.U</TT> operations followed by a
         <TT>ST.B</TT>/<TT>ST.H</TT>/<TT>ST.W</TT>.

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
#include <arch/tricore/tc131.h>
#include <arch/tricore/analyses/bit/tcbitdfa.h>

// Include local headers
#include "peep_extrst.h"


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
TC_Peep_EXTRST::TC_Peep_EXTRST( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_BitOpt { f },
  WIR_Peephole { f }
{
  DSTART( "TC_Peep_EXTRST::TC_Peep_EXTRST(WIR_Function&)" );

  addPeepholeSize( 1 );
};


/*
  Destructor.
*/
TC_Peep_EXTRST::~TC_Peep_EXTRST( void )
{
  DSTART( "virtual TC_Peep_EXTRST::~TC_Peep_EXTRST()" );
};


/*
  optimize performs the peephole optimization.
*/
void TC_Peep_EXTRST::optimize( void )
{
  DSTART( "void TC_Peep_EXTRST::optimize()" );

  WIR_Peephole::optimize();
};


//
// Protected class methods
//

/*
  runOptimization performs peephole optimization in the given function.
*/
void TC_Peep_EXTRST::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void TC_Peep_EXTRST::runOptimization(WIR_Function&)" );

  TC_BitDFA analyzer { f };
  setDFA( analyzer );

  // Perform bit-true data and value flow analysis first.
  if ( mRunDFA && ( mBitDFA != nullptr ) )
    mBitDFA->analyze();

  WIR_Peephole::runOptimization( f );
  verifyLocations( f );
};


/*
  matchPeephole determines whether the specified peephole matches with the
  operation pattern EXTR, ST.
*/
bool TC_Peep_EXTRST::matchPeephole( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC_Peep_EXTRST::matchPeephole(const peephole&)" );

  mFoundSTs.clear();
  WIR_Operation &o0 = p[ 0 ]->get().begin()->get();

  if ( ( ( o0.getOpCode() == TC13::OpCode::EXTR ) ||
         ( o0.getOpCode() == TC13::OpCode::EXTR_U ) ) &&
       ( o0.getOperationFormat() == TC13::OperationFormat::DDC5C5 ) &&
       ( dynamic_cast<TC_Const5_Unsigned &>(
           o0.getExplicitParameter( 3 ) ).getUnsignedValue() == 0 ) ) {
    // We found an EXTR. Check all operations that are data flow-dependent on
    // the EXTR's result.
    unsigned int w =
      dynamic_cast<TC_Const5_Unsigned &>(
        o0.getExplicitParameter( 4 ) ).getUnsignedValue();
    auto &pExtr1 =
      dynamic_cast<WIR_RegisterParameter &>( o0.getExplicitParameter( 1 ) );

    // Determine the bitValue container of the EXTR's defined parameter.
    auto &c1 = pExtr1.getContainers<WIR_BitValues>().begin()->get();

    // Iterate all out-edges.
    for ( auto &outEdge : c1.getOutValues() ) {
      auto &o1 = outEdge.rp->getOperation();

      if ( ( ( ( o1.getOpCode() == TC13::OpCode::ST_B ) && ( w >= 8 ) ) ||
             ( ( o1.getOpCode() == TC13::OpCode::ST_H ) && ( w >= 16 ) ) ||
             ( ( o1.getOpCode() == TC13::OpCode::ST_W ) && ( w >= 32 ) ) ) &&
           ( pExtr1.getRegister() ==
             dynamic_cast<WIR_RegisterParameter &>(
               o1.getExplicitParameter(
                 o1.getExplicitParameters().size() ) ).getRegister() ) &&
           !o1.getDontOptimize() ) {
        // Determine the bitValue container of the ST's used parameter.
        auto &pStLast =
          o1.getExplicitParameter( o1.getExplicitParameters().size() );
        auto &c2 = pStLast.getContainers<WIR_BitValues>().begin()->get();

        if ( c2.getInValues().size() == 1 )
          mFoundSTs.insert( o1 );
      }
    }
  }

  return( !mFoundSTs.empty() );
};


/*
  transformPeephole optimizes the operation pattern EXTR, ST in the specified
  peephole.
*/
void TC_Peep_EXTRST::transformPeephole( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual void TC_Peep_EXTRST::transformPeephole(const peephole&)" );

  WIR_Operation &o0 = p[ 0 ]->get().begin()->get();
  auto &pExtr1 =
    dynamic_cast<WIR_RegisterParameter &>( o0.getExplicitParameter( 1 ) );
  auto &pExtr2 =
    dynamic_cast<WIR_RegisterParameter &>( o0.getExplicitParameter( 2 ) );
  auto &c1 = pExtr1.getContainers<WIR_BitValues>().begin()->get();

  for ( WIR_Operation &o1 : mFoundSTs ) {
    auto it =
      o1.findParameter(
        o1.getExplicitParameter( o1.getExplicitParameters().size() ) );
    auto &pStLast = it->get();

    // Erase the register used in the ST from the EXTR's defined parameter.
    c1.eraseOutValues( pStLast );

    // We simply patch the ST operation such that it uses pExtr2. The EXTR
    // should then be removed by a subsequent dead code elimination.
    it =
      o1.replaceParameter(
        it, WIR_RegisterParameter { pExtr2.getRegister(), WIR_Usage::use } );

    // Attach a bit-value container to the newly produced register parameter.
    auto *cont = new WIR_BitValues();
    it->get().insertContainer( cont );

    // Duplicate all incoming bit-values from the EXTR's used parameter to the
    // ST's new register parameter.
    auto &c2 = pExtr2.getContainers<WIR_BitValues>().begin()->get();
    for ( auto &inEdge : c2.getInValues() ) {
      cont->insertInValues(
        *(inEdge.rp), WIR_UpDownValue { inEdge.downVal },
        WIR_UpDownValue { inEdge.upVal } );

      auto &srcContainer =
        inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
      srcContainer.insertOutValues(
        it->get(), WIR_UpDownValue { inEdge.downVal },
        WIR_UpDownValue { inEdge.upVal } );
    }
  }
};

}       // namespace WIR
