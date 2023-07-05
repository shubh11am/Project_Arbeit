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
  @file peep_ldextr.cc
  @brief This file implements a peephole optimizer for
         <TT>LD.B</TT>/<TT>LD.BU</TT>/<TT>LD.H</TT>/<TT>LD.HU</TT>/<TT>LD.W</TT>
         operations followed by a <TT>EXTR</TT>/<TT>EXTR.U</TT>.

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
#include "peep_ldextr.h"


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
TC_Peep_LDEXTR::TC_Peep_LDEXTR( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_BitOpt { f },
  WIR_Peephole { f },
  mPatchLd { false },
  mNewOpcode { nullptr }
{
  DSTART( "TC_Peep_LDEXTR::TC_Peep_LDEXTR(WIR_Function&)" );

  addPeepholeSize( 1 );
};


/*
  Destructor.
*/
TC_Peep_LDEXTR::~TC_Peep_LDEXTR( void )
{
  DSTART( "virtual TC_Peep_LDEXTR::~TC_Peep_LDEXTR()" );
};


/*
  optimize performs the peephole optimization.
*/
void TC_Peep_LDEXTR::optimize( void )
{
  DSTART( "void TC_Peep_LDEXTR::optimize()" );

  WIR_Peephole::optimize();
};


//
// Protected class methods
//

/*
  runOptimization performs peephole optimization in the given function.
*/
void TC_Peep_LDEXTR::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void TC_Peep_LDEXTR::runOptimization(WIR_Function&)" );

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
  operation pattern LD, EXTR.
*/
bool TC_Peep_LDEXTR::matchPeephole( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC_Peep_LDEXTR::matchPeephole(const peephole&)" );

  mFoundEXTRs.clear();
  mPatchLd = false;
  WIR_Operation &o0 = p[ 0 ]->get().begin()->get();

  // A small lambda to check for a proper EXTR operation.
  auto isEXTR = [&]( unsigned int l, bool eq, bool u, bool allSame ) -> bool {
    bool res = false;
    auto &pLd1 =
      dynamic_cast<WIR_RegisterParameter &>( o0.getExplicitParameter( 1 ) );
    auto &c = pLd1.getContainers<WIR_BitValues>().begin()->get();
    auto opc = u ? TC13::OpCode::EXTR_U : TC13::OpCode::EXTR;

    // Iterate all out-edges.
    for ( auto &outEdge : c.getOutValues() ) {
      auto &o1 = outEdge.rp->getOperation();

      if ( ( o1.getOpCode() == opc ) &&
           ( o1.getOperationFormat() == TC13::OperationFormat::DDC5C5 ) &&
           ( dynamic_cast<WIR_RegisterParameter &>(
               o1.getExplicitParameter( 2 ) ).getRegister() ==
                 pLd1.getRegister() ) &&
           ( o1.getExplicitParameter( 2 ).getContainers<WIR_BitValues>().begin()->get().getInValues().size() == 1 ) &&
           ( dynamic_cast<TC_Const5_Unsigned &>(
               o1.getExplicitParameter( 3 ) ).getUnsignedValue() == 0 ) &&
           !o1.getDontOptimize() ) {
        unsigned int w =
          dynamic_cast<TC_Const5_Unsigned &>(
            o1.getExplicitParameter( 4 ) ).getUnsignedValue();

        if ( ( eq && ( w == l ) ) || ( !eq && ( w >= l ) ) ) {
          mFoundEXTRs.insert( o1 );
          res = true;
        }
      }
    }

    if ( !allSame )
      return( res );
    else
      return( mFoundEXTRs.size() == c.getOutValues().size() );
  };

  if ( ( ( o0.getOpCode() == TC13::OpCode::LD_B ) &&
         isEXTR( 8, false, false, false ) ) ||
       ( ( o0.getOpCode() == TC13::OpCode::LD_BU ) &&
         isEXTR( 9, false, false, false ) ) ||
       ( ( o0.getOpCode() == TC13::OpCode::LD_BU ) &&
         isEXTR( 8, false, true, false ) ) ||
       ( ( o0.getOpCode() == TC13::OpCode::LD_H ) &&
         isEXTR( 16, false, false, false ) ) ||
       ( ( o0.getOpCode() == TC13::OpCode::LD_HU ) &&
         isEXTR( 16, false, true, false ) ) ||
       ( ( o0.getOpCode() == TC13::OpCode::LD_HU ) &&
         isEXTR( 17, false, false, false ) ) ) {}
  else

  if ( ( o0.getOpCode() == TC13::OpCode::LD_B ) &&
       isEXTR( 8, true, true, true ) ) {
    mPatchLd = true;
    mNewOpcode = &TC13::OpCode::LD_BU;
  } else

  if ( ( o0.getOpCode() == TC13::OpCode::LD_BU ) && ( o0.getSize() == 4 ) &&
       isEXTR( 8, true, false, true ) ) {
    mPatchLd = true;
    mNewOpcode = &TC13::OpCode::LD_B;
  } else

  if ( ( o0.getOpCode() == TC13::OpCode::LD_H ) && ( o0.getSize() == 4 ) &&
       isEXTR( 16, true, true, true ) ) {
    mPatchLd = true;
    mNewOpcode = &TC13::OpCode::LD_HU;
  } else

  if ( ( o0.getOpCode() == TC13::OpCode::LD_H ) && ( o0.getSize() == 4 ) &&
       isEXTR( 8, true, false, true ) ) {
    mPatchLd = true;
    mNewOpcode = &TC13::OpCode::LD_B;
  } else

  if ( ( o0.getOpCode() == TC13::OpCode::LD_H ) &&
       isEXTR( 8, true, true, true ) ) {
    mPatchLd = true;
    mNewOpcode = &TC13::OpCode::LD_BU;
  } else

  if ( ( o0.getOpCode() == TC13::OpCode::LD_HU ) &&
       isEXTR( 16, true, false, true ) ) {
    mPatchLd = true;
    mNewOpcode = &TC13::OpCode::LD_H;
  } else

  if ( ( o0.getOpCode() == TC13::OpCode::LD_HU ) &&
       isEXTR( 8, true, false, true ) ) {
    mPatchLd = true;
    mNewOpcode = &TC13::OpCode::LD_B;
  } else

  if ( ( o0.getOpCode() == TC13::OpCode::LD_HU ) &&
       isEXTR( 8, true, true, true ) ) {
    mPatchLd = true;
    mNewOpcode = &TC13::OpCode::LD_BU;
  } else

  if ( ( o0.getOpCode() == TC13::OpCode::LD_W ) && ( o0.getSize() == 4 ) &&
       ( ( o0.getOperationFormat() != TC13::OperationFormat::DAC16BOA ) ||
         ( ( o0.getOperationFormat() == TC13::OperationFormat::DAC16BOA ) &&
           ( dynamic_cast<TC_Const16_Signed &>(
               o0.getExplicitParameter( 3 ) ).getSignedValue() >=
               TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( dynamic_cast<TC_Const16_Signed &>(
               o0.getExplicitParameter( 3 ) ).getSignedValue() <=
               TC_Const10_Signed::getMaxValue( 10 ) ) ) ) &&
       ( o0.getOperationFormat() != TC13::OperationFormat::DALC16BOA ) &&
       isEXTR( 8, true, false, true ) ) {
    mPatchLd = true;
    mNewOpcode = &TC13::OpCode::LD_B;
  } else

  if ( ( o0.getOpCode() == TC13::OpCode::LD_W ) &&
       ( ( o0.getOperationFormat() != TC13::OperationFormat::DAC16BOA ) ||
         ( ( o0.getOperationFormat() == TC13::OperationFormat::DAC16BOA ) &&
           ( dynamic_cast<TC_Const16_Signed &>(
               o0.getExplicitParameter( 3 ) ).getSignedValue() >=
               TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( dynamic_cast<TC_Const16_Signed &>(
               o0.getExplicitParameter( 3 ) ).getSignedValue() <=
               TC_Const10_Signed::getMaxValue( 10 ) ) ) ) &&
       ( o0.getOperationFormat() != TC13::OperationFormat::DALC16BOA ) &&
       ( o0.getOperationFormat() != TC13::OperationFormat::SISPC10_2 ) &&
       isEXTR( 8, true, true, true ) ) {
    mPatchLd = true;
    mNewOpcode = &TC13::OpCode::LD_BU;
  } else

  if ( ( o0.getOpCode() == TC13::OpCode::LD_W ) &&
       ( ( o0.getOperationFormat() != TC13::OperationFormat::DAC16BOA ) ||
         ( ( o0.getOperationFormat() == TC13::OperationFormat::DAC16BOA ) &&
           ( dynamic_cast<TC_Const16_Signed &>(
               o0.getExplicitParameter( 3 ) ).getSignedValue() >=
               TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( dynamic_cast<TC_Const16_Signed &>(
               o0.getExplicitParameter( 3 ) ).getSignedValue() <=
               TC_Const10_Signed::getMaxValue( 10 ) ) ) ) &&
       ( o0.getOperationFormat() != TC13::OperationFormat::DALC16BOA ) &&
       ( o0.getOperationFormat() != TC13::OperationFormat::SISPC10_2 ) &&
       isEXTR( 16, true, false, true ) ) {
    mPatchLd = true;
    mNewOpcode = &TC13::OpCode::LD_H;
  } else

  if ( ( o0.getOpCode() == TC13::OpCode::LD_W ) && ( o0.getSize() == 4 ) &&
       ( ( o0.getOperationFormat() != TC13::OperationFormat::DAC16BOA ) ||
         ( ( o0.getOperationFormat() == TC13::OperationFormat::DAC16BOA ) &&
           ( dynamic_cast<TC_Const16_Signed &>(
               o0.getExplicitParameter( 3 ) ).getSignedValue() >=
               TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( dynamic_cast<TC_Const16_Signed &>(
               o0.getExplicitParameter( 3 ) ).getSignedValue() <=
               TC_Const10_Signed::getMaxValue( 10 ) ) ) ) &&
       ( o0.getOperationFormat() != TC13::OperationFormat::DALC16BOA ) &&
       isEXTR( 16, true, true, true ) ) {
    mPatchLd = true;
    mNewOpcode = &TC13::OpCode::LD_HU;
  }

  return( !mFoundEXTRs.empty() );
};


/*
  transformPeephole optimizes the operation pattern LD, EXTR in the specified
  peephole.
*/
void TC_Peep_LDEXTR::transformPeephole( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual void TC_Peep_LDEXTR::transformPeephole(const peephole&)" );

  WIR_Instruction &i = p[ 0 ]->get();

  // Let's update the LD operation first, if required.
  if ( mPatchLd ) {
    WIR_Operation &o0 = i.begin()->get();

    if ( o0.getOperationFormat() == TC13::OperationFormat::DC18ABSA )
      i.pushBackOperation(
        { *mNewOpcode, TC13::OperationFormat::DC18ABSA,
          WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 1 ) ) ),
          TC_Const18_Unsigned(
            dynamic_cast<TC_Const18_Unsigned &>(
              o0.getExplicitParameter( 2 ) ) ) } );
    else

    if ( o0.getOperationFormat() == TC13::OperationFormat::DLABSA )
      i.pushBackOperation(
        { *mNewOpcode, TC13::OperationFormat::DLABSA,
          WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 1 ) ) ),
          WIR_LabelParameter(
            dynamic_cast<WIR_LabelParameter &>(
              o0.getExplicitParameter( 2 ) ) ) } );
    else

    if ( o0.getOperationFormat() == TC13::OperationFormat::DAC10BOA )
      i.pushBackOperation(
        { *mNewOpcode, TC13::OperationFormat::DAC10BOA,
          WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 1 ) ) ),
          WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 2 ) ) ),
          TC_Const10_Signed(
            dynamic_cast<TC_Const10_Signed &>(
              o0.getExplicitParameter( 3 ) ) ) } );
    else

    if ( o0.getOperationFormat() == TC13::OperationFormat::DPBRA )
      i.pushBackOperation(
        { *mNewOpcode, TC13::OperationFormat::DPBRA,
          WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 1 ) ) ),
          WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 2 ) ) ) } );
    else

    if ( o0.getOperationFormat() == TC13::OperationFormat::DPC10CA )
      i.pushBackOperation(
        { *mNewOpcode, TC13::OperationFormat::DPC10CA,
          WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 1 ) ) ),
          WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 2 ) ) ),
          TC_Const10_Signed(
            dynamic_cast<TC_Const10_Signed &>(
              o0.getExplicitParameter( 3 ) ) ) } );
    else

    if ( o0.getOperationFormat() == TC13::OperationFormat::DAC10PIA )
      i.pushBackOperation(
        { *mNewOpcode, TC13::OperationFormat::DAC10PIA,
          WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 1 ) ) ),
          WIR_AddressingModeParameter(
            dynamic_cast<WIR_AddressingModeParameter &>(
              o0.getExplicitParameter( 2 ) ) ),
          WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 3 ) ) ),
          TC_Const10_Signed(
            dynamic_cast<TC_Const10_Signed &>(
              o0.getExplicitParameter( 4 ) ) ) } );
    else

    if ( o0.getOperationFormat() == TC13::OperationFormat::DAC16BOA )
      i.pushBackOperation(
        { *mNewOpcode, TC13::OperationFormat::DAC10BOA,
          WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 1 ) ) ),
          WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 2 ) ) ),
          TC_Const10_Signed(
            dynamic_cast<TC_Const16_Signed &>(
              o0.getExplicitParameter( 3 ) ).getSignedValue() ) } );

    // Duplicate all bit value containers from o0 to the new operation.
    auto it = o0.getExplicitParameters().begin();
    auto it1 = i.rbegin()->get().getExplicitParameters().begin();

    for ( ; it != o0.getExplicitParameters().end(); ++it, ++it1 ) {

      if ( ( it->get().getType() == WIR_ParameterType::reg ) &&
           ( dynamic_cast<WIR_RegisterParameter &>( it->get() ).isDefined() ||
             dynamic_cast<WIR_RegisterParameter &>( it->get() ).isDefUsed() ) )
        mNewLocation.insert(
          { it->get().getID(),
            ref( dynamic_cast<WIR_RegisterParameter &>( it1->get() ) ) } );

      if ( it->get().containsContainers(
             WIR_BitValues::getContainerTypeID() ) ) {
        auto &cOrig = it->get().getContainers<WIR_BitValues>().begin()->get();

        auto *cNew = new WIR_BitValues();
        it1->get().insertContainer( cNew );

        for ( auto &inEdge : cOrig.getInValues() ) {
          cNew->insertInValues(
            *(inEdge.rp), WIR_UpDownValue { inEdge.downVal },
            WIR_UpDownValue { inEdge.upVal } );

          auto &srcContainer =
            inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
          srcContainer.insertOutValues(
            it1->get(), WIR_UpDownValue { inEdge.downVal },
            WIR_UpDownValue { inEdge.upVal } );
        }

        for ( auto &outEdge : cOrig.getOutValues() ) {
          cNew->insertOutValues(
            *(outEdge.rp), WIR_UpDownValue { outEdge.downVal },
            WIR_UpDownValue { outEdge.upVal } );

          auto &tgtContainer =
            outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
          tgtContainer.insertInValues(
            it1->get(), WIR_UpDownValue { outEdge.downVal },
            WIR_UpDownValue { outEdge.upVal } );
        }
      }
    }

    // Remove o0 from all its related bitValue containers.
    for ( auto it = o0.begin(); it != o0.end(); ++it )
      if ( it->get().containsContainers(
             WIR_BitValues::getContainerTypeID() ) ) {
        auto &cOrig = it->get().getContainers<WIR_BitValues>().begin()->get();

        for ( auto &inEdge : cOrig.getInValues() ) {
          auto &srcContainer =
            inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
          srcContainer.eraseOutValues( it->get() );
        }

        for ( auto &outEdge : cOrig.getOutValues() ) {
          auto &tgtContainer =
            outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
          tgtContainer.eraseInValues( it->get() );
        }
      }
  }

  // We have to erase all the identified EXTR operations. For the sake of
  // simplicty, we replace each EXTR by a MOV which will then be folded away
  // during coalescing of the subsequent register allocator.
  for ( WIR_Operation &o1 : mFoundEXTRs ) {
    auto &pExtr1 =
      dynamic_cast<WIR_RegisterParameter &>( o1.getExplicitParameter( 1 ) );
    auto &pExtr2 =
      dynamic_cast<WIR_RegisterParameter &>( o1.getExplicitParameter( 2 ) );

    WIR_Instruction &i = o1.getInstruction();
    auto *r1 =
      new WIR_RegisterParameter( pExtr1.getRegister(), WIR_Usage::def );
    auto *r2 =
      new WIR_RegisterParameter( pExtr2.getRegister(), WIR_Usage::use );
    i.pushBackOperation(
      { TC13::OpCode::MOV_RR,
        m16BitOperations ?
          TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
        r1, r2 } );

    mNewLocation.insert( { pExtr1.getID(), ref( *r1 ) } );

    // Copy bit values from pExtr1 to r1.
    auto &cont1 = pExtr1.getContainers<WIR_BitValues>().begin()->get();
    auto *c1 = new WIR_BitValues();
    r1->insertContainer( c1 );
    for ( auto &outEdge : cont1.getOutValues() ) {
      c1->insertOutValues(
        *(outEdge.rp), WIR_UpDownValue { outEdge.downVal },
        WIR_UpDownValue { outEdge.upVal } );

      auto &tgtContainer =
        outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
      tgtContainer.insertInValues(
        *r1, WIR_UpDownValue { outEdge.downVal },
        WIR_UpDownValue { outEdge.upVal } );
      tgtContainer.eraseInValues( pExtr1 );
    }

    // Copy bit values from pExtr2 to r2.
    auto &cont2 = pExtr2.getContainers<WIR_BitValues>().begin()->get();
    auto *c2 = new WIR_BitValues();
    r2->insertContainer( c2 );
    for ( auto &inEdge : cont2.getInValues() ) {
      c2->insertInValues(
        *(inEdge.rp), WIR_UpDownValue { inEdge.downVal },
        WIR_UpDownValue { inEdge.upVal } );

      auto &srcContainer =
        inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
      srcContainer.insertOutValues(
        *r2, WIR_UpDownValue { inEdge.downVal },
        WIR_UpDownValue { inEdge.upVal } );
      srcContainer.eraseOutValues( pExtr2 );
    }
  }

  // Update location bits.
  updateLocations( i.getBasicBlock().getFunction() );

  // Erase original operations that became superfluous now.
  if ( mPatchLd )
    i.eraseOperation( i.begin() );

  for ( WIR_Operation &o1 : mFoundEXTRs )
    o1.getInstruction().eraseOperation( o1.getInstruction().begin() );
};

}       // namespace WIR
