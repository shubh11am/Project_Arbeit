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
  @file peep_ldst.cc
  @brief This file implements a peephole optimizer for <TT>LD</TT> operations
         followed by a redundant <TT>ST</TT>.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <map>
#include <set>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>
#include <arch/tricore/analyses/bit/tcbitdfa.h>

// Include local headers
#include "peep_ldst.h"


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
TC_Peep_LDST::TC_Peep_LDST( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_BitOpt { f },
  WIR_Peephole { f }
{
  DSTART( "TC_Peep_LDST::TC_Peep_LDST(WIR_Function&)" );

  addPeepholeSize( 1 );
};


/*
  Destructor.
*/
TC_Peep_LDST::~TC_Peep_LDST( void )
{
  DSTART( "virtual TC_Peep_LDST::~TC_Peep_LDST()" );
};


/*
  optimize performs the peephole optimization.
*/
void TC_Peep_LDST::optimize( void )
{
  DSTART( "void TC_Peep_LDST::optimize()" );

  WIR_Peephole::optimize();
};


//
// Protected class methods
//

/*
  runOptimization performs peephole optimization in the given function.
*/
void TC_Peep_LDST::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void TC_Peep_LDST::runOptimization(WIR_Function&)" );

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
  operation pattern LD, ST.
*/
bool TC_Peep_LDST::matchPeephole( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC_Peep_LDST::matchPeephole(const peephole&)" );

  static bool initDone = false;
  static map<WIR_BaseProcessor::OpCode,
             WIR_BaseProcessor::OpCode> opCodeMatches;
  static map<WIR_BaseProcessor::OperationFormat,
             set<WIR_BaseProcessor::OperationFormat>> opFormatMatches;

  if ( !initDone ) {
    opCodeMatches = {
      { TC13::OpCode::LD_A, TC13::OpCode::ST_A },
      { TC13::OpCode::LD_B, TC13::OpCode::ST_B },
      { TC13::OpCode::LD_BU, TC13::OpCode::ST_B },
      { TC13::OpCode::LD_D, TC13::OpCode::ST_D },
      { TC13::OpCode::LD_DA, TC13::OpCode::ST_DA },
      { TC13::OpCode::LD_H, TC13::OpCode::ST_H },
      { TC13::OpCode::LD_HU, TC13::OpCode::ST_H },
      { TC13::OpCode::LD_Q, TC13::OpCode::ST_Q },
      { TC13::OpCode::LD_W, TC13::OpCode::ST_W } };

  opFormatMatches = {
    // LD.A + ST.A
    { TC13::OperationFormat::AC18ABSA, { TC13::OperationFormat::C18AABSA } },
    { TC13::OperationFormat::ALABSA, { TC13::OperationFormat::LAABSA } },
    { TC13::OperationFormat::AAC10BOA,
      { TC13::OperationFormat::AC10ABOA, TC13::OperationFormat::SSPC10I_1,
        TC13::OperationFormat::SAC4I_1, TC13::OperationFormat::SAA_4,
        TC13::OperationFormat::SIC4A } },
    { TC13::OperationFormat::AAC16BOA,
      { TC13::OperationFormat::AC10ABOA, TC13::OperationFormat::SSPC10I_1,
        TC13::OperationFormat::SAC4I_1, TC13::OperationFormat::SAA_4,
        TC13::OperationFormat::SIC4A } },
    { TC13::OperationFormat::SISPC10_1,
      { TC13::OperationFormat::SSPC10I_1, TC13::OperationFormat::AC10ABOA } },
    { TC13::OperationFormat::SAA_2,
      { TC13::OperationFormat::SAA_4, TC13::OperationFormat::AC10ABOA } },
    { TC13::OperationFormat::SAIC4,
      { TC13::OperationFormat::SIC4A, TC13::OperationFormat::AC10ABOA } },
    { TC13::OperationFormat::SIAC4_1,
      { TC13::OperationFormat::SAC4I_1, TC13::OperationFormat::AC10ABOA } },

    // LD.B + ST.B; LD.BU + ST.B; LD.H + ST.H; LD.HU + ST.H; LD.Q + ST.Q;
    // LD.W + ST.W
    { TC13::OperationFormat::DC18ABSA, { TC13::OperationFormat::C18DABSA_1 } },
    { TC13::OperationFormat::DLABSA, { TC13::OperationFormat::LDABSA_1 } },
    { TC13::OperationFormat::DAC10BOA,
      { TC13::OperationFormat::AC10DBOA_1, TC13::OperationFormat::AC16DBOA,
        TC13::OperationFormat::SAC4I_2, TC13::OperationFormat::SAD_2,
        TC13::OperationFormat::SIC4D, TC13::OperationFormat::SSPC10I_2 } },

    // LD.BU + ST.B; LD.H + ST.H; LD.W + ST.W
    { TC13::OperationFormat::SDA_2,
      { TC13::OperationFormat::SAD_2, TC13::OperationFormat::AC10DBOA_1,
        TC13::OperationFormat::AC16DBOA } },
    { TC13::OperationFormat::SDIC4_1,
      { TC13::OperationFormat::SIC4D, TC13::OperationFormat::AC10DBOA_1,
        TC13::OperationFormat::AC16DBOA } },
    { TC13::OperationFormat::SIAC4_2,
      { TC13::OperationFormat::SAC4I_2, TC13::OperationFormat::AC10DBOA_1,
        TC13::OperationFormat::AC16DBOA } },

    // LD.D + ST.D
    { TC13::OperationFormat::EC18ABSA, { TC13::OperationFormat::C18EABSA } },
    { TC13::OperationFormat::ELABSA, { TC13::OperationFormat::LEABSA } },
    { TC13::OperationFormat::EAC10BOA, { TC13::OperationFormat::AC10EBOA } },

    // LD.DA + ST.DA
    { TC13::OperationFormat::PC18ABSA, { TC13::OperationFormat::C18PABSA } },
    { TC13::OperationFormat::PLABSA, { TC13::OperationFormat::LPABSA } },
    { TC13::OperationFormat::PAC10BOA, { TC13::OperationFormat::AC10PBOA } },

    // LD.W + ST.W
    { TC13::OperationFormat::DAC16BOA,
      { TC13::OperationFormat::AC16DBOA, TC13::OperationFormat::AC10DBOA_1,
        TC13::OperationFormat::SSPC10I_2, TC13::OperationFormat::SAC4I_2,
        TC13::OperationFormat::SAD_2, TC13::OperationFormat::SIC4D } },
    { TC13::OperationFormat::DALC16BOA, { TC13::OperationFormat::ALC16DBOA } },
    { TC13::OperationFormat::SISPC10_2,
      { TC13::OperationFormat::SSPC10I_2, TC13::OperationFormat::AC10DBOA_1,
        TC13::OperationFormat::AC16DBOA } } };

    initDone = true;
  }

  mFoundSTs.clear();
  WIR_Operation &ld = p[ 0 ]->get().begin()->get();

  // Check whether the peephole is a LD with appropriate format.
  auto itOpCode = opCodeMatches.find( ld.getOpCode() );
  auto itOpFormat = opFormatMatches.find( ld.getOperationFormat() );
  if ( ( itOpCode == opCodeMatches.end() ) ||
       ( itOpFormat == opFormatMatches.end() ) )
    return( false );

  // Check all operations that are data flow-dependent on the LD's result.
  auto &pLd1 =
    dynamic_cast<WIR_RegisterParameter &>( ld.getExplicitParameter( 1 ) );
  auto &cLd1 = pLd1.getContainers<WIR_BitValues>().begin()->get();

  // Iterate all out-edges of the LD.
  for ( auto &outEdge : cLd1.getOutValues() ) {
    auto &st = outEdge.rp->getOperation();

    if ( ( st.getOpCode() != itOpCode->second ) ||
         ( !(itOpFormat->second.count( st.getOperationFormat() )) ) ||
         st.getDontOptimize() )
      continue;

    // We found a matching ST. Ensure that the ST is data flow-dependent only on
    // the LD.
    auto &cSt = outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
    if ( cSt.getInValues().size() != 1 )
      continue;

    // Ensure that both the LD and the ST refer to the same address.
    if ( !sameAddress( ld, st ) )
      continue;

    // Finally ensure that unsafe ST operations that have side effects or that
    // access volatile symbols are not removed. Stack accesses are always
    // considered safe, since volatile symbols should never make it onto the
    // stack.
    if ( !isStackAccess( st ) && accessesVolatileData( st ) )
      continue;

    mFoundSTs.insert( st );
  }

  return( !mFoundSTs.empty() );
};


/*
  transformPeephole optimizes the operation pattern LD, ST in the specified
  peephole.
*/
void TC_Peep_LDST::transformPeephole( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual void TC_Peep_LDST::transformPeephole(const peephole&)" );

  (void) p;

  for ( WIR_Operation &st : mFoundSTs ) {
    // Remove the ST from all its related bitValue containers.
    for ( auto it = st.begin(); it != st.end(); ++it )
      if ( it->get().containsContainers(
             WIR_BitValues::getContainerTypeID() ) ) {
        auto &c = it->get().getContainers<WIR_BitValues>().begin()->get();

        for ( auto &inEdge : c.getInValues() ) {
          auto &srcContainer =
            inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
          srcContainer.eraseOutValues( it->get() );
        }

        for ( auto &outEdge : c.getOutValues() ) {
          auto &tgtContainer =
            outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
          tgtContainer.eraseInValues( it->get() );
        }
      }

    // Finally erase the ST.
    WIR_Instruction &i = st.getInstruction();
    WIR_BasicBlock &b = i.getBasicBlock();
    b.eraseInstruction( b.findInstruction( i ) );
  }
};


//
// Private class methods
//

/*
  sameAddress checks whether the specified LD and ST operations refer to the
  same address.
*/
bool TC_Peep_LDST::sameAddress( const WIR_Operation &ld,
                                const WIR_Operation &st ) const
{
  DSTART(
    "bool TC_Peep_LDST::sameAddress(const WIR_Operation&, const WIR_Operation&) const" );

  static bool initDone = false;
  static set<WIR_BaseProcessor::OperationFormat> absAddressing;

  if ( !initDone ) {
    absAddressing = {
      TC13::OperationFormat::AC18ABSA, TC13::OperationFormat::ALABSA,
      TC13::OperationFormat::DC18ABSA, TC13::OperationFormat::DLABSA,
      TC13::OperationFormat::EC18ABSA, TC13::OperationFormat::ELABSA,
      TC13::OperationFormat::PC18ABSA, TC13::OperationFormat::PLABSA };

    initDone = true;
  }

  // For operations using absolute addressing, we simply need to check the
  // absolute address value or the label.
  if ( absAddressing.count( ld.getOperationFormat() ) ) {
    auto &ldAbsAdr = ld.getExplicitParameter( 2 );
    auto &stAbsAdr = st.getExplicitParameter( 1 );

    if ( ldAbsAdr.getType() == WIR_ParameterType::imm )
      return(
        dynamic_cast<WIR_BaseImmediateParameter &>(
          ldAbsAdr ).getUnsignedValue() ==
          dynamic_cast<WIR_BaseImmediateParameter &>(
            stAbsAdr ).getUnsignedValue() );

    else
      return(
        sameLabel(
          dynamic_cast<WIR_LabelParameter &>( ldAbsAdr ),
          dynamic_cast<WIR_LabelParameter &>( stAbsAdr ) ) );
  }

  // For the very special constellation LD.W (DALC16BOA) + ST.W (ALC16DBOA),
  // let's check the involved labels first.
  if ( ( ld.getOperationFormat() == TC13::OperationFormat::DALC16BOA ) &&
       !sameLabel(
         dynamic_cast<WIR_LabelParameter &>( ld.getExplicitParameter( 3 ) ),
         dynamic_cast<WIR_LabelParameter &>( st.getExplicitParameter( 2 ) ) ) )
    return( false );

  // For all other constellations, base+offset addressing is used. Thus, we have
  // to determine both the LD's and the ST's used base registers and offsets.
  auto &ldBase =
    dynamic_cast<WIR_RegisterParameter &>( ld.getExplicitParameter( 2 ) );
  long long ldOffset =
    ( ( ld.getOperationFormat() == TC13::OperationFormat::SAA_2 ) ||
      ( ld.getOperationFormat() == TC13::OperationFormat::SDA_2 ) ) ? 0 :
      dynamic_cast<WIR_BaseImmediateParameter &>(
        ld.getExplicitParameter(
          ld.getExplicitParameters().size() ) ).getSignedValue();

  auto &stBase =
    dynamic_cast<WIR_RegisterParameter &>( st.getExplicitParameter( 1 ) );
  long long stOffset =
    ( ( st.getOperationFormat() == TC13::OperationFormat::SAA_4 ) ||
      ( st.getOperationFormat() == TC13::OperationFormat::SAD_2 ) ) ? 0 :
      dynamic_cast<WIR_BaseImmediateParameter &>(
        st.getExplicitParameter(
          st.getExplicitParameters().size() - 1 ) ).getSignedValue();

  // If the LD and the ST refer to different base registers or offsets, their
  // addresses are not the same.
  if ( ( ldBase.getRegister() != stBase.getRegister() ) ||
       ( ldOffset != stOffset ) )
    return( false );

  // Finally, we need to ensure that the involved base address register is not
  // modified between the LD and the ST. For this purpose we check all incoming
  // edges of both ldBase and stBase. If these edge sets are produced by the
  // very same operations, the address register must hold the same value.
  set<WIR_id_t> ldBaseProducers;
  auto &cLdBase = ldBase.getContainers<WIR_BitValues>().begin()->get();
  for ( auto &inEdge : cLdBase.getInValues() )
    ldBaseProducers.insert( inEdge.rp->getID() );

  set<WIR_id_t> stBaseProducers;
  auto &cStBase = stBase.getContainers<WIR_BitValues>().begin()->get();
  for ( auto &inEdge : cStBase.getInValues() ) {
    // A definition of the address register reaches the ST but not the LD.
    if ( !ldBaseProducers.count( inEdge.rp->getID() ) )
      return( false );

    stBaseProducers.insert( inEdge.rp->getID() );
  }

  // A definition of the address register reaches the LD but not the ST.
  if ( ldBaseProducers.size() != stBaseProducers.size() )
    return( false );

  return( true );
};


/*
  sameLabel checks whether two label parameters are equal.
*/
bool TC_Peep_LDST::sameLabel( const WIR_LabelParameter &lab1,
                              const WIR_LabelParameter &lab2 ) const
{
  DSTART(
    "bool TC_Peep_LDST::sameLabel(const WIR_LabelParameter&, const WIR_LabelParameter&) const" );

  if ( lab1.getLabelType() != lab2.getLabelType() )
    return( false );

  switch ( lab1.getLabelType() ) {
    case WIR_SymbolType::block:
      return( lab1.getBasicBlock() == lab2.getBasicBlock() );

    case WIR_SymbolType::data:
      return( lab1.getData() == lab2.getData() );

    case WIR_SymbolType::function:
      return( lab1.getFunction() == lab2.getFunction() );
  }

  return( false );
};


/*
  isStackAccesses whether a LD or ST operation uses the stack pointer and thus
  accesses the stack.
*/
bool TC_Peep_LDST::isStackAccess( const WIR_Operation &o ) const
{
  DSTART( "bool TC_Peep_LDST::isStackAccess(const WIR_Operation&) const" );

  for ( WIR_Parameter &p : o )
    if ( p.getType() == WIR_ParameterType::reg ) {
      auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );

      if ( ( rp.isUsed() || rp.isDefUsed() ) && TC13::isSP( rp.getRegister() ) )
        return( true );
    }

  return( false );
};


/*
  accessesVolatileSymbol determines whether a ST operation accesses a volatile
  data object.

  In order to determine which data objects are actually accessed, attached data
  access containers are inspected as well as information from bit-true data flow
  analysis is considered.
*/
bool TC_Peep_LDST::accessesVolatileData( const WIR_Operation &o ) const
{
  DSTART(
    "bool TC_Peep_LDST::accessesVolatileData(const WIR_Operation&) const" );

  // Check symbols from data access containers first.
  auto &f = o.getInstruction().getBasicBlock().getFunction();
  auto &sys = f.getCompilationUnit().getSystem();

  if ( o.containsContainers( WIR_DataAccess::getContainerTypeID() ) ) {
    auto &cont = o.getContainers<WIR_DataAccess>().begin()->get();

    for ( WIR_Data &d : cont.getData() )
      if ( sys.findSymbol( d ).isVolatile() )
        return( true );
  }

  // Check symbols from bit-level information next.
  auto &p1 = o.getExplicitParameter( 1 );
  if ( p1.getType() == WIR_ParameterType::reg ) {
    auto &rp1 = dynamic_cast<WIR_RegisterParameter &>( p1 );

    if ( ( rp1.getRegister().getType() == TC13::RegisterType::aReg ) &&
         ( rp1.isUsed() || rp1.isDefUsed() ) ) {
      auto &cont = rp1.getContainers<WIR_BitValues>().begin()->get();

      // Check all in-edges of a ST's base address register.
      for ( auto &inEdge : cont.getInValues() ) {
        auto &inVal = inEdge.upVal;
        bool commonSymbolFound = false;

        // All bits of an in-edge must be locations refering to the very same
        // symbol.
        for ( unsigned int i = 0; i < inVal.getBitWidth(); ++i ) {
          if ( inVal[ i ] != WIR_L4::bL ) {
            commonSymbolFound = false;
            break;
          }

          auto &l = inVal.getLocation( i );
          if ( !l.isSymbol() ) {
            commonSymbolFound = false;
            break;
          }

          if ( i == 0 ) {
            commonSymbolFound = true;
            continue;
          }

          if ( ( l.getSymbol() != inVal.getLocation( i - 1 ).getSymbol() ) ||
               ( l.getBitPosition() != i ) ) {
            commonSymbolFound = false;
            break;
          }
        }

        if ( commonSymbolFound &&
             inVal.getLocation( 0 ).getSymbol().isVolatile() )
          return( true );
      }
    }
  }

  return( false );
};

}       // namespace WIR
