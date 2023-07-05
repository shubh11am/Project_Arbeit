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
  @file tcasmcontext.cc
  @brief This file implements a container storing data that is internally used
         by the TriCore assembly code parser.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <sstream>
#include <stdexcept>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>
#include <libuseful/stringtools.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>

// Include local headers
#include "tcasmaddress.h"
#include "tcasmlabel.h"
#include "tcasmconstant.h"
#include "tcasmcontext.h"
#include "tcasmtemplateregister.h"
#include <arch/tricore/asmparser/tcasmyacc.hh>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Destructor.
*/
TC_AsmContext::~TC_AsmContext( void )
{
  DSTART( "TC_AsmContext::~TC_AsmContext()" );
};


/*
  getTemplateArgument returns a copy of the ith template assembly argument.

  The caller of getTemplateArgument is responsible for the proper deletion of
  the returned object.
*/
TC_AsmArgument *TC_AsmContext::getTemplateArgument( unsigned int i ) const
{
  DSTART(
    "TC_AsmArgument* TC_AsmContext::getTemplateArgument(unsigned int) const" );

  ufAssertT(
    i < mTemplateArguments.size(),
    "Template argument '%" << i << "' not found, it should be within '%0' " <<
    "and '%" << mTemplateArguments.size() - 1 << "'." );

  DOUT( "Retrieving template argument '%" << i << "'." << endl );

  return( mTemplateArguments.at( i )->clone() );
};


/*
  hasTemplateArguments returns whether template assembly arguments have to be
  considered while parsing.
*/
bool TC_AsmContext::hasTemplateArguments( void ) const
{
  DSTART( "bool TC_AsmContext::hasTemplateArguments() const" );

  return( !mTemplateArguments.empty() );
};


/*
  pushBackOperation appends a new operation to be generated.
*/
bool TC_AsmContext::pushBackOperation( const MnemonicData &m,
                                       const std::vector<TC_AsmArgument *> &a )
{
  DSTART(
    "bool TC_AsmContext::pushBackOperation(const MnemonicData&, const vector<TC_AsmArgument*>&)" );

  bool res = false;
  auto w = m.w;
  auto &o = *(m.m);

  auto blockIt = mBasicBlocks.rbegin();
  WIR_BasicBlock &b = blockIt->get();

  WIR_Operation::enforceParameterChecking();

  // This lambda is used to push back WIR operations at the current basic
  // block's tail.
  auto pbOp = [&]( WIR_Operation &&o ) {
    b.pushBackInstruction( WIR_Instruction( std::move( o ) ) );
    check16BitOperation( b.rbegin()->get().getOperations().front().get() );
    res = true;
  };

  // This lambda is used to generate WIR register parameters.
  auto regParam = [&]( unsigned int i, WIR_Usage u ) {
    if ( dynamic_cast<TC_AsmRegister *>( a[ i ] ) != nullptr )
      return(
        new WIR_RegisterParameter(
          dynamic_cast<TC_AsmRegister *>( a[ i ] )->getRegister(), u ) );
    else
      return(
        new WIR_RegisterParameter(
          dynamic_cast<TC_AsmTemplateRegister *>( a[ i ] )->getRegister(),
          u ) );
  };

  // This lambda is used to generate WIR extended address register parameters.
  auto pregParam = [&]( unsigned int i, WIR_Usage u ) {
    return(
      new WIR_RegisterParameter(
        dynamic_cast<TC_AsmRegister *>( a[ i ] )->getRegister().getParent(),
        u ) );
  };

  // This lambda is used to generate WIR index address register parameters.
  auto iaregParam = [&]( unsigned int i, WIR_Usage u ) {
    return(
      new WIR_RegisterParameter(
        dynamic_cast<TC_AsmAddress *>( a[ i ] )->getRegister(), u ) );
  };

  // This lambda is used to generate WIR index extended address register
  // parameters.
  auto ipregParam = [&]( unsigned int i, WIR_Usage u ) {
    return(
      new WIR_RegisterParameter(
        dynamic_cast<TC_AsmAddress *>( a[ i ] )->getRegister().getParent(),
        u ) );
  };

  // This lambda is used to obtain a signed constant argument value.
  auto sConst = [&]( unsigned int i ) {
    return(
      dynamic_cast<TC_AsmConstant *>( a[ i ] )->getSignedValue() );
  };

  // This lambda is used to obtain an unsigned constant argument value.
  auto uConst = [&]( unsigned int i ) {
    return(
      dynamic_cast<TC_AsmConstant *>( a[ i ] )->getUnsignedValue() );
  };

  // This lambda is used to obtain an index address parameter's offset.
  auto iaregOffset = [&]( unsigned int i ) {
    return(
      dynamic_cast<TC_AsmAddress *>( a[ i ] )->getOffset() );
  };

  // This lambda is used to generate WIR addressing mode parameters.
  auto amodeParam = [&]( unsigned int i ) {
    return(
      new WIR_AddressingModeParameter(
        a[ i ]->getType() == TC_AsmArgument::Type::AMODE_PREINC ?
          TC13::AddressingMode::pre : TC13::AddressingMode::post ) );
  };

  // This lambda is used to obtain a label argument.
  auto labelParam = [&]( unsigned int i ) {
    string label =
      dynamic_cast<TC_AsmAddress *>( a[ i ] ) ?
        dynamic_cast<TC_AsmAddress *>( a[ i ] )->getName() :
        dynamic_cast<TC_AsmLabel *>( a[ i ] )->getName();

    if ( mBlockOfLabel.count( label ) )
      return( new WIR_LabelParameter( mBlockOfLabel.at( label ) ) );

    auto &sys = mFunction.getCompilationUnit().getSystem();
    if ( !sys.containsSymbol( label ) )
      return( (WIR_LabelParameter *) nullptr );

    auto &sym = sys.findSymbol( label );
    if ( sym.getType() == WIR_SymbolType::block )
      return( new WIR_LabelParameter( sym.getBasicBlock() ) );
    else
    if ( sym.getType() == WIR_SymbolType::data )
      return( new WIR_LabelParameter( sym.getData() ) );
    else
      return( new WIR_LabelParameter( sym.getFunction() ) );
  };

  // This lambda is used to obtain a function label argument.
  auto fctLabelParam = [&]( unsigned int i ) {
    string label = dynamic_cast<TC_AsmLabel *>( a[ i ] )->getName();
    auto &sys = mFunction.getCompilationUnit().getSystem();

    for ( WIR_Symbol &sym : sys.getSymbols() )
      if ( ( sym.getType() == WIR_SymbolType::function ) &&
           ( sym.getName() == label ) )
        return( new WIR_LabelParameter( sym.getFunction() ) );

    if ( sys.containsSymbol( label ) ) {
      auto &sym = sys.findSymbol( label );
      return( new WIR_LabelParameter( sym.getFunction() ) );
    } else {
      // Non-existing WIR function. Create an external WIR function.
      WIR_Function &f =
        sys.begin()->get().pushFrontFunction( WIR_Function( label ) );
      auto &sym = sys.findSymbol( f );
      sym.setExtern();
      f.setDontOptimize();
      return( new WIR_LabelParameter( f ) );
    }
  };

  // This lambda is used to add a string parameter to the very last operation.
  // This string parameter contains a label that could not yet be resolved to a
  // correct symbol now.
  auto labelStringParam = [&]( unsigned int i ) {
    string label =
      dynamic_cast<TC_AsmAddress *>( a[ i ] ) ?
        dynamic_cast<TC_AsmAddress *>( a[ i ] )->getName() :
        dynamic_cast<TC_AsmLabel *>( a[ i ] )->getName();
    WIR_StringParameter sParam( label );
    sParam.setImplicit();

    WIR_Operation &op = b.rbegin()->get().begin()->get();
    op.pushBackParameter( sParam );
    mOperationsToBeResolved.insert( op );
  };

  switch ( a.size() ) {

    case 0: {
      if ( ( o == TC13::OpCode::DEBUG ) || ( o == TC13::OpCode::NOP ) ) {
        if ( w == TC_AsmOperationWidth::CODE16 )
          pbOp( { o, TC13::OperationFormat::S } );
        else
          pbOp( { o, TC13::OperationFormat::SYS } );
      } else

      if ( ( o == TC13::OpCode::RET ) || ( o == TC13::OpCode::RFE ) ) {
        if ( w == TC_AsmOperationWidth::CODE16 )
          pbOp(
            { o, TC13::OperationFormat::SPSW,
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::def ) } );
        else
          pbOp(
            { o, TC13::OperationFormat::PSW,
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::def ) } );

        if ( o == TC13::OpCode::RET ) {
          auto &op = b.rbegin()->get().begin()->get();

          // Add implicit parameters for a proper def-use analysis.
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 2 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 2 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getARegP( 11 ), WIR_Usage::use, true ) );
        }
      } else

      if ( ( o == TC13::OpCode::DISABLE ) || ( o == TC13::OpCode::DSYNC ) ||
           ( o == TC13::OpCode::ENABLE ) || ( o == TC13::OpCode::ISYNC ) ||
           ( o == TC13::OpCode::RSLCX ) || ( o == TC13::OpCode::RSTV ) ||
           ( o == TC13::OpCode::SVLCX ) || ( o == TC13::OpCode::TLBFLUSH_A ) ||
           ( o == TC13::OpCode::TLBFLUSH_B ) ||
           ( o == TC13::OpCode::TRAPSV ) || ( o == TC13::OpCode::TRAPV ) )
        pbOp( { o, TC13::OperationFormat::SYS } );
      else

      if ( o == TC13::OpCode::RFM )
        pbOp(
          { o, TC13::OperationFormat::PSW,
            new WIR_RegisterParameter( getPSW_C(), WIR_Usage::def ) } );

      break;
    }

    case 1: {
      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::CONST ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::CONST8 ) &&
             ( o == TC13::OpCode::BISR ) )
          pbOp(
            { o, TC13::OperationFormat::SC8,
              new TC_Const8_Unsigned( uConst( 0 ) ) } );
        else

        if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) &&
             ( o == TC13::OpCode::BISR ) )
          pbOp(
            { o, TC13::OperationFormat::C9,
              new TC_Const9_Unsigned( uConst( 0 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) &&
             ( o == TC13::OpCode::SYSCALL ) )
          pbOp(
            { o, TC13::OperationFormat::C9PSW,
              new TC_Const9_Unsigned( uConst( 0 ) ),
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_ABS ) &&
             ( ( o == TC13::OpCode::LDLCX ) || ( o == TC13::OpCode::LDUCX ) ||
               ( o == TC13::OpCode::STLCX ) ) ) {
          pbOp(
            { o, TC13::OperationFormat::C18ABSA,
              new TC_Const18_Unsigned( uConst( 0 ) ) } );

          auto &op = b.rbegin()->get().begin()->get();

          // Add implicit parameters for a proper def-use analysis.
          if ( o == TC13::OpCode::LDLCX ) {
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 2 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 4 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 6 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 0 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 2 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 4 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 6 ), WIR_Usage::def, true ) );
          }

          if ( o == TC13::OpCode::STLCX ) {
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 2 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 4 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 6 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getARegP( 11 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 0 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 2 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 4 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 6 ), WIR_Usage::use, true ) );
          }

          if ( o == TC13::OpCode::LDUCX ) {
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 10 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 12 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 14 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 8 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 10 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 12 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 14 ), WIR_Usage::def, true ) );
          }
        } else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_ABS ) &&
             ( o == TC13::OpCode::STUCX ) ) {
          pbOp(
            { o, TC13::OperationFormat::C18ABSAPSW,
              new TC_Const18_Unsigned( uConst( 0 ) ),
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::use ) } );

          auto &op = b.rbegin()->get().begin()->get();

          // Add implicit parameters for a proper def-use analysis.
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 10 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 12 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 14 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 8 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 10 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 12 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 14 ), WIR_Usage::use, true ) );
        }
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) ) {
        if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE ) &&
             ( ( o == TC13::OpCode::CACHEA_I ) ||
               ( o == TC13::OpCode::CACHEA_W ) ||
               ( o == TC13::OpCode::CACHEA_WI ) ||
               ( o == TC13::OpCode::LDLCX ) ||
               ( o == TC13::OpCode::LDUCX ) ||
               ( o == TC13::OpCode::STLCX ) ||
               // TriCore TC1.3.1
               ( o == TC131::OpCode::CACHEI_W ) ||
               ( o == TC131::OpCode::CACHEI_WI ) ) ) {
          pbOp(
            { o, TC13::OperationFormat::AC10BOA,
              iaregParam( 0, WIR_Usage::use ),
              new TC_Const10_Signed( iaregOffset( 0 ) ) } );

          auto &op = b.rbegin()->get().begin()->get();

          // Add implicit parameters for a proper def-use analysis.
          if ( o == TC13::OpCode::LDLCX ) {
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 2 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 4 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 6 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 0 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 2 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 4 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 6 ), WIR_Usage::def, true ) );
          }

          if ( o == TC13::OpCode::STLCX ) {
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 2 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 4 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 6 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getARegP( 11 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 0 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 2 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 4 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 6 ), WIR_Usage::use, true ) );
          }

          if ( o == TC13::OpCode::LDUCX ) {
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 10 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 12 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 14 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 8 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 10 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 12 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 14 ), WIR_Usage::def, true ) );
          }
        } else

        if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE ) &&
             ( o == TC13::OpCode::STUCX ) ) {
          pbOp(
            { o, TC13::OperationFormat::AC10BOAPSW,
              iaregParam( 0, WIR_Usage::use ),
              new TC_Const10_Signed( iaregOffset( 0 ) ),
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::use ) } );

          auto &op = b.rbegin()->get().begin()->get();

          // Add implicit parameters for a proper def-use analysis.
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 10 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 12 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 14 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 8 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 10 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 12 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 14 ), WIR_Usage::use, true ) );
        } else

        if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BREV ) &&
             ( ( o == TC13::OpCode::CACHEA_I ) ||
               ( o == TC13::OpCode::CACHEA_W ) ||
               ( o == TC13::OpCode::CACHEA_WI ) ) )
          pbOp(
            { o, TC13::OperationFormat::PBRA,
              ipregParam( 0, WIR_Usage::defuse ) } );
        else

        if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_CIRC ) &&
             ( ( o == TC13::OpCode::CACHEA_I ) ||
               ( o == TC13::OpCode::CACHEA_W ) ||
               ( o == TC13::OpCode::CACHEA_WI ) ) )
          pbOp(
            { o, TC13::OperationFormat::PC10CA,
              ipregParam( 0, WIR_Usage::defuse ),
              new TC_Const10_Signed( iaregOffset( 0 ) ) } );
        else

        if ( ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINC ) ||
               a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_PREINC ) ) &&
             ( ( o == TC13::OpCode::CACHEA_I ) ||
               ( o == TC13::OpCode::CACHEA_W ) ||
               ( o == TC13::OpCode::CACHEA_WI ) ||
               // TriCore TC1.3.1
               ( o == TC131::OpCode::CACHEI_W ) ||
               ( o == TC131::OpCode::CACHEI_WI ) ) )
          pbOp(
            { o, TC13::OperationFormat::AC10PIA,
              amodeParam( 0 ), iaregParam( 0, WIR_Usage::defuse ),
              new TC_Const10_Signed( iaregOffset( 0 ) ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DISP ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::CALL ) || ( o == TC13::OpCode::CALLA ) ||
               ( o == TC13::OpCode::JL ) || ( o == TC13::OpCode::JLA ) ) ) {
          pbOp( { o, TC13::OperationFormat::L, fctLabelParam( 0 ) } );

          auto &op = b.rbegin()->get().begin()->get();

          // Add implicit parameters for a proper def-use analysis.
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 4 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 6 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 4 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 6 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 2 ), WIR_Usage::def, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 2 ), WIR_Usage::def, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getARegP( 11 ), WIR_Usage::def, true ) );

          if ( ( o == TC13::OpCode::CALL ) || ( o == TC13::OpCode::CALLA ) )
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getARegP( 10 ), WIR_Usage::def, true ) );
        }  else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::J ) || ( o == TC13::OpCode::JA ) ||
               ( o == TC13::OpCode::LOOPU ) ) ) {
          auto *lParam = labelParam( 0 );
          if ( lParam != nullptr )
            pbOp( { o, TC13::OperationFormat::L, lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::L, new WIR_LabelParameter( b ) } );
            labelStringParam( 0 );
          }
        } else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             ( o == TC13::OpCode::CALL ) ) {
          pbOp( { o, TC13::OperationFormat::SL, fctLabelParam( 0 ) } );

          auto &op = b.rbegin()->get().begin()->get();

          // Add implicit parameters for a proper def-use analysis.
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 4 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 6 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 4 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 6 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 2 ), WIR_Usage::def, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 2 ), WIR_Usage::def, true ) );
        } else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             ( o == TC13::OpCode::J ) ) {
          auto *lParam = labelParam( 0 );
          if ( lParam != nullptr )
            pbOp( { o, TC13::OperationFormat::SL, lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::SL, new WIR_LabelParameter( b ) } );
            labelStringParam( 0 );
          }
        } else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::LDLCX ) || ( o == TC13::OpCode::LDUCX ) ||
               ( o == TC13::OpCode::STLCX ) ) ) {
          pbOp( { o, TC13::OperationFormat::LABSA, labelParam( 0 ) } );

          auto &op = b.rbegin()->get().begin()->get();

          // Add implicit parameters for a proper def-use analysis.
          if ( o == TC13::OpCode::LDLCX ) {
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 2 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 4 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 6 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 0 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 2 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 4 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 6 ), WIR_Usage::def, true ) );
          }

          if ( o == TC13::OpCode::STLCX ) {
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 2 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 4 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 6 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getARegP( 11 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 0 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 2 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 4 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 6 ), WIR_Usage::use, true ) );
          }

          if ( o == TC13::OpCode::LDUCX ) {
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 10 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 12 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 14 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 8 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 10 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 12 ), WIR_Usage::def, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 14 ), WIR_Usage::def, true ) );
          }
        } else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( o == TC13::OpCode::STUCX ) ) {
          pbOp(
            { o, TC13::OperationFormat::LABSAPSW, labelParam( 0 ),
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::use ) } );

          auto &op = b.rbegin()->get().begin()->get();

          // Add implicit parameters for a proper def-use analysis.
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 10 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 12 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 14 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 8 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 10 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 12 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 14 ), WIR_Usage::use, true ) );
        }
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AREG ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::CALLI ) || ( o == TC13::OpCode::JLI ) ) ) {
          pbOp(
            { o, TC13::OperationFormat::A, regParam( 0, WIR_Usage::use ) } );

          auto &op = b.rbegin()->get().begin()->get();

          // Add implicit parameters for a proper def-use analysis.
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 4 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 6 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 4 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 6 ), WIR_Usage::use, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getPRegP( 2 ), WIR_Usage::def, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getERegP( 2 ), WIR_Usage::def, true ) );
          op.pushBackParameter(
            new WIR_RegisterParameter(
              getARegP( 11 ), WIR_Usage::def, true ) );

          if ( o == TC13::OpCode::CALLI )
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getARegP( 10 ), WIR_Usage::def, true ) );
        } else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( o == TC13::OpCode::JI ) ) {
          pbOp(
            { o, TC13::OperationFormat::SA, regParam( 0, WIR_Usage::use ) } );

          auto &op = b.rbegin()->get().begin()->get();
          auto &rp = dynamic_cast<WIR_RegisterParameter &>( op.begin()->get() );
          auto &reg = rp.getRegister();
          bool isReturn = false;

          if ( reg == mProcessor.A11() )
            isReturn = true;
          if ( reg.isVirtual() ) {
            auto &vReg = dynamic_cast<WIR_VirtualRegister &>( reg );
            if ( vReg.isPrecolored() &&
                 ( vReg.getPrecolor() == mProcessor.A11() ) )
              isReturn = true;
          }

          if ( isReturn ) {
            // Add implicit parameters for a proper def-use analysis.
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 2 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 2 ), WIR_Usage::use, true ) );
          }
        } else

        if ( ( w == TC_AsmOperationWidth::CODE32 ) &&
             ( o == TC13::OpCode::JI ) ) {
          pbOp(
            { o, TC13::OperationFormat::A, regParam( 0, WIR_Usage::use ) } );

          auto &op = b.rbegin()->get().begin()->get();
          auto &rp = dynamic_cast<WIR_RegisterParameter &>( op.begin()->get() );
          auto &reg = rp.getRegister();
          bool isReturn = false;

          if ( reg == mProcessor.A11() )
            isReturn = true;
          if ( reg.isVirtual() ) {
            auto &vReg = dynamic_cast<WIR_VirtualRegister &>( reg );
            if ( vReg.isPrecolored() &&
                 ( vReg.getPrecolor() == mProcessor.A11() ) )
              isReturn = true;
          }

          if ( isReturn ) {
            // Add implicit parameters for a proper def-use analysis.
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getPRegP( 2 ), WIR_Usage::use, true ) );
            op.pushBackParameter(
              new WIR_RegisterParameter(
                getERegP( 2 ), WIR_Usage::use, true ) );
          }
        }
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( ( o == TC13::OpCode::NOT ) || ( o == TC13::OpCode::RSUB ) ||
               ( o == TC13::OpCode::SAT_B ) || ( o == TC13::OpCode::SAT_BU ) ||
               ( o == TC13::OpCode::SAT_H ) || ( o == TC13::OpCode::SAT_HU ) ) )
          pbOp(
            { o, TC13::OperationFormat::SD,
              regParam( 0, WIR_Usage::defuse ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::TLBDEMAP ) ||
               ( o == TC13::OpCode::TLBPROBE_A ) ||
               ( o == TC13::OpCode::TLBPROBE_I ) ||
               ( o == TC13::OpCode::UPDFL ) ) )
          pbOp(
            { o, TC13::OperationFormat::D, regParam( 0, WIR_Usage::use ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::TLBMAP ) )
        pbOp(
          { o, TC13::OperationFormat::E, regParam( 0, WIR_Usage::use ) } );

      break;
    }

    case 2: {
      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::ABS ) || ( o == TC13::OpCode::ABS_B ) ||
               ( o == TC13::OpCode::ABS_H ) || ( o == TC13::OpCode::ABSS ) ||
               ( o == TC13::OpCode::ABSS_H ) || ( o == TC13::OpCode::CLO ) ||
               ( o == TC13::OpCode::CLO_H ) || ( o == TC13::OpCode::CLS ) ||
               ( o == TC13::OpCode::CLS_H ) || ( o == TC13::OpCode::CLZ ) ||
               ( o == TC13::OpCode::CLZ_H ) || ( o == TC13::OpCode::FTOI ) ||
               // cppcheck-suppress duplicateExpression
               ( o == TC131::OpCode::FTOIZ ) || ( o == TC13::OpCode::FTOU ) ||
               // cppcheck-suppress duplicateExpression
               ( o == TC131::OpCode::FTOUZ ) || ( o == TC13::OpCode::ITOF ) ||
               ( o == TC13::OpCode::PARITY ) ||
               ( o == TC13::OpCode::QSEED_F ) || ( o == TC13::OpCode::SAT_B ) ||
               ( o == TC13::OpCode::SAT_BU ) || ( o == TC13::OpCode::SAT_H ) ||
               ( o == TC13::OpCode::SAT_HU ) || ( o == TC13::OpCode::UTOF ) ||
               // TriCore TC1.3.1
               ( o == TC131::OpCode::FTOIZ ) ||
               ( o == TC131::OpCode::FTOUZ ) ) )
          pbOp(
            { o, TC13::OperationFormat::DD,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( ( o == TC13::OpCode::ADD ) || ( o == TC13::OpCode::ADDS ) ||
               ( o == TC13::OpCode::AND ) || ( o == TC13::OpCode::MUL ) ||
               ( o == TC13::OpCode::OR ) || ( o == TC13::OpCode::SUB ) ||
               ( o == TC13::OpCode::SUBS ) || ( o == TC13::OpCode::XOR ) ) )
          pbOp(
            { o, TC13::OperationFormat::SDD_2,
              regParam( 0, WIR_Usage::defuse ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( o == TC13::OpCode::MOV ) )
          pbOp(
            { TC13::OpCode::MOV_RR, TC13::OperationFormat::SDD_1,
              regParam( 0, WIR_Usage::def ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w == TC_AsmOperationWidth::CODE32 ) &&
             ( o == TC13::OpCode::MOV ) )
          pbOp(
            { TC13::OpCode::MOV_RR, TC13::OperationFormat::DD,
              regParam( 0, WIR_Usage::def ),
              regParam( 1, WIR_Usage::use ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
             ( ( o == TC13::OpCode::ADD ) || ( o == TC13::OpCode::SH ) ) )
          pbOp(
            { o, TC13::OperationFormat::SDC4_2,
              regParam( 0, WIR_Usage::defuse ),
              new TC_Const4_Signed( sConst( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
             ( o == TC13::OpCode::SHA ) )
          pbOp(
            { o, TC13::OperationFormat::SDC4PSW,
              regParam( 0, WIR_Usage::defuse ),
              new TC_Const4_Signed( sConst( 1 ) ),
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::def ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( a[ 0 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST8 ) &&
             ( ( o == TC13::OpCode::AND ) || ( o == TC13::OpCode::OR ) ) )
          pbOp(
            { o, TC13::OperationFormat::SIC8_2,
              regParam( 0, WIR_Usage::defuse ),
              new TC_Const8_Unsigned( uConst( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_ABS ) &&
             ( ( o == TC13::OpCode::LD_B ) || ( o == TC13::OpCode::LD_BU ) ||
               ( o == TC13::OpCode::LD_H ) || ( o == TC13::OpCode::LD_HU ) ||
               ( o == TC13::OpCode::LD_Q ) || ( o == TC13::OpCode::LD_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::DC18ABSA,
              regParam( 0, WIR_Usage::def ),
              new TC_Const18_Unsigned( uConst( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST16 ) &&
             ( ( o == TC13::OpCode::MOV_U ) || ( o == TC13::OpCode::MOVH ) ) )
          pbOp(
            { o, TC13::OperationFormat::DC16_2,
              regParam( 0, WIR_Usage::def ),
              new TC_Const16_Unsigned( uConst( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST16 ) &&
             ( o == TC13::OpCode::MFCR ) )
          pbOp(
            { o, TC13::OperationFormat::DC16PSW,
              regParam( 0, WIR_Usage::def ),
              new TC_Const16_Unsigned( uConst( 1 ) ),
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( a[ 0 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST8 ) &&
             ( o == TC13::OpCode::MOV ) )
          pbOp(
            { o, TC13::OperationFormat::SIC8_1,
              regParam( 0, WIR_Usage::def ),
              new TC_Const8_Unsigned( uConst( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
             ( o == TC13::OpCode::MOV ) )
          pbOp(
            { o, TC13::OperationFormat::SDC4_1,
              regParam( 0, WIR_Usage::def ),
              new TC_Const4_Signed( sConst( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST16 ) &&
             ( o == TC13::OpCode::MOV ) )
          pbOp(
            { o, TC13::OperationFormat::DC16_1,
              regParam( 0, WIR_Usage::def ),
              new TC_Const16_Signed( sConst( 1 ) ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
             ( o == TC13::OpCode::ADD_A ) )
          pbOp(
            { o, TC13::OperationFormat::SAC4_2,
              regParam( 0, WIR_Usage::defuse ),
              new TC_Const4_Signed( sConst( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_ABS ) &&
             ( ( o == TC13::OpCode::LD_A ) || ( o == TC13::OpCode::LEA ) ) )
          pbOp(
            { o, TC13::OperationFormat::AC18ABSA,
              regParam( 0, WIR_Usage::def ),
              new TC_Const18_Unsigned( uConst( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_ABS ) &&
             ( o == TC13::OpCode::LD_DA ) )
          pbOp(
            { o, TC13::OperationFormat::PC18ABSA,
              pregParam( 0, WIR_Usage::def ),
              new TC_Const18_Unsigned( uConst( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
             ( o == TC13::OpCode::MOV_A ) )
          pbOp(
            { o, TC13::OperationFormat::SAC4_1,
              regParam( 0, WIR_Usage::def ),
              new TC_Const4_Unsigned( uConst( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST16 ) &&
             ( o == TC13::OpCode::MOVH_A ) )
          pbOp(
            { o, TC13::OperationFormat::AC16,
              regParam( 0, WIR_Usage::def ),
              new TC_Const16_Unsigned( uConst( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( a[ 0 ]->getType() == TC_AsmArgument::Type::AREG_A10 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST8 ) &&
             ( o == TC13::OpCode::SUB_A ) )
          pbOp(
            { o, TC13::OperationFormat::SSPC8,
              regParam( 0, WIR_Usage::defuse ),
              new TC_Const8_Unsigned( uConst( 1 ) ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AREG ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( o == TC13::OpCode::ADD_A ) )
          pbOp(
            { o, TC13::OperationFormat::SAA_5,
              regParam( 0, WIR_Usage::defuse ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( o == TC13::OpCode::MOV_AA ) )
          pbOp(
            { o, TC13::OperationFormat::SAA_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w == TC_AsmOperationWidth::CODE32 ) &&
             ( o == TC13::OpCode::MOV_AA ) )
          pbOp(
            { o, TC13::OperationFormat::AA,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( ( o == TC13::OpCode::BSPLIT ) || ( o == TC13::OpCode::UNPACK ) ) )
        pbOp(
          { o, TC13::OperationFormat::ED,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AREG ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::EQZ_A ) || ( o == TC13::OpCode::NEZ_A ) ) )
          pbOp(
            { o, TC13::OperationFormat::DA,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( o == TC13::OpCode::MOV_D ) )
          pbOp(
            { o, TC13::OperationFormat::SDA_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w == TC_AsmOperationWidth::CODE32 ) &&
             ( o == TC13::OpCode::MOV_D ) )
          pbOp(
            { o, TC13::OperationFormat::DA,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DISP ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( a[ 0 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
             ( ( o == TC13::OpCode::JNZ ) || ( o == TC13::OpCode::JZ ) ) ) {
          auto *lParam = labelParam( 1 );
          if ( lParam != nullptr )
            pbOp(
              { o, TC13::OperationFormat::SIL,
                regParam( 0, WIR_Usage::use ), lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::SIL,
                regParam( 0, WIR_Usage::use ), new WIR_LabelParameter( b ) } );
            labelStringParam( 1 );
          }
        } else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( ( o == TC13::OpCode::JGEZ ) || ( o == TC13::OpCode::JGTZ ) ||
               ( o == TC13::OpCode::JLEZ ) || ( o == TC13::OpCode::JLTZ ) ||
               ( o == TC13::OpCode::JNZ ) || ( o == TC13::OpCode::JZ ) ) ) {
          auto *lParam = labelParam( 1 );
          if ( lParam != nullptr )
            pbOp(
              { o, TC13::OperationFormat::SDL,
                regParam( 0, WIR_Usage::use ), lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::SDL,
                regParam( 0, WIR_Usage::use ), new WIR_LabelParameter( b ) } );
            labelStringParam( 1 );
          }
        } else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::LD_B ) || ( o == TC13::OpCode::LD_BU ) ||
               ( o == TC13::OpCode::LD_H ) || ( o == TC13::OpCode::LD_HU ) ||
               ( o == TC13::OpCode::LD_Q ) || ( o == TC13::OpCode::LD_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::DLABSA,
              regParam( 0, WIR_Usage::def ), labelParam( 1 ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DISP ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::JNZ_A ) || ( o == TC13::OpCode::JZ_A ) ) ) {
          auto *lParam = labelParam( 1 );
          if ( lParam != nullptr )
            pbOp(
              { o, TC13::OperationFormat::AL_2,
                regParam( 0, WIR_Usage::use ), lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::AL_2,
                regParam( 0, WIR_Usage::use ), new WIR_LabelParameter( b ) } );
            labelStringParam( 1 );
          }
        } else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::JNZ_A ) || ( o == TC13::OpCode::JZ_A ) ) ) {
          auto *lParam = labelParam( 1 );
          if ( lParam != nullptr )
            pbOp(
              { o, TC13::OperationFormat::SAL_1,
                regParam( 0, WIR_Usage::use ), lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::SAL_1,
                regParam( 0, WIR_Usage::use ), new WIR_LabelParameter( b ) } );
            labelStringParam( 1 );
          }
        } else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::LD_A ) || ( o == TC13::OpCode::LEA ) ) )
          pbOp(
            { o, TC13::OperationFormat::ALABSA,
              regParam( 0, WIR_Usage::def ), labelParam( 1 ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( o == TC13::OpCode::LD_DA ) )
          pbOp(
            { o, TC13::OperationFormat::PLABSA,
              pregParam( 0, WIR_Usage::def ), labelParam( 1 ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( o == TC13::OpCode::LOOP ) )
          pbOp(
            { o, TC13::OperationFormat::AL_3,
              regParam( 0, WIR_Usage::defuse ), labelParam( 1 ) } );
        else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             ( o == TC13::OpCode::LOOP ) )
          pbOp(
            { o, TC13::OperationFormat::SAL_2,
              regParam( 0, WIR_Usage::defuse ), labelParam( 1 ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE ) &&
             ( ( o == TC13::OpCode::LD_A ) || ( o == TC13::OpCode::LEA ) ) )
          pbOp(
            { o, TC13::OperationFormat::AAC10BOA,
              regParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              new TC_Const10_Signed( iaregOffset( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BREV ) &&
             ( o == TC13::OpCode::LD_A ) )
          pbOp(
            { o, TC13::OperationFormat::APBRA,
              regParam( 0, WIR_Usage::def ),
              ipregParam( 1, WIR_Usage::defuse ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_CIRC ) &&
             ( o == TC13::OpCode::LD_A ) )
          pbOp(
            { o, TC13::OperationFormat::APC10CA,
              regParam( 0, WIR_Usage::def ),
              ipregParam( 1, WIR_Usage::defuse ),
              new TC_Const10_Signed( iaregOffset( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINC ) ||
               a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_PREINC ) ) &&
             ( o == TC13::OpCode::LD_A ) ) {
          long long offset = iaregOffset( 1 );

          if ( a[ 1 ]->getType() == TC_AsmArgument::Type::AMODE_POSTINCCNST )
            offset = 4;

          pbOp(
            { o, TC13::OperationFormat::AAC10PIA,
              regParam( 0, WIR_Usage::def ), amodeParam( 1 ),
              iaregParam( 1, WIR_Usage::defuse ),
              new TC_Const10_Signed( offset ) } );
        } else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASELO ) &&
             ( ( o == TC13::OpCode::LD_A ) || ( o == TC13::OpCode::LEA ) ) )
          pbOp(
            { o, TC13::OperationFormat::AAC16BOA,
              regParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              new TC_Const16_Signed( iaregOffset( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASELOLAB ) &&
             ( ( o == TC13::OpCode::LD_A ) || ( o == TC13::OpCode::LEA ) ) )
          pbOp(
            { o, TC13::OperationFormat::AALC16BOA,
              regParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              labelParam( 1 ), new TC_Const16_Signed( 0 ) } );
        else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             ( a[ 0 ]->getType() == TC_AsmArgument::Type::AREG_A15 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_SPREL ) &&
             ( o == TC13::OpCode::LD_A ) )
          pbOp(
            { o, TC13::OperationFormat::SISPC10_1,
              regParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              new TC_Const10_Unsigned( iaregOffset( 1 ) ) } );
        else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_REGI ) &&
             ( o == TC13::OpCode::LD_A ) )
          pbOp(
            { o, TC13::OperationFormat::SAA_2,
              regParam( 0, WIR_Usage::def ),
              iaregParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINC ) &&
             ( o == TC13::OpCode::LD_A ) )
          pbOp(
            { o, TC13::OperationFormat::SAA_3,
              regParam( 0, WIR_Usage::def ),
              iaregParam( 1, WIR_Usage::defuse ) } );
        else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_A15BASE4 ) &&
             ( o == TC13::OpCode::LD_A ) )
          pbOp(
            { o, TC13::OperationFormat::SAIC4,
              regParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              new TC_Const4_Unsigned( iaregOffset( 1 ) ) } );
        else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             ( a[ 0 ]->getType() == TC_AsmArgument::Type::AREG_A15 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE4 ) &&
             ( o == TC13::OpCode::LD_A ) )
          pbOp(
            { o, TC13::OperationFormat::SIAC4_1,
              regParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              new TC_Const4_Unsigned( iaregOffset( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE ) &&
             ( o == TC13::OpCode::LD_DA ) )
          pbOp(
            { o, TC13::OperationFormat::PAC10BOA,
              pregParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              new TC_Const10_Signed( iaregOffset( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BREV ) &&
             ( o == TC13::OpCode::LD_DA ) )
          pbOp(
            { o, TC13::OperationFormat::PPBRA_1,
              pregParam( 0, WIR_Usage::def ),
              ipregParam( 1, WIR_Usage::defuse ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_CIRC ) &&
             ( o == TC13::OpCode::LD_DA ) )
          pbOp(
            { o, TC13::OperationFormat::PPC10CA,
              pregParam( 0, WIR_Usage::def ),
              ipregParam( 1, WIR_Usage::defuse ),
              new TC_Const10_Signed( iaregOffset( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINC ) ||
               a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_PREINC ) ) &&
             ( o == TC13::OpCode::LD_DA ) )
          pbOp(
            { o, TC13::OperationFormat::PAC10PIA,
              pregParam( 0, WIR_Usage::def ), amodeParam( 1 ),
              iaregParam( 1, WIR_Usage::defuse ),
              new TC_Const10_Signed( iaregOffset( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASEHILAB ) &&
             ( o == TC13::OpCode::MOVH_A ) )
          pbOp(
            { o, TC13::OperationFormat::AL_1,
              regParam( 0, WIR_Usage::def ), labelParam( 1 ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE ) &&
             ( ( o == TC13::OpCode::LD_B ) || ( o == TC13::OpCode::LD_HU ) ||
               ( o == TC13::OpCode::LD_Q ) || ( o == TC13::OpCode::LD_BU ) ||
               ( o == TC13::OpCode::LD_H ) || ( o == TC13::OpCode::LD_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::DAC10BOA,
              regParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              new TC_Const10_Signed( iaregOffset( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BREV ) &&
             ( ( o == TC13::OpCode::LD_B ) || ( o == TC13::OpCode::LD_BU ) ||
               ( o == TC13::OpCode::LD_H ) || ( o == TC13::OpCode::LD_HU ) ||
               ( o == TC13::OpCode::LD_Q ) || ( o == TC13::OpCode::LD_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::DPBRA,
              regParam( 0, WIR_Usage::def ),
              ipregParam( 1, WIR_Usage::defuse ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_CIRC ) &&
             ( ( o == TC13::OpCode::LD_B ) || ( o == TC13::OpCode::LD_BU ) ||
               ( o == TC13::OpCode::LD_H ) || ( o == TC13::OpCode::LD_HU ) ||
               ( o == TC13::OpCode::LD_Q ) || ( o == TC13::OpCode::LD_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::DPC10CA,
              regParam( 0, WIR_Usage::def ), ipregParam( 1, WIR_Usage::defuse ),
              new TC_Const10_Signed( iaregOffset( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINC ) ||
               a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_PREINC ) ) &&
             ( ( o == TC13::OpCode::LD_B ) || ( o == TC13::OpCode::LD_BU ) ||
               ( o == TC13::OpCode::LD_H ) || ( o == TC13::OpCode::LD_HU ) ||
               ( o == TC13::OpCode::LD_Q ) || ( o == TC13::OpCode::LD_W ) ) ) {
          long long offset = iaregOffset( 1 );

          if ( a[ 1 ]->getType() == TC_AsmArgument::Type::AMODE_POSTINCCNST ) {
            if ( o == TC13::OpCode::LD_BU )
              offset = 1;
            else

            if ( o == TC13::OpCode::ST_H )
              offset = 2;
            else

            if ( o == TC13::OpCode::ST_W )
              offset = 4;
          }

          pbOp(
            { o, TC13::OperationFormat::DAC10PIA,
              regParam( 0, WIR_Usage::def ), amodeParam( 1 ),
              iaregParam( 1, WIR_Usage::defuse ),
              new TC_Const10_Signed( offset ) } );
        } else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_REGI ) &&
             ( ( o == TC13::OpCode::LD_BU ) || ( o == TC13::OpCode::LD_H ) ||
               ( o == TC13::OpCode::LD_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::SDA_2,
              regParam( 0, WIR_Usage::def ),
              iaregParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINC ) &&
             ( ( o == TC13::OpCode::LD_BU ) || ( o == TC13::OpCode::LD_H ) ||
               ( o == TC13::OpCode::LD_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::SDA_3,
              regParam( 0, WIR_Usage::def ),
              iaregParam( 1, WIR_Usage::defuse ) } );
        else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_A15BASE4 ) &&
             ( ( o == TC13::OpCode::LD_BU ) || ( o == TC13::OpCode::LD_H ) ||
               ( o == TC13::OpCode::LD_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::SDIC4_1,
              regParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              new TC_Const4_Unsigned( iaregOffset( 1 ) ) } );
        else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             ( a[ 0 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE4 ) &&
             ( ( o == TC13::OpCode::LD_BU ) || ( o == TC13::OpCode::LD_H ) ||
               ( o == TC13::OpCode::LD_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::SIAC4_2,
              regParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              new TC_Const4_Unsigned( iaregOffset( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASELO ) &&
             ( o == TC13::OpCode::LD_W ) )
          pbOp(
            { o, TC13::OperationFormat::DAC16BOA,
              regParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              new TC_Const16_Signed( iaregOffset( 1 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASELOLAB ) &&
             ( o == TC13::OpCode::LD_W ) )
          pbOp(
            { o, TC13::OperationFormat::DALC16BOA,
              regParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              labelParam( 1 ), new TC_Const16_Signed( 0 ) } );
        else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             ( a[ 0 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_SPREL ) &&
             ( o == TC13::OpCode::LD_W ) )
          pbOp(
            { o, TC13::OperationFormat::SISPC10_2,
              regParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              new TC_Const10_Unsigned( iaregOffset( 1 ) ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_ABS ) &&
           ( o == TC13::OpCode::LD_D ) )
        pbOp(
          { o, TC13::OperationFormat::EC18ABSA,
            regParam( 0, WIR_Usage::def ),
            new TC_Const18_Unsigned( uConst( 1 ) ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DISP ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::LD_D ) )
        pbOp(
          { o, TC13::OperationFormat::ELABSA,
            regParam( 0, WIR_Usage::def ), labelParam( 1 ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::LD_D ) ) {
        if ( a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE ) )
          pbOp(
            { o, TC13::OperationFormat::EAC10BOA,
              regParam( 0, WIR_Usage::def ), iaregParam( 1, WIR_Usage::use ),
              new TC_Const10_Signed( iaregOffset( 1 ) ) } );
        else

        if ( a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_BREV ) )
          pbOp(
            { o, TC13::OperationFormat::EPBRA,
              regParam( 0, WIR_Usage::def ),
              ipregParam( 1, WIR_Usage::defuse ) } );
        else

        if ( a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_CIRC ) )
          pbOp(
            { o, TC13::OperationFormat::EPC10CA,
              regParam( 0, WIR_Usage::def ), ipregParam( 1, WIR_Usage::defuse ),
              new TC_Const10_Signed( iaregOffset( 1 ) ) } );
        else

        if ( ( a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINC ) ||
               a[ 1 ]->isCompatible( TC_AsmArgument::Type::AMODE_PREINC ) ) )
          pbOp(
            { o, TC13::OperationFormat::EAC10PIA,
              regParam( 0, WIR_Usage::def ), amodeParam( 1 ),
              iaregParam( 1, WIR_Usage::defuse ),
              new TC_Const10_Signed( iaregOffset( 1 ) ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::CONST ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_ABS ) &&
           ( ( o == TC13::OpCode::LDMST ) || ( o == TC13::OpCode::ST_D ) ) )
        pbOp(
          { o, TC13::OperationFormat::C18EABSA,
            new TC_Const18_Unsigned( uConst( 0 ) ),
            regParam( 1, WIR_Usage::use ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DISP ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( ( o == TC13::OpCode::LDMST ) || ( o == TC13::OpCode::ST_D ) ) )
        pbOp(
          { o, TC13::OperationFormat::LEABSA,
            labelParam( 0 ), regParam( 1, WIR_Usage::use ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( ( o == TC13::OpCode::LDMST ) || ( o == TC13::OpCode::ST_D ) ) ) {
        if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE ) )
          pbOp(
            { o, TC13::OperationFormat::AC10EBOA,
              iaregParam( 0, WIR_Usage::use ),
              new TC_Const10_Signed( iaregOffset( 0 ) ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BREV ) )
          pbOp(
            { o, TC13::OperationFormat::PEBRA,
              ipregParam( 0, WIR_Usage::defuse ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_CIRC ) )
          pbOp(
            { o, TC13::OperationFormat::PC10ECA,
              ipregParam( 0, WIR_Usage::defuse ),
              new TC_Const10_Signed( iaregOffset( 0 ) ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINC ) ||
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_PREINC ) )
          pbOp(
            { o, TC13::OperationFormat::AC10EPIA,
              amodeParam( 0 ), iaregParam( 0, WIR_Usage::defuse ),
              new TC_Const10_Signed( iaregOffset( 0 ) ),
              regParam( 1, WIR_Usage::use ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           ( o == TC13::OpCode::MOV_A ) ) {
        if ( w != TC_AsmOperationWidth::CODE32 )
          pbOp(
            { o, TC13::OperationFormat::SAD_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ) } );
        else
          pbOp(
            { o, TC13::OperationFormat::AD,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::CONST ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) ) {
        if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::CONST16 ) &&
             ( o == TC13::OpCode::MTCR ) )
          pbOp(
            { o, TC13::OperationFormat::C16DPSW,
              new TC_Const16_Unsigned( uConst( 0 ) ),
              regParam( 1, WIR_Usage::use ),
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::def ) } );
        else

        if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_ABS ) &&
             ( ( o == TC13::OpCode::ST_B ) || ( o == TC13::OpCode::ST_H ) ||
               ( o == TC13::OpCode::ST_Q ) || ( o == TC13::OpCode::ST_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::C18DABSA_1,
              new TC_Const18_Unsigned( uConst( 0 ) ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_ABS ) &&
             ( o == TC13::OpCode::SWAP_W ) )
          pbOp(
            { o, TC13::OperationFormat::C18DABSA_2,
              new TC_Const18_Unsigned( uConst( 0 ) ),
              regParam( 1, WIR_Usage::defuse ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::CONST ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_ABS ) ) {
        if ( o == TC13::OpCode::ST_A )
          pbOp(
            { o, TC13::OperationFormat::C18AABSA,
              new TC_Const18_Unsigned( uConst( 0 ) ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( o == TC13::OpCode::ST_DA )
          pbOp(
            { o, TC13::OperationFormat::C18PABSA,
              new TC_Const18_Unsigned( uConst( 0 ) ),
              pregParam( 1, WIR_Usage::use ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DISP ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) ) {
        if ( o == TC13::OpCode::ST_A )
          pbOp(
            { o, TC13::OperationFormat::LAABSA,
              labelParam( 0 ), regParam( 1, WIR_Usage::use ) } );
        else

        if ( o == TC13::OpCode::ST_DA )
          pbOp(
            { o, TC13::OperationFormat::LPABSA,
              labelParam( 0 ), pregParam( 1, WIR_Usage::use ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AREG ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( o == TC13::OpCode::ST_A ) ) {
          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE ) )
            pbOp(
              { o, TC13::OperationFormat::AC10ABOA,
                iaregParam( 0, WIR_Usage::use ),
                new TC_Const10_Signed( iaregOffset( 0 ) ),
                regParam( 1, WIR_Usage::use ) } );
          else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BREV ) )
            pbOp(
              { o, TC13::OperationFormat::PABRA,
                ipregParam( 0, WIR_Usage::defuse ),
                regParam( 1, WIR_Usage::use ) } );
          else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_CIRC ) )
            pbOp(
              { o, TC13::OperationFormat::PC10ACA,
                ipregParam( 0, WIR_Usage::defuse ),
                new TC_Const10_Signed( iaregOffset( 0 ) ),
                regParam( 1, WIR_Usage::use ) } );
          else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINC ) ||
               a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_PREINC ) ) {
            long long offset = iaregOffset( 0 );

            if ( a[ 0 ]->getType() == TC_AsmArgument::Type::AMODE_POSTINCCNST )
              offset = 4;

            pbOp(
              { o, TC13::OperationFormat::AC10APIA,
                amodeParam( 0 ), iaregParam( 0, WIR_Usage::defuse ),
                new TC_Const10_Signed( offset ),
                regParam( 1, WIR_Usage::use ) } );
          }
        } else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_SPREL ) &&
             ( a[ 1 ]->getType() == TC_AsmArgument::Type::AREG_A15 ) &&
             ( o == TC13::OpCode::ST_A ) )
          pbOp(
            { o, TC13::OperationFormat::SSPC10I_1,
              iaregParam( 0, WIR_Usage::use ),
              new TC_Const10_Unsigned( iaregOffset( 0 ) ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE4 ) &&
             ( a[ 1 ]->getType() == TC_AsmArgument::Type::AREG_A15 ) &&
             ( o == TC13::OpCode::ST_A ) )
          pbOp(
            { o, TC13::OperationFormat::SAC4I_1,
              iaregParam( 0, WIR_Usage::use ),
              new TC_Const4_Unsigned( iaregOffset( 0 ) ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_REGI ) &&
             ( o == TC13::OpCode::ST_A ) )
          pbOp(
            { o, TC13::OperationFormat::SAA_4,
              iaregParam( 0, WIR_Usage::use ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINCCNST ) &&
             ( o == TC13::OpCode::ST_A ) )
          pbOp(
            { o, TC13::OperationFormat::SAA_6,
              iaregParam( 0, WIR_Usage::defuse ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_A15BASE4 ) &&
             ( o == TC13::OpCode::ST_A ) )
          pbOp(
            { o, TC13::OperationFormat::SIC4A,
              iaregParam( 0, WIR_Usage::use ),
              new TC_Const4_Unsigned( iaregOffset( 0 ) ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( o == TC13::OpCode::ST_DA ) ) {
          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE ) )
            pbOp(
              { o, TC13::OperationFormat::AC10PBOA,
                iaregParam( 0, WIR_Usage::use ),
                new TC_Const10_Signed( iaregOffset( 0 ) ),
                pregParam( 1, WIR_Usage::use ) } );
          else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BREV ) )
            pbOp(
              { o, TC13::OperationFormat::PPBRA_2,
                ipregParam( 0, WIR_Usage::defuse ),
                pregParam( 1, WIR_Usage::use ) } );
          else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_CIRC ) )
            pbOp(
              { o, TC13::OperationFormat::PC10PCA,
                ipregParam( 0, WIR_Usage::defuse ),
                new TC_Const10_Signed( iaregOffset( 0 ) ),
                pregParam( 1, WIR_Usage::use ) } );
          else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINC ) ||
               a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_PREINC ) )
            pbOp(
              { o, TC13::OperationFormat::AC10PPIA,
                amodeParam( 0 ), iaregParam( 0, WIR_Usage::defuse ),
                new TC_Const10_Signed( iaregOffset( 0 ) ),
                pregParam( 1, WIR_Usage::use ) } );
        }
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DISP ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) ) {
        if ( ( o == TC13::OpCode::ST_B ) || ( o == TC13::OpCode::ST_H ) ||
             ( o == TC13::OpCode::ST_Q ) || ( o == TC13::OpCode::ST_W ) )
          pbOp(
            { o, TC13::OperationFormat::LDABSA_1,
              labelParam( 0 ), regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( o == TC13::OpCode::SWAP_W ) )
          pbOp(
            { o, TC13::OperationFormat::LDABSA_2,
              labelParam( 0 ), regParam( 1, WIR_Usage::defuse ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::ST_B ) || ( o == TC13::OpCode::ST_H ) ||
               ( o == TC13::OpCode::ST_Q ) || ( o == TC13::OpCode::ST_W ) ) ) {
          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE ) )
            pbOp(
              { o, TC13::OperationFormat::AC10DBOA_1,
                iaregParam( 0, WIR_Usage::use ),
                new TC_Const10_Signed( iaregOffset( 0 ) ),
                regParam( 1, WIR_Usage::use ) } );
          else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BREV ) )
            pbOp(
              { o, TC13::OperationFormat::PDBRA_1,
                ipregParam( 0, WIR_Usage::defuse ),
                regParam( 1, WIR_Usage::use ) } );
          else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_CIRC ) )
            pbOp(
              { o, TC13::OperationFormat::PC10DCA_1,
                ipregParam( 0, WIR_Usage::defuse ),
                new TC_Const10_Signed( iaregOffset( 0 ) ),
                regParam( 1, WIR_Usage::use ) } );
          else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINC ) ||
               a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_PREINC ) ) {
            long long offset = iaregOffset( 0 );

            if ( a[ 0 ]->getType() ==
                   TC_AsmArgument::Type::AMODE_POSTINCCNST ) {
              if ( o == TC13::OpCode::ST_B )
                offset = 1;
              else

              if ( o == TC13::OpCode::ST_H )
                offset = 2;

              else
                offset = 4;
            }

            pbOp(
              { o, TC13::OperationFormat::AC10DPIA_1,
                amodeParam( 0 ), iaregParam( 0, WIR_Usage::defuse ),
                new TC_Const10_Signed( offset ),
                regParam( 1, WIR_Usage::use ) } );
          } else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASELO ) &&
               ( o == TC13::OpCode::ST_W ) )
            pbOp(
              { o, TC13::OperationFormat::AC16DBOA,
                iaregParam( 0, WIR_Usage::use ),
                new TC_Const16_Signed( iaregOffset( 0 ) ),
                regParam( 1, WIR_Usage::use ) } );
          else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASELOLAB ) &&
               ( o == TC13::OpCode::ST_W ) )
            pbOp(
              { o, TC13::OperationFormat::ALC16DBOA,
                iaregParam( 0, WIR_Usage::use ), labelParam( 0 ),
                new TC_Const16_Signed( 0 ),
                regParam( 1, WIR_Usage::use ) } );
        } else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE4 ) &&
             ( a[ 1 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
             ( ( o == TC13::OpCode::ST_B ) || ( o == TC13::OpCode::ST_H ) ||
               ( o == TC13::OpCode::ST_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::SAC4I_2,
              iaregParam( 0, WIR_Usage::use ),
              new TC_Const4_Unsigned( iaregOffset( 0 ) ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_REGI ) &&
             ( ( o == TC13::OpCode::ST_B ) || ( o == TC13::OpCode::ST_H ) ||
               ( o == TC13::OpCode::ST_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::SAD_2,
              iaregParam( 0, WIR_Usage::use ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINCCNST ) &&
             ( ( o == TC13::OpCode::ST_B ) || ( o == TC13::OpCode::ST_H ) ||
               ( o == TC13::OpCode::ST_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::SAD_3,
              iaregParam( 0, WIR_Usage::defuse ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_A15BASE4 ) &&
             ( ( o == TC13::OpCode::ST_B ) || ( o == TC13::OpCode::ST_H ) ||
               ( o == TC13::OpCode::ST_W ) ) )
          pbOp(
            { o, TC13::OperationFormat::SIC4D,
              iaregParam( 0, WIR_Usage::use ),
              new TC_Const4_Unsigned( iaregOffset( 0 ) ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_SPREL ) &&
             ( a[ 1 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
             ( o == TC13::OpCode::ST_W ) )
          pbOp(
            { o, TC13::OperationFormat::SSPC10I_2,
              iaregParam( 0, WIR_Usage::use ),
              new TC_Const10_Unsigned( iaregOffset( 0 ) ),
              regParam( 1, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( o == TC13::OpCode::SWAP_W ) ) {
          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BASE ) )
            pbOp(
              { o, TC13::OperationFormat::AC10DBOA_2,
                iaregParam( 0, WIR_Usage::use ),
                new TC_Const10_Signed( iaregOffset( 0 ) ),
                regParam( 1, WIR_Usage::defuse ) } );
          else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_BREV ) )
            pbOp(
              { o, TC13::OperationFormat::PDBRA_2,
                ipregParam( 0, WIR_Usage::defuse ),
                regParam( 1, WIR_Usage::defuse ) } );
          else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_CIRC ) )
            pbOp(
              { o, TC13::OperationFormat::PC10DCA_2,
                ipregParam( 0, WIR_Usage::defuse ),
                new TC_Const10_Signed( iaregOffset( 0 ) ),
                regParam( 1, WIR_Usage::defuse ) } );
          else

          if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_POSTINC ) ||
               a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_PREINC ) )
            pbOp(
              { o, TC13::OperationFormat::AC10DPIA_2,
                amodeParam( 0 ), iaregParam( 0, WIR_Usage::defuse ),
                new TC_Const10_Signed( iaregOffset( 0 ) ),
                regParam( 1, WIR_Usage::defuse ) } );
        }
      }

      break;
    }

    case 3: {
      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) &&
             ( o == TC13::OpCode::ADDIH ) )
            pbOp(
              { o, TC13::OperationFormat::DDC9_1,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                new TC_Const9_Signed( sConst( 2 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) &&
             ( ( o == TC13::OpCode::ABSDIF ) ||
               ( o == TC13::OpCode::ABSDIFS ) || ( o == TC13::OpCode::ADDS ) ||
               ( o == TC13::OpCode::ADDS_U ) ||
               ( o == TC13::OpCode::EQANY_B ) ||
               ( o == TC13::OpCode::EQANY_H ) || ( o == TC13::OpCode::GE ) ||
               ( o == TC13::OpCode::MAX ) || ( o == TC13::OpCode::MIN ) ||
               ( o == TC13::OpCode::MUL ) || ( o == TC13::OpCode::MULS ) ||
               ( o == TC13::OpCode::NE ) || ( o == TC13::OpCode::RSUB ) ||
               ( o == TC13::OpCode::RSUBS ) || ( o == TC13::OpCode::RSUBS_U ) ||
               ( o == TC13::OpCode::SH ) || ( o == TC13::OpCode::SH_H ) ||
               ( o == TC13::OpCode::SHA_H ) || ( o == TC13::OpCode::SHAS ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDC9_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              new TC_Const9_Signed( sConst( 2 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) &&
             ( o == TC13::OpCode::SHA ) )
          pbOp(
            { o, TC13::OperationFormat::DDC9PSW_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              new TC_Const9_Signed( sConst( 2 ) ),
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::def ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) &&
             ( o == TC13::OpCode::ADDC ) )
          pbOp(
            { o, TC13::OperationFormat::DDC9PSW_2,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              new TC_Const9_Signed( sConst( 2 ) ),
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::defuse ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) &&
             ( o == TC13::OpCode::ADDX ) )
          pbOp(
            { o, TC13::OperationFormat::DDC9PSW_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              new TC_Const9_Signed( sConst( 2 ) ),
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::def ) } );
        else

        if ( ( o == TC13::OpCode::EQ ) || ( o == TC13::OpCode::LT ) ) {
          if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
               ( a[ 0 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
               a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) )
            pbOp(
              { o, TC13::OperationFormat::SIDC4,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                new TC_Const4_Signed( sConst( 2 ) ) } );
          else

          if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
               a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) )
            pbOp(
              { o, TC13::OperationFormat::DDC9_1,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                new TC_Const9_Signed( sConst( 2 ) ) } );
        } else

        if ( o == TC13::OpCode::ADD ) {
          if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
               ( a[ 0 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
               a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) )
            pbOp(
              { o, TC13::OperationFormat::SIDC4,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                new TC_Const4_Signed( sConst( 2 ) ) } );
          else

          if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
               ( a[ 1 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
               a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) )
            pbOp(
              { o, TC13::OperationFormat::SDIC4_2,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                new TC_Const4_Signed( sConst( 2 ) ) } );
          else

          if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
               a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) )
            pbOp(
              { o, TC13::OperationFormat::DDC9_1,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                new TC_Const9_Signed( sConst( 2 ) ) } );
        } else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST16 ) &&
             ( o == TC13::OpCode::ADDI ) )
          pbOp(
            { o, TC13::OperationFormat::DDC16_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              new TC_Const16_Signed( sConst( 2 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST16 ) &&
             ( o == TC13::OpCode::ADDIH ) )
          pbOp(
            { o, TC13::OperationFormat::DDC16_2,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              new TC_Const16_Unsigned( uConst( 2 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) &&
             ( ( o == TC13::OpCode::AND ) || ( o == TC13::OpCode::ANDN ) ||
               ( o == TC13::OpCode::GE_U ) || ( o == TC13::OpCode::LT_U ) ||
               ( o == TC13::OpCode::MAX_U ) || ( o == TC13::OpCode::MIN_U ) ||
               ( o == TC13::OpCode::MULS_U ) || ( o == TC13::OpCode::NAND ) ||
               ( o == TC13::OpCode::NOR ) || ( o == TC13::OpCode::OR ) ||
               ( o == TC13::OpCode::ORN ) || ( o == TC13::OpCode::XNOR ) ||
               ( o == TC13::OpCode::XOR ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDC9_2,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              new TC_Const9_Unsigned( uConst( 2 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) &&
             ( ( o == TC13::OpCode::AND_EQ ) || ( o == TC13::OpCode::AND_GE ) ||
               ( o == TC13::OpCode::AND_LT ) || ( o == TC13::OpCode::AND_NE ) ||
               ( o == TC13::OpCode::OR_EQ ) || ( o == TC13::OpCode::OR_GE ) ||
               ( o == TC13::OpCode::OR_LT ) || ( o == TC13::OpCode::OR_NE ) ||
               ( o == TC13::OpCode::SH_EQ ) || ( o == TC13::OpCode::SH_GE ) ||
               ( o == TC13::OpCode::SH_LT ) || ( o == TC13::OpCode::SH_NE ) ||
               ( o == TC13::OpCode::XOR_EQ ) || ( o == TC13::OpCode::XOR_GE ) ||
               ( o == TC13::OpCode::XOR_LT ) ||
               ( o == TC13::OpCode::XOR_NE ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDC9_3,
              regParam( 0, WIR_Usage::defuse ), regParam( 1, WIR_Usage::use ),
              new TC_Const9_Signed( sConst( 2 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) &&
             ( ( o == TC13::OpCode::AND_GE_U ) ||
               ( o == TC13::OpCode::AND_LT_U ) ||
               ( o == TC13::OpCode::OR_GE_U ) ||
               ( o == TC13::OpCode::OR_LT_U ) ||
               ( o == TC13::OpCode::SH_GE_U ) ||
               ( o == TC13::OpCode::SH_LT_U ) ||
               ( o == TC13::OpCode::XOR_GE_U ) ||
               ( o == TC13::OpCode::XOR_LT_U ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDC9_4,
              regParam( 0, WIR_Usage::defuse ), regParam( 1, WIR_Usage::use ),
              new TC_Const9_Unsigned( uConst( 2 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( a[ 1 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
             a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
             ( ( o == TC13::OpCode::CADD ) || ( o == TC13::OpCode::CADDN ) ||
               ( o == TC13::OpCode::CMOV ) || ( o == TC13::OpCode::CMOVN ) ) )
          pbOp(
            { o, TC13::OperationFormat::SDIC4_3,
              regParam( 0, WIR_Usage::defuse ), regParam( 1, WIR_Usage::use ),
              new TC_Const4_Signed( sConst( 2 ) ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::ABSDIF ) ||
               ( o == TC13::OpCode::ABSDIF_B ) ||
               ( o == TC13::OpCode::ABSDIF_H ) ||
               ( o == TC13::OpCode::ABSDIFS ) ||
               ( o == TC13::OpCode::ABSDIFS_H ) ||
               ( o == TC13::OpCode::ADD_B ) || ( o == TC13::OpCode::ADD_F ) ||
               ( o == TC13::OpCode::ADD_H ) || ( o == TC13::OpCode::ADDS ) ||
               ( o == TC13::OpCode::ADDS_H ) ||
               ( o == TC13::OpCode::ADDS_HU ) ||
               ( o == TC13::OpCode::ADDS_U ) || ( o == TC13::OpCode::AND ) ||
               ( o == TC13::OpCode::ANDN ) || ( o == TC13::OpCode::BMERGE ) ||
               ( o == TC13::OpCode::CMP_F ) || ( o == TC13::OpCode::DIV_F ) ||
               ( o == TC13::OpCode::EQ_B ) || ( o == TC13::OpCode::EQ_H ) ||
               ( o == TC13::OpCode::EQ_W ) || ( o == TC13::OpCode::EQANY_B ) ||
               ( o == TC13::OpCode::EQANY_H ) ||
               ( o == TC13::OpCode::FTOQ31 ) ||
               // cppcheck-suppress duplicateExpression
               ( o == TC131::OpCode::FTOQ31Z ) || ( o == TC13::OpCode::GE ) ||
               ( o == TC13::OpCode::GE_U ) || ( o == TC13::OpCode::LT_B ) ||
               ( o == TC13::OpCode::LT_BU ) || ( o == TC13::OpCode::LT_H ) ||
               ( o == TC13::OpCode::LT_HU ) || ( o == TC13::OpCode::LT_U ) ||
               ( o == TC13::OpCode::LT_W ) || ( o == TC13::OpCode::LT_WU ) ||
               ( o == TC13::OpCode::MAX ) || ( o == TC13::OpCode::MAX_B ) ||
               ( o == TC13::OpCode::MAX_BU ) || ( o == TC13::OpCode::MAX_H ) ||
               ( o == TC13::OpCode::MAX_HU ) || ( o == TC13::OpCode::MAX_U ) ||
               ( o == TC13::OpCode::MIN ) || ( o == TC13::OpCode::MIN_B ) ||
               ( o == TC13::OpCode::MIN_BU ) || ( o == TC13::OpCode::MIN_H ) ||
               ( o == TC13::OpCode::MIN_HU ) || ( o == TC13::OpCode::MIN_U ) ||
               ( o == TC13::OpCode::MUL ) || ( o == TC13::OpCode::MUL_F ) ||
               ( o == TC13::OpCode::MULS ) || ( o == TC13::OpCode::MULS_U ) ||
               ( o == TC13::OpCode::NAND ) || ( o == TC13::OpCode::NE ) ||
               ( o == TC13::OpCode::NOR ) || ( o == TC13::OpCode::OR ) ||
               ( o == TC13::OpCode::ORN ) || ( o == TC13::OpCode::Q31TOF ) ||
               ( o == TC13::OpCode::SH ) || ( o == TC13::OpCode::SH_H ) ||
               ( o == TC13::OpCode::SHA_H ) || ( o == TC13::OpCode::SHAS ) ||
               ( o == TC13::OpCode::SUB_B ) || ( o == TC13::OpCode::SUB_F ) ||
               ( o == TC13::OpCode::SUB_H ) || ( o == TC13::OpCode::SUBS ) ||
               ( o == TC13::OpCode::SUBS_H ) ||
               ( o == TC13::OpCode::SUBS_HU ) ||
               ( o == TC13::OpCode::SUBS_U ) || ( o == TC13::OpCode::XNOR ) ||
               ( o == TC13::OpCode::XOR ) ||
               // TriCore TC1.3.1
               ( o == TC131::OpCode::FTOQ31Z ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDD_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::ADDC ) || ( o == TC13::OpCode::SUBC ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDDPSW_2,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ),
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::defuse ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::ADDX ) || ( o == TC13::OpCode::SUBX ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDDPSW_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ),
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::def ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( o == TC13::OpCode::SHA ) )
          pbOp(
            { o, TC13::OperationFormat::DDDPSW_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ),
              new WIR_RegisterParameter( getPSW_C(), WIR_Usage::def ) } );
        else

        if ( ( o == TC13::OpCode::EQ ) || ( o == TC13::OpCode::LT ) ) {
          if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
               ( a[ 0 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) )
            pbOp(
              { o, TC13::OperationFormat::SIDD,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ) } );
          else

          if ( w != TC_AsmOperationWidth::CODE16 )
            pbOp(
              { o, TC13::OperationFormat::DDD_1,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ) } );
        } else

        if ( ( o == TC13::OpCode::ADD ) || ( o == TC13::OpCode::SUB ) ) {
          if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
               ( a[ 0 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) )
            pbOp(
              { o, TC13::OperationFormat::SIDD,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ) } );
          else

          if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
               ( a[ 1 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) )
            pbOp(
              { o, TC13::OperationFormat::SDID_1,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ) } );
          else

          if ( w != TC_AsmOperationWidth::CODE16 )
            pbOp(
              { o, TC13::OperationFormat::DDD_1,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ) } );
        } else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::AND_EQ ) || ( o == TC13::OpCode::AND_GE ) ||
               ( o == TC13::OpCode::AND_GE_U ) ||
               ( o == TC13::OpCode::AND_LT ) ||
               ( o == TC13::OpCode::AND_LT_U ) ||
               ( o == TC13::OpCode::AND_NE ) || ( o == TC13::OpCode::OR_EQ ) ||
               ( o == TC13::OpCode::OR_GE ) || ( o == TC13::OpCode::OR_GE_U ) ||
               ( o == TC13::OpCode::OR_LT ) || ( o == TC13::OpCode::OR_LT_U ) ||
               ( o == TC13::OpCode::OR_NE ) || ( o == TC13::OpCode::SH_EQ ) ||
               ( o == TC13::OpCode::SH_GE ) || ( o == TC13::OpCode::SH_GE_U ) ||
               ( o == TC13::OpCode::SH_LT ) || ( o == TC13::OpCode::SH_LT_U ) ||
               ( o == TC13::OpCode::SH_NE ) || ( o == TC13::OpCode::XOR_EQ ) ||
               ( o == TC13::OpCode::XOR_GE ) ||
               ( o == TC13::OpCode::XOR_GE_U ) ||
               ( o == TC13::OpCode::XOR_LT ) ||
               ( o == TC13::OpCode::XOR_LT_U ) ||
               ( o == TC13::OpCode::XOR_NE ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDD_2,
              regParam( 0, WIR_Usage::defuse ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( a[ 1 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
             ( ( o == TC13::OpCode::CMOV ) || ( o == TC13::OpCode::CMOVN ) ) )
          pbOp(
            { o, TC13::OperationFormat::SDID_2,
              regParam( 0, WIR_Usage::defuse ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( ( o == TC13::OpCode::ADD_A ) || ( o == TC13::OpCode::SUB_A ) ) )
        pbOp(
          { o, TC13::OperationFormat::AAA,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            regParam( 2, WIR_Usage::use ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST16 ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::ADDIH_A ) )
        pbOp(
          { o, TC13::OperationFormat::AAC16,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            new TC_Const16_Unsigned( uConst( 2 ) ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::ADDSC_AT ) )
        pbOp(
          { o, TC13::OperationFormat::AAD,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            regParam( 2, WIR_Usage::use ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( ( o == TC13::OpCode::DVADJ ) || ( o == TC13::OpCode::DVSTEP ) ||
             ( o == TC13::OpCode::DVSTEP_U ) ||
             ( o == TC13::OpCode::IXMAX ) || ( o == TC13::OpCode::IXMAX_U ) ||
             ( o == TC13::OpCode::IXMIN ) ||
             ( o == TC13::OpCode::IXMIN_U ) ) )
        pbOp(
          { o, TC13::OperationFormat::EED,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            regParam( 2, WIR_Usage::use ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( ( o == TC13::OpCode::DVINIT ) || ( o == TC13::OpCode::DVINIT_B ) ||
             ( o == TC13::OpCode::DVINIT_BU ) ||
             ( o == TC13::OpCode::DVINIT_H ) ||
             ( o == TC13::OpCode::DVINIT_HU ) ||
             ( o == TC13::OpCode::DVINIT_U ) || ( o == TC13::OpCode::MUL ) ||
             ( o == TC13::OpCode::MUL_U ) ) )
        pbOp(
          { o, TC13::OperationFormat::EDD,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            regParam( 2, WIR_Usage::use ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( ( o == TC13::OpCode::EQ_A ) || ( o == TC13::OpCode::GE_A ) ||
             ( o == TC13::OpCode::LT_A ) || ( o == TC13::OpCode::NE_A ) ) )
        pbOp(
          { o, TC13::OperationFormat::DAA,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            regParam( 2, WIR_Usage::use ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( ( o == TC13::OpCode::EXTR ) || ( o == TC13::OpCode::EXTR_U ) ) )
        pbOp(
          { o, TC13::OperationFormat::DDE,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            regParam( 2, WIR_Usage::use ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DISP ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
             ( ( o == TC13::OpCode::JGE ) || ( o == TC13::OpCode::JLT ) ||
               ( o == TC13::OpCode::JEQ ) || ( o == TC13::OpCode::JNE ) ) ) {
          auto *lParam = labelParam( 2 );
          if ( lParam != nullptr )
            pbOp(
              { o, TC13::OperationFormat::DC4L_1,
                regParam( 0, WIR_Usage::use ),
                new TC_Const4_Signed( sConst( 1 ) ), lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::DC4L_1,
                regParam( 0, WIR_Usage::use ),
                new TC_Const4_Signed( sConst( 1 ) ),
                new WIR_LabelParameter( b ) } );
            labelStringParam( 2 );
          }
        } else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             ( a[ 0 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
             ( ( o == TC13::OpCode::JEQ ) || ( o == TC13::OpCode::JNE ) ) ) {
          auto *lParam = labelParam( 2 );
          if ( lParam != nullptr )
            pbOp(
              { o, TC13::OperationFormat::SIC4L,
                regParam( 0, WIR_Usage::use ),
                new TC_Const4_Signed( sConst( 1 ) ), lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::SIC4L,
                regParam( 0, WIR_Usage::use ),
                new TC_Const4_Signed( sConst( 1 ) ),
                new WIR_LabelParameter( b ) } );
            labelStringParam( 2 );
          }
        } else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
             ( ( o == TC13::OpCode::JGE_U ) ||
               ( o == TC13::OpCode::JLT_U ) ) ) {
          auto *lParam = labelParam( 2 );
          if ( lParam != nullptr )
            pbOp(
              { o, TC13::OperationFormat::DC4L_2,
                regParam( 0, WIR_Usage::use ),
                new TC_Const4_Unsigned( uConst( 1 ) ), lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::DC4L_2,
                regParam( 0, WIR_Usage::use ),
                new TC_Const4_Unsigned( uConst( 1 ) ),
                new WIR_LabelParameter( b ) } );
            labelStringParam( 2 );
          }
        } else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
             ( ( o == TC13::OpCode::JNED ) || ( o == TC13::OpCode::JNEI ) ) ) {
          auto *lParam = labelParam( 2 );
          if ( lParam != nullptr )
            pbOp(
              { o, TC13::OperationFormat::DC4L_3,
                regParam( 0, WIR_Usage::defuse ),
                new TC_Const4_Signed( sConst( 1 ) ), lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::DC4L_3,
                regParam( 0, WIR_Usage::defuse ),
                new TC_Const4_Signed( sConst( 1 ) ),
                new WIR_LabelParameter( b ) } );
            labelStringParam( 2 );
          }
        } else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
             ( ( o == TC13::OpCode::JNZ_T ) || ( o == TC13::OpCode::JZ_T ) ) ) {
          auto *lParam = labelParam( 2 );
          if ( lParam != nullptr )
            pbOp(
              { o, TC13::OperationFormat::DC5L,
                regParam( 0, WIR_Usage::use ),
                new TC_Const5_Unsigned( uConst( 1 ) ), lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::DC5L,
                regParam( 0, WIR_Usage::use ),
                new TC_Const5_Unsigned( uConst( 1 ) ),
                new WIR_LabelParameter( b ) } );
            labelStringParam( 2 );
          }
        } else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             ( a[ 0 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
             a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
             ( ( o == TC13::OpCode::JNZ_T ) || ( o == TC13::OpCode::JZ_T ) ) ) {
          auto *lParam = labelParam( 2 );
          if ( lParam != nullptr )
            pbOp(
              { o, TC13::OperationFormat::SIC5L,
                regParam( 0, WIR_Usage::use ),
                new TC_Const5_Unsigned( uConst( 1 ) ), lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::SIC5L,
                regParam( 0, WIR_Usage::use ),
                new TC_Const5_Unsigned( uConst( 1 ) ),
                new WIR_LabelParameter( b ) } );
            labelStringParam( 2 );
          }
        }

      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DISP ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::JGE ) || ( o == TC13::OpCode::JGE_U ) ||
               ( o == TC13::OpCode::JLT ) || ( o == TC13::OpCode::JLT_U ) ||
               ( o == TC13::OpCode::JEQ ) || ( o == TC13::OpCode::JNE ) ) ) {
          auto *lParam = labelParam( 2 );
          if ( lParam != nullptr )
            pbOp(
              { o, TC13::OperationFormat::DDL_1,
                regParam( 0, WIR_Usage::use ), regParam( 1, WIR_Usage::use ),
                lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::DDL_1,
                regParam( 0, WIR_Usage::use ), regParam( 1, WIR_Usage::use ),
                new WIR_LabelParameter( b ) } );
            labelStringParam( 2 );
          }
        } else

        if ( ( w == TC_AsmOperationWidth::CODE16 ) &&
             ( a[ 0 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
             ( ( o == TC13::OpCode::JEQ ) || ( o == TC13::OpCode::JNE ) ) ) {
          auto *lParam = labelParam( 2 );
          if ( lParam != nullptr )
            pbOp(
              { o, TC13::OperationFormat::SIDL,
                regParam( 0, WIR_Usage::use ), regParam( 1, WIR_Usage::use ),
                lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::SIDL,
                regParam( 0, WIR_Usage::use ), regParam( 1, WIR_Usage::use ),
                new WIR_LabelParameter( b ) } );
            labelStringParam( 2 );
          }
        } else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             ( ( o == TC13::OpCode::JNED ) || ( o == TC13::OpCode::JNEI ) ) ) {
          auto *lParam = labelParam( 2 );
          if ( lParam != nullptr )
            pbOp(
              { o, TC13::OperationFormat::DDL_2,
                regParam( 0, WIR_Usage::defuse ), regParam( 1, WIR_Usage::use ),
                lParam } );
          else {
            // The desired label does not yet exist. So, generate a branch to
            // an incorrect destination, add the correct label as implicit
            // string parameter and try to resolve this issue later.
            pbOp(
              { o, TC13::OperationFormat::DDL_2,
                regParam( 0, WIR_Usage::defuse ), regParam( 1, WIR_Usage::use ),
                new WIR_LabelParameter( b ) } );
            labelStringParam( 2 );
          }
        }
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DISP ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( ( o == TC13::OpCode::JEQ_A ) || ( o == TC13::OpCode::JNE_A ) ) ) {
        auto *lParam = labelParam( 2 );
        if ( lParam != nullptr )
          pbOp(
            { o, TC13::OperationFormat::AAL,
              regParam( 0, WIR_Usage::use ), regParam( 1, WIR_Usage::use ),
              lParam } );
        else {
          // The desired label does not yet exist. So, generate a branch to
          // an incorrect destination, add the correct label as implicit
          // string parameter and try to resolve this issue later.
          pbOp(
            { o, TC13::OperationFormat::AAL,
              regParam( 0, WIR_Usage::use ), regParam( 1, WIR_Usage::use ),
              new WIR_LabelParameter( b ) } );
          labelStringParam( 2 );
        }
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) ) {
        if ( o == TC13::OpCode::MUL )
          pbOp(
            { o, TC13::OperationFormat::EDC9_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              new TC_Const9_Signed( sConst( 2 ) ) } );
        else

        if ( o == TC13::OpCode::MUL_U )
          pbOp(
            { o, TC13::OperationFormat::EDC9_2,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              new TC_Const9_Unsigned( uConst( 2 ) ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::PACK ) )
        pbOp(
          { o, TC13::OperationFormat::DEDPSW,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            regParam( 2, WIR_Usage::use ),
            new WIR_RegisterParameter( getPSW_C(), WIR_Usage::use ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AMODE_ABS ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST3 ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST1 ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::ST_T ) )
        pbOp(
          { o, TC13::OperationFormat::C18C3C1,
            new TC_Const18_Unsigned( uConst( 0 ) ),
            new TC_Const3_Unsigned( uConst( 1 ) ),
            new TC_Const1_Unsigned( uConst( 2 ) ) } );

      break;
    }

    case 4: {
      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::AREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST ) &&
           ( o == TC13::OpCode::ADDSC_A ) ) {
        if ( ( w != TC_AsmOperationWidth::CODE32 ) &&
             ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_D15 ) &&
             a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST2 ) )
          pbOp(
            { o, TC13::OperationFormat::SAAIC2,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ),
              new TC_Const2_Unsigned( uConst( 3 ) ) } );
        else

        if ( ( w != TC_AsmOperationWidth::CODE16 ) &&
             a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST2 ) )
          pbOp(
            { o, TC13::OperationFormat::AADC2,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ),
              new TC_Const2_Unsigned( uConst( 3 ) ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) ) {
        if ( a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) &&
             ( ( o == TC13::OpCode::CADD ) || ( o == TC13::OpCode::CADDN ) ||
               ( o == TC13::OpCode::SEL ) || ( o == TC13::OpCode::SELN ) ||
               ( o == TC13::OpCode::MADD ) || ( o == TC13::OpCode::MADDS ) ||
               ( o == TC13::OpCode::MSUB ) || ( o == TC13::OpCode::MSUBS ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDDC9_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ),
              new TC_Const9_Signed( sConst( 3 ) ) } );
        else

        if ( a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
             ( ( o == TC13::OpCode::DEXTR ) || ( o == TC13::OpCode::EXTR ) ||
               ( o == TC13::OpCode::EXTR_U ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDDC5,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ),
              new TC_Const5_Unsigned( uConst( 3 ) ) } );
        else

        if ( a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) &&
             ( ( o == TC13::OpCode::MADDS_U ) ||
               ( o == TC13::OpCode::MSUBS_U ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDDC9_2,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ),
              new TC_Const9_Unsigned( uConst( 3 ) ) } );
        else

        if ( a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST1 ) &&
             ( ( o == TC13::OpCode::MUL_Q ) ||
               ( o == TC13::OpCode::MULR_Q ) ) ) {
          if ( ( a[ 1 ]->getType() == TC_AsmArgument::Type::DREG_U ) &&
               ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_U ) )
            pbOp(
              { o, TC13::OperationFormat::DDDC1_9,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 3 ) ) } );
          else
          if ( ( a[ 1 ]->getType() == TC_AsmArgument::Type::DREG_L ) &&
               ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_L ) )
            pbOp(
              { o, TC13::OperationFormat::DDDC1_8,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 3 ) ) } );
          else
          if ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_U )
            pbOp(
              { o, TC13::OperationFormat::DDDC1_5,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 3 ) ) } );
          else
          if ( ( o == TC13::OpCode::MUL_Q ) &&
               ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_L ) )
            pbOp(
              { o, TC13::OperationFormat::DDDC1_2,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 3 ) ) } );
          else
          if ( o == TC13::OpCode::MUL_Q )
            pbOp(
              { o, TC13::OperationFormat::DDDC1_1,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 3 ) ) } );
        } else

        if ( a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST1 ) &&
             ( o == TC13::OpCode::MULR_H ) ) {
          if ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_LL )
            pbOp(
              { o, TC13::OperationFormat::DDDC1_3,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 3 ) ) } );
          else
          if ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_LU )
            pbOp(
              { o, TC13::OperationFormat::DDDC1_4,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 3 ) ) } );
          else
          if ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_UL )
            pbOp(
              { o, TC13::OperationFormat::DDDC1_6,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 3 ) ) } );
          else
          if ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_UU )
            pbOp(
              { o, TC13::OperationFormat::DDDC1_7,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 3 ) ) } );
        }
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( ( o == TC13::OpCode::CADD ) || ( o == TC13::OpCode::CADDN ) ||
             ( o == TC13::OpCode::CSUB ) || ( o == TC13::OpCode::CSUBN ) ||
             ( o == TC13::OpCode::DEXTR ) || ( o == TC13::OpCode::MADD_F ) ||
             ( o == TC13::OpCode::MSUB_F ) || ( o == TC13::OpCode::SEL ) ||
             ( o == TC13::OpCode::SELN ) || ( o == TC13::OpCode::MADD ) ||
             ( o == TC13::OpCode::MADDS ) || ( o == TC13::OpCode::MADDS_U ) ||
             ( o == TC13::OpCode::MSUB ) || ( o == TC13::OpCode::MSUBS ) ||
             ( o == TC13::OpCode::MSUBS_U ) ) )
        pbOp(
          { o, TC13::OperationFormat::DDDD,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( ( o == TC13::OpCode::EXTR ) || ( o == TC13::OpCode::EXTR_U ) ) )
        pbOp(
          { o, TC13::OperationFormat::DDC5C5,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            new TC_Const5_Unsigned( uConst( 2 ) ),
            new TC_Const5_Unsigned( uConst( 3 ) ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::IMASK ) )
        pbOp(
          { o, TC13::OperationFormat::EC4C5C5,
            regParam( 0, WIR_Usage::def ),
            new TC_Const4_Unsigned( uConst( 1 ) ),
            new TC_Const5_Unsigned( uConst( 2 ) ),
            new TC_Const5_Unsigned( uConst( 3 ) ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::IMASK ) )
        pbOp(
          { o, TC13::OperationFormat::EC4DC5,
            regParam( 0, WIR_Usage::def ),
            new TC_Const4_Unsigned( uConst( 1 ) ),
            regParam( 2, WIR_Usage::use ),
            new TC_Const5_Unsigned( uConst( 3 ) ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::IMASK ) )
        pbOp(
          { o, TC13::OperationFormat::EDC5C5,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            new TC_Const5_Unsigned( uConst( 2 ) ),
            new TC_Const5_Unsigned( uConst( 3 ) ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) ) {
        if ( a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
             ( o == TC13::OpCode::IMASK ) )
          pbOp(
            { o, TC13::OperationFormat::EDDC5,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ),
              new TC_Const5_Unsigned( uConst( 3 ) ) } );
        else

        if ( a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST1 ) &&
             ( ( o == TC13::OpCode::MUL_H ) ||
               ( o == TC13::OpCode::MULM_H ) ) ) {
            if ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_LL )
              pbOp(
                { o, TC13::OperationFormat::EDDC1_3,
                  regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                  regParam( 2, WIR_Usage::use ),
                  new TC_Const1_Unsigned( uConst( 3 ) ) } );
            else
            if ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_LU )
              pbOp(
                { o, TC13::OperationFormat::EDDC1_4,
                  regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                  regParam( 2, WIR_Usage::use ),
                  new TC_Const1_Unsigned( uConst( 3 ) ) } );
            else
            if ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_UL )
              pbOp(
                { o, TC13::OperationFormat::EDDC1_6,
                  regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                  regParam( 2, WIR_Usage::use ),
                  new TC_Const1_Unsigned( uConst( 3 ) ) } );
            else
            if ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_UU )
              pbOp(
                { o, TC13::OperationFormat::EDDC1_7,
                  regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                  regParam( 2, WIR_Usage::use ),
                  new TC_Const1_Unsigned( uConst( 3 ) ) } );
        } else

        if ( a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST1 ) &&
             ( o == TC13::OpCode::MUL_Q ) ) {
          if ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_U )
            pbOp(
              { o, TC13::OperationFormat::EDDC1_5,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 3 ) ) } );
          else
          if ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_L )
            pbOp(
              { o, TC13::OperationFormat::EDDC1_2,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 3 ) ) } );
          else
            pbOp(
              { o, TC13::OperationFormat::EDDC1_1,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 3 ) ) } );
        }
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::INSERT ) )
        pbOp(
          { o, TC13::OperationFormat::DDC4E,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            new TC_Const4_Unsigned( uConst( 2 ) ),
            regParam( 3, WIR_Usage::use ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::INSERT ) )
        pbOp(
          { o, TC13::OperationFormat::DDDE,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST9 ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) ) {
        if ( ( o == TC13::OpCode::MADD ) || ( o == TC13::OpCode::MADDS ) ||
             ( o == TC13::OpCode::MSUB ) || ( o == TC13::OpCode::MSUBS ) )
          pbOp(
            { o, TC13::OperationFormat::EEDC9_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ),
              new TC_Const9_Signed( sConst( 3 ) ) } );
        else

        if ( ( o == TC13::OpCode::MADD_U ) || ( o == TC13::OpCode::MSUB_U ) ||
             ( o == TC13::OpCode::MADDS_U ) || ( o == TC13::OpCode::MSUBS_U ) )
          pbOp(
            { o, TC13::OperationFormat::EEDC9_2,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ),
              new TC_Const9_Unsigned( uConst( 3 ) ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( ( o == TC13::OpCode::MADD ) || ( o == TC13::OpCode::MADDS ) ||
             ( o == TC13::OpCode::MADDS_U ) || ( o == TC13::OpCode::MSUB ) ||
             ( o == TC13::OpCode::MSUBS ) || ( o == TC13::OpCode::MSUBS_U ) ||
             ( o == TC13::OpCode::MADD_U ) || ( o == TC13::OpCode::MSUB_U ) ) )
        pbOp(
          { o, TC13::OperationFormat::EEDD,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ) } );

      break;
    }

    case 5: {
      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 4 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) ) {
        if ( a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
             ( ( o == TC13::OpCode::AND_AND_T ) ||
               ( o == TC13::OpCode::AND_ANDN_T ) ||
               ( o == TC13::OpCode::AND_NOR_T ) ||
               ( o == TC13::OpCode::AND_OR_T ) ||
               ( o == TC13::OpCode::OR_AND_T ) ||
               ( o == TC13::OpCode::OR_ANDN_T ) ||
               ( o == TC13::OpCode::OR_NOR_T ) ||
               ( o == TC13::OpCode::OR_OR_T ) ||
               ( o == TC13::OpCode::SH_AND_T ) ||
               ( o == TC13::OpCode::SH_ANDN_T ) ||
               ( o == TC13::OpCode::SH_NAND_T ) ||
               ( o == TC13::OpCode::SH_NOR_T ) ||
               ( o == TC13::OpCode::SH_OR_T ) ||
               ( o == TC13::OpCode::SH_ORN_T ) ||
               ( o == TC13::OpCode::SH_XNOR_T ) ||
               ( o == TC13::OpCode::SH_XOR_T ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDC5DC5_2,
              regParam( 0, WIR_Usage::defuse ), regParam( 1, WIR_Usage::use ),
              new TC_Const5_Unsigned( uConst( 2 ) ),
              regParam( 3, WIR_Usage::use ),
              new TC_Const5_Unsigned( uConst( 4 ) ) } );
        else

        if ( a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
             ( ( o == TC13::OpCode::AND_T ) || ( o == TC13::OpCode::ANDN_T ) ||
               ( o == TC13::OpCode::INS_T ) || ( o == TC13::OpCode::INSN_T ) ||
               ( o == TC13::OpCode::NAND_T ) || ( o == TC13::OpCode::NOR_T ) ||
               ( o == TC13::OpCode::OR_T ) || ( o == TC13::OpCode::ORN_T ) ||
               ( o == TC13::OpCode::XNOR_T ) || ( o == TC13::OpCode::XOR_T ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDC5DC5_1,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              new TC_Const5_Unsigned( uConst( 2 ) ),
              regParam( 3, WIR_Usage::use ),
              new TC_Const5_Unsigned( uConst( 4 ) ) } );
        else

        if ( a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
             ( o == TC13::OpCode::INSERT ) )
          pbOp(
            { o, TC13::OperationFormat::DDC4DC5,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              new TC_Const4_Unsigned( uConst( 2 ) ),
              regParam( 3, WIR_Usage::use ),
              new TC_Const5_Unsigned( uConst( 4 ) ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::CONST4 ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
           a[ 4 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::INSERT ) )
        pbOp(
          { o, TC13::OperationFormat::DDC4C5C5,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            new TC_Const4_Unsigned( uConst( 2 ) ),
            new TC_Const5_Unsigned( uConst( 3 ) ),
            new TC_Const5_Unsigned( uConst( 4 ) ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
           a[ 4 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( o == TC13::OpCode::INSERT ) )
        pbOp(
          { o, TC13::OperationFormat::DDDC5C5,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            regParam( 2, WIR_Usage::use ),
            new TC_Const5_Unsigned( uConst( 3 ) ),
            new TC_Const5_Unsigned( uConst( 4 ) ) } );
      else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 4 ]->isCompatible( TC_AsmArgument::Type::CONST ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) ) {
        if ( a[ 4 ]->isCompatible( TC_AsmArgument::Type::CONST5 ) &&
             ( o == TC13::OpCode::INSERT ) )
          pbOp(
            { o, TC13::OperationFormat::DDDDC5,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
              new TC_Const5_Unsigned( uConst( 4 ) ) } );
        else

        if ( a[ 4 ]->isCompatible( TC_AsmArgument::Type::CONST1 ) &&
             ( ( o == TC13::OpCode::MADD_Q ) ||
               ( o == TC13::OpCode::MADDS_Q ) ||
               ( o == TC13::OpCode::MSUB_Q ) ||
               ( o == TC13::OpCode::MSUBS_Q ) ) ) {
          if ( ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_L ) &&
               ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_L ) )
            pbOp(
              { o, TC13::OperationFormat::DDDDC1_8,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
          if ( ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_U ) &&
               ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_U ) )
            pbOp(
              { o, TC13::OperationFormat::DDDDC1_9,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
          if ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_L )
            pbOp(
              { o, TC13::OperationFormat::DDDDC1_2,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
          if ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_U )
            pbOp(
              { o, TC13::OperationFormat::DDDDC1_5,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
            pbOp(
              { o, TC13::OperationFormat::DDDDC1_1,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
        } else

        if ( a[ 4 ]->isCompatible( TC_AsmArgument::Type::CONST1 ) &&
             ( ( o == TC13::OpCode::MADDR_H ) ||
               ( o == TC13::OpCode::MADDRS_H ) ||
               ( o == TC13::OpCode::MADDSUR_H ) ||
               ( o == TC13::OpCode::MADDSURS_H ) ||
               ( o == TC13::OpCode::MSUBADR_H ) ||
               ( o == TC13::OpCode::MSUBADRS_H ) ||
               ( o == TC13::OpCode::MSUBR_H ) ||
               ( o == TC13::OpCode::MSUBRS_H ) ) ) {
          if ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_LL )
            pbOp(
              { o, TC13::OperationFormat::DDDDC1_3,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
          if ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_LU )
            pbOp(
              { o, TC13::OperationFormat::DDDDC1_4,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
          if ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_UL )
            pbOp(
              { o, TC13::OperationFormat::DDDDC1_6,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
          if ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_UU )
            pbOp(
              { o, TC13::OperationFormat::DDDDC1_7,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
        } else

        if ( ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_L ) &&
             ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_L ) &&
             ( a[ 4 ]->getType() == TC_AsmArgument::Type::CONST1 ) &&
             ( ( o == TC13::OpCode::MADDR_Q ) ||
               ( o == TC13::OpCode::MADDRS_Q ) ||
               ( o == TC13::OpCode::MSUBR_Q ) ||
               ( o == TC13::OpCode::MSUBRS_Q ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDDDC1_8,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
              new TC_Const1_Unsigned( uConst( 4 ) ) } );
        else

        if ( ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_U ) &&
             ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_U ) &&
             ( a[ 4 ]->getType() == TC_AsmArgument::Type::CONST1 ) &&
             ( ( o == TC13::OpCode::MADDR_Q ) ||
               ( o == TC13::OpCode::MADDRS_Q ) ||
               ( o == TC13::OpCode::MSUBR_Q ) ||
               ( o == TC13::OpCode::MSUBRS_Q ) ) )
          pbOp(
            { o, TC13::OperationFormat::DDDDC1_9,
              regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
              regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
              new TC_Const1_Unsigned( uConst( 4 ) ) } );
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 3 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 4 ]->isCompatible( TC_AsmArgument::Type::CONST ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           a[ 4 ]->isCompatible( TC_AsmArgument::Type::CONST1 ) ) {
        if ( ( o == TC13::OpCode::MADD_H ) || ( o == TC13::OpCode::MADDM_H ) ||
             ( o == TC13::OpCode::MADDMS_H ) ||
             ( o == TC13::OpCode::MADDS_H ) ||
             ( o == TC13::OpCode::MADDSU_H ) ||
             ( o == TC13::OpCode::MADDSUM_H ) ||
             ( o == TC13::OpCode::MADDSUMS_H ) ||
             ( o == TC13::OpCode::MADDSUS_H ) ||
             ( o == TC13::OpCode::MSUB_H ) || ( o == TC13::OpCode::MSUBAD_H ) ||
             ( o == TC13::OpCode::MSUBADM_H ) ||
             ( o == TC13::OpCode::MSUBADMS_H ) ||
             ( o == TC13::OpCode::MSUBADS_H ) ||
             ( o == TC13::OpCode::MSUBM_H ) ||
             ( o == TC13::OpCode::MSUBMS_H ) ||
             ( o == TC13::OpCode::MSUBS_H ) ) {
          if ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_LL )
            pbOp(
              { o, TC13::OperationFormat::EEDDC1_3,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
          if ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_LU )
            pbOp(
              { o, TC13::OperationFormat::EEDDC1_4,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
          if ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_UL )
            pbOp(
              { o, TC13::OperationFormat::EEDDC1_6,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
          if ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_UU )
            pbOp(
              { o, TC13::OperationFormat::EEDDC1_7,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
        } else

        if ( ( o == TC13::OpCode::MADD_Q ) || ( o == TC13::OpCode::MADDS_Q ) ||
             ( o == TC13::OpCode::MSUB_Q ) || ( o == TC13::OpCode::MSUBS_Q ) ) {
          if ( ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_L ) &&
               ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_L ) )
            pbOp(
              { o, TC13::OperationFormat::EEDDC1_8,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
          if ( ( a[ 2 ]->getType() == TC_AsmArgument::Type::DREG_U ) &&
               ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_U ) )
            pbOp(
              { o, TC13::OperationFormat::EEDDC1_9,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
          if ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_L )
            pbOp(
              { o, TC13::OperationFormat::EEDDC1_2,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
          if ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_U )
            pbOp(
              { o, TC13::OperationFormat::EEDDC1_5,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
          else
            pbOp(
              { o, TC13::OperationFormat::EEDDC1_1,
                regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
                regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
                new TC_Const1_Unsigned( uConst( 4 ) ) } );
        }
      } else

      if ( a[ 0 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           a[ 1 ]->isCompatible( TC_AsmArgument::Type::EREG ) &&
           a[ 2 ]->isCompatible( TC_AsmArgument::Type::DREG ) &&
           ( a[ 3 ]->getType() == TC_AsmArgument::Type::DREG_UL ) &&
           ( a[ 4 ]->getType() == TC_AsmArgument::Type::CONST1 ) &&
           ( w != TC_AsmOperationWidth::CODE16 ) &&
           ( ( o == TC13::OpCode::MADDR_H ) ||
             ( o == TC13::OpCode::MADDRS_H ) ||
             ( o == TC13::OpCode::MSUBR_H ) ||
             ( o == TC13::OpCode::MSUBRS_H ) ) )
        pbOp(
          { o, TC13::OperationFormat::DEDDC1,
            regParam( 0, WIR_Usage::def ), regParam( 1, WIR_Usage::use ),
            regParam( 2, WIR_Usage::use ), regParam( 3, WIR_Usage::use ),
            new TC_Const1_Unsigned( uConst( 4 ) ) } );

      break;
    }

    default:
      break;

  }

  for ( auto *p : a )
    delete( p );

  // Start a new basic block if o modifies the control flow.
  if ( res ) {
    WIR_Operation &op = b.rbegin()->get().begin()->get();
    if ( op.isCall() || op.isIndirectCall() || op.isReturn() ||
        op.isConditionalJump() || op.isUnconditionalJump() ||
        op.isIndirectJump() ) {
      auto &b = mFunction.insertBasicBlock( mIt, {} )->get();
      mBasicBlocks.push_back( b );
    }
  }

  WIR_Operation::resetParameterChecking();

  return( res );
};


/*
  insertBasicBlock inserts a new basic block and associates it with the given
  label name.
*/
void TC_AsmContext::insertBasicBlock( const std::string &n )
{
  DSTART( "void TC_AsmContext::insertBasicBlock(const string&)" );

  // Create the new WIR basic block.
  auto &b = mFunction.insertBasicBlock( mIt, {} )->get();
  mBasicBlocks.push_back( b );

  // Associate the new WIR basic block with the label name.
  mBlockOfLabel.insert( { n, b } );
};


/*
  getProcessor returns the current TriCore processor.
*/
TC13 &TC_AsmContext::getProcessor( void ) const
{
  DSTART( "TC13& TC_AsmContext::getProcessor() const" );

  return( mProcessor );
};


/*
  getBasicBlocks returns the list of basic blocks modified/created by the
  parser.
*/
const std::list<std::reference_wrapper<WIR_BasicBlock>> &TC_AsmContext::getBasicBlocks( void ) const
{
  DSTART( "const list<reference_wrapper<WIR_BasicBlock> >& TC_AsmContext::getBasicBlocks() const" );

  return( mBasicBlocks );
};


/*
  getARegP determines the physical address register with the specified number.
*/
const TC_ARegP &TC_AsmContext::getARegP( int n ) const
{
  DSTART( "const TC_ARegP& TC_AsmContext::getARegP(int) const" );

  ufAssert( ( n >= 0 ) && ( n <= 15 ) );

  if ( n == 0 )
    return( mProcessor.A0() );
  else
  if ( n == 1 )
    return( mProcessor.A1() );
  else
  if ( n == 2 )
    return( mProcessor.A2() );
  else
  if ( n == 3 )
    return( mProcessor.A3() );
  else
  if ( n == 4 )
    return( mProcessor.A4() );
  else
  if ( n == 5 )
    return( mProcessor.A5() );
  else
  if ( n == 6 )
    return( mProcessor.A6() );
  else
  if ( n == 7 )
    return( mProcessor.A7() );
  else
  if ( n == 8 )
    return( mProcessor.A8() );
  else
  if ( n == 9 )
    return( mProcessor.A9() );
  else
  if ( n == 10 )
    return( mProcessor.A10() );
  else
  if ( n == 11 )
    return( mProcessor.A11() );
  else
  if ( n == 12 )
    return( mProcessor.A12() );
  else
  if ( n == 13 )
    return( mProcessor.A13() );
  else
  if ( n == 14 )
    return( mProcessor.A14() );
  else
    return( mProcessor.A15() );
};


/*
  getDRegP determines the physical data register with the specified number.
*/
const TC_DRegP &TC_AsmContext::getDRegP( int n ) const
{
  DSTART( "const TC_DRegP& TC_AsmContext::getDRegP(int) const" );

  ufAssert( ( n >= 0 ) && ( n <= 15 ) );

  if ( n == 0 )
    return( mProcessor.D0() );
  else
  if ( n == 1 )
    return( mProcessor.D1() );
  else
  if ( n == 2 )
    return( mProcessor.D2() );
  else
  if ( n == 3 )
    return( mProcessor.D3() );
  else
  if ( n == 4 )
    return( mProcessor.D4() );
  else
  if ( n == 5 )
    return( mProcessor.D5() );
  else
  if ( n == 6 )
    return( mProcessor.D6() );
  else
  if ( n == 7 )
    return( mProcessor.D7() );
  else
  if ( n == 8 )
    return( mProcessor.D8() );
  else
  if ( n == 9 )
    return( mProcessor.D9() );
  else
  if ( n == 10 )
    return( mProcessor.D10() );
  else
  if ( n == 11 )
    return( mProcessor.D11() );
  else
  if ( n == 12 )
    return( mProcessor.D12() );
  else
  if ( n == 13 )
    return( mProcessor.D13() );
  else
  if ( n == 14 )
    return( mProcessor.D14() );
  else
    return( mProcessor.D15() );
};


/*
  getERegP determines the physical extended data register with the specified
  number.
*/
const TC_ERegP &TC_AsmContext::getERegP( int n ) const
{
  DSTART( "const TC_ERegP& TC_AsmContext::getERegP(int) const" );

  ufAssert( ( n >= 0 ) && ( n <= 15 ) && ( n % 2 == 0 ) );

  if ( n == 0 )
    return( mProcessor.E0() );
  else
  if ( n == 2 )
    return( mProcessor.E2() );
  else
  if ( n == 4 )
    return( mProcessor.E4() );
  else
  if ( n == 6 )
    return( mProcessor.E6() );
  else
  if ( n == 8 )
    return( mProcessor.E8() );
  else
  if ( n == 10 )
    return( mProcessor.E10() );
  else
  if ( n == 12 )
    return( mProcessor.E12() );
  else
    return( mProcessor.E14() );
};


/*
  getPRegP determines the physical extended address register with the specified
  number.
*/
const TC_PRegP &TC_AsmContext::getPRegP( int n ) const
{
  DSTART( "const TC_PRegP& TC_AsmContext::getPRegP(int) const" );

  ufAssert( ( n >= 0 ) && ( n <= 15 ) && ( n % 2 == 0 ) );

  if ( n == 0 )
    return( mProcessor.P0() );
  else
  if ( n == 2 )
    return( mProcessor.P2() );
  else
  if ( n == 4 )
    return( mProcessor.P4() );
  else
  if ( n == 6 )
    return( mProcessor.P6() );
  else
  if ( n == 8 )
    return( mProcessor.P8() );
  else
  if ( n == 10 )
    return( mProcessor.P10() );
  else
  if ( n == 12 )
    return( mProcessor.P12() );
  else
    return( mProcessor.P14() );
};


/*
  getPSW_C determines the physical carry bit of the Processor Status word.
*/
const TC_PSWBit &TC_AsmContext::getPSW_C( void ) const
{
  DSTART( "const TC_PSWBit& TC_AsmContext::getPSW_C() const" );

  return( mProcessor.PSW_C() );
};


/*
  setError sets a new error message along with its location.
*/
void TC_AsmContext::setError( const location &l, const std::string &m )
{
  DSTART( "void TC_AsmContext::setError(const location&, const string&)" );

  mErrLocation = l;

  // Some little beautifications of error messages.
  mErrMessage = m;
  replaceAll( mErrMessage, "COMMA", "','" );
  replaceAll( mErrMessage, "SEMICOLON", "';'" );
  replaceAll( mErrMessage, "COLON", "':'" );
  replaceAll( mErrMessage, "NEWLINE", "new line" );
  replaceAll( mErrMessage, "BREV", "'+r'" );
  replaceAll( mErrMessage, "CIRC", "'+c'" );
  replaceAll( mErrMessage, "PLUS", "'+'" );
  replaceAll( mErrMessage, "LBRACKET", "'['" );
  replaceAll( mErrMessage, "RBRACKET", "']'" );
  replaceAll( mErrMessage, "HI", "'HI:'" );
  replaceAll( mErrMessage, "LO", "'LO:'" );
  replaceAll( mErrMessage, "CODE16", "'.code16'" );
  replaceAll( mErrMessage, "CODE32", "'.code32'" );
  replaceAll( mErrMessage, "MNEMONIC", "opcode" );
  replaceAll( mErrMessage, "TPLARGMODA", "'%A' template argument" );
  replaceAll( mErrMessage, "TPLARGMODH", "'%H' template argument" );
  replaceAll( mErrMessage, "TPLARGMODL", "'%L' template argument" );
  replaceAll( mErrMessage, "STRING", "string literal" );
  replaceAll( mErrMessage, "AREG10", "'%a10'" );
  replaceAll( mErrMessage, "AREG15", "'%a15'" );
  replaceAll( mErrMessage, "AREG", "address register" );
  replaceAll( mErrMessage, "DREG15", "'%d15'" );
  replaceAll( mErrMessage, "DREGUU", "data register (UU)" );
  replaceAll( mErrMessage, "DREGUL", "data register (UL)" );
  replaceAll( mErrMessage, "DREGU", "data register (U)" );
  replaceAll( mErrMessage, "DREGLU", "data register (LU)" );
  replaceAll( mErrMessage, "DREGLL", "data register (LL)" );
  replaceAll( mErrMessage, "DREGL", "data register (L)" );
  replaceAll( mErrMessage, "DREG", "data register" );
  replaceAll( mErrMessage, "EREG", "extended register" );
  replaceAll( mErrMessage, "IMMEDIATE_S", "signed immediate value" );
  replaceAll( mErrMessage, "IMMEDIATE_U", "unsigned immediate value" );
  replaceAll( mErrMessage, "TPLARGUU", "template argument (UU)" );
  replaceAll( mErrMessage, "TPLARGUL", "template argument (UL)" );
  replaceAll( mErrMessage, "TPLARGU", "template argument (U)" );
  replaceAll( mErrMessage, "TPLARGLU", "template argument (LU)" );
  replaceAll( mErrMessage, "TPLARGLL", "template argument (LL)" );
  replaceAll( mErrMessage, "TPLARGL", "template argument (L)" );
  replaceAll( mErrMessage, "TPLARG", "template argument" );
};


/*
  getErrMessage returns the current error message.
*/
std::string TC_AsmContext::getErrMessage( void ) const
{
  DSTART( "string TC_AsmContext::getErrMessage() const" );

  return( mErrMessage );
};


/*
  getErrLocation returns the current error location.
*/
location TC_AsmContext::getErrLocation( void ) const
{
  DSTART( "location TC_AsmContext::getErrLocation() const" );

  return( mErrLocation );
};


/*
  setGenerate16BitOperations sets whether an optimization shall generate 16 bits
  wide operations or not.
*/
void TC_AsmContext::setGenerate16BitOperations( bool f )
{
  DSTART( "void TC_AsmContext::setGenerate16BitOperations(bool)" );

  m16BitOperations = f;
};


/*
  getGenerate16BitOperations returns whether an optimization shall generate 16
  bits wide operations or not.
*/
bool TC_AsmContext::getGenerate16BitOperations( void ) const
{
  DSTART( "bool TC_AsmContext::getGenerate16BitOperations() const" );

  return( m16BitOperations );
};


//
// Private class methods
//

/*
  Default constructor creating a container for the specified assembly template
  arguments and basic block.
*/
TC_AsmContext::TC_AsmContext( const std::vector<std::unique_ptr<TC_AsmArgument>> &tArgs,
                              WIR_BasicBlock &b ) :
  mTemplateArguments { tArgs },
  mBasicBlocks { b },
  mFunction { b.getFunction() },
  mProcessor {
    dynamic_cast<TC13 &>(
      b.getFunction().getCompilationUnit().getSystem().findSymbol(
        b.getFunction() ).getSection().getProcessor() ) },
  m16BitOperations { true }
{
  DSTART( "TC_AsmContext::TC_AsmContext(const vector<unique_ptr<TC_AsmArgument> >&, WIR_BasicBlock&)" );

  auto it = mFunction.findBasicBlock( b );
  mIt = ++it;
};


/*
  check16BitOperation checks whether an operation is 16 bits wide and whether
  such operations shall actually be generated.

  If the parser is about to actually generate a 16 bits wide operation while
  this is not wanted, a warning is emitted.
*/
void TC_AsmContext::check16BitOperation( const WIR_Operation &o ) const
{
  DSTART(
    "void TC_AsmContext::check16BitOperation(const WIR_Operation&) const" );

  if ( !m16BitOperations && ( o.getSize() == 2 ) ) {
    stringstream str;
    str << tricore << o;

    ufWarnMsg << "Found 16-bit operation in inline assembly despite them being "
              << "deactivated: '" + trim( str.str() ) + "'." << endl;
  }
};

}       // namespace WIR
