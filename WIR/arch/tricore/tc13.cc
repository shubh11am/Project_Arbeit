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
  @file tc13.cc
  @brief This file implements the specific interface of the Infineon TriCore
         V1.3 architecture.

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
#include <arch/tricore/tc13.h>
#include <arch/tricore/tc131.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for TC13 processor architectures.
*/
TC13::TC13( void ) :
  WIR_Processor<TC13> {}
{
  DSTART( "TC13::TC13()" );

  // Specify the processor architecture modeled by this class.
  setProcessorName( "Infineon TriCore" );
  setISAName( "TC1.3" );

  // Create physical registers bottom-up, i.e., from the leaf registers upwards
  // towards complex hierarchical registers.
  // For the meaning of the individual registers, please refer to file
  // doc/TriCore_131_Core_Architecture.pdf, page 3-2.
  addPhReg<TC_ARegP>( "0" );
  addPhReg<TC_ARegP>( "1" );
  addPhReg<TC_ARegP>( "2" );
  addPhReg<TC_ARegP>( "3" );
  addPhReg<TC_ARegP>( "4" );
  addPhReg<TC_ARegP>( "5" );
  addPhReg<TC_ARegP>( "6" );
  addPhReg<TC_ARegP>( "7" );
  addPhReg<TC_ARegP>( "8" );
  addPhReg<TC_ARegP>( "9" );
  addPhReg<TC_ARegP>( "10", true );
  addPhReg<TC_ARegP>( "11" );
  addPhReg<TC_ARegP>( "12" );
  addPhReg<TC_ARegP>( "13" );
  addPhReg<TC_ARegP>( "14" );
  addPhReg<TC_ARegP>( "15" );
  addPhReg<TC_DRegP>( "0" );
  addPhReg<TC_DRegP>( "1" );
  addPhReg<TC_DRegP>( "2" );
  addPhReg<TC_DRegP>( "3" );
  addPhReg<TC_DRegP>( "4" );
  addPhReg<TC_DRegP>( "5" );
  addPhReg<TC_DRegP>( "6" );
  addPhReg<TC_DRegP>( "7" );
  addPhReg<TC_DRegP>( "8" );
  addPhReg<TC_DRegP>( "9" );
  addPhReg<TC_DRegP>( "10" );
  addPhReg<TC_DRegP>( "11" );
  addPhReg<TC_DRegP>( "12" );
  addPhReg<TC_DRegP>( "13" );
  addPhReg<TC_DRegP>( "14" );
  addPhReg<TC_DRegP>( "15" );

  addPhReg<TC_ERegP>( "0" );
  dynamic_cast<TC_ERegP &>(
    mPhRegReferences.back().get() ).addChilds( D0(), D1() );

  addPhReg<TC_ERegP>( "2" );
  dynamic_cast<TC_ERegP &>(
    mPhRegReferences.back().get() ).addChilds( D2(), D3() );

  addPhReg<TC_ERegP>( "4" );
  dynamic_cast<TC_ERegP &>(
    mPhRegReferences.back().get() ).addChilds( D4(), D5() );

  addPhReg<TC_ERegP>( "6" );
  dynamic_cast<TC_ERegP &>(
    mPhRegReferences.back().get() ).addChilds( D6(), D7() );

  addPhReg<TC_ERegP>( "8" );
  dynamic_cast<TC_ERegP &>(
    mPhRegReferences.back().get() ).addChilds( D8(), D9() );

  addPhReg<TC_ERegP>( "10" );
  dynamic_cast<TC_ERegP &>(
    mPhRegReferences.back().get() ).addChilds( D10(), D11() );

  addPhReg<TC_ERegP>( "12" );
  dynamic_cast<TC_ERegP &>(
    mPhRegReferences.back().get() ).addChilds( D12(), D13() );

  addPhReg<TC_ERegP>( "14" );
  dynamic_cast<TC_ERegP &>(
    mPhRegReferences.back().get() ).addChilds( D14(), D15() );

  addPhReg<TC_PRegP>( "0" );
  dynamic_cast<TC_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( A0(), A1() );

  addPhReg<TC_PRegP>( "2" );
  dynamic_cast<TC_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( A2(), A3() );

  addPhReg<TC_PRegP>( "4" );
  dynamic_cast<TC_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( A4(), A5() );

  addPhReg<TC_PRegP>( "6" );
  dynamic_cast<TC_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( A6(), A7() );

  addPhReg<TC_PRegP>( "8" );
  dynamic_cast<TC_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( A8(), A9() );

  addPhReg<TC_PRegP>( "10" );
  dynamic_cast<TC_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( A10(), A11() );

  addPhReg<TC_PRegP>( "12" );
  dynamic_cast<TC_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( A12(), A13() );

  addPhReg<TC_PRegP>( "14" );
  dynamic_cast<TC_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( A14(), A15() );

  addPhReg<TC_PSWBit>( "C" );
};


/*
  Copy constructor.
*/
TC13::TC13( const TC13 &__o ) :
  WIR_Processor<TC13> { __o }
{
  DSTART( "TC13::TC13(const TC13&)" );
};


/*
  Move constructor.
*/
TC13::TC13( TC13 &&__o ) :
  WIR_Processor<TC13> { move( __o ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
TC13::~TC13( void )
{
  DSTART( "virtual TC13::~TC13()" );
};


/*
  Copy-assignment operator.
*/
TC13 & TC13::operator = ( const TC13 &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Processor<TC13>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
TC13 & TC13::operator = ( TC13 &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Processor<TC13>::operator = ( move( __o ) );

  return( *this );
};


/*
  init performs some global initialization tasks for TC13 processor
  architectures.

  This includes setting up the TC13's machine operation formats and the
  assignment of valid operation formats to TC13 opcodes.

  init shall be called globally by WIR_Init(). It shall only perform tasks that
  cannot be expressed as initializations of static class members (since the
  order of static initialization is unspecified in C++) and that thus require
  execution by active code.
*/
void TC13::init( void )
{
  DSTART( "static void TC13::init()" );

  //
  // Register TC13 I/O functions.
  //

  WIR_Registry::registerBasicBlockDumper(
    getProcessorTypeID(), dumpTCBasicBlock );
  WIR_Registry::registerCompilationUnitDumper(
    getProcessorTypeID(), dumpTCCompilationUnit );
  WIR_Registry::registerDataDumper( getProcessorTypeID(), dumpTCData );
  WIR_Registry::registerDataSectionDumper(
    getProcessorTypeID(), dumpTCDataSection );
  WIR_Registry::registerFunctionDumper( getProcessorTypeID(), dumpTCFunction );
  WIR_Registry::registerLdScriptDumper( getProcessorTypeID(), dumpTCLdScript );
  WIR_Registry::registerLdScriptSectionDumper(
    getProcessorTypeID(), dumpTCLdScriptSection );
  WIR_Registry::registerOperationDumper(
    getProcessorTypeID(), dumpTCOperation );
  WIR_Registry::registerRegisterParameterDumper(
    getProcessorTypeID(), dumpTCRegisterParameter );
  WIR_Registry::registerCommentDumper( getProcessorTypeID(), dumpTCComment );
  WIR_Registry::registerFileInfoDumper( getProcessorTypeID(), dumpTCFileInfo );


  //
  // TC13 operation formats.
  //

  WIR_BasicBlock b;
  TC_ARegV *aregV = new TC_ARegV;
  TC_DRegV *dregV = new TC_DRegV;
  TC_ERegV *eregV = new TC_ERegV;
  TC_PRegV *pregV = new TC_PRegV;
  TC_ARegP *areg10P = new TC_ARegP( "10" );
  TC_ARegP *areg15P = new TC_ARegP( "15" );
  TC_DRegP *dreg15P = new TC_DRegP( "15" );
  TC_PSWBit *pswC = new TC_PSWBit( "C" );

  WIR_Registry::registerOperationFormat(
    OperationFormat::A,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AAA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AAC10BOA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AAC10PIA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AAC16,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const16_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AAC16BOA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const16_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AAD,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AADC2,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const2_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AAL,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AALC16BOA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_LabelParameter( b ),
      new TC_Const16_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC10ABOA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC10APIA,
    { new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC10BOA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC10BOAPSW,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *pswC, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC10DBOA_1,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC10DBOA_2,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC10DPIA_1,
    { new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC10DPIA_2,
    { new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC10EBOA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC10EPIA,
    { new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC10PBOA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC10PIA,
    { new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC10PPIA,
    { new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC16,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC16DBOA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const16_Signed( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC18ABSA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new TC_Const18_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AD,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AL_1,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AL_2,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AL_3,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::ALABSA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::ALC16DBOA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_LabelParameter( b ),
      new TC_Const16_Signed( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::APBRA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::APC10CA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::C16DPSW,
    { new TC_Const16_Unsigned( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *pswC, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::C18AABSA,
    { new TC_Const18_Unsigned( 0 ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::C18ABSA,
    { new TC_Const18_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::C18ABSAPSW,
    { new TC_Const18_Unsigned( 0 ),
      new WIR_RegisterParameter( *pswC, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::C18C3C1,
    { new TC_Const18_Unsigned( 0 ),
      new TC_Const3_Unsigned( 0 ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::C18DABSA_1,
    { new TC_Const18_Unsigned( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::C18DABSA_2,
    { new TC_Const18_Unsigned( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::C18EABSA,
    { new TC_Const18_Unsigned( 0 ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::C18PABSA,
    { new TC_Const18_Unsigned( 0 ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::C9,
    { new TC_Const9_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::C9PSW,
    { new TC_Const9_Unsigned( 0 ),
      new WIR_RegisterParameter( *pswC, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::D,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DA,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DAA,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DAC10BOA,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DAC10PIA,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DAC16BOA,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const16_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DALC16BOA,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_LabelParameter( b ),
      new TC_Const16_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DC16_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DC16_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DC16PSW,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0 ),
      new WIR_RegisterParameter( *pswC, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DC18ABSA,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new TC_Const18_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DC4L_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const4_Signed( 0 ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DC4L_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const4_Unsigned( 0 ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DC4L_3,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ),
      new TC_Const4_Signed( 0 ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DC5L,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DD,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC16_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const16_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC16_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const16_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC4C5C5,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const4_Unsigned( 0 ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC4DC5,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const4_Unsigned( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC4E,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const4_Unsigned( 0 ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC5C5,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC5DC5_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC5DC5_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC9_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const9_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC9_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const9_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC9_3,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const9_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC9_4,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const9_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC9PSW_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const9_Signed( 0 ),
      new WIR_RegisterParameter( *pswC, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDC9PSW_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const9_Signed( 0 ),
      new WIR_RegisterParameter( *pswC, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDD_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDD_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDC1_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDC1_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDC1_3,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDC1_4,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDC1_5,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDC1_6,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDC1_7,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDC1_8,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDC1_9,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDC5,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDC5C5,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDC9_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const9_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDC9_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const9_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDD,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDDC1_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDDC1_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDDC1_3,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDDC1_4,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDDC1_5,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDDC1_6,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDDC1_7,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDDC1_8,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDDC1_9,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDDC5,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDE,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDPSW_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *pswC, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDDPSW_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *pswC, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDE,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDL_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DDL_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DEDPSW,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *pswC, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DEDDC1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DLABSA,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DPBRA,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::DPC10CA,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::E,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EAC10BOA,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EAC10PIA,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EC18ABSA,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new TC_Const18_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EC4C5C5,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new TC_Const4_Unsigned( 0 ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EC4DC5,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new TC_Const4_Unsigned( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::ED,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EDC5C5,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EDC9_1,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const9_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EDC9_2,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const9_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EDD,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EDDC1_1,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EDDC1_2,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EDDC1_3,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EDDC1_4,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EDDC1_5,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EDDC1_6,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EDDC1_7,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EDDC5,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EED,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EEDC9_1,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const9_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EEDC9_2,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const9_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EEDD,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EEDDC1_1,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EEDDC1_2,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EEDDC1_3,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EEDDC1_4,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EEDDC1_5,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EEDDC1_6,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EEDDC1_7,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EEDDC1_8,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EEDDC1_9,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const1_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::ELABSA,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EPBRA,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::EPC10CA,
    { new WIR_RegisterParameter( *eregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::L,
    { new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::LAABSA,
    { new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::LABSA,
    { new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::LABSAPSW,
    { new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( *pswC, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::LDABSA_1,
    { new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::LDABSA_2,
    { new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::LEABSA,
    { new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::LPABSA,
    { new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PABRA,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PAC10BOA,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PAC10PIA,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PBRA,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PC10ACA,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PC10CA,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PC10DCA_1,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PC10DCA_2,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PC10ECA,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PC10PCA,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PC18ABSA,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::def ),
      new TC_Const18_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PDBRA_1,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PDBRA_2,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PEBRA,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *eregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PLABSA,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::def ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PPBRA_1,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PPBRA_2,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PPC10CA,
    { new WIR_RegisterParameter( *pregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::PSW,
    { new WIR_RegisterParameter( *pswC, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::S, {} );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SA,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAA_1,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAA_2,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAA_3,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAA_4,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAA_5,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAA_6,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAAIC2,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dreg15P, WIR_Usage::use ),
      new TC_Const2_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAC4_1,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new TC_Const4_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAC4_2,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new TC_Const4_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAC4I_1,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const4_Unsigned( 0 ),
      new WIR_RegisterParameter( *areg15P, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAC4I_2,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const4_Unsigned( 0 ),
      new WIR_RegisterParameter( *dreg15P, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAD_1,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAD_2,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAD_3,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAIC4,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *areg15P, WIR_Usage::use ),
      new TC_Const4_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAL_1,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SAL_2,
    { new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SC8,
    { new TC_Const8_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SD,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDA_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDA_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDA_3,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDC4_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new TC_Const4_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDC4_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ),
      new TC_Const4_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDC4PSW,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ),
      new TC_Const4_Signed( 0 ),
      new WIR_RegisterParameter( *pswC, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDD_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDD_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDIC4_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *areg15P, WIR_Usage::use ),
      new TC_Const4_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDIC4_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dreg15P, WIR_Usage::use ),
      new TC_Const4_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDIC4_3,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *dreg15P, WIR_Usage::use ),
      new TC_Const4_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDID_1,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *dreg15P, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDID_2,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *dreg15P, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SDL,
    { new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SIAC4_1,
    { new WIR_RegisterParameter( *areg15P, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const4_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SIAC4_2,
    { new WIR_RegisterParameter( *dreg15P, WIR_Usage::def ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ),
      new TC_Const4_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SIC4A,
    { new WIR_RegisterParameter( *areg15P, WIR_Usage::use ),
      new TC_Const4_Unsigned( 0 ),
      new WIR_RegisterParameter( *aregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SIC4D,
    { new WIR_RegisterParameter( *areg15P, WIR_Usage::use ),
      new TC_Const4_Unsigned( 0 ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SIC4L,
    { new WIR_RegisterParameter( *dreg15P, WIR_Usage::use ),
      new TC_Const4_Signed( 0 ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SIC5L,
    { new WIR_RegisterParameter( *dreg15P, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SIC8_1,
    { new WIR_RegisterParameter( *dreg15P, WIR_Usage::def ),
      new TC_Const8_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SIC8_2,
    { new WIR_RegisterParameter( *dreg15P, WIR_Usage::defuse ),
      new TC_Const8_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SIDC4,
    { new WIR_RegisterParameter( *dreg15P, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new TC_Const4_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SIDD,
    { new WIR_RegisterParameter( *dreg15P, WIR_Usage::def ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SIDL,
    { new WIR_RegisterParameter( *dreg15P, WIR_Usage::use ),
      new WIR_RegisterParameter( *dregV, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SIL,
    { new WIR_RegisterParameter( *dreg15P, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SISPC10_1,
    { new WIR_RegisterParameter( *areg15P, WIR_Usage::def ),
      new WIR_RegisterParameter( *areg10P, WIR_Usage::use ),
      new TC_Const10_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SISPC10_2,
    { new WIR_RegisterParameter( *dreg15P, WIR_Usage::def ),
      new WIR_RegisterParameter( *areg10P, WIR_Usage::use ),
      new TC_Const10_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SL, { new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SPSW,
    { new WIR_RegisterParameter( *pswC, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SSPC8,
    { new WIR_RegisterParameter( *areg10P, WIR_Usage::defuse ),
      new TC_Const8_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SSPC10I_1,
    { new WIR_RegisterParameter( *areg10P, WIR_Usage::use ),
      new TC_Const10_Unsigned( 0 ),
      new WIR_RegisterParameter( *areg15P, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SSPC10I_2,
    { new WIR_RegisterParameter( *areg10P, WIR_Usage::use ),
      new TC_Const10_Unsigned( 0 ),
      new WIR_RegisterParameter( *dreg15P, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::SYS, {} );


  //
  // TC13 opcode to operation format mapping.
  //

  WIR_Registry::registerOpCode( OpCode::ABS, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::ABS_B, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::ABS_H, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::ABSDIF, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::ABSDIF, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ABSDIF_B, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ABSDIF_H, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ABSDIFS, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::ABSDIFS, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ABSDIFS_H, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ABSS, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::ABSS_H, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::SDC4_2 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::SDIC4_2 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::SIDC4 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::SDD_2 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::SDID_1 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::SIDD );
  WIR_Registry::registerOpCode( OpCode::ADD_A, OperationFormat::AAA );
  WIR_Registry::registerOpCode( OpCode::ADD_A, OperationFormat::SAC4_2 );
  WIR_Registry::registerOpCode( OpCode::ADD_A, OperationFormat::SAA_5 );
  WIR_Registry::registerOpCode( OpCode::ADD_B, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ADD_F, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ADD_H, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ADDC, OperationFormat::DDC9PSW_2 );
  WIR_Registry::registerOpCode( OpCode::ADDC, OperationFormat::DDDPSW_2 );
  WIR_Registry::registerOpCode( OpCode::ADDI, OperationFormat::DDC16_1 );
  WIR_Registry::registerOpCode( OpCode::ADDIH, OperationFormat::DDC16_2 );
  WIR_Registry::registerOpCode( OpCode::ADDIH_A, OperationFormat::AAC16 );
  WIR_Registry::registerOpCode( OpCode::ADDS, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::ADDS, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ADDS, OperationFormat::SDD_2 );
  WIR_Registry::registerOpCode( OpCode::ADDS_H, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ADDS_HU, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ADDS_U, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::ADDS_U, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ADDSC_A, OperationFormat::AADC2 );
  WIR_Registry::registerOpCode( OpCode::ADDSC_A, OperationFormat::SAAIC2 );
  WIR_Registry::registerOpCode( OpCode::ADDSC_AT, OperationFormat::AAD );
  WIR_Registry::registerOpCode( OpCode::ADDX, OperationFormat::DDC9PSW_1 );
  WIR_Registry::registerOpCode( OpCode::ADDX, OperationFormat::DDDPSW_1 );
  WIR_Registry::registerOpCode( OpCode::AND, OperationFormat::DDC9_2 );
  WIR_Registry::registerOpCode( OpCode::AND, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::AND, OperationFormat::SIC8_2 );
  WIR_Registry::registerOpCode( OpCode::AND, OperationFormat::SDD_2 );
  WIR_Registry::registerOpCode( OpCode::AND_AND_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::AND_ANDN_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::AND_EQ, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::AND_EQ, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::AND_GE, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::AND_GE, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::AND_GE_U, OperationFormat::DDC9_4 );
  WIR_Registry::registerOpCode( OpCode::AND_GE_U, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::AND_LT, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::AND_LT, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::AND_LT_U, OperationFormat::DDC9_4 );
  WIR_Registry::registerOpCode( OpCode::AND_LT_U, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::AND_NE, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::AND_NE, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::AND_NOR_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::AND_OR_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::AND_T, OperationFormat::DDC5DC5_1 );
  WIR_Registry::registerOpCode( OpCode::ANDN, OperationFormat::DDC9_2 );
  WIR_Registry::registerOpCode( OpCode::ANDN, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ANDN_T, OperationFormat::DDC5DC5_1 );
  WIR_Registry::registerOpCode( OpCode::BISR, OperationFormat::C9 );
  WIR_Registry::registerOpCode( OpCode::BISR, OperationFormat::SC8 );
  WIR_Registry::registerOpCode( OpCode::BMERGE, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::BSPLIT, OperationFormat::ED );
  WIR_Registry::registerOpCode( OpCode::CACHEA_I, OperationFormat::AC10BOA );
  WIR_Registry::registerOpCode( OpCode::CACHEA_I, OperationFormat::PBRA );
  WIR_Registry::registerOpCode( OpCode::CACHEA_I, OperationFormat::PC10CA );
  WIR_Registry::registerOpCode( OpCode::CACHEA_I, OperationFormat::AC10PIA );
  WIR_Registry::registerOpCode( OpCode::CACHEA_W, OperationFormat::AC10BOA );
  WIR_Registry::registerOpCode( OpCode::CACHEA_W, OperationFormat::PBRA );
  WIR_Registry::registerOpCode( OpCode::CACHEA_W, OperationFormat::PC10CA );
  WIR_Registry::registerOpCode( OpCode::CACHEA_W, OperationFormat::AC10PIA );
  WIR_Registry::registerOpCode( OpCode::CACHEA_WI, OperationFormat::AC10BOA );
  WIR_Registry::registerOpCode( OpCode::CACHEA_WI, OperationFormat::PBRA );
  WIR_Registry::registerOpCode( OpCode::CACHEA_WI, OperationFormat::PC10CA );
  WIR_Registry::registerOpCode( OpCode::CACHEA_WI, OperationFormat::AC10PIA );
  WIR_Registry::registerOpCode( OpCode::CADD, OperationFormat::DDDC9_1 );
  WIR_Registry::registerOpCode( OpCode::CADD, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::CADD, OperationFormat::SDIC4_3 );
  WIR_Registry::registerOpCode( OpCode::CADDN, OperationFormat::DDDC9_1 );
  WIR_Registry::registerOpCode( OpCode::CADDN, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::CADDN, OperationFormat::SDIC4_3 );
  WIR_Registry::registerOpCode( OpCode::CALL, OperationFormat::L );
  WIR_Registry::registerOpCode( OpCode::CALL, OperationFormat::SL );
  WIR_Registry::registerOpCode( OpCode::CALLA, OperationFormat::L );
  WIR_Registry::registerOpCode( OpCode::CALLI, OperationFormat::A );
  WIR_Registry::registerOpCode( OpCode::CLO, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::CLO_H, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::CLS, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::CLS_H, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::CLZ, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::CLZ_H, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::CMOV, OperationFormat::SDIC4_3 );
  WIR_Registry::registerOpCode( OpCode::CMOV, OperationFormat::SDID_2 );
  WIR_Registry::registerOpCode( OpCode::CMOVN, OperationFormat::SDIC4_3 );
  WIR_Registry::registerOpCode( OpCode::CMOVN, OperationFormat::SDID_2 );
  WIR_Registry::registerOpCode( OpCode::CMP_F, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::CSUB, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::CSUBN, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::DEBUG, OperationFormat::SYS );
  WIR_Registry::registerOpCode( OpCode::DEBUG, OperationFormat::S );
  WIR_Registry::registerOpCode( OpCode::DEXTR, OperationFormat::DDDC5 );
  WIR_Registry::registerOpCode( OpCode::DEXTR, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::DISABLE, OperationFormat::SYS );
  WIR_Registry::registerOpCode( OpCode::DIV_F, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::DSYNC, OperationFormat::SYS );
  WIR_Registry::registerOpCode( OpCode::DVADJ, OperationFormat::EED );
  WIR_Registry::registerOpCode( OpCode::DVINIT, OperationFormat::EDD );
  WIR_Registry::registerOpCode( OpCode::DVINIT_B, OperationFormat::EDD );
  WIR_Registry::registerOpCode( OpCode::DVINIT_BU, OperationFormat::EDD );
  WIR_Registry::registerOpCode( OpCode::DVINIT_H, OperationFormat::EDD );
  WIR_Registry::registerOpCode( OpCode::DVINIT_HU, OperationFormat::EDD );
  WIR_Registry::registerOpCode( OpCode::DVINIT_U, OperationFormat::EDD );
  WIR_Registry::registerOpCode( OpCode::DVSTEP, OperationFormat::EED );
  WIR_Registry::registerOpCode( OpCode::DVSTEP_U, OperationFormat::EED );
  WIR_Registry::registerOpCode( OpCode::ENABLE, OperationFormat::SYS );
  WIR_Registry::registerOpCode( OpCode::EQ, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::EQ, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::EQ, OperationFormat::SIDC4 );
  WIR_Registry::registerOpCode( OpCode::EQ, OperationFormat::SIDD );
  WIR_Registry::registerOpCode( OpCode::EQ_A, OperationFormat::DAA );
  WIR_Registry::registerOpCode( OpCode::EQ_B, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::EQ_H, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::EQ_W, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::EQANY_B, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::EQANY_B, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::EQANY_H, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::EQANY_H, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::EQZ_A, OperationFormat::DA );
  WIR_Registry::registerOpCode( OpCode::EXTR, OperationFormat::DDC5C5 );
  WIR_Registry::registerOpCode( OpCode::EXTR, OperationFormat::DDE );
  WIR_Registry::registerOpCode( OpCode::EXTR, OperationFormat::DDDC5 );
  WIR_Registry::registerOpCode( OpCode::EXTR_U, OperationFormat::DDC5C5 );
  WIR_Registry::registerOpCode( OpCode::EXTR_U, OperationFormat::DDE );
  WIR_Registry::registerOpCode( OpCode::EXTR_U, OperationFormat::DDDC5 );
  WIR_Registry::registerOpCode( OpCode::FTOI, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::FTOQ31, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::FTOU, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::GE, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::GE, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::GE_A, OperationFormat::DAA );
  WIR_Registry::registerOpCode( OpCode::GE_U, OperationFormat::DDC9_2 );
  WIR_Registry::registerOpCode( OpCode::GE_U, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::IMASK, OperationFormat::EC4C5C5 );
  WIR_Registry::registerOpCode( OpCode::IMASK, OperationFormat::EC4DC5 );
  WIR_Registry::registerOpCode( OpCode::IMASK, OperationFormat::EDC5C5 );
  WIR_Registry::registerOpCode( OpCode::IMASK, OperationFormat::EDDC5 );
  WIR_Registry::registerOpCode( OpCode::INS_T, OperationFormat::DDC5DC5_1 );
  WIR_Registry::registerOpCode( OpCode::INSERT, OperationFormat::DDC4C5C5 );
  WIR_Registry::registerOpCode( OpCode::INSERT, OperationFormat::DDC4E );
  WIR_Registry::registerOpCode( OpCode::INSERT, OperationFormat::DDC4DC5 );
  WIR_Registry::registerOpCode( OpCode::INSERT, OperationFormat::DDDC5C5 );
  WIR_Registry::registerOpCode( OpCode::INSERT, OperationFormat::DDDE );
  WIR_Registry::registerOpCode( OpCode::INSERT, OperationFormat::DDDDC5 );
  WIR_Registry::registerOpCode( OpCode::INSN_T, OperationFormat::DDC5DC5_1 );
  WIR_Registry::registerOpCode( OpCode::ISYNC, OperationFormat::SYS );
  WIR_Registry::registerOpCode( OpCode::ITOF, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::IXMAX, OperationFormat::EED );
  WIR_Registry::registerOpCode( OpCode::IXMAX_U, OperationFormat::EED );
  WIR_Registry::registerOpCode( OpCode::IXMIN, OperationFormat::EED );
  WIR_Registry::registerOpCode( OpCode::IXMIN_U, OperationFormat::EED );
  WIR_Registry::registerOpCode( OpCode::J, OperationFormat::L );
  WIR_Registry::registerOpCode( OpCode::J, OperationFormat::SL );
  WIR_Registry::registerOpCode( OpCode::JA, OperationFormat::L );
  WIR_Registry::registerOpCode( OpCode::JEQ, OperationFormat::DC4L_1 );
  WIR_Registry::registerOpCode( OpCode::JEQ, OperationFormat::DDL_1 );
  WIR_Registry::registerOpCode( OpCode::JEQ, OperationFormat::SIC4L );
  WIR_Registry::registerOpCode( OpCode::JEQ, OperationFormat::SIDL );
  WIR_Registry::registerOpCode( OpCode::JEQ_A, OperationFormat::AAL );
  WIR_Registry::registerOpCode( OpCode::JGE, OperationFormat::DC4L_1 );
  WIR_Registry::registerOpCode( OpCode::JGE, OperationFormat::DDL_1 );
  WIR_Registry::registerOpCode( OpCode::JGE_U, OperationFormat::DC4L_2 );
  WIR_Registry::registerOpCode( OpCode::JGE_U, OperationFormat::DDL_1 );
  WIR_Registry::registerOpCode( OpCode::JGEZ, OperationFormat::SDL );
  WIR_Registry::registerOpCode( OpCode::JGTZ, OperationFormat::SDL );
  WIR_Registry::registerOpCode( OpCode::JI, OperationFormat::A );
  WIR_Registry::registerOpCode( OpCode::JI, OperationFormat::SA );
  WIR_Registry::registerOpCode( OpCode::JL, OperationFormat::L );
  WIR_Registry::registerOpCode( OpCode::JLA, OperationFormat::L );
  WIR_Registry::registerOpCode( OpCode::JLEZ, OperationFormat::SDL );
  WIR_Registry::registerOpCode( OpCode::JLI, OperationFormat::A );
  WIR_Registry::registerOpCode( OpCode::JLT, OperationFormat::DC4L_1 );
  WIR_Registry::registerOpCode( OpCode::JLT, OperationFormat::DDL_1 );
  WIR_Registry::registerOpCode( OpCode::JLT_U, OperationFormat::DC4L_2 );
  WIR_Registry::registerOpCode( OpCode::JLT_U, OperationFormat::DDL_1 );
  WIR_Registry::registerOpCode( OpCode::JLTZ, OperationFormat::SDL );
  WIR_Registry::registerOpCode( OpCode::JNE, OperationFormat::DC4L_1 );
  WIR_Registry::registerOpCode( OpCode::JNE, OperationFormat::DDL_1 );
  WIR_Registry::registerOpCode( OpCode::JNE, OperationFormat::SIC4L );
  WIR_Registry::registerOpCode( OpCode::JNE, OperationFormat::SIDL );
  WIR_Registry::registerOpCode( OpCode::JNE_A, OperationFormat::AAL );
  WIR_Registry::registerOpCode( OpCode::JNED, OperationFormat::DC4L_3 );
  WIR_Registry::registerOpCode( OpCode::JNED, OperationFormat::DDL_2 );
  WIR_Registry::registerOpCode( OpCode::JNEI, OperationFormat::DC4L_3 );
  WIR_Registry::registerOpCode( OpCode::JNEI, OperationFormat::DDL_2 );
  WIR_Registry::registerOpCode( OpCode::JNZ, OperationFormat::SDL );
  WIR_Registry::registerOpCode( OpCode::JNZ, OperationFormat::SIL );
  WIR_Registry::registerOpCode( OpCode::JNZ_A, OperationFormat::AL_2 );
  WIR_Registry::registerOpCode( OpCode::JNZ_A, OperationFormat::SAL_1 );
  WIR_Registry::registerOpCode( OpCode::JNZ_T, OperationFormat::DC5L );
  WIR_Registry::registerOpCode( OpCode::JNZ_T, OperationFormat::SIC5L );
  WIR_Registry::registerOpCode( OpCode::JZ, OperationFormat::SDL );
  WIR_Registry::registerOpCode( OpCode::JZ, OperationFormat::SIL );
  WIR_Registry::registerOpCode( OpCode::JZ_A, OperationFormat::AL_2 );
  WIR_Registry::registerOpCode( OpCode::JZ_A, OperationFormat::SAL_1 );
  WIR_Registry::registerOpCode( OpCode::JZ_T, OperationFormat::DC5L );
  WIR_Registry::registerOpCode( OpCode::JZ_T, OperationFormat::SIC5L );
  WIR_Registry::registerOpCode( OpCode::LD_A, OperationFormat::AC18ABSA );
  WIR_Registry::registerOpCode( OpCode::LD_A, OperationFormat::ALABSA );
  WIR_Registry::registerOpCode( OpCode::LD_A, OperationFormat::AAC10BOA );
  WIR_Registry::registerOpCode( OpCode::LD_A, OperationFormat::APBRA );
  WIR_Registry::registerOpCode( OpCode::LD_A, OperationFormat::APC10CA );
  WIR_Registry::registerOpCode( OpCode::LD_A, OperationFormat::AAC10PIA );
  WIR_Registry::registerOpCode( OpCode::LD_A, OperationFormat::AAC16BOA );
  WIR_Registry::registerOpCode( OpCode::LD_A, OperationFormat::AALC16BOA );
  WIR_Registry::registerOpCode( OpCode::LD_A, OperationFormat::SISPC10_1 );
  WIR_Registry::registerOpCode( OpCode::LD_A, OperationFormat::SAA_2 );
  WIR_Registry::registerOpCode( OpCode::LD_A, OperationFormat::SAA_3 );
  WIR_Registry::registerOpCode( OpCode::LD_A, OperationFormat::SAIC4 );
  WIR_Registry::registerOpCode( OpCode::LD_A, OperationFormat::SIAC4_1 );
  WIR_Registry::registerOpCode( OpCode::LD_B, OperationFormat::DC18ABSA );
  WIR_Registry::registerOpCode( OpCode::LD_B, OperationFormat::DLABSA );
  WIR_Registry::registerOpCode( OpCode::LD_B, OperationFormat::DAC10BOA );
  WIR_Registry::registerOpCode( OpCode::LD_B, OperationFormat::DPBRA );
  WIR_Registry::registerOpCode( OpCode::LD_B, OperationFormat::DPC10CA );
  WIR_Registry::registerOpCode( OpCode::LD_B, OperationFormat::DAC10PIA );
  WIR_Registry::registerOpCode( OpCode::LD_BU, OperationFormat::DC18ABSA );
  WIR_Registry::registerOpCode( OpCode::LD_BU, OperationFormat::DLABSA );
  WIR_Registry::registerOpCode( OpCode::LD_BU, OperationFormat::DAC10BOA );
  WIR_Registry::registerOpCode( OpCode::LD_BU, OperationFormat::DPBRA );
  WIR_Registry::registerOpCode( OpCode::LD_BU, OperationFormat::DPC10CA );
  WIR_Registry::registerOpCode( OpCode::LD_BU, OperationFormat::DAC10PIA );
  WIR_Registry::registerOpCode( OpCode::LD_BU, OperationFormat::SDA_2 );
  WIR_Registry::registerOpCode( OpCode::LD_BU, OperationFormat::SDA_3 );
  WIR_Registry::registerOpCode( OpCode::LD_BU, OperationFormat::SDIC4_1 );
  WIR_Registry::registerOpCode( OpCode::LD_BU, OperationFormat::SIAC4_2 );
  WIR_Registry::registerOpCode( OpCode::LD_D, OperationFormat::EC18ABSA );
  WIR_Registry::registerOpCode( OpCode::LD_D, OperationFormat::ELABSA );
  WIR_Registry::registerOpCode( OpCode::LD_D, OperationFormat::EAC10BOA );
  WIR_Registry::registerOpCode( OpCode::LD_D, OperationFormat::EPBRA );
  WIR_Registry::registerOpCode( OpCode::LD_D, OperationFormat::EPC10CA );
  WIR_Registry::registerOpCode( OpCode::LD_D, OperationFormat::EAC10PIA );
  WIR_Registry::registerOpCode( OpCode::LD_DA, OperationFormat::PC18ABSA );
  WIR_Registry::registerOpCode( OpCode::LD_DA, OperationFormat::PLABSA );
  WIR_Registry::registerOpCode( OpCode::LD_DA, OperationFormat::PAC10BOA );
  WIR_Registry::registerOpCode( OpCode::LD_DA, OperationFormat::PPBRA_1 );
  WIR_Registry::registerOpCode( OpCode::LD_DA, OperationFormat::PPC10CA );
  WIR_Registry::registerOpCode( OpCode::LD_DA, OperationFormat::PAC10PIA );
  WIR_Registry::registerOpCode( OpCode::LD_H, OperationFormat::DC18ABSA );
  WIR_Registry::registerOpCode( OpCode::LD_H, OperationFormat::DLABSA );
  WIR_Registry::registerOpCode( OpCode::LD_H, OperationFormat::DAC10BOA );
  WIR_Registry::registerOpCode( OpCode::LD_H, OperationFormat::DPBRA );
  WIR_Registry::registerOpCode( OpCode::LD_H, OperationFormat::DPC10CA );
  WIR_Registry::registerOpCode( OpCode::LD_H, OperationFormat::DAC10PIA );
  WIR_Registry::registerOpCode( OpCode::LD_H, OperationFormat::SDA_2 );
  WIR_Registry::registerOpCode( OpCode::LD_H, OperationFormat::SDA_3 );
  WIR_Registry::registerOpCode( OpCode::LD_H, OperationFormat::SDIC4_1 );
  WIR_Registry::registerOpCode( OpCode::LD_H, OperationFormat::SIAC4_2 );
  WIR_Registry::registerOpCode( OpCode::LD_HU, OperationFormat::DC18ABSA );
  WIR_Registry::registerOpCode( OpCode::LD_HU, OperationFormat::DLABSA );
  WIR_Registry::registerOpCode( OpCode::LD_HU, OperationFormat::DAC10BOA );
  WIR_Registry::registerOpCode( OpCode::LD_HU, OperationFormat::DPBRA );
  WIR_Registry::registerOpCode( OpCode::LD_HU, OperationFormat::DPC10CA );
  WIR_Registry::registerOpCode( OpCode::LD_HU, OperationFormat::DAC10PIA );
  WIR_Registry::registerOpCode( OpCode::LD_Q, OperationFormat::DC18ABSA );
  WIR_Registry::registerOpCode( OpCode::LD_Q, OperationFormat::DLABSA );
  WIR_Registry::registerOpCode( OpCode::LD_Q, OperationFormat::DAC10BOA );
  WIR_Registry::registerOpCode( OpCode::LD_Q, OperationFormat::DPBRA );
  WIR_Registry::registerOpCode( OpCode::LD_Q, OperationFormat::DPC10CA );
  WIR_Registry::registerOpCode( OpCode::LD_Q, OperationFormat::DAC10PIA );
  WIR_Registry::registerOpCode( OpCode::LD_W, OperationFormat::DC18ABSA );
  WIR_Registry::registerOpCode( OpCode::LD_W, OperationFormat::DLABSA );
  WIR_Registry::registerOpCode( OpCode::LD_W, OperationFormat::DAC10BOA );
  WIR_Registry::registerOpCode( OpCode::LD_W, OperationFormat::DPBRA );
  WIR_Registry::registerOpCode( OpCode::LD_W, OperationFormat::DPC10CA );
  WIR_Registry::registerOpCode( OpCode::LD_W, OperationFormat::DAC10PIA );
  WIR_Registry::registerOpCode( OpCode::LD_W, OperationFormat::DAC16BOA );
  WIR_Registry::registerOpCode( OpCode::LD_W, OperationFormat::DALC16BOA );
  WIR_Registry::registerOpCode( OpCode::LD_W, OperationFormat::SISPC10_2 );
  WIR_Registry::registerOpCode( OpCode::LD_W, OperationFormat::SDA_2 );
  WIR_Registry::registerOpCode( OpCode::LD_W, OperationFormat::SDA_3 );
  WIR_Registry::registerOpCode( OpCode::LD_W, OperationFormat::SDIC4_1 );
  WIR_Registry::registerOpCode( OpCode::LD_W, OperationFormat::SIAC4_2 );
  WIR_Registry::registerOpCode( OpCode::LDLCX, OperationFormat::C18ABSA );
  WIR_Registry::registerOpCode( OpCode::LDLCX, OperationFormat::LABSA );
  WIR_Registry::registerOpCode( OpCode::LDLCX, OperationFormat::AC10BOA );
  WIR_Registry::registerOpCode( OpCode::LDMST, OperationFormat::C18EABSA );
  WIR_Registry::registerOpCode( OpCode::LDMST, OperationFormat::LEABSA );
  WIR_Registry::registerOpCode( OpCode::LDMST, OperationFormat::AC10EBOA );
  WIR_Registry::registerOpCode( OpCode::LDMST, OperationFormat::PEBRA );
  WIR_Registry::registerOpCode( OpCode::LDMST, OperationFormat::PC10ECA );
  WIR_Registry::registerOpCode( OpCode::LDMST, OperationFormat::AC10EPIA );
  WIR_Registry::registerOpCode( OpCode::LDUCX, OperationFormat::C18ABSA );
  WIR_Registry::registerOpCode( OpCode::LDUCX, OperationFormat::LABSA );
  WIR_Registry::registerOpCode( OpCode::LDUCX, OperationFormat::AC10BOA );
  WIR_Registry::registerOpCode( OpCode::LEA, OperationFormat::AC18ABSA );
  WIR_Registry::registerOpCode( OpCode::LEA, OperationFormat::ALABSA );
  WIR_Registry::registerOpCode( OpCode::LEA, OperationFormat::AAC10BOA );
  WIR_Registry::registerOpCode( OpCode::LEA, OperationFormat::AAC16BOA );
  WIR_Registry::registerOpCode( OpCode::LEA, OperationFormat::AALC16BOA );
  WIR_Registry::registerOpCode( OpCode::LOOP, OperationFormat::AL_3 );
  WIR_Registry::registerOpCode( OpCode::LOOP, OperationFormat::SAL_2 );
  WIR_Registry::registerOpCode( OpCode::LOOPU, OperationFormat::L );
  WIR_Registry::registerOpCode( OpCode::LT, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::LT, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::LT, OperationFormat::SIDC4 );
  WIR_Registry::registerOpCode( OpCode::LT, OperationFormat::SIDD );
  WIR_Registry::registerOpCode( OpCode::LT_A, OperationFormat::DAA );
  WIR_Registry::registerOpCode( OpCode::LT_B, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::LT_BU, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::LT_H, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::LT_HU, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::LT_U, OperationFormat::DDC9_2 );
  WIR_Registry::registerOpCode( OpCode::LT_U, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::LT_W, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::LT_WU, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MADD, OperationFormat::DDDC9_1 );
  WIR_Registry::registerOpCode( OpCode::MADD, OperationFormat::EEDC9_1 );
  WIR_Registry::registerOpCode( OpCode::MADD, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::MADD, OperationFormat::EEDD );
  WIR_Registry::registerOpCode( OpCode::MADD_F, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::MADD_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MADD_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MADD_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MADD_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MADD_Q, OperationFormat::DDDDC1_1 );
  WIR_Registry::registerOpCode( OpCode::MADD_Q, OperationFormat::EEDDC1_1 );
  WIR_Registry::registerOpCode( OpCode::MADD_Q, OperationFormat::DDDDC1_2 );
  WIR_Registry::registerOpCode( OpCode::MADD_Q, OperationFormat::EEDDC1_2 );
  WIR_Registry::registerOpCode( OpCode::MADD_Q, OperationFormat::DDDDC1_5 );
  WIR_Registry::registerOpCode( OpCode::MADD_Q, OperationFormat::EEDDC1_5 );
  WIR_Registry::registerOpCode( OpCode::MADD_Q, OperationFormat::DDDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MADD_Q, OperationFormat::EEDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MADD_Q, OperationFormat::DDDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MADD_Q, OperationFormat::EEDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MADD_U, OperationFormat::EEDC9_2 );
  WIR_Registry::registerOpCode( OpCode::MADD_U, OperationFormat::EEDD );
  WIR_Registry::registerOpCode( OpCode::MADDM_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MADDM_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MADDM_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MADDM_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MADDMS_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MADDMS_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MADDMS_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MADDMS_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MADDR_H, OperationFormat::DDDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MADDR_H, OperationFormat::DDDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MADDR_H, OperationFormat::DDDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MADDR_H, OperationFormat::DEDDC1 );
  WIR_Registry::registerOpCode( OpCode::MADDR_H, OperationFormat::DDDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MADDR_Q, OperationFormat::DDDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MADDR_Q, OperationFormat::DDDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MADDRS_H, OperationFormat::DDDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MADDRS_H, OperationFormat::DDDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MADDRS_H, OperationFormat::DDDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MADDRS_H, OperationFormat::DEDDC1 );
  WIR_Registry::registerOpCode( OpCode::MADDRS_H, OperationFormat::DDDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MADDRS_Q, OperationFormat::DDDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MADDRS_Q, OperationFormat::DDDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MADDS, OperationFormat::DDDC9_1 );
  WIR_Registry::registerOpCode( OpCode::MADDS, OperationFormat::EEDC9_1 );
  WIR_Registry::registerOpCode( OpCode::MADDS, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::MADDS, OperationFormat::EEDD );
  WIR_Registry::registerOpCode( OpCode::MADDS_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MADDS_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MADDS_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MADDS_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MADDS_Q, OperationFormat::DDDDC1_1 );
  WIR_Registry::registerOpCode( OpCode::MADDS_Q, OperationFormat::EEDDC1_1 );
  WIR_Registry::registerOpCode( OpCode::MADDS_Q, OperationFormat::DDDDC1_2 );
  WIR_Registry::registerOpCode( OpCode::MADDS_Q, OperationFormat::EEDDC1_2 );
  WIR_Registry::registerOpCode( OpCode::MADDS_Q, OperationFormat::DDDDC1_5 );
  WIR_Registry::registerOpCode( OpCode::MADDS_Q, OperationFormat::EEDDC1_5 );
  WIR_Registry::registerOpCode( OpCode::MADDS_Q, OperationFormat::DDDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MADDS_Q, OperationFormat::EEDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MADDS_Q, OperationFormat::DDDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MADDS_Q, OperationFormat::EEDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MADDS_U, OperationFormat::DDDC9_2 );
  WIR_Registry::registerOpCode( OpCode::MADDS_U, OperationFormat::EEDC9_2 );
  WIR_Registry::registerOpCode( OpCode::MADDS_U, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::MADDS_U, OperationFormat::EEDD );
  WIR_Registry::registerOpCode( OpCode::MADDSU_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MADDSU_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MADDSU_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MADDSU_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MADDSUM_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MADDSUM_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MADDSUM_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MADDSUM_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MADDSUMS_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MADDSUMS_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MADDSUMS_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MADDSUMS_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MADDSUR_H, OperationFormat::DDDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MADDSUR_H, OperationFormat::DDDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MADDSUR_H, OperationFormat::DDDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MADDSUR_H, OperationFormat::DDDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MADDSURS_H, OperationFormat::DDDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MADDSURS_H, OperationFormat::DDDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MADDSURS_H, OperationFormat::DDDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MADDSURS_H, OperationFormat::DDDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MADDSUS_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MADDSUS_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MADDSUS_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MADDSUS_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MAX, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::MAX, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MAX_B, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MAX_BU, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MAX_H, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MAX_HU, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MAX_U, OperationFormat::DDC9_2 );
  WIR_Registry::registerOpCode( OpCode::MAX_U, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MFCR, OperationFormat::DC16PSW );
  WIR_Registry::registerOpCode( OpCode::MIN, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::MIN, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MIN_B, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MIN_BU, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MIN_H, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MIN_HU, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MIN_U, OperationFormat::DDC9_2 );
  WIR_Registry::registerOpCode( OpCode::MIN_U, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MOV, OperationFormat::DC16_1 );
  WIR_Registry::registerOpCode( OpCode::MOV, OperationFormat::SIC8_1 );
  WIR_Registry::registerOpCode( OpCode::MOV, OperationFormat::SDC4_1 );
  WIR_Registry::registerOpCode( OpCode::MOV_RR, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::MOV_RR, OperationFormat::SDD_1 );
  WIR_Registry::registerOpCode( OpCode::MOV_A, OperationFormat::AD );
  WIR_Registry::registerOpCode( OpCode::MOV_A, OperationFormat::SAC4_1 );
  WIR_Registry::registerOpCode( OpCode::MOV_A, OperationFormat::SAD_1 );
  WIR_Registry::registerOpCode( OpCode::MOV_AA, OperationFormat::AA );
  WIR_Registry::registerOpCode( OpCode::MOV_AA, OperationFormat::SAA_1 );
  WIR_Registry::registerOpCode( OpCode::MOV_D, OperationFormat::DA );
  WIR_Registry::registerOpCode( OpCode::MOV_D, OperationFormat::SDA_1 );
  WIR_Registry::registerOpCode( OpCode::MOV_U, OperationFormat::DC16_2 );
  WIR_Registry::registerOpCode( OpCode::MOVH, OperationFormat::DC16_2 );
  WIR_Registry::registerOpCode( OpCode::MOVH_A, OperationFormat::AC16 );
  WIR_Registry::registerOpCode( OpCode::MOVH_A, OperationFormat::AL_1 );
  WIR_Registry::registerOpCode( OpCode::MSUB, OperationFormat::DDDC9_1 );
  WIR_Registry::registerOpCode( OpCode::MSUB, OperationFormat::EEDC9_1 );
  WIR_Registry::registerOpCode( OpCode::MSUB, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::MSUB, OperationFormat::EEDD );
  WIR_Registry::registerOpCode( OpCode::MSUB_F, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::MSUB_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MSUB_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MSUB_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MSUB_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MSUB_Q, OperationFormat::DDDDC1_1 );
  WIR_Registry::registerOpCode( OpCode::MSUB_Q, OperationFormat::EEDDC1_1 );
  WIR_Registry::registerOpCode( OpCode::MSUB_Q, OperationFormat::DDDDC1_2 );
  WIR_Registry::registerOpCode( OpCode::MSUB_Q, OperationFormat::EEDDC1_2 );
  WIR_Registry::registerOpCode( OpCode::MSUB_Q, OperationFormat::DDDDC1_5 );
  WIR_Registry::registerOpCode( OpCode::MSUB_Q, OperationFormat::EEDDC1_5 );
  WIR_Registry::registerOpCode( OpCode::MSUB_Q, OperationFormat::DDDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MSUB_Q, OperationFormat::EEDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MSUB_Q, OperationFormat::DDDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MSUB_Q, OperationFormat::EEDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MSUB_U, OperationFormat::EEDC9_2 );
  WIR_Registry::registerOpCode( OpCode::MSUB_U, OperationFormat::EEDD );
  WIR_Registry::registerOpCode( OpCode::MSUBAD_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MSUBAD_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MSUBAD_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MSUBAD_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MSUBADM_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MSUBADM_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MSUBADM_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MSUBADM_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MSUBADMS_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MSUBADMS_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MSUBADMS_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MSUBADMS_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MSUBADR_H, OperationFormat::DDDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MSUBADR_H, OperationFormat::DDDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MSUBADR_H, OperationFormat::DDDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MSUBADR_H, OperationFormat::DDDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MSUBADRS_H, OperationFormat::DDDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MSUBADRS_H, OperationFormat::DDDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MSUBADRS_H, OperationFormat::DDDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MSUBADRS_H, OperationFormat::DDDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MSUBADS_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MSUBADS_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MSUBADS_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MSUBADS_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MSUBM_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MSUBM_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MSUBM_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MSUBM_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MSUBMS_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MSUBMS_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MSUBMS_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MSUBMS_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MSUBR_H, OperationFormat::DDDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MSUBR_H, OperationFormat::DDDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MSUBR_H, OperationFormat::DDDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MSUBR_H, OperationFormat::DEDDC1 );
  WIR_Registry::registerOpCode( OpCode::MSUBR_H, OperationFormat::DDDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MSUBR_Q, OperationFormat::DDDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MSUBR_Q, OperationFormat::DDDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MSUBRS_H, OperationFormat::DDDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MSUBRS_H, OperationFormat::DDDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MSUBRS_H, OperationFormat::DDDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MSUBRS_H, OperationFormat::DEDDC1 );
  WIR_Registry::registerOpCode( OpCode::MSUBRS_H, OperationFormat::DDDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MSUBRS_Q, OperationFormat::DDDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MSUBRS_Q, OperationFormat::DDDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MSUBS, OperationFormat::DDDC9_1 );
  WIR_Registry::registerOpCode( OpCode::MSUBS, OperationFormat::EEDC9_1 );
  WIR_Registry::registerOpCode( OpCode::MSUBS, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::MSUBS, OperationFormat::EEDD );
  WIR_Registry::registerOpCode( OpCode::MSUBS_H, OperationFormat::EEDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_H, OperationFormat::EEDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_H, OperationFormat::EEDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_H, OperationFormat::EEDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_Q, OperationFormat::DDDDC1_1 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_Q, OperationFormat::EEDDC1_1 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_Q, OperationFormat::DDDDC1_2 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_Q, OperationFormat::EEDDC1_2 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_Q, OperationFormat::DDDDC1_5 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_Q, OperationFormat::EEDDC1_5 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_Q, OperationFormat::DDDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_Q, OperationFormat::EEDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_Q, OperationFormat::DDDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_Q, OperationFormat::EEDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_U, OperationFormat::DDDC9_2 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_U, OperationFormat::EEDC9_2 );
  WIR_Registry::registerOpCode( OpCode::MSUBS_U, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::MSUBS_U, OperationFormat::EEDD );
  WIR_Registry::registerOpCode( OpCode::MTCR, OperationFormat::C16DPSW );
  WIR_Registry::registerOpCode( OpCode::MUL, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::MUL, OperationFormat::EDC9_1 );
  WIR_Registry::registerOpCode( OpCode::MUL, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MUL, OperationFormat::EDD );
  WIR_Registry::registerOpCode( OpCode::MUL, OperationFormat::SDD_2 );
  WIR_Registry::registerOpCode( OpCode::MUL_F, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MUL_H, OperationFormat::EDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MUL_H, OperationFormat::EDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MUL_H, OperationFormat::EDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MUL_H, OperationFormat::EDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MUL_Q, OperationFormat::DDDC1_1 );
  WIR_Registry::registerOpCode( OpCode::MUL_Q, OperationFormat::EDDC1_1 );
  WIR_Registry::registerOpCode( OpCode::MUL_Q, OperationFormat::DDDC1_2 );
  WIR_Registry::registerOpCode( OpCode::MUL_Q, OperationFormat::EDDC1_2 );
  WIR_Registry::registerOpCode( OpCode::MUL_Q, OperationFormat::DDDC1_5 );
  WIR_Registry::registerOpCode( OpCode::MUL_Q, OperationFormat::EDDC1_5 );
  WIR_Registry::registerOpCode( OpCode::MUL_Q, OperationFormat::DDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MUL_Q, OperationFormat::DDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MUL_U, OperationFormat::EDC9_2 );
  WIR_Registry::registerOpCode( OpCode::MUL_U, OperationFormat::EDD );
  WIR_Registry::registerOpCode( OpCode::MULM_H, OperationFormat::EDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MULM_H, OperationFormat::EDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MULM_H, OperationFormat::EDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MULM_H, OperationFormat::EDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MULR_H, OperationFormat::DDDC1_3 );
  WIR_Registry::registerOpCode( OpCode::MULR_H, OperationFormat::DDDC1_4 );
  WIR_Registry::registerOpCode( OpCode::MULR_H, OperationFormat::DDDC1_6 );
  WIR_Registry::registerOpCode( OpCode::MULR_H, OperationFormat::DDDC1_7 );
  WIR_Registry::registerOpCode( OpCode::MULR_Q, OperationFormat::DDDC1_8 );
  WIR_Registry::registerOpCode( OpCode::MULR_Q, OperationFormat::DDDC1_9 );
  WIR_Registry::registerOpCode( OpCode::MULS, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::MULS, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::MULS_U, OperationFormat::DDC9_2 );
  WIR_Registry::registerOpCode( OpCode::MULS_U, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::NAND, OperationFormat::DDC9_2 );
  WIR_Registry::registerOpCode( OpCode::NAND, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::NAND_T, OperationFormat::DDC5DC5_1 );
  WIR_Registry::registerOpCode( OpCode::NE, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::NE, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::NE_A, OperationFormat::DAA );
  WIR_Registry::registerOpCode( OpCode::NEZ_A, OperationFormat::DA );
  WIR_Registry::registerOpCode( OpCode::NOP, OperationFormat::SYS );
  WIR_Registry::registerOpCode( OpCode::NOP, OperationFormat::S );
  WIR_Registry::registerOpCode( OpCode::NOR, OperationFormat::DDC9_2 );
  WIR_Registry::registerOpCode( OpCode::NOR, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::NOR_T, OperationFormat::DDC5DC5_1 );
  WIR_Registry::registerOpCode( OpCode::NOT, OperationFormat::SD );
  WIR_Registry::registerOpCode( OpCode::OR, OperationFormat::DDC9_2 );
  WIR_Registry::registerOpCode( OpCode::OR, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::OR, OperationFormat::SIC8_2 );
  WIR_Registry::registerOpCode( OpCode::OR, OperationFormat::SDD_2 );
  WIR_Registry::registerOpCode( OpCode::OR_AND_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::OR_ANDN_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::OR_EQ, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::OR_EQ, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::OR_GE, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::OR_GE, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::OR_GE_U, OperationFormat::DDC9_4 );
  WIR_Registry::registerOpCode( OpCode::OR_GE_U, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::OR_LT, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::OR_LT, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::OR_LT_U, OperationFormat::DDC9_4 );
  WIR_Registry::registerOpCode( OpCode::OR_LT_U, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::OR_NE, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::OR_NE, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::OR_NOR_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::OR_OR_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::OR_T, OperationFormat::DDC5DC5_1 );
  WIR_Registry::registerOpCode( OpCode::ORN, OperationFormat::DDC9_2 );
  WIR_Registry::registerOpCode( OpCode::ORN, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ORN_T, OperationFormat::DDC5DC5_1 );
  WIR_Registry::registerOpCode( OpCode::PACK, OperationFormat::DEDPSW );
  WIR_Registry::registerOpCode( OpCode::PARITY, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::Q31TOF, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::QSEED_F, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::RET, OperationFormat::PSW );
  WIR_Registry::registerOpCode( OpCode::RET, OperationFormat::SPSW );
  WIR_Registry::registerOpCode( OpCode::RFE, OperationFormat::PSW );
  WIR_Registry::registerOpCode( OpCode::RFE, OperationFormat::SPSW );
  WIR_Registry::registerOpCode( OpCode::RFM, OperationFormat::PSW );
  WIR_Registry::registerOpCode( OpCode::RSLCX, OperationFormat::SYS );
  WIR_Registry::registerOpCode( OpCode::RSTV, OperationFormat::SYS );
  WIR_Registry::registerOpCode( OpCode::RSUB, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::RSUB, OperationFormat::SD );
  WIR_Registry::registerOpCode( OpCode::RSUBS, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::RSUBS_U, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::SAT_B, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::SAT_B, OperationFormat::SD );
  WIR_Registry::registerOpCode( OpCode::SAT_BU, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::SAT_BU, OperationFormat::SD );
  WIR_Registry::registerOpCode( OpCode::SAT_H, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::SAT_H, OperationFormat::SD );
  WIR_Registry::registerOpCode( OpCode::SAT_HU, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::SAT_HU, OperationFormat::SD );
  WIR_Registry::registerOpCode( OpCode::SEL, OperationFormat::DDDC9_1 );
  WIR_Registry::registerOpCode( OpCode::SEL, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::SELN, OperationFormat::DDDC9_1 );
  WIR_Registry::registerOpCode( OpCode::SELN, OperationFormat::DDDD );
  WIR_Registry::registerOpCode( OpCode::SH, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::SH, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::SH, OperationFormat::SDC4_2 );
  WIR_Registry::registerOpCode( OpCode::SH_AND_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::SH_ANDN_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::SH_EQ, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::SH_EQ, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::SH_GE, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::SH_GE, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::SH_GE_U, OperationFormat::DDC9_4 );
  WIR_Registry::registerOpCode( OpCode::SH_GE_U, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::SH_H, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::SH_H, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::SH_LT, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::SH_LT, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::SH_LT_U, OperationFormat::DDC9_4 );
  WIR_Registry::registerOpCode( OpCode::SH_LT_U, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::SH_NAND_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::SH_NE, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::SH_NE, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::SH_NOR_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::SH_OR_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::SH_ORN_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::SH_XNOR_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::SH_XOR_T, OperationFormat::DDC5DC5_2 );
  WIR_Registry::registerOpCode( OpCode::SHA, OperationFormat::DDC9PSW_1 );
  WIR_Registry::registerOpCode( OpCode::SHA, OperationFormat::DDDPSW_1 );
  WIR_Registry::registerOpCode( OpCode::SHA, OperationFormat::SDC4PSW );
  WIR_Registry::registerOpCode( OpCode::SHA_H, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::SHA_H, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::SHAS, OperationFormat::DDC9_1 );
  WIR_Registry::registerOpCode( OpCode::SHAS, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::ST_A, OperationFormat::C18AABSA );
  WIR_Registry::registerOpCode( OpCode::ST_A, OperationFormat::LAABSA );
  WIR_Registry::registerOpCode( OpCode::ST_A, OperationFormat::AC10ABOA );
  WIR_Registry::registerOpCode( OpCode::ST_A, OperationFormat::PABRA );
  WIR_Registry::registerOpCode( OpCode::ST_A, OperationFormat::PC10ACA );
  WIR_Registry::registerOpCode( OpCode::ST_A, OperationFormat::AC10APIA );
  WIR_Registry::registerOpCode( OpCode::ST_A, OperationFormat::SSPC10I_1 );
  WIR_Registry::registerOpCode( OpCode::ST_A, OperationFormat::SAC4I_1 );
  WIR_Registry::registerOpCode( OpCode::ST_A, OperationFormat::SAA_4 );
  WIR_Registry::registerOpCode( OpCode::ST_A, OperationFormat::SAA_6 );
  WIR_Registry::registerOpCode( OpCode::ST_A, OperationFormat::SIC4A );
  WIR_Registry::registerOpCode( OpCode::ST_B, OperationFormat::C18DABSA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_B, OperationFormat::LDABSA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_B, OperationFormat::AC10DBOA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_B, OperationFormat::PDBRA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_B, OperationFormat::PC10DCA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_B, OperationFormat::AC10DPIA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_B, OperationFormat::SAC4I_2 );
  WIR_Registry::registerOpCode( OpCode::ST_B, OperationFormat::SAD_2 );
  WIR_Registry::registerOpCode( OpCode::ST_B, OperationFormat::SAD_3 );
  WIR_Registry::registerOpCode( OpCode::ST_B, OperationFormat::SIC4D );
  WIR_Registry::registerOpCode( OpCode::ST_D, OperationFormat::C18EABSA );
  WIR_Registry::registerOpCode( OpCode::ST_D, OperationFormat::LEABSA );
  WIR_Registry::registerOpCode( OpCode::ST_D, OperationFormat::AC10EBOA );
  WIR_Registry::registerOpCode( OpCode::ST_D, OperationFormat::PEBRA );
  WIR_Registry::registerOpCode( OpCode::ST_D, OperationFormat::PC10ECA );
  WIR_Registry::registerOpCode( OpCode::ST_D, OperationFormat::AC10EPIA );
  WIR_Registry::registerOpCode( OpCode::ST_DA, OperationFormat::C18PABSA );
  WIR_Registry::registerOpCode( OpCode::ST_DA, OperationFormat::LPABSA );
  WIR_Registry::registerOpCode( OpCode::ST_DA, OperationFormat::AC10PBOA );
  WIR_Registry::registerOpCode( OpCode::ST_DA, OperationFormat::PPBRA_2 );
  WIR_Registry::registerOpCode( OpCode::ST_DA, OperationFormat::PC10PCA );
  WIR_Registry::registerOpCode( OpCode::ST_DA, OperationFormat::AC10PPIA );
  WIR_Registry::registerOpCode( OpCode::ST_H, OperationFormat::C18DABSA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_H, OperationFormat::LDABSA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_H, OperationFormat::AC10DBOA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_H, OperationFormat::PDBRA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_H, OperationFormat::PC10DCA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_H, OperationFormat::AC10DPIA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_H, OperationFormat::SAC4I_2 );
  WIR_Registry::registerOpCode( OpCode::ST_H, OperationFormat::SAD_2 );
  WIR_Registry::registerOpCode( OpCode::ST_H, OperationFormat::SAD_3 );
  WIR_Registry::registerOpCode( OpCode::ST_H, OperationFormat::SIC4D );
  WIR_Registry::registerOpCode( OpCode::ST_Q, OperationFormat::C18DABSA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_Q, OperationFormat::LDABSA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_Q, OperationFormat::AC10DBOA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_Q, OperationFormat::PDBRA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_Q, OperationFormat::PC10DCA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_Q, OperationFormat::AC10DPIA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_T, OperationFormat::C18C3C1 );
  WIR_Registry::registerOpCode( OpCode::ST_W, OperationFormat::C18DABSA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_W, OperationFormat::LDABSA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_W, OperationFormat::AC10DBOA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_W, OperationFormat::PDBRA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_W, OperationFormat::PC10DCA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_W, OperationFormat::AC10DPIA_1 );
  WIR_Registry::registerOpCode( OpCode::ST_W, OperationFormat::AC16DBOA );
  WIR_Registry::registerOpCode( OpCode::ST_W, OperationFormat::ALC16DBOA );
  WIR_Registry::registerOpCode( OpCode::ST_W, OperationFormat::SSPC10I_2 );
  WIR_Registry::registerOpCode( OpCode::ST_W, OperationFormat::SAC4I_2 );
  WIR_Registry::registerOpCode( OpCode::ST_W, OperationFormat::SAD_2 );
  WIR_Registry::registerOpCode( OpCode::ST_W, OperationFormat::SAD_3 );
  WIR_Registry::registerOpCode( OpCode::ST_W, OperationFormat::SIC4D );
  WIR_Registry::registerOpCode( OpCode::STLCX, OperationFormat::C18ABSA );
  WIR_Registry::registerOpCode( OpCode::STLCX, OperationFormat::LABSA );
  WIR_Registry::registerOpCode( OpCode::STLCX, OperationFormat::AC10BOA );
  WIR_Registry::registerOpCode( OpCode::STUCX, OperationFormat::C18ABSAPSW );
  WIR_Registry::registerOpCode( OpCode::STUCX, OperationFormat::LABSAPSW );
  WIR_Registry::registerOpCode( OpCode::STUCX, OperationFormat::AC10BOAPSW );
  WIR_Registry::registerOpCode( OpCode::SUB, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::SUB, OperationFormat::SDD_2 );
  WIR_Registry::registerOpCode( OpCode::SUB, OperationFormat::SDID_1 );
  WIR_Registry::registerOpCode( OpCode::SUB, OperationFormat::SIDD );
  WIR_Registry::registerOpCode( OpCode::SUB_A, OperationFormat::AAA );
  WIR_Registry::registerOpCode( OpCode::SUB_A, OperationFormat::SSPC8 );
  WIR_Registry::registerOpCode( OpCode::SUB_B, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::SUB_F, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::SUB_H, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::SUBC, OperationFormat::DDDPSW_2 );
  WIR_Registry::registerOpCode( OpCode::SUBS, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::SUBS, OperationFormat::SDD_2 );
  WIR_Registry::registerOpCode( OpCode::SUBS_H, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::SUBS_HU, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::SUBS_U, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::SUBX, OperationFormat::DDDPSW_1 );
  WIR_Registry::registerOpCode( OpCode::SVLCX, OperationFormat::SYS );
  WIR_Registry::registerOpCode( OpCode::SWAP_W, OperationFormat::C18DABSA_2 );
  WIR_Registry::registerOpCode( OpCode::SWAP_W, OperationFormat::LDABSA_2 );
  WIR_Registry::registerOpCode( OpCode::SWAP_W, OperationFormat::AC10DBOA_2 );
  WIR_Registry::registerOpCode( OpCode::SWAP_W, OperationFormat::PDBRA_2 );
  WIR_Registry::registerOpCode( OpCode::SWAP_W, OperationFormat::PC10DCA_2 );
  WIR_Registry::registerOpCode( OpCode::SWAP_W, OperationFormat::AC10DPIA_2 );
  WIR_Registry::registerOpCode( OpCode::SYSCALL, OperationFormat::C9PSW );
  WIR_Registry::registerOpCode( OpCode::TLBDEMAP, OperationFormat::D );
  WIR_Registry::registerOpCode( OpCode::TLBFLUSH_A, OperationFormat::SYS );
  WIR_Registry::registerOpCode( OpCode::TLBFLUSH_B, OperationFormat::SYS );
  WIR_Registry::registerOpCode( OpCode::TLBMAP, OperationFormat::E );
  WIR_Registry::registerOpCode( OpCode::TLBPROBE_A, OperationFormat::D );
  WIR_Registry::registerOpCode( OpCode::TLBPROBE_I, OperationFormat::D );
  WIR_Registry::registerOpCode( OpCode::TRAPSV, OperationFormat::SYS );
  WIR_Registry::registerOpCode( OpCode::TRAPV, OperationFormat::SYS );
  WIR_Registry::registerOpCode( OpCode::UNPACK, OperationFormat::ED );
  WIR_Registry::registerOpCode( OpCode::UPDFL, OperationFormat::D );
  WIR_Registry::registerOpCode( OpCode::UTOF, OperationFormat::DD );
  WIR_Registry::registerOpCode( OpCode::XNOR, OperationFormat::DDC9_2 );
  WIR_Registry::registerOpCode( OpCode::XNOR, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::XNOR_T, OperationFormat::DDC5DC5_1 );
  WIR_Registry::registerOpCode( OpCode::XOR, OperationFormat::DDC9_2 );
  WIR_Registry::registerOpCode( OpCode::XOR, OperationFormat::DDD_1 );
  WIR_Registry::registerOpCode( OpCode::XOR, OperationFormat::SDD_2 );
  WIR_Registry::registerOpCode( OpCode::XOR_EQ, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::XOR_EQ, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::XOR_GE, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::XOR_GE, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::XOR_GE_U, OperationFormat::DDC9_4 );
  WIR_Registry::registerOpCode( OpCode::XOR_GE_U, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::XOR_LT, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::XOR_LT, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::XOR_LT_U, OperationFormat::DDC9_4 );
  WIR_Registry::registerOpCode( OpCode::XOR_LT_U, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::XOR_NE, OperationFormat::DDC9_3 );
  WIR_Registry::registerOpCode( OpCode::XOR_NE, OperationFormat::DDD_2 );
  WIR_Registry::registerOpCode( OpCode::XOR_T, OperationFormat::DDC5DC5_1 );


  //
  // Register this current processor model.
  //

  WIR_Registry::registerProcessor( TC13() );


  //
  // Finally, initialize derived processor models.
  //

  TC131::init();
};


/*
  Access to physical address register A0 (global address register).
*/
const TC_ARegP &TC13::A0( void ) const
{
  DSTART( "const TC_ARegP& TC13::A0() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 0 ).get() ) );
};


/*
  Access to physical address register A1 (global address register).
*/
const TC_ARegP &TC13::A1( void ) const
{
  DSTART( "const TC_ARegP& TC13::A1() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 1 ).get() ) );
};


/*
  Access to physical address register A2.
*/
const TC_ARegP &TC13::A2( void ) const
{
  DSTART( "const TC_ARegP& TC13::A2() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 2 ).get() ) );
};


/*
  Access to physical address register A3.
*/
const TC_ARegP &TC13::A3( void ) const
{
  DSTART( "const TC_ARegP& TC13::A3() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 3 ).get() ) );
};


/*
  Access to physical address register A4.
*/
const TC_ARegP &TC13::A4( void ) const
{
  DSTART( "const TC_ARegP& TC13::A4() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 4 ).get() ) );
};


/*
  Access to physical address register A5.
*/
const TC_ARegP &TC13::A5( void ) const
{
  DSTART( "const TC_ARegP& TC13::A5() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 5 ).get() ) );
};


/*
  Access to physical address register A6.
*/
const TC_ARegP &TC13::A6( void ) const
{
  DSTART( "const TC_ARegP& TC13::A6() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 6 ).get() ) );
};


/*
  Access to physical address register A7.
*/
const TC_ARegP &TC13::A7( void ) const
{
  DSTART( "const TC_ARegP& TC13::A7() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 7 ).get() ) );
};


/*
  Access to physical address register A8 (global address register).
*/
const TC_ARegP &TC13::A8( void ) const
{
  DSTART( "const TC_ARegP& TC13::A8() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 8 ).get() ) );
};


/*
  Access to physical address register A9 (global address register).
*/
const TC_ARegP &TC13::A9( void ) const
{
  DSTART( "const TC_ARegP& TC13::A9() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 9 ).get() ) );
};


/*
  Access to physical address register A10 (stack pointer).
*/
const TC_ARegP &TC13::A10( void ) const
{
  DSTART( "const TC_ARegP& TC13::A10() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 10 ).get() ) );
};


/*
  Access to physical address register A11 (return address pointer).
*/
const TC_ARegP &TC13::A11( void ) const
{
  DSTART( "const TC_ARegP& TC13::A11() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 11 ).get() ) );
};


/*
  Access to physical address register A12.
*/
const TC_ARegP &TC13::A12( void ) const
{
  DSTART( "const TC_ARegP& TC13::A12() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 12 ).get() ) );
};


/*
  Access to physical address register A13.
*/
const TC_ARegP &TC13::A13( void ) const
{
  DSTART( "const TC_ARegP& TC13::A13() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 13 ).get() ) );
};


/*
  Access to physical address register A14.
*/
const TC_ARegP &TC13::A14( void ) const
{
  DSTART( "const TC_ARegP& TC13::A14() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 14 ).get() ) );
};


/*
  Access to physical address register A15 (implicit address register).
*/
const TC_ARegP &TC13::A15( void ) const
{
  DSTART( "const TC_ARegP& TC13::A15() const" );

  return( dynamic_cast<TC_ARegP &>( mPhRegReferences.at( 15 ).get() ) );
};


/*
  Access to stack pointer.
*/
const TC_ARegP &TC13::SP( void ) const
{
  DSTART( "const TC_ARegP& TC13::SP() const" );

  return( A10() );
};


/*
  Access to return address pointer.
*/
const TC_ARegP &TC13::RA( void ) const
{
  DSTART( "const TC_ARegP& TC13::RA() const" );

  return( A11() );
};


/*
  Access to physical data register D0.
*/
const TC_DRegP &TC13::D0( void ) const
{
  DSTART( "const TC_DRegP& TC13::D0() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 16 ).get() ) );
};


/*
  Access to physical data register D1.
*/
const TC_DRegP &TC13::D1( void ) const
{
  DSTART( "const TC_DRegP& TC13::D1() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 17 ).get() ) );
};


/*
  Access to physical data register D2.
*/
const TC_DRegP &TC13::D2( void ) const
{
  DSTART( "const TC_DRegP& TC13::D2() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 18 ).get() ) );
};


/*
  Access to physical data register D3.
*/
const TC_DRegP &TC13::D3( void ) const
{
  DSTART( "const TC_DRegP& TC13::D3() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 19 ).get() ) );
};


/*
  Access to physical data register D4.
*/
const TC_DRegP &TC13::D4( void ) const
{
  DSTART( "const TC_DRegP& TC13::D4() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 20 ).get() ) );
};


/*
  Access to physical data register D5.
*/
const TC_DRegP &TC13::D5( void ) const
{
  DSTART( "const TC_DRegP& TC13::D5() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 21 ).get() ) );
};


/*
  Access to physical data register D6.
*/
const TC_DRegP &TC13::D6( void ) const
{
  DSTART( "const TC_DRegP& TC13::D6() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 22 ).get() ) );
};


/*
  Access to physical data register D7.
*/
const TC_DRegP &TC13::D7( void ) const
{
  DSTART( "const TC_DRegP& TC13::D7() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 23 ).get() ) );
};


/*
  Access to physical data register D8.
*/
const TC_DRegP &TC13::D8( void ) const
{
  DSTART( "const TC_DRegP& TC13::D8() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 24 ).get() ) );
};


/*
  Access to physical data register D9.
*/
const TC_DRegP &TC13::D9( void ) const
{
  DSTART( "const TC_DRegP& TC13::D9() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 25 ).get() ) );
};


/*
  Access to physical data register D10.
*/
const TC_DRegP &TC13::D10( void ) const
{
  DSTART( "const TC_DRegP& TC13::D10() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 26 ).get() ) );
};


/*
  Access to physical data register D11.
*/
const TC_DRegP &TC13::D11( void ) const
{
  DSTART( "const TC_DRegP& TC13::D11() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 27 ).get() ) );
};


/*
  Access to physical data register D12.
*/
const TC_DRegP &TC13::D12( void ) const
{
  DSTART( "const TC_DRegP& TC13::D12() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 28 ).get() ) );
};


/*
  Access to physical data register D13.
*/
const TC_DRegP &TC13::D13( void ) const
{
  DSTART( "const TC_DRegP& TC13::D13() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 29 ).get() ) );
};


/*
  Access to physical data register D14.
*/
const TC_DRegP &TC13::D14( void ) const
{
  DSTART( "const TC_DRegP& TC13::D14() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 30 ).get() ) );
};


/*
  Access to physical data register D15 (implicit data register).
*/
const TC_DRegP &TC13::D15( void ) const
{
  DSTART( "const TC_DRegP& TC13::D15() const" );

  return( dynamic_cast<TC_DRegP &>( mPhRegReferences.at( 31 ).get() ) );
};


/*
  Access to physical extended data register E0.
*/
const TC_ERegP &TC13::E0( void ) const
{
  DSTART( "const TC_ERegP& TC13::E0() const" );

  return( dynamic_cast<TC_ERegP &>( mPhRegReferences.at( 32 ).get() ) );
};


/*
  Access to physical extended data register E2.
*/
const TC_ERegP &TC13::E2( void ) const
{
  DSTART( "const TC_ERegP& TC13::E2() const" );

  return( dynamic_cast<TC_ERegP &>( mPhRegReferences.at( 33 ).get() ) );
};


/*
  Access to physical extended data register E4.
*/
const TC_ERegP &TC13::E4( void ) const
{
  DSTART( "const TC_ERegP& TC13::E4() const" );

  return( dynamic_cast<TC_ERegP &>( mPhRegReferences.at( 34 ).get() ) );
};


/*
  Access to physical extended data register E6.
*/
const TC_ERegP &TC13::E6( void ) const
{
  DSTART( "const TC_ERegP& TC13::E6() const" );

  return( dynamic_cast<TC_ERegP &>( mPhRegReferences.at( 35 ).get() ) );
};


/*
  Access to physical extended data register E8.
*/
const TC_ERegP &TC13::E8( void ) const
{
  DSTART( "const TC_ERegP& TC13::E8() const" );

  return( dynamic_cast<TC_ERegP &>( mPhRegReferences.at( 36 ).get() ) );
};


/*
  Access to physical extended data register E10.
*/
const TC_ERegP &TC13::E10( void ) const
{
  DSTART( "const TC_ERegP& TC13::E10() const" );

  return( dynamic_cast<TC_ERegP &>( mPhRegReferences.at( 37 ).get() ) );
};


/*
  Access to physical extended data register E12.
*/
const TC_ERegP &TC13::E12( void ) const
{
  DSTART( "const TC_ERegP& TC13::E12() const" );

  return( dynamic_cast<TC_ERegP &>( mPhRegReferences.at( 38 ).get() ) );
};


/*
  Access to physical extended data register E14.
*/
const TC_ERegP &TC13::E14( void ) const
{
  DSTART( "const TC_ERegP& TC13::E14() const" );

  return( dynamic_cast<TC_ERegP &>( mPhRegReferences.at( 39 ).get() ) );
};


/*
  Access to physical extended address register P0.
*/
const TC_PRegP &TC13::P0( void ) const
{
  DSTART( "const TC_PRegP& TC13::P0() const" );

  return( dynamic_cast<TC_PRegP &>( mPhRegReferences.at( 40 ).get() ) );
};


/*
  Access to physical extended address register P2.
*/
const TC_PRegP &TC13::P2( void ) const
{
  DSTART( "const TC_PRegP& TC13::P2() const" );

  return( dynamic_cast<TC_PRegP &>( mPhRegReferences.at( 41 ).get() ) );
};


/*
  Access to physical extended address register P4.
*/
const TC_PRegP &TC13::P4( void ) const
{
  DSTART( "const TC_PRegP& TC13::P4() const" );

  return( dynamic_cast<TC_PRegP &>( mPhRegReferences.at( 42 ).get() ) );
};


/*
  Access to physical extended address register P6.
*/
const TC_PRegP &TC13::P6( void ) const
{
  DSTART( "const TC_PRegP& TC13::P6() const" );

  return( dynamic_cast<TC_PRegP &>( mPhRegReferences.at( 43 ).get() ) );
};


/*
  Access to physical extended address register P8.
*/
const TC_PRegP &TC13::P8( void ) const
{
  DSTART( "const TC_PRegP& TC13::P8() const" );

  return( dynamic_cast<TC_PRegP &>( mPhRegReferences.at( 44 ).get() ) );
};


/*
  Access to physical extended address register P10.
*/
const TC_PRegP &TC13::P10( void ) const
{
  DSTART( "const TC_PRegP& TC13::P10() const" );

  return( dynamic_cast<TC_PRegP &>( mPhRegReferences.at( 45 ).get() ) );
};


/*
  Access to physical extended address register P12.
*/
const TC_PRegP &TC13::P12( void ) const
{
  DSTART( "const TC_PRegP& TC13::P12() const" );

  return( dynamic_cast<TC_PRegP &>( mPhRegReferences.at( 46 ).get() ) );
};


/*
  Access to physical extended address register P14.
*/
const TC_PRegP &TC13::P14( void ) const
{
  DSTART( "const TC_PRegP& TC13::P14() const" );

  return( dynamic_cast<TC_PRegP &>( mPhRegReferences.at( 47 ).get() ) );
};


/*
  Access to physical Carry bit of Processor Status Word PSW.
*/
const TC_PSWBit &TC13::PSW_C( void ) const
{
  DSTART( "const TC_PSWBit& TC13::PSW_C() const" );

  return( dynamic_cast<TC_PSWBit &>( mPhRegReferences.at( 48 ).get() ) );
};


/*
  isSP checks whether the specified register is the TriCore's stack pointer.
*/
bool TC13::isSP( const WIR_BaseRegister &r )
{
  DSTART( "static bool TC13::isSP(const WIR_BaseRegister&)" );

  if ( r.isVirtual() ) {
    auto &vr = dynamic_cast<const WIR_VirtualRegister &>( r );

    if ( vr.isPrecolored() )
      return( vr.getPrecolor().isStackPointer() );
  } else {
    auto &pr = dynamic_cast<const WIR_PhysicalRegister &>( r );

    return( pr.isStackPointer() );
  }

  return( false );
};


/*
  isRA checks whether the specified register is the TriCore's return address
  register A11.
*/
bool TC13::isRA( const WIR_BaseRegister &r )
{
  DSTART( "static bool TC13::isRA(const WIR_BaseRegister&)" );

  if ( r.isVirtual() ) {
    auto &vr = dynamic_cast<const WIR_VirtualRegister &>( r );

    if ( vr.isPrecolored() && ( vr.getPrecolor().getName() == "a11" ) )
      return( true );
  } else
    return( r.getName() == "a11" );

  return( false );
};


/*
  isA15 checks whether the specified register is the TriCore's implicit address
  register A15.
*/
bool TC13::isA15( const WIR_BaseRegister &r )
{
  DSTART( "static bool TC13::isA15(const WIR_BaseRegister&)" );

  if ( r.isVirtual() ) {
    auto &vr = dynamic_cast<const WIR_VirtualRegister &>( r );

    if ( vr.isPrecolored() && ( vr.getPrecolor().getName() == "a15" ) )
      return( true );
  } else
    return( r.getName() == "a15" );

  return( false );
};


/*
  isD15 checks whether the specified register is the TriCore's implicit data
  register D15.
*/
bool TC13::isD15( const WIR_BaseRegister &r )
{
  DSTART( "static bool TC13::isD15(const WIR_BaseRegister&)" );

  if ( r.isVirtual() ) {
    auto &vr = dynamic_cast<const WIR_VirtualRegister &>( r );

    if ( vr.isPrecolored() && ( vr.getPrecolor().getName() == "d15" ) )
      return( true );
  } else
    return( r.getName() == "d15" );

  return( false );
};


/*
  isPSW_C checks whether the specified register is the TriCore's carry bit of
  the Processor Status Word (PSW).
*/
bool TC13::isPSW_C( const WIR_BaseRegister &r )
{
  DSTART( "static bool TC13::isPSW_C(const WIR_BaseRegister&)" );

  if ( r.isPhysical() )
    return( r.getName() == "PSW.C" );

  return( false );
};


/*
  isLCReg checks whether the specified register is part of the TriCore's lower
  register context.
*/
bool TC13::isLCReg( const WIR_BaseRegister &r )
{
  DSTART( "static bool TC13::isLCReg(const WIR_BaseRegister&)" );

  const set<string> lcRegs {
    "a2", "a3", "a4", "a5", "a6", "a7",
    "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7",
    "e0", "e2", "e4", "e6",
    "p2", "p4", "p6" };

  if ( r.isVirtual() ) {
    auto &vr = dynamic_cast<const WIR_VirtualRegister &>( r );

    if ( vr.isPrecolored() && lcRegs.count( vr.getPrecolor().getName() ) )
      return( true );
  } else
    return( lcRegs.count( r.getName() ) == 1 );

  return( false );
};


/*
  isIP determines whether the given operation is a TriCore IP operation.

  See: TriCore DSP Optimization Guide - Part 1: Instruction Set, Chapter 13.1.3.
*/
bool TC13::isIP( const WIR_Operation &o )
{
  DSTART( "static bool TC13::isIP(const WIR_Operation&)" );

  auto m = o.getOpCode();

  if ( ( m == OpCode::ABS ) || ( m == OpCode::ABS_B ) ||
       ( m == OpCode::ABS_H ) || ( m == OpCode::ABSDIF ) ||
       ( m == OpCode::ABSDIF_B ) || ( m == OpCode::ABSDIF_H ) ||
       ( m == OpCode::ABSDIFS ) || ( m == OpCode::ABSDIFS_H ) ||
       ( m == OpCode::ABSS ) || ( m == OpCode::ABSS_H ) ||
       ( m == OpCode::ADD ) || ( m == OpCode::ADDC ) ||
       ( m == OpCode::ADDI ) || ( m == OpCode::ADDIH ) ||
       ( m == OpCode::ADDS ) || ( m == OpCode::ADDS_H ) ||
       ( m == OpCode::ADDS_HU ) || ( m == OpCode::ADDS_U ) ||
       ( m == OpCode::ADDX ) || ( m == OpCode::AND ) ||
       ( m == OpCode::AND_AND_T ) || ( m == OpCode::AND_ANDN_T ) ||
       ( m == OpCode::AND_EQ ) || ( m == OpCode::AND_GE ) ||
       ( m == OpCode::AND_GE_U ) || ( m == OpCode::AND_LT ) ||
       ( m == OpCode::AND_LT_U ) || ( m == OpCode::AND_NE ) ||
       ( m == OpCode::AND_NOR_T ) || ( m == OpCode::AND_OR_T ) ||
       ( m == OpCode::AND_T ) || ( m == OpCode::ANDN ) ||
       ( m == OpCode::ANDN_T ) || ( m == OpCode::BMERGE ) ||
       ( m == OpCode::BSPLIT ) || ( m == OpCode::CADD ) ||
       ( m == OpCode::CADDN ) || ( m == OpCode::CLO ) ||
       ( m == OpCode::CLO_H ) || ( m == OpCode::CLS ) ||
       ( m == OpCode::CLS_H ) || ( m == OpCode::CLZ ) ||
       ( m == OpCode::CLZ_H ) || ( m == OpCode::CMOV ) ||
       ( m == OpCode::CMOVN ) || ( m == OpCode::CSUB ) ||
       ( m == OpCode::CSUBN ) || ( m == OpCode::DEXTR ) ||
       ( m == OpCode::EQ ) || ( m == OpCode::EQ_B ) ||
       ( m == OpCode::EQ_H ) || ( m == OpCode::EQ_W ) ||
       ( m == OpCode::EQANY_B ) || ( m == OpCode::EQANY_H ) ||
       ( m == OpCode::EXTR ) || ( m == OpCode::EXTR_U ) ||
       ( m == OpCode::GE ) || ( m == OpCode::GE_U ) ||
       ( m == OpCode::INS_T ) || ( m == OpCode::INSERT ) ||
       ( m == OpCode::INSN_T ) || ( m == OpCode::LT ) ||
       ( m == OpCode::LT_B ) || ( m == OpCode::LT_BU ) ||
       ( m == OpCode::LT_H ) || ( m == OpCode::LT_HU ) ||
       ( m == OpCode::LT_U ) || ( m == OpCode::LT_W ) ||
       ( m == OpCode::LT_WU ) || ( m == OpCode::MADD ) ||
       ( m == OpCode::MADD_F ) || ( m == OpCode::MADD_H ) ||
       ( m == OpCode::MADD_Q ) || ( m == OpCode::MADD_U ) ||
       ( m == OpCode::MADDM_H ) || ( m == OpCode::MADDMS_H ) ||
       ( m == OpCode::MADDR_H ) || ( m == OpCode::MADDR_Q ) ||
       ( m == OpCode::MADDRS_H ) || ( m == OpCode::MADDRS_Q ) ||
       ( m == OpCode::MADDS ) || ( m == OpCode::MADDS_H ) ||
       ( m == OpCode::MADDS_Q ) || ( m == OpCode::MADDS_U ) ||
       ( m == OpCode::MADDSU_H ) || ( m == OpCode::MADDSUM_H ) ||
       ( m == OpCode::MADDSUMS_H ) || ( m == OpCode::MADDSUR_H ) ||
       ( m == OpCode::MADDSURS_H ) || ( m == OpCode::MADDSUS_H ) ||
       ( m == OpCode::MAX ) || ( m == OpCode::MAX_B ) ||
       ( m == OpCode::MAX_BU ) || ( m == OpCode::MAX_H ) ||
       ( m == OpCode::MAX_HU ) || ( m == OpCode::MAX_U ) ||
       ( m == OpCode::MIN ) || ( m == OpCode::MIN_B ) ||
       ( m == OpCode::MIN_BU ) || ( m == OpCode::MIN_H ) ||
       ( m == OpCode::MIN_HU ) || ( m == OpCode::MIN_U ) ||
       ( m == OpCode::MOV ) || ( m == OpCode::MOV_RR ) ||
       ( m == OpCode::MOV_A ) || ( m == OpCode::MOV_D ) ||
       ( m == OpCode::MOV_U ) || ( m == OpCode::MOVH ) ||
       ( m == OpCode::MSUB ) || ( m == OpCode::MSUB_F ) ||
       ( m == OpCode::MSUB_H ) || ( m == OpCode::MSUB_Q ) ||
       ( m == OpCode::MSUB_U ) || ( m == OpCode::MSUBAD_H ) ||
       ( m == OpCode::MSUBADM_H ) || ( m == OpCode::MSUBADMS_H ) ||
       ( m == OpCode::MSUBADR_H ) || ( m == OpCode::MSUBADRS_H ) ||
       ( m == OpCode::MSUBADS_H ) || ( m == OpCode::MSUBM_H ) ||
       ( m == OpCode::MSUBMS_H ) || ( m == OpCode::MSUBR_H ) ||
       ( m == OpCode::MSUBR_Q ) || ( m == OpCode::MSUBRS_H ) ||
       ( m == OpCode::MSUBRS_Q ) || ( m == OpCode::MSUBS ) ||
       ( m == OpCode::MSUBS_H ) || ( m == OpCode::MSUBS_Q ) ||
       ( m == OpCode::MSUBS_U ) || ( m == OpCode::MUL ) ||
       ( m == OpCode::MUL_F ) || ( m == OpCode::MUL_H ) ||
       ( m == OpCode::MUL_Q ) || ( m == OpCode::MUL_U ) ||
       ( m == OpCode::MULM_H ) || ( m == OpCode::MULR_H ) ||
       ( m == OpCode::MULR_Q ) || ( m == OpCode::MULS ) ||
       ( m == OpCode::MULS_U ) || ( m == OpCode::NAND ) ||
       ( m == OpCode::NAND_T ) || ( m == OpCode::NE ) ||
       ( m == OpCode::NOR ) || ( m == OpCode::NOR_T ) ||
       ( m == OpCode::OR ) || ( m == OpCode::OR_AND_T ) ||
       ( m == OpCode::OR_ANDN_T ) || ( m == OpCode::OR_EQ ) ||
       ( m == OpCode::OR_GE ) || ( m == OpCode::OR_GE_U ) ||
       ( m == OpCode::OR_LT ) || ( m == OpCode::OR_LT_U ) ||
       ( m == OpCode::OR_NE ) || ( m == OpCode::OR_NOR_T ) ||
       ( m == OpCode::OR_OR_T ) || ( m == OpCode::OR_T ) ||
       ( m == OpCode::ORN ) || ( m == OpCode::ORN_T ) ||
       ( m == OpCode::PACK ) || ( m == OpCode::PARITY ) ||
       ( m == OpCode::RSUB ) || ( m == OpCode::RSUBS ) ||
       ( m == OpCode::RSUBS_U ) || ( m == OpCode::SAT_B ) ||
       ( m == OpCode::SAT_BU ) || ( m == OpCode::SAT_H ) ||
       ( m == OpCode::SAT_HU ) || ( m == OpCode::SEL ) ||
       ( m == OpCode::SELN ) || ( m == OpCode::SH ) ||
       ( m == OpCode::SH_AND_T ) || ( m == OpCode::SH_ANDN_T ) ||
       ( m == OpCode::SH_EQ ) || ( m == OpCode::SH_GE ) ||
       ( m == OpCode::SH_GE_U ) || ( m == OpCode::SH_H ) ||
       ( m == OpCode::SH_LT ) || ( m == OpCode::SH_LT_U ) ||
       ( m == OpCode::SH_NAND_T ) || ( m == OpCode::SH_NE ) ||
       ( m == OpCode::SH_NOR_T ) || ( m == OpCode::SH_OR_T ) ||
       ( m == OpCode::SH_ORN_T ) || ( m == OpCode::SH_XNOR_T ) ||
       ( m == OpCode::SH_XOR_T ) || ( m == OpCode::SHA ) ||
       ( m == OpCode::SHA_H ) || ( m == OpCode::SHAS ) ||
       ( m == OpCode::SUB ) || ( m == OpCode::SUB_B ) ||
       ( m == OpCode::SUB_H ) || ( m == OpCode::SUBC ) ||
       ( m == OpCode::SUBS ) || ( m == OpCode::SUBS_H ) ||
       ( m == OpCode::SUBS_HU ) || ( m == OpCode::SUBS_U ) ||
       ( m == OpCode::SUBX ) || ( m == OpCode::UNPACK ) ||
       ( m == OpCode::XNOR ) || ( m == OpCode::XNOR_T ) ||
       ( m == OpCode::XOR ) || ( m == OpCode::XOR_EQ ) ||
       ( m == OpCode::XOR_GE ) || ( m == OpCode::XOR_GE_U ) ||
       ( m == OpCode::XOR_LT ) || ( m == OpCode::XOR_LT_U ) ||
       ( m == OpCode::XOR_NE ) || ( m == OpCode::XOR_T ) )
    return( true );

  return( false );
};


/*
  isLP determines whether the given operation is a TriCore LP operation.
*/
bool TC13::isLP( const WIR_Operation &o )
{
  DSTART( "static bool TC13::isLP(const WIR_Operation&)" );

  auto m = o.getOpCode();

  if ( ( m == OpCode::LOOP ) || ( m == OpCode::LOOPU ) )
    return( true );

  return( false );
};


/*
  isLS determines whether the given operation is a TriCore LS operation.

  See: TriCore DSP Optimization Guide - Part 1: Instruction Set, Chapter 13.1.3.
*/
bool TC13::isLS( const WIR_Operation &o )
{
  DSTART( "static bool TC13::isLS(const WIR_Operation&)" );

  auto m = o.getOpCode();

  if ( ( m == OpCode::ADD_A ) || ( m == OpCode::EQ_A ) ||
       ( m == OpCode::EQZ_A ) || ( m == OpCode::GE_A ) ||
       ( m == OpCode::J ) || ( m == OpCode::LD_A ) ||
       ( m == OpCode::LD_B ) || ( m == OpCode::LD_BU ) ||
       ( m == OpCode::LD_D ) || ( m == OpCode::LD_DA ) ||
       ( m == OpCode::LD_H ) || ( m == OpCode::LD_HU ) ||
       ( m == OpCode::LD_Q ) || ( m == OpCode::LD_W ) ||
       ( m == OpCode::LDLCX ) || ( m == OpCode::LDMST ) ||
       ( m == OpCode::LDUCX ) || ( m == OpCode::LEA ) ||
       ( m == OpCode::LT_A ) || ( m == OpCode::MOV_A ) ||
       ( m == OpCode::MOV_AA ) || ( m == OpCode::MOVH_A ) ||
       ( m == OpCode::MOV_D ) || ( m == OpCode::NE_A ) ||
       ( m == OpCode::NEZ_A ) || ( m == OpCode::NOP ) ||
       ( m == OpCode::ST_A ) || ( m == OpCode::ST_B ) ||
       ( m == OpCode::ST_D ) || ( m == OpCode::ST_DA ) ||
       ( m == OpCode::ST_H ) || ( m == OpCode::ST_Q ) ||
       ( m == OpCode::ST_T ) || ( m == OpCode::ST_W ) ||
       ( m == OpCode::STLCX ) || ( m == OpCode::STUCX ) ||
       ( m == OpCode::SUB_A ) )
    return( true );

  return( false );
};


/*
  adjustStack allocates additional space in the specified function's stack frame
  and adjusts all stack-related memory accesses accordingly.

  According to the TriCore EABI (section 2.2.2.1), the stack pointer points
  to the bottom (low address) of the stack frame. The stack pointer
  alignment is 8 bytes. The argument overflow area for outgoing arguments
  must be located at the bottom (low address end) of the frame, with the
  first overflow argument at zero offset from the stack pointer:

  (Stack
    growing
    direction)
        |
        |   +-------------------------+      (high address)
        |   | Local Variables Frame 1 |
        |   +-------------------------+
        |   | Argument Overflow Area, |
        |   | Function 2 Arguments    |      (first argument passed on stack)
        |   +-------------------------+
        |   | Local Variables Frame 2 |
        |   +-------------------------+
        |   | Argument Overflow Area, |
        |   | Function 3 Arguments    |
        |   +-------------------------+ <--- Stack Pointer (SP) at entry
        V   | Local Variables Frame 3 |      (CALL) to Function 3
            +-------------------------+
            | Argument Overflow Area  |
            +-------------------------+      (low address)
*/
void TC13::adjustStack( WIR_Function &f, int size,
                        const std::list<std::reference_wrapper<WIR_Instruction>> &insertedSpillCode )
{
  DSTART(
    "static void TC13::adjustStack(WIR_Function&, int, const list<reference_wrapper<WIR_Instruction> >&)" );

  DOUT(
    "Adjusting stack of function '" << f.getName() << "' by " << size <<
    " bytes." << endl );

  if ( size == 0 )
    return;

  // Align stack pointer to 8 bytes, see the comment of this function above.
  int remainder = size % 8;
  if ( remainder != 0 ) {
    size += 8 - remainder;
    DOUT( "Aligning to next 8-byte boundary of " << size << "." << endl );
  }

  // Determine the involved processor core.
  WIR_System &sys = f.getCompilationUnit().getSystem();
  const WIR_Section &sec = sys.findSymbol( f ).getSection();
  TC13 &tc = dynamic_cast<TC13 &>( sec.getProcessor() );
  auto &stackPointer = tc.SP();

  // Convert list insertedSpillCode into a set of IDs for convenience.
  set<WIR_id_t> spillCode;
  for ( WIR_Instruction &i : insertedSpillCode )
    spillCode.insert( i.getID() );

  // Determine first non-empty instruction and operation of f.
  WIR_Instruction *iid = nullptr;

  for ( WIR_BasicBlock &b : f ) {
    for ( WIR_Instruction &i : b )
      if ( !i.getOperations().empty() ) {
        iid = &i;
        break;
      }

    if ( iid != nullptr )
      break;
  }

  if ( iid == nullptr )
    return;

  set<WIR_id_t> sacIDs;
  auto &firstIns = *iid;
  WIR_Operation &oldSAC = firstIns.getOperations().front();

  // Determine potential second instruction for stack allocation.
  auto nextOp = firstIns.getOperations().end();
  bool nextOpFound = false;
  for ( auto it =
          std::next( firstIns.getBasicBlock().findInstruction( firstIns ) );
        it != firstIns.getBasicBlock().getInstructions().end(); ++it )
    if ( !it->get().getOperations().empty() ) {
      nextOp = it->get().getOperations().begin();
      nextOpFound = true;
      break;
    }

  // Test if f already has some stack-allocating code (SAC) and adjust it.
  if ( isStackPointerSUB( oldSAC ) ) {
    DOUT( "Adjusting SUB.A stack-allocating code." << endl );

    // SUB_A SAC.
    auto pos = oldSAC.getParameters().begin();
    ++pos;
    unsigned int oldStackFrameSize =
      dynamic_cast<TC_Const8_Unsigned &>( pos->get() ).getValue();

    if ( oldStackFrameSize + size <= TC_Const8_Unsigned::getMaxValue( 8 ) ) {
      oldSAC.replaceParameter(
        pos, TC_Const8_Unsigned( oldStackFrameSize + size ) );
      sacIDs.insert( oldSAC.getID() );
    } else
      sacIDs.insert(
        firstIns.replaceOperation(
          firstIns.getOperations().begin(),
          { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
            WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
            WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
            TC_Const16_Signed(
              (int) -( oldStackFrameSize + size ) ) } )->get().getID() );
  } else

  if ( nextOpFound &&
       ( ( isStackPointerADDIHA( oldSAC ) &&
           isStackPointerLEA( nextOp->get() ) ) ||
         ( isStackPointerADDIHA( nextOp->get() ) &&
           isStackPointerLEA( oldSAC ) ) ) ) {
    DOUT( "Adjusting ADDIH.A + LEA stack-allocating code." << endl );

    // ADDIH_A + LEA SAC (in any order).
    WIR_Operation &addiha =
      ( isStackPointerADDIHA( oldSAC ) ? oldSAC : nextOp->get() );
    WIR_Operation &lea =
      ( isStackPointerLEA( oldSAC ) ? oldSAC : nextOp->get() );

    auto addih_pos = std::prev( addiha.getParameters().end() );
    unsigned int addih_const =
      dynamic_cast<TC_Const16_Unsigned &>( addih_pos->get() ).getValue();

    auto lea_pos = std::prev( lea.getParameters().end() );
    signed int lea_const =
      dynamic_cast<TC_Const16_Signed &>( lea_pos->get() ).getValue();

    long oldStackFrameSize = ( addih_const * 0x10000 ) + lea_const;
    long newStackFrameSize = oldStackFrameSize - size;

    unsigned int addih_new_const =
      ( (newStackFrameSize + 0x8000) / (unsigned int) 0x10000 ) & 0xFFFF;
    int lea_new_const = (newStackFrameSize) & 0xFFFF;

    if ( lea_new_const > TC_Const16_Signed::getMaxValue( 16 ) )
      lea_new_const =
        TC_Const16_Signed::getMinValue( 16 ) - 1 +
        ( lea_new_const - TC_Const16_Signed::getMaxValue( 16 ) );
    else

    if ( lea_new_const < TC_Const16_Signed::getMinValue( 16 ) )
      lea_new_const =
        TC_Const16_Signed::getMaxValue( 16 ) +
        ( lea_new_const - TC_Const16_Signed::getMinValue( 16 ) );

    addiha.replaceParameter(
      addih_pos, TC_Const16_Unsigned( addih_new_const ) );
    lea.replaceParameter( lea_pos, TC_Const16_Signed( lea_new_const ) );

    sacIDs.insert( addiha.getID() );
    sacIDs.insert( lea.getID() );
  } else

  if ( isStackPointerLEA( oldSAC ) ) {
    DOUT( "Adjusting LEA stack-allocating code." << endl );

    // LEA SAC.
    auto lea_pos = std::prev( oldSAC.getParameters().end() );
    signed int oldStackFrameSize =
      dynamic_cast<TC_Const16_Signed &>( lea_pos->get() ).getValue();

    oldSAC.replaceParameter(
      lea_pos, TC_Const16_Signed( oldStackFrameSize - (int) size ) );
    sacIDs.insert( oldSAC.getID() );
  } else {
    // No SAC.
    auto &leai1 =
      f.getBasicBlocks().front().get().insertInstruction(
        f.getBasicBlocks().front().get().begin(), {} )->get();
    leai1.pushBackOperation(
      { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
        WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
        WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
        TC_Const16_Signed( (int) -size ) } );
    sacIDs.insert( leai1.getOperations().front().get().getID() );
    DOUT( "Inserting LEA stack-allocating code." << tricore << endl << leai1 );
  }

  // Adjust all old accesses to the stack.
  for ( WIR_BasicBlock &b : f )
    for ( auto it = b.begin(); it != b.end(); ++it ) {
      WIR_Instruction &i = it->get();

      // Check whether the current instruction contains parameters that are
      // marked as don't touch since they access the overflow area.
      bool isOverflowAccess = false;
      for ( WIR_Operation &o : i )
        for ( WIR_Parameter &p : o )
          if ( p.getDontOptimize() ) {
            isOverflowAccess = true;
            p.setDontOptimize( false );
          }

      // Adjust stack access only if it's not an access to the overflow region
      // or a regular spill instruction.
      if ( !isOverflowAccess && !spillCode.count( i.getID() ) &&
           !i.getOperations().empty() ) {
        WIR_Operation &o = i.getOperations().front().get();

        // Don't adjust the original operation for stack frame allocation again!
        if ( sacIDs.count( o.getID() ) )
          continue;

        auto &p = o.getExplicitParameters();

        // Adjust MOV.AA operations.
        if ( ( o.getOpCode() == TC13::OpCode::MOV_AA ) &&
             ( dynamic_cast<WIR_RegisterParameter &>(
                 p.back().get() ).getRegister() == stackPointer ) ) {
          DOUT( "Adjusting stack access of MOV.AA." << endl );

          // Replace MOV_AA with A10 as second parameter by an adjusted LEA.
          i.replaceOperation(
            i.getOperations().begin(),
            { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
              WIR_RegisterParameter(
                dynamic_cast<WIR_RegisterParameter &>( p.front().get() ) ),
              WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
              TC_Const16_Signed( size ) } );

          continue;
        }

        // Adjust ADDIH.A operations.
        if ( ( o.getOpCode() == TC13::OpCode::ADDIH_A ) &&
             ( dynamic_cast<WIR_RegisterParameter &>(
                 (++(p.begin()))->get() ).getRegister() == stackPointer ) ) {
          DOUT( "Adjusting stack access of ADDIH.A." << endl );

          // Adjust ADDIH.A with A10 as second parameter by adding an adjusted
          // LEA before.
          b.insertInstruction( it,
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                WIR_RegisterParameter(
                  dynamic_cast<WIR_RegisterParameter &>( p.front().get() ) ),
                WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                TC_Const16_Signed( size ) } } );

          o.replaceParameter(
            o.findParameter( std::next( p.begin() )->get() ),
            WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                p.front().get() ).getRegister(),
              WIR_Usage::use ) );

          continue;
        }

        // Adjust ST.A operations.
        if ( ( o.getOpCode() == TC13::OpCode::ST_A ) &&
             ( ( o.getOperationFormat() == TC13::OperationFormat::AC10ABOA ) ||
               ( o.getOperationFormat() == TC13::OperationFormat::PC10ACA ) ||
               (  o.getOperationFormat() == TC13::OperationFormat::AC10APIA ) ||
               (  o.getOperationFormat() == TC13::OperationFormat::SIC4A ) ) &&
             ( dynamic_cast<WIR_RegisterParameter &>(
                 p.back().get() ).getRegister() == stackPointer ) ) {
          DOUT( "Adjusting stack access of ST.A." << endl );

          // The following forms of ST.A are covered here:
          //
          //   ST.A [%axyz] const, %a10
          //
          // For all of them, it is valid to add the current spill stack size
          // to A10 temporarily.

          // We need to insert new LEAs surrounding the current ST.A.
          b.insertInstruction( it,
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
                WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                TC_Const16_Signed( size ) } } );
          b.insertInstruction( std::next( it ),
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
                WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                TC_Const16_Signed( (int) -size ) } } );

          continue;
        }

        // Adjust any kind of base+offset addressing using the stack pointer.

        // This lambda is used to obtain an unsigned constant argument value.
        auto getReg = [&]( unsigned int i ) -> WIR_BaseRegister & {
          return(
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( i ) ).getRegister() );
        };

        bool aac10boa =
          ( o.getOperationFormat() == TC13::OperationFormat::AAC10BOA ) &&
          isSP( getReg( 2 ) );
        bool aac16boa =
          ( o.getOperationFormat() == TC13::OperationFormat::AAC16BOA ) &&
          isSP( getReg( 2 ) );
        bool ac10aboa =
          ( o.getOperationFormat() == TC13::OperationFormat::AC10ABOA ) &&
          isSP( getReg( 1 ) );
        bool ac10boa =
          ( o.getOperationFormat() == TC13::OperationFormat::AC10BOA ) &&
          isSP( getReg( 1 ) );
        bool ac10dboa_1 =
          ( o.getOperationFormat() == TC13::OperationFormat::AC10DBOA_1 ) &&
          isSP( getReg( 1 ) );
        bool ac10dboa_2 =
          ( o.getOperationFormat() == TC13::OperationFormat::AC10DBOA_2 ) &&
          isSP( getReg( 1 ) );
        bool ac10eboa =
          ( o.getOperationFormat() == TC13::OperationFormat::AC10EBOA ) &&
          isSP( getReg( 1 ) );
        bool ac10pboa =
          ( o.getOperationFormat() == TC13::OperationFormat::AC10PBOA ) &&
          isSP( getReg( 1 ) );
        bool ac16dboa =
          ( o.getOperationFormat() == TC13::OperationFormat::AC16DBOA ) &&
          isSP( getReg( 1 ) );
        bool dac10boa =
          ( o.getOperationFormat() == TC13::OperationFormat::DAC10BOA ) &&
          isSP( getReg( 2 ) );
        bool dac16boa =
          ( o.getOperationFormat() == TC13::OperationFormat::DAC16BOA ) &&
          isSP( getReg( 2 ) );
        bool eac10boa =
          ( o.getOperationFormat() == TC13::OperationFormat::EAC10BOA ) &&
          isSP( getReg( 2 ) );
        bool pac10boa =
          ( o.getOperationFormat() == TC13::OperationFormat::PAC10BOA ) &&
          isSP( getReg( 2 ) );
        bool sispc10_1 =
          o.getOperationFormat() == TC13::OperationFormat::SISPC10_1;
        bool sispc10_2 =
          o.getOperationFormat() == TC13::OperationFormat::SISPC10_2;
        bool sspc10i_1 =
          o.getOperationFormat() == TC13::OperationFormat::SSPC10I_1;
        bool sspc10i_2 =
          o.getOperationFormat() == TC13::OperationFormat::SSPC10I_2;

        if ( aac10boa || aac16boa || ac10aboa || ac10boa || ac10dboa_1 ||
             ac10dboa_2 || ac10eboa || ac10pboa || ac16dboa || dac10boa ||
             dac16boa || eac10boa || pac10boa || sispc10_1 || sispc10_2 ||
             sspc10i_1 || sspc10i_2 ) {
          DOUT( "Adjusting stack access in " << o << endl );

          // Determine immediate parameter and current stack offset.
          auto pos = p.end();
          for ( auto pit = p.begin(); pit != p.end(); ++pit )
            if ( pit->get().getType() == WIR_ParameterType::imm ) {
              pos = pit;
              break;
            }
          auto &ip = dynamic_cast<WIR_BaseImmediateParameter &>( pos->get() );
          long long currentOffset =
            ip.isSigned() ? ip.getSignedValue() : ip.getUnsignedValue();

          // Replace SP-relative offset only if the offset is >= 0. Offsets < 0
          // used for passing arguments to called functions remain unchanged!
          if ( currentOffset >= 0 ) {
            long long newOffset = currentOffset + size;

            if ( ( o.getOpCode() == TC13::OpCode::LEA ) && aac10boa &&
                 ( newOffset > TC_Const10_Signed::getMaxValue( 10 ) ) &&
                 ( newOffset <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
              DOUT( "Replacing LEA (AAC10BOA) by LEA (AAC16BOA)." << endl );

              // Replace LEA with operation format AAC10BOA simply by LEA with
              // AAC16BOA.
              i.replaceOperation( i.getOperations().begin(),
                { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>( p.front().get() ) ),
                  WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                  TC_Const16_Signed( newOffset ) } );
            } else

            if ( ( o.getOpCode() == TC13::OpCode::LEA ) &&
                 ( newOffset > TC_Const16_Signed::getMaxValue( 16 ) ) ) {
              DOUT( "Replacing LEA by ADDIH.A + LEA." << endl );

              // Replace LEA by ADDIH.A + LEA.
              int const_hi =
                ( (newOffset + 0x8000) / (unsigned int) 0x10000 ) & 0xFFFF;
              int const_low = newOffset & 0xFFFF;

              if ( const_low > TC_Const16_Signed::getMaxValue( 16 ) )
                const_low =
                  TC_Const16_Signed::getMinValue( 16 ) - 1 +
                  ( const_low - TC_Const16_Signed::getMaxValue( 16 ) );
              else

              if ( const_low < TC_Const16_Signed::getMinValue( 16 ) )
                const_low =
                  TC_Const16_Signed::getMaxValue( 16 ) +
                  ( const_low - TC_Const16_Signed::getMinValue( 16 ) );

              b.insertInstruction( it,
                { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                    WIR_RegisterParameter(
                      dynamic_cast<WIR_RegisterParameter &>(
                        p.front().get() ) ),
                    WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                    TC_Const16_Unsigned( (unsigned int) const_hi ) } } );
              i.replaceOperation(
                i.getOperations().begin(),
                { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>( p.front().get() ) ),
                  WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                  TC_Const16_Signed( const_low ) } );
            } else

            if ( ( ( aac10boa || ac10aboa || ac10boa || ac10dboa_1 ||
                     ac10dboa_2 || ac10eboa || ac10pboa || dac10boa ||
                     eac10boa || pac10boa ) &&
                   ( newOffset > TC_Const10_Signed::getMaxValue( 10 ) ) ) ||
                 ( ( aac16boa || ac16dboa || dac16boa ) &&
                   ( newOffset > TC_Const16_Signed::getMaxValue( 16 ) ) ) ||
                 ( ( sispc10_1 || sispc10_2 || sspc10i_1 || sspc10i_2 ) &&
                   ( ( newOffset >
                       (int) TC_Const10_Unsigned::getMaxValue( 10 ) ) ||
                     ( newOffset % 4 != 0 ) ) ) ) {
              DOUT(
                "Generating two LEAs before/after the current operation." <<
                endl );

              // For the current operation, the adjusted offset is too large to
              // fit into the base+offset addressing mode. We need to insert new
              // LEAs surrounding the current operation.
              b.insertInstruction( it,
                { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                    WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
                    WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                    TC_Const16_Signed( newOffset ) } } );
              b.insertInstruction( std::next( it ),
                { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                    WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
                    WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                    TC_Const16_Signed( -newOffset ) } } );

              if ( sispc10_1 || sispc10_2 || sspc10i_1 || sspc10i_2 )
                dynamic_cast<TC_Const10_Unsigned &>( ip ).setValue( 0 );
              else

              if ( ip.getBitWidth() == 10 )
                dynamic_cast<TC_Const10_Signed &>( ip ).setValue( 0 );
              else
                dynamic_cast<TC_Const16_Signed &>( ip ).setValue( 0 );
            } else {
              DOUT( "Replacing stack offset parameter." << endl );

              // The adjusted SP-relative offset is small enough to still fit
              // into the current operation.
              if ( sispc10_1 || sispc10_2 || sspc10i_1 || sspc10i_2 )
                dynamic_cast<TC_Const10_Unsigned &>( ip ).setValue( newOffset );
              else

              if ( ip.getBitWidth() == 10 )
                dynamic_cast<TC_Const10_Signed &>( ip ).setValue( newOffset );
              else
                dynamic_cast<TC_Const16_Signed &>( ip ).setValue( newOffset );
            }
          }
        }
      }
    }
};


/*
  isStackPointerADDIHA returns whether the given operation is an ADDIH_A that
  modifies the stack pointer.
*/
bool TC13::isStackPointerADDIHA( const WIR_Operation &o )
{
  DSTART( "static bool TC13::isStackPointerADDIHA(const WIR_Operation&)" );

  if ( ( o.getOpCode() == TC13::OpCode::ADDIH_A ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::AAC16 ) &&
       isSP(
         dynamic_cast<WIR_RegisterParameter &>(
           o.getExplicitParameters().begin()->get() ).getRegister() ) &&
       isSP(
         dynamic_cast<WIR_RegisterParameter &>(
           std::next(
             o.getExplicitParameters().begin() )->get() ).getRegister() ) )
    return( true );

  return( false );
};


/*
  isStackPointerLEA returns whether the given operation is a LEA that modifies
  the stack pointer.
*/
bool TC13::isStackPointerLEA( const WIR_Operation &o )
{
  DSTART( "static bool TC13::isStackPointerLEA(const WIR_Operation&)" );

  if ( ( o.getOpCode() == TC13::OpCode::LEA ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::AAC10BOA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::AAC16BOA ) ) &&
       isSP(
         dynamic_cast<WIR_RegisterParameter &>(
           o.getExplicitParameters().begin()->get() ).getRegister() ) &&
       isSP(
         dynamic_cast<WIR_RegisterParameter &>(
           std::next(
             o.getExplicitParameters().begin() )->get() ).getRegister() ) )
    return( true );

  return( false );
};


/*
  isStackpointerSUB returns whether the given operation is a subtraction that
  modifies the stack pointer.
*/
bool TC13::isStackPointerSUB( const WIR_Operation &o )
{
  DSTART( "static bool TC13::isStackPointerSUB(const WIR_Operation&)" );

  if ( ( o.getOpCode() == TC13::OpCode::SUB_A ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::SSPC8 ) &&
       isSP(
         dynamic_cast<WIR_RegisterParameter &>(
           o.getExplicitParameters().begin()->get() ).getRegister() ) )
    return( true );

  return( false );
};

}       // namespace WIR
