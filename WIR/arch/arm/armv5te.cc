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
  @file armv5te.cc
  @brief This file implements the specific interface of the ARMv5TE instruction
         set architecture.

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
#include <arch/arm/armv5te.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for ARMv5TE-based processor architectures.
*/
ARMv5TE::ARMv5TE( void ) :
  ARMv5T {}
{
  DSTART( "ARMv5TE::ARMv5TE()" );

  // Specify the processor architecture modeled by this class.
  setISAName( "ARMv5TE" );

  // Create physical register pairs.
  addPhReg<ARMv5TE_PRegP>( "0" );
  dynamic_cast<ARMv5TE_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( R0(), R1() );

  addPhReg<ARMv5TE_PRegP>( "2" );
  dynamic_cast<ARMv5TE_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( R2(), R3() );

  addPhReg<ARMv5TE_PRegP>( "4" );
  dynamic_cast<ARMv5TE_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( R4(), R5() );

  addPhReg<ARMv5TE_PRegP>( "6" );
  dynamic_cast<ARMv5TE_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( R6(), R7() );

  addPhReg<ARMv5TE_PRegP>( "8" );
  dynamic_cast<ARMv5TE_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( R8(), R9() );

  addPhReg<ARMv5TE_PRegP>( "10" );
  dynamic_cast<ARMv5TE_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( R10(), R11() );

  addPhReg<ARMv5TE_PRegP>( "12" );
  dynamic_cast<ARMv5TE_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( R12(), R13() );

  addPhReg<ARMv5TE_PRegP>( "14" );
  dynamic_cast<ARMv5TE_PRegP &>(
    mPhRegReferences.back().get() ).addChilds( R14(), R15() );
};


/*
  Copy constructor.
*/
ARMv5TE::ARMv5TE( const ARMv5TE &__o ) :
  ARMv5T { __o }
{
  DSTART( "ARMv5TE::ARMv5TE(const ARMv5TE&)" );
};


/*
  Move constructor.
*/
ARMv5TE::ARMv5TE( ARMv5TE &&__o ) :
  ARMv5T { move( __o ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
ARMv5TE::~ARMv5TE( void )
{
  DSTART( "virtual ARMv5TE::~ARMv5TE()" );
};


/*
  Copy-assignment operator.
*/
ARMv5TE & ARMv5TE::operator = ( const ARMv5TE &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ARMv5T::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
ARMv5TE & ARMv5TE::operator = ( ARMv5TE &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ARMv5T::operator = ( move( __o ) );

  return( *this );
};


/*
  init performs some global initialization tasks for ARMv5TE-based processor
  architectures.

  This includes setting up the ARMv5TE machine operation formats and the
  assignment of valid operation formats to ARMv5TE opcodes.

  init shall be called globally by WIR_Init(). It shall only perform tasks that
  cannot be expressed as initializations of static class members (since the
  order of static initialization is unspecified in C++) and that thus require
  execution by active code.
*/
void ARMv5TE::init( void )
{
  DSTART( "static void ARMv5TE::init()" );


  //
  // Register ARMv5TE operation formats.
  //

  ARM_RegV *regV = new ARM_RegV;
  ARMv5TE_PRegV *pregV = new ARMv5TE_PRegV;

  WIR_Registry::registerOperationFormat(
    OperationFormat::CAORRS_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::p0 ),
      new ARM_Const3_CoprocessorOpcode( 1 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_StringParameter( "CRm" ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CAORRS_2,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::p0 ),
      new ARM_Const3_CoprocessorOpcode( 1 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_StringParameter( "CRm" ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CPARAC8_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::def ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::pre ),
      new WIR_RegisterParameter( *regV, WIR_Usage::defuse ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new ARM_Const8_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CPARAC8_2,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::pre ),
      new WIR_RegisterParameter( *regV, WIR_Usage::defuse ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new ARM_Const8_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CPARAR_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::def ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::pre ),
      new WIR_RegisterParameter( *regV, WIR_Usage::defuse ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CPARAR_2,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::pre ),
      new WIR_RegisterParameter( *regV, WIR_Usage::defuse ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CPRAC8_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new ARM_Const8_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CPRAC8_2,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new ARM_Const8_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CPRAR_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CPRAR_2,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *pregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RAC12_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new ARM_Const12_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RAR_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RAR_2,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RARAC60_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::lsr ),
      new ARM_Const6_Unsigned0( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RARC5_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new ARM_Const5_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RARC50_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new ARM_Const5_Unsigned0( 1 ) } );


  //
  // ARMv5TE opcode to operation format mapping.
  //

  WIR_Registry::registerOpCode( OpCode::LDRD, OperationFormat::CPARAC8_1 );
  WIR_Registry::registerOpCode( OpCode::LDRD, OperationFormat::CPARAR_1 );
  WIR_Registry::registerOpCode( OpCode::LDRD, OperationFormat::CPRAC8_1 );
  WIR_Registry::registerOpCode( OpCode::LDRD, OperationFormat::CPRAR_1 );
  WIR_Registry::registerOpCode( OpCode::MCRR, OperationFormat::CAORRS_2 );
  WIR_Registry::registerOpCode( OpCode::MRRC, OperationFormat::CAORRS_1 );
  WIR_Registry::registerOpCode( OpCode::PLD, OperationFormat::RAC12_1 );
  WIR_Registry::registerOpCode( OpCode::PLD, OperationFormat::RAR_1 );
  WIR_Registry::registerOpCode( OpCode::PLD, OperationFormat::RAR_2 );
  WIR_Registry::registerOpCode( OpCode::PLD, OperationFormat::RARAC60_1 );
  WIR_Registry::registerOpCode( OpCode::PLD, OperationFormat::RARC5_1 );
  WIR_Registry::registerOpCode( OpCode::PLD, OperationFormat::RARC50_1 );
  WIR_Registry::registerOpCode( OpCode::QADD, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::QDADD, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::QDSUB, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::QSUB, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLABB, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLABT, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLATB, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLATT, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLALBB, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLALBT, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLALTB, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLALTT, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLAWB, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLAWT, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMULBB, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMULBT, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMULTB, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMULTT, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMULWB, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMULWT, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::STRD, OperationFormat::CPARAC8_2 );
  WIR_Registry::registerOpCode( OpCode::STRD, OperationFormat::CPARAR_2 );
  WIR_Registry::registerOpCode( OpCode::STRD, OperationFormat::CPRAC8_2 );
  WIR_Registry::registerOpCode( OpCode::STRD, OperationFormat::CPRAR_2 );


  //
  // Register this current processor model.
  //

  WIR_Registry::registerProcessor( ARMv5TE() );
};


/*
  Access to physical register pair 0 (R0 + R1).
*/
const ARMv5TE_PRegP &ARMv5TE::P0( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( dynamic_cast<ARMv5TE_PRegP &>( mPhRegReferences.at( 16 ).get() ) );
};


/*
  Access to physical register pair 2 (R2 + R3).
*/
const ARMv5TE_PRegP &ARMv5TE::P2( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( dynamic_cast<ARMv5TE_PRegP &>( mPhRegReferences.at( 17 ).get() ) );
};


/*
  Access to physical register pair 4 (R4 + R5).
*/
const ARMv5TE_PRegP &ARMv5TE::P4( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( dynamic_cast<ARMv5TE_PRegP &>( mPhRegReferences.at( 18 ).get() ) );
};


/*
  Access to physical register pair 6 (R6 + R7).
*/
const ARMv5TE_PRegP &ARMv5TE::P6( void ) const
{
  DSTART( "const ARMv5TE_PRegP& ARMv5TE::P6() const" );

  return( dynamic_cast<ARMv5TE_PRegP &>( mPhRegReferences.at( 19 ).get() ) );
};


/*
  Access to physical register pair 8 (R8 + R9).
*/
const ARMv5TE_PRegP &ARMv5TE::P8( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( dynamic_cast<ARMv5TE_PRegP &>( mPhRegReferences.at( 20 ).get() ) );
};


/*
  Access to physical register pair 10 (R10 + R11).
*/
const ARMv5TE_PRegP &ARMv5TE::P10( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( dynamic_cast<ARMv5TE_PRegP &>( mPhRegReferences.at( 21 ).get() ) );
};


/*
  Access to physical register pair 12 (R12 + R13).
*/
const ARMv5TE_PRegP &ARMv5TE::P12( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( dynamic_cast<ARMv5TE_PRegP &>( mPhRegReferences.at( 22 ).get() ) );
};


/*
  Access to physical register pair 14 (R14 + R15).
*/
const ARMv5TE_PRegP &ARMv5TE::P14( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( dynamic_cast<ARMv5TE_PRegP &>( mPhRegReferences.at( 23 ).get() ) );
};


//
// Private class methods
//

/*
  clone creates a copy of a ARMv5TE processor.
*/
WIR_BaseProcessor *ARMv5TE::clone( void ) const
{
  DSTART( "virtual WIR_BaseProcessor* ARMv5TE::clone() const" );

  return( new ARMv5TE( *this ) );
};

}       // namespace WIR
