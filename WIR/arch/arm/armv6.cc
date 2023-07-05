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
  @file armv6.cc
  @brief This file implements the specific interface of the ARMv6 instruction
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
#include <arch/arm/armv6.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for ARMv6-based processor architectures.
*/
ARMv6::ARMv6( void ) :
  ARMv5TEJ {}
{
  DSTART( "ARMv6::ARMv6()" );

  // Specify the processor architecture modeled by this class.
  setISAName( "ARMv6" );
};


/*
  Copy constructor.
*/
ARMv6::ARMv6( const ARMv6 &__o ) :
  ARMv5TEJ { __o }
{
  DSTART( "ARMv6::ARMv6(const ARMv6&)" );
};


/*
  Move constructor.
*/
ARMv6::ARMv6( ARMv6 &&__o ) :
  ARMv5TEJ { move( __o ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
ARMv6::~ARMv6( void )
{
  DSTART( "virtual ARMv6::~ARMv6()" );
};


/*
  Copy-assignment operator.
*/
ARMv6 & ARMv6::operator = ( const ARMv6 &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ARMv5TEJ::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
ARMv6 & ARMv6::operator = ( ARMv6 &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ARMv5TEJ::operator = ( move( __o ) );

  return( *this );
};


/*
  init performs some global initialization tasks for ARMv6-based processor
  architectures.

  This includes setting up the ARMv6 machine operation formats and the
  assignment of valid operation formats to ARMv6 opcodes.

  init shall be called globally by WIR_Init(). It shall only perform tasks that
  cannot be expressed as initializations of static class members (since the
  order of static initialization is unspecified in C++) and that thus require
  execution by active code.
*/
void ARMv6::init( void )
{
  DSTART( "static void ARMv6::init()" );


  //
  // Register ARMv6 operation formats.
  //

  ARM_RegV *regV = new ARM_RegV;

  WIR_Registry::registerOperationFormat(
    OperationFormat::A_1,
    { new WIR_AddressingModeParameter( AddressingMode::cpsra ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::A_2,
    WIR_OperationFormat {
      new WIR_AddressingModeParameter( AddressingMode::be ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC5_1,
    { new WIR_AddressingModeParameter( AddressingMode::cpsra ),
      new ARM_Const5_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC5_2,
    { new WIR_AddressingModeParameter( AddressingMode::ia ),
      new ARM_Const5_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AC5_3,
    { new WIR_AddressingModeParameter( AddressingMode::ia ),
      new ARM_Const5_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AORRS_1,
    { new WIR_AddressingModeParameter( ARM_Base::AddressingMode::p0 ),
      new ARM_Const3_CoprocessorOpcode( 1 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_StringParameter( "CRm" ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AORRS_2,
    { new WIR_AddressingModeParameter( ARM_Base::AddressingMode::p0 ),
      new ARM_Const3_CoprocessorOpcode( 1 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_StringParameter( "CRm" ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AR_1,
    { new WIR_AddressingModeParameter( ARM_Base::AddressingMode::ea ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AR_2,
    { new WIR_AddressingModeParameter( ARM_Base::AddressingMode::ea ),
      new WIR_RegisterParameter( *regV, WIR_Usage::defuse ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::C5_1,
    { new ARM_Const5_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CRC4R_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new ARM_Const4_CoprocessorOpcode( 1 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CRC5SPR_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new ARM_Const5_SatPos( 1 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CRC60R_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new ARM_Const6_Unsigned0( 32 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CRC5RC5_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new ARM_Const5_Unsigned0( 31 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new ARM_Const5_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CRC5RC60_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new ARM_Const5_Unsigned0( 31 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new ARM_Const6_Unsigned0( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CRC60RC5_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new ARM_Const6_Unsigned0( 32 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new ARM_Const5_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CRC60RC60_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new ARM_Const6_Unsigned0( 32 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new ARM_Const6_Unsigned0( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CRR_7,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CRRA_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::ror0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CRRRA_1,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::ror0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CRRRC5_3,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new ARM_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CRRRR_5,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TA_1,
    { new WIR_AddressingModeParameter( AddressingMode::cpsra ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TA_2,
    { new WIR_AddressingModeParameter( AddressingMode::be ) } );


  //
  // ARMv6 opcode to operation format mapping.
  //

  WIR_Registry::registerOpCode( OpCode::CPS, OperationFormat::C5_1 );
  WIR_Registry::registerOpCode( OpCode::CPSID, OperationFormat::A_1 );
  WIR_Registry::registerOpCode( OpCode::CPSID, OperationFormat::AC5_1 );
  WIR_Registry::registerOpCode( OpCode::CPSID, OperationFormat::TA_1 );
  WIR_Registry::registerOpCode( OpCode::CPSIE, OperationFormat::A_1 );
  WIR_Registry::registerOpCode( OpCode::CPSIE, OperationFormat::AC5_1 );
  WIR_Registry::registerOpCode( OpCode::CPSIE, OperationFormat::TA_1 );
  WIR_Registry::registerOpCode( OpCode::CPY, OperationFormat::CRR_1 );
  WIR_Registry::registerOpCode( OpCode::CPY, OperationFormat::TRR_2 );
  WIR_Registry::registerOpCode( OpCode::LDREX, OperationFormat::CRR_7 );
  WIR_Registry::registerOpCode( OpCode::MCRR2, OperationFormat::AORRS_2 );
  WIR_Registry::registerOpCode( OpCode::MRRC2, OperationFormat::AORRS_1 );
  WIR_Registry::registerOpCode( OpCode::PKHBT, OperationFormat::CRRRC5_1 );
  WIR_Registry::registerOpCode( OpCode::PKHTB, OperationFormat::CRRRC5_3 );
  WIR_Registry::registerOpCode( OpCode::QADD16, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::QADD8, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::QADDSUBX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::QSUB16, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::QSUB8, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::QSUBADDX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::REV, OperationFormat::CRR_1 );
  WIR_Registry::registerOpCode( OpCode::REV, OperationFormat::TRR_1 );
  WIR_Registry::registerOpCode( OpCode::REV16, OperationFormat::CRR_1 );
  WIR_Registry::registerOpCode( OpCode::REV16, OperationFormat::TRR_1 );
  WIR_Registry::registerOpCode( OpCode::REVSH, OperationFormat::CRR_1 );
  WIR_Registry::registerOpCode( OpCode::REVSH, OperationFormat::TRR_1 );
  WIR_Registry::registerOpCode( OpCode::RFE, OperationFormat::AR_1 );
  WIR_Registry::registerOpCode( OpCode::RFE, OperationFormat::AR_2 );
  WIR_Registry::registerOpCode( OpCode::SADD16, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SADD8, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SADDSUBX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SEL, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SETEND, OperationFormat::A_2 );
  WIR_Registry::registerOpCode( OpCode::SETEND, OperationFormat::TA_2 );
  WIR_Registry::registerOpCode( OpCode::SHADD16, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SHADD8, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SHADDSUBX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SHSUB16, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SHSUB8, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SHSUBADDX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLAD, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLADX, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLALD, OperationFormat::CRRRR_5 );
  WIR_Registry::registerOpCode( OpCode::SMLALDX, OperationFormat::CRRRR_5 );
  WIR_Registry::registerOpCode( OpCode::SMLSD, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLSDX, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMLSLD, OperationFormat::CRRRR_5 );
  WIR_Registry::registerOpCode( OpCode::SMLSLDX, OperationFormat::CRRRR_5 );
  WIR_Registry::registerOpCode( OpCode::SMMLA, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMMLAR, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMMLS, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMMLSR, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMMUL, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMMULR, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMUAD, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMUADX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMUSD, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SMUSDX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SRS, OperationFormat::AC5_2 );
  WIR_Registry::registerOpCode( OpCode::SRS, OperationFormat::AC5_3 );
  WIR_Registry::registerOpCode( OpCode::SSAT, OperationFormat::CRC60R_1 );
  WIR_Registry::registerOpCode( OpCode::SSAT, OperationFormat::CRC60RC5_1 );
  WIR_Registry::registerOpCode( OpCode::SSAT, OperationFormat::CRC60RC60_1 );
  WIR_Registry::registerOpCode( OpCode::SSAT16, OperationFormat::CRC5SPR_1 );
  WIR_Registry::registerOpCode( OpCode::SSUB16, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SSUB8, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SSUBADDX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::STREX, OperationFormat::CRRR_5 );
  WIR_Registry::registerOpCode( OpCode::SXTAB, OperationFormat::CRRRA_1 );
  WIR_Registry::registerOpCode( OpCode::SXTAB16, OperationFormat::CRRRA_1 );
  WIR_Registry::registerOpCode( OpCode::SXTAH, OperationFormat::CRRRA_1 );
  WIR_Registry::registerOpCode( OpCode::SXTB, OperationFormat::CRRA_1 );
  WIR_Registry::registerOpCode( OpCode::SXTB, OperationFormat::TRR_1 );
  WIR_Registry::registerOpCode( OpCode::SXTB16, OperationFormat::CRRA_1 );
  WIR_Registry::registerOpCode( OpCode::SXTH, OperationFormat::CRRA_1 );
  WIR_Registry::registerOpCode( OpCode::SXTH, OperationFormat::TRR_1 );
  WIR_Registry::registerOpCode( OpCode::UADD16, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UADD8, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UADDSUBX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UHADD16, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UHADD8, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UHADDSUBX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UHSUB16, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UHSUB8, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UHSUBADDX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UMAAL, OperationFormat::CRRRR_5 );
  WIR_Registry::registerOpCode( OpCode::UQADD16, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UQADD8, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UQADDSUBX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UQSUB16, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UQSUB8, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UQSUBADDX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::USAD8, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::USADA8, OperationFormat::CRRRR_1 );
  WIR_Registry::registerOpCode( OpCode::USAT, OperationFormat::CRC5RC5_1 );
  WIR_Registry::registerOpCode( OpCode::USAT, OperationFormat::CRC5RC60_1 );
  WIR_Registry::registerOpCode( OpCode::USAT16, OperationFormat::CRC4R_1 );
  WIR_Registry::registerOpCode( OpCode::USUB16, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::USUB8, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::USUBADDX, OperationFormat::CRRR_1 );
  WIR_Registry::registerOpCode( OpCode::UXTAB, OperationFormat::CRRRA_1 );
  WIR_Registry::registerOpCode( OpCode::UXTAB16, OperationFormat::CRRRA_1 );
  WIR_Registry::registerOpCode( OpCode::UXTAH, OperationFormat::CRRRA_1 );
  WIR_Registry::registerOpCode( OpCode::UXTB, OperationFormat::CRRA_1 );
  WIR_Registry::registerOpCode( OpCode::UXTB, OperationFormat::TRR_1 );
  WIR_Registry::registerOpCode( OpCode::UXTB16, OperationFormat::CRRA_1 );
  WIR_Registry::registerOpCode( OpCode::UXTH, OperationFormat::CRRA_1 );
  WIR_Registry::registerOpCode( OpCode::UXTH, OperationFormat::TRR_1 );


  //
  // Register this current processor model.
  //

  WIR_Registry::registerProcessor( ARMv6() );
};


//
// Private class methods
//

/*
  clone creates a copy of a ARMv6 processor.
*/
WIR_BaseProcessor *ARMv6::clone( void ) const
{
  DSTART( "virtual WIR_BaseProcessor* ARMv6::clone() const" );

  return( new ARMv6( *this ) );
};

}       // namespace WIR
