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
  @file armv5t.cc
  @brief This file implements the specific interface of the ARMv5T instruction
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
#include <arch/arm/armv5t.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for ARMv5T-based processor architectures.
*/
ARMv5T::ARMv5T( void ) :
  ARMv4T {}
{
  DSTART( "ARMv5T::ARMv5T()" );

  // Specify the processor architecture modeled by this class.
  setISAName( "ARMv5T" );
};


/*
  Copy constructor.
*/
ARMv5T::ARMv5T( const ARMv5T &__o ) :
  ARMv4T { __o }
{
  DSTART( "ARMv5T::ARMv5T(const ARMv5T&)" );
};


/*
  Move constructor.
*/
ARMv5T::ARMv5T( ARMv5T &&__o ) :
  ARMv4T { move( __o ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
ARMv5T::~ARMv5T( void )
{
  DSTART( "virtual ARMv5T::~ARMv5T()" );
};


/*
  Copy-assignment operator.
*/
ARMv5T & ARMv5T::operator = ( const ARMv5T &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ARMv4T::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
ARMv5T & ARMv5T::operator = ( ARMv5T &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ARMv4T::operator = ( move( __o ) );

  return( *this );
};


/*
  init performs some global initialization tasks for ARMv5T-based processor
  architectures.

  This includes setting up the ARMv5T machine operation formats and the
  assignment of valid operation formats to ARMv5T opcodes.

  init shall be called globally by WIR_Init(). It shall only perform tasks that
  cannot be expressed as initializations of static class members (since the
  order of static initialization is unspecified in C++) and that thus require
  execution by active code.
*/
void ARMv5T::init( void )
{
  DSTART( "static void ARMv5T::init()" );


  //
  // Register ARMv5T operation formats.
  //

  WIR_BasicBlock b;
  ARM_RegV *regV = new ARM_RegV;

  WIR_Registry::registerOperationFormat(
    OperationFormat::AORSSO_2,
    { new WIR_AddressingModeParameter( ARM_Base::AddressingMode::p0 ),
      new ARM_Const3_CoprocessorOpcode( 1 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_StringParameter( "CRn" ),
      new WIR_StringParameter( "CRm" ),
      new ARM_Const3_CoprocessorOpcode( 2 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::AOSSSO,
    { new WIR_AddressingModeParameter( ARM_Base::AddressingMode::p0 ),
      new ARM_Const4_CoprocessorOpcode( 1 ),
      new WIR_StringParameter( "CRn" ),
      new WIR_StringParameter( "CRd" ),
      new WIR_StringParameter( "CRm" ),
      new ARM_Const3_CoprocessorOpcode( 2 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::ASARAC8_1,
    { new WIR_AddressingModeParameter( ARM_Base::AddressingMode::p0 ),
      new WIR_StringParameter( "CRd" ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::pre ),
      new WIR_RegisterParameter( *regV, WIR_Usage::defuse ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new ARM_Const10_CoprocessorOffset( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::ASARAC8_2,
    { new WIR_AddressingModeParameter( ARM_Base::AddressingMode::p0 ),
      new WIR_StringParameter( "CRd" ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::pre ),
      new WIR_RegisterParameter( *regV, WIR_Usage::defuse ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new ARM_Const10_CoprocessorOffset( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::ASRAC8_1,
    { new WIR_AddressingModeParameter( ARM_Base::AddressingMode::p0 ),
      new WIR_StringParameter( "CRd" ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new ARM_Const10_CoprocessorOffset( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::ASRAC8_2,
    { new WIR_AddressingModeParameter( ARM_Base::AddressingMode::p0 ),
      new WIR_StringParameter( "CRd" ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_AddressingModeParameter( ARM_Base::AddressingMode::plus ),
      new ARM_Const10_CoprocessorOffset( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::ASRC8_1,
    { new WIR_AddressingModeParameter( ARM_Base::AddressingMode::p0 ),
      new WIR_StringParameter( "CRd" ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new ARM_Const8_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::ASRC8_2,
    { new WIR_AddressingModeParameter( ARM_Base::AddressingMode::p0 ),
      new WIR_StringParameter( "CRd" ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new ARM_Const8_Unsigned( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::C16_1,
    { new ARM_Const16_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::L,
    { new WIR_LabelParameter( b ) } );


  //
  // ARMv5T opcode to operation format mapping.
  //

  WIR_Registry::registerOpCode( OpCode::BKPT, OperationFormat::C16_1 );
  WIR_Registry::registerOpCode( OpCode::BLX, OperationFormat::L );
  WIR_Registry::registerOpCode( OpCode::BLX, OperationFormat::CR_3 );
  WIR_Registry::registerOpCode( OpCode::CDP2, OperationFormat::AOSSSO );
  WIR_Registry::registerOpCode( OpCode::CLZ, OperationFormat::CRR_1 );
  WIR_Registry::registerOpCode( OpCode::LDC2, OperationFormat::ASRAC8_1 );
  WIR_Registry::registerOpCode( OpCode::LDC2, OperationFormat::ASRAC8_2 );
  WIR_Registry::registerOpCode( OpCode::LDC2, OperationFormat::ASARAC8_1 );
  WIR_Registry::registerOpCode( OpCode::LDC2, OperationFormat::ASARAC8_2 );
  WIR_Registry::registerOpCode( OpCode::LDC2, OperationFormat::ASRC8_1 );
  WIR_Registry::registerOpCode( OpCode::LDC2, OperationFormat::ASRC8_2 );
  WIR_Registry::registerOpCode( OpCode::MCR2, OperationFormat::AORSSO_2 );
  WIR_Registry::registerOpCode( OpCode::STC2, OperationFormat::ASRAC8_1 );
  WIR_Registry::registerOpCode( OpCode::STC2, OperationFormat::ASRAC8_2 );
  WIR_Registry::registerOpCode( OpCode::STC2, OperationFormat::ASARAC8_1 );
  WIR_Registry::registerOpCode( OpCode::STC2, OperationFormat::ASARAC8_2 );
  WIR_Registry::registerOpCode( OpCode::STC2, OperationFormat::ASRC8_1 );
  WIR_Registry::registerOpCode( OpCode::STC2, OperationFormat::ASRC8_2 );

  WIR_Registry::registerOpCode( OpCode::BLX, OperationFormat::TL_2 );
  WIR_Registry::registerOpCode( OpCode::BLX, OperationFormat::TR_1 );


  //
  // Register this current processor model.
  //

  WIR_Registry::registerProcessor( ARMv5T() );
};


//
// Private class methods
//

/*
  clone creates a copy of a ARMv5T processor.
*/
WIR_BaseProcessor *ARMv5T::clone( void ) const
{
  DSTART( "virtual WIR_BaseProcessor* ARMv5T::clone() const" );

  return( new ARMv5T( *this ) );
};

}       // namespace WIR
