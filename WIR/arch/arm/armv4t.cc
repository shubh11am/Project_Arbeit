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
  @file armv4t.cc
  @brief This file implements the specific interface of the ARMv4T instruction
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
#include <arch/arm/armv4t.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for ARMv4T-based processor architectures.
*/
ARMv4T::ARMv4T( void ) :
  ARM_Base {}
{
  DSTART( "ARMv4T::ARMv4T()" );

  // Specify the processor architecture modeled by this class.
  setISAName( "ARMv4T" );
};


/*
  Copy constructor.
*/
ARMv4T::ARMv4T( const ARMv4T &__o ) :
  ARM_Base { __o }
{
  DSTART( "ARMv4T::ARMv4T(const ARMv4T&)" );
};


/*
  Move constructor.
*/
ARMv4T::ARMv4T( ARMv4T &&__o ) :
  ARM_Base { move( __o ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
ARMv4T::~ARMv4T( void )
{
  DSTART( "virtual ARMv4T::~ARMv4T()" );
};


/*
  Copy-assignment operator.
*/
ARMv4T & ARMv4T::operator = ( const ARMv4T &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ARM_Base::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
ARMv4T & ARMv4T::operator = ( ARMv4T &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ARM_Base::operator = ( move( __o ) );

  return( *this );
};


/*
  init performs some global initialization tasks for ARMv4T-based processor
  architectures.

  This includes setting up the ARMv4T machine operation formats and the
  assignment of valid operation formats to ARMv4T opcodes.

  init shall be called globally by WIR_Init(). It shall only perform tasks that
  cannot be expressed as initializations of static class members (since the
  order of static initialization is unspecified in C++) and that thus require
  execution by active code.
*/
void ARMv4T::init( void )
{
  DSTART( "static void ARMv4T::init()" );


  //
  // Register ARMv4T operation formats.
  //

  WIR_BasicBlock b;
  ARM_RegV *regV = new ARM_RegV;
  ARM_LoRegV *loregV = new ARM_LoRegV;
  ARM_HiRegP *sp = new ARM_HiRegP( "13", true );
  ARM_HiRegP *lr = new ARM_HiRegP( "14" );
  ARM_HiRegP *pc = new ARM_HiRegP( "15" );

  WIR_Registry::registerOperationFormat(
    OperationFormat::CR_3,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TC8_1,
    { new ARM_Const8_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TC9_1,
    { new ARM_Const9_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TCL,
    { new WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TL_1,
    { new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TL_2,
    { new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR1_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR1PC,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pc, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR1_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR1LR,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *lr, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR2_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR2PC,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pc, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR2_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR2LR,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *lr, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR3_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR3PC,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pc, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR3_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR3LR,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *lr, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR4_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR4PC,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pc, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR4_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR4LR,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *lr, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR5_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR5PC,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pc, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR5_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR5LR,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *lr, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR6_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR6PC,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pc, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR6_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR6LR,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *lr, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR7_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR7PC,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pc, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR7_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR7LR,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *lr, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR8_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR8PC,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pc, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR8_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TR8LR,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *lr, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRC8_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new ARM_Const8_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRC8_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new ARM_Const8_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRC8_3,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new ARM_Const8_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRPCC10_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pc, WIR_Usage::use ),
      new ARM_Const10_Unsigned4( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRPCC10_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *pc, WIR_Usage::use ),
      new ARM_Const10_Unsigned4( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR_2,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR_3,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR_4,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR_5,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR_6,
    { new WIR_RegisterParameter( *regV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR1_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR1_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR2_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR2_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR3_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR3_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR4_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR4_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR5_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR5_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR6_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR6_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR7_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR7_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR8_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::def ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRR8_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::defuse ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRRC3_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new ARM_Const3_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRRC5_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new ARM_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRRC5_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new ARM_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRRC5_3,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new ARM_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRRC6_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new ARM_Const6_Unsigned0( 1 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRRC6_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new ARM_Const6_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRRC6_3,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new ARM_Const6_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRRC7_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new ARM_Const7_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRRC7_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new ARM_Const7_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRRR_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRRR_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRRR_3,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *loregV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRSPC10_1,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *sp, WIR_Usage::use ),
      new ARM_Const10_Unsigned4( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRSPC10_2,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::def ),
      new WIR_RegisterParameter( *sp, WIR_Usage::use ),
      new ARM_Const10_Unsigned4( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::TRSPC10_3,
    { new WIR_RegisterParameter( *loregV, WIR_Usage::use ),
      new WIR_RegisterParameter( *sp, WIR_Usage::use ),
      new ARM_Const10_Unsigned4( 0 ) } );


  //
  // ARMv4T opcode to operation format mapping.
  //

  WIR_Registry::registerOpCode( OpCode::ADC, OperationFormat::TRR_5 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::TRRC3_1 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::TRC8_3 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::TRRR_1 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::TRR_6 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::TRPCC10_1 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::TRSPC10_1 );
  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::TC9_1 );
  WIR_Registry::registerOpCode( OpCode::AND, OperationFormat::TRR_5 );
  WIR_Registry::registerOpCode( OpCode::ASR, OperationFormat::TRRC6_1 );
  WIR_Registry::registerOpCode( OpCode::ASR, OperationFormat::TRR_5 );
  WIR_Registry::registerOpCode( OpCode::B, OperationFormat::TCL );
  WIR_Registry::registerOpCode( OpCode::B, OperationFormat::TL_1 );
  WIR_Registry::registerOpCode( OpCode::BIC, OperationFormat::TRR_5 );
  WIR_Registry::registerOpCode( OpCode::BL, OperationFormat::TL_2 );
  WIR_Registry::registerOpCode( OpCode::BX, OperationFormat::CR_3 );
  WIR_Registry::registerOpCode( OpCode::BX, OperationFormat::TR_1 );
  WIR_Registry::registerOpCode( OpCode::CMN, OperationFormat::TRR_5 );
  WIR_Registry::registerOpCode( OpCode::CMP, OperationFormat::TRC8_2 );
  WIR_Registry::registerOpCode( OpCode::CMP, OperationFormat::TRR_3 );
  WIR_Registry::registerOpCode( OpCode::CMP, OperationFormat::TRR_4 );
  WIR_Registry::registerOpCode( OpCode::EOR, OperationFormat::TRR_5 );
  WIR_Registry::registerOpCode( OpCode::LDMIA, OperationFormat::TRR1_1 );
  WIR_Registry::registerOpCode( OpCode::LDMIA, OperationFormat::TRR2_1 );
  WIR_Registry::registerOpCode( OpCode::LDMIA, OperationFormat::TRR3_1 );
  WIR_Registry::registerOpCode( OpCode::LDMIA, OperationFormat::TRR4_1 );
  WIR_Registry::registerOpCode( OpCode::LDMIA, OperationFormat::TRR5_1 );
  WIR_Registry::registerOpCode( OpCode::LDMIA, OperationFormat::TRR6_1 );
  WIR_Registry::registerOpCode( OpCode::LDMIA, OperationFormat::TRR7_1 );
  WIR_Registry::registerOpCode( OpCode::LDMIA, OperationFormat::TRR8_1 );
  WIR_Registry::registerOpCode( OpCode::LDR, OperationFormat::TRRC7_1 );
  WIR_Registry::registerOpCode( OpCode::LDR, OperationFormat::TRRR_2 );
  WIR_Registry::registerOpCode( OpCode::LDR, OperationFormat::TRPCC10_2 );
  WIR_Registry::registerOpCode( OpCode::LDR, OperationFormat::TRSPC10_2 );
  WIR_Registry::registerOpCode( OpCode::LDRB, OperationFormat::TRRC5_2 );
  WIR_Registry::registerOpCode( OpCode::LDRB, OperationFormat::TRRR_2 );
  WIR_Registry::registerOpCode( OpCode::LDRH, OperationFormat::TRRC6_2 );
  WIR_Registry::registerOpCode( OpCode::LDRH, OperationFormat::TRRR_2 );
  WIR_Registry::registerOpCode( OpCode::LDRSB, OperationFormat::TRRR_2 );
  WIR_Registry::registerOpCode( OpCode::LDRSH, OperationFormat::TRRR_2 );
  WIR_Registry::registerOpCode( OpCode::LSL, OperationFormat::TRRC5_1 );
  WIR_Registry::registerOpCode( OpCode::LSL, OperationFormat::TRR_5 );
  WIR_Registry::registerOpCode( OpCode::LSR, OperationFormat::TRRC6_1 );
  WIR_Registry::registerOpCode( OpCode::LSR, OperationFormat::TRR_5 );
  WIR_Registry::registerOpCode( OpCode::MOV, OperationFormat::TRC8_1 );
  WIR_Registry::registerOpCode( OpCode::MOV, OperationFormat::TRR_1 );
  WIR_Registry::registerOpCode( OpCode::MOV, OperationFormat::TRR_2 );
  WIR_Registry::registerOpCode( OpCode::MUL, OperationFormat::TRR_5 );
  WIR_Registry::registerOpCode( OpCode::MVN, OperationFormat::TRR_1 );
  WIR_Registry::registerOpCode( OpCode::NEG, OperationFormat::TRR_1 );
  WIR_Registry::registerOpCode( OpCode::ORR, OperationFormat::TRR_5 );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR1_1 );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR1PC );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR2_1 );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR2PC );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR3_1 );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR3PC );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR4_1 );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR4PC );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR5_1 );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR5PC );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR6_1 );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR6PC );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR7_1 );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR7PC );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR8_1 );
  WIR_Registry::registerOpCode( OpCode::POP, OperationFormat::TR8PC );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR1_2 );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR1LR );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR2_2 );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR2LR );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR3_2 );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR3LR );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR4_2 );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR4LR );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR5_2 );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR5LR );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR6_2 );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR6LR );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR7_2 );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR7LR );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR8_2 );
  WIR_Registry::registerOpCode( OpCode::PUSH, OperationFormat::TR8LR );
  WIR_Registry::registerOpCode( OpCode::ROR, OperationFormat::TRR_5 );
  WIR_Registry::registerOpCode( OpCode::SBC, OperationFormat::TRR_5 );
  WIR_Registry::registerOpCode( OpCode::STMIA, OperationFormat::TRR1_2 );
  WIR_Registry::registerOpCode( OpCode::STMIA, OperationFormat::TRR2_2 );
  WIR_Registry::registerOpCode( OpCode::STMIA, OperationFormat::TRR3_2 );
  WIR_Registry::registerOpCode( OpCode::STMIA, OperationFormat::TRR4_2 );
  WIR_Registry::registerOpCode( OpCode::STMIA, OperationFormat::TRR5_2 );
  WIR_Registry::registerOpCode( OpCode::STMIA, OperationFormat::TRR6_2 );
  WIR_Registry::registerOpCode( OpCode::STMIA, OperationFormat::TRR7_2 );
  WIR_Registry::registerOpCode( OpCode::STMIA, OperationFormat::TRR8_2 );
  WIR_Registry::registerOpCode( OpCode::STR, OperationFormat::TRRC7_2 );
  WIR_Registry::registerOpCode( OpCode::STR, OperationFormat::TRRR_3 );
  WIR_Registry::registerOpCode( OpCode::STR, OperationFormat::TRSPC10_3 );
  WIR_Registry::registerOpCode( OpCode::STRB, OperationFormat::TRRC5_3 );
  WIR_Registry::registerOpCode( OpCode::STRB, OperationFormat::TRRR_3 );
  WIR_Registry::registerOpCode( OpCode::STRH, OperationFormat::TRRC6_3 );
  WIR_Registry::registerOpCode( OpCode::STRH, OperationFormat::TRRR_3 );
  WIR_Registry::registerOpCode( OpCode::SUB, OperationFormat::TRRC3_1 );
  WIR_Registry::registerOpCode( OpCode::SUB, OperationFormat::TRC8_3 );
  WIR_Registry::registerOpCode( OpCode::SUB, OperationFormat::TRRR_1 );
  WIR_Registry::registerOpCode( OpCode::SUB, OperationFormat::TC9_1 );
  WIR_Registry::registerOpCode( OpCode::SWI, OperationFormat::TC8_1 );
  WIR_Registry::registerOpCode( OpCode::TST, OperationFormat::TRR_3 );


  //
  // Register this current processor model.
  //

  WIR_Registry::registerProcessor( ARMv4T() );
};


//
// Private class methods
//

/*
  clone creates a copy of a ARMv4T processor.
*/
WIR_BaseProcessor *ARMv4T::clone( void ) const
{
  DSTART( "virtual WIR_BaseProcessor* ARMv4T::clone() const" );

  return( new ARMv4T( *this ) );
};

}       // namespace WIR
