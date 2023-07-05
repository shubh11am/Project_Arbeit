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
  @file wirmisc.cc
  @brief This file implements miscellaneous global functions that are used
         everywhere within the %WIR library.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <iostream>
#include <map>
#include <stdarg.h>
#include <stdlib.h>
#include <string>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#ifdef HAVE_ARM
#include <arch/arm/armbase.h>
#endif
#include <arch/generic/mips.h>
#ifdef HAVE_RISCV
#include <arch/riscv/rv32i.h>
#endif
#ifdef HAVE_TRICORE
#include <arch/tricore/tc13.h>
#endif


//
// Code section
//

namespace WIR {


using namespace std;


/*
  WIR_Init performs some global initialization tasks for the entire WIR library.

  This includes initialization of the debug macros, the I/O system and ISA-
  specific initializations.
*/
void WIR_Init( void )
{
  // Initialize debug macros.
  DINIT( string( DEBUGMACRO_CONF ) + "/wir.conf", "cout" );

  DSTART( "void WIR_Init()" );

  // Register default WIR I/O functions.
  WIR_Registry::registerBasicBlockDumper( 0, dumpWIRBasicBlock );
  WIR_Registry::registerBlockLabelDumper( 0, dumpWIRBlockLabel );
  WIR_Registry::registerCommentDumper( 0, dumpWIRComment );
  WIR_Registry::registerCompilationUnitDumper( 0, dumpWIRCompilationUnit );
  WIR_Registry::registerDataDumper( 0, dumpWIRData );
  WIR_Registry::registerDataSectionDumper( 0, dumpWIRDataSection );
  WIR_Registry::registerFileInfoDumper( 0, dumpWIRFileInfo );
  WIR_Registry::registerFunctionDumper( 0, dumpWIRFunction );
  WIR_Registry::registerInstructionDumper( 0, dumpWIRInstruction );
  WIR_Registry::registerOperationDumper( 0, dumpWIROperation );
  WIR_Registry::registerAddressingModeParameterDumper(
    0, dumpWIRAddressingModeParameter );
  WIR_Registry::registerConditionFieldParameterDumper(
    0, dumpWIRConditionFieldParameter );
  WIR_Registry::registerLabelParameterDumper( 0, dumpWIRLabelParameter );
  WIR_Registry::registerImmediateParameterDumper( 0, dumpWIRImmediateParameter );
  WIR_Registry::registerRegisterParameterDumper( 0, dumpWIRRegisterParameter );
  WIR_Registry::registerLdScriptDumper( 0, dumpLdScript );
  WIR_Registry::registerLdScriptSectionDumper( 0, dumpLDScriptSection );
  WIR_Registry::registerSystemDumper( 0, dumpWIRSystem );

  // Initialize WIR processor models.
  #ifdef HAVE_ARM
  ARM_Base::init();
  #endif

  MIPS::init();

  #ifdef HAVE_RISCV
  RV32I::init();
  #endif

  #ifdef HAVE_TRICORE
  TC13::init();
  #endif
};


/*
  WIR_Module returns the name of this WIR class library.
*/
string WIR_Module( void )
{
  DSTART( "string WIR_Module()" );

  return( string( WIR_MODULE ) );
};


/*
  WIR_Version returns the version of this WIR class library.
*/
string WIR_Version( void )
{
  DSTART( "string WIR_Version()" );

  return( string( WIR_VERSION ) );
};


/*
  WIR_ProcessorIO adds a new index for I/O streams in order to establish
  WIR-specific I/O manipulators.
*/
int WIR_ProcessorIO( void )
{
  DSTART( "int WIR_ProcessorIO()" );

  static int i = ios_base::xalloc();
  return( i );
};


/*
  count1Bits computes how many bits in the binary representation of the passed
  value are set to 1.
*/
unsigned int count1Bits( unsigned int i )
{
  DSTART( "unsigned int count1Bits(unsigned int)" );

  unsigned int mask = 1u;
  unsigned int res = 0;

  for ( unsigned int j = 0; j < sizeof( i ) * 8; ++j, mask <<= 1 ) {
    if ( ( i & mask ) != 0 )
      ++res;
  }

  return( res );
};


/*
  isPowerOfTwo returns whether the passed value is a power of 2.
*/
bool isPowerOfTwo( unsigned int i )
{
  DSTART( "bool isPowerOfTwo(unsigned int)" );

  return( count1Bits( i ) == 1 );
};

}       // namespace WIR
