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
  @file armconst3coprocessoropcode.cc
  @brief This file implements unsigned 3 bits-wide immediate parameters for
         coprocessor opcodes.

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
#include <arch/arm/armbase.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for unsigned const3 parameters.

  The constructor ensures that __i lies in the range of values that can be
  represented with 3 bits (unsigned).
*/
ARM_Const3_CoprocessorOpcode::ARM_Const3_CoprocessorOpcode( unsigned long long __i ) :
  WIR_UnsignedImmediateParameter<ARM_Const3_CoprocessorOpcode> { __i, 3 }
{
  DSTART(
    "ARM_Const3_CoprocessorOpcode::ARM_Const3_CoprocessorOpcode(long long unsigned int)" );
};


/*
  Copy constructor.
*/
ARM_Const3_CoprocessorOpcode::ARM_Const3_CoprocessorOpcode( const ARM_Const3_CoprocessorOpcode &__o ) :
  WIR_UnsignedImmediateParameter<ARM_Const3_CoprocessorOpcode> { __o }
{
  DSTART(
    "ARM_Const3_CoprocessorOpcode::ARM_Const3_CoprocessorOpcode(const ARM_Const3_CoprocessorOpcode&)" );
};


/*
  Move constructor.
*/
ARM_Const3_CoprocessorOpcode::ARM_Const3_CoprocessorOpcode( ARM_Const3_CoprocessorOpcode &&__o ) :
  WIR_UnsignedImmediateParameter<ARM_Const3_CoprocessorOpcode> { move( __o ) }
{
  DSTART(
    "ARM_Const3_CoprocessorOpcode::ARM_Const3_CoprocessorOpcode(ARM_Const3_CoprocessorOpcode&&)" );
};


/*
  Destructor.
*/
ARM_Const3_CoprocessorOpcode::~ARM_Const3_CoprocessorOpcode( void )
{
  DSTART(
    "virtual ARM_Const3_CoprocessorOpcode::~ARM_Const3_CoprocessorOpcode()" );
};


/*
  Copy-assignment operator.
*/
ARM_Const3_CoprocessorOpcode & ARM_Const3_CoprocessorOpcode::operator = ( const ARM_Const3_CoprocessorOpcode &__o )
{
  DSTART(
    "ARM_Const3_CoprocessorOpcode& ARM_Const3_CoprocessorOpcode::operator=(const ARM_Const3_CoprocessorOpcode&)" );

  WIR_UnsignedImmediateParameter<ARM_Const3_CoprocessorOpcode>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
ARM_Const3_CoprocessorOpcode & ARM_Const3_CoprocessorOpcode::operator = ( ARM_Const3_CoprocessorOpcode &&__o )
{
  DSTART(
    "ARM_Const3_CoprocessorOpcode& ARM_Const3_CoprocessorOpcode::operator=(ARM_Const3_CoprocessorOpcode&&)" );

  WIR_UnsignedImmediateParameter<ARM_Const3_CoprocessorOpcode>::operator = ( move( __o ) );

  return( *this );
};

}       // namespace WIR
