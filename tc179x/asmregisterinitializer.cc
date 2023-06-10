/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2018 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file asmregisterinitializer.cc
  @brief This file implements a class initializing %WIR TriCore registers from
         high-level specifiers in GNU inline assembler code.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include ICD headers
#include <icd-c.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>

// Include LIBUSEFUL headers
#include <libuseful/debugmacros.h>

// Include local headers
#include "asmregisterinitializer.h"
#include "instructionfactory.h"
#include "tc_incl.h"


//
// Code section
//

using namespace std;
using namespace WIR;


//
// Public class methods
//

/*
  Default constructor creating an ASM register initializer for a given Assembly
  argument and a TriCore instruction factory to be used for code generation.
*/
AsmRegisterInitializer::AsmRegisterInitializer( const IR_AsmOperand &o,
                                                const InstructionFactory &f ) :
  mOperand { o },
  mFactory { f },
  mReg { nullptr },
  mSymExp { dynamic_cast<IR_SymbolExp *>( o.getExpression() ) }
{
  DSTART(
    "AsmRegisterInitializer::AsmRegisterInitializer(const IR_AsmOperand&, const InstructionFactory&)" );

  if ( !mSymExp ) {
    auto *intConstExp = dynamic_cast<IR_IntConstExp *>( o.getExpression() );
    if ( !intConstExp )
      operandParseError(
        o.getExpression(),
        "Only symbol expressions or integer constants can be bound to a data "
          "register", "" );

    mValue = &(intConstExp->getValue());
  }
};


/*
  Destructor.
*/
AsmRegisterInitializer::~AsmRegisterInitializer( void )
{
  DSTART( "virtual AsmRegisterInitializer::~AsmRegisterInitializer()" );
};


/*
  getDReg returns the virtual data register associated with this object.

  Depending on the register's context, getDReg also generates assembly code to
  properly initialize the data register.
*/
const WIR::TC_DRegV &AsmRegisterInitializer::getDReg( void )
{
  DSTART( "virtual const TC_DRegV& AsmRegisterInitializer::getDReg()" );

  if ( mReg != nullptr )
    return( dynamic_cast<const TC_DRegV &>( *mReg ) );

  if ( mSymExp ) {
    auto &r = loadReg( *mSymExp );

    if ( r.getType() == TC13::RegisterType::aReg ) {
      auto &dReg = mFactory.createDReg();
      mFactory.insertMOV_D( dReg, dynamic_cast<TC_ARegV &>( r ) );
      mReg = &dReg;
      return( dReg );
    } else

    if ( r.getType() == TC13::RegisterType::dReg ) {
      mReg = dynamic_cast<WIR_VirtualRegister *>( &r );
      return( dynamic_cast<TC_DRegV &>( r ) );
    } else

    if ( r.getType() == TC13::RegisterType::eReg ) {
      mReg = &(dynamic_cast<TC_ERegV &>( r ).begin()->get());
      return(
        dynamic_cast<TC_DRegV &>(
          dynamic_cast<TC_ERegV &>( r ).begin()->get() ) );
    }
  } else

  if ( mValue ) {
    auto &dReg = mFactory.createDReg();
    mFactory.insertMOVConstant( dReg, mValue->getIntValue() );
    mReg = &dReg;
    return( dReg );
  } else
    operandParseError(
      mOperand.getExpression(),
      "Only symbol expressions or integer constants can be bound to a data "
        "register", "" );

  return( dynamic_cast<TC_DRegV &>( *mReg ) );
};


/*
  getEReg returns the virtual extended register associated with this object.

  Depending on the register's context, getEReg also generates assembly code to
  properly initialize the extended register.
*/
const WIR::TC_ERegV &AsmRegisterInitializer::getEReg( void )
{
  DSTART( "virtual const TC_ERegV& AsmRegisterInitializer::getEReg()" );

  if ( mReg != nullptr )
    return( dynamic_cast<TC_ERegV &>( *mReg ) );

  if ( mSymExp ) {
    auto &r = loadReg( *mSymExp );

    if ( r.getType() == TC13::RegisterType::aReg ) {
      auto &eReg = mFactory.createEReg();
      mFactory.insertMOV_D(
        dynamic_cast<TC_DRegV &>( eReg.begin()->get() ),
        dynamic_cast<TC_ARegV &>( r ) );
      mReg = &eReg;
      return( eReg );
    } else

    if ( r.getType() == TC13::RegisterType::dReg ) {
      auto &eReg = mFactory.createEReg();
      mFactory.insertMOV(
        dynamic_cast<TC_DRegV &>( eReg.begin()->get() ),
        dynamic_cast<TC_DRegV &>( r ) );
      mReg = &eReg;
      return( eReg );
    } else

    if ( r.getType() == TC13::RegisterType::eReg ) {
      mReg = dynamic_cast<WIR_VirtualRegister *>( &r );
      return( dynamic_cast<TC_ERegV &>( r ) );
    }
  } else

  if ( mValue ) {
    auto &eReg = mFactory.createEReg();
    mFactory.insertMOVConstant( eReg, *mValue );
    mReg = &eReg;
    return( eReg );
  } else
    operandParseError(
      mOperand.getExpression(),
      "Only symbol expressions or integer constants can be bound to a data "
        "register", "" );

  return( dynamic_cast<TC_ERegV &>( *mReg ) );
};
