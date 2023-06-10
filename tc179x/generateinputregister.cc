/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2011 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include standard headers
#include <iostream>
#include <string>

// Include LIBUSEFUL headers
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include ICD headers
#include <icd-c.h>
#include <llir3/llir3.h>

// Include local headers
#include "generateinputregister.h"
#include "instructionfactory.h"
#include "tc_incl.h"

using namespace std;


static void operandParseError( IR_Exp *exp, const string &errormsg )
{
  ostringstream str;
  exp->write( str );
  IR_Stmt &stmt = exp->getStmt();

  throw
    ufFatalError(
      stmt.getFileContext().getFilename(), stmt.getFileContext().getLine(),
      errormsg + string( ": " ) + str.str() + " is neither." );
}

GenerateInputRegister::GenerateInputRegister( InstructionFactory* instructionFactory,
                       const IR_AsmOperand& operand ) :
  mInstructionFactory( instructionFactory ),
  mReg( 0 ),
  mSymExp( dynamic_cast<IR_SymbolExp*>( operand.getExpression() ) ),
  mOperand( operand )
{
  if ( !mSymExp ) {
    IR_IntConstExp *intConstExp = dynamic_cast<IR_IntConstExp *>( operand.getExpression() );
    if ( !intConstExp ) {
      operandParseError(
        operand.getExpression(),
        string( "Only symbol expressions or integer constants can be bound " ) +
          "to a data register" );
    }
    mValue = &intConstExp->getValue();
  }
}

LLIR_Register *GenerateInputRegister::getEReg()
{
  if ( mReg )
    return mReg;
  if ( mSymExp ){
    LLIR_Register *reg = loadRegister( mSymExp );
    string regname( reg->GetName() );
    if ( regname.compare( 0,2, VREG_E ) == 0 ) {
      mReg = reg;
      return mReg;
    } else if ( regname.compare( 0, 2, VREG_D ) == 0 ) {
      mReg = mInstructionFactory->CreateERegister( "" );
      mInstructionFactory->insertMOV( reg, mReg->GetFirstChild() );
      return mReg;
    } else if ( regname.compare( 0,2, VREG_A ) == 0 ) {
      mReg = mInstructionFactory->CreateERegister( "" );
      mInstructionFactory->insertMOV_D( mReg->GetFirstChild(), reg );
      return mReg;
    }

  ufAssertT( 0, "Unknown vreg type." );

  } else if ( mValue ) {
    mReg = mInstructionFactory->CreateERegister( "" );

    mInstructionFactory->insertMOVConstantLL( mReg, mValue );

    return mReg;
  } else {
    operandParseError(
      mOperand.getExpression(),
      string( "Only symbol expressions or integer constants can be bound " ) +
        "to a data register" );
  }
  return 0;
}

LLIR_Register *GenerateInputRegister::getDReg()
{
  if ( mReg )
    return mReg;
  if ( mSymExp ){
    LLIR_Register *reg = loadRegister( mSymExp );
    string regname( reg->GetName() );
    if ( regname.compare( 0,2, VREG_E ) == 0 ) {
      mReg = reg->GetFirstChild();
      return mReg;
    } else if ( regname.compare( 0, 2, VREG_D ) == 0 ) {
      mReg = reg;
      return mReg;
    } else if ( regname.compare( 0,2, VREG_A ) == 0 ) {
      mReg = mInstructionFactory->CreateRegister( "" );
      mInstructionFactory->insertMOV_D( mReg, reg );
      return mReg;
    }
    ufAssertT( 0, "Unknown vreg type." );
  } else if ( mValue ) {
      mReg = mInstructionFactory->CreateRegister( "" );
      int value = mValue->getIntValue();
      mInstructionFactory->insertMOVConstant( mReg, value );
      return mReg;
  } else {
    operandParseError(
      mOperand.getExpression(),
      string( "Only symbol expressions or integer constants can be bound " ) +
        "to a data register" );
  }
  return 0;
}

