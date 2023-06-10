/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2007 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include standard headers
#include <cassert>
#include <cmath>
#include <algorithm>

// Include ICD headers
#include <icd-c.h>
#include <llir3/llir3.h>

// Include local headers
#include "registrar.h"
#include "cast.h"

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include misc headers
#include <misc/misc.h>

#include "arm_incl.h"
#include "casting_incl.h"


#ifdef DEBUG_WCC
// Debug output function
std::string getTypeString( const IR_Type& t )
{
  std::stringstream out;
  t.write( out );
  char line[255];
  out.getline( line, 255 );
  return line;
}
#endif

using namespace std;

ConstCastIntegralResult *Cast::constantCastToIntegral( const IR_Exp &exp )
{
  if ( effectiveType( exp ).isIntegralType() ) {

    ConstCastResult * const tempRes = constantCast( exp );
    if ( tempRes ) {
      ufAssertT( tempRes->type->isIntegralType(), "Invalid result!" );
      ConstCastIntegralResult * const result = new ConstCastIntegralResult(
                                  tempRes->value.integral, *tempRes->type );
      delete tempRes;
      return result;
    } else {
      return nullptr;
    }

  } else {
    return nullptr;
  }
}

Cast::ConstCastResult *Cast::constantCast( const IR_Exp &exp )
{
  ConstCastResult *result = nullptr;

  const IR_IntConstExp * const icexp =
    dynamic_cast<const IR_IntConstExp*>( &exp );
  const IR_FloatConstExp * const fcexp =
    dynamic_cast<const IR_FloatConstExp*>( &exp );
  const IR_UnaryExp * const uexp = dynamic_cast<const IR_UnaryExp*>( &exp );
  const IR_SizeOfExp * const soexp = dynamic_cast<const IR_SizeOfExp*>( &exp );
  const IR_SymbolExp * const symexp = dynamic_cast<const IR_SymbolExp*>( &exp );

  if ( icexp ) {
    result = new ConstCastResult;
    result->type = &icexp->getType();
    result->value.integral.setBitwidth( result->type->bitSize(),
                                        result->type->isSignedType() );
    result->value.integral = icexp->getValue();
  } else
  if ( fcexp ) {
    result = new ConstCastResult;
    result->type = &fcexp->getType();

    switch ( result->type->getType() ) {
      case IR_Type::FLOAT:
        result->value.flt = fcexp->getValue().getSingleValue();
        break;
      case IR_Type::DOUBLE:
        result->value.dbl = fcexp->getValue().getDoubleValue();
        break;
      case IR_Type::LONG_DOUBLE:
        result->value.dbl = fcexp->getValue().getLongDoubleValue();
        break;
      default:
        ufAssertT( 0, "Invalid type!" );
        break;
    }
  } else
  if ( symexp && symexp->getSymbol().getEnumType() ) {
    result = new ConstCastResult;
    result->type = &symexp->getType();

    IR_Symbol &sym = symexp->getSymbol();
    result->value.integral.setBitwidth( result->type->bitSize(),
                                        result->type->isSignedType() );
    result->value.integral = sym.getEnumType()->getValue( &sym );
  } else
  if ( soexp ) {
    result = new ConstCastResult;
    result->type = &soexp->getType();
    result->value.integral.setBitwidth( result->type->bitSize(),
                                        result->type->isSignedType() );
    result->value.integral = computeSizeOf( &soexp->getBaseType() );
  } else
  if ( uexp && uexp->getOperator() == IR_UnaryExp::SIZEOF ) {
    result = new ConstCastResult;
    result->type = &uexp->getType();
    result->value.integral.setBitwidth( result->type->bitSize(),
                                        result->type->isSignedType() );
    result->value.integral = computeSizeOf( &uexp->getOp().getType() );
  } else
  if ( uexp && uexp->getOperator() == IR_UnaryExp::PLUS ) {
    result = constantCast( uexp->getOp() );
    if ( !result ) {
      return nullptr;
    }
    result->type = &uexp->getType();
  } else
  if ( uexp && uexp->getOperator() == IR_UnaryExp::MINUS ) {
    result = constantCast( uexp->getOp() );
    if ( !result ) {
      return nullptr;
    }
    result->type = &uexp->getType();

    switch ( result->type->getType() ) {
      case IR_Type::FLOAT:
        result->value.flt = -result->value.flt;
        break;
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE:
        result->value.dbl = -result->value.dbl;
        break;
      default:
        result->value.integral.setBitwidth( result->type->bitSize(),
                                            result->type->isSignedType() );
        result->value.integral = -result->value.integral;
        break;
    }
  } else
  if ( uexp && uexp->getOperator() == IR_UnaryExp::CAST ) {
    result = constantCast( uexp->getOp() );
    if ( !result ) {
      return nullptr;
    }

    result->type = &uexp->getType();

    IR_Type::Type sourceType = uexp->getOp().getType().getType();
    switch ( result->type->getType() ) {
      case IR_Type::FLOAT:
        switch ( sourceType ) {
          case IR_Type::FLOAT:
            result->value.flt = constantCastToFloat( result->value.flt );
            break;
          case IR_Type::DOUBLE:
          case IR_Type::LONG_DOUBLE:
            result->value.flt = constantCastToFloat( result->value.dbl );
            break;
          default:
            // TODO: The floating point cast results do not always match the
            //       tricore representation. Therefore casting of integrals to
            //       float is deactivated at the moment.
            delete result;
            return nullptr;
            //result->value.flt = constantCastToFloat( result->value.integral );
            //break;
        }
        break;
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE:
        switch ( sourceType ) {
          case IR_Type::FLOAT:
            result->value.dbl = constantCastToDouble( result->value.flt );
            break;
          case IR_Type::DOUBLE:
          case IR_Type::LONG_DOUBLE:
            result->value.dbl = constantCastToDouble( result->value.dbl );
            break;
          default:
            // TODO: The floating point cast results do not always match the
            //       tricore representation. Therefore casting of integrals to
            //       float is deactivated at the moment.
            delete result;
            return nullptr;
            //result->value.dbl = constantCastToDouble( result->value.integral );
            //break;
        }
        break;
      default:
        result->value.integral.setBitwidth( result->type->bitSize(),
                                            result->type->isSignedType() );
        switch ( sourceType ) {
          case IR_Type::FLOAT:
            result->value.integral = constantCastToIntegral( result->value.flt,
                                      *result->type );
            break;
          case IR_Type::DOUBLE:
          case IR_Type::LONG_DOUBLE:
            result->value.integral = constantCastToIntegral( result->value.dbl,
                                      *result->type );
            break;
          default:
            result->value.integral = constantCastToIntegral( result->value.integral,
                                      *result->type );
            break;
        }
        break;
    }

  } else {
    return nullptr;
  }
  ufAssertT( result, "Result should have been set by the cases above!" );

  // Handle the possible implicit cast
  if ( exp.getImplicitCastType() ) {
    result->type = exp.getImplicitCastType();

    IR_Type::Type sourceType = exp.getType().getType();
    switch ( result->type->getType() ) {
      case IR_Type::FLOAT:
        switch ( sourceType ) {
          case IR_Type::FLOAT:
            result->value.flt = constantCastToFloat( result->value.flt );
            break;
          case IR_Type::DOUBLE:
          case IR_Type::LONG_DOUBLE:
            result->value.flt = constantCastToFloat( result->value.dbl );
            break;
          default:
            // TODO: The floating point cast results do not always match the
            //       tricore representation. Therefore casting of integrals to
            //       float is deactivated at the moment.
            delete result;
            return nullptr;
            //result->value.flt = constantCastToFloat( result->value.integral );
            //break;
        }
        break;
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE:
        switch ( sourceType ) {
          case IR_Type::FLOAT:
            result->value.dbl = constantCastToDouble( result->value.flt );
            break;
          case IR_Type::DOUBLE:
          case IR_Type::LONG_DOUBLE:
            result->value.dbl = constantCastToDouble( result->value.dbl );
            break;
          default:
            // TODO: The floating point cast results do not always match the
            //       tricore representation. Therefore casting of integrals to
            //       float is deactivated at the moment.
            delete result;
            return nullptr;
            //result->value.dbl = constantCastToDouble( result->value.integral );
            //break;
        }
        break;
      default:
        result->value.integral.setBitwidth( result->type->bitSize(),
                                            result->type->isSignedType() );
        switch ( sourceType ) {
          case IR_Type::FLOAT:
            result->value.integral = constantCastToIntegral( result->value.flt,
                                      *result->type );
            break;
          case IR_Type::DOUBLE:
          case IR_Type::LONG_DOUBLE:
            result->value.integral = constantCastToIntegral( result->value.dbl,
                                      *result->type );
            break;
          default:
            result->value.integral = constantCastToIntegral( result->value.integral,
                                      *result->type );
            break;
        }
        break;
    }
  }

  return result;
}

Float Cast::constantCastToFloat( const Float &f )
{
  return f;
}
Float Cast::constantCastToFloat( const Double &d )
{
  return Float( d );
}
Float Cast::constantCastToFloat( const IR_Integer &i )
{
  return Float( i.getRawValue() );
}

Double Cast::constantCastToDouble( const Float &f )
{
  return Double( f );
}
Double Cast::constantCastToDouble( const Double &d )
{
  return d;
}
Double Cast::constantCastToDouble( const IR_Integer &i )
{
  return Double( i.getRawValue() );
}

IR_Integer Cast::constantCastToIntegral( const Float &f,
                                         const IR_Type &targetType )
{
  ufAssertT( targetType.isScalarType(), "Invalid argument!" );
  IR_Integer result( f, targetType.bitSize(), targetType.isSignedType() );
  if ( targetType.getType() == IR_Type::BOOL ) {
    result = ( f == 0 ? 0 : 1 );
  }
  return result;
}
IR_Integer Cast::constantCastToIntegral( const Double &d,
                                         const IR_Type &targetType )
{
  ufAssertT( targetType.isScalarType(), "Invalid argument!" );
  IR_Integer result( d, targetType.bitSize(), targetType.isSignedType() );
  if ( targetType.getType() == IR_Type::BOOL ) {
    result = ( d == 0 ? 0 : 1 );
  }
  return result;
}
IR_Integer Cast::constantCastToIntegral( const IR_Integer &i,
                                         const IR_Type &targetType )
{
  ufAssertT( targetType.isScalarType(), "Invalid argument!" );
  IR_Integer result( 0, targetType.bitSize(), targetType.isSignedType() );
  if ( targetType.getType() == IR_Type::BOOL ) {
    result = ( i == 0 ? 0 : 1 );
  } else {
    result = i;
  }
  return result;
}

LLIR_Register *Cast::castToBool( LLIR_Register *reg, IR_Exp *exp ){
  LLIR_Register* lhs = ARMINSTRUCTIONS.CreateRegister( "" );
  if ( reg->GetNumberOfChildren() == 2 ){
    // Generate the operation.
    // Compare LS child of reg against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, reg->GetFirstChild(), 0, exp );
    // Compare MS child of reg against zero, iff the LS child compared equal
    // to zero.
    ARMINSTRUCTIONS.insertCMP( OPER_EQ, reg->GetNextChild( reg->GetFirstChild() ),
                            0, exp );
  } else {
    // Compare with zero, condition flag Z is set to 1 if equal and 0 if not.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, reg, 0, exp );
  }
  // Insert 0 if the comparison set Z to 1.
  ARMINSTRUCTIONS.insertMOV( OPER_EQ, lhs, 0, exp );
  // Insert 1 if the comparison set Z to 0.
  ARMINSTRUCTIONS.insertMOV( OPER_NE, lhs, 1, exp );
  return lhs;
}

LLIR_Register* Cast::castToChar( LLIR_Register *reg,  const IR_Type &sourcetype,
                                 const IR_Type &destinationtype, IR_Exp *exp ){
  if( destinationtype.bitSize() < sourcetype.bitSize() ){
    LLIR_Register* lhs = ARMINSTRUCTIONS.CreateRegister( "" );
    if ( sourcetype.getType() == IR_Type::LONG_LONG ||
         sourcetype.getType() == IR_Type::UNSIGNED_LONG_LONG ){
      // Bitwise AND of the least significant child register with 0xFF such that
      // all bits above the 8th are cut.
      ARMINSTRUCTIONS.insertAND( lhs, reg->GetFirstChild(), 0xFF, exp  );
    }
    else {
      // Bitwise AND with 0xFF such that every bit over the 8th is cut from the
      // start value.
      ARMINSTRUCTIONS.insertAND( lhs, reg, 0xFF, exp  );
    }
    return lhs;
  }
  return reg;
}

LLIR_Register* Cast::castToShort( LLIR_Register *reg, const IR_Type &sourcetype,
                                  const IR_Type &destinationtype, IR_Exp *exp ){
  if( destinationtype.bitSize() < sourcetype.bitSize() ){
    LLIR_Register* lhs = ARMINSTRUCTIONS.CreateRegister( "" );
    // Bitwise AND with 0xFF00 such that every bit over the 16th is cut from
    // the start value.
    ARMINSTRUCTIONS.insertMOV( lhs, 0xFF00, exp );
    ARMINSTRUCTIONS.insertADD( lhs, lhs, 0xFF, exp );
    if ( sourcetype.getType() == IR_Type::LONG_LONG ||
         sourcetype.getType() == IR_Type::UNSIGNED_LONG_LONG ){
      // Apply AND on least significant child register.
      ARMINSTRUCTIONS.insertAND( lhs, reg->GetFirstChild(), lhs, exp );
    }
    else {
      ARMINSTRUCTIONS.insertAND( lhs, reg, lhs, exp );
    }
    return lhs;
  }
  else
    return reg;
}

LLIR_Register* Cast::castToLong( LLIR_Register *reg, const IR_Type &sourcetype,
                                 IR_Exp *exp ){
  if ( sourcetype.getType() == IR_Type::LONG_LONG ||
       sourcetype.getType() == IR_Type::UNSIGNED_LONG_LONG ){
    // Sanity check.
    ufAssert( reg->GetNumberOfChildren() == 2 );
    // Keep the least significant/first child.
    return reg->GetFirstChild();
  }
  else {
    LLIR_Register *lhs = ARMINSTRUCTIONS.CreateRegister( "" );
    size_t shiftVal = ( sourcetype.bitSize() == 16 ? 16 : 24 );
    // First, move the value to the resulting reg.
    ARMINSTRUCTIONS.insertMOV( lhs, reg, exp );
    ARMINSTRUCTIONS.insertMOV( OPER_AL, lhs, lhs, OPER_LSL, shiftVal, exp );
    if( sourcetype.isSignedType() ){
      ARMINSTRUCTIONS.insertMOV( OPER_AL, lhs, lhs, OPER_ASR, shiftVal, exp );
    } else {
      ARMINSTRUCTIONS.insertMOV( OPER_AL, lhs, lhs, OPER_LSR, shiftVal, exp );
    }
    return lhs;
  }
}

LLIR_Register *Cast::castToPointer( LLIR_Register *reg ){
    return reg;
}

LLIR_Register *Cast::castToFloat( LLIR_Register *reg, const IR_Type &sourcetype,
                            IR_Exp *exp ){
  LLIR_Register* lhs = ARMINSTRUCTIONS.CreateRegister( "" );

  switch( sourcetype.getType() ){
    case IR_Type::BOOL:
      ARMINSTRUCTIONS.insertMOV( lhs, reg, exp );
      // Compare with zero, condition flag Z is set to 1 if equal and 0 if not
      ARMINSTRUCTIONS.insertCMP( OPER_AL, reg ,0, exp );
      // Insert 0 if the comparison set Z to 1
      ARMINSTRUCTIONS.insertMOV( OPER_EQ, lhs, 0, exp );
      // Insert 0x3f800000, which is 1 in float representation if the comparison
      // set Z to 0
      ARMINSTRUCTIONS.insertMOV( OPER_NE, lhs, 0x3f800000, exp );
      return lhs;
      break;
    case IR_Type::CHAR:
    case IR_Type::SHORT:
      reg = castToLong( reg, sourcetype, exp );
      [[fallthrough]];
    case IR_Type::LONG:
    case IR_Type::INT:
      ARMINSTRUCTIONS.insertMOV( lhs, reg, exp );
      ARMINSTRUCTIONS.insertFloatOperation( lhs,
        InstructionFactory::SoftFloatSymbol::ITOF, exp );
      return lhs;
      break;
    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::UNSIGNED_SHORT:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::UNSIGNED_INT:
      ARMINSTRUCTIONS.insertMOV( lhs, reg, exp );
      ARMINSTRUCTIONS.insertFloatOperation( lhs,
        InstructionFactory::SoftFloatSymbol::UTOF, exp );
      return lhs;
      break;
    case IR_Type::LONG_LONG:
      ARMINSTRUCTIONS.insertLLTOF( OPER_AL, lhs, reg, exp );
      return lhs;
    case IR_Type::UNSIGNED_LONG_LONG:
      ARMINSTRUCTIONS.insertULLTOF( OPER_AL, lhs, reg, exp );
      return lhs;
    case IR_Type::DOUBLE:
    case IR_Type::LONG_DOUBLE:
      // Sanity check.
      ufAssert( reg->GetNumberOfChildren() == 2 );
      // Insert the call.
      ARMINSTRUCTIONS.insertDTOF( OPER_AL, lhs, reg, exp );
      return lhs;
      break;
    default:
      return lhs;
      break;
  }
  return reg;
}

LLIR_Register *Cast::floatToInt( LLIR_Register *reg, const IR_Type &destinationtype,
                           IR_Exp * exp ){
  LLIR_Register *temp = NULL;
  LLIR_Register* lhs = ARMINSTRUCTIONS.CreateRegister( "" );
  ARMINSTRUCTIONS.insertMOV( lhs, reg, exp );
  switch( destinationtype.getType() ){
    case IR_Type::BOOL:
      ARMINSTRUCTIONS.insertFloatOperation( lhs,
      InstructionFactory::SoftFloatSymbol::FTOI, exp);
      return castToBool( lhs,exp );
      break;
    case IR_Type::CHAR:
      ARMINSTRUCTIONS.insertFloatOperation( lhs,
        InstructionFactory::SoftFloatSymbol::FTOI, exp);
      ARMINSTRUCTIONS.insertAND( lhs,lhs,0xFF,exp );
      return lhs;
      break;
    case IR_Type::SHORT:
      ARMINSTRUCTIONS.insertFloatOperation( lhs,
        InstructionFactory::SoftFloatSymbol::FTOI, exp );
      temp = ARMINSTRUCTIONS.CreateRegister( "" );
      ARMINSTRUCTIONS.insertMOV( temp, 0xFF00, exp );
      ARMINSTRUCTIONS.insertADD( temp, temp, 0xFF, exp );
      ARMINSTRUCTIONS.insertAND( lhs, lhs, temp, exp );
      return lhs;
      break;
    case IR_Type::INT:
      ARMINSTRUCTIONS.insertFloatOperation( lhs,
        InstructionFactory::SoftFloatSymbol::FTOI, exp );
      return lhs;
      break;
    case IR_Type::UNSIGNED_CHAR:
      ARMINSTRUCTIONS.insertFloatOperation( lhs,
        InstructionFactory::SoftFloatSymbol::FTOU, exp );
      ARMINSTRUCTIONS.insertAND( lhs, lhs, 0xFF, exp );
      return lhs;
      break;
    case IR_Type::UNSIGNED_SHORT:
      ARMINSTRUCTIONS.insertFloatOperation( lhs,
        InstructionFactory::SoftFloatSymbol::FTOU, exp );
      temp = ARMINSTRUCTIONS.CreateRegister( "" );
      ARMINSTRUCTIONS.insertMOV( temp, 0xFF00, exp );
      ARMINSTRUCTIONS.insertADD( temp, temp, 0xFF, exp );
      ARMINSTRUCTIONS.insertAND( lhs, lhs, temp, exp );
      return lhs;
      break;
    case IR_Type::UNSIGNED_INT:
      ARMINSTRUCTIONS.insertFloatOperation( lhs,
        InstructionFactory::SoftFloatSymbol::FTOU, exp );
      return lhs;
      break;
    default:
      return lhs;
      break;
  }
  return lhs;
}

LLIR_Register* Cast::doubleToInt( LLIR_Register *ereg,
  const IR_Type &destinationtype, IR_Exp * exp ){
  if ( destinationtype.getType() == IR_Type::LONG_LONG ){
    // Sanity check.
    ufAssert( ereg->GetNumberOfChildren() == 2 );
    // Create extended result register.
    LLIR_Register *result = ARMINSTRUCTIONS.CreateERegister( "" );
    // Sanity check.
    ufAssert( result->GetNumberOfChildren() == 2 );
    // Insert the call to DTOLL routine.
    ARMINSTRUCTIONS.insertDTOLL( OPER_AL, result, ereg, exp );
    return result;
  }
  else if ( destinationtype.getType() == IR_Type::UNSIGNED_LONG_LONG ){
    // Sanity check.
    ufAssert( ereg->GetNumberOfChildren() == 2 );
    // Create extended result register.
    LLIR_Register *result = ARMINSTRUCTIONS.CreateERegister( "" );
    // Sanity check.
    ufAssert( result->GetNumberOfChildren() == 2 );
    // Insert the call to DTOULL routine.
    ARMINSTRUCTIONS.insertDTOULL( OPER_AL, result, ereg, exp );
    return result;
  }
  // Implement insertDTOI/UI in if and call it here to use softfloat routines.
  // For all other destinationtypes the double value is casted to a float first.
  else {
    // Sanity check.
    ufAssert( ereg->GetNumberOfChildren() == 2 );
    // Create a register to store the float value.
    LLIR_Register *castedToFloat;
    // Cast the given value to float.
    IR_Type sourcetype = IR_Type::DOUBLE;
    castedToFloat = castToFloat( ereg, sourcetype, exp );
    // Float to integer.
    return floatToInt( castedToFloat, destinationtype, exp );
  }
}

LLIR_Register* Cast::castToLonglong( LLIR_Register *reg,
  const IR_Type &sourcetype, IR_Exp *exp ){
  // Create an extended result register.
  LLIR_Register *eresult = ARMINSTRUCTIONS.CreateERegister( "" );
  // Sanity check.
  ufAssert( eresult->GetNumberOfChildren() == 2 );

  // If the sourcetype is < 32 bit, the top bits have to be filled according to
  // their signedness. LSR shifts the value and fills the top bits with zeros.
  // ASR shifts the value and fills the top bits according to the highest bit of
  // the input value.
  if( sourcetype.getType() == IR_Type::CHAR ||
      sourcetype.getType() == IR_Type::UNSIGNED_CHAR ||
      sourcetype.getType() == IR_Type::SHORT ||
      sourcetype.getType() == IR_Type::UNSIGNED_SHORT ){
    // Determine shift value for the source value in the least significant
    // register corresponding to the bitsize of the source value.
    size_t shiftVal = ( sourcetype.bitSize() == 16 ? 16 : 24 );
    // Shift.
    ARMINSTRUCTIONS.insertMOV( OPER_AL, reg, reg, OPER_LSL, shiftVal, exp );
    if( sourcetype.isSignedType() )
      ARMINSTRUCTIONS.insertMOV( OPER_AL, reg, reg, OPER_ASR, shiftVal, exp );
    else
      ARMINSTRUCTIONS.insertMOV( OPER_AL, reg, reg, OPER_LSR, shiftVal, exp );
  }

  // Move value of reg into the first/least significant child.
  ARMINSTRUCTIONS.insertMOV( eresult->GetFirstChild(), reg, exp );
  // Fill up the most significant/second child according to the sinedness of the
  // input value. LSR shifts the value and fills the top bits with zeros. ASR
  // shifts the value and fills the top bits according to the highest bit of the
  // input value.
  if( sourcetype.isSignedType() ) {
    ARMINSTRUCTIONS.insertMOV( OPER_AL,
                             eresult->GetNextChild( eresult->GetFirstChild() ),
                             reg, OPER_ASR, 32, exp );
  } else {
    ARMINSTRUCTIONS.insertMOV( OPER_AL,
                             eresult->GetNextChild( eresult->GetFirstChild() ),
                             reg, OPER_LSR, 32, exp );
  }
  return eresult;
}

LLIR_Register* Cast::castToDouble( LLIR_Register *reg, const IR_Type &sourcetype,
                                    IR_Exp *exp ){
  // Create extended return register.
  LLIR_Register* result = ARMINSTRUCTIONS.CreateERegister( "" );
  // Sanity check.
  ufAssert( result->GetNumberOfChildren() == 2 );

  if ( sourcetype.getType() == IR_Type::LONG_LONG ) {
    // Sanity check.
    ufAssert( reg->GetNumberOfChildren() == 2 );
    // Insert the call.
    ARMINSTRUCTIONS.insertLLTOD( OPER_AL, result, reg, exp );
  }

  else if ( sourcetype.getType() == IR_Type::UNSIGNED_LONG_LONG ) {
    // Sanity check.
    ufAssert( reg->GetNumberOfChildren() == 2 );
    // Insert the call.
    ARMINSTRUCTIONS.insertULLTOD( OPER_AL, result, reg, exp );
  }

  else if ( sourcetype.getType() == IR_Type::FLOAT ) {
    // Sanity check.
    ufAssert( reg->GetNumberOfChildren() != 2 );
    // Insert the call.
    ARMINSTRUCTIONS.insertFTOD( OPER_AL, result, reg, exp );
  }
  // Else first cast to float and cast the new float value to double.
  else {
    // Create a register to store the float value.
    LLIR_Register *castedToFloat;
    // Cast of the given value to a float value.
    castedToFloat = castToFloat( reg, sourcetype, exp );

    // Cast the float value to a double value.
    ARMINSTRUCTIONS.insertFTOD( OPER_AL, result, castedToFloat, exp );
  }
  return result;
}

int Cast::castingCosts( const IR_Type &sourcetype,
                        const IR_Type &destinationtype )
{
  int costsForAll = CT( INS_MOV_32 );
  int floatOperationCost = CT( INS_BL_32 ) + CT( INS_MOV_32 );
  if(sourcetype.isEqual( destinationtype.getType() ) ){
    return 0;
  }
  else if( sourcetype.isIntegralType() ){
  switch( destinationtype.getType() ){
    case IR_Type::BOOL:
      if ( sourcetype.getType() == IR_Type::LONG_LONG ||
           sourcetype.getType() == IR_Type::UNSIGNED_LONG_LONG )
        return ( costsForAll + CT( INS_CMP_32 ) + 3 * CT( INS_MOV_32 ) );
      else
        return ( costsForAll + CT( INS_CMP_32 ) + 2 * CT( INS_MOV_32 ) );
      break;
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
      if( destinationtype.bitSize() < sourcetype.bitSize() ){
        if ( sourcetype.getType() == IR_Type::LONG_LONG ||
             sourcetype.getType() == IR_Type::UNSIGNED_LONG_LONG )
          return ( costsForAll + CT( INS_AND_32 ) + CT( INS_MOV_32 ) );
        else
          return ( costsForAll + CT( INS_AND_32 ) );
      }
      else
        return 0;
      break;
    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT:
      if( destinationtype.bitSize() < sourcetype.bitSize() )
        if ( sourcetype.getType() == IR_Type::LONG_LONG ||
             sourcetype.getType() == IR_Type::UNSIGNED_LONG_LONG ){
          return ( costsForAll + 2 * CT( INS_MOV_32 ) + CT( INS_ADD_32 ) +
                 CT( INS_AND_32 ) );
        }
        else
          return ( costsForAll + CT( INS_MOV_32 ) + CT( INS_ADD_32 ) +
                 CT( INS_AND_32 ) );
      else
        return 0;
      break;
    case IR_Type::LONG:
    case IR_Type::INT:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::UNSIGNED_INT:
      if ( sourcetype.getType() == IR_Type::LONG_LONG ||
           sourcetype.getType() == IR_Type::UNSIGNED_LONG_LONG )
        return ( costsForAll + CT( INS_MOV_32 ) );
      else {
        if( destinationtype.bitSize() < sourcetype.bitSize() )
          return ( costsForAll + CT( INS_AND_32 ) );
        else if( ( sourcetype.bitSize() == 16 ) && sourcetype.isSignedType() )
          return ( costsForAll + CT( INS_CMP_32 ) + CT( INS_ADD_32 ) +
                  CT( INS_ADD_32 ) );
        else
          return 0;
      }
      break;
    case IR_Type::LONG_LONG:
    case IR_Type::UNSIGNED_LONG_LONG:
      return ( costsForAll + 6 * CT( INS_MOV_32 ) );
      break;
    case IR_Type::POINTER:
      return 0;
      break;
    case IR_Type::FLOAT:
      return ( costsForAll + floatOperationCost );
    case IR_Type::DOUBLE:
    case IR_Type::LONG_DOUBLE:
      // Long long integer cast to double costs.
      if ( sourcetype.getType() == IR_Type::LONG_LONG ||
           sourcetype.getType() == IR_Type::UNSIGNED_LONG_LONG ) {
        return ( costsForAll + CT( INS_BL_32 ) + 4 * CT( INS_MOV_32 ) );
      }
      else {
        // Int to float costs.
        costsForAll += floatOperationCost;
        // Float to double costs.
        costsForAll += CT( INS_BL_32 ) + 3 * CT( INS_MOV_32 );
        return costsForAll;
      }
      break;
    default:
      return costsForAll;
    break;
    }
  }
  else if( sourcetype.isPointerType() ){
    switch( destinationtype.getType() ){
      case IR_Type::BOOL:
        return ( costsForAll + CT( INS_CMP_32 )+ 2 * CT( INS_MOV_32 ) );
        break;
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR:
        return ( costsForAll + CT( INS_AND_32 ) );
        break;
      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT:
        return ( costsForAll + CT( INS_MOV_32 ) + CT( INS_AND_32 ) );
        break;
      default:
        return costsForAll;
        break;
    }
  }
  else if( sourcetype.isFloatType() ){
    if( destinationtype.isIntegralType() ){
      switch( destinationtype.getType() ){
        case IR_Type::BOOL:
          return ( costsForAll + CT( INS_CMP_32 )+ 2* CT( INS_MOV_32 ) );
          break;
        case IR_Type::CHAR:
          return ( costsForAll + CT( INS_AND_32 ) + floatOperationCost );
          break;
        case IR_Type::SHORT:
          return ( costsForAll + CT( INS_MOV_32 ) + CT( INS_AND_32 ) +
                   floatOperationCost );
          break;
        case IR_Type::INT:
          return ( costsForAll + floatOperationCost );
          break;
        case IR_Type::UNSIGNED_CHAR:
          return ( costsForAll + CT( INS_AND_32 ) + floatOperationCost );
          break;
        case IR_Type::UNSIGNED_SHORT:
          return ( CT( INS_MOV_32 ) + CT( INS_AND_32 ) + floatOperationCost );
          break;
        case IR_Type::UNSIGNED_INT:
          return ( costsForAll + floatOperationCost );
          break;
        default:
            return ( costsForAll + floatOperationCost );
            break;
      }
      return costsForAll;
    }
    else if ( destinationtype.getType() == IR_Type::DOUBLE ||
              destinationtype.getType() == IR_Type::LONG_DOUBLE ){
      // Float to double costs.
      return costsForAll + CT( INS_BL_32 ) + 3 * CT( INS_MOV_32 );
    }

  } else if ( sourcetype.getType() == IR_Type::DOUBLE ||
              sourcetype.getType() == IR_Type::LONG_DOUBLE ) {
    if( destinationtype.isIntegralType() ) {
      // Double to long long integer costs.
      if ( destinationtype.getType() == IR_Type::LONG_LONG ||
           destinationtype.getType() == IR_Type::UNSIGNED_LONG_LONG ){
        return CT( INS_BL_32 ) + 4 * CT( INS_MOV_32 );
      }
      // Double to other integer costs.
      else {
        // Double-float costs.
        costsForAll += CT( INS_BL_32 ) + 3 * CT( INS_MOV_32 );
        // Float-int costs.
        IR_Type sourcet = IR_Type::FLOAT;
        IR_Type destinationt = destinationtype.getType();
        costsForAll += castingCosts( sourcet, destinationt );
        return costsForAll;
      }
    }
    else if( destinationtype.getType() == IR_Type::FLOAT ){
      return costsForAll + CT( INS_BL_32 ) + 3 * CT( INS_MOV_32 );
    }
    return costsForAll;
  }
  return costsForAll;
}

int Cast::castingCosts( const IR_Type::Type sourcetype,
                        const IR_Type::Type destinationtype, IR* ir ){
  // Create temporary IR_Types.
  IR_Type s( sourcetype );
  s.setIR( ir );
  IR_Type d( destinationtype );
  d.setIR( ir );

  return castingCosts( s, d );
}

// Truncates a given register reg and places it in the res register.
void Cast::truncate( LLIR_Register* reg, LLIR_Register* res,
                     const IR_Type& sourcetype, IR_Exp *exp )
{
  // Check if the sourcetype is supported.
  ufAssert( sourcetype.isIntegralType() && sourcetype.bitSize() <= 32 );

  // Truncate the register to the sourcetype's length.
  if ( sourcetype.getType() != IR_Type::BOOL ) {
    int shiftLen = 32 - sourcetype.bitSize();

    // First, move the value to the resulting reg.
    if ( res != reg )
      ARMINSTRUCTIONS.insertMOV( res, reg, exp );
    // Then truncate.
    ARMINSTRUCTIONS.insertMOV( OPER_AL, res, res, OPER_LSL, shiftLen, exp );
    ARMINSTRUCTIONS.insertMOV( OPER_AL, res, res, OPER_LSR, shiftLen, exp );

  } else {
    // Handle bool separately.
    LLIR_Register* tmpRes = castToBool( reg, exp );
    // Move into the acutal result register.
    ARMINSTRUCTIONS.insertMOV( res, tmpRes, exp );
  }

  return;
}

int Cast::truncateCosts( const IR_Type& sourcetype )
{
  // Check if the sourcetype is supported.
  ufAssert( sourcetype.isIntegralType() && sourcetype.bitSize() <= 32 );

  // Right now, all types cost the same to truncate.
  return 3 * CT( INS_MOV_32 );
}

LLIR_Register* Cast::doCasting( const IR_Type& dstType, const IR_Type& srcType,
                                LLIR_Register* srcReg, IR_Exp* exp )
{
  // Small helper lambda.
  // Long and int are the same for ARM7.
  auto sameTargetType = [] ( const IR_Type& a, const IR_Type& b ) -> bool {
    if ( a.isEqual( b.getType() ) ||
         ( a.getType() == IR_Type::LONG && b.getType() == IR_Type::INT ) ||
         ( a.getType() == IR_Type::INT && b.getType() == IR_Type::LONG ) ||
         ( a.getType() == IR_Type::UNSIGNED_INT &&
           b.getType() == IR_Type::UNSIGNED_LONG ) ||
         ( a.getType() == IR_Type::UNSIGNED_LONG &&
           b.getType() == IR_Type::UNSIGNED_INT ) ||
         ( a.getType() == IR_Type::DOUBLE &&
           b.getType() == IR_Type::LONG_DOUBLE ) ||
         ( a.getType() == IR_Type::LONG_DOUBLE &&
           b.getType() == IR_Type::DOUBLE ) ) {
      return true;
    } else {
      return false;
    }
  };

  // No casting if both types are identical.
  if( sameTargetType( srcType, dstType ) ) {
    return( srcReg );
  }
  // If the srcType is _Bool the value can just be passed except when
  // the dstType is float
  if(srcType.getType() == IR_Type::BOOL ){
    if( dstType.getType() == IR_Type::FLOAT ){
      return(castToFloat( srcReg, srcType, exp ) );
    } else if( dstType.getType() == IR_Type::DOUBLE ||
               dstType.getType() == IR_Type::LONG_DOUBLE ){
      return( castToDouble( srcReg, srcType, exp ) );

    } else{
      return( srcReg );
    }
  }
  // This prevents the mistake that getUnsignedType() will return
  // UNSIGNED_INT for BOOL in the two following checks
  if( dstType.getType() != IR_Type::BOOL ){
    // No casting if the difference is just about signed or unsigned
    if( srcType.isSignedType() && dstType.isUnsignedType() ){
      if( sameTargetType( *srcType.getUnsignedType(), dstType.getType() ) ) {
        return( srcReg );
      }
    }
    if( srcType.isUnsignedType() && dstType.isSignedType() ){
      if( sameTargetType( srcType, *dstType.getUnsignedType() ) ) {
        return( srcReg );
      }
    }
  }
  if( srcType.isIntegralType() ){
    switch( dstType.getType() ){
      case IR_Type::BOOL:
        return( castToBool( srcReg, exp ) );
        break;
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR:
        return( castToChar( srcReg, srcType, dstType,
                 exp ) );
        break;
      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT:
        return( castToShort( srcReg, srcType, dstType,
                 exp ) );
        break;
      case IR_Type::LONG:
      case IR_Type::INT:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::UNSIGNED_INT:
        return( castToLong( srcReg, srcType, exp ) );
        break;
      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
        return( castToLonglong( srcReg, srcType, exp ) );
        break;
      case IR_Type::POINTER:
      case IR_Type::ARRAY:
        return( castToPointer( srcReg ) );
        break;
      case IR_Type::FLOAT:
        return( castToFloat( srcReg, srcType, exp ) );
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE:
        return( castToDouble( srcReg, srcType, exp ) );
        break;
      default:
        break;
    }
  }
  else if( srcType.isPointerType() ){
    switch( dstType.getType() ){
      case IR_Type::BOOL:
        return( castToBool( srcReg, exp ) );
        break;
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR:
        return( castToChar( srcReg, srcType, dstType,
                 exp ) );
        break;
      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT:
        return( castToShort( srcReg, srcType, dstType,
                 exp ) );
        break;
      default:
        return( srcReg );
        break;
    }
  }
  else if( srcType.isFloatType() ){
    if( srcType.getType() == IR_Type::FLOAT ) {
      if( dstType.isIntegralType() ){
        return( floatToInt( srcReg, dstType, exp ) );
      }
      else if ( dstType.getType() == IR_Type::DOUBLE ||
                dstType.getType() == IR_Type::LONG_DOUBLE ){
        return( castToDouble( srcReg, srcType, exp ) );
      }
    }
    else if ( srcType.getType() == IR_Type::DOUBLE ||
              srcType.getType() == IR_Type::LONG_DOUBLE ) {
      if( dstType.isIntegralType() ) {
        return( doubleToInt( srcReg, dstType, exp ) );
      } else if( dstType.getType() == IR_Type::FLOAT ){
        return( castToFloat( srcReg, srcType, exp ) );
      }
    }
  }

  // This should never happen.
  throw std::runtime_error( "Uncovered type!" );
}

