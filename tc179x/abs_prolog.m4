/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2010 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


AbsType isAbsCondExp( const IR_Exp &exp )
{
  auto *condExp = dynamic_cast<const IR_CondExp *>( &exp );
  if ( !condExp )
    return( AbsType::ABS_NONE );

  // Pattern matching.
  auto *condition = dynamic_cast<IR_BinaryExp *>( &condExp->getCond() );
  const IR_BinaryExp::Operator condOp = condition->getOperator();
  if ( ( condOp != IR_BinaryExp::LT ) &&
       ( condOp != IR_BinaryExp::GT ) &&
       ( condOp != IR_BinaryExp::LEQ ) &&
       ( condOp != IR_BinaryExp::GEQ ) )
    return( AbsType::ABS_NONE );

  // Pattern matching.
  auto *zeroExp = dynamic_cast<IR_IntConstExp *>( &condition->getOp1() );
  if ( !zeroExp )
    zeroExp = dynamic_cast<IR_IntConstExp *>( &condition->getOp2() );
  if ( !zeroExp )
    return( AbsType::ABS_NONE );

  // Pattern matching.
  IR_Exp *aExp =
    ( zeroExp == &condition->getOp1() ) ?
      &condition->getOp2() : &condition->getOp1();
  IR_Exp *positiveSide = nullptr;
  IR_Exp *negativeSide = nullptr;

  if ( aExp == &condition->getOp1() ) {
    switch ( condOp ) {
      case IR_BinaryExp::LT:
      case IR_BinaryExp::LEQ: {
        positiveSide = &condExp->getExp2();
        negativeSide = &condExp->getExp1();
        break;
      }

      case IR_BinaryExp::GT:
      case IR_BinaryExp::GEQ: {
        positiveSide = &condExp->getExp1();
        negativeSide = &condExp->getExp2();
        break;
      }

      default:
        return( AbsType::ABS_NONE );
    }
  } else {
    switch ( condOp ) {
      case IR_BinaryExp::LT:
      case IR_BinaryExp::LEQ: {
        positiveSide = &condExp->getExp1();
        negativeSide = &condExp->getExp2();
        break;
      }

      case IR_BinaryExp::GT:
      case IR_BinaryExp::GEQ: {
        positiveSide = &condExp->getExp2();
        negativeSide = &condExp->getExp1();
        break;
      }

      default:
        return( AbsType::ABS_NONE );
    }
  }

  auto *minusExp = dynamic_cast<IR_UnaryExp *>( negativeSide );
  if ( !minusExp || ( minusExp->getOperator() != IR_UnaryExp::MINUS ) ||
       ( minusExp->getOp() != *aExp ) || ( *positiveSide != *aExp ) )
    return( AbsType::ABS_NONE );

  // 3)
  if ( !isDRegType( *condExp ) || !condExp->getType().isIntegralType() )
    return( AbsType::ABS_NONE );

  // 4)
  if ( condition->getOp1().getType().isUnsignedType() ||
       condition->getOp2().getType().isUnsignedType() )
    return( AbsType::ABS_NONE );

  // 1) and 2)
  bool thenDef = false;
  bool elseDef = false;
  condExp->getExp1().iterateExpressions(
    ( void (*)( IR_Exp &, void * ) ) checkDefOrVolatile, &thenDef );
  condExp->getExp2().iterateExpressions(
    ( void (*)( IR_Exp &, void * ) ) checkDefOrVolatile, &elseDef );

  if ( thenDef || elseDef )
    return( AbsType::ABS_NONE );

  return( AbsType::ABS_ABS );
};
