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


MinMaxRegisterComparison::MinMaxRegisterComparison( void ) :
  op1 { nullptr },
  op2 { nullptr },
  r1 { ref( dummyDRegV ) },
  r2 { ref( dummyDRegV ) }
{
};


MinMaxRegisterComparison::MinMaxRegisterComparison( LLIR_Register *p1,
                                                    LLIR_Register *p2,
                                                    const reference_wrapper<WIR::TC_DRegV> &p3,
                                                    const reference_wrapper<WIR::TC_DRegV> &p4 ) :
  op1 { p1 },
  op2 { p2 },
  r1 { p3 },
  r2 { p4 }
{
};


MinMaxConstantComparison::MinMaxConstantComparison( void ) :
  op1 { nullptr },
  r { ref( dummyDRegV ) },
  v { 0 }
{
};


MinMaxConstantComparison::MinMaxConstantComparison( LLIR_Register *p1,
                                                    const reference_wrapper<WIR::TC_DRegV> &p2,
                                                    const IR_Integer &c ) :
  op1 { p1 },
  r { p2 },
  v { c }
{
};


/*
  A simple ICD-C iterator to detect expressions with defs or volatile exps.
*/
void checkDefOrVolatile( IR_Exp &exp, bool *hasDef )
{
  DSTART( "void checkDefOrVolatile(IR_Exp&, bool*)" );

  if ( exp.isDef() ||
       exp.getType().isVolatile() ||
       dynamic_cast<IR_CallExp *>( &exp ) )
    *hasDef = true;
};


/*
  isMinMaxCondExp determines whether the given expression is a conditional
  expression of a certain form feasible for the TriCore's MIN/MAX operations.

  The actually supported forms of an IR expression are:
    ( A < B )  ? A : B
    ( B > A )  ? A : B
    ( A <= B ) ? A : B
    ( B >= A ) ? A : B

  or the analogous forms where the then and else branch are exchanged. The
  former corresponds to a minimum, and the latter to a maximum operation. The IR
  expression can be covered by a TriCore MIN / MAX operation if:

    1) A and B do not define anything
    2) A and B do not contain volatile symbols
    3) the result type is integral and can be stored in a data register
    4) A and B are either both signed or both unsigned
*/
MinMaxType isMinMaxCondExp( const IR_Exp &exp )
{
  DSTART( "MinMaxType isMinMaxCondExp(const IR_Exp&)" );

  auto *condExp = dynamic_cast<const IR_CondExp *>( &exp );

  if ( !condExp )
    return( MinMaxType::MINMAX_NONE );

  // Pattern Matching.
  auto *condition = dynamic_cast<IR_BinaryExp *>( &condExp->getCond() );
  const IR_BinaryExp::Operator condOp = condition->getOperator();
  if ( ( condOp != IR_BinaryExp::LT ) &&
       ( condOp != IR_BinaryExp::GT ) &&
       ( condOp != IR_BinaryExp::LEQ ) &&
       ( condOp != IR_BinaryExp::GEQ ) )
    return( MinMaxType::MINMAX_NONE );

  // 3)
  if ( !isDRegType( *condExp ) || !condExp->getType().isIntegralType() )
    return( MinMaxType::MINMAX_NONE );

  // 4)
  // If the sign of the expressions differs, we may not use the MIN/MAX
  // operations, because it is unclear whether they mimick the C comparison
  // semantics correctly in that case.
  if ( condExp->getExp1().getType().isUnsignedType() !=
       condExp->getExp2().getType().isUnsignedType() )
    return( MinMaxType::MINMAX_NONE );

  // 1) and 2)
  bool thenDef = false;
  bool elseDef = false;
  condExp->getExp1().iterateExpressions(
    (void(*)(IR_Exp&, void*) ) checkDefOrVolatile, &thenDef );
  condExp->getExp2().iterateExpressions(
    (void(*)(IR_Exp&, void*) ) checkDefOrVolatile, &elseDef );

  if ( thenDef || elseDef )
    return( MinMaxType::MINMAX_NONE );

  bool isMin = false;
  bool isMax = false;

  // Final stage of pattern matching:
  // Minimum:
  // ( A < B )  ? A : B
  // ( B > A )  ? A : B
  // ( A <= B ) ? A : B
  // ( B >= A ) ? A : B
  //
  // Maximum:
  // ( A < B )  ? B : A
  // ( B > A )  ? B : A
  // ( A <= B ) ? B : A
  // ( B >= A ) ? B : A
  if ( ( condition->getOp1() == condExp->getExp1() ) &&
       ( condition->getOp2() == condExp->getExp2() ) ) {
    switch ( condOp ) {
      case IR_BinaryExp::LEQ:
      case IR_BinaryExp::LT: {
        isMin = true;
        break;
      }

      case IR_BinaryExp::GEQ:
      case IR_BinaryExp::GT: {
        isMax = true;
        break;
      }

      default:
        return( MinMaxType::MINMAX_NONE );
    }
  } else

  if ( ( condition->getOp1() == condExp->getExp2() ) &&
       ( condition->getOp2() == condExp->getExp1() ) ) {
    switch ( condOp ) {
      case IR_BinaryExp::LEQ:
      case IR_BinaryExp::LT: {
        isMax = true;
        break;
      }

      case IR_BinaryExp::GEQ:
      case IR_BinaryExp::GT: {
        isMin = true;
        break;
      }

      default:
        return( MinMaxType::MINMAX_NONE );
    }
  } else
    return( MinMaxType::MINMAX_NONE );

  // Generate result depending on signedness.
  const bool isUnsigned = condExp->getExp1().getType().isUnsignedType();
  if ( isMin ) {
    if ( isUnsigned )
      return( MinMaxType::MINMAX_MIN_UNSIGNED );
    else
      return( MinMaxType::MINMAX_MIN_SIGNED );
  } else

  if ( isMax ) {
    if ( isUnsigned )
      return( MinMaxType::MINMAX_MAX_UNSIGNED );
    else
      return( MinMaxType::MINMAX_MAX_SIGNED );
  } else
    return( MinMaxType::MINMAX_NONE );
};
