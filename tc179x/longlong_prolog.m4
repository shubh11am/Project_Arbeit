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


/*
  Workaround for getting an integer type object
*/
shared_ptr<IR_SourceSymbol> getIntSymbol( void )
{
  return( make_shared<IR_SourceSymbol>( "", *new IR_Type( IR_Type::INT ) ) );
};


/*
  Extracts the low-valued register from a long long ereg.
*/
LLIR_Register *getLVLLChild( LLIR_Register *ereg )
{
  if ( ereg )
    return( ereg->GetFirstChild() );
  else
    return( nullptr );
};


/*
  Extracts the high-valued register from a long long ereg.
*/
LLIR_Register *getHVLLChild( LLIR_Register *ereg )
{
  if ( ereg )
    return( ereg->GetNextChild( ereg->GetFirstChild() ) );
  else
    return( nullptr );
};


/*
  Returns whether 'exp' is a unary increment.
*/
bool isUnaryIncrement( const IR_Exp &exp )
{
  auto *uexp = dynamic_cast<const IR_UnaryExp *>( &exp );
  return(
    uexp &&
    ( ( uexp->getOperator() == IR_UnaryExp::PREINC ) ||
      ( uexp->getOperator() == IR_UnaryExp::POSTINC ) ) );
};


/*
  Returns whether 'exp' is a unary decrement.
*/
bool isUnaryDecrement( const IR_Exp &exp )
{
  auto *uexp = dynamic_cast<const IR_UnaryExp *>( &exp );
  return(
    uexp &&
    ( ( uexp->getOperator() == IR_UnaryExp::PREDEC ) ||
      ( uexp->getOperator() == IR_UnaryExp::POSTDEC ) ) );
};


/*
  Returns whether the given expression is added to or subtracted from a pointer.
*/
bool isUsedForPointerArithmetic( const IR_Exp &exp )
{
  auto *aexp = dynamic_cast<const IR_AssignExp *>( exp.getParent() );
  auto *bexp = dynamic_cast<const IR_BinaryExp *>( exp.getParent() );

  if ( ( aexp && ( ( aexp->getOperator() == IR_AssignExp::PLUS ) ||
                   ( aexp->getOperator() == IR_AssignExp::MINUS ) ) ) ||
       ( bexp && ( ( bexp->getOperator() == IR_BinaryExp::PLUS ) ||
                   ( bexp->getOperator() == IR_BinaryExp::MINUS ) ) ) ) {
    IR_Exp &op1 = aexp ? aexp->getLHS() : bexp->getOp1();
    IR_Exp &op2 = aexp ? aexp->getRHS() : bexp->getOp2();
    IR_Exp &otherOperand = ( &exp == &op1 ) ? op2 : op1;

    return( dynamic_cast<IR_PointerType *>( &otherOperand.getType() ) );
  } else
    return( false );
};


/*
  Returns whether the given expression is an index inside an index expression,
  as, e.g., "x" in "a[x]" is.
*/
bool isUsedAsIndex( const IR_Exp &exp )
{
  auto *iexp = dynamic_cast<const IR_IndexExp *>( exp.getParent() );
  return( iexp && ( &iexp->getIndexExp() == &exp ) );
};
