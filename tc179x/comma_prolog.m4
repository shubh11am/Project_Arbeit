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
  isFirstCommaOperand determines whether the given IR expression is the first
  operand of a comma expression.
*/
bool isFirstCommaOperand( const IR_Exp &exp )
{
  DSTART( "bool isFirstCommaOperand(const IR_Exp&)" );

  auto *parent = dynamic_cast<IR_BinaryExp *>( exp.getParent() );

  return(
    parent && ( parent->getOperator() == IR_BinaryExp::COMMA ) &&
    ( &parent->getOp1() == &exp ) );
};
