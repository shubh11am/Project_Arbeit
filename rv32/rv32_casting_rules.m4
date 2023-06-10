#
#
#  This source file belongs to the
#
#           Hamburg University of Technology (TUHH)
#             WCC Compiler Framework
#
#  and is property of its respective copyright holder. It must neither be used
#  nor published even in parts without explicit written permission.
#
#  Copyright 2009 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#

explicit_castable_reg: tpm_UnaryExpCAST( reg )
{
  $cost[0] = $cost[2];
}
=
{
   RV32::DEBUG_RULE_ACTION( "explicit_castable_reg: tpm_UnaryExpCAST( reg )", $1 );

  auto &p = $action[2]();
  return( p );
};


areg: tpm_ImplicitCast( constAddress )
{
  // This is not really a casting rule, it just loads constant addresses to an
  // address register. But until this casting operator is met, it is not clear
  // that an integer constant really is a constant address.
  IR_Type *type_tmp = $1->getExp()->getImplicitCastType();
  if ( RV32::isARegType( *type_tmp ) )
    $cost[0] = $cost[2] + RV32I::OperationFormat::RRC12_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  if ( type_tmp && ! type_tmp->getRefCount() )
    delete type_tmp;
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_ImplicitCast( constAddress )", $1 );

  auto adr = $action[2]().getIntValue();
  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertMOVConstant( r, adr, $1->getExp() );

  return( r );
};


constAddress: tpm_IntConstExp
{
  // This is only used to propagate the address value. A subsequent cast
  // operation will do the actual work.
  $cost[0] = 0;
}
=
{
  RV32::DEBUG_RULE_ACTION( "constAddress: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue() );
};


areg: implicit_castable_reg
{
  IR_Type &sourceType = $1->getExp()->getType();
  IR_Type &targetType = *$1->getExp()->getImplicitCastType();

  if ( RV32::isARegType( targetType ) )
    $cost[0] = $cost[1]; 
    // + Cast::castingCost( targetType, sourceType ); castingCost not yet operational
  else
    $cost[0] = COST_INFINITY;

  if ( ! targetType.getRefCount() )
    delete &targetType;
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: implicit_castable_reg", $1 );

  IR_Type &sourceType = $1->getExp()->getType();
  IR_Type &targetType = *$1->getExp()->getImplicitCastType();
  auto &p = $action[1]();

  auto &r = dynamic_cast<RV_RegV &>(
      RV32::Cast::doCasting( targetType, sourceType, p, $1->getExp() ) );

  if ( ! targetType.getRefCount() )
    delete &targetType;

  return( r );
};


implicit_castable_reg: tpm_ImplicitCast( areg )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "implicit_castable_reg: tpm_ImplicitCast( areg )", $1 );

  auto &p = $action[2]();
  return( p );
};


areg: tpm_UnaryExpCAST( constAddress )
{
  // This is not really a casting rule, it just loads constant addresses to an
  // address register. But until this casting operator is met, it is not clear
  // that an integer constant really is a constant address.
  if ( RV32::isARegType( $1->getExp()->getType() ) )
    $cost[0] = $cost[2] + RV32I::OperationFormat::RC12R_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_UnaryExpCAST( constAddress )", $1 );

  auto adr = $action[2]().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();

  RVINSTRUCTIONS.insertMOVConstant( r, adr, $1->getExp() );

  return( r );
};


reg: explicit_castable_reg
{
  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  IR_Type &sourceType = uexp->getOp().getType();
  IR_Type &targetType = uexp->getType();

  if ( RV32::isDRegType( targetType ) )
    $cost[0] = $cost[1]; 
    // + Cast::castingCost( targetType, sourceType ); castingCost not yet operational
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: explicit_castable_reg", $1 );

  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  IR_Type &sourceType = uexp->getOp().getType();
  IR_Type &targetType = uexp->getType();
  auto &p = $action[1]();

  auto &r = dynamic_cast<RV_RegV &>(
    RV32::Cast::doCasting( targetType, sourceType, p, $1->getExp() ) );


  return( r );
};


explicit_castable_reg: tpm_UnaryExpCAST( areg )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "explicit_castable_reg: tpm_UnaryExpCAST( areg )", $1 );

  auto &p = $action[2]();
  return( p );
};


reg: implicit_castable_reg
{
  IR_Type &sourceType = $1->getExp()->getType();
  IR_Type &targetType = *$1->getExp()->getImplicitCastType();

  if ( RV32::isDRegType( targetType ) )
    $cost[0] = $cost[1]; 
    // + Cast::castingCost( targetType, sourceType ); castingCost not yet operational
  else
    $cost[0] = COST_INFINITY;

  if ( ! targetType.getRefCount() )
    delete &targetType;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: implicit_castable_reg", $1 );

  IR_Type &sourceType = $1->getExp()->getType();
  IR_Type &targetType = *$1->getExp()->getImplicitCastType();
  auto &p = $action[1]();

  auto &r = dynamic_cast<RV_RegV &>(
    RV32::Cast::doCasting( targetType, sourceType, p, $1->getExp() ) );

  if ( ! targetType.getRefCount() )
    delete &targetType;

  return( r );
};


implicit_castable_reg: tpm_ImplicitCast( reg )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "implicit_castable_reg: tpm_ImplicitCast( reg )", $1 );

  auto &p = $action[2]();
  return( p );
};


areg: explicit_castable_reg
{
  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  IR_Type &sourceType = uexp->getOp().getType();
  IR_Type &targetType = uexp->getType();

  if ( RV32::isARegType( targetType ) )
    $cost[0] = $cost[1]; 
    // + Cast::castingCost( targetType, sourceType ); castingCost not yet operational
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: explicit_castable_reg", $1 );

  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  IR_Type &sourceType = uexp->getOp().getType();
  IR_Type &targetType = uexp->getType();

  auto &p = $action[1]();

  auto &r = dynamic_cast<RV_RegV &>(
      RV32::Cast::doCasting( targetType, sourceType, p, $1->getExp() ) );

    return( r );
};