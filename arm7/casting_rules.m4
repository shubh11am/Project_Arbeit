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


explicit_castable_reg: tpm_UnaryExpCAST( dreg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "explicit_castable_reg: tpm_UnaryExpCAST( dreg )", $1 );

  return( $action[2]() );
};


explicit_castable_reg: tpm_UnaryExpCAST( ereg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "explicit_castable_reg: tpm_UnaryExpCAST( ereg )", $1 );

  return( $action[2]() );
};


cast_result: explicit_castable_reg
{
  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  auto &sourcetype = uexp->getOp().getType();
  auto &destinationtype = uexp->getType();

  $cost[0] = $cost[1] + Cast::castingCosts( sourcetype, destinationtype );
}
=
{
  DEBUG_RULE_ACTION( "cast_result: explicit_castable_reg", $1 );

  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  auto &sourcetype = uexp->getOp().getType();
  auto &destinationtype = uexp->getType();

  auto *srcReg = $action[1]();

  return( Cast::doCasting( destinationtype, sourcetype, srcReg, uexp ) );
};


implicit_castable_reg: tpm_ImplicitCast( dreg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "implicit_castable_reg: tpm_ImplicitCast( dreg )", $1 );

  return( $action[2]() );
};


implicit_castable_reg: tpm_ImplicitCast( ereg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "implicit_castable_reg: tpm_ImplicitCast( ereg )", $1 );

  return( $action[2]() );
};


cast_result:implicit_castable_reg
{
  auto &sourcetype = $1->getExp()->getType();
  auto &destinationtype = *( $1->getExp()->getImplicitCastType() );

  $cost[0] = $cost[1] + Cast::castingCosts( sourcetype, destinationtype );
}
=
{
  DEBUG_RULE_ACTION( "cast_result:implicit_castable_reg", $1 );

  auto &sourcetype = $1->getExp()->getType();
  auto &destinationtype = *( $1->getExp()->getImplicitCastType() );

  auto *srcReg = $action[1]();

  return(
    Cast::doCasting( destinationtype, sourcetype, srcReg, $1->getExp() ) );
};


dreg: tpm_UnaryExpCAST( tpm_IntConstExp )
{
  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  auto &destinationtype = uexp->getType();

  switch ( destinationtype.getType() ) {
    case IR_Type::BOOL:
    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::CHAR: {
      $cost[0] = $cost[1] + $cost[2] + CT( INS_MOV_32 );
      break;
      }

    case IR_Type::UNSIGNED_SHORT:
    case IR_Type::SHORT: {
      $cost[0] = $cost[1] + $cost[2] + CT( INS_MOV_32) + CT( INS_ORR_32);
      break;
      }

    default: {
      $cost[0] = COST_INFINITY;
      break;
    }
  }
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpCAST( tpm_IntConstExp )", $1 );

  long long constValue =
    dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue().getIntValue();
  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  auto &destinationtype = uexp->getType();

  auto *lhs = ARMINSTRUCTIONS.CreateRegister( "" );

  switch ( destinationtype.getType() ) {
    case IR_Type::BOOL: {
      if ( constValue == 0 )
        ARMINSTRUCTIONS.insertMOV( lhs, 0, $1->getExp() );
      else
        ARMINSTRUCTIONS.insertMOV( lhs, 1, $1->getExp() );
      break;
    }

    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::CHAR: {
      constValue &= 0xFF;
      ARMINSTRUCTIONS.insertMOV( lhs, constValue, $1->getExp() );
      break;
    }

    case IR_Type::UNSIGNED_SHORT:
    case IR_Type::SHORT: {
      constValue &= 0xFFFF;
      ARMINSTRUCTIONS.insertMOV_ORR( lhs, constValue, $1->getExp() );
      break;
    }

    default:
      break;
  }

  return( lhs );
};


dreg: tpm_ImplicitCast( tpm_IntConstExp )
{
  auto &destinationtype = *( $1->getExp()->getImplicitCastType() );

  switch ( destinationtype.getType() ) {
    case IR_Type::BOOL:
    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::CHAR: {
      $cost[0] = $cost[1] + $cost[2] + CT( INS_MOV_32 );
      break;
    }

    case IR_Type::UNSIGNED_SHORT:
    case IR_Type::SHORT: {
      $cost[0] = $cost[1] + $cost[2] + CT( INS_MOV_32) + CT( INS_ORR_32);
      break;
    }

    default: {
      $cost[0] = COST_INFINITY;
      break;
    }
  }
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_ImplicitCast( tpm_IntConstExp )", $1 );

  long long constValue =
    dynamic_cast<IR_IntConstExp*>( $1->getExp() )->getValue().getIntValue();
  auto &destinationtype = *( $1->getExp()->getImplicitCastType() );

  auto *lhs = ARMINSTRUCTIONS.CreateRegister( "" );

  switch ( destinationtype.getType() ) {
    case IR_Type::BOOL: {
      if ( constValue == 0 )
        ARMINSTRUCTIONS.insertMOV( lhs, 0, $1->getExp() );
      else
        ARMINSTRUCTIONS.insertMOV( lhs, 1, $1->getExp() );
      break;
    }

    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::CHAR: {
      constValue &= 0xFF;
      ARMINSTRUCTIONS.insertMOV( lhs, constValue, $1->getExp() );
      break;
    }

    case IR_Type::UNSIGNED_SHORT:
    case IR_Type::SHORT: {
      constValue &= 0xFFFF;
      ARMINSTRUCTIONS.insertMOV_ORR( lhs, constValue, $1->getExp() );
      break;
    }

    default:
      break;
  }

  return( lhs );
};


areg: tpm_ImplicitCast( tpm_StringConstExp )
{
  // TODO: create function for this rule and areg: tpm_StringConstExp
  $cost[0] = CT( INS_LDR_32 );
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_ImplicitCast( tpm_StringConstExp )", $1 );

  auto *stringConstExp = dynamic_cast<IR_StringConstExp *>( $1->getExp() );
  string stringConst = stringConstExp->getValue();

  // Generate target register holding the result of the current operation.
  auto *reg = ARMINSTRUCTIONS.CreateRegister( "", true );

  // Obtain label in section .rodata.
  string label = ARMCODESEL->getRODataLabel( stringConst );

  // Check if a literal pool was already created.
  auto *func = ARMCODESEL->getLastLLIRFunction();
  string poolLabel = func->getDataSectionAddressLabel( label );

  // If not, create one.
  if ( poolLabel.empty() ) {
    auto *addrBB = createLiteralPool( label, $1->getExp() );
    poolLabel = addrBB->GetLabel();
  }

  // Generate the operation.
  ARMINSTRUCTIONS.insertLDR( OPER_AL, reg, false, poolLabel, $1->getExp() );

  return( reg );
};


###############################################################################
#
# Pointer support (conversions to and from aregs)
#
###############################################################################

implicit_castable_reg: tpm_ImplicitCast( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "implicit_castable_reg: tpm_ImplicitCast( areg )", $1 );

  return( $action[2]() );
};


explicit_castable_reg: tpm_UnaryExpCAST( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "explicit_castable_reg: tpm_UnaryExpCAST( areg )", $1 );

  return( $action[2]() );
};


dreg: cast_result
{
  // Acquire the destination type.
  auto *destType =
    $1->getExp()->getImplicitCastType() ?
      $1->getExp()->getImplicitCastType() : &$1->getExp()->getType();

  // Only apply if the result register is a single register and *not* of pointer
  // type.
  if ( !isPointerType( *destType ) && isRegType( *destType ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: cast_result", $1 );

  // Simply pass through the already casted register.
  return( $action[1]() );
};


ereg: cast_result
{
  // Acquire the destination type.
  auto *destType =
    $1->getExp()->getImplicitCastType() ?
      $1->getExp()->getImplicitCastType() : &$1->getExp()->getType();

  // Only apply if the result register is an extended register and *not* of
  // pointer type.
  if ( !isPointerType( *destType ) && !isRegType( *destType ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: cast_result", $1 );

  // Simply pass through the already casted register.
  return( $action[1]() );
};


areg: cast_result
{
  // Acquire the destination type.
  auto *destType =
    $1->getExp()->getImplicitCastType() ?
      $1->getExp()->getImplicitCastType() : &$1->getExp()->getType();

  // Only apply if the result register *is* of pointer type.
  if ( isPointerType( *destType ) || isArrayType( *destType ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: cast_result", $1 );

  // Simply pass through the already casted register.
  return( $action[1]() );
};
