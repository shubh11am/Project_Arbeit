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
#  Copyright 2010 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


##############################################################################
#
# Main rules to generate the MIN/MAX instructions
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_CondExp( minmax_reg_compare, dreg, dreg )
{
  switch ( isMinMaxCondExp( *$1->getExp() ) ) {
    case MinMaxType::MINMAX_MIN_UNSIGNED:
    case MinMaxType::MINMAX_MIN_SIGNED:
    case MinMaxType::MINMAX_MAX_UNSIGNED:
    case MinMaxType::MINMAX_MAX_SIGNED: {
      $cost[0] = $cost[2] + TC13::OperationFormat::DDD_1.getSize();
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
  DEBUG_RULE_ACTION(
    "dreg: tpm_CondExp( minmax_reg_compare, dreg, dreg )", $1 );

  auto minMaxType = isMinMaxCondExp( *$1->getExp() );
  auto comp = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  switch ( minMaxType ) {
    case MinMaxType::MINMAX_MIN_UNSIGNED: {
      TCINSTRUCTIONS.insertMIN_U( reg, comp.op1, comp.op2, $1->getExp() );
      break;
    }

    case MinMaxType::MINMAX_MIN_SIGNED: {
      TCINSTRUCTIONS.insertMIN( reg, comp.op1, comp.op2, $1->getExp() );
      break;
    }

    case MinMaxType::MINMAX_MAX_UNSIGNED: {
      TCINSTRUCTIONS.insertMAX_U( reg, comp.op1, comp.op2, $1->getExp() );
      break;
    }

    case MinMaxType::MINMAX_MAX_SIGNED: {
      TCINSTRUCTIONS.insertMAX( reg, comp.op1, comp.op2, $1->getExp() );
      break;
    }

    default:
      break;
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  switch ( minMaxType ) {
    case MinMaxType::MINMAX_MIN_UNSIGNED: {
      TCINSTRUCTIONS.insertMIN_U( r, comp.r1, comp.r2, $1->getExp() );
      break;
    }

    case MinMaxType::MINMAX_MIN_SIGNED: {
      TCINSTRUCTIONS.insertMIN( r, comp.r1, comp.r2, $1->getExp() );
      break;
    }

    case MinMaxType::MINMAX_MAX_UNSIGNED: {
      TCINSTRUCTIONS.insertMAX_U( r, comp.r1, comp.r2, $1->getExp() );
      break;
    }

    case MinMaxType::MINMAX_MAX_SIGNED: {
      TCINSTRUCTIONS.insertMAX( r, comp.r1, comp.r2, $1->getExp() );
      break;
    }

    default:
      break;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_CondExp( minmax_const_compare, dreg, dreg )
{
  switch ( isMinMaxCondExp( *$1->getExp() ) ) {
    case MinMaxType::MINMAX_MIN_UNSIGNED:
    case MinMaxType::MINMAX_MAX_UNSIGNED: {
      $cost[0] = $cost[2] + TC13::OperationFormat::DDC9_2.getSize();
      break;
    }

    case MinMaxType::MINMAX_MIN_SIGNED:
    case MinMaxType::MINMAX_MAX_SIGNED: {
      $cost[0] = $cost[2] + TC13::OperationFormat::DDC9_1.getSize();
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
  DEBUG_RULE_ACTION(
    "dreg: tpm_CondExp( minmax_const_compare, dreg, dreg )", $1 );

  auto minMaxType = isMinMaxCondExp( *$1->getExp() );
  auto comp = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  switch ( minMaxType ) {
    case MinMaxType::MINMAX_MIN_UNSIGNED: {
      TCINSTRUCTIONS.insertMIN_U(
        reg, comp.op1, comp.v.getIntValue(), $1->getExp() );
      break;
    }

    case MinMaxType::MINMAX_MIN_SIGNED: {
      TCINSTRUCTIONS.insertMIN(
        reg, comp.op1, comp.v.getIntValue(), $1->getExp() );
      break;
    }

    case MinMaxType::MINMAX_MAX_UNSIGNED: {
      TCINSTRUCTIONS.insertMAX_U(
        reg, comp.op1, comp.v.getIntValue(), $1->getExp() );
      break;
    }

    case MinMaxType::MINMAX_MAX_SIGNED: {
      TCINSTRUCTIONS.insertMAX(
        reg, comp.op1, comp.v.getIntValue(), $1->getExp() );
      break;
    }

    default:
      break;
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  switch ( minMaxType ) {
    case MinMaxType::MINMAX_MIN_UNSIGNED: {
      TCINSTRUCTIONS.insertMIN_U(
        r, comp.r, comp.v.getIntValue(), $1->getExp() );
      break;
    }

    case MinMaxType::MINMAX_MIN_SIGNED: {
      TCINSTRUCTIONS.insertMIN(
        r, comp.r, comp.v.getIntValue(), $1->getExp() );
      break;
    }

    case MinMaxType::MINMAX_MAX_UNSIGNED: {
      TCINSTRUCTIONS.insertMAX_U(
        r, comp.r, comp.v.getIntValue(), $1->getExp() );
      break;
    }

    case MinMaxType::MINMAX_MAX_SIGNED: {
      TCINSTRUCTIONS.insertMAX(
        r, comp.r, comp.v.getIntValue(), $1->getExp() );
      break;
    }

    default:
      break;
  }

  return( make_pair( reg, ref( r ) ) );
};


##############################################################################
#
# Register comparisons
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
minmax_reg_compare: tpm_BinaryExpLT( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "minmax_reg_compare: tpm_BinaryExpLT( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  return(
    MinMaxRegisterComparison( p1.first, p2.first, p1.second, p2.second ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
minmax_reg_compare: tpm_BinaryExpGT( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "minmax_reg_compare: tpm_BinaryExpGT( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  return(
    MinMaxRegisterComparison( p1.first, p2.first, p1.second, p2.second ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
minmax_reg_compare: tpm_BinaryExpLEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "minmax_reg_compare: tpm_BinaryExpLEQ( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  return(
    MinMaxRegisterComparison( p1.first, p2.first, p1.second, p2.second ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
minmax_reg_compare: tpm_BinaryExpGEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "minmax_reg_compare: tpm_BinaryExpGEQ( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  return(
    MinMaxRegisterComparison( p1.first, p2.first, p1.second, p2.second ) );
};


##############################################################################
#
# Constant comparisons with constant at right side.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
minmax_const_compare: tpm_BinaryExpLT( dreg, const9 )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "minmax_const_compare: tpm_BinaryExpLT( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]();

  return( MinMaxConstantComparison { p.first, p.second, v } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
minmax_const_compare: tpm_BinaryExpGT( dreg, const9 )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "minmax_const_compare: tpm_BinaryExpGT( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]();

  return( MinMaxConstantComparison { p.first, p.second, v } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
minmax_const_compare: tpm_BinaryExpLEQ( dreg, const9 )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "minmax_const_compare: tpm_BinaryExpLEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]();

  return( MinMaxConstantComparison { p.first, p.second, v } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
minmax_const_compare: tpm_BinaryExpGEQ( dreg, const9 )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "minmax_const_compare: tpm_BinaryExpGEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]();

  return( MinMaxConstantComparison { p.first, p.second, v } );
};


##############################################################################
#
# Constant comparisons with constant at left side.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
minmax_const_compare: tpm_BinaryExpLT( const9, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "minmax_const_compare: tpm_BinaryExpLT( const9, dreg )", $1 );

  auto v = $action[2]();
  auto p = $action[3]();

  return( MinMaxConstantComparison { p.first, p.second, v } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
minmax_const_compare: tpm_BinaryExpGT( const9, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "minmax_const_compare: tpm_BinaryExpGT( const9, dreg )", $1 );

  auto v = $action[2]();
  auto p = $action[3]();

  return( MinMaxConstantComparison { p.first, p.second, v } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
minmax_const_compare: tpm_BinaryExpLEQ( const9, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "minmax_const_compare: tpm_BinaryExpLEQ( const9, dreg )", $1 );

  auto v = $action[2]();
  auto p = $action[3]();

  return( MinMaxConstantComparison { p.first, p.second, v } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
minmax_const_compare: tpm_BinaryExpGEQ( const9, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "minmax_const_compare: tpm_BinaryExpGEQ( const9, dreg )", $1 );

  auto v = $action[2]();
  auto p = $action[3]();

  return( MinMaxConstantComparison { p.first, p.second, v } );
};
