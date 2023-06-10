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


###############################################################################
#
#
# Type conversions
#
#
###############################################################################

# The general idea of the casting rules is, that we have two rules for each
# type of register-nonterminal, namely
#
# castable: tpm_ImplicitCast( 'nonterminal' )
# 'nonterminal': castable
#
# where the first rule does nothing (checking the source type is not needed
# anymore) and the second rule checks the target type and does the real
# conversion. The alternativ, to explicitly generate all combinations of
# casting rules directly, without the help of the 'castable' nonterminal, has
# the majors disadvantage that the number of such combinations is equal to
# #nonterminals over 2 (exponential) and therefore it is very likely to end up
# with missing rules or errors in single rules then.
#
# We also have 'implicit_castable_reg' and 'explicit_castable_reg', because when
# using a single 'castable' object, we are no longer able to determine the
# source and target type in the cost part of the "<...> : castable" rules.
#
# Casting rules for the deref_... nonterminals are not required, because cast
# operators do not produce lvalues and as such, a construction like
#
# ((long)my_stack_int) = ...
#
# is illegal.


#############################################################################
#
#
# Handle implicit casts (for registers)
#
#
#############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
implicit_castable_reg: tpm_ImplicitCast( ereg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "implicit_castable_reg: tpm_ImplicitCast( ereg )", $1 );

  auto p = $action[2]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
implicit_castable_reg: tpm_ImplicitCast( dreg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "implicit_castable_reg: tpm_ImplicitCast( dreg )", $1 );

  auto p = $action[2]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
implicit_castable_reg: tpm_ImplicitCast( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "implicit_castable_reg: tpm_ImplicitCast( areg )", $1 );

  auto p = $action[2]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
implicit_castable_reg: tpm_ImplicitCast( llereg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "implicit_castable_reg: tpm_ImplicitCast( llereg )", $1 );

  auto p = $action[2]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: implicit_castable_reg
{
  IR_Type &sourceType = $1->getExp()->getType();
  IR_Type &targetType = *$1->getExp()->getImplicitCastType();

  if ( isDoubleType( targetType ) )
    $cost[0] = $cost[1] + Cast::castingCost( targetType, sourceType );
  else
    $cost[0] = COST_INFINITY;

  if ( ! targetType.getRefCount() )
    delete &targetType;
}
=
{
  DEBUG_RULE_ACTION( "ereg: implicit_castable_reg", $1 );

  IR_Type &sourceType = $1->getExp()->getType();
  IR_Type &targetType = *$1->getExp()->getImplicitCastType();
  auto p = $action[1]();

  // LLIR
  LLIR_Register *reg = Cast::doCasting( targetType, sourceType, p.first );

  // WIR
  auto &r =
    dynamic_cast<TC_ERegV &>(
      Cast::doCasting(
        targetType, sourceType,
        dynamic_cast<WIR_VirtualRegister &>( *(p.second) ), $1->getExp() ) );

  if ( ! targetType.getRefCount() )
    delete &targetType;

  if ( reg )
    return( make_pair( reg, ref( r ) ) );
  else
    return( make_pair( p.first, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: implicit_castable_reg
{
  IR_Type &sourceType = $1->getExp()->getType();
  IR_Type &targetType = *$1->getExp()->getImplicitCastType();

  if ( isDRegType( targetType ) )
    $cost[0] = $cost[1] + Cast::castingCost( targetType, sourceType );
  else
    $cost[0] = COST_INFINITY;

  if ( ! targetType.getRefCount() )
    delete &targetType;
}
=
{
  DEBUG_RULE_ACTION( "dreg: implicit_castable_reg", $1 );

  IR_Type &sourceType = $1->getExp()->getType();
  IR_Type &targetType = *$1->getExp()->getImplicitCastType();
  auto p = $action[1]();

  // LLIR
  LLIR_Register *reg = Cast::doCasting( targetType, sourceType, p.first );

  // WIR
  auto &r =
    dynamic_cast<TC_DRegV &>(
      Cast::doCasting(
        targetType, sourceType,
        dynamic_cast<WIR_VirtualRegister &>( *(p.second) ), $1->getExp() ) );

  if ( ! targetType.getRefCount() )
    delete &targetType;

  if ( reg )
    return( make_pair( reg, ref( r ) ) );
  else
    return( make_pair( p.first, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: implicit_castable_reg
{
  IR_Type &sourceType = $1->getExp()->getType();
  IR_Type &targetType = *$1->getExp()->getImplicitCastType();

  if ( isARegType( targetType ) )
    $cost[0] = $cost[1] + Cast::castingCost( targetType, sourceType );
  else
    $cost[0] = COST_INFINITY;

  if ( ! targetType.getRefCount() )
    delete &targetType;
}
=
{
  DEBUG_RULE_ACTION( "areg: implicit_castable_reg", $1 );

  IR_Type &sourceType = $1->getExp()->getType();
  IR_Type &targetType = *$1->getExp()->getImplicitCastType();
  auto p = $action[1]();

  // LLIR
  LLIR_Register *reg = Cast::doCasting( targetType, sourceType, p.first );

  // WIR
  auto &r =
    dynamic_cast<TC_ARegV &>(
      Cast::doCasting(
        targetType, sourceType,
        dynamic_cast<WIR_VirtualRegister &>( *(p.second) ), $1->getExp() ) );

  if ( ! targetType.getRefCount() )
    delete &targetType;

  if ( reg )
    return( make_pair( reg, ref( r ) ) );
  else
    return( make_pair( p.first, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: implicit_castable_reg
{
  IR_Type &sourceType = $1->getExp()->getType();
  IR_Type &targetType = *$1->getExp()->getImplicitCastType();

  if ( isLongLongType( targetType ) )
    $cost[0] = $cost[1] + Cast::castingCost( targetType, sourceType );
  else
    $cost[0] = COST_INFINITY;

  if ( ! targetType.getRefCount() )
    delete &targetType;
}
=
{
  DEBUG_RULE_ACTION( "llereg: implicit_castable_reg", $1 );

  IR_Type &sourceType = $1->getExp()->getType();
  IR_Type &targetType = *$1->getExp()->getImplicitCastType();
  auto p = $action[1]();

  // LLIR
  LLIR_Register *reg = Cast::doCasting( targetType, sourceType, p.first );

  // WIR
  auto &r =
    dynamic_cast<TC_ERegV &>(
      Cast::doCasting(
        targetType, sourceType,
        dynamic_cast<WIR_VirtualRegister &>( *(p.second) ), $1->getExp() ) );

  if ( ! targetType.getRefCount() )
    delete &targetType;

  if ( reg )
    return( make_pair( reg, ref( r ) ) );
  else
    return( make_pair( p.first, ref( r ) ) );
};


#############################################################################
#
#
# Handle explicit casts (for registers)
#
#
#############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
explicit_castable_reg: tpm_UnaryExpCAST( ereg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "explicit_castable_reg: tpm_UnaryExpCAST( ereg )", $1 );

  auto p = $action[2]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
explicit_castable_reg: tpm_UnaryExpCAST( dreg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "explicit_castable_reg: tpm_UnaryExpCAST( dreg )", $1 );

  auto p = $action[2]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
explicit_castable_reg: tpm_UnaryExpCAST( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "explicit_castable_reg: tpm_UnaryExpCAST( areg )", $1 );

  auto p = $action[2]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
explicit_castable_reg: tpm_UnaryExpCAST( llereg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "explicit_castable_reg: tpm_UnaryExpCAST( llereg )", $1 );

  auto p = $action[2]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: explicit_castable_reg
{
  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  IR_Type &sourceType = uexp->getOp().getType();
  IR_Type &targetType = uexp->getType();

  if ( isDoubleType( targetType ) )
    $cost[0] = $cost[1] + Cast::castingCost( targetType, sourceType );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: explicit_castable_reg", $1 );

  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  IR_Type &sourceType = uexp->getOp().getType();
  IR_Type &targetType = uexp->getType();
  auto p = $action[1]();

  // LLIR
  LLIR_Register *reg = Cast::doCasting( targetType, sourceType, p.first );

  // WIR
  auto &r =
    dynamic_cast<TC_ERegV &>(
      Cast::doCasting(
        targetType, sourceType,
        dynamic_cast<WIR_VirtualRegister &>( *(p.second) ), $1->getExp() ) );

  if ( reg )
    return( make_pair( reg, ref( r ) ) );
  else
    return( make_pair( p.first, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: explicit_castable_reg
{
  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  IR_Type &sourceType = uexp->getOp().getType();
  IR_Type &targetType = uexp->getType();

  if ( isDRegType( targetType ) )
    $cost[0] = $cost[1] + Cast::castingCost( targetType, sourceType );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: explicit_castable_reg", $1 );

  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  IR_Type &sourceType = uexp->getOp().getType();
  IR_Type &targetType = uexp->getType();
  auto p = $action[1]();

  // LLIR
  LLIR_Register *reg = Cast::doCasting( targetType, sourceType, p.first );

  // WIR
  auto &r =
    dynamic_cast<TC_DRegV &>(
      Cast::doCasting(
        targetType, sourceType,
        dynamic_cast<WIR_VirtualRegister &>( *(p.second) ), $1->getExp() ) );

  if ( reg )
    return( make_pair( reg, ref( r ) ) );
  else
    return( make_pair( p.first, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: explicit_castable_reg
{
  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  IR_Type &sourceType = uexp->getOp().getType();
  IR_Type &targetType = uexp->getType();

  if ( isARegType( targetType ) )
    $cost[0] = $cost[1] + Cast::castingCost( targetType, sourceType );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: explicit_castable_reg", $1 );

  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  IR_Type &sourceType = uexp->getOp().getType();
  IR_Type &targetType = uexp->getType();

  auto p = $action[1]();

  // LLIR
  LLIR_Register *reg = Cast::doCasting( targetType, sourceType, p.first );

  // WIR
  auto &r =
    dynamic_cast<TC_ARegV &>(
      Cast::doCasting(
        targetType, sourceType,
        dynamic_cast<WIR_VirtualRegister &>( *(p.second) ), $1->getExp() ) );

  if ( reg )
    return( make_pair( reg, ref( r ) ) );
  else
    return( make_pair( p.first, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: explicit_castable_reg
{
  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  IR_Type &sourceType = uexp->getOp().getType();
  IR_Type &targetType = uexp->getType();

  if ( isLongLongType( targetType ) )
    $cost[0] = $cost[1] + Cast::castingCost( targetType, sourceType );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "llereg: explicit_castable_reg", $1 );

  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp() );
  IR_Type &sourceType = uexp->getOp().getType();
  IR_Type &targetType = uexp->getType();

  auto p = $action[1]();

  // LLIR
  LLIR_Register *reg = Cast::doCasting( targetType, sourceType, p.first );

  // WIR
  auto &r =
    dynamic_cast<TC_ERegV &>(
      Cast::doCasting(
        targetType, sourceType,
        dynamic_cast<WIR_VirtualRegister &>( *(p.second) ), $1->getExp() ) );

  if ( reg )
    return( make_pair( reg, ref( r ) ) );
  else
    return( make_pair( p.first, ref( r ) ) );
};


#############################################################################
#
#
# Handle casts (for constants)
#
#
#############################################################################

#############################################################
#
# Implicit cast initial rules
#
#############################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( constAddress )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( constAddress )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( addrOffset )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( addrOffset )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( const4 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( const4 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( uconst4 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( uconst4 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( const9 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( const9 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( uconst9 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( uconst9 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( const16 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( const16 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( constant0 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( constant0 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( constant8 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( constant8 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( constant256 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( constant256 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( powerOfTwo )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( powerOfTwo )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( negPowerOfTwo )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( negPowerOfTwo )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( const9_neg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( const9_neg )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( constantf )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( constantf )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( constantd )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( constantd )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( llconst )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( llconst )", $1 );
  return;
};


#############################################################
#
# Explicit cast initial rules
#
#############################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( constAddress )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( constAddress )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( addrOffset )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( addrOffset )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( const4 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( const4 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( uconst4 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( uconst4 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( const9 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( const9 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( uconst9 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( uconst9 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( const16 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( const16 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( constant0 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( constant0 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( constant8 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( constant8 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( constant256 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( constant256 )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( powerOfTwo )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( powerOfTwo )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( negPowerOfTwo )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( negPowerOfTwo )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( const9_neg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( const9_neg )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( constantf )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( constantf )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( constantd )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( constantd )", $1 );
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( llconst )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( llconst )", $1 );
  return;
};


#############################################################
#
# Rules to skip nested constant casts
#
#############################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_ImplicitCast( castable_const )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_ImplicitCast( castable_const )", $1 );

  $action[2]();
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
castable_const: tpm_UnaryExpCAST( castable_const )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "castable_const: tpm_UnaryExpCAST( castable_const )", $1 );

  $action[2]();
  return;
};


#############################################################
#
# Rules that take a constant and its stack of casts, apply all
# casts and return the final value in the correct type.
#
#############################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
addrOffset: castable_const
{
  if ( effectiveType( *$1->getExp() ).isIntegralType() ) {
    auto *casted = Cast::constantCastToIntegral( *$1->getExp() );

    if ( isAddrOffset( casted->value, casted->type ) )
      $cost[0] = $cost[1];
    else
      $cost[0] = COST_INFINITY;
    delete( casted );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "addrOffset: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToIntegral( *$1->getExp() );
  IR_Integer res = casted->value;
  delete( casted );

  return( res );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const4: castable_const
{
  if ( effectiveType( *$1->getExp() ).isIntegralType() ) {
    auto *casted = Cast::constantCastToIntegral( *$1->getExp() );

    if ( isConst4( casted->value, casted->type ) )
      $cost[0] = $cost[1];
    else
      $cost[0] = COST_INFINITY;
    delete( casted );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const4: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToIntegral( *$1->getExp() );
  IR_Integer res = casted->value;
  delete( casted );

  return( res );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst4: castable_const
{
  if ( effectiveType( *$1->getExp() ).isIntegralType() ) {
    auto *casted = Cast::constantCastToIntegral( *$1->getExp() );

    if ( isUConst4( casted->value, casted->type ) )
      $cost[0] = $cost[1];
    else
      $cost[0] = COST_INFINITY;
    delete( casted );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "uconst4: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToIntegral( *$1->getExp() );
  IR_Integer res = casted->value;
  delete( casted );

  return( res );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9: castable_const
{
  if ( effectiveType( *$1->getExp() ).isIntegralType() ) {
    auto *casted = Cast::constantCastToIntegral( *$1->getExp() );

    if ( isConst9( casted->value, casted->type ) )
      $cost[0] = $cost[1];
    else
      $cost[0] = COST_INFINITY;
    delete( casted );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const9: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToIntegral( *$1->getExp() );
  IR_Integer res = casted->value;
  delete( casted );

  return( res );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst9: castable_const
{
  if ( effectiveType( *$1->getExp() ).isIntegralType() ) {
    auto *casted = Cast::constantCastToIntegral( *$1->getExp() );

    if ( isUConst9( casted->value, casted->type ) )
      $cost[0] = $cost[1];
    else
      $cost[0] = COST_INFINITY;
    delete( casted );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "uconst9: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToIntegral( *$1->getExp() );
  IR_Integer res = casted->value;
  delete( casted );

  return( res );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const16: castable_const
{
  if ( effectiveType( *$1->getExp() ).isIntegralType() ) {
    auto *casted = Cast::constantCastToIntegral( *$1->getExp() );

    if ( isConst16( casted->value, casted->type ) )
      $cost[0] = $cost[1];
    else
      $cost[0] = COST_INFINITY;
    delete( casted );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const16: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToIntegral( *$1->getExp() );
  IR_Integer res = casted->value;
  delete( casted );

  return( res );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant0: castable_const
{
  if ( effectiveType( *$1->getExp() ).isIntegralType() ) {
    auto *casted = Cast::constantCastToIntegral( *$1->getExp() );

    if ( isConstant0( casted->value, casted->type ) )
      $cost[0] = $cost[1];
    else
      $cost[0] = COST_INFINITY;
    delete( casted );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constant0: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToIntegral( *$1->getExp() );
  IR_Integer res = casted->value;
  delete( casted );

  return( res );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant8: castable_const
{
  if ( effectiveType( *$1->getExp() ).isIntegralType() ) {
    auto *casted = Cast::constantCastToIntegral( *$1->getExp() );

    if ( isConstant8( casted->value, casted->type ) )
      $cost[0] = $cost[1];
    else
      $cost[0] = COST_INFINITY;
    delete( casted );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constant8: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToIntegral( *$1->getExp() );
  IR_Integer res = casted->value;
  delete( casted );

  return( res );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant256: castable_const
{
  if ( effectiveType( *$1->getExp() ).isIntegralType() ) {
    auto *casted = Cast::constantCastToIntegral( *$1->getExp() );

    if ( isConstant256( casted->value, casted->type ) )
      $cost[0] = $cost[1];
    else
      $cost[0] = COST_INFINITY;
    delete( casted );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constant256: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToIntegral( *$1->getExp() );
  IR_Integer res = casted->value;
  delete( casted );

  return( res );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
powerOfTwo: castable_const
{
  if ( effectiveType( *$1->getExp() ).isIntegralType() ) {
    auto *casted = Cast::constantCastToIntegral( *$1->getExp() );

    if ( isPowerOfTwo( casted->value, casted->type ) )
      $cost[0] = $cost[1];
    else
      $cost[0] = COST_INFINITY;
    delete( casted );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "powerOfTwo: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToIntegral( *$1->getExp() );
  int power = isPowerOfTwo( casted->value, casted->type );
  delete( casted );

  return( IR_Integer( power ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
negPowerOfTwo: castable_const
{
  if ( effectiveType( *$1->getExp() ).isIntegralType() ) {
    auto *casted = Cast::constantCastToIntegral( *$1->getExp() );

    if ( isNegativePowerOfTwo( casted->value, casted->type ) )
      $cost[0] = $cost[1];
    else
      $cost[0] = COST_INFINITY;
    delete( casted );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "negPowerOfTwo: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToIntegral( *$1->getExp() );
  int power = isNegativePowerOfTwo( casted->value, casted->type );
  delete( casted );

  return( IR_Integer( power ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9_neg: castable_const
{
  if ( effectiveType( *$1->getExp() ).isIntegralType() ) {
    auto *casted = Cast::constantCastToIntegral( *$1->getExp() );

    if ( isConst9Neg( casted->value, casted->type ) )
      $cost[0] = $cost[1];
    else
      $cost[0] = COST_INFINITY;
    delete( casted );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const9_neg: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToIntegral( *$1->getExp() );
  IR_Integer res = casted->value;
  delete( casted );

  return( res );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constantf: castable_const
{
  if ( isFloatType( effectiveType( *$1->getExp() ) ) ) {
    // Not all floating point values may be statically castable.
    auto *casted = Cast::constantCastToFloat( *$1->getExp() );

    if ( casted ) {
      $cost[0] = $cost[1];
      delete( casted );
    } else
      $cost[0] = COST_INFINITY;
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constantf: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToFloat( *$1->getExp() );
  Float res = *casted;
  delete( casted );

  return( res );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constantd: castable_const
{
  if ( isDoubleType( effectiveType( *$1->getExp() ) ) ) {
    // Not all floating point values may be statically castable.
    auto *casted = Cast::constantCastToDouble( *$1->getExp() );

    if ( casted ) {
      $cost[0] = $cost[1];
      delete( casted );
    } else
      $cost[0] = COST_INFINITY;
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constantd: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToDouble( *$1->getExp() );
  Double res = *casted;
  delete( casted );

  return( res );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llconst: castable_const
{
  if ( isLongLongType( effectiveType( *$1->getExp() ) ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "llconst: castable_const", $1 );

  $action[1]();

  auto *casted = Cast::constantCastToIntegral( *$1->getExp() );
  IR_Integer res = casted->value;
  delete( casted );

  return( res );
};
