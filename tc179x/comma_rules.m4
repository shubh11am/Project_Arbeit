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


# In this file, all comma expressions are represented in general by two rules.
# One rule of the form:
#
# result_nonterminal: tpmBinaryExp( first_comma_operand, result_nonterminal )
#
# And a second rule of the form:
#
# first_comma_operand: xy_nonterminal
#
# This way, we can implicitly cover all combinations of nonterminals with a
# minimal set of concise rules.


###############################################################################
#
#
# Rules that match the COMMA operator
#
#
###############################################################################


###############################
#
# Register nonterminals
#
###############################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_BinaryExpCOMMA( first_comma_operand, ereg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "ereg: tpm_BinaryExpCOMMA( first_comma_operand, ereg )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpCOMMA( first_comma_operand, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "dreg: tpm_BinaryExpCOMMA( first_comma_operand, dreg )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_BinaryExpCOMMA( first_comma_operand, areg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "areg: tpm_BinaryExpCOMMA( first_comma_operand, areg )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpCOMMA( first_comma_operand, llereg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "llereg: tpm_BinaryExpCOMMA( first_comma_operand, llereg )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
composed_type_object: tpm_BinaryExpCOMMA( first_comma_operand,
                                          composed_type_object )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "composed_type_object: tpm_BinaryExpCOMMA( first_comma_operand, composed_type_object )",
    $1 );

  $action[2]();
  return( $action[3]() );
};


###############################
#
# Relation nonterminals
#
###############################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpCOMMA( first_comma_operand, rel )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "rel: tpm_BinaryExpCOMMA( first_comma_operand, rel )", $1 );

  $action[2]();
  $action[3]( block, b, false );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpCOMMA( first_comma_operand, nrel )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "nrel: tpm_BinaryExpCOMMA( first_comma_operand, nrel )", $1 );

  $action[2]();
  $action[3]( block, b, false );

  return;
};


###############################
#
# Constant nonterminals
#
###############################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
constAddress: tpm_BinaryExpCOMMA( first_comma_operand, constAddress )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "constAddress: tpm_BinaryExpCOMMA( first_comma_operand, constAddress )",
    $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
addrOffset: tpm_BinaryExpCOMMA( first_comma_operand, addrOffset )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "addrOffset: tpm_BinaryExpCOMMA( first_comma_operand, addrOffset )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const4: tpm_BinaryExpCOMMA( first_comma_operand, const4 )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "const4: tpm_BinaryExpCOMMA( first_comma_operand, const4 )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst4: tpm_BinaryExpCOMMA( first_comma_operand, uconst4 )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "uconst4: tpm_BinaryExpCOMMA( first_comma_operand, uconst4 )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9: tpm_BinaryExpCOMMA( first_comma_operand, const9 )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "const9: tpm_BinaryExpCOMMA( first_comma_operand, const9 )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst9: tpm_BinaryExpCOMMA( first_comma_operand, uconst9 )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "uconst9: tpm_BinaryExpCOMMA( first_comma_operand, uconst9 )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const16: tpm_BinaryExpCOMMA( first_comma_operand, const16 )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "const16: tpm_BinaryExpCOMMA( first_comma_operand, const16 )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant0: tpm_BinaryExpCOMMA( first_comma_operand, constant0 )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "constant0: tpm_BinaryExpCOMMA( first_comma_operand, constant0 )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant8: tpm_BinaryExpCOMMA( first_comma_operand, constant8 )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "constant8: tpm_BinaryExpCOMMA( first_comma_operand, constant8 )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant256: tpm_BinaryExpCOMMA( first_comma_operand, constant256 )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "constant256: tpm_BinaryExpCOMMA( first_comma_operand, constant256 )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
powerOfTwo: tpm_BinaryExpCOMMA( first_comma_operand, powerOfTwo )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "powerOfTwo: tpm_BinaryExpCOMMA( first_comma_operand, powerOfTwo )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
negPowerOfTwo: tpm_BinaryExpCOMMA( first_comma_operand, negPowerOfTwo )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "negPowerOfTwo: tpm_BinaryExpCOMMA( first_comma_operand, negPowerOfTwo )",
    $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9_neg: tpm_BinaryExpCOMMA( first_comma_operand, const9_neg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "const9_neg: tpm_BinaryExpCOMMA( first_comma_operand, const9_neg )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constantf: tpm_BinaryExpCOMMA( first_comma_operand, constantf )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "constantf: tpm_BinaryExpCOMMA( first_comma_operand, constantf )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constantd: tpm_BinaryExpCOMMA( first_comma_operand, constantd )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "constantd: tpm_BinaryExpCOMMA( first_comma_operand, constantd )", $1 );

  $action[2]();
  return( $action[3]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llconst: tpm_BinaryExpCOMMA( first_comma_operand, llconst )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "llconst: tpm_BinaryExpCOMMA( first_comma_operand, llconst )", $1 );

  $action[2]();
  return( $action[3]() );
};


###############################################################################
#
#
# Rules that match the first operand of the comma.
#
#
###############################################################################


###############################
#
# Register nonterminals
#
###############################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: ereg
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: ereg", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: dreg
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: dreg", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: areg
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: areg", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: llereg
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: llereg", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: composed_type_object
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: composed_type_object", $1 );

  $action[1]();

  return;
};


###############################
#
# Constant nonterminals
#
###############################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: constAddress
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: constAddress", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: addrOffset
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: addrOffset", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: const4
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: const4", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: uconst4
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: uconst4", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: const9
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: const9", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: uconst9
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: uconst9", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: const16
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: const16", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: constant0
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: constant0", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: constant8
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: constant8", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: constant256
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: constant256", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: powerOfTwo
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: powerOfTwo", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: negPowerOfTwo
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: negPowerOfTwo", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: const9_neg
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: const9_neg", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: constantf
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: constantf", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: constantd
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: constantd", $1 );

  $action[1]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
first_comma_operand: llconst
{
  $cost[0] = isFirstCommaOperand( *$1->getExp() ) ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "first_comma_operand: llconst", $1 );

  $action[1]();

  return;
};
