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


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_CondExp( abs_compare, dreg, dreg )
{
  const AbsType absType = isAbsCondExp( *$1->getExp() );
  if ( absType != AbsType::ABS_NONE )
    $cost[0] = $cost[2] + TC13::OperationFormat::DD.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_CondExp( abs_compare, dreg, dreg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertABS( reg, p.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertABS(
    r, dynamic_cast<TC_DRegV &>( *(p.second) ), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
abs_compare: tpm_BinaryExpLT( dreg, constant0 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "abs_compare: tpm_BinaryExpLT( dreg, constant0 )", $1 );

  auto p = $action[2]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
abs_compare: tpm_BinaryExpLT( constant0, dreg )
{
  $cost[0] = $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "abs_compare: tpm_BinaryExpLT( constant0, dreg )", $1 );

  auto p = $action[3]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
abs_compare: tpm_BinaryExpGT( dreg, constant0 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "abs_compare: tpm_BinaryExpGT( dreg, constant0 )", $1 );

  auto p = $action[2]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
abs_compare: tpm_BinaryExpGT( constant0, dreg )
{
  $cost[0] = $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "abs_compare: tpm_BinaryExpGT( constant0, dreg )", $1 );

  auto p = $action[3]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
abs_compare: tpm_BinaryExpLEQ( dreg, constant0 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "abs_compare: tpm_BinaryExpLEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
abs_compare: tpm_BinaryExpLEQ( constant0, dreg )
{
  $cost[0] = $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "abs_compare: tpm_BinaryExpLEQ( constant0, dreg )", $1 );

  auto p = $action[3]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
abs_compare: tpm_BinaryExpGEQ( dreg, constant0 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "abs_compare: tpm_BinaryExpGEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();
  return( make_pair( p.first, &(p.second.get()) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
abs_compare: tpm_BinaryExpGEQ( constant0, dreg )
{
  $cost[0] = $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "abs_compare: tpm_BinaryExpGEQ( constant0, dreg )", $1 );

  auto p = $action[3]();
  return( make_pair( p.first, &(p.second.get()) ) );
};
