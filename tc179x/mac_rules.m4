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


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpPLUS( dreg, reg_const )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() &&
       checkSignednessForMADD( *$1->getExp() ) ) {
    if ( $0->getExp()->getType().isSignedType() )
      $cost[0] += TC13::OperationFormat::DDDC9_1.getSize();
    else
      $cost[0] += TC13::OperationFormat::DDDC9_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpPLUS( dreg, reg_const )", $1 );

  auto p = $action[2]();
  auto multPair = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( $0->getExp()->getType().isSignedType() )
    TCINSTRUCTIONS.insertMADD(
      reg, p.first, multPair.reg0, multPair.constant, $1->getExp() );
  else
    // Apparently, MADD.U/MSUB.U are not supported by the TriCore assembler, the
    // tricore-gcc itself generates the signed version in any case, too.
    // Therefore, we also generate the signed version here, though the unsigned
    // one would be required according to the instruction set documentation.
    TCINSTRUCTIONS.insertMADD(
      reg, p.first, multPair.reg0, multPair.constant, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  if ( $0->getExp()->getType().isSignedType() )
    TCINSTRUCTIONS.insertMADD(
      r, p.second, multPair.r, multPair.constant, $1->getExp() );
  else
    // Apparently, MADD.U/MSUB.U do not support operation format DDDC9_1 as
    // MADD/MSUB do. Instead, they only feature format EEDC9_2. The tricore-gcc
    // itself generates the signed version in any case, too. Therefore, we also
    // generate the signed version here, though the unsigned one would be
    // required according to the instruction set documentation.
    TCINSTRUCTIONS.insertMADD(
      r, p.second, multPair.r, multPair.constant, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpPLUS( dreg, reg_pair )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() &&
       checkSignednessForMADD( *$1->getExp() ) ) {
    if ( $0->getExp()->getType().isSignedType() )
      $cost[0] += TC13::OperationFormat::DDDD.getSize();
    else
      $cost[0] += TC13::OperationFormat::DDDD.getSize();
  } else

  if ( ( $2->getExp()->getType().getType() == IR_Type::FLOAT ) &&
       ( $3->getExp()->getType().getType() == IR_Type::FLOAT ) ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() &&
         TCCODESEL->getConfig()->getEmitFpuMacInstructions() )
      $cost[0] += TC13::OperationFormat::DDDD.getSize();
    else
      // MADD.F is not supported by soft-float libraries.
      $cost[0] = COST_INFINITY;
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpPLUS( dreg, reg_pair )", $1 );

  auto p = $action[2]();
  auto multPair = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( $0->getExp()->getType().isIntegralType() ) {
    if ( $0->getExp()->getType().isSignedType() )
      TCINSTRUCTIONS.insertMADD(
        reg, p.first, multPair.reg0, multPair.reg1, $1->getExp() );
    else
      // Apparently MADD.U/MSUB.U are not supported by the TriCore assembler,
      // the tricore-gcc itself generates the signed version in any case, too.
      // Therefore, we also generate the signed version here, though the
      // unsigned one would be required, according to the instruction set
      // documentation.
      TCINSTRUCTIONS.insertMADD(
        reg, p.first, multPair.reg0, multPair.reg1, $1->getExp() );
  } else

  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    TCINSTRUCTIONS.insertMADD_F(
      reg, p.first, multPair.reg0, multPair.reg1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  if ( $0->getExp()->getType().isIntegralType() ) {
    if ( $0->getExp()->getType().isSignedType() )
      TCINSTRUCTIONS.insertMADD(
        r, p.second, multPair.r1, multPair.r2, $1->getExp() );
    else
      // Apparently, MADD.U/MSUB.U do not support operation format DDDC9_1 as
      // MADD/MSUB do. Instead, they only feature format EEDC9_2. The
      // tricore-gcc itself generates the signed version in any case, too.
      // Therefore, we also generate the signed version here, though the
      // unsigned one would be required according to the instruction set
      // documentation.
      TCINSTRUCTIONS.insertMADD(
        r, p.second, multPair.r1, multPair.r2, $1->getExp() );
  } else

  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    TCINSTRUCTIONS.insertMADD_F(
      r, p.second, multPair.r1, multPair.r2, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpMINUS( dreg, reg_const )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() &&
       checkSignednessForMADD( *$1->getExp() ) ) {
    if ( $0->getExp()->getType().isSignedType() )
      $cost[0] += TC13::OperationFormat::DDDC9_1.getSize();
    else
      $cost[0] += TC13::OperationFormat::DDDC9_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMINUS( dreg, reg_const )", $1 );

  auto p = $action[2]();
  auto multPair = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( $0->getExp()->getType().isSignedType() )
    TCINSTRUCTIONS.insertMSUB(
      reg, p.first, multPair.reg0, multPair.constant, $1->getExp() );
  else
    // Apparently, MADD.U/MSUB.U are not supported by the TriCore assembler, the
    // tricore-gcc itself generates the signed version in any case, too.
    // Therefore, we also generate the signed version here, though the unsigned
    // one would be required according to the instruction set documentation.
    TCINSTRUCTIONS.insertMSUB(
      reg, p.first, multPair.reg0, multPair.constant, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  if ( $0->getExp()->getType().isSignedType() )
    TCINSTRUCTIONS.insertMSUB(
      r, p.second, multPair.r, multPair.constant, $1->getExp() );
  else
    // Apparently, MADD.U/MSUB.U do not support operation format DDDC9_1 as
    // MADD/MSUB do. Instead, they only feature format EEDC9_2. The tricore-gcc
    // itself generates the signed version in any case, too. Therefore, we also
    // generate the signed version here, though the unsigned one would be
    // required according to the instruction set documentation.
    TCINSTRUCTIONS.insertMSUB(
      r, p.second, multPair.r, multPair.constant, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpMINUS( dreg, reg_pair )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() &&
       checkSignednessForMADD( *$1->getExp() ) ) {
    if ( $0->getExp()->getType().isSignedType() )
      $cost[0] += TC13::OperationFormat::DDDD.getSize();
    else
      $cost[0] += TC13::OperationFormat::DDDD.getSize();
  } else

  if ( ( $2->getExp()->getType().getType() == IR_Type::FLOAT ) &&
       ( $3->getExp()->getType().getType() == IR_Type::FLOAT ) ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() &&
         TCCODESEL->getConfig()->getEmitFpuMacInstructions() )
      $cost[0] += TC13::OperationFormat::DDDD.getSize();
    else
      // MSUB.F is not supported by soft-float libraries.
      $cost[0] = COST_INFINITY;
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMINUS( dreg, reg_pair )", $1 );

  auto p = $action[2]();
  auto multPair = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( $0->getExp()->getType().isIntegralType() ) {
    if ( $0->getExp()->getType().isSignedType() )
      TCINSTRUCTIONS.insertMSUB(
        reg, p.first, multPair.reg0, multPair.reg1, $1->getExp() );
    else
      // Apparently, MADD.U/MSUB.U are not supported by the TriCore assembler,
      // the tricore-gcc itself generates the signed version in any case, too.
      // Therefore, we also generate the signed version here, though the
      // unsigned one would be required, according to the instruction set
      // documentation.
      TCINSTRUCTIONS.insertMSUB(
        reg, p.first, multPair.reg0, multPair.reg1, $1->getExp() );
  } else

  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    TCINSTRUCTIONS.insertMSUB_F(
      reg, p.first, multPair.reg0, multPair.reg1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  if ( $0->getExp()->getType().isIntegralType() ) {
    if ( $0->getExp()->getType().isSignedType() )
      TCINSTRUCTIONS.insertMSUB(
        r, p.second, multPair.r1, multPair.r2, $1->getExp() );
    else
      // Apparently, MADD.U/MSUB.U do not support operation format DDDC9_1 as
      // MADD/MSUB do. Instead, they only feature format EEDC9_2. The
      // tricore-gcc itself generates the signed version in any case, too.
      // Therefore, we also generate the signed version here, though the
      // unsigned one would be required according to the instruction set
      // documentation.
      TCINSTRUCTIONS.insertMSUB(
        r, p.second, multPair.r1, multPair.r2, $1->getExp() );
  } else

  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    TCINSTRUCTIONS.insertMSUB_F(
      r, p.second, multPair.r1, multPair.r2, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
reg_const: tpm_BinaryExpMULT( dreg, const9 )
{
  // This rule does nothing but just returns its operands to generate an MADD.
  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3];
  else
    // No support for float constants.
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "reg_const: tpm_BinaryExpMULT( dreg, const9 )", $1 );

  auto p = $action[2]();
  unsigned long v = $action[3]().getIntValue();

  return( RegConst( p.first, p.second, v ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
reg_pair: tpm_BinaryExpMULT( dreg, dreg )
{
  // This rule does nothing but just returns its operands to generate an MSUB.
  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3];
  else

  if ( ( $2->getExp()->getType().getType() == IR_Type::FLOAT ) &&
       ( $3->getExp()->getType().getType() == IR_Type::FLOAT ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "reg_pair: tpm_BinaryExpMULT( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  return( RegPair( p1.first, p2.first, p1.second, p2.second ) );
};
