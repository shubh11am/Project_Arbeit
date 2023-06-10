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


# This file holds all component access rules and assignment rules for composed
# types. The separation between rules in this file and those in
# composed_rules.m4 is a bit arbitrary. Indeed, both files could be merged in
# the future.

# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_dreg: tpm_ComponentAccessExp( composed_type_object, component_offset )
{
  IR_Exp &componentExp = *$3->getExp();

  $cost[0] = $cost[2] + $cost[3];

  if ( isDRegType( componentExp ) ) {
    if ( isCharType( componentExp ) || isShortType( componentExp ) ||
         ( componentExp.getType().getType() == IR_Type::BOOL ) )
      $cost[0] += TC13::OperationFormat::DAC10BOA.getSize();
    else
      $cost[0] += TC13::OperationFormat::DAC16BOA.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "deref_dreg: tpm_ComponentAccessExp( composed_type_object, component_offset )",
    $1 );

  auto compAddr = $action[2]();
  long fieldOffset = $action[3]();

  const int byteOffset = compAddr.getOffset() + fieldOffset;
  IR_Exp &componentExp = *$3->getExp();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  LLIR_Register *regBase = compAddr.getARegister();

  if ( loadResult ) {
    if ( isCharType( componentExp ) ) {
      if ( componentExp.getType().isSignedType() )
        TCINSTRUCTIONS.insertLD_B(
          reg, OPER_BASE, regBase, byteOffset, $1->getExp() );
      else
        TCINSTRUCTIONS.insertLD_BU(
          reg, OPER_BASE, regBase, byteOffset, $1->getExp() );
    } else
    if ( componentExp.getType().getType() == IR_Type::BOOL )
      TCINSTRUCTIONS.insertLD_BU(
        reg, OPER_BASE, regBase, byteOffset, $1->getExp() );
    else
    if ( isShortType( componentExp ) ) {
      if ( componentExp.getType().isSignedType() )
        TCINSTRUCTIONS.insertLD_H(
          reg, OPER_BASE, regBase, byteOffset, $1->getExp() );
      else
        TCINSTRUCTIONS.insertLD_HU(
          reg, OPER_BASE, regBase, byteOffset, $1->getExp() );
    } else
      TCINSTRUCTIONS.insertLD_W(
        reg, OPER_BASE, regBase, byteOffset, $1->getExp() );
  }

  // WIR
  TC_DRegV *r = nullptr;
  auto &bReg = compAddr.getAReg();

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createDReg());

    if ( isCharType( componentExp ) ) {
      if ( componentExp.getType().isSignedType() )
        TCINSTRUCTIONS.insertLD_B( *r, bReg, byteOffset, $1->getExp() );
      else
        TCINSTRUCTIONS.insertLD_BU( *r, bReg, byteOffset, $1->getExp() );
    } else
    if ( componentExp.getType().getType() == IR_Type::BOOL )
      TCINSTRUCTIONS.insertLD_BU( *r, bReg, byteOffset, $1->getExp() );
    else
    if ( isShortType( componentExp ) ) {
      if ( componentExp.getType().isSignedType() )
        TCINSTRUCTIONS.insertLD_H( *r, bReg, byteOffset, $1->getExp() );
      else
        TCINSTRUCTIONS.insertLD_HU( *r, bReg, byteOffset, $1->getExp() );
    } else
      TCINSTRUCTIONS.insertLD_W( *r, bReg, byteOffset, $1->getExp() );
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        regBase, bReg, byteOffset, &$0->getExp()->getType(), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_ereg: tpm_ComponentAccessExp( composed_type_object, component_offset )
{
  if ( isDoubleType( *$3->getExp() ) )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::EAC10BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "deref_ereg: tpm_ComponentAccessExp( composed_type_object, component_offset )",
    $1 );

  auto compAddr = $action[2]();
  long fieldOffset = $action[3]();

  int byteOffset = compAddr.getOffset() + fieldOffset;

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  if ( loadResult )
    TCINSTRUCTIONS.insertLD_D(
      reg, OPER_BASE, compAddr.getARegister(), byteOffset, $1->getExp() );

  // WIR
  TC_ERegV *r = nullptr;
  auto &bReg = compAddr.getAReg();

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createEReg());

    TCINSTRUCTIONS.insertLD_D( *r, bReg, byteOffset, $1->getExp() );
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        compAddr.getARegister(), bReg, byteOffset,
        &$0->getExp()->getType(), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_areg: tpm_ComponentAccessExp( composed_type_object, component_offset )
{
  if ( isPointerType( *$3->getExp() ) )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AAC16BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "deref_areg: tpm_ComponentAccessExp( composed_type_object, component_offset )",
    $1 );

  auto compAddr = $action[2]();
  long fieldOffset = $action[3]();

  const int byteOffset = compAddr.getOffset() + fieldOffset;

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  LLIR_Register *regBase = compAddr.getARegister();

  if ( loadResult )
    TCINSTRUCTIONS.insertLD_A(
      reg, OPER_BASE, regBase, byteOffset, $1->getExp() );

  // WIR
  TC_ARegV *r = nullptr;
  auto &bReg = compAddr.getAReg();

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createAReg());

    TCINSTRUCTIONS.insertLD_A( *r, bReg, byteOffset, $1->getExp() );
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        regBase, bReg, byteOffset, &$0->getExp()->getType(), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_llereg: tpm_ComponentAccessExp( composed_type_object, component_offset )
{
  if ( isLongLongType( *$3->getExp() ) )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_LD_D_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "deref_llereg: tpm_ComponentAccessExp( composed_type_object, component_offset )",
    $1 );

  auto compAddr = $action[2]();
  long fieldOffset = $action[3]();

  const int byteOffset = compAddr.getOffset() + fieldOffset;

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *regBase = compAddr.getARegister();

  if ( loadResult )
    TCINSTRUCTIONS.insertLD_D(
      reg, OPER_BASE, regBase, byteOffset, $1->getExp() );

  // WIR
  TC_ERegV *r = nullptr;
  auto &bReg = compAddr.getAReg();

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createEReg());

    TCINSTRUCTIONS.insertLD_D( *r, bReg, byteOffset, $1->getExp() );
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        regBase, bReg, byteOffset, &$0->getExp()->getType(), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_ComponentAccessExp( composed_type_object, component_offset )
{
  // This rule generates code for accesses to arrays in structs.
  // Arrays are a special case in that sense that no load instruction is
  // generated, but only the array address is computed and returned. In
  // addition, their symbol itself is not modifiable (fixed base address) so
  // that we only generate an 'areg' here, and no 'deref_areg'.

  if ( isArrayType( *$3->getExp() ) )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AAC16BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "areg: tpm_ComponentAccessExp( composed_type_object, component_offset )",
    $1 );

  // Get access location
  auto compAddr = $action[2]();
  long fieldOffset = $action[3]();

  const int byteOffset = compAddr.getOffset() + fieldOffset;

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertLEA(
    reg, OPER_BASE, compAddr.getARegister(), byteOffset, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertLEA( r, compAddr.getAReg(), byteOffset, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
composed_type_object: areg
{
  // This conversion rule takes care of the implicit dereferencing step that is
  // performed when a component access expression is applied to a pointer
  // variable. This way, we can use the existing rules that work on the
  // 'composed_type_object' nonterminal to perform the actual access.
  auto *cexp =
    dynamic_cast<IR_ComponentAccessExp *>( $1->getExp()->getParent() );

  if ( isComposedPointer( *$1->getExp() ) &&
       cexp && ( &cexp->getBaseExp() == $1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "composed_type_object: areg", $1 );

  auto p = $action[1]();

  return( TC_AddressWithOffset { p.first, p.second, 0 } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
composed_type_object: tpm_AssignExpASSIGN( composed_type_object,
                                           composed_type_object )
{
  // This rule handles copy operations through assignments of structs. It does
  // not have the same properties as the assignments in "assignment.m4" (no
  // casting of operands), it therefore must stay here.
  auto *ctype = dynamic_cast<IR_ComposedType *>( &$2->getExp()->getType() );
  ufAssertT ( ctype, "Invalid type for 'composed_type_object' nonterminal!" );

  $cost[0] = $cost[2] + $cost[3] + copyComposedTypeCost( *ctype );

  // TODO: Add a rule
  // composed_type_object: tpm_AssignExpASSIGN( composed_type_object,
  //                                            composed_call_result )
  // that directly copies the call result into the target location instead of
  // copying it to the result buffer first, if it was returned in a register.
}
=
{
  DEBUG_RULE_ACTION(
    "composed_type_object: tpm_AssignExpASSIGN( composed_type_object, composed_type_object )",
    $1 );

  ufAssertT(
    $1->getExp()->getType().isAssignmentCompatible( $2->getExp()->getType() ),
    "Assignment of incompatible composed types is not allowed!" );

  auto op1 = $action[2]();
  auto op2 = $action[3]();

  auto *ctype = dynamic_cast<IR_ComposedType *>( &$2->getExp()->getType() );

  copyComposedType(
    *ctype, op2.getARegister(), op2.getAReg(), op2.getOffset(),
    op1.getARegister(), op1.getAReg(), op1.getOffset(),
    *TCCODESEL->getLastLLIRBB(), TCCODESEL->getCurrentInstruction(),
    *TC179x_wirBB, TC179x_wirBB->end() );

  return( op1 );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
composed_type_object: tpm_ComponentAccessExp( composed_type_object,
                                              component_offset )
{
  // This rules handles accesses to nested structs
  if ( isComposedType( *$3->getExp() ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "composed_type_object: tpm_ComponentAccessExp( composed_type_object, component_offset )",
    $1 );

  auto res = $action[2]();
  long byteOffset = $action[3]();

  return(
    TC_AddressWithOffset {
      res.getARegister(), res.getAReg(), res.getOffset() + byteOffset } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
component_offset: tpm_SymbolExp
{
  // This rule generates the offset from the base pointer for a given composed
  // type field (except for bitfields, they are handled separately).
  if ( isComponentExp( *$1->getExp() ) && !isBitfieldType( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "component_offset: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  IR_ComposedType *compType = getParentComposedType( *symExp );

  // Compute offset.
  return( compType->getOffset( symExp->getSymbol() ) );
};

include(composed_struct_bitfield_rules.m4)
