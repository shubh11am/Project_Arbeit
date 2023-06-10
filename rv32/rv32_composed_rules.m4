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
# initializer list rules for structs
#
#
###############################################################################

###############################################################################
#
#
# struct rules (without casting, just nonterminal conversion)
#
#
###############################################################################

stmt: tpm_ExpStmt( composed_type_object )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ExpStmt( composed_type_object )", $1 );

  // Generate operand trees.
  $action[2]( false );
};

deref_dreg: tpm_ComponentAccessExp( composed_type_object, component_offset )
{
  if ( !isArrayType( *$3->getExp() ) && !isPointerType( *$3->getExp() ) )
    // This is the worst case cost, if the value is requested and not just the
    // address.
    $cost[0] = $cost[2] + $cost[3] + CT( INS_LDR_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "deref_dreg: tpm_ComponentAccessExp( composed_type_object,component_offset )", $1 );

  // Get access location.
  auto compAddr = $action[2]( dryRun );
  long fieldOffset = $action[3]();

  // Compute the final offset.
  auto byteOffset = compAddr.getOffset() + fieldOffset;

  // Do not insert any instructions on a dry run, only return the lvalue with
  // the relevant information.
  if ( dryRun )
    return(
      ARM_LValue {
        nullptr,
        ARM_AddressModification {
          static_cast<LLIR_Register *>( nullptr ), byteOffset,
          &effectiveType( *$0->getExp() ), AddressModification::ModTime::NONE,
          AddressModification::ModOper::ADD, true, true },
        true } );

  // Generate parameters for the access.
  auto *reg0 = ARMINSTRUCTIONS.CreateRegister( "" );
  auto *reg_base = compAddr.getARegister();

  // Now finally do the access if requested.
  if ( loadResult ) {
    IR_Exp &compExp = *$3->getExp();

    // Use different load instructions depending on the type of the variable.
    switch ( compExp.getType().getType() ) {
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        ARMINSTRUCTIONS.insertLDRB(
          OPER_IMMOFF, reg0, reg_base, byteOffset, &compExp );
        break;
      }

      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT: {
        ARMINSTRUCTIONS.insertLDRH(
          OPER_IMMOFF, reg0, reg_base, byteOffset, &compExp );
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::POINTER:
      case IR_Type::FLOAT: {
        ARMINSTRUCTIONS.insertLDR(
          OPER_IMMOFF, reg0, reg_base, byteOffset, &compExp );
        break;
      }

      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE: {
        ufAssertT( 0, "Uncovered case!" );
        break;
      }

      case IR_Type::ARRAY: {
        ARMINSTRUCTIONS.insertLDR(
          OPER_IMMOFF, reg0, reg_base, byteOffset, &compExp );
        break;
      }

      default:
        break;
    }
  }

  // Generate result.
  return(
    ARM_LValue {
      loadResult ? reg0 : nullptr,
      ARM_AddressModification {
        reg_base, byteOffset, &effectiveType( *$0->getExp() ),
        AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
        true } } );
};
