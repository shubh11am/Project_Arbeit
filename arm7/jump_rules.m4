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
#  Copyright 2005 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


# jump statements (goto, continue, break)

stmt: tpm_JumpStmt
{
  // tpm_JumpStmt combines goto, break and continue. All three cases will lead
  // to an unconditional jump.
  $cost[0] = CT( INS_B_32 );
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_JumpStmt", $1 );

  // tpm_JumpStmt combines goto, break and continue. Since these jumps are
  // unconditional, jumping to the implicit successor of the current basic block
  // suffices.

  stringstream ss;
  ss << $1->getStmt()->getImplicitJumpBasicBlock();
  string block_label = ARMCODESEL->getBlockLabel( ss.str() );

  ARMINSTRUCTIONS.insertB( OPER_AL, block_label );
};


# label statements (:label)

stmt: tpm_LabelStmt
{
  // A label does not generate any code, it's free.
  $cost[0] = 0;
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_LabelStmt", $1 );

  // Nothing to do.
};


# switch statement (switch(...) {...})

stmt: tpm_SwitchStmt( dreg )
{
  // First acquire the IR_SwitchStmt pointer of this switch-expression.
  auto *switchStmt = dynamic_cast<IR_SwitchStmt *>( $1->getStmt() );

  // A switch implementation with a non-constant select expression has three
  // cost sources:

  // A - evaluating the select expression itself.
  $cost[0] = $cost[2];

  // B - one TEQ and one conditional B for every case. Additionally, large
  //     constants may have to be loaded into a register first.
  for ( auto &c : switchStmt->getCases() ) {
    // Guaranteed TEQ and B instruction.
    $cost[0] += CT( INS_TEQ_32 ) + CT( INS_B_32 );

    // If the comparison operand couldn't be encoded as an immediate operand,
    // add the cost of the MOV_ORR instructions on top.
    int compVal = c.second->getValue().getIntValue();
    if( !isValidARMImmediate( compVal ) )
      $cost[0] += ARMINSTRUCTIONS.insertMOV_ORR_Cost( compVal );
  }

  // C - a single B for the default / matching-failure case.
  $cost[0] += CT( INS_B_32 );
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_SwitchStmt( dreg )", $1 );

  // Acquire the IR_SwitchStmt pointer of this switch-expression.
  auto *switchStmt = dynamic_cast<IR_SwitchStmt *>( $1->getStmt() );

  // Also acquire the register.
  auto *selectReg = $action[2]();

  // First, run through all its "case"s.
  for ( auto &c : switchStmt->getCases() ) {
    // Get the case value.
    int value = c.second->getValue().getIntValue();

    // Check whether the value can be encoded as an immedite operand.
    if ( isValidARMImmediate( value ) )
      // If so, only insert the TEQ instruction to check for equality with the
      // current case value.
      ARMINSTRUCTIONS.insertTEQ(
        selectReg, value, $1->getExp(), InstructionFactory::SWITCH_STMT );
    else {
      // If not, the value has to first be loaded.
      auto *valueReg = ARMINSTRUCTIONS.CreateRegister( "" );
      ARMINSTRUCTIONS.insertMOV_ORR(
        valueReg, value, $1->getExp(), InstructionFactory::SWITCH_STMT );

      // And then tested for equality.
      ARMINSTRUCTIONS.insertTEQ(
        selectReg, valueReg, $1->getExp(), InstructionFactory::SWITCH_STMT );
    }

    // Next, get the label for the case-statement's basic block.
    stringstream labelStream;
    labelStream << c.second->getBasicBlock();
    string label = ARMCODESEL->getBlockLabel( labelStream.str() );

    // Finally, insert a conditional jump to that case-label.
    ARMINSTRUCTIONS.insertB(
      OPER_EQ, label, $1->getExp(), InstructionFactory::SWITCH_STMT );

    // A conditional branch was just inserted, this concluded the current
    // LLIR-BasicBlock.
    ARMCODESEL->beginNewLLIRBasicBlock( *$1->getStmt()->getBasicBlock() );
  }

  // None of the cases matched. Checking for the defaut label.
  if (switchStmt->getDefault() != nullptr ) {
    // Create a label for the default case and branch there.
    stringstream labelStream;
    labelStream << switchStmt->getDefault()->getBasicBlock();
    string label = ARMCODESEL->getBlockLabel( labelStream.str() );

    // And branch to that default-label unconditionally.
    ARMINSTRUCTIONS.insertB(
      OPER_AL, label, $1->getExp(), InstructionFactory::SWITCH_STMT );
  } else {
    // No default-label found. In this case, branch out of the whole
    // switch-stmt.
    stringstream labelStream;
    labelStream << switchStmt->getContinueBasicBlock();
    string label = ARMCODESEL->getBlockLabel( labelStream.str() );

    // Branch unconditionally.
    ARMINSTRUCTIONS.insertB(
      OPER_AL, label, $1->getExp(), InstructionFactory::SWITCH_STMT );
  }
};


# switch with constant select expression

stmt: tpm_SwitchStmt( tpm_IntConstExp )
{
  // Only a single branch is inserted.
  $cost[0] = CT( INS_B_32 );
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_SwitchStmt( tpm_IntConstExp )", $1 );

  // Acquire the IR_SwitchStmt pointer of this switch-expression.
  auto *switchStmt = dynamic_cast<IR_SwitchStmt *>( $1->getStmt() );

  // Also acquire the constant expression.
  auto &selectInteger =
    dynamic_cast<const IR_IntConstExp &>( *$2->getExp() ).getValue();

  // A switch statement with a constant select expression essentially boils
  // down to a goto, potentially with a lot of unreachable code as well.

  // First, attempt to find the appropriate case.
  auto *matchingCase = switchStmt->getCase( selectInteger );

  if ( matchingCase ) {
    // A matching case was found. Jump there.

    // First, generate the label.
    stringstream labelStream;
    labelStream << matchingCase->getBasicBlock();
    string label = ARMCODESEL->getBlockLabel( labelStream.str() );

    // Then, jump there.
    ARMINSTRUCTIONS.insertB( OPER_AL, label );
  } else

  if ( switchStmt->getDefault() != nullptr ) {
    // Otherwise, try the default statement.

    // First, generate the label.
    stringstream labelStream;
    labelStream << switchStmt->getDefault()->getBasicBlock();
    string label = ARMCODESEL->getBlockLabel( labelStream.str() );

    // Then, jump there.
    ARMINSTRUCTIONS.insertB( OPER_AL, label );
  } else {
    // There is no default-label. Jump outside.

    // First, generate the label.
    stringstream labelStream;
    labelStream << switchStmt->getContinueBasicBlock();
    string label = ARMCODESEL->getBlockLabel( labelStream.str() );

    // Then, jump there.
    ARMINSTRUCTIONS.insertB( OPER_AL, label );
  }
};


stmt: tpm_CaseStmt
{
  // A case statement is effectively a label, free.
  $cost[0] = 0;
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_CaseStmt", $1 );

  // Nothing to do.
};


stmt: tpm_DefaultStmt
{
  // A default statement is effectively a label, free.
  $cost[0] = 0;
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_DefaultStmt", $1 );

  // Nothing to do.
};
