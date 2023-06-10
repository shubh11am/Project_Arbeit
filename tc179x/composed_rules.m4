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
stmt: tpm_ExpStmt( composed_type_object )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ExpStmt( composed_type_object )", $1 );

  $action[2]();

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
composed_type_object: tpm_SymbolExp
{
  // This rule handles address generation for local composed symbols.
  auto *exp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  IR_Symbol *sym = &exp->getSymbol();

  if ( isComposedType( *exp ) && !exp->getSymbol().isGlobal() ) {

    map<IR_Symbol *, SymbolInfo> &symmap = *TCCODESEL->getStack()->getSymbolMap();
    auto it = symmap.find( sym );
    const enum SymbolInfo::Type symbolType =
      ( it != symmap.end() ) ?
        (*it).second.getSymbolType() : SymbolInfo::LOCAL_VAR;

    if ( ( it == symmap.end() ) ||
         // Structs may be passed in an areg (base pointer), dreg (if they are
         // smaller than 64 bits) or via the stack (if the registers are full),
         // or it may be just a local (stack) variable.
         ( symbolType == SymbolInfo::LOCAL_STACK_VAR ) ||
         ( symbolType == SymbolInfo::D_ARGUMENT ) ||
         ( symbolType == SymbolInfo::A_ARGUMENT ) ||
         ( symbolType == SymbolInfo::LOCAL_COMPOSED_ARGUMENT ) ) {
      $cost[0] = 0;

      if ( !TCCODESEL->getStack()->getComposedPushCostAdded( sym ) )
        $cost[0] += copyComposedOnStackCost( *sym );
    } else
     $cost[0] = COST_INFINITY;
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "composed_type_object: tpm_SymbolExp [1]", $1 );

  auto *exp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  IR_Symbol *sym = &exp->getSymbol();

  // LLIR
  auto *areg = TCINSTRUCTIONS.CreateRegister( PHREG_SP, true );

  // WIR
  auto &sp = TCINSTRUCTIONS.createAReg();
  TC179x_wirFct->insertPrecolor( sp, TC179x_wirProc->SP() );

  // The Tricore EABI requires us to use a wierd struct argument passing
  // behaviour:
  //
  // sizeof(struct) <= 64 bits : pass as reg/ereg
  // else                      : pass as pointer (address register)
  //
  // In the second case, the callee is responsible for making a copy of the
  // passed struct when the struct is actually accessed inside the callee.
  // This code does that copy operation when the symbol is first accessed.
  if ( sym->getSymbolTable().getFunction() &&
       !TCCODESEL->getStack()->getComposedPushed( sym ) )
    copyComposedOnStack( *sym, *TCCODESEL->getLastLLIRFunction() );

  return(
    TC_AddressWithOffset {
      areg, sp,
      ( sym->getSymbolTable().getFunction() ) ?
        TCCODESEL->getStack()->getComposedParameterBufferOffset( sym ) :
        TCCODESEL->getStack()->getSymbolOffset( sym ) } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
composed_type_object: tpm_SymbolExp
{
  // This rule handles address generation for global composed symbols.
  auto *exp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );

  if ( isComposedType( *exp ) && exp->getSymbol().isGlobal() )
    $cost[0] =
      TC13::OperationFormat::AL_1.getSize() +
      TC13::OperationFormat::AALC16BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "composed_type_object: tpm_SymbolExp [2]", $1 );

  auto *exp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  IR_Symbol &sym = exp->getSymbol();

  // Determine the symbol's label.
  string label =
    ( sym.getType().getStorageClass() == IR_Type::STATIC ) ?
      TCCODESEL->getStaticName( &sym ) : sym.getWrittenName();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );

  TCINSTRUCTIONS.insertMOVH_A( reg, OPER_LAB_HI, label, exp );
  TCINSTRUCTIONS.insertLEA( reg, OPER_BASE, reg, OPER_LAB_LO, label, exp );

  // WIR
  auto &d = TCCODESEL->getWIRData( sym );

  auto &r = TCINSTRUCTIONS.createAReg();

  TCINSTRUCTIONS.insertMOVH_A( r, d, exp );
  TCINSTRUCTIONS.insertLEA( r, r, d, exp );

  return( TC_AddressWithOffset { reg, r, 0 } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpADDR( composed_type_object )
{
  if ( isARegType( *$1->getExp() ) )
    $cost[0] = $cost[2] + TC13::OperationFormat::AAC16BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( composed_type_object )", $1 );

  auto addrOffset = $action[2]();

  // LLIR
  auto *reg =
    &loadAccessLocationToAReg(
      addrOffset.getARegister(), addrOffset.getOffset(), "", $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();

  if ( addrOffset.getOffset() == 0 )
    TCINSTRUCTIONS.insertMOV_AA( r, addrOffset.getAReg(), $1->getExp() );
  else
    TCINSTRUCTIONS.insertLEA(
      r, addrOffset.getAReg(), addrOffset.getOffset(), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_UnaryExpADDR( composed_type_object )
{
  if ( isARegType( *$1->getExp() ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_UnaryExpADDR( composed_type_object )", $1 );

  // Acquire the type of the operand, the base type of the pointer that we are
  // about to construct.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $1->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // Get the operand.
  auto awo = $action[2]();

  return(
    TC_AddressModification {
      awo.getARegister(), awo.getAReg(), awo.getOffset(), &t, true,
      AddressModification::ModTime::NONE, AddressModification::ModOper::ADD } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
composed_type_object: tpm_UnaryExpDEREF( areg )
{
  if ( isComposedType( *$1->getExp() ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "composed_type_object: tpm_UnaryExpDEREF( areg )", $1 );

  auto p = $action[2]();

  return( TC_AddressWithOffset { p.first, p.second, 0 } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9: tpm_UnaryExpSIZEOF( composed_type_object )
{
  int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= TC_Const9_Signed::getMinValue( 9 ) ) &&
       ( byteSize <= TC_Const9_Signed::getMaxValue( 9 ) ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const9: tpm_UnaryExpSIZEOF( composed_type_object )", $1 );

  return( (long) computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst9: tpm_UnaryExpSIZEOF( composed_type_object )
{
  int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= 0 ) &&
       ( byteSize <= (int) TC_Const9_Unsigned::getMaxValue( 9 ) ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "uconst9: tpm_UnaryExpSIZEOF( composed_type_object )", $1 );

  return( (long) computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const16: tpm_UnaryExpSIZEOF( composed_type_object )
{
  int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( byteSize <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] = 10 * $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "const16: tpm_UnaryExpSIZEOF( composed_type_object )", $1 );

  return( (long) computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
composed_call_result: tpm_CallExp( called_function, arg )
{
  if ( isComposedType( *$0->getExp() ) ) {
    $cost[0] = $cost[2] + $cost[3];

    // Add approximate cost for loading the result address if neccessary.
    if ( Stack::getStackSize( &$0->getExp()->getType() ) > 8 )
      $cost[0] += TC13::OperationFormat::SAA_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "composed_call_result: tpm_CallExp( called_function, arg )", $1 );

  TC_AddressWithOffset res;
  auto *theCall = dynamic_cast<IR_CallExp *>( $1->getExp() );
  unsigned int structSize = Stack::getStackSize( &$0->getExp()->getType() );

  // For structs which are bigger than 64 bits, we must specify in address
  // register A4 the address to which the callee shall write the result.
  LLIR_Register *areg4 = nullptr;
  TC_ARegV *a4 = nullptr;

  if ( structSize > 8 ) {

    // We need to generate the contents of A4 here so that the callee can write
    // the result to that location.
    areg4 = TCINSTRUCTIONS.CreateRegister( "", true );
    bindToPHREG( *areg4, 4 );

    a4 = &(TCINSTRUCTIONS.createAReg());
    bindToPHREG( *a4, 4 );

    res = TC_AddressWithOffset { areg4, *a4, 0 };

    // TODO: For assignment expressions that assign the function result to a
    //       variable, we could pass the location of the variable in A4.
    //       Unfortunately, we have no access to the 'TC_AddressWithOffset'
    //       class that is generated by the LHS of the assignment. A clean
    //       solution would be to add a target 'TC_AddressWithOffset' class as a
    //       parameter of 'composed_call_result' that specifies where to store
    //       the result if it must be stored in memory. An assignment rule
    //       'composed_type_object: tpm_AssignExpASSIGN( composed_type_object,
    //                                                   composed_call_result )'
    //       could then handle the situation by forwarding the lhs address
    //       information to the rhs call expression via the new parameter.

    // Let the callee write to the return value buffer that was allocated.
    IR_Function *f = &$1->getExp()->getStmt().getFunction();
    unsigned int off = TCCODESEL->getStack()->getCallResultBufferOffset( f );

    loadAccessLocationToAReg(
      TCINSTRUCTIONS.CreateRegister( PHREG_SP, true ), off, areg4->GetName(),
      $1->getExp() );

    auto &sp = TCINSTRUCTIONS.createAReg();
    TC179x_wirFct->insertPrecolor( sp, TC179x_wirProc->SP() );

    if ( off == 0 )
      TCINSTRUCTIONS.insertMOV_AA( *a4, sp, $1->getExp() );
    else
      TCINSTRUCTIONS.insertLEA( *a4, sp, off, $1->getExp() );
  }

  // For type conversions of complex types, certain library routines might need
  // to be invoked. This cannot be done while the argument vector is mapped into
  // regs or onto stack, since those invocations would overwrite the already
  // assigned registers. Because of this, a two-pass approach is necessary.
  regptrList rhsRegs;
  regptrList lhsRegs;

  argList args;
  argList dryArgs;

  $action[3]( true, 0, 0, theCall, &lhsRegs, &rhsRegs, args, dryArgs );
  $action[3]( false, 0, 0, theCall, &lhsRegs, &rhsRegs, args, dryArgs );

  // In case that the function call's result is returned via A4 (see code
  // regarding areg4 and its comments above), we must make sure that this
  // register A4 is also added as implicit parameter to the function call.
  // Otherwise, the DEF/USE relationships for the CALL instruction are
  // incorrect, potentially leading to an incorrect register allocation.
  if ( a4 ) {
    lhsRegs.push_front( areg4 );
    args.push_front( *a4 );
  }

  if ( structSize <= 4 )
    $action[2]( lhsRegs, args, RegType::DATA_REGISTER );
  else
  if ( structSize <= 8 )
    $action[2]( lhsRegs, args, RegType::EXTENDED_REGISTER );
  else
    $action[2]( lhsRegs, args, RegType::NO_REGISTER );

  return( res );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
composed_type_object: composed_call_result
{
  // This rule converts the result of a function call that returns a struct to a
  // 'composed_type_object' nonterminal. This is neccessary, because the struct
  // may have been passed in a register, and in this case, we must copy it to
  // the stack to obtain a 'composed_type_object' representation.
  $cost[0] = $cost[1];

  int composedTypeSize = Stack::getStackSize( &$0->getExp()->getType() );

  if ( composedTypeSize <= 4 )
    $cost[0] += TC13::OperationFormat::AC16DBOA.getSize();
  else
  if ( composedTypeSize <= 8 )
    $cost[0] += TC13::OperationFormat::AC10EBOA.getSize();
}
=
{
  DEBUG_RULE_ACTION( "composed_type_object: composed_call_result", $1 );

  $action[1]();

  IR_Function *currentFunction = &$1->getExp()->getStmt().getFunction();
  int composedTypeSize = Stack::getStackSize( &$0->getExp()->getType() );
  int off = TCCODESEL->getStack()->getCallResultBufferOffset( currentFunction );

  // LLIR
  LLIR_Register *areg_sp = TCINSTRUCTIONS.CreateRegister( PHREG_SP, true );

  if ( composedTypeSize <= 4 ) {
    // If the composed object was returned in a register, store it on the stack.
    LLIR_Register *d2 = TCINSTRUCTIONS.CreateRegister( PHREG_D2 );
    TCINSTRUCTIONS.insertST_W( OPER_BASE, areg_sp, off, d2, $1->getExp() );
  } else

  if ( composedTypeSize <= 8 ) {
    // If the composed object was returned in a register, store it on the stack.
    LLIR_Register *e2 = TCINSTRUCTIONS.CreateERegister( PHREG_E2 );
    TCINSTRUCTIONS.insertST_D( OPER_BASE, areg_sp, off, e2, $1->getExp() );
  }

  // WIR
  auto &sp = TCINSTRUCTIONS.createAReg();
  TC179x_wirFct->insertPrecolor( sp, TC179x_wirProc->SP() );

  if ( composedTypeSize <= 4 ) {
    // If the composed object was returned in a register, store it on the stack.
    auto &d2 = TCINSTRUCTIONS.createDReg();
    TC179x_wirFct->insertPrecolor( d2, TC179x_wirProc->D2() );
    TCINSTRUCTIONS.insertST_W( sp, off, d2, $1->getExp() );
  } else

  if ( composedTypeSize <= 8 ) {
    // If the composed object was returned in a register, store it on the stack.
    auto &e2 = TCINSTRUCTIONS.createEReg();
    TC179x_wirFct->insertPrecolor( e2, TC179x_wirProc->E2() );
    TCINSTRUCTIONS.insertST_D( sp, off, e2, $1->getExp() );
  }

  return( TC_AddressWithOffset { areg_sp, sp, off } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
arg: tpm_CallExpARG( composed_type_object, arg )
{
  // This rule handles composed type function parameters. Those are not affected
  // by any implicit casting when the call is done. The TriCore EABI dictates
  // that a structure of less than 64 bits is passed in a register or extended
  // register. Bigger structures must be passed via an address register. The
  // callee is then responsible for generating copy code that copies the struct
  // into its local stack area.

  // Functions in prototype form are not supported yet, because in that case,
  // the TriCore ABI forces us to derive a proper function type from the actual
  // parameters, which possibly means from the actual parameters in all places
  // where the function is called. This is not done yet.
  auto *cexp = dynamic_cast<IR_CallExp *>( $2->getExp()->getParent() );

  if ( cexp->getFunctionType().isPrototypeForm() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::AAC16BOA.getSize() +
      TC13::OperationFormat::SAA_1.getSize();
  else {
    throw ufFatalError( "Functions without prototype are not supported." );
    $cost[0] = COST_INFINITY;
  }
}
=
{
  DEBUG_RULE_ACTION( "arg: tpm_CallExpARG( composed_type_object, arg )", $1 );

  const int composedObjSize = Stack::getStackSize( &$2->getExp()->getType() );
  int incr = 0;

  if ( dryrun ) {

    auto compAddr = $action[2]();
    LLIR_Register *reg = nullptr;

    if ( composedObjSize <= 4 ) {
      // Load a data register with the struct.
      reg = TCINSTRUCTIONS.CreateRegister( "" );
      TCINSTRUCTIONS.insertLD_W(
        reg, OPER_BASE, compAddr.getARegister(), compAddr.getOffset(),
        $2->getExp() );

      auto &r = TCINSTRUCTIONS.createDReg();
      TCINSTRUCTIONS.insertLD_W(
        r, compAddr.getAReg(), compAddr.getOffset(), $2->getExp() );
      dryArgs.push_back( r );
    } else

    if ( composedObjSize <= 8 ) {
      // Load an extended register with the struct.
      reg = TCINSTRUCTIONS.CreateERegister( "" );
      TCINSTRUCTIONS.insertLD_D(
        reg, OPER_BASE, compAddr.getARegister(), compAddr.getOffset(),
        $2->getExp() );

      auto &r = TCINSTRUCTIONS.createEReg();
      TCINSTRUCTIONS.insertLD_D(
        r, compAddr.getAReg(), compAddr.getOffset(), $2->getExp() );
      dryArgs.push_back( r );
    } else {
      // Load an address register with the base address of the struct in memory.
      reg =
        &loadAccessLocationToAReg(
          compAddr.getARegister(), compAddr.getOffset(), "", $2->getExp() );

      auto &r = TCINSTRUCTIONS.createAReg();
      if ( compAddr.getOffset() == 0 )
        TCINSTRUCTIONS.insertMOV_AA( r, compAddr.getAReg(), $2->getExp() );
      else
        TCINSTRUCTIONS.insertLEA(
          r, compAddr.getAReg(), compAddr.getOffset(), $2->getExp() );
      dryArgs.push_back( r );
    }

    rhsRegs->push_back( reg );

  } else {

    int regNo =
      Stack::isPassedThroughRegister( theCall->getFunctionType(), index );

    // LLIR
    LLIR_Register *rhsreg = rhsRegs->front();
    rhsRegs->pop_front();

    LLIR_Register *sp = TCINSTRUCTIONS.CreateRegister( PHREG_SP );
    LLIR_Register *lhsreg = nullptr;

    // We might pass the struct in a register, extended register or address
    // register, depending on its size (see TriCore EABI). All three ways work
    // very differently and are distinguished by the following if clause.
    if ( composedObjSize <= 4 ) {
      // RHS is a data register with the struct.

      if ( regNo ) {
        // Pass argument via registers.
        lhsreg = &getFunctionArgumentRegister( *theCall, index, regNo );
        TCINSTRUCTIONS.insertMOV( lhsreg, rhsreg, $1->getExp() );
      } else {
        // Pass argument via stack.
        incr = intBytes;
        TCINSTRUCTIONS.insertST_W( OPER_BASE, sp, offset, rhsreg, $1->getExp() );
        TCCODESEL->getLastLLIRBB()->GetLastIns()->AddPragma(
          new LLIR_Pragma( "Passing overflow function parameter", true ) );
      }
    } else

    if ( composedObjSize <= 8 ) {
      // RHS is an extended register with the struct.

      if ( regNo ) {
        // Pass argument via registers.
        lhsreg = &getFunctionArgumentRegister( *theCall, index, regNo );
        TCINSTRUCTIONS.insertMOV( lhsreg, rhsreg, $1->getExp() );
      } else {
        // Pass argument via stack.
        incr = 2 * intBytes;
        TCINSTRUCTIONS.insertST_D( OPER_BASE, sp, offset, rhsreg, $1->getExp() );
        TCCODESEL->getLastLLIRBB()->GetLastIns()->AddPragma(
          new LLIR_Pragma( "Passing overflow function parameter", true ) );
      }
    } else {
      // RHS is an address register with the base address of the struct in
      // memory.

      if ( regNo ) {
        // Pass argument via registers.
        lhsreg = &getFunctionArgumentRegister( *theCall, index, regNo );
        TCINSTRUCTIONS.insertMOV_AA( lhsreg, rhsreg, $1->getExp() );
      } else {
        // Pass argument via stack.
        incr = pointerBytes;
        TCINSTRUCTIONS.insertST_A( OPER_BASE, sp, offset, rhsreg, $1->getExp() );
        TCCODESEL->getLastLLIRBB()->GetLastIns()->AddPragma(
          new LLIR_Pragma( "Passing overflow function parameter", true ) );
      }
    }

    if ( ( lhsRegs != nullptr ) && ( lhsreg != nullptr ) )
      lhsRegs->push_back( lhsreg );

    // WIR
    auto &r = dryArgs.front().get();
    dryArgs.pop_front();

    // We might pass the struct in a register, extended register or address
    // register, depending on its size (see TriCore EABI). All three ways work
    // very differently and are distinguished by the following if clause.
    if ( composedObjSize <= 4 ) {
      // RHS is a data register with the struct.

      if ( regNo ) {
        // Pass argument via registers.
        auto &phreg =
          dynamic_cast<TC_DRegV &>( getFctArgReg( *theCall, index, regNo ) );
        TCINSTRUCTIONS.insertMOV(
          phreg, dynamic_cast<TC_DRegV &>( r ), $1->getExp() );

        args.push_back( phreg );
      } else {
        // Pass argument via stack.
        incr = intBytes;

        auto &sp = TCINSTRUCTIONS.createAReg();
        TC179x_wirFct->insertPrecolor( sp, TC179x_wirProc->SP() );
        TCINSTRUCTIONS.insertST_W(
          sp, offset, dynamic_cast<TC_DRegV &>( r ), $1->getExp() );

        // Mark access to overflow region for register allocator.
        TC179x_wirBB->rbegin()->get().begin()->get().begin()->
          get().setDontOptimize();
      }
    } else

    if ( composedObjSize <= 8 ) {
      // RHS is an extended register with the struct.

      if ( regNo ) {
        // Pass argument via registers.
        auto &phreg =
          dynamic_cast<TC_ERegV &>( getFctArgReg( *theCall, index, regNo ) );
        TCINSTRUCTIONS.insertMOV(
          phreg, dynamic_cast<TC_ERegV &>( r ), $1->getExp() );

        args.push_back( phreg );
      } else {
        // Pass argument via stack.
        incr = 2 * intBytes;

        auto &sp = TCINSTRUCTIONS.createAReg();
        TC179x_wirFct->insertPrecolor( sp, TC179x_wirProc->SP() );
        TCINSTRUCTIONS.insertST_D(
          sp, offset, dynamic_cast<TC_ERegV &>( r ), $1->getExp() );

        // Mark access to overflow region for register allocator.
        TC179x_wirBB->rbegin()->get().begin()->get().begin()->
          get().setDontOptimize();
      }
    } else {
      // RHS is an address register with the base address of the struct in
      // memory.

      if ( regNo ) {
        // Pass argument via registers.
        auto &phreg =
          dynamic_cast<TC_ARegV &>( getFctArgReg( *theCall, index, regNo ) );
        TCINSTRUCTIONS.insertMOV_AA(
          phreg, dynamic_cast<TC_ARegV &>( r ), $1->getExp() );

        args.push_back( phreg );
      } else {
        // Pass argument via stack.
        incr = pointerBytes;

        auto &sp = TCINSTRUCTIONS.createAReg();
        TC179x_wirFct->insertPrecolor( sp, TC179x_wirProc->SP() );
        TCINSTRUCTIONS.insertST_A(
          sp, offset, dynamic_cast<TC_ARegV &>( r ), $1->getExp() );

        // Mark access to overflow region for register allocator.
        TC179x_wirBB->rbegin()->get().begin()->get().begin()->
          get().setDontOptimize();
      }
    }
  }

  $action[3](
    dryrun, index + 1, offset + incr, theCall, lhsRegs, rhsRegs, args,
    dryArgs );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ReturnStmt( composed_type_object )
{
  $cost[0] = $cost[2] + TC13::OperationFormat::S.getSize();

  int composedTypeSize = Stack::getStackSize( &$2->getExp()->getType() );
  if ( composedTypeSize <= 4 )
    $cost[0] += TC13::OperationFormat::DAC16BOA.getSize();
  else

  if ( composedTypeSize <= 8 )
    $cost[0] += TC13::OperationFormat::EAC10BOA.getSize();
  else
    $cost[0] +=
      TC13::OperationFormat::SAA_1.getSize() +
      copyComposedTypeCost(
        dynamic_cast<IR_ComposedType &>( $2->getExp()->getType() ) );
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ReturnStmt( composed_type_object )", $1 );

  auto compAddr = $action[2]();

  int composedTypeSize = Stack::getStackSize( &$2->getExp()->getType() );

  // Load the result into the result register or store it at the given target
  // location if the result type is too big.
  if ( composedTypeSize <= 4 ) {
    // LLIR
    // Load the result into D2.
    LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
    bindToPHREG( *reg, 2 );
    TCINSTRUCTIONS.insertLD_W(
      reg, OPER_BASE, compAddr.getARegister(), compAddr.getOffset(),
      $1->getExp() );
    TCINSTRUCTIONS.insertRET(
      reg, $1->getExp(), InstructionFactory::RETURN_STMT );

    // WIR
    // Load the result into D2.
    auto &d2 = TCINSTRUCTIONS.createDReg();
    bindToPHREG( d2, 2 );
    TCINSTRUCTIONS.insertLD_W(
      d2, compAddr.getAReg(), compAddr.getOffset(), $1->getExp() );
    TCINSTRUCTIONS.insertRET(
      d2, $1->getExp(), InstructionFactory::RETURN_STMT );

    return;
  } else

  if ( composedTypeSize <= 8 ) {
    // LLIR
    // Load the result into E2.
    LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
    bindToPHREG( *reg, 2 );
    TCINSTRUCTIONS.insertLD_D(
      reg, OPER_BASE, compAddr.getARegister(), compAddr.getOffset(),
      $1->getExp() );
    TCINSTRUCTIONS.insertRET(
      reg, $1->getExp(), InstructionFactory::RETURN_STMT );

    // WIR
    // Load the result into E2.
    auto &e2 = TCINSTRUCTIONS.createEReg();
    bindToPHREG( e2, 2 );
    TCINSTRUCTIONS.insertLD_D(
      e2, compAddr.getAReg(), compAddr.getOffset(), $1->getExp() );
    TCINSTRUCTIONS.insertRET(
      e2, $1->getExp(), InstructionFactory::RETURN_STMT );

    return;
  } else {
    // LLIR
    // Copy the operand to the result struct.
    LLIR_Register *areg_target = TCINSTRUCTIONS.CreateRegister( "", true );
    LLIR_Register *areg_4 = TCINSTRUCTIONS.CreateRegister( "", true );
    bindToPHREG( *areg_4, 4 );

    // Backup address of result struct from A4 at the function start.
    LLIR_Instruction *insMov = insMOV_AA( areg_target, areg_4 );
    TCCODESEL->getLastLLIRFunction()->GetFirstBB()->InsertIns( insMov );

    // WIR
    auto &tgt = TCINSTRUCTIONS.createAReg();
    auto &a4 = TCINSTRUCTIONS.createAReg();
    bindToPHREG( a4, 4 );

    // Backup address of result struct from A4 at the function start.
    TC179x_wirFct->begin()->get().pushFrontInstruction(
      { { TC13::OpCode::MOV_AA,
          TCCODESEL->getGenerate16BitOperations() ?
            TC13::OperationFormat::SAA_1 : TC13::OperationFormat::AA,
          new WIR_RegisterParameter( tgt, WIR_Usage::def ),
          new WIR_RegisterParameter( a4, WIR_Usage::use ) } } );

    // Insert copy code (in the current block).
    copyComposedType(
      dynamic_cast<IR_ComposedType&>( $2->getExp()->getType() ),
      compAddr.getARegister(), compAddr.getAReg(), compAddr.getOffset(),
      areg_target, tgt, 0, *TCCODESEL->getLastLLIRBB(),
      TCCODESEL->getCurrentInstruction(), *TC179x_wirBB, TC179x_wirBB->end() );
  }

  // LLIR
  TCINSTRUCTIONS.insertRET( $1->getExp(), InstructionFactory::RETURN_STMT );

  // WIR
  TCINSTRUCTIONS.insertRETURN( $1->getExp(), InstructionFactory::RETURN_STMT );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
composed_type_object: tpm_IndexExp( areg, addrOffset )
{
  if ( isComposedArray( *$2->getExp() ) || isComposedPointer( *$2->getExp() ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "composed_type_object: tpm_IndexExp( areg, addrOffset )", $1 );

  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  auto p = $action[2]();
  int off = $action[3]().getIntValue() * byteSize;

  return( TC_AddressWithOffset { p.first, p.second, off } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
composed_type_object: tpm_IndexExp( areg, dreg )
{
  if ( isComposedArray( *$2->getExp() ) ||
       isComposedPointer( *$2->getExp() ) ) {
    auto byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );
    $cost[0] =
      $cost[2] + $cost[3] + loadRegisterRelativeAddressCost( byteSize );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "composed_type_object: tpm_IndexExp( areg, dreg )", $1 );

  auto byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg =
    loadRegisterRelativeAddress( p1.first, p2.first, byteSize, $1->getExp() );

  // WIR
  auto &r = loadRegRelativeAddr( p1.second, p2.second, byteSize, $1->getExp() );

  return( TC_AddressWithOffset { reg, r, 0 } );
};


include(composed_initlist_rules.m4)
include(composed_struct_rules.m4)
