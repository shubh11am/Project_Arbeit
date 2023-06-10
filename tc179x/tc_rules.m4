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


###############################################################################
#
#
# Statement rules
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_AsmStmt
{
  // These are not the true costs in terms of the complexity of the generated
  // code, but this does not matter, since this is the only rule that handles
  // inline assembly at all.
  $cost[0] = 1;
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_AsmStmt", $1 );

  auto *asmStmt = dynamic_cast<IR_AsmStmt *>( $1->getStmt() );
  const vector<IR_AsmOperand *> &operandExpressions = asmStmt->getOperands();

  DOUT( "Processing " << operandExpressions.size() << " operands." << endl );
  vector<Tricap::Argument *> arguments;
  vector<TC_LValue> lvalues;
  vector<unique_ptr<TC_AsmArgument>> args;

  unsigned int i = 0;
  for ( auto *op : operandExpressions ) {
    auto p =
      ( i < (unsigned int) asmStmt->getInputOperandsIndex() ) ?
        generateOutputOperand( op, lvalues ) :
        generateInputOperand( op, arguments, args );

    arguments.push_back( p.first );
    args.push_back( unique_ptr<TC_AsmArgument>( p.second ) );

    ++i;
  }

  // LLIR
  TCINSTRUCTIONS.insertInlineAsm( *asmStmt, arguments );

  // WIR
  TCINSTRUCTIONS.insertInlineAsm( *asmStmt, args );

  for ( auto &deref : lvalues )
    deref.storeBack();

  for ( auto it = arguments.begin(); it != arguments.end(); ++it )
    delete( *it );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_LabelStmt
{
  $cost[0] = 0;
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_LabelStmt", $1 );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ExpStmt( dreg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ExpStmt( dreg )", $1 );

  $action[2]();
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ExpStmt( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ExpStmt( areg )", $1 );

  $action[2]();
  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ExpStmt( ereg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ExpStmt( ereg )", $1 );

  $action[2]();
  return;
};


###############################################################################
#
#
# Integer constants of various bit widths, string constants and floats
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
addrOffset: tpm_IntConstExp
{
  if ( isAddrOffset( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "addrOffset: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp*>( $1->getExp() )->getValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const4: tpm_IntConstExp
{
  if ( isConst4( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const4: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp*>( $1->getExp() )->getValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const4: tpm_SymbolExp
{
  // This rule handles enum symbol constants.
  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  if ( sym.getEnumType() &&
       isConst4( sym.getEnumType()->getValue( &sym ), sym.getType() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const4: tpm_SymbolExp", $1 );

  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();
  return( sym.getEnumType()->getValue( &sym ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst4: tpm_IntConstExp
{
  if ( isUConst4( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "uconst4: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp*>( $1->getExp() )->getValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst4: tpm_SymbolExp
{
  // This rule handles enum symbol constants.
  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  if ( sym.getEnumType() &&
       isUConst4( sym.getEnumType()->getValue( &sym ), sym.getType() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "uconst4: tpm_SymbolExp", $1 );

  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();
  return( sym.getEnumType()->getValue( &sym ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9: tpm_IntConstExp
{
  if ( isConst9( *$1->getExp() ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const9: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp*>( $1->getExp() )->getValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9: tpm_SymbolExp
{
  // This rule handles enum symbol constants.
  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  if ( sym.getEnumType() &&
       isConst9( sym.getEnumType()->getValue( &sym ), sym.getType() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const9: tpm_SymbolExp", $1 );

  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();
  return( sym.getEnumType()->getValue( &sym ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst9: tpm_IntConstExp
{
  if ( isUConst9( *$1->getExp() ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "uconst9: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp*>( $1->getExp() )->getValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst9: tpm_SymbolExp
{
  // This rule handles enum symbol constants.
  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  if ( sym.getEnumType() &&
       isUConst9( sym.getEnumType()->getValue( &sym ), sym.getType() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "uconst9: tpm_SymbolExp", $1 );

  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();
  return( sym.getEnumType()->getValue( &sym ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const16: tpm_IntConstExp
{
  if ( isConst16( *$1->getExp() ) )
    $cost[0] = 1000;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const16: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp*>( $1->getExp() )->getValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const16: tpm_SymbolExp
{
  // This rule handles enum symbol constants.
  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  if ( sym.getEnumType() &&
       isConst16( sym.getEnumType()->getValue( &sym ), sym.getType() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const16: tpm_SymbolExp", $1 );

  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();
  return( sym.getEnumType()->getValue( &sym ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant0: tpm_IntConstExp
{
  if ( isConstant0( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constant0: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp*>( $1->getExp() )->getValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant0: tpm_SymbolExp
{
  // This rule handles enum symbol constants.
  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  if ( sym.getEnumType() &&
       isConstant0( sym.getEnumType()->getValue( &sym ), sym.getType() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constant0: tpm_SymbolExp", $1 );

  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();
  return( sym.getEnumType()->getValue( &sym ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant8: tpm_IntConstExp
{
  if ( isConstant8( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constant8: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp*>( $1->getExp() )->getValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant8: tpm_SymbolExp
{
  // This rule handles enum symbol constants.
  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  if ( sym.getEnumType() &&
       isConstant8( sym.getEnumType()->getValue( &sym ), sym.getType() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constant8: tpm_SymbolExp", $1 );

  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();
  return( sym.getEnumType()->getValue( &sym ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant256: tpm_IntConstExp
{
  if ( isConstant256( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constant256: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp*>( $1->getExp() )->getValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant256: tpm_SymbolExp
{
  // This rule handles enum symbol constants.
  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  if ( sym.getEnumType() &&
       isConstant256( sym.getEnumType()->getValue( &sym ), sym.getType() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constant256: tpm_SymbolExp", $1 );

  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();
  return( sym.getEnumType()->getValue( &sym ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
powerOfTwo: tpm_IntConstExp
{
  if ( isPowerOfTwo( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "powerOfTwo: tpm_IntConstExp", $1 );

  return( IR_Integer( isPowerOfTwo( *$1->getExp() ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
powerOfTwo: tpm_SymbolExp
{
  // This rule handles enum symbol constants.
  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  if ( sym.getEnumType() &&
       isPowerOfTwo( sym.getEnumType()->getValue( &sym ), sym.getType() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "powerOfTwo: tpm_SymbolExp", $1 );

  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();
  return(
    IR_Integer(
      isPowerOfTwo( sym.getEnumType()->getValue( &sym ), sym.getType() ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9_neg: tpm_IntConstExp
{
  if ( isConst9Neg( *$1->getExp() ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const9_neg: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp*>( $1->getExp() )->getValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9_neg: tpm_SymbolExp
{
  // This rule handles enum symbol constants.
  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  if ( sym.getEnumType() &&
       isConst9Neg( sym.getEnumType()->getValue( &sym ), sym.getType() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const9_neg: tpm_SymbolExp", $1 );

  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();
  return( sym.getEnumType()->getValue( &sym ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constantd: tpm_FloatConstExp
{
  if ( ( $1->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $1->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constantd: tpm_FloatConstExp", $1 );

  IR_Float f = dynamic_cast<IR_FloatConstExp *>( $1->getExp() )->getValue();

  if ( f.getType() == IR_Type::DOUBLE )
    return( f.getDoubleValue() );
  else
    return( f.getLongDoubleValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constantf: tpm_FloatConstExp
{
  if ( $1->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constantf: tpm_FloatConstExp", $1 );

  auto *exp = dynamic_cast<IR_FloatConstExp *>( $1->getExp() );

  return( exp->getValue().getSingleValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_IntConstExp
{
  if ( $1->getExp()->getType().isIntegralType() &&
       ( $1->getExp()->getType().bitSize() <= 32 ) ) {
    long long constValue = getConstIntValue( $1 );
    int bitWidth = getBitWidth( constValue );
    $cost[0] = 10000;

    if ( bitWidth <= 4 )
      $cost[0] += TC13::OperationFormat::SDC4_1.getSize();
    else

    if ( bitWidth <= 16 )
      $cost[0] += TC13::OperationFormat::DC16_1.getSize();
    else

    if ( ( bitWidth == 17 ) && ( constValue >= 0 ) )
      $cost[0] += TC13::OperationFormat::DC16_2.getSize();
    else

    if ( bitWidth >= 17 )
      $cost[0] +=
        TC13::OperationFormat::DC16_2.getSize() +
        TC13::OperationFormat::DDC16_1.getSize();
    else
      $cost[0] = COST_INFINITY;
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_IntConstExp", $1 );

  /*
    Loading of an integer constant may not be achieved in a direct single step
    instruction. If the integer is larger than 16 bits, one has to use MOVH and
    ADDI operations to perform the move.
  */
  const long long constValue =
    dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOVConstant( reg, constValue );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOVConstant( r, constValue, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: const16
{
  $cost[0] = $cost[1] + TC13::OperationFormat::DC16_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: const16", $1 );

  auto v = $action[1]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( reg, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOV( r, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: constantd
{
  $cost[0] =
    $cost[1] +
    2 * ( TC13::OperationFormat::DC16_2.getSize() +
          TC13::OperationFormat::DDC16_1.getSize() );
}
=
{
  DEBUG_RULE_ACTION( "ereg: constantd", $1 );

  auto v = $action[1]();

  typedef Double::Value::Composed Bits;
  Bits bits = v.getValue().getComposed();

  unsigned long low = bits & 0xffffffff;
  unsigned long high = (bits >> 32) & 0xffffffff;

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertMOVH_ADDI( reg->GetFirstChild(), low, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH_ADDI(
    reg->GetNextChild( reg->GetFirstChild() ), high , $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertMOVH_ADDI(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), low, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH_ADDI(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), high , $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: constantf
{
  $cost[0] =
    $cost[1] + TC13::OperationFormat::DC16_2.getSize() +
    TC13::OperationFormat::DDC16_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: constantf", $1 );

  Float f = $action[1]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOVH_ADDI(
    reg, f.getValue().getComposed(), $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOVH_ADDI( r, f.getValue().getComposed(), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_StringConstExp
{
  $cost[0] =
    TC13::OperationFormat::AC16.getSize() +
    TC13::OperationFormat::AALC16BOA.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_StringConstExp", $1 );

  // Determine string constant.
  string str = dynamic_cast<IR_StringConstExp *>( $1->getExp() )->getValue();

  // LLIR
  string theLabel = TCCODESEL->getRODataLabel( str );
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertMOVH_A( reg, OPER_LAB_HI, theLabel, $1->getExp() );
  TCINSTRUCTIONS.insertLEA(
    reg, OPER_BASE, reg, OPER_LAB_LO, theLabel, $1->getExp() );

  // WIR
  auto &data = TCCODESEL->getStringConstData( str );
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertMOVH_A( r, data, $1->getExp() );
  TCINSTRUCTIONS.insertLEA( r, r, data, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constAddress: tpm_IntConstExp
{
  // This is only used to propagate the address value. A subsequent cast
  // operation will do the actual work.
  $cost[0] = 0;
}
=
{
  DEBUG_RULE_ACTION( "constAddress: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_ImplicitCast( constAddress )
{
  // This is not really a casting rule, it just loads constant addresses to an
  // address register. But until this casting operator is met, it is not clear
  // that an integer constant really is a constant address.
  IR_Type *type_tmp = $1->getExp()->getImplicitCastType();
  if ( isARegType( *type_tmp ) )
    $cost[0] =
      TC13::OperationFormat::AC16.getSize() +
      TC13::OperationFormat::AAC16BOA.getSize();
  else
    $cost[0] = COST_INFINITY;

  if ( type_tmp && ! type_tmp->getRefCount() )
    delete type_tmp;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_ImplicitCast( constAddress )", $1 );

  auto adr = $action[2]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertLEA( reg, adr );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertLEA( r, adr, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpCAST( constAddress )
{
  // This is not really a casting rule, it just loads constant addresses to an
  // address register. But until this casting operator is met, it is not clear
  // that an integer constant really is a constant address.
  if ( isARegType( *$1->getExp() ) )
    $cost[0] =
      TC13::OperationFormat::AC16.getSize() +
      TC13::OperationFormat::AAC16BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpCAST( constAddress )", $1 );

  auto adr = $action[2]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertLEA( reg, adr );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertLEA( r, adr, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
# Simple expressions
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_SymbolExp
{
  // Handles register 'dreg' symbols (non-stack, non-global).
  IR_SymbolExp &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( isDRegType( sym ) && !sym.isGlobal() && !sym.getEnumType() &&
       ( TCCODESEL->getStack()->getSymbolOffset( &sym ) < 0 ) )
    $cost[0] = loadRegisterSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );

  // LLIR
  LLIR_Register *reg = loadRegisterSymbol( *symExp );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( loadRegSym( *symExp ) );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_dreg: tpm_SymbolExp
{
  // Handles local stack 'dreg' symbols.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( isDRegType( sym ) && !sym.isGlobal() && !sym.getEnumType() &&
       ( TCCODESEL->getStack()->getSymbolOffset( &sym ) >= 0 ) )
    $cost[0] = loadStackSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_dreg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  return( loadStackSymbol( *symExp, loadResult ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_SymbolExp
{
  // Handles enum symbols.
  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  if ( sym.getEnumType() ) {
    IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();
    auto *enumType = sym.getEnumType();
    const int enumValue = enumType->getValue( &sym ).getIntValue();

    auto *unaryParentExp =
      dynamic_cast<IR_UnaryExp *>( $1->getExp()->getParent() );
    bool unaryParent =
      ( unaryParentExp != nullptr ) &&
      ( unaryParentExp->getOperator() == IR_UnaryExp::MINUS ) &&
      ( enumValue == static_cast<int>( maxUnsignedConst32Value / 2 ) );

    const int bitWidth = getBitWidth( enumValue );

    if ( bitWidth <= 4 )
      $cost[0] = TC13::OperationFormat::SDC4_1.getSize();
    else

    if ( bitWidth <= 16 )
      $cost[0] = TC13::OperationFormat::DC16_1.getSize();
    else

    if ( ( bitWidth == 17 ) && ( enumValue >= 0 ) )
      $cost[0] = TC13::OperationFormat::DC16_2.getSize();
    else

    if ( ( ( bitWidth >= 17 ) && ( bitWidth <= 32 ) ) || unaryParent )
      $cost[0] =
        TC13::OperationFormat::DC16_2.getSize() +
        TC13::OperationFormat::DDC16_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_SymbolExp", $1 );

  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();
  auto *enumType = sym.getEnumType();
  const int enumValue = enumType->getValue( &sym ).getIntValue();

  auto *unaryParentExp =
    dynamic_cast<IR_UnaryExp *>( $1->getExp()->getParent() );
  bool unaryParent =
    ( unaryParentExp != nullptr ) &&
    ( unaryParentExp->getOperator() == IR_UnaryExp::MINUS ) &&
    ( enumValue == static_cast<int>( maxUnsignedConst32Value / 2 ) );

  // Integer values larger than 16 bits cannot be moved into a register using
  // the immediate addressing mode but must be moved in two steps using MOVH and
  // ADDI.
  const int bitWidth = getBitWidth( enumValue );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( bitWidth < 17 )
    TCINSTRUCTIONS.insertMOV( reg, enumValue, $1->getExp() );
  else

  if ( ( bitWidth == 17 ) && ( enumValue >= 0 ) )
    TCINSTRUCTIONS.insertMOV_U( reg, enumValue, $1->getExp() );
  else

  if ( ( ( bitWidth >= 17 ) && ( bitWidth <= 32 ) ) || unaryParent )
    TCINSTRUCTIONS.insertMOVH_ADDI( reg, enumValue, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  if ( bitWidth < 17 )
    TCINSTRUCTIONS.insertMOVConstant( r, enumValue, $1->getExp() );
  else

  if ( ( bitWidth == 17 ) && ( enumValue >= 0 ) )
    TCINSTRUCTIONS.insertMOV_U( r, enumValue, $1->getExp() );
  else

  if ( ( ( bitWidth >= 17 ) && ( bitWidth <= 32 ) ) || unaryParent )
    TCINSTRUCTIONS.insertMOVH_ADDI( r, enumValue, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_dreg: tpm_SymbolExp
{
  // Handles global 'dreg' symbols (except enums).
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( isDRegType( sym ) && sym.isGlobal() && !sym.getEnumType() )
    $cost[0] = loadGlobalSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_dreg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  return( loadGlobalSymbol( *symExp, loadResult ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_SymbolExp
{
  // Handles register 'ereg' symbols (non-stack, non-global).
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( isDoubleType( sym ) && !sym.isGlobal() &&
       ( TCCODESEL->getStack()->getSymbolOffset( &sym ) < 0 ) )
    $cost[0] = loadRegisterSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );

  // LLIR
  LLIR_Register *reg = loadRegisterSymbol( *symExp );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( loadRegSym( *symExp ) );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_ereg: tpm_SymbolExp
{
  // Handles local stack 'ereg' symbols.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( isDoubleType( sym ) && !sym.isGlobal() &&
       ( TCCODESEL->getStack()->getSymbolOffset( &sym ) >= 0 ) )
    $cost[0] = loadStackSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_ereg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  return( loadStackSymbol( *symExp, loadResult ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_ereg: tpm_SymbolExp
{
  // Handles global 'ereg' symbols.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( isDoubleType( sym ) && sym.isGlobal() )
    $cost[0] = loadGlobalSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_ereg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  return( loadGlobalSymbol( *symExp, loadResult ) );
};


###############################################################################
#
#
# Arithmetic Operations
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpPLUS( dreg, const9 )
{
  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else

  if ( $0->getExp()->getType().isRealType() )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpPLUS( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertADD( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertADD( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_BinaryExpPLUS( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + SOFTFLOAT_COST;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpPLUS( ereg, ereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertADD_D( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertADD_D( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpPLUS( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDD_1.getSize();
  else
  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] += FLOAT_COST( TC13::OperationFormat::DDD_1 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpPLUS( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( $0->getExp()->getType().isIntegralType() )
    TCINSTRUCTIONS.insertADD( reg, p1.first, p2.first, $1->getExp() );
  else
  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    TCINSTRUCTIONS.insertADD_F( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( $0->getExp()->getType().isIntegralType() )
    TCINSTRUCTIONS.insertADD( r, p1.second, p2.second, $1->getExp() );
  else
  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    TCINSTRUCTIONS.insertADD_F( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_BinaryExpMINUS( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + SOFTFLOAT_COST;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpMINUS( ereg, ereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertSUB_D( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertSUB_D( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpMINUS( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDD_1.getSize();
  else
  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] += FLOAT_COST( TC13::OperationFormat::DDD_1 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMINUS( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( $0->getExp()->getType().isIntegralType() )
    TCINSTRUCTIONS.insertSUB( reg, p1.first, p2.first, $1->getExp() );
  else
  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    TCINSTRUCTIONS.insertSUB_F( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( $0->getExp()->getType().isIntegralType() )
    TCINSTRUCTIONS.insertSUB( r, p1.second, p2.second, $1->getExp() );
  else
  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    TCINSTRUCTIONS.insertSUB_F( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpMULT( dreg, const9 )
{
  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else

  if ( $0->getExp()->getType().isRealType() )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMULT( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMUL( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMUL( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpMULT( dreg, powerOfTwo )
{
  const long long v = isPowerOfTwo( *$3->getExp() );
  if ( $0->getExp()->getType().isIntegralType() &&
       ( v >= TC_Const9_Signed::getMinValue( 9 ) ) &&
       ( v <= TC_Const9_Signed::getMaxValue( 9 ) ) )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMULT( dreg, powerOfTwo )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertSHA( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertSHA( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpMULT( dreg, negPowerOfTwo )
{
  const long long v = isPowerOfTwo( *$3->getExp() );
  if ( $0->getExp()->getType().isIntegralType() &&
       ( v >= TC_Const9_Signed::getMinValue( 9 ) ) &&
       ( v <= TC_Const9_Signed::getMaxValue( 9 ) ) )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMULT( dreg, negPowerOfTwo )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertSHA( reg, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertRSUB( reg, reg, 0, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertSHA( r, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertRSUB( r, r, 0, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_BinaryExpMULT( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + SOFTFLOAT_COST;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpMULT( ereg, ereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertMUL_D( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertMUL_D( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpMULT( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDD_1.getSize();
  else
  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] += FLOAT_COST( TC13::OperationFormat::DDD_1 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMULT( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( $0->getExp()->getType().isIntegralType() )
    TCINSTRUCTIONS.insertMUL( reg, p1.first, p2.first, $1->getExp() );
  else
  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    TCINSTRUCTIONS.insertMUL_F( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( $0->getExp()->getType().isIntegralType() )
    TCINSTRUCTIONS.insertMUL( r, p1.second, p2.second, $1->getExp() );
  else
  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    TCINSTRUCTIONS.insertMUL_F( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpDIV( dreg, powerOfTwo )
{
  if ( $0->getExp()->getType().isIntegralType() ) {
    const long long v = isPowerOfTwo( *$3->getExp() );
    if ( $2->getExp()->getType().isUnsignedType() &&
         ( -v >= TC_Const9_Signed::getMinValue( 9 ) ) &&
         ( -v <= TC_Const9_Signed::getMaxValue( 9 ) ) )
      $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
    else
    if ( ( -( 32 - v ) >= TC_Const9_Signed::getMinValue( 9 ) ) &&
         ( -( 32 - v ) <= TC_Const9_Signed::getMaxValue( 9 ) ) &&
         ( -v >= TC_Const9_Signed::getMinValue( 9 ) ) &&
         ( -v <= TC_Const9_Signed::getMaxValue( 9 ) ) )
      $cost[0] =
        $cost[2] + $cost[3] + 3 * TC13::OperationFormat::DDC9_1.getSize() +
        TC13::OperationFormat::DDD_1.getSize();
    else
      $cost[0] = COST_INFINITY;
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpDIV( dreg, powerOfTwo )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( $2->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertSH( reg, p.first, -v, $1->getExp() );
  else {
    // Emit code to achieve a normalization of signed variables to 0 if the
    // dividend is larger than the divisor.
    LLIR_Register *regTemp = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertSHA( regTemp, p.first, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( regTemp, regTemp, -( 32 - v ), $1->getExp() );
    TCINSTRUCTIONS.insertADD( reg, p.first, regTemp, $1->getExp() );

    // Generate the operation.
    TCINSTRUCTIONS.insertSHA( reg, reg, -v, $1->getExp() );
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( $2->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertSH( r, p.second, -v, $1->getExp() );
  else {
    // Emit code to achieve a normalization of signed variables to 0 if the
    // dividend is larger than the divisor.
    auto &tmpReg = TCINSTRUCTIONS.createDReg();
    TCINSTRUCTIONS.insertSHA( tmpReg, p.second, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( tmpReg, tmpReg, -( 32 - v ), $1->getExp() );
    TCINSTRUCTIONS.insertADD( r, p.second, tmpReg, $1->getExp() );

    // Generate the operation.
    TCINSTRUCTIONS.insertSHA( r, r, -v, $1->getExp() );
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpDIV( dreg, negPowerOfTwo )
{
  if ( $0->getExp()->getType().isIntegralType() ) {
    $cost[0] = $cost[2] + $cost[3];

    const long long v = isPowerOfTwo( *$3->getExp() );
    if ( $2->getExp()->getType().isUnsignedType() ) {
      if ( $0->getExp()->getType().isUnsignedType() ) {
        long long value = -2;
        for ( int i = 1; i < v; ++i, value *= 2 ) ;

        if ( value >= -32768 )
          $cost[0] += TC13::OperationFormat::DC16_1.getSize();
        else
          $cost[0] += TC13::OperationFormat::DC16_2.getSize();
        $cost[0] += TC13::OperationFormat::DDD_1.getSize();
      } else
      if ( ( v >= TC_Const9_Signed::getMinValue( 9 ) ) &&
           ( v <= TC_Const9_Signed::getMaxValue( 9 ) ) )
        $cost[0] += 2 * TC13::OperationFormat::DDC9_1.getSize();
      else
        $cost[0] = COST_INFINITY;
    } else
    if ( ( -v >= TC_Const9_Signed::getMinValue( 9 ) ) &&
         ( -v <= TC_Const9_Signed::getMaxValue( 9 ) ) &&
         ( -( 32 - v ) >= TC_Const9_Signed::getMinValue( 9 ) ) &&
         ( -( 32 - v ) <= TC_Const9_Signed::getMaxValue( 9 ) ) )
      $cost[0] +=
        4 * TC13::OperationFormat::DDC9_1.getSize() +
        TC13::OperationFormat::DDD_1.getSize();
    else
      $cost[0] = COST_INFINITY;
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpDIV( dreg, negPowerOfTwo )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( $2->getExp()->getType().isUnsignedType() ) {
    if ( $0->getExp()->getType().isUnsignedType() ) {
      long long value = -2;
      for ( int i = 1; i < v; ++i, value *= 2 ) ;

      if ( value >= -32768 )
        TCINSTRUCTIONS.insertMOV( reg, value, $1->getExp() );
      else {
        int low, high;

        low = value & 0x0000FFFF;

        if ( low > maxSignedShortValue )
          low = minSignedShortValue + ( low - maxSignedShortValue ) - 1;

        high = value - low;

        for ( int i = 0; i < 16; ++i )
          high /= 2;

        if ( high < 0 )
          high += maxUnsignedShortValue + 1;

        TCINSTRUCTIONS.insertMOVH( reg, high, $1->getExp() );
      }

      // Generate the operation.
      TCINSTRUCTIONS.insertGE_U( reg, p.first, reg, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertSH( reg, p.first, -v, $1->getExp() );
      TCINSTRUCTIONS.insertRSUB( reg, reg, 0, $1->getExp() );
    }
  } else {
    // Emit code to achieve a normalization of signed variables to 0 if the
    // dividend is larger than the divisor.
    TCINSTRUCTIONS.insertSHA( reg, p.first, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( reg, reg, -( 32 - v ), $1->getExp() );
    TCINSTRUCTIONS.insertADD( reg, p.first, reg, $1->getExp() );

    // Generate the operation.
    TCINSTRUCTIONS.insertSHA( reg, reg, -v, $1->getExp() );
    TCINSTRUCTIONS.insertRSUB( reg, reg, 0, $1->getExp() );
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( $2->getExp()->getType().isUnsignedType() ) {
    if ( $0->getExp()->getType().isUnsignedType() ) {
      long long value = -2;
      for ( int i = 1; i < v; ++i, value *= 2 ) ;

      if ( value >= -32768 )
        TCINSTRUCTIONS.insertMOV( r, value, $1->getExp() );
      else {
        int low, high;

        low = value & 0x0000FFFF;

        if ( low > maxSignedShortValue )
          low = minSignedShortValue + ( low - maxSignedShortValue ) - 1;

        high = value - low;

        for ( int i = 0; i < 16; ++i )
          high /= 2;

        if ( high < 0 )
          high += maxUnsignedShortValue + 1;

        TCINSTRUCTIONS.insertMOVH( r, high, $1->getExp() );
      }

      // Generate the operation.
      TCINSTRUCTIONS.insertGE_U( r, p.second, r, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertSH( r, p.second, -v, $1->getExp() );
      TCINSTRUCTIONS.insertRSUB( r, r, 0, $1->getExp() );
    }
  } else {
    // Emit code to achieve a normalization of signed variables to 0 if the
    // dividend is larger than the divisor.
    TCINSTRUCTIONS.insertSHA( r, p.second, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( r, r, -( 32 - v ), $1->getExp() );
    TCINSTRUCTIONS.insertADD( r, p.second, r, $1->getExp() );

    // Generate the operation.
    TCINSTRUCTIONS.insertSHA( r, r, -v, $1->getExp() );
    TCINSTRUCTIONS.insertRSUB( r, r, 0, $1->getExp() );
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_BinaryExpDIV( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + SOFTFLOAT_COST;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpDIV( ereg, ereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertDIV_D( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertDIV_D( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpDIV( dreg, dreg )
{
  IR_Type &resultType = $0->getExp()->getType();

  $cost[0] = $cost[2] + $cost[3];

  if ( resultType.isIntegralType() ) {

    $cost[0] +=
      TC13::OperationFormat::EDD.getSize() +
      TC13::OperationFormat::SDD_1.getSize();

    switch ( resultType.getType() ) {
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL:
        $cost[0] += TC13::OperationFormat::EED.getSize();
        break;

      case IR_Type::CHAR:
        $cost[0] += 2 * TC13::OperationFormat::EED.getSize();
        break;

      case IR_Type::UNSIGNED_SHORT:
        $cost[0] += 2 * TC13::OperationFormat::EED.getSize();
        break;

      case IR_Type::SHORT:
        $cost[0] += 3 * TC13::OperationFormat::EED.getSize();
        break;

      case IR_Type::UNSIGNED_INT:
      case IR_Type::UNSIGNED_LONG:
        $cost[0] += 4 * TC13::OperationFormat::EED.getSize();
        break;

      case IR_Type::INT:
      case IR_Type::LONG:
        $cost[0] += 5 * TC13::OperationFormat::EED.getSize();
        break;

      default:
        $cost[0] = COST_INFINITY;
        break;
    }

  } else

  if ( resultType.getType() == IR_Type::FLOAT )
    $cost[0] += FLOAT_COST( TC13::OperationFormat::DDD_1 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpDIV( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  IR_Type &resultType = $0->getExp()->getType();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( resultType.isIntegralType() ) {
    bool isUnsigned = resultType.isUnsignedType();

    switch ( resultType.getType() ) {

      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL:
        TCINSTRUCTIONS.insertDIV_B(
          p1.first, p2.first, reg, isUnsigned, $1->getExp() );
        break;

      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT:
        TCINSTRUCTIONS.insertDIV_H(
          p1.first, p2.first, reg, isUnsigned, $1->getExp() );
        break;

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
        TCINSTRUCTIONS.insertDIV_W(
          p1.first, p2.first, reg, isUnsigned, $1->getExp() );
        break;

      default:
        break;

    }
  } else
  if ( resultType.getType() == IR_Type::FLOAT )
    TCINSTRUCTIONS.insertDIV_F( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  if ( resultType.isIntegralType() ) {
    bool isUnsigned = resultType.isUnsignedType();

    switch ( resultType.getType() ) {

      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL:
        TCINSTRUCTIONS.insertDIV_B(
          r, p1.second, p2.second, isUnsigned, $1->getExp() );
        break;

      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT:
        TCINSTRUCTIONS.insertDIV_H(
          r, p1.second, p2.second, isUnsigned, $1->getExp() );
        break;

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
        TCINSTRUCTIONS.insertDIV_W(
          r, p1.second, p2.second, isUnsigned, $1->getExp() );
        break;

      default:
        break;

    }
  } else
  if ( resultType.getType() == IR_Type::FLOAT )
    TCINSTRUCTIONS.insertDIV_F( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpMOD( dreg, powerOfTwo )
{
  if ( $0->getExp()->getType().isIntegralType() ) {
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC4C5C5.getSize() +
      TC13::OperationFormat::DDD_1.getSize();

    const long long v = isPowerOfTwo( *$3->getExp() );
    if ( $2->getExp()->getType().isUnsignedType() &&
         ( v >= 0 ) &&
         ( v <= (long long) TC_Const4_Unsigned::getMaxValue( 4 ) ) )
      $cost[0] += 0;
    else
    if ( ( -( 32 - v ) >= TC_Const9_Signed::getMinValue( 9 ) ) &&
         ( -( 32 - v ) <= TC_Const9_Signed::getMaxValue( 9 ) ) &&
         ( v >= 0 ) &&
         ( v <= (long long) TC_Const4_Unsigned::getMaxValue( 4 ) ) )
      $cost[0] +=
        2 * TC13::OperationFormat::DDC9_1.getSize() +
        TC13::OperationFormat::DDD_1.getSize();
    else
      $cost[0] = COST_INFINITY;
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMOD( dreg, powerOfTwo )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( $2->getExp()->getType().isUnsignedType() )
    // This one computes '( reg / powerOfTwo ) * powerOfTwo'.
    TCINSTRUCTIONS.insertINSERT( reg, p.first, 0, 0, v, $1->getExp() );
  else {
    // Emit code to achieve a normalization of signed variables to 0 if the
    // dividend is larger than the divisor.
    TCINSTRUCTIONS.insertSHA( reg, p.first, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( reg, reg, -( 32 - v ), $1->getExp() );
    TCINSTRUCTIONS.insertADD( reg, p.first, reg, $1->getExp() );

    // This one computes '( reg / powerOfTwo ) * powerOfTwo'.
    TCINSTRUCTIONS.insertINSERT( reg, reg, 0, 0, v, $1->getExp() );
  }
  TCINSTRUCTIONS.insertSUB( reg, p.first, reg, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  if ( $2->getExp()->getType().isUnsignedType() )
    // This one computes '( reg / powerOfTwo ) * powerOfTwo'.
    TCINSTRUCTIONS.insertINSERT( r, p.second, 0, 0, v, $1->getExp() );
  else {
    // Emit code to achieve a normalization of signed variables to 0 if the
    // dividend is larger than the divisor.
    TCINSTRUCTIONS.insertSHA( r, p.second, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( r, r, -( 32 - v ), $1->getExp() );
    TCINSTRUCTIONS.insertADD( r, p.second, r, $1->getExp() );

    // This one computes '( reg / powerOfTwo ) * powerOfTwo'.
    TCINSTRUCTIONS.insertINSERT( r, r, 0, 0, v, $1->getExp() );
  }
  TCINSTRUCTIONS.insertSUB( r, p.second, r, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpMOD( dreg, negPowerOfTwo )
{
  if ( $0->getExp()->getType().isIntegralType() ) {
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize();

    const long long v = isPowerOfTwo( *$3->getExp() );
    if ( $2->getExp()->getType().isUnsignedType() )
      if ( $0->getExp()->getType().isUnsignedType() ) {
        long long value = -2;
        for ( int i = 1; i < v; ++i, value *= 2 ) ;

        if ( value >= -32768 )
          $cost[0] += TC13::OperationFormat::DC16_1.getSize();
        else
          $cost[0] += TC13::OperationFormat::DC16_2.getSize();
        $cost[0] += 2 * TC13::OperationFormat::DDD_1.getSize();
      } else
      if ( ( v >= 0 ) &&
           ( v <= (long long) TC_Const4_Unsigned::getMaxValue( 4 ) ) )
        $cost[0] += TC13::OperationFormat::DDC4C5C5.getSize();
      else
        $cost[0] = COST_INFINITY;
    else
    if ( ( v >= 0 ) &&
         ( v <= (long long) TC_Const4_Unsigned::getMaxValue( 4 ) ) &&
         ( -( 32 - v ) >= TC_Const9_Signed::getMinValue( 9 ) ) &&
         ( -( 32 - v ) <= TC_Const9_Signed::getMaxValue( 9 ) ) )
      $cost[0] +=
        2 * TC13::OperationFormat::DDC9_1.getSize() +
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DDC4C5C5.getSize();
    else
      $cost[0] = COST_INFINITY;
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMOD( dreg, negPowerOfTwo )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( $2->getExp()->getType().isUnsignedType() )
    if ( $0->getExp()->getType().isUnsignedType() ) {
      LLIR_Register *tmpReg = TCINSTRUCTIONS.CreateRegister( "" );

      long long value = -2;
      for ( int i = 1; i < v; ++i, value *= 2 ) ;

      if ( value >= -32768 )
        TCINSTRUCTIONS.insertMOV( tmpReg, value, $1->getExp() );
      else {
        int low, high;

        low = value & 0x0000FFFF;

        if ( low > maxSignedShortValue )
          low = minSignedShortValue + ( low - maxSignedShortValue ) - 1;

        high = value - low;

        for ( int i = 0; i < 16; ++i )
          high /= 2;

        if ( high < 0 )
          high += maxUnsignedShortValue + 1;

        TCINSTRUCTIONS.insertMOVH( tmpReg, high, $1->getExp() );
      }

      // Generate the operation.
      TCINSTRUCTIONS.insertGE_U( reg, p.first, tmpReg, $1->getExp() );
      TCINSTRUCTIONS.insertMUL( reg, reg, tmpReg, $1->getExp() );
    } else
      // This one computes '( reg / powerOfTwo ) * powerOfTwo'.
      TCINSTRUCTIONS.insertINSERT( reg, p.first, 0, 0, v, $1->getExp() );
  else {
    // Emit code to achieve a normalization of signed variables to 0 if the
    // dividend is larger than the divisor.
    TCINSTRUCTIONS.insertSHA( reg, p.first, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( reg, reg, -( 32 - v ), $1->getExp() );
    TCINSTRUCTIONS.insertADD( reg, p.first, reg, $1->getExp() );

    // This one computes '( reg / powerOfTwo ) * powerOfTwo'.
    TCINSTRUCTIONS.insertINSERT( reg, reg, 0, 0, v, $1->getExp() );
  }
  TCINSTRUCTIONS.insertSUB( reg, p.first, reg, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  if ( $2->getExp()->getType().isUnsignedType() )
    if ( $0->getExp()->getType().isUnsignedType() ) {
      auto &tmpReg = TCINSTRUCTIONS.createDReg();

      long long value = -2;
      for ( int i = 1; i < v; ++i, value *= 2 ) ;

      if ( value >= -32768 )
        TCINSTRUCTIONS.insertMOV( tmpReg, value, $1->getExp() );
      else {
        int low, high;

        low = value & 0x0000FFFF;

        if ( low > maxSignedShortValue )
          low = minSignedShortValue + ( low - maxSignedShortValue ) - 1;

        high = value - low;

        for ( int i = 0; i < 16; ++i )
          high /= 2;

        if ( high < 0 )
          high += maxUnsignedShortValue + 1;

        TCINSTRUCTIONS.insertMOVH( tmpReg, high, $1->getExp() );
      }

      // Generate the operation.
      TCINSTRUCTIONS.insertGE_U( r, p.second, tmpReg, $1->getExp() );
      TCINSTRUCTIONS.insertMUL( r, r, tmpReg, $1->getExp() );
    } else
      // This one computes '( reg / powerOfTwo ) * powerOfTwo'.
      TCINSTRUCTIONS.insertINSERT( r, p.second, 0, 0, v, $1->getExp() );
  else {
    // Emit code to achieve a normalization of signed variables to 0 if the
    // dividend is larger than the divisor.
    TCINSTRUCTIONS.insertSHA( r, p.second, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( r, r, -( 32 - v ), $1->getExp() );
    TCINSTRUCTIONS.insertADD( r, p.second, r, $1->getExp() );

    // This one computes '( reg / powerOfTwo ) * powerOfTwo'.
    TCINSTRUCTIONS.insertINSERT( r, r, 0, 0, v, $1->getExp() );
  }
  TCINSTRUCTIONS.insertSUB( r, p.second, r, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpMOD( dreg, dreg )
{
  enum IR_Type::Type t = $0->getExp()->getType().getType();

  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::EDD.getSize() +
    TC13::OperationFormat::EED.getSize() +
    TC13::OperationFormat::SDD_1.getSize();

  if ( ( t == IR_Type::SHORT ) || ( t == IR_Type::UNSIGNED_SHORT ) )
    $cost[0] += TC13::OperationFormat::EED.getSize();
  else
  if ( ( t != IR_Type::CHAR ) && ( t != IR_Type::UNSIGNED_CHAR ) &&
       ( t != IR_Type::BOOL ) )
    $cost[0] += 3 * TC13::OperationFormat::EED.getSize();

  if ( !$0->getExp()->getType().isUnsignedType() )
    $cost[0] += TC13::OperationFormat::EED.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMOD( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  enum IR_Type::Type t = $0->getExp()->getType().getType();
  bool isUnsigned = $0->getExp()->getType().isUnsignedType();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( ( t == IR_Type::CHAR ) || ( t == IR_Type::UNSIGNED_CHAR ) ||
       ( t == IR_Type::BOOL ) )
    TCINSTRUCTIONS.insertMOD_B(
      p1.first, p2.first, reg, isUnsigned, $1->getExp() );
  else
  if ( ( t == IR_Type::SHORT ) || ( t == IR_Type::UNSIGNED_SHORT ) )
    TCINSTRUCTIONS.insertMOD_H(
      p1.first, p2.first, reg, isUnsigned, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOD_W(
      p1.first, p2.first, reg, isUnsigned, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  if ( ( t == IR_Type::CHAR ) || ( t == IR_Type::UNSIGNED_CHAR ) ||
       ( t == IR_Type::BOOL ) )
    TCINSTRUCTIONS.insertMOD_B(
      r, p1.second, p2.second, isUnsigned, $1->getExp() );
  else
  if ( ( t == IR_Type::SHORT ) || ( t == IR_Type::UNSIGNED_SHORT ) )
    TCINSTRUCTIONS.insertMOD_H(
      r, p1.second, p2.second, isUnsigned, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOD_W(
      r, p1.second, p2.second, isUnsigned, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
# Logical AND and OR operators
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGAND( ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * TC13::OperationFormat::SDC4_1.getSize() +
    2 * SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGAND( ereg, ereg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is false, the entire AND expression is treated as false
    and the RHS expression is never evaluated.
  */

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogAND = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGAND );

  // LLIR
  // Generate label for the basic block after the current operation.
  string label0 = LLIR::getUniqueLabel();
  string jmpLabel = ( logAndEndLabel != "" ? logAndEndLabel : label0 );
  if ( isNestedLogAND ) {
    if ( logAndEndLabel == "" )
      logAndEndLabel = label0;
  } else
    logAndEndLabel = "";

  // WIR
  // Create new basic block immediately after this '&&' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logAndEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logAndEndBlock;
  bool newEndBlock = ( logAndEndBlock == nullptr );
  if ( isNestedLogAND ) {
    if ( logAndEndBlock == nullptr )
      logAndEndBlock = &b;
  } else
    logAndEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( regTmp->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOV(
    regTmp->GetNextChild( regTmp->GetFirstChild() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( reg, p1.first, regTmp, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  // Generate MOV and conditional JMP still in the current basic block.
  auto &tmpReg = TCINSTRUCTIONS.createEReg();
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOVConstant( tmpReg, 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( r, p1.second, tmpReg, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertEQ_D( reg, p2.first, regTmp, $1->getExp() );

  // Insert new basic block after the AND operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertEQ_D( r, p2.second, tmpReg, $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logAndEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGAND( areg, areg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize() +
    2 * TC13::OperationFormat::SDA_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGAND( areg, areg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is false, the entire AND expression is treated as false
    and the RHS expression is never evaluated.
  */

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogAND = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGAND );

  // LLIR
  // Generate label for the basic block after the current operation.
  string label0 = LLIR::getUniqueLabel();
  string jmpLabel = ( logAndEndLabel != "" ? logAndEndLabel : label0 );
  if ( isNestedLogAND ) {
    if ( logAndEndLabel == "" )
      logAndEndLabel = label0;
  } else
    logAndEndLabel = "";

  // WIR
  // Create new basic block immediately after this '&&' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logAndEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logAndEndBlock;
  bool newEndBlock = ( logAndEndBlock == nullptr );
  if ( isNestedLogAND ) {
    if ( logAndEndBlock == nullptr )
      logAndEndBlock = &b;
  } else
    logAndEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV_D( reg, p1.first, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOV_D( r, p1.second, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertMOV_D( reg, p2.first, $1->getExp() );

  // Insert new basic block after the AND operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertMOV_D( r, p2.second, $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logAndEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGAND( dreg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGAND( dreg, dreg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is false, the entire AND expression is treated as false
    and the RHS expression is never evaluated.
  */

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogAND = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGAND );

  // LLIR
  // Generate label for the basic block after the current operation.
  string label0 = LLIR::getUniqueLabel();
  string jmpLabel = ( logAndEndLabel != "" ? logAndEndLabel : label0 );
  if ( isNestedLogAND ) {
    if ( logAndEndLabel == "" )
      logAndEndLabel = label0;
  } else
    logAndEndLabel = "";

  // WIR
  // Create new basic block immediately after this '&&' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logAndEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logAndEndBlock;
  bool newEndBlock = ( logAndEndBlock == nullptr );
  if ( isNestedLogAND ) {
    if ( logAndEndBlock == nullptr )
      logAndEndBlock = &b;
  } else
    logAndEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a && b && c && d
  //             so the first match will be
  //             ( a && b && c ) && ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  // In the future this could be replaced by a more involved rule framework that
  // uses special nonterminals to handle the situation. Then also the global
  // variable communication above could be eliminated.
  LLIR_Register *reg = nullptr;
  if ( isNestedLogAND )
    reg = p1.first;
  else {
    reg = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertMOV( reg, p1.first, $1->getExp() );
  }

  TCINSTRUCTIONS.insertJEQ( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  // Generate MOV and conditional JMP still in the current basic block.
  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a && b && c && d
  //             so the first match will be
  //             ( a && b && c ) && ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  auto &r = isNestedLogAND ? p1.second.get() : TCINSTRUCTIONS.createDReg();
  if ( !isNestedLogAND )
    TCINSTRUCTIONS.insertMOV( r, p1.second, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );

  // Insert new basic block after the AND operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logAndEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGAND( areg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::SDA_1.getSize() +
    TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGAND( areg, dreg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is false, the entire AND expression is treated as false
    and the RHS expression is never evaluated.
  */

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogAND = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGAND );

  // LLIR
  // Generate label for the basic block after the current operation.
  string label0 = LLIR::getUniqueLabel();
  string jmpLabel = ( logAndEndLabel != "" ? logAndEndLabel : label0 );
  if ( isNestedLogAND ) {
    if ( logAndEndLabel == "" )
      logAndEndLabel = label0;
  } else
    logAndEndLabel = "";

  // WIR
  // Create new basic block immediately after this '&&' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logAndEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logAndEndBlock;
  bool newEndBlock = ( logAndEndBlock == nullptr );
  if ( isNestedLogAND ) {
    if ( logAndEndBlock == nullptr )
      logAndEndBlock = &b;
  } else
    logAndEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV_D( reg, p1.first, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOV_D( r, p1.second, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );

  // Insert new basic block after the AND operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logAndEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGAND( dreg, areg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::SDA_1.getSize() +
    TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGAND( dreg, areg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is false, the entire AND expression is treated as false
    and the RHS expression is never evaluated.
  */

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogAND = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGAND );

  // LLIR
  // Generate label for the basic block after the current operation.
  string label0 = LLIR::getUniqueLabel();
  string jmpLabel = ( logAndEndLabel != "" ? logAndEndLabel : label0 );
  if ( isNestedLogAND ) {
    if ( logAndEndLabel == "" )
      logAndEndLabel = label0;
  } else
    logAndEndLabel = "";

  // WIR
  // Create new basic block immediately after this '&&' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logAndEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logAndEndBlock;
  bool newEndBlock = ( logAndEndBlock == nullptr );
  if ( isNestedLogAND ) {
    if ( logAndEndBlock == nullptr )
      logAndEndBlock = &b;
  } else
    logAndEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a && b && c && d
  //             so the first match will be
  //             ( a && b && c ) && ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  // In the future this could be replaced by a more involved rule framework that
  // uses special nonterminals to handle the situation. Then also the global
  // variable communication above could be eliminated.
  LLIR_Register *reg = nullptr;
  if ( isNestedLogAND )
    reg = p1.first;
  else {
    reg = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertMOV( reg, p1.first, $1->getExp() );
  }

  TCINSTRUCTIONS.insertJEQ( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  // Generate MOV and conditional JMP still in the current basic block.
  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a && b && c && d
  //             so the first match will be
  //             ( a && b && c ) && ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  auto &r = isNestedLogAND ? p1.second.get() : TCINSTRUCTIONS.createDReg();
  if ( !isNestedLogAND )
    TCINSTRUCTIONS.insertMOV( r, p1.second, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertMOV_D( reg, p2.first, $1->getExp() );

  // Insert new basic block after the AND operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertMOV_D( r, p2.second, $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logAndEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGOR( ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * TC13::OperationFormat::SDC4_1.getSize() +
    2 * SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGOR( ereg, ereg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is true, the entire OR expression is treated as true and
    the RHS expression is never evaluated.
  */

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogOR = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGOR );

  // LLIR
  // Generate label for the basic block after the current operation.
  string label0 = LLIR::getUniqueLabel();
  string jmpLabel = ( logOrEndLabel != "" ? logOrEndLabel : label0 );
  if ( isNestedLogOR ) {
    if ( logOrEndLabel == "" )
      logOrEndLabel = label0;
  } else
    logOrEndLabel = "";

  // WIR
  // Create new basic block immediately after this '||' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logOrEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logOrEndBlock;
  bool newEndBlock = ( logOrEndBlock == nullptr );
  if ( isNestedLogOR ) {
    if ( logOrEndBlock == nullptr )
      logOrEndBlock = &b;
  } else
    logOrEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( regTmp->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOV(
    regTmp->GetNextChild( regTmp->GetFirstChild() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( reg, p1.first, regTmp, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  // Generate MOV and conditional JMP still in the current basic block.
  auto &tmpReg = TCINSTRUCTIONS.createEReg();
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOVConstant( tmpReg, 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( r, p1.second, tmpReg, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertEQ_D( reg, p2.first, regTmp, $1->getExp() );

  // Insert new basic block after the OR operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertEQ_D( r, p2.second, tmpReg, $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logAndEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGOR( areg, areg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize() +
    2 * TC13::OperationFormat::SDA_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGOR( areg, areg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is true, the entire OR expression is treated as true and
    the RHS expression is never evaluated.
  */

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogOR = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGOR );

  // LLIR
  // Generate label for the basic block after the current operation.
  string label0 = LLIR::getUniqueLabel();
  string jmpLabel = ( logOrEndLabel != "" ? logOrEndLabel : label0 );
  if ( isNestedLogOR ) {
    if ( logOrEndLabel == "" )
      logOrEndLabel = label0;
  } else
    logOrEndLabel = "";

  // WIR
  // Create new basic block immediately after this '||' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logOrEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logOrEndBlock;
  bool newEndBlock = ( logOrEndBlock == nullptr );
  if ( isNestedLogOR ) {
    if ( logOrEndBlock == nullptr )
      logOrEndBlock = &b;
  } else
    logOrEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV_D( reg, p1.first, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOV_D( r, p1.second, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertMOV_D( reg, p2.first, $1->getExp() );

  // Insert new basic block after the AND operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertMOV_D( r, p2.second, $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logOrEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGOR( dreg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGOR( dreg, dreg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is true, the entire OR expression is treated as true and
    the RHS expression is never evaluated.
  */

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogOR = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGOR );

  // LLIR
  // Generate label for the basic block after the current operation.
  string label0 = LLIR::getUniqueLabel();
  string jmpLabel = ( logOrEndLabel != "" ? logOrEndLabel : label0 );
  if ( isNestedLogOR ) {
    if ( logOrEndLabel == "" )
      logOrEndLabel = label0;
  } else
    logOrEndLabel = "";

  // WIR
  // Create new basic block immediately after this '||' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logOrEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logOrEndBlock;
  bool newEndBlock = ( logOrEndBlock == nullptr );
  if ( isNestedLogOR ) {
    if ( logOrEndBlock == nullptr )
      logOrEndBlock = &b;
  } else
    logOrEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a || b || c || d
  //             so the first match will be
  //             ( a || b || c ) || ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  // TODO: In the future this could be replaced by a more involved rule
  // framework that uses special nonterminals to handle the situation. Then also
  // the global variable communication above could be eliminated.
  LLIR_Register *reg = nullptr;
  if ( isNestedLogOR )
    reg = p1.first;
  else {
    reg = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertMOV( reg, p1.first, $1->getExp() );
  }

  TCINSTRUCTIONS.insertJNE( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  // Generate MOV and conditional JMP still in the current basic block.
  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a || b || c || d
  //             so the first match will be
  //             ( a || b || c ) || ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  auto &r = isNestedLogOR ? p1.second.get() : TCINSTRUCTIONS.createDReg();
  if ( !isNestedLogOR )
    TCINSTRUCTIONS.insertMOV( r, p1.second, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );

  // Insert new basic block after the OR operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logOrEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGOR( areg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::SDA_1.getSize() +
    TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGOR( areg, dreg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is true, the entire OR expression is treated as true and
    the RHS expression is never evaluated.
  */

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogOR = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGOR );

  // LLIR
  // Generate label for the basic block after the current operation.
  string label0 = LLIR::getUniqueLabel();
  string jmpLabel = ( logOrEndLabel != "" ? logOrEndLabel : label0 );
  if ( isNestedLogOR ) {
    if ( logOrEndLabel == "" )
      logOrEndLabel = label0;
  } else
    logOrEndLabel = "";

  // WIR
  // Create new basic block immediately after this '||' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logOrEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logOrEndBlock;
  bool newEndBlock = ( logOrEndBlock == nullptr );
  if ( isNestedLogOR ) {
    if ( logOrEndBlock == nullptr )
      logOrEndBlock = &b;
  } else
    logOrEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV_D( reg, p1.first, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOV_D( r, p1.second, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );

  // Insert new basic block after the OR operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logOrEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGOR( dreg, areg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGOR( dreg, areg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is true, the entire OR expression is treated as true and
    the RHS expression is never evaluated.
  */

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogOR = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGOR );

  // LLIR
  // Generate label for the basic block after the current operation.
  string label0 = LLIR::getUniqueLabel();
  string jmpLabel = ( logOrEndLabel != "" ? logOrEndLabel : label0 );
  if ( isNestedLogOR ) {
    if ( logOrEndLabel == "" )
      logOrEndLabel = label0;
  } else
    logOrEndLabel = "";

  // WIR
  // Create new basic block immediately after this '||' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logOrEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logOrEndBlock;
  bool newEndBlock = ( logOrEndBlock == nullptr );
  if ( isNestedLogOR ) {
    if ( logOrEndBlock == nullptr )
      logOrEndBlock = &b;
  } else
    logOrEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a || b || c || d
  //             so the first match will be
  //             ( a || b || c ) || ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  // TODO: In the future this could be replaced by a more involved rule
  // framework that uses special nonterminals to handle the situation. Then also
  // the global variable communication above could be eliminated.
  LLIR_Register *reg = nullptr;
  if ( isNestedLogOR )
    reg = p1.first;
  else {
    reg = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertMOV( reg, p1.first, $1->getExp() );
  }

  TCINSTRUCTIONS.insertJNE( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  // Generate MOV and conditional JMP still in the current basic block.
  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a || b || c || d
  //             so the first match will be
  //             ( a || b || c ) || ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  auto &r = isNestedLogOR ? p1.second.get() : TCINSTRUCTIONS.createDReg();
  if ( !isNestedLogOR )
    TCINSTRUCTIONS.insertMOV( r, p1.second, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertMOV_D( reg, p2.first, $1->getExp() );

  // Insert new basic block after the AND operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertMOV_D( r, p2.second, $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logOrEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpLOGNOT( ereg )
{
  $cost[0] =
    $cost[2] + 2 * TC13::OperationFormat::SDC4_1.getSize() + SOFTFLOAT_COST +
    TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpLOGNOT( ereg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( regTmp->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOV(
    regTmp->GetNextChild( regTmp->GetFirstChild() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( reg, p.first, regTmp, $1->getExp() );
  TCINSTRUCTIONS.insertEQ( reg, reg, 0, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createEReg();
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOVConstant( tmpReg, 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( r, p.second, tmpReg, $1->getExp() );
  TCINSTRUCTIONS.insertEQ( r, r, 0, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpLOGNOT( dreg )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + TC13::OperationFormat::DDC9_1.getSize();
  else
  if ( $2->getExp()->getType().getType() == IR_Type::FLOAT ) {
    $cost[0] = TC13::OperationFormat::SDC4_1.getSize();
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DDC5DC5_1.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DDC9_1.getSize();
  } else
    ufAssertT( 0, "Unsupported type" );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpLOGNOT( dreg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( $2->getExp()->getType().isIntegralType() )
    TCINSTRUCTIONS.insertEQ( reg, p.first, 0, $1->getExp() );
  else
  if ( $2->getExp()->getType().getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertMOV( regTmp, 0, $1->getExp() );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p.first, regTmp, $1->getExp() );
      TCINSTRUCTIONS.insertOR_T( reg, regTmp, 1, regTmp, 1, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertEQ_F( regTmp, p.first, regTmp );
      TCINSTRUCTIONS.insertEQ( reg, regTmp, 0, $1->getExp() );
    }
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  if ( $2->getExp()->getType().isIntegralType() )
    TCINSTRUCTIONS.insertEQ( r, p.second, 0, $1->getExp() );
  else
  if ( $2->getExp()->getType().getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();
    TCINSTRUCTIONS.insertMOV( tmpReg, 0, $1->getExp() );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p.second, tmpReg, $1->getExp() );
      TCINSTRUCTIONS.insertOR_T( r, tmpReg, 1, tmpReg, 1, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertEQ_F( tmpReg, p.second, tmpReg, $1->getExp() );
      TCINSTRUCTIONS.insertEQ( r, tmpReg, 0, $1->getExp() );
    }
  }

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
# Bitwise logical operators
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpAND( dreg, uconst9 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpAND( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertAND( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertAND( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpAND( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpAND( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertAND( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertAND( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpAND( dreg, const9_neg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpAND( dreg, const9_neg )", $1 );

  auto p = $action[2]();
  auto difference =
    TC_Const9_Unsigned::getMaxValue( 32 ) - $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertANDN( reg, p.first, difference, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertANDN( r, p.second, difference, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpOR( dreg, uconst9 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpOR( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertOR( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertOR( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpOR( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpOR( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertOR( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertOR( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpOR( dreg, const9_neg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpOR( dreg, const9_neg )", $1 );

  auto p = $action[2]();
  auto difference =
    TC_Const9_Unsigned::getMaxValue( 32 ) - $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertORN( reg, p.first, difference, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertORN( r, p.second, difference, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpXOR( dreg, uconst9 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpXOR( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertXOR( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertXOR( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpXOR( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpXOR( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertXOR( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertXOR( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpXOR( dreg, const9_neg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpXOR( dreg, const9_neg )", $1 );

  auto p = $action[2]();
  auto difference =
    TC_Const9_Unsigned::getMaxValue( 32 ) - $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertXNOR( reg, p.first, difference, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertXNOR( r, p.second, difference, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpBITNOT( dreg )
{
  $cost[0] = $cost[2] + TC13::OperationFormat::DDC9_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpBITNOT( dreg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertNOR( reg, p.first, 0, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNOR( r, p.second, 0, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
# Shift operators
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpSHL( dreg, const9 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpSHL( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    TCINSTRUCTIONS.insertSHA( reg, p.first, v, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    TCINSTRUCTIONS.insertSHA( r, p.second, v, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpSHL( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpSHL( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    TCINSTRUCTIONS.insertSHA( reg, p1.first, p2.first, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    TCINSTRUCTIONS.insertSHA( r, p1.second, p2.second, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpSHR( dreg, const9 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpSHR( dreg, const9 )", $1 );

  auto t = $2->getExp()->getType().getType();
  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    if ( ( t == IR_Type::UNSIGNED_INT ) || ( t == IR_Type::UNSIGNED_LONG ) )
      TCINSTRUCTIONS.insertSH( reg, p.first, -v, $1->getExp() );
    else
      TCINSTRUCTIONS.insertSHA( reg, p.first, -v, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( reg, p.first, -v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    if ( ( t == IR_Type::UNSIGNED_INT ) || ( t == IR_Type::UNSIGNED_LONG ) )
      TCINSTRUCTIONS.insertSH( r, p.second, -v, $1->getExp() );
    else
      TCINSTRUCTIONS.insertSHA( r, p.second, -v, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( r, p.second, -v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpSHR( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpSHR( dreg, dreg )", $1 );

  auto t = $2->getExp()->getType().getType();
  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertRSUB( regTmp, p2.first, 0, $1->getExp() );
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    if ( ( t == IR_Type::UNSIGNED_INT ) || ( t == IR_Type::UNSIGNED_LONG ) )
      TCINSTRUCTIONS.insertSH( reg, p1.first, regTmp, $1->getExp() );
    else
      TCINSTRUCTIONS.insertSHA( reg, p1.first, regTmp, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( reg, p1.first, regTmp, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  auto &tmpReg = TCINSTRUCTIONS.createDReg();

  TCINSTRUCTIONS.insertRSUB( tmpReg, p2.second, 0, $1->getExp() );
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    if ( ( t == IR_Type::UNSIGNED_INT ) || ( t == IR_Type::UNSIGNED_LONG ) )
      TCINSTRUCTIONS.insertSH( r, p1.second, tmpReg, $1->getExp() );
    else
      TCINSTRUCTIONS.insertSHA( r, p1.second, tmpReg, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( r, p1.second, tmpReg, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
# Unary expressions
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant0: tpm_UnaryExpMINUS( constant0 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "constant0: tpm_UnaryExpMINUS( constant0 )", $1 );

  return( -$action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const4: tpm_UnaryExpMINUS( const4 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "const4: tpm_UnaryExpMINUS( const4 )", $1 );

  return( -$action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const4: tpm_UnaryExpMINUS( constant8 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "const4: tpm_UnaryExpMINUS( constant8 )", $1 );

  return( -$action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9: tpm_UnaryExpMINUS( const9 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "const9: tpm_UnaryExpMINUS( const9 )", $1 );

  return( -$action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9: tpm_UnaryExpMINUS( constant256 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "const9: tpm_UnaryExpMINUS( constant256 )", $1 );

  return( -$action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const16: tpm_UnaryExpMINUS( const16 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "const16: tpm_UnaryExpMINUS( const16 )", $1 );

  return( -$action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
negPowerOfTwo: tpm_UnaryExpMINUS( powerOfTwo )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "negPowerOfTwo: tpm_UnaryExpMINUS( powerOfTwo )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
powerOfTwo: tpm_UnaryExpMINUS( negPowerOfTwo )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "powerOfTwo: tpm_UnaryExpMINUS( negPowerOfTwo )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_UnaryExpMINUS( ereg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::SDC4_1.getSize() +
    TC13::OperationFormat::DC16_2.getSize() + SOFTFLOAT_COST;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpMINUS( ereg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertMOV( regTmp->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    regTmp->GetNextChild( regTmp->GetFirstChild() ), 49136, $1->getExp() );
  TCINSTRUCTIONS.insertMUL_D( reg, p.first, regTmp, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createEReg();
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( tmpReg.begin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ), 49136, $1->getExp() );
  TCINSTRUCTIONS.insertMUL_D( r, p.second, tmpReg, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpMINUS( dreg )
{
  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + TC13::OperationFormat::DDC9_1.getSize();
  else
  if (  $0->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] =
      $cost[2] +  TC13::OperationFormat::DC16_2.getSize() +
      TC13::OperationFormat::DDC9_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpMINUS( dreg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( $0->getExp()->getType().isIntegralType() )
    TCINSTRUCTIONS.insertRSUB( reg, p.first, 0, $1->getExp() );
  else
  if (  $0->getExp()->getType().getType() == IR_Type::FLOAT ) {
    LLIR_Register *regMask = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertMOVH( regMask, 1 << 15, $1->getExp() );
    TCINSTRUCTIONS.insertXOR( reg, p.first, regMask, $1->getExp() );
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( $0->getExp()->getType().isIntegralType() )
    TCINSTRUCTIONS.insertRSUB( r, p.second, 0, $1->getExp() );
  else
  if (  $0->getExp()->getType().getType() == IR_Type::FLOAT ) {
    auto &regMask = TCINSTRUCTIONS.createDReg();
    TCINSTRUCTIONS.insertMOVH( regMask, 1 << 15, $1->getExp() );
    TCINSTRUCTIONS.insertXOR( r, p.second, regMask, $1->getExp() );
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constantd: tpm_UnaryExpMINUS( constantd )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "constantd: tpm_UnaryExpMINUS( constantd )", $1 );

  return( -$action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constantf: tpm_UnaryExpMINUS( constantf )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "constantf: tpm_UnaryExpMINUS( constantf )", $1 );

  return( -$action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant0: tpm_UnaryExpPLUS( constant0 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "constant0: tpm_UnaryExpPLUS( constant0 )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const4: tpm_UnaryExpPLUS( const4 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "const4: tpm_UnaryExpPLUS( const4 )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant8: tpm_UnaryExpPLUS( constant8 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "constant8: tpm_UnaryExpPLUS( constant8 )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
powerOfTwo: tpm_UnaryExpPLUS( powerOfTwo )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "powerOfTwo: tpm_UnaryExpPLUS( powerOfTwo )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
negPowerOfTwo: tpm_UnaryExpPLUS( negPowerOfTwo )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "negPowerOfTwo: tpm_UnaryExpPLUS( negPowerOfTwo )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9: tpm_UnaryExpPLUS( const9 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "const9: tpm_UnaryExpPLUS( const9 )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constant256: tpm_UnaryExpPLUS( constant256 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "constant256: tpm_UnaryExpPLUS( constant256 )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpPLUS( dreg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPLUS( dreg )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_UnaryExpPLUS( ereg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPLUS( ereg )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constantf: tpm_UnaryExpPLUS( constantf )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "constantf: tpm_UnaryExpPLUS( constantf )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
constantd: tpm_UnaryExpPLUS( constantd )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "constantd: tpm_UnaryExpPLUS( constantd )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_UnaryExpPOSTDEC( deref_ereg )
{
  $cost[0] =
    $cost[2] + 8 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::SDC4_1.getSize() +
    TC13::OperationFormat::DC16_2.getSize() +
    TC13::OperationFormat::L.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPOSTDEC( deref_ereg )", $1 );

  auto lvalue = $action[2]( true );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *regTmp = lvalue.getResultRegister();
  LLIR_Register *oneReg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, regTmp, $1->getExp() );
  TCINSTRUCTIONS.insertMOV( oneReg->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    oneReg->GetNextChild( oneReg->GetFirstChild() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertSUB_D( regTmp, regTmp, oneReg, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  auto &tmpReg = dynamic_cast<TC_ERegV &>( *(lvalue.getResultReg()) );
  auto &reg1 = TCINSTRUCTIONS.createEReg();

  TCINSTRUCTIONS.insertMOV( r, tmpReg, $1->getExp() );
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( reg1.begin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    dynamic_cast<TC_DRegV &>( reg1.rbegin()->get() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertSUB_D( tmpReg, tmpReg, reg1, $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_UnaryExpPOSTDEC( ereg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::SDC4_1.getSize() +
    TC13::OperationFormat::DC16_2.getSize() + SOFTFLOAT_COST;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPOSTDEC( ereg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertMOV( reg, p.first, $1->getExp() );
  TCINSTRUCTIONS.insertMOV( regTmp->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    regTmp->GetNextChild( regTmp->GetFirstChild() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertSUB_D( p.first, p.first, regTmp, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createEReg();
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertMOV( r, p.second, $1->getExp() );
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( tmpReg.begin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertSUB_D( p.second, p.second, tmpReg, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpPOSTDEC( dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  $cost[0] = $cost[2] + TC13::OperationFormat::SDD_1.getSize();

  if ( t.isIntegralType() ) {
    $cost[0] += TC13::OperationFormat::DDC9_1.getSize();
    auto tc = Cast::truncationCost( t, $2->getExp()->getType() );
    if ( tc != 0 )
      $cost[0] += tc + TC13::OperationFormat::SDD_1.getSize();
  } else
  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] +=
      TC13::OperationFormat::DC16_2.getSize() +
      TC13::OperationFormat::DDC16_1.getSize() +
      FLOAT_COST( TC13::OperationFormat::DDD_1 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPOSTDEC( dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );
  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( reg, p.first, $1->getExp() );

  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( p.first, p.first, -1, $1->getExp() );

    LLIR_Register *regTmp =
      Cast::doTruncation( t, $2->getExp()->getType(), p.first );
    if ( regTmp )
      TCINSTRUCTIONS.insertMOV( p.first, regTmp, $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    Float f( 1.0f );
    TCINSTRUCTIONS.insertMOVH_ADDI(
      regTmp, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertSUB_F( p.first, p.first, regTmp, $1->getExp() );
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOV( r, p.second, $1->getExp() );

  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( p.second, p.second, -1, $1->getExp() );

    auto &tmpReg =
      Cast::doTruncation(
        t, $2->getExp()->getType(), p.second, $1->getExp() );
    if ( tmpReg != p.second.get() )
      TCINSTRUCTIONS.insertMOV(
        p.second, dynamic_cast<TC_DRegV &>( tmpReg ), $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    Float f( 1.0f );
    TCINSTRUCTIONS.insertMOVH_ADDI(
      tmpReg, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertSUB_F( p.second, p.second, tmpReg, $1->getExp() );
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpPOSTDEC( deref_dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  $cost[0] =
    $cost[2] + TC13::OperationFormat::SDD_1.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );

  if ( t.isIntegralType() )
    $cost[0] +=
      TC13::OperationFormat::DDC9_1.getSize() +
      Cast::truncationCost( t, $2->getExp()->getType() );
  else
  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] +=
      TC13::OperationFormat::DC16_2.getSize() +
      TC13::OperationFormat::DDC16_1.getSize() +
      FLOAT_COST( TC13::OperationFormat::DDD_1 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPOSTDEC( deref_dreg )", $1 );

  auto lvalue = $action[2]( true );
  auto &t = effectiveType( *$2->getExp() );

  // LLIR
  LLIR_Register *regTmp = lvalue.getResultRegister();
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, regTmp, $1->getExp() );

  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( regTmp, regTmp, -1, $1->getExp() );

    LLIR_Register *tmp =
      Cast::doTruncation( t, $2->getExp()->getType(), regTmp );
    if ( tmp )
      TCINSTRUCTIONS.insertMOV( regTmp, tmp, $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *tmp = TCINSTRUCTIONS.CreateRegister( "" );
    Float f( 1.0f );

    TCINSTRUCTIONS.insertMOVH_ADDI(
      tmp, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertSUB_F( regTmp, regTmp, tmp, $1->getExp() );
  }

  // WIR
  auto &tmpReg = dynamic_cast<TC_DRegV &>( *(lvalue.getResultReg()) );
  auto &r = TCINSTRUCTIONS.createDReg();

  TCINSTRUCTIONS.insertMOV( r, tmpReg, $1->getExp() );

  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( tmpReg, tmpReg, -1, $1->getExp() );

    auto &tmp =
      Cast::doTruncation( t, $2->getExp()->getType(), tmpReg, $1->getExp() );
    if ( tmp != tmpReg )
      TCINSTRUCTIONS.insertMOV(
        tmpReg, dynamic_cast<TC_DRegV &>( tmp ), $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmp = TCINSTRUCTIONS.createDReg();
    Float f( 1.0f );

    TCINSTRUCTIONS.insertMOVH_ADDI(
      tmp, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertSUB_F( tmpReg, tmpReg, tmp, $1->getExp() );
  }

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_UnaryExpPOSTINC( deref_ereg )
{
  $cost[0] =
    $cost[2] + 8 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::SDC4_1.getSize() +
    TC13::OperationFormat::DC16_2.getSize() +
    TC13::OperationFormat::L.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPOSTINC( deref_ereg )", $1 );

  auto lvalue = $action[2]( true );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *regTmp = lvalue.getResultRegister();
  LLIR_Register *oneReg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, regTmp, $1->getExp() );
  TCINSTRUCTIONS.insertMOV( oneReg->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    oneReg->GetNextChild( oneReg->GetFirstChild() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertADD_D( regTmp, regTmp, oneReg, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  auto &tmpReg = dynamic_cast<TC_ERegV &>( *(lvalue.getResultReg()) );
  auto &reg1 = TCINSTRUCTIONS.createEReg();

  TCINSTRUCTIONS.insertMOV( r, tmpReg, $1->getExp() );
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( reg1.begin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    dynamic_cast<TC_DRegV &>( reg1.rbegin()->get() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertADD_D( tmpReg, tmpReg, reg1, $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_UnaryExpPOSTINC( ereg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::SDC4_1.getSize() +
    TC13::OperationFormat::DC16_2.getSize() + SOFTFLOAT_COST;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPOSTINC( ereg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertMOV( reg, p.first, $1->getExp() );
  TCINSTRUCTIONS.insertMOV( regTmp->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    regTmp->GetNextChild( regTmp->GetFirstChild() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertADD_D( p.first, p.first, regTmp, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createEReg();
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertMOV( r, p.second, $1->getExp() );
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( tmpReg.begin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertADD_D( p.second, p.second, tmpReg, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpPOSTINC( dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  $cost[0] = $cost[2] + TC13::OperationFormat::SDD_1.getSize();

  if ( t.isIntegralType() ) {
    $cost[0] += TC13::OperationFormat::DDC9_1.getSize();
    auto tc = Cast::truncationCost( t, $2->getExp()->getType() );
    if ( tc != 0 )
      $cost[0] += tc + TC13::OperationFormat::SDD_1.getSize();
  } else
  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] +=
      TC13::OperationFormat::DC16_2.getSize() +
      TC13::OperationFormat::DDC16_1.getSize() +
      FLOAT_COST( TC13::OperationFormat::DDD_1 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPOSTINC( dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );
  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( reg, p.first, $1->getExp() );

  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( p.first, p.first, 1, $1->getExp() );

    LLIR_Register *regTmp =
      Cast::doTruncation( t, $2->getExp()->getType(), p.first );
    if ( regTmp )
      TCINSTRUCTIONS.insertMOV( p.first, regTmp, $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    Float f( 1.0f );
    TCINSTRUCTIONS.insertMOVH_ADDI(
      regTmp, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertADD_F( p.first, p.first, regTmp, $1->getExp() );
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOV( r, p.second, $1->getExp() );

  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( p.second, p.second, 1, $1->getExp() );

    auto &tmpReg =
      Cast::doTruncation(
        t, $2->getExp()->getType(), p.second, $1->getExp() );
    if ( tmpReg != p.second.get() )
      TCINSTRUCTIONS.insertMOV(
        p.second, dynamic_cast<TC_DRegV &>( tmpReg ), $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    Float f( 1.0f );
    TCINSTRUCTIONS.insertMOVH_ADDI(
      tmpReg, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertADD_F( p.second, p.second, tmpReg, $1->getExp() );
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpPOSTINC( deref_dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  $cost[0] =
    $cost[2] + TC13::OperationFormat::SDD_1.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );

  if ( t.isIntegralType() )
    $cost[0] +=
      TC13::OperationFormat::DDC9_1.getSize() +
      Cast::truncationCost( t, $2->getExp()->getType() );
  else
  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] +=
      TC13::OperationFormat::DC16_2.getSize() +
      TC13::OperationFormat::DDC16_1.getSize() +
      FLOAT_COST( TC13::OperationFormat::DDD_1 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPOSTINC( deref_dreg )", $1 );

  auto lvalue = $action[2]( true );
  auto &t = effectiveType( *$2->getExp() );

  // LLIR
  LLIR_Register *regTmp = lvalue.getResultRegister();
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  // Generate the operation.
  TCINSTRUCTIONS.insertMOV( reg, regTmp, $1->getExp() );

  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( regTmp, regTmp, 1, $1->getExp() );

    LLIR_Register *tmp =
      Cast::doTruncation( t, $2->getExp()->getType(), regTmp );
    if ( tmp )
      TCINSTRUCTIONS.insertMOV( regTmp, tmp, $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *tmp = TCINSTRUCTIONS.CreateRegister( "" );
    Float f( 1.0f );

    TCINSTRUCTIONS.insertMOVH_ADDI(
      tmp, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertADD_F( regTmp, regTmp, tmp, $1->getExp() );
  }

  // WIR
  auto &tmpReg = dynamic_cast<TC_DRegV &>( *(lvalue.getResultReg()) );
  auto &r = TCINSTRUCTIONS.createDReg();

  TCINSTRUCTIONS.insertMOV( r, tmpReg, $1->getExp() );

  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( tmpReg, tmpReg, 1, $1->getExp() );

    auto &tmp =
      Cast::doTruncation( t, $2->getExp()->getType(), tmpReg, $1->getExp() );
    if ( tmp != tmpReg )
      TCINSTRUCTIONS.insertMOV(
        tmpReg, dynamic_cast<TC_DRegV &>( tmp ), $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmp = TCINSTRUCTIONS.createDReg();
    Float f( 1.0f );

    TCINSTRUCTIONS.insertMOVH_ADDI(
      tmp, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertADD_F( tmpReg, tmpReg, tmp, $1->getExp() );
  }

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_UnaryExpPREDEC( deref_ereg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::SDC4_1.getSize() +
    TC13::OperationFormat::DC16_2.getSize() +
    6 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPREDEC( deref_ereg )", $1 );

  auto lvalue = $action[2]( true );

  // LLIR
  LLIR_Register *reg = lvalue.getResultRegister();
  LLIR_Register *oneReg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( oneReg->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    oneReg->GetNextChild( oneReg->GetFirstChild() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertSUB_D( reg, reg, oneReg, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( *(lvalue.getResultReg()) );
  auto &reg1 = TCINSTRUCTIONS.createEReg();

  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( reg1.begin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    dynamic_cast<TC_DRegV &>( reg1.rbegin()->get() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertSUB_D( r, r, reg1, $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_UnaryExpPREDEC( ereg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::SDC4_1.getSize() +
    TC13::OperationFormat::DC16_2.getSize() + SOFTFLOAT_COST;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPREDEC( ereg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertMOV( regTmp->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    regTmp->GetNextChild( regTmp->GetFirstChild() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertSUB_D( p.first, p.first, regTmp, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( tmpReg.begin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertSUB_D( p.second, p.second, tmpReg, $1->getExp() );

  return( p );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpPREDEC( dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  $cost[0] = $cost[2];

  if ( t.isIntegralType() ) {
    $cost[0] += TC13::OperationFormat::DDC9_1.getSize();
    auto tc = Cast::truncationCost( t, $2->getExp()->getType() );
    if ( tc != 0 )
      $cost[0] += tc + TC13::OperationFormat::SDD_1.getSize();
  } else
  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] +=
      TC13::OperationFormat::DC16_2.getSize() +
      TC13::OperationFormat::DDC16_1.getSize() +
      FLOAT_COST( TC13::OperationFormat::DDD_1 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPREDEC( dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );
  auto p = $action[2]();

  // LLIR
  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( p.first, p.first, -1, $1->getExp() );

    LLIR_Register *regTmp =
      Cast::doTruncation( t, $2->getExp()->getType(), p.first );
    if ( regTmp )
      TCINSTRUCTIONS.insertMOV( p.first, regTmp, $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    Float f( 1.0f );
    TCINSTRUCTIONS.insertMOVH_ADDI(
      regTmp, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertSUB_F( p.first, p.first, regTmp, $1->getExp() );
  }

  // WIR
  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( p.second, p.second, -1, $1->getExp() );

    auto &tmpReg =
      Cast::doTruncation(
        t, $2->getExp()->getType(), p.second, $1->getExp() );
    if ( tmpReg != p.second.get() )
      TCINSTRUCTIONS.insertMOV(
        p.second, dynamic_cast<TC_DRegV &>( tmpReg ), $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    Float f( 1.0f );
    TCINSTRUCTIONS.insertMOVH_ADDI(
      tmpReg, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertSUB_F( p.second, p.second, tmpReg, $1->getExp() );
  }

  return( p );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpPREDEC( deref_dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  $cost[0] = $cost[2] + TC_AddressModification::createStoreCost( $2->getExp() );

  if ( t.isIntegralType() )
    $cost[0] +=
      TC13::OperationFormat::DDC9_1.getSize() +
      Cast::truncationCost( t, $2->getExp()->getType() );
  else
  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] +=
      TC13::OperationFormat::DC16_2.getSize() +
      TC13::OperationFormat::DDC16_1.getSize() +
      FLOAT_COST( TC13::OperationFormat::DDD_1 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPREDEC( deref_dreg )", $1 );

  auto lvalue = $action[2]( true );
  auto &t = effectiveType( *$2->getExp() );

  // LLIR
  LLIR_Register *reg = lvalue.getResultRegister();

  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( reg, reg, -1, $1->getExp() );

    LLIR_Register *tmp =
      Cast::doTruncation( t, $2->getExp()->getType(), reg );
    if ( tmp )
      TCINSTRUCTIONS.insertMOV( reg, tmp, $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *tmp = TCINSTRUCTIONS.CreateRegister( "" );
    Float f( 1.0f );

    TCINSTRUCTIONS.insertMOVH_ADDI(
      tmp, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertSUB_F( reg, reg, tmp, $1->getExp() );
  }

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( *(lvalue.getResultReg()) );

  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( r, r, -1, $1->getExp() );

    auto &tmp =
      Cast::doTruncation( t, $2->getExp()->getType(), r, $1->getExp() );
    if ( tmp != r )
      TCINSTRUCTIONS.insertMOV(
        r, dynamic_cast<TC_DRegV &>( tmp ), $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmp = TCINSTRUCTIONS.createDReg();
    Float f( 1.0f );

    TCINSTRUCTIONS.insertMOVH_ADDI(
      tmp, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertSUB_F( r, r, tmp, $1->getExp() );
  }

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_UnaryExpPREINC( deref_ereg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::SDC4_1.getSize() +
    TC13::OperationFormat::DC16_2.getSize() +
    6 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPREINC( deref_ereg )", $1 );

  auto lvalue = $action[2]( true );

  // LLIR
  LLIR_Register *reg = lvalue.getResultRegister();
  LLIR_Register *oneReg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( oneReg->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    oneReg->GetNextChild( oneReg->GetFirstChild() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertADD_D( reg, reg, oneReg, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( *(lvalue.getResultReg()) );
  auto &reg1 = TCINSTRUCTIONS.createEReg();

  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( reg1.begin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    dynamic_cast<TC_DRegV &>( reg1.rbegin()->get() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertADD_D( r, r, reg1, $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_UnaryExpPREINC( ereg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::SDC4_1.getSize() +
    TC13::OperationFormat::DC16_2.getSize() + SOFTFLOAT_COST;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPREINC( ereg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertMOV( regTmp->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    regTmp->GetNextChild( regTmp->GetFirstChild() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertADD_D( p.first, p.first, regTmp, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( tmpReg.begin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ), 16368, $1->getExp() );
  TCINSTRUCTIONS.insertADD_D( p.second, p.second, tmpReg, $1->getExp() );

  return( p );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpPREINC( dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  $cost[0] = $cost[2];

  if ( t.isIntegralType() ) {
    $cost[0] += TC13::OperationFormat::DDC9_1.getSize();
    auto tc = Cast::truncationCost( t, $2->getExp()->getType() );
    if ( tc != 0 )
      $cost[0] += tc + TC13::OperationFormat::SDD_1.getSize();
  } else
  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] +=
      TC13::OperationFormat::DC16_2.getSize() +
      TC13::OperationFormat::DDC16_1.getSize() +
      FLOAT_COST( TC13::OperationFormat::DDD_1 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPREINC( dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );
  auto p = $action[2]();

  // LLIR
  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( p.first, p.first, 1, $1->getExp() );

    LLIR_Register *regTmp =
      Cast::doTruncation( t, $2->getExp()->getType(), p.first );
    if ( regTmp )
      TCINSTRUCTIONS.insertMOV( p.first, regTmp, $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    Float f( 1.0f );
    TCINSTRUCTIONS.insertMOVH_ADDI(
      regTmp, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertADD_F( p.first, p.first, regTmp, $1->getExp() );
  }

  // WIR
  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( p.second, p.second, 1, $1->getExp() );

    auto &tmpReg =
      Cast::doTruncation(
        t, $2->getExp()->getType(), p.second, $1->getExp() );
    if ( tmpReg != p.second.get() )
      TCINSTRUCTIONS.insertMOV(
        p.second, dynamic_cast<TC_DRegV &>( tmpReg ), $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    Float f( 1.0f );
    TCINSTRUCTIONS.insertMOVH_ADDI(
      tmpReg, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertADD_F( p.second, p.second, tmpReg, $1->getExp() );
  }

  return( p );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpPREINC( deref_dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  $cost[0] = $cost[2] + TC_AddressModification::createStoreCost( $2->getExp() );

  if ( t.isIntegralType() )
    $cost[0] +=
      TC13::OperationFormat::DDC9_1.getSize() +
      Cast::truncationCost( t, $2->getExp()->getType() );
  else
  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] +=
      TC13::OperationFormat::DC16_2.getSize() +
      TC13::OperationFormat::DDC16_1.getSize() +
      FLOAT_COST( TC13::OperationFormat::DDD_1 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPREINC( deref_dreg )", $1 );

  auto lvalue = $action[2]( true );
  auto &t = effectiveType( *$2->getExp() );

  // LLIR
  LLIR_Register *reg = lvalue.getResultRegister();

  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( reg, reg, 1, $1->getExp() );

    LLIR_Register *tmp =
      Cast::doTruncation( t, $2->getExp()->getType(), reg );
    if ( tmp )
      TCINSTRUCTIONS.insertMOV( reg, tmp, $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *tmp = TCINSTRUCTIONS.CreateRegister( "" );
    Float f( 1.0f );

    TCINSTRUCTIONS.insertMOVH_ADDI(
      tmp, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertADD_F( reg, reg, tmp, $1->getExp() );
  }

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( *(lvalue.getResultReg()) );

  if ( t.isIntegralType() ) {
    TCINSTRUCTIONS.insertADD( r, r, 1, $1->getExp() );

    auto &tmp =
      Cast::doTruncation( t, $2->getExp()->getType(), r, $1->getExp() );
    if ( tmp != r )
      TCINSTRUCTIONS.insertMOV(
        r, dynamic_cast<TC_DRegV &>( tmp ), $1->getExp() );
  } else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmp = TCINSTRUCTIONS.createDReg();
    Float f( 1.0f );

    TCINSTRUCTIONS.insertMOVH_ADDI(
      tmp, f.getValue().getComposed(), $1->getExp() );
    TCINSTRUCTIONS.insertADD_F( r, r, tmp, $1->getExp() );
  }

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
# Sizeof expressions
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9: tpm_UnaryExpSIZEOF( ereg )
{
  int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= TC_Const9_Signed::getMinValue( 9 ) ) &&
       ( byteSize <= TC_Const9_Signed::getMaxValue( 9 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const9: tpm_UnaryExpSIZEOF( ereg )", $1 );

  return( computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst9: tpm_UnaryExpSIZEOF( ereg )
{
  int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= 0 ) &&
       ( byteSize <= (int) TC_Const9_Unsigned::getMaxValue( 9 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "uconst9: tpm_UnaryExpSIZEOF( ereg )", $1 );

  return( computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const16: tpm_UnaryExpSIZEOF( ereg )
{
  int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( byteSize <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] = 1000;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const16: tpm_UnaryExpSIZEOF( ereg )", $1 );

  return( computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9: tpm_UnaryExpSIZEOF( dreg )
{
  int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= TC_Const9_Signed::getMinValue( 9 ) ) &&
       ( byteSize <= TC_Const9_Signed::getMaxValue( 9 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const9: tpm_UnaryExpSIZEOF( dreg )", $1 );

  return( computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst9: tpm_UnaryExpSIZEOF( dreg )
{
  int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= 0 ) &&
       ( byteSize <= (int) TC_Const9_Unsigned::getMaxValue( 9 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "uconst9: tpm_UnaryExpSIZEOF( dreg )", $1 );

  return( computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const16: tpm_UnaryExpSIZEOF( dreg )
{
  int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( byteSize <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] = 1000;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const16: tpm_UnaryExpSIZEOF( dreg )", $1 );

  return( computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9: tpm_UnaryExpSIZEOF( areg )
{
  int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= TC_Const9_Signed::getMinValue( 9 ) ) &&
       ( byteSize <= TC_Const9_Signed::getMaxValue( 9 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const9: tpm_UnaryExpSIZEOF( areg )", $1 );

  return( computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst9: tpm_UnaryExpSIZEOF( areg )
{
  int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= 0 ) &&
       ( byteSize <= (int) TC_Const9_Unsigned::getMaxValue( 9 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "uconst9: tpm_UnaryExpSIZEOF( areg )", $1 );

  return( computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const16: tpm_UnaryExpSIZEOF( areg )
{
  int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( byteSize <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] = 1000;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const16: tpm_UnaryExpSIZEOF( areg )", $1 );

  return( computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9: tpm_SizeOfExp
{
  int byteSize = computeSizeOf( $1 );

  if ( ( byteSize >= TC_Const9_Signed::getMinValue( 9 ) ) &&
       ( byteSize <= TC_Const9_Signed::getMaxValue( 9 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const9: tpm_SizeOfExp", $1 );

  return( computeSizeOf( $1 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst9: tpm_SizeOfExp
{
  int byteSize = computeSizeOf( $1 );

  if ( ( byteSize >= 0 ) &&
       ( byteSize <= (int) TC_Const9_Unsigned::getMaxValue( 9 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "uconst9: tpm_SizeOfExp", $1 );

  return( computeSizeOf( $1 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const16: tpm_SizeOfExp
{
  int byteSize = computeSizeOf( $1 );

  if ( ( byteSize >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( byteSize <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] = 1000;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const16: tpm_SizeOfExp", $1 );

  return( computeSizeOf( $1 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_SizeOfExp
{
  // TODO: This rule should be deleted and replaced by one that returns an
  //       integer constant (which would be the rule that produces 'const16')
  //       plus a universal conversion rule 'dreg: const16'. The same scheme
  //       should be applied to the rule 'dreg: tpm_IntConstExp' to improve the
  //       modularity of the code.

  long long v = computeSizeOf( $1 );

  if ( $1->getExp()->getType().getType() != IR_Type::FLOAT ) {
    if ( ( v >= TC_Const4_Signed::getMinValue( 4 ) ) &&
         ( v <= TC_Const4_Signed::getMaxValue( 4 ) ) )
      $cost[0] = 10000 + TC13::OperationFormat::SDC4_1.getSize();
    else

    if ( ( v >= TC_Const16_Signed::getMinValue( 16 ) ) &&
         ( v <= TC_Const16_Signed::getMaxValue( 16 ) ) )
      $cost[0] = 10000 + TC13::OperationFormat::DC16_1.getSize();
    else

    if ( ( v >= 0 ) && ( v <= (int) TC_Const16_Unsigned::getMaxValue( 16 ) ) )
      $cost[0] = 10000 + TC13::OperationFormat::DC16_2.getSize();
    else
      $cost[0] =
        10000 + TC13::OperationFormat::DC16_2.getSize() +
        TC13::OperationFormat::DDC16_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_SizeOfExp", $1 );

  long long constValue = computeSizeOf( $1 );
  int bitWidth = getBitWidth( constValue );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( ( bitWidth == 17 ) && ( constValue >= 0 ) )
    TCINSTRUCTIONS.insertMOV_U( reg, constValue, $1->getExp() );
  else
  if ( ( bitWidth >= 17 ) && ( bitWidth <= 32 ) )
    TCINSTRUCTIONS.insertMOVH_ADDI( reg, constValue, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOVConstant( r, constValue, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


#####################################################################
#
#
# If-then-else statements
#
#
#####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_IfStmt( nrel )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_IfStmt( nrel )", $1 );

  // LLIR
  stringstream ss;
  ss << dynamic_cast<IR_IfStmt *>( $1->getStmt() )->getContinueBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );

  // WIR
  auto &b =
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_IfStmt *>( $1->getStmt() )->getContinueBasicBlock() );

  $action[2]( bb, b, false );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_IfStmt( ereg )
{
  $cost[0] =
    $cost[2] + 2 * TC13::OperationFormat::SDC4_1.getSize() + SOFTFLOAT_COST +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_IfStmt( ereg )", $1 );

  auto p = $action[2]();

  // LLIR
  stringstream ss;
  ss << dynamic_cast<IR_IfStmt *>( $1->getStmt() )->getContinueBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );

  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertMOV( regTmp->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    regTmp->GetNextChild( regTmp->GetFirstChild() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( reg, p.first, regTmp, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( reg, 0, bb, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createEReg();
  auto &r = TCINSTRUCTIONS.createDReg();

  TCINSTRUCTIONS.insertMOVConstant( tmpReg, 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( r, p.second, tmpReg, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ(
    r, 0,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_IfStmt *>( $1->getStmt() )->getContinueBasicBlock() ),
    $1->getExp() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_IfStmt( dreg )
{
  $cost[0] = $cost[2] + TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_IfStmt( dreg )", $1 );

  auto p = $action[2]();

  // LLIR
  stringstream ss;
  ss << dynamic_cast<IR_IfStmt *>( $1->getStmt() )->getContinueBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );
  TCINSTRUCTIONS.insertJEQ( p.first, 0, bb, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJEQ(
    p.second, 0,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_IfStmt *>( $1->getStmt() )->getContinueBasicBlock() ),
    $1->getExp() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_IfStmt( areg )
{
  $cost[0] = $cost[2] + TC13::OperationFormat::AL_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_IfStmt( areg )", $1 );

  auto p = $action[2]();

  // LLIR
  stringstream ss;
  ss << dynamic_cast<IR_IfStmt *>( $1->getStmt() )->getContinueBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );
  TCINSTRUCTIONS.insertJZ_A( p.first, bb, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJZ_A(
    p.second,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_IfStmt *>( $1->getStmt() )->getContinueBasicBlock() ),
    $1->getExp() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_IfElseStmt( nrel )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_IfElseStmt( nrel )", $1 );

  // LLIR
  stringstream ss;
  ss << dynamic_cast<IR_IfElseStmt *>( $1->getStmt() )->getFalseBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );

  // WIR
  auto &b =
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_IfElseStmt *>( $1->getStmt() )->getFalseBasicBlock() );

  $action[2]( bb, b, false );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_IfElseStmt( ereg )
{
  $cost[0] =
    $cost[2] + 2 * TC13::OperationFormat::SDC4_1.getSize() + SOFTFLOAT_COST +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_IfElseStmt( ereg )", $1 );

  auto p = $action[2]();

  // LLIR
  stringstream ss;
  ss << dynamic_cast<IR_IfElseStmt *>( $1->getStmt() )->getFalseBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );

  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertMOV( regTmp->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    regTmp->GetNextChild( regTmp->GetFirstChild() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( reg, p.first, regTmp, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( reg, 0, bb, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createEReg();
  auto &r = TCINSTRUCTIONS.createDReg();

  TCINSTRUCTIONS.insertMOVConstant( tmpReg, 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( r, p.second, tmpReg, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ(
    r, 0,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_IfElseStmt *>( $1->getStmt() )->getFalseBasicBlock() ),
    $1->getExp() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_IfElseStmt( dreg )
{
  $cost[0] = $cost[2] + TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_IfElseStmt( dreg )", $1 );

  auto p = $action[2]();

  // LLIR
  stringstream ss;
  ss << dynamic_cast<IR_IfElseStmt *>( $1->getStmt() )->getFalseBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );
  TCINSTRUCTIONS.insertJEQ( p.first, 0, bb, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJEQ(
    p.second, 0,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_IfElseStmt *>( $1->getStmt() )->getFalseBasicBlock() ),
    $1->getExp() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_IfElseStmt( areg )
{
  $cost[0] = $cost[2] + TC13::OperationFormat::AL_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_IfElseStmt( areg )", $1 );

  auto p = $action[2]();

  // LLIR
  stringstream ss;
  ss << dynamic_cast<IR_IfElseStmt *>( $1->getStmt() )->getFalseBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );
  TCINSTRUCTIONS.insertJZ_A( p.first, bb, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJZ_A(
    p.second,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_IfElseStmt *>( $1->getStmt() )->getFalseBasicBlock() ),
    $1->getExp() );
};


#####################################################################
#
#
# For-loop
#
#
#####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ForStmt( nrel )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ForStmt( nrel )", $1 );

  auto *loop = dynamic_cast<IR_LoopStmt *>( $1->getStmt()->getParent() );

  // LLIR
  stringstream tmpStr, ss;
  tmpStr << $1->getStmt()->getBasicBlock();
  string bb = TCCODESEL->getBlockLabel( tmpStr.str() );

  // Generate new basic block for loop condition, if not yet existing.
  if ( !TCCODESEL->containsBB( bb ) )
    beginNewLLIRBasicBlock( bb.c_str(), *$1->getStmt()->getBasicBlock() );
  TCCODESEL->setCurrentInstruction( nullptr );

  ss << loop->getFalseBasicBlock();
  bb = TCCODESEL->getBlockLabel( ss.str() );

  // WIR
  auto &b = TCCODESEL->getWIRBlock( loop->getFalseBasicBlock() );

  $action[2]( bb, b, true );

  // LLIR
  TCCODESEL->getLastLLIRBB()->AddPragma(
    new LLIR_Pragma( "Loop condition: FOR", true ) );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ForStmt( dreg )
{
  $cost[0] = $cost[2] + TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ForStmt( dreg )", $1 );

  // LLIR
  stringstream tmpStr, ss;
  tmpStr << $1->getStmt()->getBasicBlock();
  string bb = TCCODESEL->getBlockLabel( tmpStr.str() );
  if ( !TCCODESEL->containsBB( bb ) )
    beginNewLLIRBasicBlock( bb.c_str(), *$1->getStmt()->getBasicBlock() );

  auto p = $action[2]();

  auto *loop = dynamic_cast<IR_LoopStmt *>( $1->getStmt()->getParent() );
  ss << loop->getFalseBasicBlock();
  bb = TCCODESEL->getBlockLabel( ss.str() );

  TCINSTRUCTIONS.insertJEQ(
    p.first, 0, bb, $1->getExp(), InstructionFactory::FOR_STMT );
  TCCODESEL->getLastLLIRBB()->AddPragma(
    new LLIR_Pragma( "Loop condition: FOR", true ) );

  // WIR
  TCINSTRUCTIONS.insertJEQ(
    p.second, 0,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_LoopStmt *>(
        $1->getStmt()->getParent() )->getFalseBasicBlock() ),
    $1->getExp(), InstructionFactory::FOR_STMT );

  WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
  jmp.insertContainer( WIR_LoopExit( true ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ForStmt( areg )
{
  $cost[0] = $cost[2] + TC13::OperationFormat::AL_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ForStmt( areg )", $1 );

  // LLIR
  stringstream tmpStr, ss;
  tmpStr << $1->getStmt()->getBasicBlock();
  string bb = TCCODESEL->getBlockLabel( tmpStr.str() );
  if ( !TCCODESEL->containsBB( bb ) )
    beginNewLLIRBasicBlock( bb.c_str(), *$1->getStmt()->getBasicBlock() );

  auto p = $action[2]();

  auto *loop = dynamic_cast<IR_LoopStmt *>( $1->getStmt()->getParent() );
  ss << loop->getFalseBasicBlock();
  bb = TCCODESEL->getBlockLabel( ss.str() );

  TCINSTRUCTIONS.insertJZ_A(
    p.first, bb, $1->getExp(), InstructionFactory::FOR_STMT );
  TCCODESEL->getLastLLIRBB()->AddPragma(
    new LLIR_Pragma( "Loop condition: FOR", true ) );

  // WIR
  TCINSTRUCTIONS.insertJZ_A(
    p.second,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_LoopStmt *>(
        $1->getStmt()->getParent() )->getFalseBasicBlock() ),
    $1->getExp(), InstructionFactory::FOR_STMT );

  WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
  jmp.insertContainer( WIR_LoopExit( true ) );
};


#####################################################################
#
#
# While-loop
#
#
#####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_WhileStmt( nrel )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_WhileStmt( nrel )", $1 );

  auto *loop = dynamic_cast<IR_LoopStmt *>( $1->getStmt() );

  // LLIR
  stringstream ss;
  ss << loop->getFalseBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );

  // WIR
  auto &b = TCCODESEL->getWIRBlock( loop->getFalseBasicBlock() );

  $action[2]( bb, b, true );

  // LLIR
  TCCODESEL->getLastLLIRBB()->AddPragma(
    new LLIR_Pragma( "Loop condition: WHILE", true ) );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_WhileStmt( dreg )
{
  $cost[0] = $cost[2] + TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_WhileStmt( dreg )", $1 );

  auto p = $action[2]();

  // LLIR
  stringstream ss;
  ss << dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getFalseBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );
  TCINSTRUCTIONS.insertJEQ(
    p.first, 0, bb, $1->getExp(), InstructionFactory::WHILE_STMT );
  TCCODESEL->getLastLLIRBB()->AddPragma(
    new LLIR_Pragma( "Loop condition: WHILE", true ) );

  // WIR
  TCINSTRUCTIONS.insertJEQ(
    p.second, 0,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getFalseBasicBlock() ),
    $1->getExp(), InstructionFactory::WHILE_STMT );

  WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
  jmp.insertContainer( WIR_LoopExit( true ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_WhileStmt( areg )
{
  $cost[0] = $cost[2] + TC13::OperationFormat::AL_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_WhileStmt( areg )", $1 );

  auto p = $action[2]();

  // LLIR
  stringstream ss;
  ss << dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getFalseBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );
  TCINSTRUCTIONS.insertJZ_A(
    p.first, bb, $1->getExp(), InstructionFactory::WHILE_STMT );
  TCCODESEL->getLastLLIRBB()->AddPragma(
    new LLIR_Pragma( "Loop condition: WHILE", true ) );

  // WIR
  TCINSTRUCTIONS.insertJZ_A(
    p.second,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getFalseBasicBlock() ),
    $1->getExp(), InstructionFactory::WHILE_STMT );

  WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
  jmp.insertContainer( WIR_LoopExit( true ) );
};


#####################################################################
#
#
# doWhile-loop
#
#
#####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_DoWhileStmt( rel )
{
  // We cannot generate a LOOP in this case, because 'rel' already implies the
  // jump.
  $cost[0] = $cost[2];
}
=
{

  DEBUG_RULE_ACTION( "stmt: tpm_DoWhileStmt( rel )", $1 );

  auto *loop = dynamic_cast<IR_LoopStmt *>( $1->getStmt() );

  // LLIR
  stringstream ss;
  ss << loop->getTrueBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );

  // WIR
  auto &b = TCCODESEL->getWIRBlock( loop->getTrueBasicBlock() );

  $action[2]( bb, b, true );

  TCCODESEL->getLastLLIRBB()->AddPragma(
    new LLIR_Pragma( "Loop condition: DOWHILE", true ) );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_DoWhileStmt( dreg )
{
  auto *doWhileStmt = dynamic_cast<IR_DoWhileStmt *>( $1->getStmt() );

  if ( TCCODESEL->getConfig()->getEnableLOOPInstruction() &&
       TCCODESEL->hasApplicableLoop( doWhileStmt ) )
    $cost[0] = transformToLOOPLoopCost( *doWhileStmt );
  else
    $cost[0] = $cost[2] + TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_DoWhileStmt( dreg )", $1 );

  auto *doWhileStmt = dynamic_cast<IR_DoWhileStmt *>( $1->getStmt() );

  if ( TCCODESEL->getConfig()->getEnableLOOPInstruction() &&
       TCCODESEL->hasApplicableLoop( doWhileStmt ) )
    transformToLOOPLoop( *doWhileStmt, $1->getExp() );
  else {
    auto p = $action[2]();

    // LLIR
    stringstream ss;
    ss << dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getTrueBasicBlock();
    string bb = TCCODESEL->getBlockLabel( ss.str() );
    TCINSTRUCTIONS.insertJNE(
      p.first, 0, bb, $1->getExp(), InstructionFactory::DOWHILE_STMT );
    TCCODESEL->getLastLLIRBB()->AddPragma(
      new LLIR_Pragma( "Loop condition: DOWHILE", true ) );

    // WIR
    TCINSTRUCTIONS.insertJNE(
      p.second, 0,
      TCCODESEL->getWIRBlock(
        dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getTrueBasicBlock() ),
      $1->getExp(), InstructionFactory::DOWHILE_STMT );

    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_DoWhileStmt( areg )
{
  auto *doWhileStmt = dynamic_cast<IR_DoWhileStmt *>( $1->getStmt() );

  if ( TCCODESEL->getConfig()->getEnableLOOPInstruction() &&
       TCCODESEL->hasApplicableLoop( doWhileStmt ) )
    $cost[0] = transformToLOOPLoopCost( *doWhileStmt );
  else
    $cost[0] = $cost[2] + TC13::OperationFormat::AL_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_DoWhileStmt( areg )", $1 );

  auto *doWhileStmt = dynamic_cast<IR_DoWhileStmt *>( $1->getStmt() );

  if ( TCCODESEL->getConfig()->getEnableLOOPInstruction() &&
       TCCODESEL->hasApplicableLoop( doWhileStmt ) )
    transformToLOOPLoop( *doWhileStmt );
  else {
    auto p = $action[2]();

    // LLIR
    stringstream ss;
    ss << dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getTrueBasicBlock();
    string bb = TCCODESEL->getBlockLabel( ss.str() );
    TCINSTRUCTIONS.insertJNZ_A(
      p.first, bb, $1->getExp(), InstructionFactory::DOWHILE_STMT );
    TCCODESEL->getLastLLIRBB()->AddPragma(
      new LLIR_Pragma( "Loop condition: DOWHILE", true ) );

    // WIR
    TCINSTRUCTIONS.insertJNZ_A(
      p.second,
      TCCODESEL->getWIRBlock(
        dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getTrueBasicBlock() ),
      $1->getExp(), InstructionFactory::DOWHILE_STMT );

    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }
};


#####################################################################
#
#
# Selection
#
#
#####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_CondExp( nrel, areg, areg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] + TC13::OperationFormat::L.getSize() +
    TC13::OperationFormat::SAA_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_CondExp( nrel, areg, areg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // WIR
  auto *currentBB = TC179x_wirBB;
  // Create basic block for LHS argument of ':' operator.
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate code for the condition in the current basic block.
  $action[2]( label1, b2, false );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertMOV_AA( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertMOV_AA( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  TCINSTRUCTIONS.insertMOV_AA( reg, p3.first, $1->getExp() );

  // Create new basic block containing future parent instructions.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertMOV_AA( r, p3.second, $1->getExp() );

  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_CondExp( dreg, areg, areg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] + TC13::OperationFormat::DC4L_1.getSize() +
    TC13::OperationFormat::L.getSize() +
    2 * TC13::OperationFormat::SAA_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_CondExp( dreg, areg, areg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // Generate code for the condition in the current basic block.
  auto p1 = $action[2]();

  // LLIR
  // Generate conditional branch in the current basic block.
  TCINSTRUCTIONS.insertJEQ( p1.first, 0, label1, $1->getExp() );

  // WIR
  // Create basic block for LHS argument of ':' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate conditional branch in the current basic block.
  TCINSTRUCTIONS.insertJEQ( p1.second, 0, b2, $1->getExp() );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for LHS argument of ':' operator.
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertMOV_AA( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertMOV_AA( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  TCINSTRUCTIONS.insertMOV_AA( reg, p3.first, $1->getExp() );

  // Create new basic block containing future parent instructions.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertMOV_AA( r, p3.second, $1->getExp() );
  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_CondExp( ereg, areg, areg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] +
    2 * TC13::OperationFormat::SDC4_1.getSize() +
    5 * TC13::OperationFormat::SDD_1.getSize() +
    2 * TC13::OperationFormat::L.getSize() +
    TC13::OperationFormat::DC4L_1.getSize() +
    2 * TC13::OperationFormat::SAA_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_CondExp( ereg, areg, areg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // Generate code for the condition in the current basic block.
  auto p1 = $action[2]();

  // LLIR
  // Generate conditional branch in the current basic block.
  LLIR_Register *regTmp1 = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *regTmp2 = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( regTmp1->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    regTmp1->GetNextChild( regTmp1->GetFirstChild() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( regTmp2, p1.first, regTmp1, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( regTmp2, 0, label1, $1->getExp() );

  // WIR
  // Create basic block for LHS argument of ':' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate conditional branch in the current basic block.
  auto &tmpEReg = TCINSTRUCTIONS.createEReg();
  auto &tmpDReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOVConstant( tmpEReg, 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( tmpDReg, p1.second, tmpEReg, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( tmpDReg, 0, b2, $1->getExp() );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for LHS argument of ':' operator.
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertMOV_AA( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertMOV_AA( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  TCINSTRUCTIONS.insertMOV_AA( reg, p3.first, $1->getExp() );

  // Create new basic block containing future parent instructions.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertMOV_AA( r, p3.second, $1->getExp() );
  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_CondExp( nrel, dreg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] + TC13::OperationFormat::L.getSize();

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    $cost[0] += 2 * TC13::OperationFormat::SDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_CondExp( nrel, dreg, dreg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // WIR
  auto *currentBB = TC179x_wirBB;
  // Create basic block for LHS argument of ':' operator.
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate code for the condition in the current basic block.
  $action[2]( label1, b2, false );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    TCINSTRUCTIONS.insertMOV( reg, p3.first, $1->getExp() );

  // WIR
  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    TCINSTRUCTIONS.insertMOV( r, p3.second, $1->getExp() );

  // LLIR
  // Create basic block for code after the '?' operator.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_CondExp( dreg, dreg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] + TC13::OperationFormat::DC4L_1.getSize() +
    TC13::OperationFormat::L.getSize();

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    $cost[0] += 2 * TC13::OperationFormat::SDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_CondExp( dreg, dreg, dreg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // Generate code for the condition in the current basic block.
  auto p1 = $action[2]();

  // LLIR
  // Generate conditional branch in the current basic block.
  TCINSTRUCTIONS.insertJEQ( p1.first, 0, label1, $1->getExp() );

  // WIR
  // Create basic block for LHS argument of ':' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate conditional branch in the current basic block.
  TCINSTRUCTIONS.insertJEQ( p1.second, 0, b2, $1->getExp() );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for LHS argument of ':' operator.
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    TCINSTRUCTIONS.insertMOV( reg, p3.first, $1->getExp() );

  // WIR
  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    TCINSTRUCTIONS.insertMOV( r, p3.second, $1->getExp() );

  // LLIR
  // Create basic block for code after the '?' operator.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_CondExp( ereg, dreg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] + TC13::OperationFormat::L.getSize() +
    2 * TC13::OperationFormat::SDC4_1.getSize() + SOFTFLOAT_COST +
    TC13::OperationFormat::DC4L_1.getSize();

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    $cost[0] += 2 * TC13::OperationFormat::SDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_CondExp( ereg, dreg, dreg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // Generate code for the condition in the current basic block.
  auto p1 = $action[2]();

  // LLIR
  // Generate conditional branch in the current basic block.
  LLIR_Register *regTmp1 = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *regTmp2 = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( regTmp1->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    regTmp1->GetNextChild( regTmp1->GetFirstChild() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( regTmp2, p1.first, regTmp1, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( regTmp2, 0, label1, $1->getExp() );

  // WIR
  // Create basic block for LHS argument of ':' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate conditional branch in the current basic block.
  auto &tmpEReg = TCINSTRUCTIONS.createEReg();
  auto &tmpDReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOVConstant( tmpEReg, 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( tmpDReg, p1.second, tmpEReg, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( tmpDReg, 0, b2, $1->getExp() );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for LHS argument of ':' operator.
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    TCINSTRUCTIONS.insertMOV( reg, p3.first, $1->getExp() );

  // WIR
  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    TCINSTRUCTIONS.insertMOV( r, p3.second, $1->getExp() );

  // LLIR
  // Create basic block for code after the '?' operator.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_CondExp( nrel, ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] +
    4 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_CondExp( nrel, ereg, ereg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // WIR
  auto *currentBB = TC179x_wirBB;
  // Create basic block for LHS argument of ':' operator.
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate code for the condition in the current basic block.
  $action[2]( label1, b2, false );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();

  TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  TCINSTRUCTIONS.insertMOV( reg, p3.first, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertMOV( r, p3.second, $1->getExp() );

  // LLIR
  // Create basic block for code after the '?' operator.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_CondExp( dreg, ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] + TC13::OperationFormat::DC4L_1.getSize() +
    4 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_CondExp( dreg, ereg, ereg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // Generate code for the condition in the current basic block.
  auto p1 = $action[2]();

  // LLIR
  // Generate conditional branch in the current basic block.
  TCINSTRUCTIONS.insertJEQ( p1.first, 0, label1, $1->getExp() );

  // WIR
  // Create basic block for LHS argument of ':' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate conditional branch in the current basic block.
  TCINSTRUCTIONS.insertJEQ( p1.second, 0, b2, $1->getExp() );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for LHS argument of ':' operator.
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();

  TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  TCINSTRUCTIONS.insertMOV( reg, p3.first, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertMOV( r, p3.second, $1->getExp() );

  // LLIR
  // Create basic block for code after the '?' operator.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_CondExp( ereg, ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] +
    2 * TC13::OperationFormat::SDC4_1.getSize() + SOFTFLOAT_COST +
    TC13::OperationFormat::DC4L_1.getSize() +
    4 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_CondExp( ereg, ereg, ereg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // Generate code for the condition in the current basic block.
  auto p1 = $action[2]();

  // LLIR
  // Generate conditional branch in the current basic block.
  LLIR_Register *regTmp1 = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *regTmp2 = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertMOV( regTmp1->GetFirstChild(), 0, $1->getExp() );
  TCINSTRUCTIONS.insertMOVH(
    regTmp1->GetNextChild( regTmp1->GetFirstChild() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( regTmp2, p1.first, regTmp1, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( regTmp2, 0, label1, $1->getExp() );

  // WIR
  // Create basic block for LHS argument of ':' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate conditional branch in the current basic block.
  auto &tmpReg1 = TCINSTRUCTIONS.createEReg();
  auto &tmpReg2 = TCINSTRUCTIONS.createDReg();

  TCINSTRUCTIONS.insertMOVConstant( tmpReg1, 0, $1->getExp() );
  TCINSTRUCTIONS.insertEQ_D( tmpReg2, p1.second, tmpReg1, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( tmpReg2, 0, b2, $1->getExp() );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for LHS argument of ':' operator.
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();

  TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  TCINSTRUCTIONS.insertMOV( reg, p3.first, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertMOV( r, p3.second, $1->getExp() );

  // LLIR
  // Create basic block for code after the '?' operator.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


#####################################################################
#
#
# Jump statements
#
#
#####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ImplicitJump
{
  $cost[0] = TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ImplicitJump", $1 );

  auto *whileStmt =
    dynamic_cast< IR_WhileStmt *>(
      $1->getBasicBlock()->getImplicitSucc()->getStatements().front() );
  auto *forStmt =
    dynamic_cast< IR_ForStmt *>(
      $1->getBasicBlock()->getStatements().front()->getParent()->getParent() );

  // LLIR
  // Generate new internal basic block for this implicit jump.
  beginNewLLIRBasicBlock( *$1->getBasicBlock() );

  // Generate unconditional jump to target block.
  stringstream ss;
  ss << $1->getBasicBlock()->getImplicitSucc();
  string lab = TCCODESEL->getBlockLabel( ss.str() );

  if ( TCCODESEL->getConfig()->getEnableLOOPUInstruction() &&
       ( ( whileStmt && TCCODESEL->hasApplicableLoop( whileStmt ) ) ||
         ( forStmt && TCCODESEL->hasApplicableLoop( forStmt ) ) ) )
    TCINSTRUCTIONS.insertLOOPU( lab, $1->getExp() );
  else
    TCINSTRUCTIONS.insertJ( lab, $1->getExp(), InstructionFactory::JUMP_STMT );

  // WIR
  // Generate new internal basic block for this implicit jump.
  TCCODESEL->startNewBasicBlock( *$1->getBasicBlock() );
  if ( TCCODESEL->getConfig()->getEnableLOOPUInstruction() &&
       ( ( whileStmt && TCCODESEL->hasApplicableLoop( whileStmt ) ) ||
         ( forStmt && TCCODESEL->hasApplicableLoop( forStmt ) ) ) )
    TCINSTRUCTIONS.insertLOOPU(
      TCCODESEL->getWIRBlock( $1->getBasicBlock()->getImplicitSucc() ),
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertJ(
      TCCODESEL->getWIRBlock( $1->getBasicBlock()->getImplicitSucc() ),
      $1->getExp(), InstructionFactory::JUMP_STMT );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_JumpStmt
{
  $cost[0] = TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_JumpStmt", $1 );

  // LLIR
  stringstream ss;
  ss << $1->getStmt()->getBasicBlock()->getImplicitSucc();
  string lab = TCCODESEL->getBlockLabel( ss.str() );

  TCINSTRUCTIONS.insertJ( lab, $1->getExp(), InstructionFactory::JUMP_STMT );

  // TODO: Why is "continue" missing here?
  if ( dynamic_cast<IR_BreakStmt *>( $1->getStmt() ) )
    TCCODESEL->getLastLLIRBB()->AddPragma(
      new LLIR_Pragma( "Loop condition: BREAK", true ) );

  // WIR
  TCINSTRUCTIONS.insertJ(
    TCCODESEL->getWIRBlock( $1->getStmt()->getBasicBlock()->getImplicitSucc() ),
    $1->getExp(), InstructionFactory::JUMP_STMT );

  if ( dynamic_cast<IR_BreakStmt *>( $1->getStmt() ) ) {
    // From the current break statement upwards, check whether the break jumps
    // out of a loop or out of a switch statement.
    bool breakOutOfLoop = false;
    auto *currentStmt = $1->getStmt();

    while ( currentStmt ) {
      if ( dynamic_cast<IR_LoopStmt *>( currentStmt ) ) {
        breakOutOfLoop = true;
        break;
      } else

      if ( dynamic_cast<IR_SwitchStmt *>( currentStmt ) )
        break;
      else
        currentStmt = currentStmt->getParent();
    }

    if ( breakOutOfLoop ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_Break() );
    }
  }

  return;
};


###############################################################################
#
#
# Switch, case and default statements
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_SwitchStmt( dreg )
{
  auto *stmt = dynamic_cast<IR_SwitchStmt *>( $1->getStmt() );
  $cost[0] = $cost[2] + TC13::OperationFormat::L.getSize();

  for ( auto c : stmt->getCases() ) {
    int caseLabelValue = c.first.getIntValue();

    if ( ( caseLabelValue >= TC_Const4_Signed::getMinValue( 4 ) ) &&
         ( caseLabelValue <= TC_Const4_Signed::getMaxValue( 4 ) ) )
      $cost[0] += TC13::OperationFormat::DC4L_1.getSize();
    else {
      if ( ( caseLabelValue >= TC_Const16_Signed::getMinValue( 16 ) ) &&
           ( caseLabelValue <= TC_Const16_Signed::getMaxValue( 16 ) ) )
        $cost[0] += TC13::OperationFormat::DC16_1.getSize();
      else

      if ( ( caseLabelValue >= 0 ) &&
           ( caseLabelValue <= (int) TC_Const16_Unsigned::getMaxValue( 16 ) ) )
        $cost[0] += TC13::OperationFormat::DC16_2.getSize();
      else
        $cost[0] +=
          TC13::OperationFormat::DC16_2.getSize() +
          TC13::OperationFormat::DDC16_1.getSize();
      $cost[0] += TC13::OperationFormat::DDL_1.getSize();
    }
  }
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_SwitchStmt( dreg )", $1 );

  auto *stmt = dynamic_cast<IR_SwitchStmt *>( $1->getStmt() );

  auto p = $action[2]();

  // LLIR
  // Traverse through all case statements and generate jump (and possibly move)
  // instructions.
  for ( auto it = stmt->getCases().begin(); it != stmt->getCases().end();
        ++it ) {
    int caseLabelValue = it->first.getIntValue();

    // Generate label for case statement used with the jump instruction below.
    // To get a unique id, read the basic block's hex address.
    stringstream caseAddress;
    caseAddress << it->second->getBasicBlock();
    string caseStmtLabel = TCCODESEL->getBlockLabel( caseAddress.str() );

    // If case label constant can be represented by 4 bits, just generate a
    // jump instructions with displacement without any mov instruction.
    if ( ( caseLabelValue >= minSignedConst4Value ) &&
         ( caseLabelValue <= maxSignedConst4Value ) )
      TCINSTRUCTIONS.insertJEQ(
        p.first, caseLabelValue, caseStmtLabel, $1->getExp(),
        InstructionFactory::SWITCH_STMT );
    else {
      // Register holds the second operator for the JGE comparison.
      LLIR_Register *reg2 = TCINSTRUCTIONS.CreateRegister( "" );

      // Generate appropriate MOV instruction for the case label.
      TCINSTRUCTIONS.insertMOVConstant( reg2, caseLabelValue, $1->getExp() );

      // Generate jump instruction using "mov" register for comparison.
      TCINSTRUCTIONS.insertJEQ(
        p.first, reg2, caseStmtLabel, $1->getExp(),
        InstructionFactory::SWITCH_STMT );
    }

    // Begin new basic block after each jump instruction.
    beginNewLLIRBasicBlock( *$1->getStmt()->getBasicBlock() );
  }

  // Finally, emit an unconditional jump to either the basic block of the
  // default-label or the succeeding basic block of the switch-statement (see
  // IR_SwitchStmt class description).
  stringstream ss;
  if ( stmt->getDefault() )
    ss << stmt->getDefault()->getBasicBlock();
  else
    ss << stmt->getContinueBasicBlock();
  string endlabel = TCCODESEL->getBlockLabel( ss.str() );
  TCINSTRUCTIONS.insertJ(
    endlabel, $1->getExp(), InstructionFactory::SWITCH_STMT );

  // WIR
  // Traverse through all case statements and generate jump (and possibly move)
  // instructions.
  for ( auto c : stmt->getCases() ) {
    int caseLabelValue = c.first.getIntValue();

    // If case label constant can be represented by 4 bits, just generate a
    // jump instructions with displacement without any mov instruction.
    if ( ( caseLabelValue >= TC_Const4_Signed::getMinValue( 4 ) ) &&
         ( caseLabelValue <= TC_Const4_Signed::getMaxValue( 4 ) ) )
      TCINSTRUCTIONS.insertJEQ(
        p.second, caseLabelValue,
        TCCODESEL->getWIRBlock( c.second->getBasicBlock() ), $1->getExp(),
        InstructionFactory::SWITCH_STMT );
    else {
      auto &tmpReg = TCINSTRUCTIONS.createDReg();
      TCINSTRUCTIONS.insertMOVConstant( tmpReg, caseLabelValue, $1->getExp() );
      TCINSTRUCTIONS.insertJEQ(
        p.second, tmpReg, TCCODESEL->getWIRBlock( c.second->getBasicBlock() ),
        $1->getExp(), InstructionFactory::SWITCH_STMT );
    }
    TCCODESEL->startNewBasicBlock( *$1->getStmt()->getBasicBlock() );
  }

  // Finally, emit an unconditional jump to either the basic block of the
  // default label or the succeeding basic block of the switch-statement (see
  // IR_SwitchStmt class description).
  TCINSTRUCTIONS.insertJ(
    TCCODESEL->getWIRBlock(
      stmt->getDefault() ?
        stmt->getDefault()->getBasicBlock() : stmt->getContinueBasicBlock() ),
    $1->getExp(), InstructionFactory::SWITCH_STMT );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_CaseStmt
{
  // There is nothing to do here, since a case label starts a new basic block
  // whose label is already automatically emitted.
  $cost[0] = 0;
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_CaseStmt", $1 );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_DefaultStmt
{
  // There is nothing to do here, since a case label starts a new basic block
  // whose label is already automatically emitted.
  $cost[0] = 0;
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_DefaultStmt", $1 );

  return;
};


##############################################################################
#
#
# Function calls, argument passing and returns
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
called_function: tpm_SymbolExp
{
  // This rule generates code for direct function calls.
  if ( isFunctionType( *$1->getExp() ) )
    $cost[0] =
      $cost[1] + TC13::OperationFormat::L.getSize() +
      TC13::OperationFormat::SDD_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "called_function: tpm_SymbolExp", $1 );

  auto &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  // LLIR
  string label = sym.getName();
  TCINSTRUCTIONS.insertCALL( label, &lhsRegs, $1->getExp() );
  LLIR_Instruction *insCall = TCCODESEL->getCurrentInstruction();

  // aiT expects that CRL2 basic blocks end with a CALL instruction, since it
  // is unknown if the called function will ever return. An example are the
  // paired C library functions setjmp/longjmp, where the control flow from the
  // longjmp returns to the point of the dynamically enclosing setjmp() call
  // (see Muchnick p.175). Thus, we start a new basic block after a call.
  TCCODESEL->beginNewLLIRBasicBlock();

  // Allocate new register to keep the function call result.
  LLIR_Register *reg = nullptr;
  LLIR_Register *reg1 = nullptr;

  if ( returnBehaviour == RegType::DATA_REGISTER ) {
    reg = TCINSTRUCTIONS.CreateRegister( "" );
    reg1 = TCINSTRUCTIONS.CreateRegister( PHREG_D2 );
    TCINSTRUCTIONS.insertMOV( reg, reg1, $1->getExp() );
  } else
  if ( returnBehaviour == RegType::ADDRESS_REGISTER ) {
    reg = TCINSTRUCTIONS.CreateRegister( "", true );
    reg1 = TCINSTRUCTIONS.CreateRegister( PHREG_A2, true );
    TCINSTRUCTIONS.insertMOV_AA( reg, reg1, $1->getExp() );
  } else
  if ( returnBehaviour == RegType::EXTENDED_REGISTER ) {
    reg = TCINSTRUCTIONS.CreateERegister( "" );
    reg1 = TCINSTRUCTIONS.CreateERegister( PHREG_E2 );
    TCINSTRUCTIONS.insertMOV( reg, reg1, $1->getExp() );
  }

  // Register the result register as an implicit operand of the CALL.
  if ( returnBehaviour != RegType::NO_REGISTER ) {
    LLIR_Operation *opCall = insCall->GetFirstOp();
    LLIR_Parameter *implicitParam = new LLIR_Parameter( reg1, USAGE_DEF, 1 );
    opCall->AddParameter( implicitParam );
  }

  // WIR
  TCINSTRUCTIONS.insertCALL(
    dynamic_cast<IR_FunctionSymbol &>( sym ), args, $1->getExp() );
  WIR_Operation &call = TC179x_wirBB->rbegin()->get().begin()->get();

  // aiT expects that CRL2 basic blocks end with a CALL instruction, since it
  // is unknown if the called function will ever return. An example are the
  // paired C library functions setjmp/longjmp, where the control flow from the
  // longjmp returns to the point of the dynamically enclosing setjmp() call
  // (see Muchnick p.175). Thus, we start a new basic block after a call.
  TCCODESEL->startNewBasicBlock();

  if ( returnBehaviour == RegType::DATA_REGISTER ) {
    auto &r = TCINSTRUCTIONS.createDReg();
    auto &d2 = TCINSTRUCTIONS.createDReg();
    TC179x_wirFct->insertPrecolor( d2, TC179x_wirProc->D2() );

    TCINSTRUCTIONS.insertMOV( r, d2, $1->getExp() );
    call.pushBackParameter( WIR_RegisterParameter( d2, WIR_Usage::def, true ) );
    return( make_pair( reg, &r ) );
  } else

  if ( returnBehaviour == RegType::ADDRESS_REGISTER ) {
    auto &r = TCINSTRUCTIONS.createAReg();
    auto &a2 = TCINSTRUCTIONS.createAReg();
    TC179x_wirFct->insertPrecolor( a2, TC179x_wirProc->A2() );

    TCINSTRUCTIONS.insertMOV_AA( r, a2, $1->getExp() );
    call.pushBackParameter( WIR_RegisterParameter( a2, WIR_Usage::def, true ) );
    return( make_pair( reg, &r ) );
  } else

  if ( returnBehaviour == RegType::EXTENDED_REGISTER ) {
    auto &r = TCINSTRUCTIONS.createEReg();
    auto &e2 = TCINSTRUCTIONS.createEReg();
    TC179x_wirFct->insertPrecolor( e2, TC179x_wirProc->E2() );

    TCINSTRUCTIONS.insertMOV( r, e2, $1->getExp() );
    call.pushBackParameter( WIR_RegisterParameter( e2, WIR_Usage::def, true ) );
    return( make_pair( reg, &r ) );
  }

  return( make_pair( reg, (WIR_BaseRegister *) nullptr ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
called_function: areg
{
  // This rule generates code for indirect function calls.
  if ( isFunctionPointer( *$1->getExp() ) || isFunctionType( *$1->getExp() ) )
    $cost[0] =
      $cost[1] + TC13::OperationFormat::A.getSize() +
      TC13::OperationFormat::SDD_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "called_function: areg", $1 );

  auto p = $action[1]();

  // LLIR
  TCINSTRUCTIONS.insertCALLI( p.first, &lhsRegs, $1->getExp() );
  LLIR_Instruction *insCall = TCCODESEL->getCurrentInstruction();

  // aiT expects that CRL2 basic blocks end with a CALL instruction, since it
  // is unknown if the called function will ever return. An example are the
  // paired C library functions setjmp/longjmp, where the control flow from the
  // longjmp returns to the point of the dynamically enclosing setjmp() call
  // (see Muchnick p.175). Thus, we start a new basic block after a call.
  TCCODESEL->beginNewLLIRBasicBlock();

  // Allocate new register to keep the function call result.
  LLIR_Register *reg = nullptr;
  LLIR_Register *regTmp = nullptr;

  if ( returnBehaviour == RegType::DATA_REGISTER ) {
    reg = TCINSTRUCTIONS.CreateRegister( "" );
    regTmp = TCINSTRUCTIONS.CreateRegister( PHREG_D2 );
    TCINSTRUCTIONS.insertMOV( reg, regTmp, $1->getExp() );
  } else
  if ( returnBehaviour == RegType::ADDRESS_REGISTER ) {
    reg = TCINSTRUCTIONS.CreateRegister( "", true );
    regTmp = TCINSTRUCTIONS.CreateRegister( PHREG_A2, true );
    TCINSTRUCTIONS.insertMOV_AA( reg, regTmp, $1->getExp() );
  } else
  if ( returnBehaviour == RegType::EXTENDED_REGISTER ) {
    reg = TCINSTRUCTIONS.CreateERegister( "" );
    regTmp = TCINSTRUCTIONS.CreateERegister( PHREG_E2 );
    TCINSTRUCTIONS.insertMOV( reg, regTmp, $1->getExp() );
  }

  // Add the result register as an implicit operand of the CALL.
  if ( returnBehaviour != RegType::NO_REGISTER ) {
    LLIR_Operation *opCall = insCall->GetFirstOp();
    LLIR_Parameter *implicitParam = new LLIR_Parameter( regTmp, USAGE_DEF, 1 );
    opCall->AddParameter( implicitParam );
  }

  // WIR
  TCINSTRUCTIONS.insertCALLI( p.second, args, $1->getExp() );
  WIR_Operation &call = TC179x_wirBB->rbegin()->get().begin()->get();

  // aiT expects that CRL2 basic blocks end with a CALL instruction, since it
  // is unknown if the called function will ever return. An example are the
  // paired C library functions setjmp/longjmp, where the control flow from the
  // longjmp returns to the point of the dynamically enclosing setjmp() call
  // (see Muchnick p.175). Thus, we start a new basic block after a call.
  TCCODESEL->startNewBasicBlock();

  if ( returnBehaviour == RegType::DATA_REGISTER ) {
    auto &r = TCINSTRUCTIONS.createDReg();
    auto &d2 = TCINSTRUCTIONS.createDReg();
    TC179x_wirFct->insertPrecolor( d2, TC179x_wirProc->D2() );

    TCINSTRUCTIONS.insertMOV( r, d2, $1->getExp() );
    call.pushBackParameter( WIR_RegisterParameter( d2, WIR_Usage::def, true ) );
    return( make_pair( reg, &r ) );
  } else

  if ( returnBehaviour == RegType::ADDRESS_REGISTER ) {
    auto &r = TCINSTRUCTIONS.createAReg();
    auto &a2 = TCINSTRUCTIONS.createAReg();
    TC179x_wirFct->insertPrecolor( a2, TC179x_wirProc->A2() );

    TCINSTRUCTIONS.insertMOV_AA( r, a2, $1->getExp() );
    call.pushBackParameter( WIR_RegisterParameter( a2, WIR_Usage::def, true ) );
    return( make_pair( reg, &r ) );
  } else

  if ( returnBehaviour == RegType::EXTENDED_REGISTER ) {
    auto &r = TCINSTRUCTIONS.createEReg();
    auto &e2 = TCINSTRUCTIONS.createEReg();
    TC179x_wirFct->insertPrecolor( e2, TC179x_wirProc->E2() );

    TCINSTRUCTIONS.insertMOV( r, e2, $1->getExp() );
    call.pushBackParameter( WIR_RegisterParameter( e2, WIR_Usage::def, true ) );
    return( make_pair( reg, &r ) );
  }

  return( make_pair( reg, (WIR_BaseRegister *) nullptr ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: tpm_CallExp( called_function, arg )
{
  if ( isDoubleType( *$0->getExp() ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_CallExp( called_function, arg )", $1 );

  auto *theCall = dynamic_cast<IR_CallExp *>( $1->getExp() );

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

  auto p = $action[2]( lhsRegs, args, RegType::EXTENDED_REGISTER );

  return(
    make_pair( p.first, ref( dynamic_cast<TC_ERegV &>( *(p.second) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_CallExp( called_function, arg )
{
  if ( isDRegType( *$0->getExp() ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_CallExp( called_function, arg )", $1 );

  auto *theCall = dynamic_cast<IR_CallExp *>( $1->getExp() );

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

  auto p =
    ( $0->getExp()->getType().getType() == IR_Type::VOID ) ?
    $action[2]( lhsRegs, args, RegType::NO_REGISTER ) :
    $action[2]( lhsRegs, args, RegType::DATA_REGISTER );

  if ( $0->getExp()->getType().getType() == IR_Type::VOID )
    return( make_pair( p.first, ref( dummyDRegV ) ) );
  else
    return(
      make_pair( p.first, ref( dynamic_cast<TC_DRegV &>( *(p.second) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_CallExp( called_function, arg )
{
  if ( isARegType( *$0->getExp() ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_CallExp( called_function, arg )", $1 );

  auto *theCall = dynamic_cast<IR_CallExp *>( $1->getExp() );

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

  auto p = $action[2]( lhsRegs, args, RegType::ADDRESS_REGISTER );

  return(
    make_pair( p.first, ref( dynamic_cast<TC_ARegV &>( *(p.second) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
arg: tpm_CallExpARG( ereg, arg )
{
  // Functions in prototype form are not supported yet, because in that case,
  // the TriCore ABI forces us to derive a proper function type from the actual
  // parameters, which possibly means from the actual parameters in all places
  // where the function is called. This is not done yet.
  auto *cexp = dynamic_cast<IR_CallExp *>( $2->getExp()->getParent() );

  if ( cexp->getFunctionType().isPrototypeForm() )
    $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::SDD_1.getSize();
  else {
    throw ufFatalError( "Functions without prototype are not supported." );
    $cost[0] = COST_INFINITY;
  }
}
=
{
  DEBUG_RULE_ACTION( "arg: tpm_CallExpARG( ereg, arg )", $1 );

  int incr = 0;

  if ( dryrun ) {

    auto p = $action[2]();

    rhsRegs->push_back( p.first );
    dryArgs.push_back( p.second.get() );

  } else {

    int regNo =
      Stack::isPassedThroughRegister( theCall->getFunctionType(), index );

    // LLIR
    LLIR_Register *rhsreg = rhsRegs->front();
    rhsRegs->pop_front();

    if ( regNo ) {
      LLIR_Register *lhsreg =
        &getFunctionArgumentRegister( *theCall, index, regNo );
      TCINSTRUCTIONS.insertMOV( lhsreg, rhsreg, $1->getExp() );

      if ( lhsRegs != nullptr )
        lhsRegs->push_back( lhsreg );
    } else {
      // Pass argument via the stack.
      incr = Stack::getStackSize( &effectiveType( *$2->getExp() ) );

      LLIR_Register *sp = TCINSTRUCTIONS.CreateRegister( PHREG_SP );
      TCINSTRUCTIONS.insertST_D( OPER_BASE, sp, offset, rhsreg, $1->getExp() );
      TCCODESEL->getLastLLIRBB()->GetLastIns()->AddPragma(
        new LLIR_Pragma( "Passing overflow function parameter", true ) );
    }

    // WIR
    auto &r = dynamic_cast<TC_ERegV &>( dryArgs.front().get() );
    dryArgs.pop_front();

    if ( regNo ) {
      // Pass argument via registers.
      auto &phreg =
        dynamic_cast<TC_ERegV &>( getFctArgReg( *theCall, index, regNo ) );
      TCINSTRUCTIONS.insertMOV( phreg, r, $1->getExp() );

      args.push_back( phreg );
    } else {
      // Pass argument via stack.
      incr = Stack::getStackSize( &effectiveType( *$2->getExp() ) );

      auto &sp = TCINSTRUCTIONS.createAReg();
      TC179x_wirFct->insertPrecolor( sp, TC179x_wirProc->SP() );
      TCINSTRUCTIONS.insertST_D( sp, offset, r, $1->getExp() );

      // Mark access to overflow region for register allocator.
      TC179x_wirBB->rbegin()->get().begin()->get().begin()->
        get().setDontOptimize();
    }
  }

  $action[3](
    dryrun, index + 1, offset + incr, theCall, lhsRegs, rhsRegs, args,
    dryArgs );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
arg: tpm_CallExpARG( dreg, arg )
{
  // Functions in prototype form are not supported yet, because in that case,
  // the TriCore ABI forces us to derive a proper function type from the actual
  // parameters, which possibly means from the actual parameters in all places
  // where the function is called. This is not done yet.
  auto *cexp = dynamic_cast<IR_CallExp *>( $2->getExp()->getParent() );

  if ( cexp->getFunctionType().isPrototypeForm() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::SDD_1.getSize();
  else {
    throw ufFatalError( "Functions without prototype are not supported." );
    $cost[0] = COST_INFINITY;
  }
}
=
{
  DEBUG_RULE_ACTION( "arg: tpm_CallExpARG( dreg, arg )", $1 );

  int incr = 0;

  if ( dryrun ) {

    // If the type is integral and smaller than 32 bits, we would need to
    // convert to the respective signed/unsigned 32-bit integer type before
    // actually passing the argument. Fortunately, our register representation
    // of such types (char / short) is always sign-extending those types to 32
    // bits so that we don't need to cast here.

    auto p = $action[2]();

    rhsRegs->push_back( p.first );
    dryArgs.push_back( p.second.get() );

  } else {

    int regNo =
      Stack::isPassedThroughRegister( theCall->getFunctionType(), index );

    // LLIR
    LLIR_Register *rhsreg = rhsRegs->front();
    rhsRegs->pop_front();

    if ( regNo ) {
      LLIR_Register *lhsreg =
        &getFunctionArgumentRegister( *theCall, index, regNo );
      TCINSTRUCTIONS.insertMOV( lhsreg, rhsreg, $1->getExp() );

      if ( lhsRegs != nullptr )
        lhsRegs->push_back( lhsreg );
    } else {
      // Pass argument via the stack.
      incr = intBytes;

      LLIR_Register *sp = TCINSTRUCTIONS.CreateRegister( PHREG_SP );
      TCINSTRUCTIONS.insertST_W( OPER_BASE, sp, offset, rhsreg, $1->getExp() );
      TCCODESEL->getLastLLIRBB()->GetLastIns()->AddPragma(
        new LLIR_Pragma( "Passing overflow function parameter", true ) );
    }

    // WIR
    auto &r = dynamic_cast<TC_DRegV &>( dryArgs.front().get() );
    dryArgs.pop_front();

    if ( regNo ) {
      // Pass argument via registers.
      auto &phreg =
        dynamic_cast<TC_DRegV &>( getFctArgReg( *theCall, index, regNo ) );
      TCINSTRUCTIONS.insertMOV( phreg, r, $1->getExp() );

      args.push_back( phreg );
    } else {
      // Pass argument via stack.
      incr = intBytes;

      auto &sp = TCINSTRUCTIONS.createAReg();
      TC179x_wirFct->insertPrecolor( sp, TC179x_wirProc->SP() );
      TCINSTRUCTIONS.insertST_W( sp, offset, r, $1->getExp() );

      // Mark access to overflow region for register allocator.
      TC179x_wirBB->rbegin()->get().begin()->get().begin()->
        get().setDontOptimize();
    }
  }

  $action[3](
    dryrun, index + 1, offset + incr, theCall, lhsRegs, rhsRegs, args,
    dryArgs );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
arg: tpm_CallExpARG( areg, arg )
{
  // The argument promotions must be present in the form of implicit casts, so
  // that there is no need to treat them here.
  // Functions in prototype form are not supported yet, because in that case,
  // the TriCore ABI forces us to derive a proper function type from the actual
  // parameters, which possibly means from the actual parameters in all places
  // where the function is called. This is not done yet.
  auto *cexp = dynamic_cast<IR_CallExp *>( $2->getExp()->getParent() );

  if ( cexp->getFunctionType().isPrototypeForm() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::SAA_1.getSize();
  else {
    throw ufFatalError( "Functions without prototype are not supported." );
    $cost[0] = COST_INFINITY;
  }
}
=
{
  DEBUG_RULE_ACTION( "arg: tpm_CallExpARG( areg, arg )", $1 );

  int incr = 0;

  if ( dryrun ) {

    auto p = $action[2]();

    rhsRegs->push_back( p.first );
    dryArgs.push_back( p.second.get() );

  } else {

    int regNo =
      Stack::isPassedThroughRegister( theCall->getFunctionType(), index );

    // LLIR
    LLIR_Register *rhsreg = rhsRegs->front();
    rhsRegs->pop_front();

    if ( regNo ) {
      LLIR_Register *lhsreg =
        &getFunctionArgumentRegister( *theCall, index, regNo );
      TCINSTRUCTIONS.insertMOV_AA( lhsreg, rhsreg, $1->getExp() );

      if ( lhsRegs != nullptr )
        lhsRegs->push_back( lhsreg );
    } else {
      // Pass argument via the stack.
      incr = Stack::getStackSize( &effectiveType( *$2->getExp() ) );

      LLIR_Register *sp = TCINSTRUCTIONS.CreateRegister( PHREG_SP );
      TCINSTRUCTIONS.insertST_A( OPER_BASE, sp, offset, rhsreg, $1->getExp() );
      TCCODESEL->getLastLLIRBB()->GetLastIns()->AddPragma(
        new LLIR_Pragma( "Passing overflow function parameter", true ) );
    }

    // WIR
    auto &r = dynamic_cast<TC_ARegV &>( dryArgs.front().get() );
    dryArgs.pop_front();

    if ( regNo ) {
      // Pass argument via registers.
      auto &phreg =
        dynamic_cast<TC_ARegV &>( getFctArgReg( *theCall, index, regNo ) );
      TCINSTRUCTIONS.insertMOV_AA( phreg, r, $1->getExp() );

      args.push_back( phreg );
    } else {
      // Pass argument via stack.
      incr = Stack::getStackSize( &effectiveType( *$2->getExp() ) );

      auto &sp = TCINSTRUCTIONS.createAReg();
      TC179x_wirFct->insertPrecolor( sp, TC179x_wirProc->SP() );
      TCINSTRUCTIONS.insertST_A( sp, offset, r, $1->getExp() );

      // Mark access to overflow region for register allocator.
      TC179x_wirBB->rbegin()->get().begin()->get().begin()->
        get().setDontOptimize();
    }
  }

  $action[3](
    dryrun, index + 1, offset + incr, theCall, lhsRegs, rhsRegs, args,
    dryArgs );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
arg: tpm_CallExpNOARG
{
  $cost[0] = 0;
}
=
{
  DEBUG_RULE_ACTION( "arg: tpm_CallExpNOARG", $1 );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ReturnStmtVOID
{
  $cost[0] = TC13::OperationFormat::S.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ReturnStmtVOID", $1 );

  // LLIR
  TCINSTRUCTIONS.insertRET( $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertRETURN( $1->getExp() );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ReturnStmt( dreg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::S.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ReturnStmt( dreg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  bindToPHREG( *reg, 2 );

  // Copy result to D2.
  TCINSTRUCTIONS.insertMOV( reg, p.first, $1->getExp() );
  TCINSTRUCTIONS.insertRET( reg, $1->getExp(), InstructionFactory::RETURN_STMT );

  // WIR
  auto &d2 = TCINSTRUCTIONS.createDReg();
  TC179x_wirFct->insertPrecolor( d2, TC179x_wirProc->D2() );
  TCINSTRUCTIONS.insertMOV( d2, p.second, $1->getExp() );
  TCINSTRUCTIONS.insertRET( d2, $1->getExp(), InstructionFactory::RETURN_STMT );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ReturnStmt( areg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::SAA_1.getSize() +
    TC13::OperationFormat::S.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ReturnStmt( areg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  bindToPHREG( *reg, 2 );

  // Copy result to A2
  TCINSTRUCTIONS.insertMOV_AA( reg, p.first, $1->getExp() );
  TCINSTRUCTIONS.insertRET( reg, $1->getExp(), InstructionFactory::RETURN_STMT );

  // WIR
  auto &a2 = TCINSTRUCTIONS.createAReg();
  TC179x_wirFct->insertPrecolor( a2, TC179x_wirProc->A2() );
  TCINSTRUCTIONS.insertMOV_AA( a2, p.second, $1->getExp() );
  TCINSTRUCTIONS.insertRET( a2, $1->getExp(), InstructionFactory::RETURN_STMT );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ReturnStmt( ereg )
{
  $cost[0] =
    $cost[2] + 2 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::S.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ReturnStmt( ereg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  bindToPHREG( *reg, 2 );

  // Copy result to E2.
  TCINSTRUCTIONS.insertMOV( reg, p.first, $1->getExp() );
  TCINSTRUCTIONS.insertRET( reg, $1->getExp(), InstructionFactory::RETURN_STMT );

  // WIR
  auto &e2 = TCINSTRUCTIONS.createEReg();
  TC179x_wirFct->insertPrecolor( e2, TC179x_wirProc->E2() );
  TCINSTRUCTIONS.insertMOV( e2, p.second, $1->getExp() );
  TCINSTRUCTIONS.insertRET( e2, $1->getExp(), InstructionFactory::RETURN_STMT );
};


##############################################################################
#
#
# Expressions for Pointer Handling
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpADDR( deref_ereg )
{
  if ( !isZeroOpADDR( *$1->getExp() ) )
    $cost[0] = $cost[2] + TC13::OperationFormat::AAC16BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( deref_ereg )", $1 );

  auto lvalue = $action[2]( false );
  lvalue.convertToBaseOffsetForm( $1->getExp() );

  // LLIR
  auto *reg =
    &loadAccessLocationToAReg(
      lvalue.getAddress().getARegister(), lvalue.getOffset(), "",
      $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  auto &aReg = dynamic_cast<TC_ARegV &>( lvalue.getAddress().getAReg() );

  if ( lvalue.getOffset() == 0 )
    TCINSTRUCTIONS.insertMOV_AA( r, aReg, $1->getExp() );
  else
    TCINSTRUCTIONS.insertLEA( r, aReg, lvalue.getOffset(), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpADDR( deref_dreg )
{
  if ( !isZeroOpADDR( *$1->getExp() ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( deref_dreg )", $1 );

  auto lvalue = $action[2]( false );
  lvalue.convertToBaseOffsetForm( $1->getExp() );

  // LLIR
  auto *reg =
    &loadAccessLocationToAReg(
      lvalue.getAddress().getARegister(), lvalue.getOffset(), "",
      $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();

  if ( lvalue.getOffset() == 0 )
    TCINSTRUCTIONS.insertMOV_AA(
      r, dynamic_cast<TC_ARegV &>( lvalue.getAddress().getAReg() ),
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertLEA(
      r, dynamic_cast<TC_ARegV &>( lvalue.getAddress().getAReg() ),
      lvalue.getOffset(), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpADDR( deref_areg )
{
  if ( !isZeroOpADDR( *$1->getExp() ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( deref_areg )", $1 );

  auto lvalue = $action[2]( false );
  lvalue.convertToBaseOffsetForm( $1->getExp() );

  // LLIR
  auto *reg =
    &loadAccessLocationToAReg(
      lvalue.getAddress().getARegister(), lvalue.getOffset(), "",
      $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();

  if ( lvalue.getOffset() == 0 )
    TCINSTRUCTIONS.insertMOV_AA(
      r, dynamic_cast<TC_ARegV &>( lvalue.getAddress().getAReg() ),
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertLEA(
      r, dynamic_cast<TC_ARegV &>( lvalue.getAddress().getAReg() ),
      lvalue.getOffset(), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpADDR( areg )
{
  // For address operations on function pointers and arrays
  if ( isArrayType( *$2->getExp() ) || isFunctionType( *$2->getExp() ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( areg )", $1 );

  // For function pointers and arrays, the symbol expression directly loads the
  // respective address, because the symbol might also be used without the ADDR
  // operator. So we don't have to load the address here.
  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_SymbolExp
{
  // Handles non-stack, non-global 'areg' symbols.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( ( isPointerType( sym ) ||
         ( isArrayType( sym ) && isFunctionArgument( symExp ) ) ) &&
       !sym.isGlobal() && ( TCCODESEL->getStack()->getSymbolOffset( &sym ) < 0 ) )
    $cost[0] = loadRegisterSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );

  // LLIR
  LLIR_Register *reg = loadRegisterSymbol( *symExp );

  // WIR
  auto &r = dynamic_cast<TC_ARegV &>( loadRegSym( *symExp ) );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_areg: tpm_SymbolExp
{
  // Handles local stack 'areg' symbols.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( ( isPointerType( sym ) ||
         ( isArrayType( sym ) && isFunctionArgument( symExp ) ) )  &&
       !sym.isGlobal() &&
       ( TCCODESEL->getStack()->getSymbolOffset( &sym ) >= 0 ) )
    $cost[0] = loadStackSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_areg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  return( loadStackSymbol( *symExp, loadResult ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_areg: tpm_SymbolExp
{
  // Handles global 'areg' symbols.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( isPointerType( sym ) && sym.isGlobal() )
    $cost[0] = loadGlobalSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_areg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  return( loadGlobalSymbol( *symExp, loadResult ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_SymbolExp
{
  // Handles function symbols - always load (global) symbols address
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( isFunctionType( sym ) )
    $cost[0] = loadGlobalSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_SymbolExp", $1 );

  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto lvalue = loadGlobalSymbol( symExp, true );

  return(
    make_pair(
      lvalue.getResultRegister(),
      ref( dynamic_cast<TC_ARegV &>( *(lvalue.getResultReg()) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_SymbolExp
{
  // This rule handles "real" arrays (not function arguments). Those arrays do
  // not need to be loaded with LD_X, only their address must be loaded into a
  // register.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( isArrayType( sym ) && !isFunctionArgument( symExp ) ) {
    $cost[0] = 0;

    if ( sym.isGlobal() )
      $cost[0] += loadGlobalSymbolCost( symExp );
    else

    if ( TCCODESEL->getStack()->getSymbolOffset( &sym ) >= 0 )
      $cost[0] += loadStackSymbolCost( symExp );
    else
      $cost[0] += loadRegisterSymbolCost( symExp );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_SymbolExp", $1 );

  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( sym.isGlobal() ) {
    auto lvalue = loadGlobalSymbol( symExp, true );
    return(
      make_pair(
        lvalue.getResultRegister(),
        ref( dynamic_cast<TC_ARegV &>( *(lvalue.getResultReg()) ) ) ) );
  } else

  if ( TCCODESEL->getStack()->getSymbolOffset( &sym ) >= 0 ) {
    auto lvalue = loadStackSymbol( symExp, true );
    return(
      make_pair(
        lvalue.getResultRegister(),
        ref( dynamic_cast<TC_ARegV &>( *(lvalue.getResultReg()) ) ) ) );
  } else {
    LLIR_Register *reg = loadRegisterSymbol( symExp );
    return(
      make_pair(
        reg, ref( dynamic_cast<TC_ARegV &>( loadRegSym( symExp ) ) ) ) );
  }
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_dreg: tpm_UnaryExpDEREF( areg )
{
  switch ( getBaseType( *$2->getExp() )->getType() ) {
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::BOOL:
    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT: {
      $cost[0] = $cost[2] + TC13::OperationFormat::DAC10BOA.getSize();
      break;
    }

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::FLOAT: {
      $cost[0] = $cost[2] + TC13::OperationFormat::DAC16BOA.getSize();
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
  DEBUG_RULE_ACTION( "deref_dreg: tpm_UnaryExpDEREF( areg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", false );

  if ( loadResult ) {
    switch ( getBaseType( *$2->getExp() )->getType() ) {
      case IR_Type::CHAR: {
        TCINSTRUCTIONS.insertLD_B( reg, OPER_BASE, p.first, 0, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        TCINSTRUCTIONS.insertLD_BU( reg, OPER_BASE, p.first, 0, $1->getExp() );
        break;
      }

      case IR_Type::SHORT: {
        TCINSTRUCTIONS.insertLD_H( reg, OPER_BASE, p.first, 0, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_SHORT: {
        TCINSTRUCTIONS.insertLD_HU( reg, OPER_BASE, p.first, 0, $1->getExp() );
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: {
        TCINSTRUCTIONS.insertLD_W( reg, OPER_BASE, p.first, 0, $1->getExp() );
        break;
      }

      default:
        break;
    }
  }

  // WIR
  TC_DRegV *r = nullptr;

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createDReg());

    switch ( getBaseType( *$2->getExp() )->getType() ) {
      case IR_Type::CHAR: {
        TCINSTRUCTIONS.insertLD_B( *r, p.second, 0, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        TCINSTRUCTIONS.insertLD_BU( *r, p.second, 0, $1->getExp() );
        break;
      }

      case IR_Type::SHORT: {
        TCINSTRUCTIONS.insertLD_H( *r, p.second, 0, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_SHORT: {
        TCINSTRUCTIONS.insertLD_HU( *r, p.second, 0, $1->getExp() );
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: {
        TCINSTRUCTIONS.insertLD_W( *r, p.second, 0, $1->getExp() );
        break;
      }

      default:
        break;
    }
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        p.first, p.second, 0, getBaseType( *$2->getExp() ), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_dreg: tpm_UnaryExpDEREF( modified_areg )
{
  // This rule implements a load instruction combined with an address
  // increment / decrement.
  switch ( getBaseType( *$2->getExp() )->getType() ) {
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::BOOL:
    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT: {
      $cost[0] = $cost[2] + TC13::OperationFormat::DAC10BOA.getSize();
      break;
    }

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::FLOAT: {
      $cost[0] = $cost[2] + TC13::OperationFormat::DAC16BOA.getSize();
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
  DEBUG_RULE_ACTION( "deref_dreg: tpm_UnaryExpDEREF( modified_areg )", $1 );

  auto amod = $action[2]();

  LLIR_Register *reg = nullptr;
  TC_DRegV *r = nullptr;

  if ( loadResult ) {
    reg = TCINSTRUCTIONS.CreateRegister( "", false );
    r = &(TCINSTRUCTIONS.createDReg());

    amod.createLoad( reg, r, $1->getExp() );
  }

  return( TC_LValue { reg, r, amod } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_ereg: tpm_UnaryExpDEREF( areg )
{
  if ( isDoubleType( *getBaseType( *$2->getExp() ) ) )
    $cost[0] = $cost[2] + TC13::OperationFormat::EAC10BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_ereg: tpm_UnaryExpDEREF( areg )", $1 );

  auto p = $action[2]();

  // LLIR
  auto *reg = TCINSTRUCTIONS.CreateERegister( "" );

  if ( loadResult )
    TCINSTRUCTIONS.insertLD_D( reg, OPER_BASE, p.first, 0, $1->getExp() );

  // WIR
  TC_ERegV *r = nullptr;

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createEReg());

    TCINSTRUCTIONS.insertLD_D( *r, p.second, 0, $1->getExp() );
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        p.first, p.second, 0, getBaseType( *$2->getExp() ), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_ereg: tpm_UnaryExpDEREF( modified_areg )
{
  // This rule implements a load instruction combined with an address
  // increment / decrement.
  if ( isDoubleType( *getBaseType( *$2->getExp() ) ) )
    $cost[0] = $cost[2] + TC13::OperationFormat::EAC10BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_ereg: tpm_UnaryExpDEREF( modified_areg )", $1 );

  auto amod = $action[2]();

  LLIR_Register *reg = nullptr;
  TC_ERegV *r = nullptr;

  if ( loadResult ) {
    reg = TCINSTRUCTIONS.CreateERegister( "" );
    r = &(TCINSTRUCTIONS.createEReg());

    amod.createLoad( reg, r, $1->getExp() );
  }

  return( TC_LValue { reg, r, amod } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_areg: tpm_UnaryExpDEREF( areg )
{
  if ( isPointerType( *getBaseType( *$2->getExp() ) ) &&
       !isFunctionPointer( *$2->getExp() ) )
    $cost[0] = $cost[2] + TC13::OperationFormat::AAC16BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_areg: tpm_UnaryExpDEREF( areg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );

  if ( loadResult )
    TCINSTRUCTIONS.insertLD_A( reg, OPER_BASE, p.first, 0, $1->getExp() );

  // WIR
  TC_ARegV *r = nullptr;

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createAReg());

    TCINSTRUCTIONS.insertLD_A( *r, p.second, 0, $1->getExp() );
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        p.first, p.second, 0, getBaseType( *$2->getExp() ), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_areg: tpm_UnaryExpDEREF( modified_areg )
{
  // This rule implements a load instruction combined with an address
  // increment / decrement.
  if ( isPointerType( *getBaseType( *$2->getExp() ) ) &&
       !isFunctionPointer( *$2->getExp() ) )
    $cost[0] = $cost[2] + TC13::OperationFormat::AAC16BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_areg: tpm_UnaryExpDEREF( modified_areg )", $1 );

  auto amod = $action[2]();

  LLIR_Register *reg = nullptr;
  TC_ARegV *r = nullptr;

  if ( loadResult ) {
    reg = TCINSTRUCTIONS.CreateRegister( "", true );
    r = &(TCINSTRUCTIONS.createAReg());

    amod.createLoad( reg, r, $1->getExp() );
  }

  return( TC_LValue { reg, r, amod } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpDEREF( areg )
{
  // This rule only handles base type arrays, because in that case, we only
  // compute the new address and return it in an areg.
  if ( isArrayType( *getBaseType( *$2->getExp() ) ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpDEREF( areg )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpDEREF( areg )
{
  // This rules handles function pointers exclusively.
  if ( isFunctionPointer( *$2->getExp() ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpDEREF( areg )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
addrOffset: tpm_UnaryExpPLUS( addrOffset )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "addrOffset: tpm_UnaryExpPLUS( addrOffset )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
addrOffset: tpm_UnaryExpMINUS( addrOffset )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "addrOffset: tpm_UnaryExpMINUS( addrOffset )", $1 );

  return( -$action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_BinaryExpPLUS( areg, addrOffset )
{
  auto *t = getBaseType( *$2->getExp() );
  const int off = getConstIntValue( $3 ) * computeSizeOf( t );

  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AAC16BOA.getSize();

  if ( ( off < TC_Const16_Signed::getMinValue( 16 ) ) ||
       ( off > TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] +=
      TC13::OperationFormat::SAA_1.getSize() +
      TC13::OperationFormat::AAC16.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_BinaryExpPLUS( areg, addrOffset )", $1 );

  auto *t = getBaseType( *$2->getExp() );
  const int off = $action[3]().getIntValue() * computeSizeOf( t );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertLEA( reg, OPER_BASE, p.first, off, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertLEA( r, p.second, off, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_BinaryExpPLUS( areg, dreg )
{
  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] = $cost[2] + $cost[3] + loadRegisterRelativeAddressCost( byteSize );
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_BinaryExpPLUS( areg, dreg )", $1 );

  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg =
    loadRegisterRelativeAddress( p1.first, p2.first, byteSize, $1->getExp() );

  // WIR
  auto &r = loadRegRelativeAddr( p1.second, p2.second, byteSize, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpMINUS( areg, areg )
{
  IR_Type *baseType = getBaseType( *$2->getExp() );
  const int byteSize = computeSizeOf( baseType );

  // Determine wether size of a pointer type is a power of two.
  bool isPowerOfTwo = false;
  long long powerVal = 2;

  for ( int i = 1; i < 32; ++i, powerVal *= 2 )
    if ( byteSize == powerVal ) {
      isPowerOfTwo = true;
      break;
    }

  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::AAA.getSize() +
    TC13::OperationFormat::SDA_1.getSize();

  if ( isPowerOfTwo )
    $cost[0] += TC13::OperationFormat::DDC9_1.getSize();
  else {
    $cost[0] +=
      TC13::OperationFormat::EDD.getSize() +
      5 * TC13::OperationFormat::EED.getSize() +
      TC13::OperationFormat::SDD_1.getSize();

    if ( ( byteSize >= TC_Const4_Signed::getMinValue( 4 ) ) &&
         ( byteSize <= TC_Const4_Signed::getMaxValue( 4 ) ) )
      $cost[0] += TC13::OperationFormat::SDC4_1.getSize();
    else
    if ( ( byteSize >= TC_Const16_Signed::getMinValue( 16 ) ) &&
         ( byteSize <= TC_Const16_Signed::getMaxValue( 16 ) ) )
      $cost[0] += TC13::OperationFormat::DC16_1.getSize();
    else
    if ( ( byteSize >= 0 ) &&
         ( byteSize <= (int) TC_Const16_Unsigned::getMaxValue( 16 ) ) )
      $cost[0] += TC13::OperationFormat::DC16_2.getSize();
    else
      $cost[0] +=
        TC13::OperationFormat::DC16_2.getSize() +
        TC13::OperationFormat::DDC16_1.getSize();
  }
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMINUS( areg, areg )", $1 );

  IR_Type *baseType = getBaseType( *$2->getExp() );
  const int byteSize = computeSizeOf( baseType );

  // Determine whether size of pointer type is a power of two.
  bool isPowerOfTwo = false;
  long long powerVal = 2;
  long power = 0;

  for ( int i = 1; i < 32; ++i, powerVal *= 2 )
    if ( byteSize == powerVal ) {
      isPowerOfTwo = true;
      power = i;
      break;
    }

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *areg = TCINSTRUCTIONS.CreateRegister( "", true );
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  // Generate the operation.
  TCINSTRUCTIONS.insertSUB_A( areg, p1.first, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertMOV_D( reg, areg, $1->getExp() );

  if ( isPowerOfTwo )
    TCINSTRUCTIONS.insertSHA( reg, reg, -power, $1->getExp() );
  else {
    LLIR_Register *tmpReg = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertMOV( tmpReg, byteSize, $1->getExp() );
    TCINSTRUCTIONS.insertDIV_W( reg, tmpReg, reg, $1->getExp() );
  }

  // WIR
  auto &tmpAReg = TCINSTRUCTIONS.createAReg();
  auto &r = TCINSTRUCTIONS.createDReg();

  // Generate the operation.
  TCINSTRUCTIONS.insertSUB_A( tmpAReg, p1.second, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertMOV_D( r, tmpAReg, $1->getExp() );

  if ( isPowerOfTwo )
    TCINSTRUCTIONS.insertSHA( r, r, -power, $1->getExp() );
  else {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();
    TCINSTRUCTIONS.insertMOVConstant( tmpReg, byteSize, $1->getExp() );
    TCINSTRUCTIONS.insertDIV_W( r, r, tmpReg, $1->getExp() );
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_BinaryExpMINUS( areg, addrOffset )
{
  auto *t = getBaseType( *$2->getExp() );
  const int off = -( getConstIntValue( $3 ) * computeSizeOf( t ) );

  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AAC16BOA.getSize();

  if ( ( off < TC_Const16_Signed::getMinValue( 16 ) ) ||
       ( off > TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] +=
      TC13::OperationFormat::SAA_1.getSize() +
      TC13::OperationFormat::AAC16.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_BinaryExpMINUS( areg, addrOffset )", $1 );

  auto *t = getBaseType( *$2->getExp() );
  const int off = -( $action[3]().getIntValue() * computeSizeOf( t ) );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertLEA( reg, OPER_BASE, p.first, off, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertLEA( r, p.second, off, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_BinaryExpMINUS( areg, dreg )
{
  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] = $cost[2] + $cost[3] + loadRegisterRelativeAddressCost( byteSize );
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_BinaryExpMINUS( areg, dreg )", $1 );

  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertRSUB( regTmp, p2.first, 0, $1->getExp() );
  LLIR_Register *reg =
    loadRegisterRelativeAddress( p1.first, regTmp, byteSize, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertRSUB( tmpReg, p2.second, 0, $1->getExp() );
  auto &r = loadRegRelativeAddr( p1.second, tmpReg, byteSize, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpPOSTDEC( areg )
{
  const int off = computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] =
    $cost[2] + TC13::OperationFormat::SAA_1.getSize() +
    TC13::OperationFormat::AAC16BOA.getSize();

  if ( ( off < TC_Const16_Signed::getMinValue( 16 ) ) ||
       ( off > TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] +=
      TC13::OperationFormat::SAA_1.getSize() +
      TC13::OperationFormat::AAC16.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPOSTDEC( areg )", $1 );

  const int off = -computeSizeOf( getBaseType( *$2->getExp() ) );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertMOV_AA( reg, p.first, $1->getExp() );
  TCINSTRUCTIONS.insertLEA( p.first, OPER_BASE, p.first, off, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertMOV_AA( r, p.second, $1->getExp() );
  TCINSTRUCTIONS.insertLEA( p.second, p.second, off, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpPOSTDEC( deref_areg )
{
  const int off = computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] =
    $cost[2] + TC13::OperationFormat::SAA_1.getSize() +
    TC13::OperationFormat::AAC16BOA.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );

  if ( ( off < TC_Const16_Signed::getMinValue( 16 ) ) ||
       ( off > TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] +=
      TC13::OperationFormat::SAA_1.getSize() + TC13::OperationFormat::AAC16.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPOSTDEC( deref_areg )", $1 );

  const int off = -computeSizeOf( getBaseType( *$2->getExp() ) );

  auto lvalue = $action[2]( true );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  LLIR_Register *regTmp = lvalue.getResultRegister();
  TCINSTRUCTIONS.insertMOV_AA( reg, regTmp, $1->getExp() );
  TCINSTRUCTIONS.insertLEA( regTmp, OPER_BASE, regTmp, off, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  auto &tmpReg = dynamic_cast<TC_ARegV &>( *(lvalue.getResultReg() ) );
  TCINSTRUCTIONS.insertMOV_AA( r, tmpReg, $1->getExp() );
  TCINSTRUCTIONS.insertLEA( tmpReg, tmpReg, off, $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_UnaryExpPOSTDEC( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPOSTDEC( areg )", $1 );

  auto p = $action[2]();

  return(
    TC_AddressModification {
      p.first, p.second, 1, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::POST, AddressModification::ModOper::SUB } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_UnaryExpPOSTDEC( deref_areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPOSTDEC( deref_areg )", $1 );

  auto lvalue = $action[2]( true );

  return(
    TC_AddressModification {
      &lvalue, 1, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::POST, AddressModification::ModOper::SUB } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpPOSTINC( areg )
{
  const int off = computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] =
    $cost[2] + TC13::OperationFormat::SAA_1.getSize() +
    TC13::OperationFormat::AAC16BOA.getSize();

  if ( ( off < TC_Const16_Signed::getMinValue( 16 ) ) ||
       ( off > TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] +=
      TC13::OperationFormat::SAA_1.getSize() +
      TC13::OperationFormat::AAC16.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPOSTINC( areg )", $1 );

  const int off = computeSizeOf( getBaseType( *$2->getExp() ) );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertMOV_AA( reg, p.first, $1->getExp() );
  TCINSTRUCTIONS.insertLEA( p.first, OPER_BASE, p.first, off, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertMOV_AA( r, p.second, $1->getExp() );
  TCINSTRUCTIONS.insertLEA( p.second, p.second, off, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpPOSTINC( deref_areg )
{
  const int off = computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] =
    $cost[2] + TC13::OperationFormat::SAA_1.getSize() +
    TC13::OperationFormat::AAC16BOA.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );

  if ( ( off < TC_Const16_Signed::getMinValue( 16 ) ) ||
       ( off > TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] +=
      TC13::OperationFormat::SAA_1.getSize() +
      TC13::OperationFormat::AAC16.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPOSTINC( deref_areg )", $1 );

  const int off = computeSizeOf( getBaseType( *$2->getExp() ) );

  auto lvalue = $action[2]( true );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  LLIR_Register *regTmp = lvalue.getResultRegister();
  TCINSTRUCTIONS.insertMOV_AA( reg, regTmp, $1->getExp() );
  TCINSTRUCTIONS.insertLEA( regTmp, OPER_BASE, regTmp, off, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  auto &tmpReg = dynamic_cast<TC_ARegV &>( *(lvalue.getResultReg()) );
  TCINSTRUCTIONS.insertMOV_AA( r, tmpReg, $1->getExp() );
  TCINSTRUCTIONS.insertLEA( tmpReg, tmpReg, off, $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_UnaryExpPOSTINC( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPOSTINC( areg )", $1 );

  auto p = $action[2]();

  return(
    TC_AddressModification {
      p.first, p.second, 1, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::POST, AddressModification::ModOper::ADD } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_UnaryExpPOSTINC( deref_areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPOSTINC( deref_areg )", $1 );

  auto lvalue = $action[2]( true );

  return(
    TC_AddressModification {
      &lvalue, 1, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::POST, AddressModification::ModOper::ADD } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpPREDEC( areg )
{
  const int off = -computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] = $cost[2] + TC13::OperationFormat::AAC16BOA.getSize();

  if ( ( off < TC_Const16_Signed::getMinValue( 16 ) ) ||
       ( off > TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] +=
      TC13::OperationFormat::SAA_1.getSize() +
      TC13::OperationFormat::AAC16.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPREDEC( areg )", $1 );

  const int off = -computeSizeOf( getBaseType( *$2->getExp() ) );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertLEA( p.first, OPER_BASE, p.first, off, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertLEA( p.second, p.second, off, $1->getExp() );

  return( p );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpPREDEC( deref_areg )
{
  const int off = -computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] =
    $cost[2] + TC13::OperationFormat::AAC16BOA.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );

  if ( ( off < TC_Const16_Signed::getMinValue( 16 ) ) ||
       ( off > TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] +=
      TC13::OperationFormat::SAA_1.getSize() +
      TC13::OperationFormat::AAC16.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPREDEC( deref_areg )", $1 );

  const int off = -computeSizeOf( getBaseType( *$2->getExp() ) );

  auto lvalue = $action[2]( true );

  // LLIR
  LLIR_Register *reg = lvalue.getResultRegister();
  TCINSTRUCTIONS.insertLEA( reg, OPER_BASE, reg, off, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ARegV &>( *(lvalue.getResultReg()) );
  TCINSTRUCTIONS.insertLEA( r, r, off, $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_UnaryExpPREDEC( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPREDEC( areg )", $1 );

  auto p = $action[2]();

  return(
    TC_AddressModification {
      p.first, p.second, 1, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::PRE, AddressModification::ModOper::SUB } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_UnaryExpPREDEC( deref_areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPREDEC( deref_areg )", $1 );

  auto lvalue = $action[2]( true );

  return(
    TC_AddressModification {
      &lvalue, 1, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::PRE, AddressModification::ModOper::SUB } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpPREINC( areg )
{
  const int off = computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] = $cost[2] + TC13::OperationFormat::AAC16BOA.getSize();

  if ( ( off < TC_Const16_Signed::getMinValue( 16 ) ) ||
       ( off > TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] +=
      TC13::OperationFormat::SAA_1.getSize() +
      TC13::OperationFormat::AAC16.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPREINC( areg )", $1 );

  const int off = computeSizeOf( getBaseType( *$2->getExp() ) );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertLEA( p.first, OPER_BASE, p.first, off, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertLEA( p.second, p.second, off, $1->getExp() );

  return( p );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpPREINC( deref_areg )
{
  const int off = computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] =
    $cost[2] + TC13::OperationFormat::AAC16BOA.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );

  if ( ( off < TC_Const16_Signed::getMinValue( 16 ) ) ||
       ( off > TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] +=
      TC13::OperationFormat::SAA_1.getSize() +
      TC13::OperationFormat::AAC16.getSize();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPREINC( deref_areg )", $1 );

  const int off = computeSizeOf( getBaseType( *$2->getExp() ) );

  auto lvalue = $action[2]( true );

  // LLIR
  LLIR_Register *reg = lvalue.getResultRegister();
  TCINSTRUCTIONS.insertLEA( reg, OPER_BASE, reg, off, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ARegV &>( *(lvalue.getResultReg()) );
  TCINSTRUCTIONS.insertLEA( r, r, off, $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: modified_areg
{
  // Chain rule that actually performs pointer arithmetics by transforming a
  // modified_areg into a plain areg.
  $cost[0] = $cost[1] + TC_AddressModification::applyModificationCost();
}
=
{
  DEBUG_RULE_ACTION( "areg: modified_areg", $1 );

  auto p = $action[1]().applyModification( $1->getExp() );
  return( make_pair( p.first, ref( *(p.second) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_UnaryExpPREINC( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPREINC( areg )", $1 );

  auto p = $action[2]();

  return(
    TC_AddressModification {
      p.first, p.second, 1, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::PRE, AddressModification::ModOper::ADD } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_UnaryExpPREINC( deref_areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPREINC( deref_areg )", $1 );

  auto lvalue = $action[2]( true );

  return(
    TC_AddressModification {
      &lvalue, 1, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::PRE, AddressModification::ModOper::ADD } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_AssignExpPLUS( areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_AssignExpPLUS( areg, tpm_IntConstExp )", $1 );

  auto p = $action[2]();

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *>( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  return(
    TC_AddressModification {
      p.first, p.second, offset, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::PRE, AddressModification::ModOper::ADD } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_AssignExpPLUS( deref_areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_AssignExpPLUS( deref_areg, tpm_IntConstExp )", $1 );

  auto lvalue = $action[2]( true );

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *>( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  return(
    TC_AddressModification {
      &lvalue, offset, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::PRE, AddressModification::ModOper::ADD } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_AssignExpMINUS( areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_AssignExpMINUS( areg, tpm_IntConstExp )", $1 );

  auto p = $action[2]();

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *>( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  return(
    TC_AddressModification {
      p.first, p.second, offset, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::PRE, AddressModification::ModOper::SUB } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_AssignExpMINUS( deref_areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_AssignExpMINUS( deref_areg, tpm_IntConstExp )", $1 );

  auto lvalue = $action[2]( true );

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *>( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  return(
    TC_AddressModification {
      &lvalue, offset, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::PRE, AddressModification::ModOper::SUB } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_BinaryExpPLUS( areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_BinaryExpPLUS( areg, tpm_IntConstExp )", $1 );

  auto p = $action[2]();

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *> ( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  return(
    TC_AddressModification {
      p.first, p.second, offset, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::NONE, AddressModification::ModOper::ADD } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_BinaryExpPLUS( deref_areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_BinaryExpPLUS( deref_areg, tpm_IntConstExp )", $1 );

  auto lvalue = $action[2]( true );

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *>( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  return(
    TC_AddressModification {
      &lvalue, offset, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::NONE, AddressModification::ModOper::ADD } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_BinaryExpMINUS( areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_BinaryExpMINUS( areg, tpm_IntConstExp )", $1 );

  auto p = $action[2]();

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *> ( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  return(
    TC_AddressModification {
      p.first, p.second, offset, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::NONE, AddressModification::ModOper::SUB } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_BinaryExpMINUS( deref_areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_BinaryExpMINUS( deref_areg, tpm_IntConstExp )", $1 );

  auto lvalue = $action[2]( true );

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *>( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  return(
    TC_AddressModification {
      &lvalue, offset, getBaseType( *$2->getExp() ), false,
      AddressModification::ModTime::NONE, AddressModification::ModOper::SUB } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpLOGNOT( areg )
{
  $cost[0] = $cost[2] + TC13::OperationFormat::DA.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpLOGNOT( areg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQZ_A( reg, p.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQZ_A( r, p.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


##############################################################################
#
#
# Expressions for Array Handling
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_ereg: tpm_IndexExp( areg, addrOffset )
{
  if ( isDoubleType( *getBaseType( *$2->getExp() ) ) )
    $cost[0] = $cost[2] + TC13::OperationFormat::EAC10BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_ereg: tpm_IndexExp( areg, addrOffset )", $1 );

  auto p = $action[2]();
  int off = $action[3]().getIntValue() * doubleBytes;

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  if ( loadResult )
    TCINSTRUCTIONS.insertLD_D( reg, OPER_BASE, p.first, off, $1->getExp() );

  // WIR
  TC_ERegV *r = nullptr;

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createEReg());

    TCINSTRUCTIONS.insertLD_D( *r, p.second, off, $1->getExp() );
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        p.first, p.second, off, getBaseType( *$2->getExp() ), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_dreg: tpm_IndexExp( areg, addrOffset )
{
  $cost[0] = $cost[2];

  switch ( getBaseType( *$2->getExp() )->getType() ) {
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::BOOL:
    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT: {
      $cost[0] += TC13::OperationFormat::DAC10BOA.getSize();
      break;
    }

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::FLOAT: {
      $cost[0] += TC13::OperationFormat::DAC16BOA.getSize();
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
  DEBUG_RULE_ACTION( "deref_dreg: tpm_IndexExp( areg, addrOffset )", $1 );

  auto *baseType = getBaseType( *$2->getExp() );

  auto p = $action[2]();
  int off = $action[3]().getIntValue() * computeSizeOf( baseType );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", false );
  if ( loadResult )
    switch ( baseType->getType() ) {
      case IR_Type::CHAR: {
        TCINSTRUCTIONS.insertLD_B( reg, OPER_BASE, p.first, off, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        TCINSTRUCTIONS.insertLD_BU( reg, OPER_BASE, p.first, off, $1->getExp() );
        break;
      }

      case IR_Type::SHORT: {
        TCINSTRUCTIONS.insertLD_H( reg, OPER_BASE, p.first, off, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_SHORT: {
        TCINSTRUCTIONS.insertLD_HU( reg, OPER_BASE, p.first, off, $1->getExp() );
        break;
      }

      default: {
        TCINSTRUCTIONS.insertLD_W( reg, OPER_BASE, p.first, off, $1->getExp() );
        break;
      }
    }

  // WIR
  TC_DRegV *r = nullptr;

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createDReg());

    switch ( baseType->getType() ) {
      case IR_Type::CHAR: {
        TCINSTRUCTIONS.insertLD_B( *r, p.second, off, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        TCINSTRUCTIONS.insertLD_BU( *r, p.second, off, $1->getExp() );
        break;
      }

      case IR_Type::SHORT: {
        TCINSTRUCTIONS.insertLD_H( *r, p.second, off, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_SHORT: {
        TCINSTRUCTIONS.insertLD_HU( *r, p.second, off, $1->getExp() );
        break;
      }

      default: {
        TCINSTRUCTIONS.insertLD_W( *r, p.second, off, $1->getExp() );
        break;
      }
    }
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification { p.first, p.second, off, baseType, true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_areg: tpm_IndexExp( areg, addrOffset )
{
  // This rule only handles base type pointers. Base type arrays must be treated
  // differently, because in that case, we only compute the new address and
  // return it in an areg (no LD_A or deref_areg).
  if ( isPointerType( *getBaseType( *$2->getExp() ) ) )
    $cost[0] = $cost[2] + TC13::OperationFormat::AAC16BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_areg: tpm_IndexExp( areg, addrOffset )", $1 );

  auto p = $action[2]();
  int off = $action[3]().getIntValue() * pointerBytes;

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  if ( loadResult )
    TCINSTRUCTIONS.insertLD_A( reg, OPER_BASE, p.first, off, $1->getExp() );

  // WIR
  TC_ARegV *r = nullptr;

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createAReg());

    TCINSTRUCTIONS.insertLD_A( *r, p.second, off, $1->getExp() );
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        p.first, p.second, off, getBaseType( *$2->getExp() ), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_IndexExp( areg, addrOffset )
{
  auto *t = getBaseType( *$2->getExp() );

  // This rule only handles base type arrays, because in that case, we only
  // compute the new address and return it in an areg.
  if ( isArrayType( *t ) ) {
    $cost[0] = $cost[2];

    const int off = getConstIntValue( $3 ) * computeSizeOf( t );

    if ( off != 0 ) {
      $cost[0] += TC13::OperationFormat::AAC16BOA.getSize();

      if ( ( off < TC_Const16_Signed::getMinValue( 16 ) ) ||
           ( off > TC_Const16_Signed::getMaxValue( 16 ) ) )
        $cost[0] +=
          TC13::OperationFormat::SAA_1.getSize() +
          TC13::OperationFormat::AAC16.getSize();
    }
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_IndexExp( areg, addrOffset )", $1 );

  auto p = $action[2]();

  const int off =
    $action[3]().getIntValue() * computeSizeOf( getBaseType( *$2->getExp() ) );

  if ( off == 0 )
    return( p );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertLEA( reg, OPER_BASE, p.first, off, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertLEA( r, p.second, off, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_ereg: tpm_IndexExp( areg, dreg )
{
  if ( isDoubleType( *getBaseType( *$2->getExp() ) ) )
    $cost[0] =
      $cost[2] + $cost[3] + loadRegisterRelativeAddressCost( doubleBytes ) +
      TC13::OperationFormat::EAC10BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_ereg: tpm_IndexExp( areg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *regTmp =
    loadRegisterRelativeAddress(
      p1.first, p2.first, doubleBytes, $1->getExp() );
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  if ( loadResult )
    TCINSTRUCTIONS.insertLD_D( reg, OPER_BASE, regTmp, 0, $1->getExp() );

  // WIR
  auto &tmpReg =
    loadRegRelativeAddr( p1.second, p2.second, doubleBytes, $1->getExp() );
  TC_ERegV *r = nullptr;

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createEReg());

    TCINSTRUCTIONS.insertLD_D( *r, tmpReg, 0, $1->getExp() );
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        regTmp, tmpReg, 0, getBaseType( *$2->getExp() ), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_dreg: tpm_IndexExp( areg, dreg )
{
  auto *baseType = getBaseType( *$2->getExp() );
  const int byteSize = computeSizeOf( baseType );

  $cost[0] = $cost[2] + $cost[3] + loadRegisterRelativeAddressCost( byteSize );

  switch ( baseType->getType() ) {
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::BOOL:
    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT: {
      $cost[0] += TC13::OperationFormat::DAC10BOA.getSize();
      break;
    }

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::FLOAT: {
      $cost[0] += TC13::OperationFormat::DAC16BOA.getSize();
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
  DEBUG_RULE_ACTION( "deref_dreg: tpm_IndexExp( areg, dreg )", $1 );

  auto *baseType = getBaseType( *$2->getExp() );
  const int byteSize = computeSizeOf( baseType );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", false );
  LLIR_Register *regTmp =
    loadRegisterRelativeAddress( p1.first, p2.first, byteSize, $1->getExp() );

  if ( loadResult )
    switch ( baseType->getType() ) {
      case IR_Type::CHAR: {
        TCINSTRUCTIONS.insertLD_B( reg, OPER_BASE, regTmp, 0, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        TCINSTRUCTIONS.insertLD_BU( reg, OPER_BASE, regTmp, 0, $1->getExp() );
        break;
      }

      case IR_Type::SHORT: {
        TCINSTRUCTIONS.insertLD_H( reg, OPER_BASE, regTmp, 0, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_SHORT: {
        TCINSTRUCTIONS.insertLD_HU( reg, OPER_BASE, regTmp, 0, $1->getExp() );
        break;
      }

      default: {
        TCINSTRUCTIONS.insertLD_W( reg, OPER_BASE, regTmp, 0, $1->getExp() );
        break;
      }
    }

  // WIR
  TC_DRegV *r = nullptr;
  auto &tmpReg =
    loadRegRelativeAddr( p1.second, p2.second, byteSize, $1->getExp() );

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createDReg());

    switch ( baseType->getType() ) {
      case IR_Type::CHAR: {
        TCINSTRUCTIONS.insertLD_B( *r, tmpReg, 0, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        TCINSTRUCTIONS.insertLD_BU( *r, tmpReg, 0, $1->getExp() );
        break;
      }

      case IR_Type::SHORT: {
        TCINSTRUCTIONS.insertLD_H( *r, tmpReg, 0, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_SHORT: {
        TCINSTRUCTIONS.insertLD_HU( *r, tmpReg, 0, $1->getExp() );
        break;
      }

      default: {
        TCINSTRUCTIONS.insertLD_W( *r, tmpReg, 0, $1->getExp() );
        break;
      }
    }
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification { regTmp, tmpReg, 0, baseType, true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_areg: tpm_IndexExp( areg, dreg )
{
  // This rule only handles base type pointers, base type arrays must be treated
  // differently, because in that case, we only compute the new address and
  // return it in an areg (no LD_A or deref_areg).
  if ( isPointerType( *getBaseType( *$2->getExp() ) ) )
    $cost[0] =
      $cost[2] + $cost[3] + loadRegisterRelativeAddressCost( pointerBytes ) +
      TC13::OperationFormat::AAC16BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_areg: tpm_IndexExp( areg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *regTmp =
    loadRegisterRelativeAddress(
      p1.first, p2.first, pointerBytes, $1->getExp() );
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );

  if ( loadResult )
    TCINSTRUCTIONS.insertLD_A( reg, OPER_BASE, regTmp, 0, $1->getExp() );

  // WIR
  auto &tmpReg =
    loadRegRelativeAddr( p1.second, p2.second, pointerBytes, $1->getExp() );
  TC_ARegV *r = nullptr;

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createAReg());

    TCINSTRUCTIONS.insertLD_A( *r, tmpReg, 0, $1->getExp() );
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        regTmp, tmpReg, 0, getBaseType( *$2->getExp() ), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_IndexExp( areg, dreg )
{
  auto *t = getBaseType( *$2->getExp() );

  // This rule only handles base type arrays. Here, we only compute the new
  // address and return it in an areg.
  if ( isArrayType( *t ) )
    $cost[0] =
      $cost[2] + $cost[3] +
      loadRegisterRelativeAddressCost( computeSizeOf( t ) );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_IndexExp( areg, dreg )", $1 );

  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg =
    loadRegisterRelativeAddress( p1.first, p2.first, byteSize, $1->getExp() );

  // WIR
  auto &r = loadRegRelativeAddr( p1.second, p2.second, byteSize, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
zero_op_areg: tpm_UnaryExpDEREF( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "zero_op_areg: tpm_UnaryExpDEREF( areg )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpADDR( zero_op_areg )
{
  if ( isZeroOpADDR( *$1->getExp() ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( zero_op_areg )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
zero_op_areg: tpm_IndexExp( areg, addrOffset )
{
  // This rule is only applicable if the parent operation is an address operator
  // (it's the only one that can take a 'zero_op_areg'). Therefore, this rule
  // just computes the access location and the address operator can then return
  // it without any memory access being performed (see ANSI C 6.5.3.2).
  $cost[0] = $cost[2] + TC13::OperationFormat::AAC16BOA.getSize();
}
=
{
  DEBUG_RULE_ACTION( "zero_op_areg: tpm_IndexExp( areg, addrOffset )", $1 );

  auto p = $action[2]();
  auto intConst = $action[3]().getIntValue();

  int off = intConst * computeSizeOf( getBaseType( *$2->getExp() ) );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertLEA( reg, OPER_BASE, p.first, off, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertLEA( r, p.second, off, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
zero_op_areg: tpm_IndexExp( areg, dreg )
{
  // This rule is only applicable if the parent operation is an address operator
  // (it's the only one that can take a 'zero_op_areg'). Therefore, this rule
  // just computes the access location and the address operator can then return
  // it without any memory access being performed (see ANSI C 6.5.3.2).
  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] = $cost[2] + $cost[3] + loadRegisterRelativeAddressCost( byteSize );
}
=
{
  DEBUG_RULE_ACTION( "zero_op_areg: tpm_IndexExp( areg, dreg )", $1 );

  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg =
    loadRegisterRelativeAddress( p1.first, p2.first, byteSize, $1->getExp() );

  // WIR
  auto &r = loadRegRelativeAddr( p1.second, p2.second, byteSize, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
zero_op_modified_areg: tpm_UnaryExpDEREF( modified_areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "zero_op_modified_areg: tpm_UnaryExpDEREF( modified_areg )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
modified_areg: tpm_UnaryExpADDR( zero_op_modified_areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_UnaryExpADDR( zero_op_modified_areg )", $1 );

  return( $action[2]() );
};


###############################################################################
#
#
# Conversion rules (without casting, just nonterminal conversion)
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: deref_dreg
{
  // This chain rule enables all deref/array-index/component-access rules to
  // produce only deref_dreg. If a parent rule requires a dreg nonterminal
  // instead, then this chain rule will extract it from the deref_dreg, which is
  // just a reg + memory access information.

  // We may only allow this conversion if the expression is not used in a
  // context that requires a deref_dreg, because the value must be written back
  // to memory.
  if ( !isMemoryWriteLocation( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: deref_dreg", $1 );

  auto lvalue = $action[1]( true );

  return(
    make_pair(
      lvalue.getResultRegister(),
      ref( dynamic_cast<TC_DRegV &>( *(lvalue.getResultReg()) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: deref_ereg
{
  // This chain rule enables all deref/array-index/component-access rules to
  // produce only deref_ereg. If a parent rule requires an ereg nonterminal
  // instead, then this chain rule will extract it from the deref_ereg, which is
  // just an ereg + memory access information.

  // We may only allow this conversion if the expression is not used in a
  // context that requires a deref_ereg, because the value must be written back
  // to memory.
  if ( !isMemoryWriteLocation( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: deref_ereg", $1 );

  auto lvalue = $action[1]( true );

  return(
    make_pair(
      lvalue.getResultRegister(),
      ref( dynamic_cast<TC_ERegV &>( *(lvalue.getResultReg()) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: deref_areg
{
  // This chain rule enables all deref/array-index/component-access rules to
  // produce only deref_areg. If a parent rule requires an areg nonterminal
  // instead, then this chain rule will extract it from the deref_areg, which is
  // just an areg + memory access information.

  // We may only allow this conversion if the expression is not used in a
  // context that requires a deref_areg, because the value must be written back
  // to memory.
  if ( !isMemoryWriteLocation( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: deref_areg", $1 );

  auto lvalue = $action[1]( true );

  return(
    make_pair(
      lvalue.getResultRegister(),
      ref( dynamic_cast<TC_ARegV &>( *(lvalue.getResultReg()) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: deref_llereg
{
  // This chain rule enables all deref/array-index/component-access rules to
  // produce only deref_llereg. If a parent rule requires an llereg nonterminal
  // instead, then this chain rule will extract it from the deref_llereg, which
  // is just an llereg + memory access information.

  // We may only allow this conversion if the expression is not used in a
  // context that requires a deref_llereg, because the value must be written
  // back to memory.
  if ( !isMemoryWriteLocation( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "llereg: deref_llereg", $1 );

  auto lvalue = $action[1]( true );

  return(
    make_pair(
      lvalue.getResultRegister(),
      ref( dynamic_cast<TC_ERegV &>( *(lvalue.getResultReg()) ) ) ) );
};
