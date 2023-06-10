/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2009 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


/*
  Returns the type of the given expression inside an enclosing init list. If the
  expression is not part of an init list, then the result is 0.
*/
const IR_Type *getTypeInInitList( const IR_Exp &initListElement )
{
  auto *initList =
    dynamic_cast<const IR_InitListExp *>( initListElement.getParent() );

  if ( initList )
    return(
      &( initList->getInitializedType(
        const_cast<IR_Exp *>( &initListElement ) ) ) );
  else
    return( nullptr );
};


/*
  initWithZeroCost computes the approximated costs of instructions that fill a
  memory area of the given size with zeroes.
*/
COST initWithZeroCost( long initSize, const IR_Exp &exp )
{
  DSTART( "COST initWithZeroCost(long int, const IR_Exp&)" );

  // These two variables control whether a loop will be created for
  // initialization or whether flat, unrolled code will be generated.
  const long flatIterationLimit = 2;
  const bool useUnrolledLoop = ( initSize / 8 ) <= flatIterationLimit;

  // If we are not generating an initialization loop, we may also omit the first
  // LEA and use base + offset addressing.
  const bool useBaseOffsetAddressing = useUnrolledLoop;

  COST cost = 2 * TC13::OperationFormat::SDC4_1.getSize();

  if ( !useBaseOffsetAddressing )
    cost += TC13::OperationFormat::AAC16BOA.getSize();

  // Generate a loop using ST.D to initialize the memory area with zero.
  const long loopIterations = initSize / 8;

  // If the number of loop iterations exceeds the 'flatIterationLimit', we
  // generate a loop. Otherwise, we generate flat code (unrolled loop).
  if ( loopIterations > flatIterationLimit ) {
    // Initialize loop counter.
    const bool useLOOP =
      !TCCODESEL->getConfig()->getEnableLOOPInstruction() ||
      TCCODESEL->getNumberOfEnclosingLOOPs( exp.getStmt() ) <= 1;

    if ( useLOOP )
      cost +=
        TC13::OperationFormat::AAC16BOA.getSize() +
        TC13::OperationFormat::AC10EPIA.getSize() +
        TC13::OperationFormat::AL_3.getSize();
    else
      cost +=
        TC13::OperationFormat::DC16_1.getSize() +
        TC13::OperationFormat::AC10EPIA.getSize() +
        TC13::OperationFormat::DC4L_3.getSize();
  } else
    // Generate an unrolled initialization loop.
    for ( int i = 0; i < loopIterations; ++i )
      cost += TC13::OperationFormat::AC10EBOA.getSize();

  return( cost );
};


/*
  initWithZero fills the memory area of the given object with zeroes.
*/
void initWithZero( const TC_AddressWithOffset &addr, long initSize,
                   const IR_Exp &exp )
{
  DSTART(
    "void initWithZero(const TC_AddressWithOffset&, long int, const IR_Exp&)" );

  // These two variables control whether a loop will be created for
  // initialization or whether flat, unrolled code will be generated.
  const long flatIterationLimit = 2;
  const bool useUnrolledLoop =
    ( ( initSize - ( addr.getOffset() % 4 ) ) / 8 ) <= flatIterationLimit;

  // If we are not generating an initialization loop, we may also omit the first
  // LEA and use base + offset addressing.
  const bool useBaseOffsetAddressing = useUnrolledLoop;
  long baseOffset = 0;

  // Load the new base address into an address register.
  LLIR_Register *areg = nullptr;
  if ( useBaseOffsetAddressing ) {
    areg = addr.getARegister();
    baseOffset = addr.getOffset();
  } else {
    areg = TCINSTRUCTIONS.CreateRegister( "", true );
    TCINSTRUCTIONS.insertLEA(
      areg, OPER_BASE, addr.getARegister(), addr.getOffset(), &exp );
  }
  auto &aReg =
    useBaseOffsetAddressing ?
      addr.getAReg() : TCINSTRUCTIONS.createAReg();
  if ( !useBaseOffsetAddressing )
    TCINSTRUCTIONS.insertLEA( aReg, addr.getAReg(), addr.getOffset(), &exp );

  // Create zero'd DReg as source for initialization.
  // LLIR
  LLIR_Register *zeroDReg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( zeroDReg, 0, &exp );

  // WIR
  auto &eReg = TCINSTRUCTIONS.createEReg();
  auto &r = dynamic_cast<TC_DRegV &>( eReg.begin()->get() );
  TCINSTRUCTIONS.insertMOV( r, 0, &exp );
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( eReg.rbegin()->get() ), 0, &exp );

  // Align to 4 bytes so that we can use ST.D afterwards.
  long init_offset = addr.getOffset();
  if ( init_offset % 2 == 1 ) {
    if ( useBaseOffsetAddressing ) {
      TCINSTRUCTIONS.insertST_B( OPER_BASE, areg, baseOffset, zeroDReg, &exp );
      TCINSTRUCTIONS.insertST_B( aReg, baseOffset, r, &exp );
      baseOffset += 1;
    } else {
      TCINSTRUCTIONS.insertST_B( OPER_POSTINC, areg, 1, zeroDReg, &exp );
      TCINSTRUCTIONS.insertST_B( TC13::AddressingMode::post, aReg, 1, r, &exp );
    }

    initSize -= 1;
    init_offset += 1;
  }

  if ( init_offset % 4 == 2 ) {
    if ( useBaseOffsetAddressing ) {
      TCINSTRUCTIONS.insertST_H( OPER_BASE, areg, baseOffset, zeroDReg, &exp );
      TCINSTRUCTIONS.insertST_H( aReg, baseOffset, r, &exp );
      baseOffset += 2;
    } else {
      TCINSTRUCTIONS.insertST_H( OPER_POSTINC, areg, 2, zeroDReg, &exp );
      TCINSTRUCTIONS.insertST_H( TC13::AddressingMode::post, aReg, 2, r, &exp );
    }

    initSize -= 2;
  }

  // Generate a loop using ST.D to initialize the memory area with zero.
  const long loopIterations = initSize / 8;
  initSize = initSize % 8;

  // Extend the zero'd DReg to an EReg to be able to use ST.D.
  LLIR_Register *zeroDReg2 = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( zeroDReg2, 0, &exp );
  LLIR_Register *zeroEReg =
    TCINSTRUCTIONS.CreateERegister( "", zeroDReg, zeroDReg2 );

  // If the number of loop iterations exceeds the 'flatIterationLimit', we
  // generate a loop. Otherwise, we generate flat code (unrolled loop).
  if ( loopIterations > flatIterationLimit ) {
    // Initialize loop counter.
    const bool useLOOP =
      !TCCODESEL->getConfig()->getEnableLOOPInstruction() ||
      TCCODESEL->getNumberOfEnclosingLOOPs( exp.getStmt() ) <= 1;

    // LLIR
    LLIR_Register *loopCounter = nullptr;

    if ( useLOOP ) {
      loopCounter = TCINSTRUCTIONS.CreateRegister( "", true );
      TCINSTRUCTIONS.insertLEA( loopCounter, loopIterations - 1, &exp );
    } else {
      loopCounter = TCINSTRUCTIONS.CreateRegister( "", false );
      TCINSTRUCTIONS.insertMOV( loopCounter, loopIterations - 1, &exp );
    }

    // Generate new loop basic block.
    LLIR_BB &loopBB = beginNewLLIRBasicBlock();
    TCCODESEL->addLoopboundToLLIR(
      &loopBB, loopIterations, loopIterations,
      LLIR_Loopbound::TAIL_CONTROLLED );

    // Generate body
    TCINSTRUCTIONS.insertST_D( OPER_POSTINC, areg, 8, zeroEReg, &exp );

    // Generate tail
    if ( useLOOP ) {
      TCINSTRUCTIONS.insertLOOP( loopCounter, loopBB.GetLabel(), &exp );
      TCCODESEL->getLastLLIRBB()->AddPragma(
        new LLIR_Pragma( "Loop condition: LOOP", true ) );
    } else {
      TCINSTRUCTIONS.insertJNED( loopCounter, 0, loopBB.GetLabel(), &exp );
      TCCODESEL->getLastLLIRBB()->AddPragma(
        new LLIR_Pragma( "Loop condition: DOWHILE", true ) );
    }

    // Generate new child basic block.
    beginNewLLIRBasicBlock();

    // WIR
    if ( useLOOP ) {
      // Set up loop counter.
      auto &lCnt = TCINSTRUCTIONS.createAReg();
      TCINSTRUCTIONS.insertLEA( lCnt, loopIterations - 1, &exp );

      // Create new basic block for initializing loop.
      auto &b = TCCODESEL->startNewBasicBlock();
      // TODO: Add loop bound to WIR BB!

      // Generate loop body.
      TCINSTRUCTIONS.insertST_D(
        TC13::AddressingMode::post, aReg, 8, eReg, &exp );

      // Generate loop condition.
      TCINSTRUCTIONS.insertLOOP( lCnt, b, &exp );
    } else {
      // Set up loop counter.
      auto &lCnt = TCINSTRUCTIONS.createDReg();
      TCINSTRUCTIONS.insertMOVConstant( lCnt, loopIterations - 1, &exp );

      // Create new basic block for initializing loop.
      auto &b = TCCODESEL->startNewBasicBlock();
      // TODO: Add loop bound to WIR BB!

      // Generate loop body.
      TCINSTRUCTIONS.insertST_D(
        TC13::AddressingMode::post, aReg, 8, eReg, &exp );

      // Generate loop condition.
      TCINSTRUCTIONS.insertJNED( lCnt, 0, b, &exp );
    }

    // Create new basic block after the initializing loop.
    TCCODESEL->startNewBasicBlock();

  } else
    // Generate an unrolled initialization loop.
    for ( int i = 0; i < loopIterations; ++i ) {
      TCINSTRUCTIONS.insertST_D( OPER_BASE, areg, baseOffset, zeroEReg, &exp );
      TCINSTRUCTIONS.insertST_D( aReg, baseOffset, eReg, &exp );
      baseOffset += 8;
    }

  // Fill the remaining memory of size (totalSize - alignPadding) % 8 with 0.
  if ( initSize >= 4 ) {
    if ( useUnrolledLoop ) {
      TCINSTRUCTIONS.insertST_W( OPER_BASE, areg, baseOffset, zeroDReg, &exp );
      TCINSTRUCTIONS.insertST_W( aReg, baseOffset, r, &exp );
      baseOffset += 4;
    } else {
      TCINSTRUCTIONS.insertST_W( OPER_POSTINC, areg, 4, zeroDReg, &exp );
      TCINSTRUCTIONS.insertST_W( TC13::AddressingMode::post, aReg, 4, r, &exp );
    }

    initSize -= 4;
  }

  if ( initSize >= 2 ) {
    if ( useUnrolledLoop ) {
      TCINSTRUCTIONS.insertST_H( OPER_BASE, areg, baseOffset, zeroDReg, &exp );
      TCINSTRUCTIONS.insertST_H( aReg, baseOffset, r, &exp );
      baseOffset += 2;
    } else {
      TCINSTRUCTIONS.insertST_H( OPER_POSTINC, areg, 2, zeroDReg, &exp );
      TCINSTRUCTIONS.insertST_H( TC13::AddressingMode::post, aReg, 2, r, &exp );
    }

    initSize -= 2;
  }

  if ( initSize >= 1 ) {
    if ( useUnrolledLoop ) {
      TCINSTRUCTIONS.insertST_B( OPER_BASE, areg, baseOffset, zeroDReg, &exp );
      TCINSTRUCTIONS.insertST_B( aReg, baseOffset, r, &exp );
    } else {
      TCINSTRUCTIONS.insertST_B( OPER_POSTINC, areg, 1, zeroDReg, &exp );
      TCINSTRUCTIONS.insertST_B( TC13::AddressingMode::post, aReg, 1, r, &exp );
    }

    initSize -= 1;
  }

  ufAssertT( initSize == 0, "Incomplete initialization!" );
};


/*
  Returns the number of initializable elements in the given type.
*/
unsigned int countAllElements( const IR_Type &t )
{
  unsigned int count = 0;
  auto *aType = dynamic_cast<const IR_ArrayType *>( &t );
  auto *cType = dynamic_cast<const IR_ComposedType *>( &t );

  if ( aType ) {
    auto *fType = dynamic_cast<const IR_FixArrayType *>( aType );
    ufAssertT( fType, "Variable-length arrays are not supported." );

    count += fType->getSize() * countAllElements( fType->getBaseType() );
  } else

  if ( cType ) {
    const list<IR_Symbol *> &components =
      const_cast<IR_ComposedType *>( cType )->getComponents().getSymbols();
    for ( auto *sym : components )
      count += countAllElements( sym->getType() );
  } else
    count += 1;

  return( count );
};


/*
  Returns the number of init expressions in the init list and its sublists.
*/
unsigned int countAllElements( const IR_InitListExp &initExp )
{
  unsigned int count = 0;

  const list<IR_Exp *> &expList = initExp.getInitExps();
  for ( auto *exp : expList ) {
    auto *sublist = dynamic_cast<IR_InitListExp *>( exp );
    if ( sublist )
      count += countAllElements( *sublist );
    else
      count += 1;
  }

  return( count );
};


/*
  Determines whether the given init list expression has initializer expressions
  for all elements of the initialized type that have to be initialized.
*/
bool coversAllElements( const IR_InitListExp &initExp )
{
  DSTART( "coversAllElements( const IR_InitListExp &initExp )" );

  // We have to perform a comparison of all elements (including nested ones),
  // because multidimensional arrays may be initialized with a flat intializer.
  const unsigned int initCount = countAllElements( initExp );
  const unsigned int typeCount = countAllElements( initExp.getType() );
  DOUT(
    "Init list expression " << getExpString( initExp ) << " initializes " <<
    initCount << " of " << typeCount << " elements!" << endl );

  return( initCount == typeCount );
};


/*
  Returns the symbol on the LHS of the assignment expression, if the given
  expression is an assignment expression.
*/
IR_Symbol *getLHSSymbol( const IR_Exp &assignment )
{
  auto *aexp = dynamic_cast<const IR_AssignExp *>( &assignment );

  if ( aexp ) {
    auto *symExp = dynamic_cast<IR_SymbolExp *>( &aexp->getLHS() );
    return ( symExp ? &symExp->getSymbol() : nullptr );
  } else
    return( nullptr );
};
