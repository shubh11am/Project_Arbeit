/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2010 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  Returns the type of the given expression inside an enclosing init list. If
  the expression is not part of an init list, then the result is 0.
 */
static const IR_Type *getTypeInInitList( const IR_Exp &initListElement )
{
  const IR_InitListExp * initList = dynamic_cast<const IR_InitListExp *>(
                                      initListElement.getParent() );

  if( initList ) {
    return &( initList->getInitializedType( const_cast<IR_Exp*>(
                                              &initListElement ) ) );
  } else {
    return 0;
  }
}

/*!
   Returns the cost for copying a composed type object from one memory
   location to another one with the function 'copyComposedType'. This is
   not 100% precise, as it does not model the extra cost of computing
   valid starting addresses when the offsets are nonzero.
 */
int copyComposedTypeCost( IR_ComposedType &composedType )
{
  unsigned int composedTypeSize = Stack::getStackSize( &composedType );
  // move address from address register
  //int cost = 2 * CT( INS_MOV_AA_32 );
  int cost = 2 * CT( INS_MOV_32 );

  while ( composedTypeSize > 0 ) {

    int elementOffset = 0;

    if ( composedTypeSize == 1 ) {

      elementOffset = charBytes;
      cost += CT( INS_LDRB_32 ) + CT( INS_STRB_32 );

    } else
    if ( ( composedTypeSize >= 2 ) && ( composedTypeSize < 4 ) ) {

      elementOffset = shortBytes;
      cost += CT( INS_LDRH_32 ) + CT( INS_STRH_32 );

    } else
    if ( ( composedTypeSize >= 4 ) && ( composedTypeSize < 8 ) ) {

      elementOffset = intBytes;
      cost += CT( INS_LDR_32 ) + CT( INS_STR_32 );

    } else {

      elementOffset = 2 * intBytes;
      cost += CT( INS_LDR_32 ) + CT( INS_STR_32 );

    }

    composedTypeSize -= elementOffset;

  }

  return cost;
}

/*!
   Copies a composed type object from one memory location to another one.

   'insertAfter' may be null, in which case the code is inserted at the start
                 of the given basic block 'targetBB'
 */
void copyComposedType( IR_ComposedType &composedType,
                       LLIR_Register *source, int source_offset,
                       LLIR_Register *target, int target_offset,
                       LLIR_BB &targetBB, LLIR_Instruction *insertAfter )
{
  DSTART( "copyComposedType" );
  ufAssertT( ( !insertAfter || insertAfter->GetBB() == &targetBB ),
    "'insertAfter' must be within 'targetBB'" );

  // Check whether we should update the current instruction in the ARMCODESEL
  // after having added all the generated instructions
  const bool updateCurrentInstruction = (
    ARMCODESEL->getLastLLIRBB() == &targetBB );

  // Define a shorthand for inserting instructions one after the other
  LLIR_Instruction *lastInserted = insertAfter;
  #define IADD( new_ins )                     \
  { LLIR_Instruction *ins = new_ins;          \
    targetBB.InsertIns( ins, lastInserted );  \
    lastInserted = ins; }                     \

  const unsigned int composedTypeSize = Stack::getStackSize( &composedType );

  // If possible we use offsets for addressing the load/store targets,
  // else we load the addresses into new registers and use postincrement
  // load/stores.
  const int minSourceOffset = ( source_offset );
  const int minTargetOffset = ( target_offset );
  const int maxSourceOffset = ( source_offset + (signed int)composedTypeSize );
  const int maxTargetOffset = ( target_offset + (signed int)composedTypeSize );
  const bool useOffsetsForSource =
    ( minSourceOffset >= minSignedConst12Value ) &&
    ( maxSourceOffset <= maxSignedConst12Value );
  const bool useOffsetsForTarget =
    ( minTargetOffset >= minSignedConst12Value ) &&
    ( maxTargetOffset <= maxSignedConst12Value );

  LLIR_Register *compTargetBase = 0;
  if ( !useOffsetsForTarget ) {
    // Get a pointer to the stack location of the composed type argument
    if ( target_offset == 0 ) {
      // We must make a copy of the base pointer first, because it may not be
      // modified by our copy operation (might be stack pointer, pointer reg,...)
      compTargetBase = ARMINSTRUCTIONS.CreateRegister( "", true );
      IADD( insMOV( OPER_AL, "", compTargetBase, target ) );
    } else {
      compTargetBase = ARMINSTRUCTIONS.CreateRegister( "", true );

      // Better: Check if can be created by rotating an 8 bit constant an even
      // number to the right.
      if ( ( target_offset >= minSignedConst8Value ) &&
           ( target_offset <= maxSignedConst8Value ) ) {
        IADD( insADD( OPER_AL, "", compTargetBase, target, target_offset ) );
      } else {
        // Split up the target offset.
        unsigned int mask = 0xFF;
        int target_offset_tmp = target_offset;
        int nrBytes = 0;
        while ( target_offset_tmp != 0 && nrBytes < 4 ) {
          int part = target_offset_tmp & mask;
          // Cut off the just copied byte.
          target_offset_tmp &= ~mask;
          LLIR_Register* addReg;
          if ( nrBytes == 0 )
            addReg = target;
          else
            addReg = compTargetBase;
          IADD( insADD( OPER_AL, "", compTargetBase, addReg, part ) );
          ++nrBytes;
          mask <<= 4;
        }
      }
    }
  }

  // Get a pointer to the memory location of the source composed object
  LLIR_Register *compSourceBase = 0;
  if ( !useOffsetsForSource ) {
    if ( source_offset == 0 ) {
      // We must make a copy of the base pointer first, because he may not be
      // modified by our copy operation (might be stack pointer, pointer reg,...)
      compSourceBase = ARMINSTRUCTIONS.CreateRegister( "", true );
      IADD( insMOV( OPER_AL, "", compSourceBase, source ) );
    } else {
      compSourceBase = ARMINSTRUCTIONS.CreateRegister( "", true );

      // Better: Check if can be created by rotating an 8 bit constant an even
      // number to the right.
      if ( ( target_offset >= minSignedConst8Value ) &&
           ( target_offset <= maxSignedConst8Value ) ) {
        IADD( insADD( OPER_AL, "", compTargetBase, source, source_offset ) );
      } else {
        // Split up the source offset.
        unsigned int mask = 0xFF;
        int source_offset_tmp = source_offset;
        int nrBytes = 0;
        while ( source_offset_tmp != 0 && nrBytes < 4 ) {
          int part = source_offset_tmp & mask;
          // Cut off the just copied byte.
          source_offset_tmp &= ~mask;
          LLIR_Register* addReg;
          if ( nrBytes == 0 )
            addReg = target;
          else
            addReg = compSourceBase;
          IADD( insADD( OPER_AL, "", compSourceBase, addReg, part ) );
          ++nrBytes;
          mask <<= 4;
        }
      }
    }
  }

  // Helper variables for copying the struct
  LLIR_Register *lhsreg  = ARMINSTRUCTIONS.CreateRegister( "", false );

  /* After calculating the address and adding the corresponding instructions,
   * the composed type must be loaded from memory and stored on the stack of the
   * function */
  // TODO: For _really_ huge structures we should generate a loop here, instead
  //       of emitting all the commands as straight line code.
  unsigned int remainingBytes = composedTypeSize;
  while ( remainingBytes > 0 ) {
    const int currentPosition = composedTypeSize - remainingBytes;
    int elementOffset = 0;

    if ( remainingBytes >= 4 ) {
      elementOffset = intBytes;
      if ( useOffsetsForSource ) {
        IADD( insLDR( OPER_AL, OPER_IMMOFF, lhsreg, source, source_offset +
                      currentPosition ) );
      } else {
        IADD( insLDR( OPER_AL, OPER_IMMOFF, lhsreg, compSourceBase,
                      elementOffset ) );
      }
      if ( useOffsetsForTarget ) {
        IADD( insSTR( OPER_AL, OPER_IMMOFF, lhsreg, target, target_offset +
                      currentPosition ) );
      } else {
        IADD( insSTR( OPER_AL, OPER_IMMOFF, lhsreg, compTargetBase,
                      elementOffset ) );
      }

    } else
    if ( remainingBytes >= 2 ) {
      elementOffset = shortBytes;
      if ( useOffsetsForSource ) {
        IADD( insLDRH( OPER_AL, OPER_IMMOFF, lhsreg, source, source_offset +
                       currentPosition ) );
      } else {
        IADD( insLDRH( OPER_AL, OPER_IMMOFF, lhsreg, compSourceBase,
                       elementOffset ) );
      }
      if ( useOffsetsForTarget ) {
        IADD( insSTRH( OPER_AL, OPER_IMMOFF, target, lhsreg, target_offset +
                       currentPosition ) );
      } else {
        IADD( insSTRH( OPER_AL, OPER_IMMOFF, compTargetBase, lhsreg,
              elementOffset ) );
      }

    } else
    if ( remainingBytes == 1 ) {
      elementOffset = charBytes;
      if ( useOffsetsForSource ) {
        IADD( insLDRB( OPER_AL, OPER_IMMOFF, lhsreg, source, source_offset +
                       currentPosition ) );
      } else {
        IADD( insLDRB( OPER_AL, OPER_IMMOFF, lhsreg, compSourceBase,
                       elementOffset ) );
      }
      if ( useOffsetsForTarget ) {
        IADD( insSTRB( OPER_AL, OPER_IMMOFF, target, lhsreg, target_offset +
                       currentPosition ) );
      } else {
        IADD( insSTRB( OPER_AL, OPER_IMMOFF, compTargetBase, lhsreg,
                       elementOffset ) );
      }

    } /*else {
      throw ufFatalError( "Not yet implemented." );

    }*/

    remainingBytes -= elementOffset;

  }

  #undef IADD

  // Inform the codeselector about the new instructions if neccessary
  if ( updateCurrentInstruction ) {
    ARMCODESEL->setCurrentInstruction( lastInserted );
  }
}

/*!
   copyComposedOnStackCost returns the cost that is needed when a composed type
   function parameter is copied to the local stack with the function
   'copyComposedOnStack'.

   'composed' is the symbol that identifies the composed object to be copied
*/
int copyComposedOnStackCost( IR_Symbol &composed )
{
  int result = 0;

  IR_ComposedType *compType = dynamic_cast<IR_ComposedType*>
    ( &composed.getType() );
  ufAssertT( compType, "Invalid argument!" );

  ufAssertT( !ARMCODESEL->getStack()->getComposedPushCostAdded( &composed ),
    "Given push cost was already accounted for!" );

  const string symbolReg = ARMCODESEL->getStack()->getSymbolReg( &composed );

  if ( symbolReg != "" ) {
    result += copyComposedTypeCost( *compType );
  } else {
    result += CT( INS_LDR_32 ) + copyComposedTypeCost( *compType );
  }

  // Mark, that the costs were already added for this argument symbol
  ARMCODESEL->getStack()->setComposedPushCostAdded( &composed );

  return result;
}

/*!
   copyComposedOnStack copies an entire composed type passed as parameter onto
   the stack.

   'composed' is the symbol that identifies the composed object to be copied
   'currentFunction' is LLIR function that has 'composed' as an argument. The
                     copy instructions are inserted at the front of the first
                     basic block in this function.
*/
void copyComposedOnStack( IR_Symbol &composed, LLIR_Function &currentFunction )
{
  DSTART( "copyComposedOnStack( IR_Symbol &theSym, LLIR_BB &theBB )" );

  IR_ComposedType *compType = dynamic_cast<IR_ComposedType*>
    ( &composed.getType() );
  ufAssertT( compType, "Invalid argument!" );

  ufAssertT( !ARMCODESEL->getStack()->getComposedPushed( &composed ),
    "Given symbol was already copied to the stack!" );
  ufAssertT( composed.getSymbolTable().getFunction(),
    "'sym' was no function parameter!" );

  // New stack location of the struct inside the stack of the callee
  LLIR_Register *target_reg = ARMINSTRUCTIONS.CreateRegister( PHREG_SP );
  const int target_offset = ARMCODESEL->getStack()->getComposedParameterBufferOffset(
                                 &composed );

  LLIR_BB *firstBB = currentFunction.GetFirstBB();

  // See if the composed object itself or its address was passed in a register
  const int paramStackOffset = ARMCODESEL->getStack()->getSymbolOffset( &composed );
  if ( paramStackOffset < 0 ) {
    DOUT( "composed passed via register" << endl );

      // Location of the struct, as passed by the calling function
      LLIR_Register &source = getFunctionArgumentRegister( composed );

      // Backup source_reg at beginning of function
      LLIR_Register *source_reg = ARMINSTRUCTIONS.CreateRegister( "", true );
      LLIR_Instruction *insMov = insMOV( OPER_AL, "", source_reg, &source );
      ARMCODESEL->getLastLLIRFunction()->GetFirstBB()->InsertIns( insMov );

      // Copy the struct to the given location and insert the copy code at the
      // start of the given function.
      copyComposedType( *compType, source_reg, 0, target_reg, target_offset,
                        *firstBB, firstBB->GetLastIns() );
  } else {
    DOUT( "composed passed via stack" << endl );
    // Copy the original struct to a local stack area

    // In this case the composed object's address was passed via the stack.
    // We need to get the address from the stack and copy the object to the
    // local stack.
    LLIR_Register * const reg_address  = ARMINSTRUCTIONS.CreateRegister( "", true );
    LLIR_Register * const stack_pointer = ARMINSTRUCTIONS.CreateRegister( PHREG_SP );

    // Get the object's address from the parameter stack
    LLIR_Instruction * const loadAddress = insLDR( OPER_AL, OPER_IMMOFF, reg_address,
      stack_pointer, paramStackOffset );
    currentFunction.GetFirstBB()->InsertIns( loadAddress );

    // Load the object into the given stack position from the specified address
    copyComposedType( *compType, reg_address, 0, target_reg, target_offset,
                      *firstBB, firstBB->GetLastIns() );
  }

  // Mark as copied
  ARMCODESEL->getStack()->setComposedPushed( &composed );
}

/*!
  Returns the symbol on the LHS of the assignment expression, if the given
  expression is an assignment expression.
 */
IR_Symbol *getLHSSymbol( IR_Exp &assignment )
{
  IR_AssignExp * const aexp = dynamic_cast<IR_AssignExp*>( &assignment );
  if ( aexp ) {
    IR_SymbolExp * const sexp = dynamic_cast<IR_SymbolExp*>( &aexp->getLHS() );
    return ( sexp ? &sexp->getSymbol() : 0 );
  } else {
    return 0;
  }
}

/*!
  Returns the number of initializable elements in the given type.
 */
static unsigned int countAllElements( const IR_Type &type )
{
  unsigned int count = 0;
  const IR_ArrayType    *aType = dynamic_cast<const IR_ArrayType*>(    &type );
  const IR_ComposedType *cType = dynamic_cast<const IR_ComposedType*>( &type );

  if ( aType ) {
    const IR_FixArrayType *fType = dynamic_cast<const IR_FixArrayType*>( aType );
    ufAssertT( fType, "variable length arrays are not supported!" );

    count += fType->getSize() * countAllElements( fType->getBaseType() );
  } else
  if ( cType ) {
    const list<IR_Symbol*> &components = const_cast<IR_ComposedType*>( cType )
                                           ->getComponents().getSymbols();
    for ( list<IR_Symbol*>::const_iterator
          itList  = components.begin();
          itList != components.end(); ++itList ) {
      count += countAllElements( ( *itList )->getType() );
    }
  } else {
    count += 1;
  }

  // Add elements of subtypes
  return count;
}
/*!
  Returns the number of init expressions in the init list and its sublists.
 */
static unsigned int countAllElements( const IR_InitListExp &initExp )
{
  unsigned int count = 0;

  const std::list<IR_Exp*> &expList = initExp.getInitExps();
  for ( std::list<IR_Exp*>::const_iterator
        itList  = expList.begin();
        itList != expList.end(); ++itList ) {
    IR_InitListExp * const sublist = dynamic_cast<IR_InitListExp*>( *itList );
    if ( sublist ) {
      count += countAllElements( *sublist );
    } else {
      count += 1;
    }
  }

  return count;
}

/*!
  Determines whether the given init list expression has initializer expressions
  for all elements of the initialized type, whcih must be initialized.
 */
static bool coversAllElements( const IR_InitListExp &initExp )
{
  DSTART( "coversAllElements( const IR_InitListExp &initExp )" );

  // We have to perform a comparison of all elements (including nested ones)
  // because multidimensional arrays may be initialized with a flat intializer.
  const unsigned int initCount = countAllElements( initExp );
  const unsigned int typeCount = countAllElements( initExp.getType() );
  DOUT( "Init list expression " << getExpString( initExp ) << " initializes "
    << initCount << " of " << typeCount << " elements!" << endl );

  return( initCount == typeCount );
}

std::deque<LLIR_Register *>* createZeroedRegisters( int count, IR_Exp *exp,
                                                   std::deque<LLIR_Register *> *existing = nullptr )
{
  std::deque<LLIR_Register *> *zeroedRegs;

  if( !existing )
   zeroedRegs = new std::deque<LLIR_Register *>();
  else
    zeroedRegs = existing;

  for( int i = zeroedRegs->size(); i < count; i++ ) {
    LLIR_Register *r = ARMINSTRUCTIONS.CreateRegister( "" );
    bindToPHREG( *r, 4 + i );
    zeroedRegs->push_back( r );
    ARMINSTRUCTIONS.insertMOV( r, 0, exp );
  }

  return zeroedRegs;
}

/*!
  Calculates the cost to zero a block of memory using initWithZero().
 */
static COST initWithZeroCost( unsigned int size, int alignment ) {

  ufAssert( size > 0 );

  COST cost = 0;

  // Instructions to ensure 4 byte alignment for the following STMs
  if ( size >= 2 &&  alignment & 0x2 ) {
    DOUT( "Alignment 0x2 is set! Inserting STRH" << endl );
    cost += CT( INS_STRH_32 );
    size -= 2;
  }

  if( size >= 1 && alignment & 0x1 ) {
    DOUT( "Alignment 0x1 is set! Inserting STRB" << endl );
    cost += CT( INS_STRB_32 );
    size--;
  }

  unsigned int registerCount = min( size / 4 , (unsigned int) 4 );

  cost += CT( INS_MOV_32 ) * min( registerCount, (unsigned int) 1);

  // Instructions for the loop
  if ( size >= 32 ) {
    unsigned int loopIterations = size / 16;
    size -= loopIterations * 16;
    cost += ARMINSTRUCTIONS.insertMOV_ORR_Cost( loopIterations );
    cost += CT( INS_STM_32 ) + CT( INS_SUB_32 ) + CT( INS_CMP_32 ) +
      CT( INS_B_32 );
  }

  // Additional STM instructions with 4/3/2/1 register
  unsigned int additionalWrites = 0;
  for ( int i = registerCount; i > 0; i--) {
    unsigned int byteCount = i * 4;
    additionalWrites += size / byteCount;
    size %= byteCount;
  }
  cost += CT( INS_STM_32 ) * additionalWrites;

  if ( size % 4 >= 2 ) {
      cost += CT( INS_STRH_32 );
      size -= 2;
  }

  if( size % 2 == 1) {
      cost += CT( INS_STRB_32 );
      size--;
  }

  ufAssert( size == 0 );
  return cost;
}

/*!
  Fills the memory area of the given object with zeroes.

  'address' is the beginning of the memory area that should be nullified.
            It will be modified.
  'size' is the size of the memory that should be nullified
  'alignment' describes the alignment of the address. The first two bits should
    be equal to those of the address.
  'exp' is the expression that caused the nullification
 */
static void initWithZero( LLIR_Register* address, unsigned int size,
                          unsigned int alignment, IR_Exp *exp,
                          std::deque<LLIR_Register*> *registers = nullptr )
{
  DSTART( "void initWithZero( LLIR_Register*, unsigned int, unsigned int, "
          "IR_Exp*, std::deque<LLIR_Register*> )" );

  ufAssert( size > 0 );

  DOUT( "Zeroing " << size << " bytes." <<
        " Alignment&0x3: " << (alignment & 0x3) << endl );

  bool ownZeroedRegs = registers == nullptr;
  registers = createZeroedRegisters( 1, exp, registers );
  LLIR_Register *zeroReg = registers->front();

  size_t alignBytes = alignment & 0x3;

  if( size >= 1 && alignBytes & 0x1 ) {
    DOUT( "Alignment 0x1 is set! Inserting STRB" << endl );
    ARMINSTRUCTIONS.insertSTRB( OPER_AL, OPER_IMMPOST, zeroReg, address, 1, exp );
    size--;
    alignBytes += 1;
  }

  if ( size >= 2 &&  alignBytes & 0x2 ) {
    DOUT( "Alignment 0x2 is set! Inserting STRH" << endl );
    ARMINSTRUCTIONS.insertSTRH( OPER_AL, OPER_IMMPOST, zeroReg, address, 2, exp );
    size -= 2;
  }

  // How many registers do we use to write zeroes to memory?
  // Capped at 4 registers because register pressure is not known.
  unsigned char registerCount = min( size / 4 , (unsigned int) 4 );

  // Create registers for stmia
  createZeroedRegisters( registerCount, exp, registers );

  // Insert a loop for STM with four registers
  if( size >= 32 ) {
    ufAssert( registerCount == 4 );

    unsigned int loopIterations = size / 16;
    size -= loopIterations * 16;

    LLIR_Register *loopCounter = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertMOV_ORR( loopCounter, loopIterations, exp );

    string loopEntry = LLIR::getUniqueLabel();

    beginNewLLIRBasicBlock( loopEntry.c_str());

    LLIR_BB *bb = ARMCODESEL->getLastLLIRBB();
    ARMCODESEL->addLoopboundToLLIR( bb, loopIterations, loopIterations,
      LLIR_Loopbound::TAIL_CONTROLLED );

    ARMINSTRUCTIONS.insertSTM( OPER_IA, OPER_AL, address, OPER_WRITEBACK,
                             registers, exp );

    ARMINSTRUCTIONS.insertSUB( loopCounter, loopCounter, 1, exp );
    ARMINSTRUCTIONS.insertCMP( OPER_AL, loopCounter, 0, exp );
    ARMINSTRUCTIONS.insertB( OPER_GT, loopEntry, exp );

    beginNewLLIRBasicBlock();
  }

  // Add STM instructions with less registers
  auto registersCopy = std::deque<LLIR_Register*>( *registers );
  for ( int i = registersCopy.size(); i > 0; i-- ) {
    if ( size >= registersCopy.size() * 4 ) {
      ARMINSTRUCTIONS.insertSTM( OPER_IA, OPER_AL, address, OPER_WRITEBACK,
                               &registersCopy, exp );
      size -= registersCopy.size() * 4;
    }

    registersCopy.pop_back();
  }

  if ( size % 4 >= 2 ) {
    DOUT( "After loop STRH" << endl );
    ARMINSTRUCTIONS.insertSTRH( OPER_AL, OPER_IMMPOST, zeroReg, address, 2, exp );
    size -= 2;
  }

  if( size % 2 == 1 ) {
    DOUT( "After loop STRB" << endl );
    ARMINSTRUCTIONS.insertSTRB( OPER_AL, OPER_IMMPOST, zeroReg, address, 1, exp );
    size--;
  }

  if ( ownZeroedRegs ) delete registers;
  ufAssert( size == 0 );
}

UninitializedMemoryArea::UninitializedMemoryArea( unsigned int size ) :
  mChunks( 1, (struct Chunk) {0, size } )
{}

void UninitializedMemoryArea::splitByInitExpressions( IR_InitListExp *initList, int offset )
{
  DSTART( "void UninitializedMemoryArea::splitByInitExpressions("
          "IR_InitListExp*, int)" );

  auto type = initList->getType();

  auto parseExpression = [&](IR_Exp *e) {

      IR_InitListExp *subExp = dynamic_cast<IR_InitListExp*>( e );
      int additionalOffset = initList->getOffset( e ).getIntValue();
      DOUT( "Offset: " << offset << ", add. offset: " << additionalOffset << endl );

      if ( subExp ) {
        DOUT( "Descending into nested init list. " << additionalOffset << endl );
        splitByInitExpressions( subExp, offset + additionalOffset);
      } else {
        IR_ArrayType* atype = dynamic_cast<IR_ArrayType*>(&initList->getType());
        int byteSize;

        if ( atype ) {
          byteSize = atype->getBaseType().sizeOf();
        } else {
          byteSize = e->getType().sizeOf();
        }

        DOUT( "size: " << byteSize << " offset: " << offset + additionalOffset
          << " type: " << IR_Debug::getShortDebugDump( e->getType() ) << endl );
        splitByArea( offset + additionalOffset, offset + additionalOffset + byteSize );
      }
  };

  if ( isArrayType( type ) ) {
    DOUT( "Array type!" << endl );
    const map<IR_Integer, IR_Exp*> &initExps = initList->getArrayInitExps();
    for( auto const& [index, exp] : initExps ) {
      DOUT( "Index " << index.getIntValue() << ": "  << endl );
      parseExpression(exp);
    }
  } else { // struct type
    DOUT( "Struct type!" << endl );
    const map<IR_Symbol*, IR_Exp*> &initExps = initList->getStructInitExps();
    for( auto const& [symbol, exp] : initExps ) {
      DOUT( "Symbol " << symbol->getName() << ": "  << endl );

      // For bitfields, multiple symbols can occupy the same word.
      // Some bits in that word might be uninitialized.
      // Thus bitfields will always be zeroed.
      if ( !isBitfieldType( symbol->getType() ) ) {
        parseExpression(exp);
      } else {
        DOUT( "skipping bitfield!" << endl );
      }
    }
  }
}

void UninitializedMemoryArea::splitByArea( unsigned int start, unsigned int end )
{
  DSTART( "void UninitializedMemoryArea::splitByArea("
          "unsigned int, unsigned int)" );
  auto it = mChunks.begin();
  DOUT( "Looking for chunk start: " << start << ", end: " << end << endl );
  for ( ; it != mChunks.end(); ++it ) {
    DOUT( "-> Checking chunk start: " << it->start << ", end: " << it->end <<
          endl );
    if ( it->start <= start && end <= it->end ) {
      break;
    }
  }

  ufAssertT( it != mChunks.end(), "Could not find chunk containing area!" );

  if ( it->start == start && it->end == end ) {
    // Area is equal to the chunk
    // Remove the chunk from the list
    mChunks.erase( it );
  } else if ( it->start == start ) {
    // Area is at the beginning of chunk
    // Move beginning of chunk to the right
    it->start = end;
  } else if ( it->end == end ) {
    // Area is at the end of the chunk.
    // Reduce size of the chunk
    it->end = start;
  } else {
    // The Area is inside the chunk
    // We need to split the chunk and insert a new one

    // Reduce size of the old chunk
    unsigned int oldEnd = it->end;
    it->end = start;

    // Insert the new chunk
    DOUT( "Inserting new chunk: " << end  << " " << oldEnd << endl );
    struct Chunk newChunk = {end, oldEnd};
    mChunks.insert( next( it ), newChunk );
  }
}

void UninitializedMemoryArea::zeroMemory( AddressWithOffset addr, IR_Exp *exp )
{
  DSTART( "void UninitializedMemoryArea::zeroMemory("
          "AddressWithOffset, IR_Exp*)" );

  if ( mChunks.empty() ) return;

  LLIR_Register *baseAddress = ARMINSTRUCTIONS.CreateRegister( "" );
  LLIR_Register *chunkAddress = ARMINSTRUCTIONS.CreateRegister( "" );
  LLIR_Register *offsetReg = ARMINSTRUCTIONS.CreateRegister( "" );

  if ( addr.getOffset() != 0 ) {
    ARMINSTRUCTIONS.insertMOV_ORR( baseAddress, addr.getOffset(), exp );
    ARMINSTRUCTIONS.insertADD( baseAddress, baseAddress, addr.getARegister(), exp );
  } else {
    ARMINSTRUCTIONS.insertMOV( baseAddress, addr.getARegister(), exp );
  }

  auto zeroedRegs = createZeroedRegisters( 4, exp );

  for ( auto chunk : mChunks ) {
    DOUT( "Zeroing Chunk: " << chunk.start << " - " << chunk.end << endl );

    // if ( isConst8( IR_Integer( chunk.start ) ) ) {
    if ( chunk.start < 256) {
      ARMINSTRUCTIONS.insertADD( OPER_AL, chunkAddress, baseAddress, chunk.start, exp );
    } else {
      ARMINSTRUCTIONS.insertMOV_ORR( offsetReg, chunk.start, exp );
      ARMINSTRUCTIONS.insertADD( chunkAddress, baseAddress, offsetReg, exp );
    }
    initWithZero( chunkAddress, chunk.end - chunk.start, addr.getOffset() + chunk.start,
              exp, zeroedRegs );
  }

  delete zeroedRegs;
}

COST UninitializedMemoryArea::zeroMemoryCost( unsigned int alignment )
{
  DSTART( "COST UninitializedMemoryArea::zeroMemoryCost( alignment addr )" );
  COST cost = 0;
  if ( mChunks.empty() ) return( cost );

  cost = ARMINSTRUCTIONS.insertMOV_ORR_Cost( alignment ) + CT( INS_ADD_32 );

  for ( auto chunk : mChunks ) {
    cost += ARMINSTRUCTIONS.insertMOV_ORR_Cost( chunk.start ) + CT( INS_ADD_32 );
    cost += initWithZeroCost( chunk.end - chunk.start, alignment + chunk.start );
  }

  return( cost );
}

//#######################################################################
//
// component type (stucts) helper function definitions
//
//#######################################################################

/*!
  Returns, whether the exp denotes a composed type element.
 */
static inline bool isComponentExp( const IR_Exp &exp )
{
  const IR_SymbolExp * const sexp = dynamic_cast<const IR_SymbolExp*>( &exp );
  if ( sexp ) {
    IR_ComponentAccessExp * const cexp = dynamic_cast<IR_ComponentAccessExp*>(
                                           sexp->getParent() );

    return cexp && &cexp->getComponentExp() == sexp;
  } else {
    return false;
  }
}

/*!
  If the symExp denotes a composed type element, this method returns the type
  of the composed type, else 0 is returned.
 */
static inline IR_ComposedType *getParentComposedType( const IR_SymbolExp &symExp )
{
  IR_ComponentAccessExp * const cexp = dynamic_cast<IR_ComponentAccessExp*>(
                                         symExp.getParent() );

  if ( cexp ) {
    IR_ComposedType * const directAccess = dynamic_cast<IR_ComposedType*>(
                                             &cexp->getBaseExp().getType() );
    IR_PointerType * const indirectAccess = dynamic_cast<IR_PointerType*>(
                                             &cexp->getBaseExp().getType() );
    IR_ComposedType * const compType = ( directAccess ? directAccess
      : dynamic_cast<IR_ComposedType*>( &indirectAccess->getBaseType() ) );
    ufAssertT( compType, "Could not determine composed type!" );

    return compType;
  } else {
    return 0;
  }
}
