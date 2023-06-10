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
  copyComposedTypeCost computes the cost of instructions copying a composed type
  object from one memory location to another one.
*/
COST copyComposedTypeCost( const IR_ComposedType &t )
{
  DSTART( "COST copyComposedTypeCost(const IR_ComposedType&)" );

  unsigned int composedTypeSize = Stack::getStackSize( &t );
  COST cost = 2 * TC13::OperationFormat::SAA_1.getSize();

  while ( composedTypeSize > 0 ) {

    int elementOffset = 0;

    if ( composedTypeSize == 1 ) {
      elementOffset = charBytes;
      cost +=
        TC13::OperationFormat::DAC10BOA.getSize() +
        TC13::OperationFormat::AC10DBOA_1.getSize();
    } else

    if ( ( composedTypeSize >= 2 ) && ( composedTypeSize < 4 ) ) {
      elementOffset = shortBytes;
      cost +=
        TC13::OperationFormat::DAC10BOA.getSize() +
        TC13::OperationFormat::AC10DBOA_1.getSize();
    } else

    if ( ( composedTypeSize >= 4 ) && ( composedTypeSize < 8 ) ) {
      elementOffset = intBytes;
      cost +=
        TC13::OperationFormat::DAC16BOA.getSize() +
        TC13::OperationFormat::AC16DBOA.getSize();
    } else {
      elementOffset = 2 * intBytes;
      cost +=
        TC13::OperationFormat::EAC10BOA.getSize() +
        TC13::OperationFormat::AC10EBOA.getSize();
    }

    composedTypeSize -= elementOffset;
  }

  return( cost );
};


/*
  brief copyComposedType copies a composed type object from one memory location
  to another one.
*/
void copyComposedType( const IR_ComposedType &t,
                       LLIR_Register *source, const TC_ARegV &src,
                       int source_offset,
                       LLIR_Register *target, const TC_ARegV &tgt,
                       int target_offset,
                       LLIR_BB &targetBB, LLIR_Instruction *insertAfter,
                       WIR_BasicBlock &b,
                       list<reference_wrapper<WIR_Instruction>>::const_iterator pos )
{
  DSTART(
    "void copyComposedType(const IR_ComposedType&, LLIR_Register*, const TC_ARegV&, int, LLIR_Register*, const TC_ARegV&, int, LLIR_BB&, LLIR_Instruction*, WIR_BasicBlock&, list<reference_wrapper<WIR_Instruction> >::const_iterator)" );

  ufAssertT( (
    !insertAfter || insertAfter->GetBB() == &targetBB ),
    "'insertAfter' must be within 'targetBB'" );
  ufAssertT( isAReg( source->GetName() ), "Invalid parameter!" );
  ufAssertT( isAReg( target->GetName() ), "Invalid parameter!" );

  // LLIR
  // Check whether we should update the current instruction of TCCODESEL after
  // having added all the generated instructions.
  const bool updateCurrentInstruction =
    ( TCCODESEL->getLastLLIRBB() == &targetBB );

  // Define a shorthand for inserting instructions one after the other
  LLIR_Instruction *lastInserted = insertAfter;
  #define IADD( new_ins )                     \
  { LLIR_Instruction *ins = new_ins;          \
    targetBB.InsertIns( ins, lastInserted );  \
    lastInserted = ins; }                     \

  const unsigned int composedTypeSize = Stack::getStackSize( &t );

  // If possible, we use offsets for addressing the load/store targets.
  // Otherwise, we load the addresses into new registers and use postincrement
  // load/stores.
  const int minSourceOffset = ( source_offset );
  const int minTargetOffset = ( target_offset );
  const int maxSourceOffset = ( source_offset + (signed int)composedTypeSize );
  const int maxTargetOffset = ( target_offset + (signed int)composedTypeSize );
  const bool useOffsetsForSource =
    ( minSourceOffset >= TC_Const10_Signed::getMinValue( 10 ) ) &&
    ( maxSourceOffset <= TC_Const10_Signed::getMaxValue( 10 ) );
  const bool useOffsetsForTarget =
    ( minTargetOffset >= TC_Const10_Signed::getMinValue( 10 ) ) &&
    ( maxTargetOffset <= TC_Const10_Signed::getMaxValue( 10 ) );

  // Determine a pointer to the memory location of the target composed object.
  LLIR_Register *compTargetBase = nullptr;
  auto &compTgtBase = TCINSTRUCTIONS.createAReg();

  if ( !useOffsetsForTarget ) {
    compTargetBase = TCINSTRUCTIONS.CreateRegister( "", true );

    // Get a pointer to the stack location of the composed type argument
    if ( target_offset == 0 ) {
      // We must make a copy of the base pointer first, because it may not be
      // modified by our copy operation (might be stack pointer, pointer areg,
      // ...).

      // LLIR
      IADD( insMOV_AA( compTargetBase, target ) );

      // WIR
      b.insertInstruction(
        pos,
        { { TC13::OpCode::MOV_AA,
            TCCODESEL->getGenerate16BitOperations() ?
              TC13::OperationFormat::SAA_1 : TC13::OperationFormat::AA,
            new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
            new WIR_RegisterParameter( tgt, WIR_Usage::use ) } } );
    } else {
      if ( ( target_offset >= TC_Const16_Signed::getMinValue( 16 ) ) &&
           ( target_offset <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
        // LLIR
        IADD( insLEA( compTargetBase, OPER_BASE, target, target_offset ) );

        // WIR
        b.insertInstruction(
          pos,
          { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
              new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
              new WIR_RegisterParameter( tgt, WIR_Usage::use ),
              new TC_Const16_Signed( target_offset ) } } );
      } else {
        auto p = TCINSTRUCTIONS.splitOffset( target_offset );

        // LLIR
        IADD( insMOV_AA( compTargetBase, target ) );
        IADD( insADDIH_A( compTargetBase, compTargetBase, p.first ) );
        IADD( insLEA( compTargetBase, OPER_BASE, compTargetBase, p.second ) );

        // WIR
        b.insertInstruction(
          pos,
          { { TC13::OpCode::MOV_AA,
              TCCODESEL->getGenerate16BitOperations() ?
                TC13::OperationFormat::SAA_1 : TC13::OperationFormat::AA,
              new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
              new WIR_RegisterParameter( tgt, WIR_Usage::use ) } } );
        b.insertInstruction(
          pos,
          { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
              new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
              new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
              new TC_Const16_Unsigned( p.first ) } } );
        b.insertInstruction(
          pos,
          { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
              new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
              new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
              new TC_Const16_Signed( p.second ) } } );
      }
    }
  }

  // Determine a pointer to the memory location of the source composed object.
  LLIR_Register *compSourceBase = nullptr;
  auto &compSrcBase = TCINSTRUCTIONS.createAReg();

  if ( !useOffsetsForSource ) {
    compSourceBase = TCINSTRUCTIONS.CreateRegister( "", true );

    if ( source_offset == 0 ) {
      // We must make a copy of the base pointer first, because it may not be
      // modified by our copy operation (might be stack pointer, pointer areg,
      // ...).

      // LLIR
      IADD( insMOV_AA( compSourceBase, source ) );

      // WIR
      b.insertInstruction(
        pos,
        { { TC13::OpCode::MOV_AA,
            TCCODESEL->getGenerate16BitOperations() ?
              TC13::OperationFormat::SAA_1 : TC13::OperationFormat::AA,
            new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
            new WIR_RegisterParameter( src, WIR_Usage::use ) } } );
    } else {
      if ( ( source_offset >= TC_Const16_Signed::getMinValue( 16 ) ) &&
           ( source_offset <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
        // LLIR
        IADD( insLEA( compSourceBase, OPER_BASE, source, source_offset ) );

        // WIR
        b.insertInstruction(
          pos,
          { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
              new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
              new WIR_RegisterParameter( src, WIR_Usage::use ),
              new TC_Const16_Signed( source_offset ) } } );
      } else {
        auto p = TCINSTRUCTIONS.splitOffset( source_offset );

        // LLIR
        IADD( insMOV_AA( compSourceBase, source ) );
        IADD( insADDIH_A( compSourceBase, compSourceBase, p.first ) );
        IADD( insLEA( compSourceBase, OPER_BASE, compSourceBase, p.second ) );

        // WIR
        b.insertInstruction(
          pos,
          { { TC13::OpCode::MOV_AA,
              TCCODESEL->getGenerate16BitOperations() ?
                TC13::OperationFormat::SAA_1 : TC13::OperationFormat::AA,
              new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
              new WIR_RegisterParameter( src, WIR_Usage::use ) } } );
        b.insertInstruction(
          pos,
          { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
              new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
              new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
              new TC_Const16_Unsigned( p.first ) } } );
        b.insertInstruction(
          pos,
          { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
              new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
              new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
              new TC_Const16_Signed( p.second ) } } );
      }
    }
  }

  // Temporary registers for copying the struct.
  LLIR_Register *lhsreg = TCINSTRUCTIONS.CreateRegister( "", false );
  LLIR_Register *lhsEReg = TCINSTRUCTIONS.CreateERegister( "" );
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  auto &tmpEReg = TCINSTRUCTIONS.createEReg();

  // After computing the address and adding the corresponding instructions, the
  // composed type must be loaded from memory and stored on the stack of the
  // function.
  // TODO: For _really_ huge structures, we should generate a loop here instead
  //       of emitting all the commands as straight-line code.
  unsigned int remainingBytes = composedTypeSize;
  while ( remainingBytes > 0 ) {

    const int currentPosition = composedTypeSize - remainingBytes;
    int elementOffset = 0;

    if ( remainingBytes == 1 ) {

      elementOffset = charBytes;

      if ( useOffsetsForSource ) {
        auto off = source_offset + currentPosition;

        // LLIR
        IADD( insLD_B( lhsreg, OPER_BASE, source, off ) );

        // WIR
        if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LD_B, TC13::OperationFormat::DAC10BOA,
                new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                new WIR_RegisterParameter( src, WIR_Usage::use ),
                new TC_Const10_Signed( off ) } } );
        else {
          auto &Ax = TCINSTRUCTIONS.createAReg();
          auto p = TCINSTRUCTIONS.splitOffset( off );

          b.insertInstruction(
            pos,
            { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                new WIR_RegisterParameter( src, WIR_Usage::use ),
                new TC_Const16_Unsigned( p.first ) } } );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_B, TC13::OperationFormat::DAC10BOA,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const10_Signed( p.second ) } } );
          else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_B, TC13::OperationFormat::DAC10BOA,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const10_Signed( 0 ) } } );
          }
        }
      } else {
        // LLIR
        IADD( insLD_B( lhsreg, OPER_POSTINC, compSourceBase, elementOffset ) );

        // WIR
        if ( ( elementOffset >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( elementOffset <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LD_B, TC13::OperationFormat::DAC10PIA,
                new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::defuse ),
                new TC_Const10_Signed( elementOffset ) } } );
        else

        if ( ( elementOffset >= TC_Const16_Signed::getMinValue( 16 ) ) &&
             ( elementOffset <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LD_B, TC13::OperationFormat::DAC10BOA,
                new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                new TC_Const10_Signed( 0 ) } } );
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                new TC_Const16_Signed( elementOffset ) } } );
        } else {
          auto p = TCINSTRUCTIONS.splitOffset( elementOffset );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_B, TC13::OperationFormat::DAC10PIA,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::defuse ),
                  new TC_Const10_Signed( p.second ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
          } else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_B, TC13::OperationFormat::DAC10BOA,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const10_Signed( 0 ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
          }
        }
      }

      if ( useOffsetsForTarget ) {
        auto off = target_offset + currentPosition;

        // LLIR
        IADD( insST_B( OPER_BASE, target, off, lhsreg ) );

        // WIR
        if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::ST_B, TC13::OperationFormat::AC10DBOA_1,
                new WIR_RegisterParameter( tgt, WIR_Usage::use ),
                new TC_Const10_Signed( off ),
                new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
        else {
          auto &Ax = TCINSTRUCTIONS.createAReg();
          auto p = TCINSTRUCTIONS.splitOffset( off );

          b.insertInstruction(
            pos,
            { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                new WIR_RegisterParameter( tgt, WIR_Usage::use ),
                new TC_Const16_Unsigned( p.first ) } } );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_B, TC13::OperationFormat::AC10DBOA_1,
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const10_Signed( p.second ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
          else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_B, TC13::OperationFormat::AC10DBOA_1,
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const10_Signed( 0 ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
          }
        }
      } else {
        // LLIR
        IADD( insST_B( OPER_POSTINC, compTargetBase, elementOffset, lhsreg ) );

        // WIR
        if ( ( elementOffset >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( elementOffset <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::ST_B, TC13::OperationFormat::AC10DPIA_1,
                new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::defuse ),
                new TC_Const10_Signed( elementOffset ),
                new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
        else

        if ( ( elementOffset >= TC_Const16_Signed::getMinValue( 16 ) ) &&
             ( elementOffset <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
          b.insertInstruction(
            pos,
            { { TC13::OpCode::ST_B, TC13::OperationFormat::AC10DBOA_1,
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                new TC_Const10_Signed( 0 ),
                new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                new TC_Const16_Signed( elementOffset ) } } );
        } else {
          auto p = TCINSTRUCTIONS.splitOffset( elementOffset );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_B, TC13::OperationFormat::AC10DPIA_1,
                  new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::defuse ),
                  new TC_Const10_Signed( p.second ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
          } else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_B, TC13::OperationFormat::AC10DBOA_1,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const10_Signed( 0 ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
          }
        }
      }
    } else

    if ( remainingBytes == 2 ) {

      elementOffset = shortBytes;

      if ( useOffsetsForSource ) {
        auto off = source_offset + currentPosition;

        // LLIR
        IADD( insLD_H( lhsreg, OPER_BASE, source, off ) );

        // WIR
        if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LD_H, TC13::OperationFormat::DAC10BOA,
                new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                new WIR_RegisterParameter( src, WIR_Usage::use ),
                new TC_Const10_Signed( off ) } } );
        else {
          auto &Ax = TCINSTRUCTIONS.createAReg();
          auto p = TCINSTRUCTIONS.splitOffset( off );

          b.insertInstruction(
            pos,
            { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                new WIR_RegisterParameter( src, WIR_Usage::use ),
                new TC_Const16_Unsigned( p.first ) } } );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_H, TC13::OperationFormat::DAC10BOA,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const10_Signed( p.second ) } } );
          else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_H, TC13::OperationFormat::DAC10BOA,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const10_Signed( 0 ) } } );
          }
        }
      } else {
        // LLIR
        IADD( insLD_H( lhsreg, OPER_POSTINC, compSourceBase, elementOffset ) );

        // WIR
        if ( ( elementOffset >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( elementOffset <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LD_H, TC13::OperationFormat::DAC10PIA,
                new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::defuse ),
                new TC_Const10_Signed( elementOffset ) } } );
        else

        if ( ( elementOffset >= TC_Const16_Signed::getMinValue( 16 ) ) &&
             ( elementOffset <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LD_H, TC13::OperationFormat::DAC10BOA,
                new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                new TC_Const10_Signed( 0 ) } } );
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                new TC_Const16_Signed( elementOffset ) } } );
        } else {
          auto p = TCINSTRUCTIONS.splitOffset( elementOffset );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_H, TC13::OperationFormat::DAC10PIA,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::defuse ),
                  new TC_Const10_Signed( p.second ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
          } else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_H, TC13::OperationFormat::DAC10BOA,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const10_Signed( 0 ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
          }
        }
      }

      if ( useOffsetsForTarget ) {
        auto off = target_offset + currentPosition;

        // LLIR
        IADD( insST_H( OPER_BASE, target, off, lhsreg ) );

        // WIR
        if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::ST_H, TC13::OperationFormat::AC10DBOA_1,
                new WIR_RegisterParameter( tgt, WIR_Usage::use ),
                new TC_Const10_Signed( off ),
                new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
        else {
          auto &Ax = TCINSTRUCTIONS.createAReg();
          auto p = TCINSTRUCTIONS.splitOffset( off );

          b.insertInstruction(
            pos,
            { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                new WIR_RegisterParameter( tgt, WIR_Usage::use ),
                new TC_Const16_Unsigned( p.first ) } } );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_H, TC13::OperationFormat::AC10DBOA_1,
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const10_Signed( p.second ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
          else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_H, TC13::OperationFormat::AC10DBOA_1,
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const10_Signed( 0 ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
          }
        }
      } else {
        // LLIR
        IADD( insST_H( OPER_POSTINC, compTargetBase, elementOffset, lhsreg ) );

        // WIR
        if ( ( elementOffset >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( elementOffset <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::ST_H, TC13::OperationFormat::AC10DPIA_1,
                new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::defuse ),
                new TC_Const10_Signed( elementOffset ),
                new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
        else

        if ( ( elementOffset >= TC_Const16_Signed::getMinValue( 16 ) ) &&
             ( elementOffset <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
          b.insertInstruction(
            pos,
            { { TC13::OpCode::ST_H, TC13::OperationFormat::AC10DBOA_1,
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                new TC_Const10_Signed( 0 ),
                new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                new TC_Const16_Signed( elementOffset ) } } );
        } else {
          auto p = TCINSTRUCTIONS.splitOffset( elementOffset );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_H, TC13::OperationFormat::AC10DPIA_1,
                  new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::defuse ),
                  new TC_Const10_Signed( p.second ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
          } else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_H, TC13::OperationFormat::AC10DBOA_1,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const10_Signed( 0 ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
          }
        }
      }
    } else

    if ( remainingBytes == 4 ) {

      elementOffset = intBytes;

      if ( useOffsetsForSource ) {
        auto off = source_offset + currentPosition;

        // LLIR
        IADD( insLD_W( lhsreg, OPER_BASE, source, off ) );

        // WIR
        if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
             ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LD_W, TC13::OperationFormat::DAC16BOA,
                new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                new WIR_RegisterParameter( src, WIR_Usage::use ),
                new TC_Const16_Signed( off ) } } );
        else {
          auto &Ax = TCINSTRUCTIONS.createAReg();
          auto p = TCINSTRUCTIONS.splitOffset( off );

          b.insertInstruction(
            pos,
            { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                new WIR_RegisterParameter( src, WIR_Usage::use ),
                new TC_Const16_Unsigned( p.first ) } } );
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LD_W, TC13::OperationFormat::DAC16BOA,
                new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                new TC_Const16_Signed( p.second ) } } );
        }
      } else {
        // LLIR
        IADD( insLD_W( lhsreg, OPER_POSTINC, compSourceBase, elementOffset ) );

        // WIR
        if ( ( elementOffset >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( elementOffset <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LD_W, TC13::OperationFormat::DAC10PIA,
                new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::defuse ),
                new TC_Const10_Signed( elementOffset ) } } );
        else

        if ( ( elementOffset >= TC_Const16_Signed::getMinValue( 16 ) ) &&
             ( elementOffset <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LD_W, TC13::OperationFormat::DAC16BOA,
                new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                new TC_Const16_Signed( 0 ) } } );
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                new TC_Const16_Signed( elementOffset ) } } );
        } else {
          auto p = TCINSTRUCTIONS.splitOffset( elementOffset );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_W, TC13::OperationFormat::DAC10PIA,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::defuse ),
                  new TC_Const10_Signed( p.second ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
          } else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_W, TC13::OperationFormat::DAC16BOA,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const16_Signed( 0 ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
          }
        }
      }

      if ( useOffsetsForTarget ) {
        auto off = target_offset + currentPosition;

        // LLIR
        IADD( insST_W( OPER_BASE, target, off, lhsreg ) );

        // WIR
        if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
             ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::ST_W, TC13::OperationFormat::AC16DBOA,
                new WIR_RegisterParameter( tgt, WIR_Usage::use ),
                new TC_Const16_Signed( off ),
                new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
        else {
          auto &Ax = TCINSTRUCTIONS.createAReg();
          auto p = TCINSTRUCTIONS.splitOffset( off );

          b.insertInstruction(
            pos,
            { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                new WIR_RegisterParameter( tgt, WIR_Usage::use ),
                new TC_Const16_Unsigned( p.first ) } } );
          b.insertInstruction(
            pos,
            { { TC13::OpCode::ST_W, TC13::OperationFormat::AC16DBOA,
                new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                new TC_Const16_Signed( p.second ),
                new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
        }
      } else {
        // LLIR
        IADD( insST_W( OPER_POSTINC, compTargetBase, elementOffset, lhsreg ) );

        // WIR
        if ( ( elementOffset >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( elementOffset <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::ST_W, TC13::OperationFormat::AC10DPIA_1,
                new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::defuse ),
                new TC_Const10_Signed( elementOffset ),
                new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
        else

        if ( ( elementOffset >= TC_Const16_Signed::getMinValue( 16 ) ) &&
             ( elementOffset <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
          b.insertInstruction(
            pos,
            { { TC13::OpCode::ST_W, TC13::OperationFormat::AC16DBOA,
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                new TC_Const16_Signed( 0 ),
                new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                new TC_Const16_Signed( elementOffset ) } } );
        } else {
          auto p = TCINSTRUCTIONS.splitOffset( elementOffset );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_W, TC13::OperationFormat::AC10DPIA_1,
                  new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::defuse ),
                  new TC_Const10_Signed( p.second ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
          } else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_W, TC13::OperationFormat::AC16DBOA,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const16_Signed( 0 ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
          }
        }
      }

    } else {

      ufAssertT( remainingBytes >= 8, "Invalid object size!" );

      elementOffset = 2 * intBytes;

      if ( useOffsetsForSource ) {
        auto off = source_offset + currentPosition;

        // LLIR
        IADD( insLD_D( lhsEReg, OPER_BASE, source, off ) );

        // WIR
        if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LD_D, TC13::OperationFormat::EAC10BOA,
                new WIR_RegisterParameter( tmpEReg, WIR_Usage::def ),
                new WIR_RegisterParameter( src, WIR_Usage::use ),
                new TC_Const10_Signed( off ) } } );
        else {
          auto &Ax = TCINSTRUCTIONS.createAReg();
          auto p = TCINSTRUCTIONS.splitOffset( off );

          b.insertInstruction(
            pos,
            { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                new WIR_RegisterParameter( src, WIR_Usage::use ),
                new TC_Const16_Unsigned( p.first ) } } );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_D, TC13::OperationFormat::EAC10BOA,
                  new WIR_RegisterParameter( tmpEReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const10_Signed( p.second ) } } );
          else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_D, TC13::OperationFormat::EAC10BOA,
                  new WIR_RegisterParameter( tmpEReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const10_Signed( 0 ) } } );
          }
        }
      } else {
        // LLIR
        IADD( insLD_D( lhsEReg, OPER_POSTINC, compSourceBase, elementOffset ) );

        // WIR
        if ( ( elementOffset >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( elementOffset <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LD_D, TC13::OperationFormat::EAC10PIA,
                new WIR_RegisterParameter( tmpEReg, WIR_Usage::def ),
                new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::defuse ),
                new TC_Const10_Signed( elementOffset ) } } );
        else

        if ( ( elementOffset >= TC_Const16_Signed::getMinValue( 16 ) ) &&
             ( elementOffset <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LD_D, TC13::OperationFormat::EAC10BOA,
                new WIR_RegisterParameter( tmpEReg, WIR_Usage::def ),
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                new TC_Const10_Signed( 0 ) } } );
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                new TC_Const16_Signed( elementOffset ) } } );
        } else {
          auto p = TCINSTRUCTIONS.splitOffset( elementOffset );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_D, TC13::OperationFormat::EAC10PIA,
                  new WIR_RegisterParameter( tmpEReg, WIR_Usage::def ),
                  new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::defuse ),
                  new TC_Const10_Signed( p.second ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
          } else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LD_D, TC13::OperationFormat::EAC10BOA,
                  new WIR_RegisterParameter( tmpEReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const10_Signed( 0 ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compSrcBase, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
          }
        }
      }

      if ( useOffsetsForTarget ) {
        auto off = target_offset + currentPosition;

        // LLIR
        IADD( insST_D( OPER_BASE, target, off, lhsEReg ) );

        // WIR
        if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EBOA,
                new WIR_RegisterParameter( tgt, WIR_Usage::use ),
                new TC_Const10_Signed( off ),
                new WIR_RegisterParameter( tmpEReg, WIR_Usage::use ) } } );
        else {
          auto &Ax = TCINSTRUCTIONS.createAReg();
          auto p = TCINSTRUCTIONS.splitOffset( off );

          b.insertInstruction(
            pos,
            { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                new WIR_RegisterParameter( tgt, WIR_Usage::use ),
                new TC_Const16_Unsigned( p.first ) } } );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EBOA,
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const10_Signed( p.second ),
                  new WIR_RegisterParameter( tmpEReg, WIR_Usage::use ) } } );
          else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EBOA,
                  new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                  new TC_Const10_Signed( 0 ),
                  new WIR_RegisterParameter( tmpEReg, WIR_Usage::use ) } } );
          }
        }
      } else {
        // LLIR
        IADD( insST_D( OPER_POSTINC, compTargetBase, elementOffset, lhsEReg ) );

        // WIR
        if ( ( elementOffset >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( elementOffset <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          b.insertInstruction(
            pos,
            { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EPIA,
                new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::defuse ),
                new TC_Const10_Signed( elementOffset ),
                new WIR_RegisterParameter( tmpEReg, WIR_Usage::use ) } } );
        else

        if ( ( elementOffset >= TC_Const16_Signed::getMinValue( 16 ) ) &&
             ( elementOffset <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
          b.insertInstruction(
            pos,
            { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EBOA,
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                new TC_Const10_Signed( 0 ),
                new WIR_RegisterParameter( tmpEReg, WIR_Usage::use ) } } );
          b.insertInstruction(
            pos,
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                new TC_Const16_Signed( elementOffset ) } } );
        } else {
          auto p = TCINSTRUCTIONS.splitOffset( elementOffset );

          if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
               ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EPIA,
                  new WIR_AddressingModeParameter( TC13::AddressingMode::post ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::defuse ),
                  new TC_Const10_Signed( p.second ),
                  new WIR_RegisterParameter( tmpEReg, WIR_Usage::use ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
          } else {
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EBOA,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const10_Signed( 0 ),
                  new WIR_RegisterParameter( tmpEReg, WIR_Usage::use ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const16_Unsigned( p.first ) } } );
            b.insertInstruction(
              pos,
              { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::def ),
                  new WIR_RegisterParameter( compTgtBase, WIR_Usage::use ),
                  new TC_Const16_Signed( p.second ) } } );
          }
        }
      }

    }

    remainingBytes -= elementOffset;
  }

  #undef IADD

  // Inform the code selector about the new instructions if neccessary.
  if ( updateCurrentInstruction )
    TCCODESEL->setCurrentInstruction( lastInserted );
};


/*
  copyComposedOnStackCost computes the cost of instructions copying a composed
  type onto the stack.
*/
COST copyComposedOnStackCost( const IR_Symbol &composed )
{
  DSTART( "COST copyComposedOnStackCost(const IR_Symbol&)" );

  COST cost = 0;

  auto *compType = dynamic_cast<IR_ComposedType *>( &composed.getType() );

  ufAssertT( compType, "Invalid argument!" );
  ufAssertT(
    !TCCODESEL->getStack()->getComposedPushCostAdded( &composed ),
    "Given push cost was already accounted for!" );

  const unsigned int composedTypeSize = Stack::getStackSize( compType );

  if ( TCCODESEL->getStack()->isSymbolRegSet( composed ) ) {
    if ( ( composedTypeSize <= 4 ) &&
         ( TCCODESEL->getStack()->getArgumentPos( &composed ) <= 4 ) )
      cost += TC13::OperationFormat::AC16DBOA.getSize();
    else

    if ( ( composedTypeSize >= 5 ) && ( composedTypeSize <= 8 ) &&
         ( TCCODESEL->getStack()->getArgumentPos( &composed ) <= 3 ) )
      cost += TC13::OperationFormat::AC10EBOA.getSize();
    else
      cost += copyComposedTypeCost( *compType );
  } else

  if ( composedTypeSize > 8 )
    cost +=
      TC13::OperationFormat::AAC16BOA.getSize() +
      copyComposedTypeCost( *compType );

  // Mark that the costs were already considered for this argument symbol.
  TCCODESEL->getStack()->setComposedPushCostAdded( &composed );

  return( cost );
};


/*
  copyComposedOnStack copies an entire composed type onto the stack.
*/
void copyComposedOnStack( const IR_Symbol &composed, LLIR_Function &f )
{
  DSTART( "void copyComposedOnStack(const IR_Symbol&, LLIR_Function&)" );

  auto *compType = dynamic_cast<IR_ComposedType *>( &composed.getType() );

  ufAssertT( compType, "Invalid argument!" );
  ufAssertT(
    !TCCODESEL->getStack()->getComposedPushed( &composed ),
    "Given symbol was already copied to the stack!" );
  ufAssertT(
    composed.getSymbolTable().getFunction(),
    "'composed' is no function parameter!" );

  // Determine stack location of the struct inside the stack of the callee.
  int target_offset =
    TCCODESEL->getStack()->getComposedParameterBufferOffset( &composed );
  const unsigned int composedTypeSize = Stack::getStackSize( compType );

  // LLIR
  LLIR_Register *target_areg = TCINSTRUCTIONS.CreateRegister( PHREG_SP, true );

  // WIR
  auto &sp = TCINSTRUCTIONS.createAReg();
  TC179x_wirFct->insertPrecolor( sp, TC179x_wirProc->SP() );

  // Check whether the composed object itself or its address was passed in a
  // register.
  const int paramStackOffset = TCCODESEL->getStack()->getSymbolOffset( &composed );
  if ( paramStackOffset < 0 ) {

    // Copy from reg/ereg or from memory, depending on struct size and argument
    // position in the argument list of the function.
    if ( ( composedTypeSize <= 4 ) &&
         ( TCCODESEL->getStack()->getArgumentPos( &composed ) <= 4 ) ) {

      // LLIR
      // Register that contains the struct, as passed by the calling function.
      LLIR_Register &source_reg = getFunctionArgumentRegister( composed );
      f.GetFirstBB()->InsertIns(
        insST_W( OPER_BASE, target_areg, target_offset, &source_reg ) );

      // WIR
      auto &srcReg = dynamic_cast<TC_DRegV &>( getFctArgReg( composed ) );

      if ( ( target_offset >= TC_Const16_Signed::getMinValue( 16 ) ) &&
           ( target_offset <= TC_Const16_Signed::getMaxValue( 16 ) ) )
        TC179x_wirFct->begin()->get().pushFrontInstruction(
          { { TC13::OpCode::ST_W, TC13::OperationFormat::AC16DBOA,
              new WIR_RegisterParameter( sp, WIR_Usage::use ),
              new TC_Const16_Signed( target_offset ),
              new WIR_RegisterParameter( srcReg, WIR_Usage::use ) } } );
      else {
        auto &Ax = TCINSTRUCTIONS.createAReg();
        auto p = TCINSTRUCTIONS.splitOffset( target_offset );

        TC179x_wirFct->begin()->get().pushFrontInstruction(
          { { TC13::OpCode::ST_W, TC13::OperationFormat::AC16DBOA,
              new WIR_RegisterParameter( Ax, WIR_Usage::use ),
              new TC_Const16_Signed( p.second ),
              new WIR_RegisterParameter( srcReg, WIR_Usage::use ) } } );
        TC179x_wirFct->begin()->get().pushFrontInstruction(
          { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
              new WIR_RegisterParameter( Ax, WIR_Usage::def ),
              new WIR_RegisterParameter( sp, WIR_Usage::use ),
              new TC_Const16_Unsigned( p.first ) } } );
      }
    } else

    if ( ( composedTypeSize >= 5 ) && ( composedTypeSize <= 8 ) &&
         ( TCCODESEL->getStack()->getArgumentPos( &composed ) <= 3 ) ) {

      // LLIR
      // Register that contains the struct, as passed by the calling function.
      LLIR_Register &source_ereg = getFunctionArgumentRegister( composed );
      f.GetFirstBB()->InsertIns(
        insST_D( OPER_BASE, target_areg, target_offset, &source_ereg ) );

      // WIR
      auto &srcReg = dynamic_cast<TC_ERegV &>( getFctArgReg( composed ) );

      if ( ( target_offset >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( target_offset <= TC_Const10_Signed::getMaxValue( 10 ) ) )
        TC179x_wirFct->begin()->get().pushFrontInstruction(
          { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EBOA,
              new WIR_RegisterParameter( sp, WIR_Usage::use ),
              new TC_Const10_Signed( target_offset ),
              new WIR_RegisterParameter( srcReg, WIR_Usage::use ) } } );
      else {
        auto &Ax = TCINSTRUCTIONS.createAReg();
        auto p = TCINSTRUCTIONS.splitOffset( target_offset );

        if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
             ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
          TC179x_wirFct->begin()->get().pushFrontInstruction(
            { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EBOA,
                new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                new TC_Const10_Signed( p.second ),
                new WIR_RegisterParameter( srcReg, WIR_Usage::use ) } } );
        else {
          TC179x_wirFct->begin()->get().pushFrontInstruction(
            { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EBOA,
                new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                new TC_Const10_Signed( 0 ),
                new WIR_RegisterParameter( srcReg, WIR_Usage::use ) } } );
          TC179x_wirFct->begin()->get().pushFrontInstruction(
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                new WIR_RegisterParameter( Ax, WIR_Usage::def ),
                new WIR_RegisterParameter( Ax, WIR_Usage::use ),
                new TC_Const16_Signed( p.second ) } } );
        }

        TC179x_wirFct->begin()->get().pushFrontInstruction(
          { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
              new WIR_RegisterParameter( Ax, WIR_Usage::def ),
              new WIR_RegisterParameter( sp, WIR_Usage::use ),
              new TC_Const16_Unsigned( p.first ) } } );
      }
    } else {

      // LLIR
      // Location of the struct, as passed by the calling function.
      LLIR_Register &source_areg = getFunctionArgumentRegister( composed );

      // WIR
      auto &srcReg = dynamic_cast<TC_ARegV &>( getFctArgReg( composed ) );

      // Copy the struct to the given location and insert the copy code at the
      // start of the given function.
      copyComposedType(
        *compType, &source_areg, srcReg, 0, target_areg, sp, target_offset,
        *f.GetFirstBB(), nullptr, TC179x_wirFct->begin()->get(),
        TC179x_wirFct->begin()->get().begin() );

    }

  } else {

    // Copy the original struct to a local stack area.
    // This is only neccessary for structs which were passed as pointers, which
    // is the case for structs bigger than 64 bit. Smaller structs which were
    // passed via the stack were passed by value, and thus there is no need to
    // copy them to a new location.
    if ( composedTypeSize > 8 ) {

      // LLIR
      // We need to get the address from the stack and copy the object to the
      // local stack.
      LLIR_Register *areg_address = TCINSTRUCTIONS.CreateRegister( "", true );
      LLIR_Register *stack_pointer = TCINSTRUCTIONS.CreateRegister( PHREG_SP );

      // Get the object's address from the parameter stack.
      LLIR_Instruction *loadAddress =
        insLD_A( areg_address, OPER_BASE, stack_pointer, paramStackOffset );
      f.GetFirstBB()->InsertIns( loadAddress );

      // WIR
      // Get the object's address from the parameter stack.
      auto &aReg = TCINSTRUCTIONS.createAReg();
      auto pos = TC179x_wirFct->begin()->get().begin();

      if ( ( paramStackOffset >= TC_Const16_Signed::getMinValue( 16 ) ) &&
           ( paramStackOffset <= TC_Const16_Signed::getMaxValue( 16 ) ) )
        TC179x_wirFct->begin()->get().insertInstruction(
          pos,
          { { TC13::OpCode::LD_A, TC13::OperationFormat::AAC16BOA,
              new WIR_RegisterParameter( aReg, WIR_Usage::def ),
              new WIR_RegisterParameter( sp, WIR_Usage::use ),
              new TC_Const16_Signed( paramStackOffset ) } } );
      else {
        auto &Ax = TCINSTRUCTIONS.createAReg();
        auto p = TCINSTRUCTIONS.splitOffset( paramStackOffset );

        TC179x_wirFct->begin()->get().insertInstruction(
          pos,
          { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
              new WIR_RegisterParameter( Ax, WIR_Usage::def ),
              new WIR_RegisterParameter( sp, WIR_Usage::use ),
              new TC_Const16_Unsigned( p.first ) } } );
        TC179x_wirFct->begin()->get().insertInstruction(
          pos,
          { { TC13::OpCode::LD_A, TC13::OperationFormat::AAC16BOA,
              new WIR_RegisterParameter( aReg, WIR_Usage::def ),
              new WIR_RegisterParameter( Ax, WIR_Usage::use ),
              new TC_Const16_Signed( p.first ) } } );
      }

      // Load the object into the given stack position from the specified
      // address.
      copyComposedType(
        *compType, areg_address, aReg, 0, target_areg, sp, target_offset,
        *f.GetFirstBB(), loadAddress, TC179x_wirFct->begin()->get(), pos );
    }
  }

  // Mark the composed symbol as copied.
  TCCODESEL->getStack()->setComposedPushed( &composed );
};


include(composed_initlist_prolog.m4)
include(composed_struct_prolog.m4)
