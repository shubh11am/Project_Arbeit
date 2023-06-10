/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file rv32instructionfactory.cc
  @brief This file implements the RISC-V RV32 instruction factory producing WIR
         machine instructions.
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include standard headers
#include <sstream>
#include <string>

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <arch/riscv/rv32imc.h>
#include <wir/wir.h>

// Include back-annotation headers
#include <backannotation/backannotation.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include misc headers
#include <misc/misc.h>

// Include local headers
#include <rv32/rv32codesel.h>
#include <rv32/rv32instructionfactory.h>
#include <rv32/rv32registrar.h>


//
// Preprocessor macros
//

#define ADDDEBUGINFO( __i, __e, __t )                                          \
  if ( __e )                                                                   \
    addDebugInfo( __i, &__e->getStmt(), __t );


//
// Code section
//

using namespace std;
using namespace WIR;


namespace RV32 {

//
// Global variable declaration section
//

extern RV32IMC *RV32_wirProc;
extern WIR_Function *RV32_wirFct;
extern WIR_BasicBlock *RV32_wirBB;


//
// Public class methods
//

/*
  Default constructor.
*/
RV32_InstructionFactory::RV32_InstructionFactory( Configuration &config,
                                                  RV32_CodeSelector &codesel ) :
  mConfig { config },
  mCodesel { codesel },
  m16BitOperations { mConfig.getParam( Configuration::GENERATE_16BIT_OPS ) }
{
  DSTART(
    "RV32_InstructionFactory::RV32_InstructionFactory(Configuration&, "
    "RV32_CodeSelector&)" );
};


/*
  Destructor.
*/
RV32_InstructionFactory::~RV32_InstructionFactory( void )
{
  DSTART( "RV32_InstructionFactory::~RV32_InstructionFactory()" );
};


/*
  Inserts an ADD instruction.

  Exact format: ADD x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertADD( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::RV_RegV &xc,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertADD(const RV_RegV&, const RV_RegV&, "
    "const RV_RegV&, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::ADD, RV32I::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an ADDI instruction.

  Exact format: ADDI x[a] (def), x[b] (use), const12 (RRC12_1)

  Handling of offsets beyond signed 12 bits is included. Exact formats:

  LUI x[a] (def), <upper 20 bits of const12> (RC20_1)
  ADDI x[a] (def), x[a] (use), <lower 12 bits of const12> (RRC12_1)
  ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertADDI( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb, int const12,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertADDI(const RV_RegV&, const RV_RegV&, "
    "int, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  // If both registers are the same and the offset is 0, no code is generated
  // since it has no effect.
  if ( ( xa != xb ) || ( const12 != 0 ) ) {
    if ( ( const12 >= RV_Const12_Signed::getMinValue( 12 ) ) &&
         ( const12 <= RV_Const12_Signed::getMaxValue( 12 ) ) ) {
      auto &i = RV32_wirBB->pushBackInstruction(
        { { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
            new WIR_RegisterParameter( xa, WIR_Usage::def ),
            new WIR_RegisterParameter( xb, WIR_Usage::use ),
            new RV_Const12_Signed( const12 ) } } );

      ADDDEBUGINFO( i, exp, type );
    } else {
      insertLUI_ADDI( xa, const12, exp, type );
      insertADD( xa, xa, xb, exp, type );
    }
  }
};


/*
  Inserts an ADDI instruction.

  Exact format: ADDI x[a] (def), x[b] (use),  %hi(d) (RRL_2)

*/
void RV32_InstructionFactory::insertADDI( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const WIR::WIR_Data &d,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertADDI(const RV_RegV&, const RV_RegV&, "
    "const WIR_BasicBlock&, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRL_2,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_LabelParameter( d ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an AND instruction.

  Exact format: AND x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertAND( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::RV_RegV &xc,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertAND(const RV_RegV&, const RV_RegV&, "
    "const RV_RegV&, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::AND, RV32I::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an ANDI instruction.

  Exact format: ANDI x[a] (def), x[b] (use), const12 (RRC12_1)
*/
void RV32_InstructionFactory::insertANDI( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb, int const12,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertANDI(const RV_RegV&, const RV_RegV&, "
    "int, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::ANDI, RV32I::OperationFormat::RRC12_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new RV_Const12_Signed( const12 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an AUIPC instruction.

  Exact format: AUIPC x[a] (def), uconst20 (RC20_1)
*/
void RV32_InstructionFactory::insertAUIPC( const WIR::RV_RegV &xa,
                                           unsigned int uconst20,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::AUIPC, RV32I::OperationFormat::RC20_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new RV_Const20_Unsigned( uconst20 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a BEQ instruction.

  Exact format: BEQ x[a] (use), x[b] (use), disp12 (RRL_1)
*/
void RV32_InstructionFactory::insertBEQ( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::WIR_BasicBlock &disp12,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertBEQ(const RV_RegV&, const RV_RegV&, "
    "const WIR_BasicBlock&, const IR_Exp*, RV32_InstructionFactory::StmtType) "
    "const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::BEQ, RV32I::OperationFormat::RRL_1,
        new WIR_RegisterParameter( xa, WIR_Usage::use ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_LabelParameter( disp12 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a BGE instruction.

  Exact format: BGE x[a] (use), x[b] (use), disp12 (RRL_1)
*/
void RV32_InstructionFactory::insertBGE( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::WIR_BasicBlock &disp12,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertBGE(const RV_RegV&, const RV_RegV&, "
    "const WIR_BasicBlock&, const IR_Exp*, RV32_InstructionFactory::StmtType) "
    "const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::BGE, RV32I::OperationFormat::RRL_1,
        new WIR_RegisterParameter( xa, WIR_Usage::use ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_LabelParameter( disp12 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a BGEU instruction.

  Exact format: BGEU x[a] (use), x[b] (use), disp12 (RRL_1)
*/
void RV32_InstructionFactory::insertBGEU( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const WIR::WIR_BasicBlock &disp12,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::BGEU, RV32I::OperationFormat::RRL_1,
        new WIR_RegisterParameter( xa, WIR_Usage::use ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_LabelParameter( disp12 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a BLT instruction.

  Exact format: BLT x[a] (use), x[b] (use), disp12 (RRL_1)
*/
void RV32_InstructionFactory::insertBLT( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::WIR_BasicBlock &disp12,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertBLT(const RV_RegV&, const RV_RegV&, "
    "const WIR_BasicBlock&, const IR_Exp*, RV32_InstructionFactory::StmtType) "
    "const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::BLT, RV32I::OperationFormat::RRL_1,
        new WIR_RegisterParameter( xa, WIR_Usage::use ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_LabelParameter( disp12 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a BLTU instruction.

  Exact format: BLTU x[a] (use), x[b] (use), disp12 (RRL_1)
*/
void RV32_InstructionFactory::insertBLTU( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const WIR::WIR_BasicBlock &disp12,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::BLTU, RV32I::OperationFormat::RRL_1,
        new WIR_RegisterParameter( xa, WIR_Usage::use ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_LabelParameter( disp12 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a BNE instruction.

  Exact format: BNE x[a] (use), x[b] (use), disp12 (RRL_1)
*/
void RV32_InstructionFactory::insertBNE( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::WIR_BasicBlock &disp12,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertBNE(const RV_RegV&, const RV_RegV&, "
    "const WIR_BasicBlock&, const IR_Exp*, RV32_InstructionFactory::StmtType) "
    "const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::BNE, RV32I::OperationFormat::RRL_1,
        new WIR_RegisterParameter( xa, WIR_Usage::use ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_LabelParameter( disp12 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a JAL instruction.

  Exact format: JAL x[a] (def), disp20 (RL_1)
*/
void RV32_InstructionFactory::insertJAL( const WIR::RV_RegV &xa,
                                         const WIR::WIR_BasicBlock &disp20,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::JAL, RV32I::OperationFormat::RL_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_LabelParameter( disp20 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a JALR instruction.

  Exact format: JALR x[a] (def), const12(x[b] (use)) (RC12R_1)
*/
void RV32_InstructionFactory::insertJALR( const WIR::RV_RegV &xa, int const12,
                                          const WIR::RV_RegV &xb,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::JALR, RV32I::OperationFormat::RC12R_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new RV_Const12_Signed( const12 ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a JALR instruction.

  Exact format: JALR x[a] (def), %lo(disp12)(x[b] (use)) (RLR_1)
*/
void RV32_InstructionFactory::insertJALR( const WIR::RV_RegV &xa,
                                          const WIR::WIR_BasicBlock &disp12,
                                          const WIR::RV_RegV &xb,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::JALR, RV32I::OperationFormat::RLR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_LabelParameter( disp12 ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a JALR instruction.

  Exact format: JALR x[a] (def), x[b] (use), const12 (RRC12_1)
*/
void RV32_InstructionFactory::insertJALR( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb, int const12,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertJALR(const RV_RegV&, const RV_RegV&, "
    "int, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::JALR, RV32I::OperationFormat::RRC12_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new RV_Const12_Signed( const12 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a JALR instruction.

  Exact format: JALR x[a] (def), x[b] (use), %lo(disp12) (RRL_2)
*/
void RV32_InstructionFactory::insertJALR( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const WIR::WIR_BasicBlock &disp12,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::JALR, RV32I::OperationFormat::RRL_2,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_LabelParameter( disp12 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a LB instruction.

  Exact format: LB x[a] (def), const12(x[b] (use)) (RC12R_1)

  Handling of address offsets beyond signed 12 bits is included. Exact
  formats:

  LUI x[a] (def), <upper 20 bits of const12> (RC20_1)
  ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1)
  LB x[a] (def), <lower 12 bits of const12>(x[a] (use)) (RC12R_1)
*/
void RV32_InstructionFactory::insertLB( const WIR::RV_RegV &xa, int const12,
                                        const WIR::RV_RegV &xb,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( ( const12 >= RV_Const12_Signed::getMinValue( 12 ) ) &&
       ( const12 <= RV_Const12_Signed::getMaxValue( 12 ) ) ) {
    auto &i = RV32_wirBB->pushBackInstruction(
      { { RV32I::OpCode::LB, RV32I::OperationFormat::RC12R_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const12_Signed( const12 ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    auto p = splitOffset( const12 );

    insertLUI( xa, p.first, exp );
    insertADD( xa, xa, xb, exp );
    insertLB( xa, p.second, xa, exp );
  }
};


/*
  Inserts a LB instruction.

  Exact format: LB x[a] (def), %lo(d)(x[b] (use)) (RLR_1)
*/
void RV32_InstructionFactory::insertLB( const WIR::RV_RegV &xa,
                                        const WIR::WIR_Data &d,
                                        const WIR::RV_RegV &xb,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::LB, RV32I::OperationFormat::RLR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_LabelParameter( d ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a LBU instruction.

  Exact format: LBU x[a] (def), const12(x[b] (use)) (RC12R_1)

  Handling of address offsets beyond signed 12 bits is included. Exact
  formats:

  LUI x[a] (def), <upper 20 bits of const12> (RC20_1)
  ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1)
  LBU x[a] (def), <lower 12 bits of const12>(x[a] (use)) (RC12R_1)
*/
void RV32_InstructionFactory::insertLBU( const WIR::RV_RegV &xa, int const12,
                                         const WIR::RV_RegV &xb,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( ( const12 >= RV_Const12_Signed::getMinValue( 12 ) ) &&
       ( const12 <= RV_Const12_Signed::getMaxValue( 12 ) ) ) {
    auto &i = RV32_wirBB->pushBackInstruction(
      { { RV32I::OpCode::LBU, RV32I::OperationFormat::RC12R_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const12_Signed( const12 ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    auto p = splitOffset( const12 );

    insertLUI( xa, p.first, exp );
    insertADD( xa, xa, xb, exp );
    insertLBU( xa, p.second, xa, exp );
  }
};


/*
  Inserts a LBU instruction.

  Exact format: LBU x[a] (def), %lo(d)(x[b] (use)) (RLR_1)
*/
void RV32_InstructionFactory::insertLBU( const WIR::RV_RegV &xa,
                                         const WIR::WIR_Data &d,
                                         const WIR::RV_RegV &xb,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::LBU, RV32I::OperationFormat::RLR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_LabelParameter( d ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a LH instruction.

  Exact format: LH x[a] (def), const12(x[b] (use)) (RC12R_1)

  Handling of address offsets beyond signed 12 bits is included. Exact
  formats:

  LUI x[a] (def), <upper 20 bits of const12> (RC20_1)
  ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1)
  LH x[a] (def), <lower 12 bits of const12>(x[a] (use)) (RC12R_1)
*/
void RV32_InstructionFactory::insertLH( const WIR::RV_RegV &xa, int const12,
                                        const WIR::RV_RegV &xb,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( ( const12 >= RV_Const12_Signed::getMinValue( 12 ) ) &&
       ( const12 <= RV_Const12_Signed::getMaxValue( 12 ) ) ) {
    auto &i = RV32_wirBB->pushBackInstruction(
      { { RV32I::OpCode::LH, RV32I::OperationFormat::RC12R_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const12_Signed( const12 ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    auto p = splitOffset( const12 );

    insertLUI( xa, p.first, exp );
    insertADD( xa, xa, xb, exp );
    insertLH( xa, p.second, xa, exp );
  }
};


/*
  Inserts a LH instruction.

  Exact format: LH x[a] (def), %lo(d)(x[b] (use)) (RLR_1)
*/
void RV32_InstructionFactory::insertLH( const WIR::RV_RegV &xa,
                                        const WIR::WIR_Data &d,
                                        const WIR::RV_RegV &xb,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::LH, RV32I::OperationFormat::RLR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_LabelParameter( d ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a LHU instruction.

  Exact format: LHU x[a] (def), const12(x[b] (use)) (RC12R_1)

  Handling of address offsets beyond signed 12 bits is included. Exact
  formats:

  LUI x[a] (def), <upper 20 bits of const12> (RC20_1)
  ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1)
  LHU x[a] (def), <lower 12 bits of const12>(x[a] (use)) (RC12R_1)
*/
void RV32_InstructionFactory::insertLHU( const WIR::RV_RegV &xa, int const12,
                                         const WIR::RV_RegV &xb,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( ( const12 >= RV_Const12_Signed::getMinValue( 12 ) ) &&
       ( const12 <= RV_Const12_Signed::getMaxValue( 12 ) ) ) {
    auto &i = RV32_wirBB->pushBackInstruction(
      { { RV32I::OpCode::LHU, RV32I::OperationFormat::RC12R_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const12_Signed( const12 ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    auto p = splitOffset( const12 );

    insertLUI( xa, p.first, exp );
    insertADD( xa, xa, xb, exp );
    insertLHU( xa, p.second, xa, exp );
  }
};


/*
  Inserts a LHU instruction.

  Exact format: LHU x[a] (def), %lo(d)(x[b] (use)) (RLR_1)
*/
void RV32_InstructionFactory::insertLHU( const WIR::RV_RegV &xa,
                                         const WIR::WIR_Data &d,
                                         const WIR::RV_RegV &xb,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::LHU, RV32I::OperationFormat::RLR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_LabelParameter( d ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a LUI instruction.

  Exact format: LUI x[a] (def), const20 (RC20_1)
*/
void RV32_InstructionFactory::insertLUI( const WIR::RV_RegV &xa,
                                         unsigned int const20,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertLUI(const RV_RegV&, unsigned int, "
    "const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::LUI, RV32I::OperationFormat::RC20_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new RV_Const20_Unsigned( const20 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a LUI instruction.

  Exact format: LUI x[a] (def), %hi(d) (RL_2)
*/
void RV32_InstructionFactory::insertLUI( const WIR::RV_RegV &xa,
                                         const WIR::WIR_Data &d,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::LUI, RV32I::OperationFormat::RL_2,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_LabelParameter( d ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a LW instruction.

  Exact format: LW x[a] (def), const12(x[b] (use)) (RC12R_1)

  Handling of address offsets beyond signed 12 bits is included. Exact
  formats:

  LUI x[a] (def), <upper 20 bits of const12> (RC20_1)
  ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1)
  LW x[a] (def), <lower 12 bits of const12>(x[a] (use)) (RC12R_1)
*/
void RV32_InstructionFactory::insertLW( const WIR::RV_RegV &xa, int const12,
                                        const WIR::RV_RegV &xb,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( ( const12 >= RV_Const12_Signed::getMinValue( 12 ) ) &&
       ( const12 <= RV_Const12_Signed::getMaxValue( 12 ) ) ) {
    auto &i = RV32_wirBB->pushBackInstruction(
      { { RV32I::OpCode::LW, RV32I::OperationFormat::RC12R_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const12_Signed( const12 ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    auto p = splitOffset( const12 );

    insertLUI( xa, p.first, exp );
    insertADD( xa, xa, xb, exp );
    insertLW( xa, p.second, xa, exp );
  }
};


/*
  Inserts a LW instruction.

  Exact format: LW x[a] (def), %lo(d)(x[b] (use)) (RLR_1)
*/
void RV32_InstructionFactory::insertLW( const WIR::RV_RegV &xa,
                                        const WIR::WIR_Data &d,
                                        const WIR::RV_RegV &xb,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::LW, RV32I::OperationFormat::RLR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_LabelParameter( d ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an OR instruction.

  Exact format: OR x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertOR( const WIR::RV_RegV &xa,
                                        const WIR::RV_RegV &xb,
                                        const WIR::RV_RegV &xc,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertOR(const RV_RegV&, const RV_RegV&, "
    "const RV_RegV&, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::OR, RV32I::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an ORI instruction.

  Exact format: ORI x[a] (def), x[b] (use), const12 (RRC12_1)
*/
void RV32_InstructionFactory::insertORI( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb, int const12,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertORI(const RV_RegV&, const RV_RegV&, "
    "int, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::ORI, RV32I::OperationFormat::RRC12_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new RV_Const12_Signed( const12 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a SB instruction.

  Exact format: SB x[a] (use), const12(x[b] (use)) (RC12R_2)

  Handling of address offsets beyond signed 12 bits is included. Exact formats:

  LUI x[z] (def), <upper 20 bits of const12> (RC20_1) \n
  ADD x[z] (def), x[z] (use), x[b] (use) (RRR_1) \n
  SB x[a] (use), <lower 12 bits of const12>(x[z] (use)) (RC12R_1)
*/
void RV32_InstructionFactory::insertSB( const WIR::RV_RegV &xa, int const12,
                                        const WIR::RV_RegV &xb,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( ( const12 >= RV_Const12_Signed::getMinValue( 12 ) ) &&
       ( const12 <= RV_Const12_Signed::getMaxValue( 12 ) ) ) {
    auto &i = RV32_wirBB->pushBackInstruction(
      { { RV32I::OpCode::SB, RV32I::OperationFormat::RC12R_2,
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new RV_Const12_Signed( const12 ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    auto &xz = createReg();
    auto p = splitOffset( const12 );

    insertLUI( xz, p.first, exp );
    insertADD( xz, xz, xb, exp );
    insertSB( xa, p.second, xz, exp );
  }
};


/*
  Inserts a SB instruction.

  Exact format: SB x[a] (use), %lo(d)(x[b] (use)) (RLR_2)
*/
void RV32_InstructionFactory::insertSB( const WIR::RV_RegV &xa,
                                        const WIR::WIR_Data &d,
                                        const WIR::RV_RegV &xb,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::SB, RV32I::OperationFormat::RLR_2,
        new WIR_RegisterParameter( xa, WIR_Usage::use ),
        new WIR_LabelParameter( d ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a SH instruction.

  Exact format: SH x[a] (use), const12(x[b] (use)) (RC12R_2)

  Handling of address offsets beyond signed 12 bits is included. Exact formats:

  LUI x[z] (def), <upper 20 bits of const12> (RC20_1) \n
  ADD x[z] (def), x[z] (use), x[b] (use) (RRR_1) \n
  SH x[a] (use), <lower 12 bits of const12>(x[z] (use)) (RC12R_1)
*/
void RV32_InstructionFactory::insertSH( const WIR::RV_RegV &xa, int const12,
                                        const WIR::RV_RegV &xb,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( ( const12 >= RV_Const12_Signed::getMinValue( 12 ) ) &&
       ( const12 <= RV_Const12_Signed::getMaxValue( 12 ) ) ) {
    auto &i = RV32_wirBB->pushBackInstruction(
      { { RV32I::OpCode::SH, RV32I::OperationFormat::RC12R_2,
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new RV_Const12_Signed( const12 ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    auto &xz = createReg();
    auto p = splitOffset( const12 );

    insertLUI( xz, p.first, exp );
    insertADD( xz, xz, xb, exp );
    insertSH( xa, p.second, xz, exp );
  }
};


/*
  Inserts a SH instruction.

  Exact format: SH x[a] (use), %lo(d)(x[b] (use)) (RLR_2)
*/
void RV32_InstructionFactory::insertSH( const WIR::RV_RegV &xa,
                                        const WIR::WIR_Data &d,
                                        const WIR::RV_RegV &xb,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::SH, RV32I::OperationFormat::RLR_2,
        new WIR_RegisterParameter( xa, WIR_Usage::use ),
        new WIR_LabelParameter( d ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an SLL instruction.

  Exact format: SLL x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertSLL( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::RV_RegV &xc,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertSLL(const RV_RegV&, const RV_RegV&, "
    "const RV_RegV&, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::SLL, RV32I::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an SLLI instruction.

  Exact format: SLLI x[a] (def), x[b] (use), uconst5 (RRC5_1)
*/
void RV32_InstructionFactory::insertSLLI( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          unsigned int uconst5,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new RV_Const5_Unsigned( uconst5 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an SLT instruction.

  Exact format: SLT x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertSLT( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::RV_RegV &xc,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertSLT(const RV_RegV&, const RV_RegV&, "
    "const RV_RegV&, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::SLT, RV32I::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  brief Inserts an SLTI instruction.

  Exact format: SLTI x[a] (def), x[b] (use), const12 (RRC12_1)
*/
void RV32_InstructionFactory::insertSLTI( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb, int const12,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertSLTI(const RV_RegV&, const RV_RegV&, "
    "int, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::SLTI, RV32I::OperationFormat::RRC12_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new RV_Const12_Signed( const12 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an SLTIU instruction.

  Exact format: SLTIU x[a] (def), x[b] (use), const12 (RRC12_1)
*/
void RV32_InstructionFactory::insertSLTIU( const WIR::RV_RegV &xa,
                                           const WIR::RV_RegV &xb, int const12,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertSLTIU(const RV_RegV&, const RV_RegV&, "
    "int, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::SLTIU, RV32I::OperationFormat::RRC12_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new RV_Const12_Signed( const12 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an SLTU instruction.

  Exact format: SLTU x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertSLTU( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const WIR::RV_RegV &xc,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertSLTU(const RV_RegV&, const RV_RegV&, "
    "const RV_RegV&, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::SLTU, RV32I::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an SRA instruction.

  Exact format: SRA x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertSRA( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::RV_RegV &xc,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertSRA(const RV_RegV&, const RV_RegV&, "
    "const RV_RegV&, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::SRA, RV32I::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an SRAI instruction.

  Exact format: SRAI x[a] (def), x[b] (use), uconst5 (RRC5_1)
*/
void RV32_InstructionFactory::insertSRAI( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          unsigned int uconst5,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::SRAI, RV32I::OperationFormat::RRC5_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new RV_Const5_Unsigned( uconst5 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an SRL instruction.

  Exact format: SRL x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertSRL( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::RV_RegV &xc,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::SRL, RV32I::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an SRLI instruction.

  Exact format: SRLI x[a] (def), x[b] (use), uconst5 (RRC5_1)
*/
void RV32_InstructionFactory::insertSRLI( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          unsigned int uconst5,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::SRLI, RV32I::OperationFormat::RRC5_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new RV_Const5_Unsigned( uconst5 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a SUB instruction.

  Exact format: SUB x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertSUB( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::RV_RegV &xc,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertSUB(const RV_RegV&, const RV_RegV&, "
    "const RV_RegV&, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32IMC::OpCode::SUB, RV32IMC::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a SW instruction.

  Exact format: SW x[a] (use), const12(x[b] (use)) (RC12R_2)

  Handling of address offsets beyond signed 12 bits is included. Exact formats:

  LUI x[z] (def), <upper 20 bits of const12> (RC20_1) \n
  ADD x[z] (def), x[z] (use), x[b] (use) (RRR_1) \n
  SW x[a] (use), <lower 12 bits of const12>(x[z] (use)) (RC12R_1)
*/
void RV32_InstructionFactory::insertSW( const WIR::RV_RegV &xa, int const12,
                                        const WIR::RV_RegV &xb,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( ( const12 >= RV_Const12_Signed::getMinValue( 12 ) ) &&
       ( const12 <= RV_Const12_Signed::getMaxValue( 12 ) ) ) {
    auto &i = RV32_wirBB->pushBackInstruction(
      { { RV32I::OpCode::SW, RV32I::OperationFormat::RC12R_2,
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new RV_Const12_Signed( const12 ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    auto &xz = createReg();
    auto p = splitOffset( const12 );

    insertLUI( xz, p.first, exp );
    insertADD( xz, xz, xb, exp );
    insertSW( xa, p.second, xz, exp );
  }
};


/*
  Inserts a SW instruction.

  Exact format: SW x[a] (use), %lo(d)(x[b] (use)) (RLR_2)
*/
void RV32_InstructionFactory::insertSW( const WIR::RV_RegV &xa,
                                        const WIR::WIR_Data &d,
                                        const WIR::RV_RegV &xb,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::SW, RV32I::OperationFormat::RLR_2,
        new WIR_RegisterParameter( xa, WIR_Usage::use ),
        new WIR_LabelParameter( d ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an XOR instruction.

  Exact format: XOR x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertXOR( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::RV_RegV &xc,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertXOR(const RV_RegV&, const RV_RegV&, "
    "const RV_RegV&, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32IMC::OpCode::XOR, RV32IMC::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts an XORI instruction.

  Exact format: XORI x[a] (def), x[b] (use), const12 (RRC12_1)
*/
void RV32_InstructionFactory::insertXORI( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb, int const12,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertXORI(const RV_RegV&, const RV_RegV&, "
    "int, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32IMC::OpCode::XORI, RV32IMC::OperationFormat::RRC12_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new RV_Const12_Signed( const12 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a DIV instruction.

  Exact format: DIV x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertDIV( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::RV_RegV &xc,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertDIV(const RV_RegV&, const RV_RegV&, "
    "const RV_RegV&, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32IM::OpCode::DIV, RV32IM::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a DIVU instruction.

  Exact format: DIVU x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertDIVU( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const WIR::RV_RegV &xc,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32IM::OpCode::DIVU, RV32IM::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a MUL instruction.

  Exact format: MUL x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertMUL( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::RV_RegV &xc,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertMUL(const RV_RegV&, const RV_RegV&, "
    "const RV_RegV&, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32IM::OpCode::MUL, RV32IM::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a MULH instruction.

  Exact format: MULH x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertMULH( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const WIR::RV_RegV &xc,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32IM::OpCode::MULH, RV32IM::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a MULHSU instruction.

  Exact format: MULHSU x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertMULHSU( const WIR::RV_RegV &xa,
                                            const WIR::RV_RegV &xb,
                                            const WIR::RV_RegV &xc,
                                            const IR_Exp *exp,
                                            StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32IM::OpCode::MULHSU, RV32IM::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a MULHU instruction.

  Exact format: MULHU x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertMULHU( const WIR::RV_RegV &xa,
                                           const WIR::RV_RegV &xb,
                                           const WIR::RV_RegV &xc,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32IM::OpCode::MULHU, RV32IM::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a REM instruction.

  Exact format: REM x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertREM( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::RV_RegV &xc,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertREM(const RV_RegV&, const RV_RegV&, "
    "const RV_RegV&, const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32IM::OpCode::REM, RV32IM::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a REMU instruction.

  Exact format: REMU x[a] (def), x[b] (use), x[c] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertREMU( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const WIR::RV_RegV &xc,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32IM::OpCode::REMU, RV32IM::OperationFormat::RRR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ),
        new WIR_RegisterParameter( xc, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.ADD instruction.

  Exact formats:

  C.ADD x[a] (defuse), x[b] (use) (SRR_2)

  or

  ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertCADD( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CADD, RV32IC::OperationFormat::SRR_2,
          new WIR_RegisterParameter( xa, WIR_Usage::defuse ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } :
        WIR_Operation { RV32I::OpCode::ADD, RV32I::OperationFormat::RRR_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.ADDI instruction.

  Exact formats:

  C.ADDI x[a] (defuse), const6 (SRC6_3)

  or

  ADDI x[a] (def), x[a] (use), const6 (RRC12_1)
*/
void RV32_InstructionFactory::insertCADDI( const WIR::RV_RegV &xa, int const6,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CADDI, RV32IC::OperationFormat::SRC6_3,
          new WIR_RegisterParameter( xa, WIR_Usage::defuse ),
          new RV_Const6_Signed( const6 ) } :
        WIR_Operation { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new RV_Const12_Signed( const6 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.ADDI16SP instruction.

  Exact formats:

  C.ADDI16SP sp (defuse), const6 (SRC6_4)

  or

  ADDI sp (def), sp (use), const6 (RRC12_1)
*/
void RV32_InstructionFactory::insertCADDI16SP( int const6,
                                               const IR_Exp *exp,
                                               StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CADDI16SP,
          RV32IC::OperationFormat::SRC6_4,
          new WIR_RegisterParameter(
            RV32::RV32_wirProc->SP(), WIR_Usage::defuse ),
          new RV_Const6_Signed( const6 ) } :
        WIR_Operation { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
          new WIR_RegisterParameter( RV32::RV32_wirProc->SP(), WIR_Usage::def ),
          new WIR_RegisterParameter( RV32::RV32_wirProc->SP(), WIR_Usage::use ),
          new RV_Const12_Signed( const6 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.ADDI4SPN instruction.

  Exact format:

  C.ADDI4SPN x[a] (def), sp (use), uconst8 (SRRC8_1)

  or

  ADDI x[a] (def), sp (use), uconst8 (RRC12_1)
*/
void RV32_InstructionFactory::insertCADDI4SPN( const WIR::RV_RegV &xa,
                                               unsigned int uconst8,
                                               const IR_Exp *exp,
                                               StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CADDI4SPN,
          RV32IC::OperationFormat::SRRC8_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( RV32::RV32_wirProc->SP(), WIR_Usage::use ),
          new RV_Const8_Unsigned( uconst8 ) } :
        WIR_Operation { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( RV32::RV32_wirProc->SP(), WIR_Usage::use ),
          new RV_Const12_Signed( uconst8 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a CALL instruction.

  Exact format: JAL x[1] (use), disp (RL_1)
*/
void RV32_InstructionFactory::insertCALL( const IR_FunctionSymbol &disp,
                                          const std::list<std::reference_wrapper<WIR::WIR_BaseRegister>> &args,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( disp.getFunction() == nullptr ) {
    // The called IR function is an incomplete function with a declaration only
    // but no implementation in the IR.
    string label = disp.getName();
    WIR_System &sys = mCodesel.getSystem();

    if ( !mCodesel.containsWIRFunction( disp ) ) {
      // The WIR system does not yet contain a symbol and WIR function for the
      // current IR function. So, create one.
      WIR_Function &f =
        sys.begin()->get().pushBackFunction( WIR_Function( label ) );
      mCodesel.setWIRFunction( disp, f );
      auto &sym = sys.findSymbol( f );
      sym.setExtern();

      // Determine if the IR function is static.
      bool fctIsStatic =
        ( disp.getType().getStorageClass() == IR_Type::STATIC );

      // Add "global" directive only if function is not static.
      sym.setGlobal( !fctIsStatic );
    }

    // Finally generate the CALL to the external WIR function.
    auto &f = mCodesel.getWIRFunction( disp );
    insertCALL( f, args, exp );

    // Add implicit parameters for proper def-use analysis.
    auto &sym = sys.findSymbol( f );
    if ( sym.isExtern() ) {
      auto &i = RV32_wirBB->getInstructions().back().get();
      auto &o = i.getOperations().back().get();
      o.pushBackParameter( new WIR_RegisterParameter( RV32::RV32_wirProc->x10(),
                                                      WIR_Usage::def,true));
      o.pushBackParameter( new WIR_RegisterParameter( RV32::RV32_wirProc->x11(),
                                                      WIR_Usage::def,true));
    }
  } else
    insertCALL(
      mCodesel.getWIRFunction( disp.getFunction() ), args, exp );
};


/*
  Inserts a CALL instruction.

  Exact format: JAL x[1] (use), disp (RL_1)
*/
void RV32_InstructionFactory::insertCALL( const WIR::WIR_Function &disp,
                                          const std::list<std::reference_wrapper<WIR::WIR_BaseRegister>> &args,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = RV32_wirBB->pushBackInstruction(
  { { RV32I::OpCode::JAL, RV32I::OperationFormat::RL_1,
      new WIR_RegisterParameter( RV32::RV32_wirProc->RA(), WIR_Usage::def ),
      new WIR_LabelParameter( disp ) } } );

  // Add implicit parameters for proper def-use analysis.
  WIR_Operation &callOp = i.begin()->get();
  for ( WIR_BaseRegister &r : args )
    callOp.pushBackParameter(
      new WIR_RegisterParameter( r, WIR_Usage::defuse, true ) );

  callOp.pushBackParameter( new WIR_RegisterParameter( RV32::RV32_wirProc->SP(),
                                                       WIR_Usage::use, true ) );
  callOp.pushBackParameter( new WIR_RegisterParameter( RV32::RV32_wirProc->RA(),
                                                       WIR_Usage::use, true ) );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a CALLI instruction.

  Exact format: JALR x[1], x[a] (use), 0 (RRC12_1)
*/
void RV32_InstructionFactory::insertCALLI( const WIR::RV_RegV &xa,
                                           const std::list<std::reference_wrapper<WIR::WIR_BaseRegister>> &args,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertCALLI(const RV_RegV&, const "
    "list<reference_wrapper<WIR_BaseRegister> >&, const IR_Exp*, "
    "InstructionFactory::StmtType) const" );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CJALR, RV32IC::OperationFormat::SR_1,
          new WIR_RegisterParameter( xa, WIR_Usage::use ) } :
        WIR_Operation { RV32I::OpCode::JALR, RV32I::OperationFormat::RRC12_1,
          new WIR_RegisterParameter( RV32::RV32_wirProc->RA(), WIR_Usage::def ),
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new RV_Const12_Signed( 0 ) } } );

  // Add implicit parameters for proper def-use analysis.
  WIR_Operation &callOp = i.begin()->get();
  for ( WIR_BaseRegister &r : args )
    callOp.pushBackParameter(
      new WIR_RegisterParameter( r, WIR_Usage::defuse, true ) );

  callOp.pushBackParameter( new WIR_RegisterParameter( RV32::RV32_wirProc->SP(),
                                                       WIR_Usage::use, true ) );
  callOp.pushBackParameter( new WIR_RegisterParameter( RV32::RV32_wirProc->RA(),
                                                       WIR_Usage::use, true ) );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.AND instruction.

  Exact formats:

  C.AND x[a] (defuse), x[b] (use) (SRR_2)

  or

  AND x[a] (def), x[a] (use), x[b] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertCAND( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CAND, RV32IC::OperationFormat::SRR_2,
          new WIR_RegisterParameter( xa, WIR_Usage::defuse ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } :
        WIR_Operation { RV32I::OpCode::AND, RV32I::OperationFormat::RRR_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.ANDI instruction.

  Exact formats:

  C.ANDI x[a] (defuse), const6 (SRC6_3)

  or

  ANDI x[a] (def), x[a] (use), const6 (RRC12_1)
*/
void RV32_InstructionFactory::insertCANDI( const WIR::RV_RegV &xa, int const6,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CANDI, RV32IC::OperationFormat::SRC6_3,
          new WIR_RegisterParameter( xa, WIR_Usage::defuse ),
          new RV_Const6_Signed( const6 ) } :
        WIR_Operation { RV32I::OpCode::ANDI, RV32I::OperationFormat::RRC12_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new RV_Const12_Signed( const6 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.BEQZ instruction.

  Exact formats:

  C.BEQZ x[a] (use), disp8 (SRL_1)

  or

  BEQ x[a] (use), x0 (use), disp8 (RRL_1)
*/
void RV32_InstructionFactory::insertCBEQZ( const WIR::RV_RegV &xa,
                                           const WIR::WIR_BasicBlock &disp8,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CBEQZ, RV32IC::OperationFormat::SRL_1,
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new WIR_LabelParameter( disp8 ) } :
        WIR_Operation { RV32I::OpCode::BEQ, RV32I::OperationFormat::RRL_1,
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new WIR_RegisterParameter( RV32::RV32_wirProc->x0(), WIR_Usage::use ),
          new WIR_LabelParameter( disp8 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.BNEZ instruction.

  Exact formats:

  C.BNEZ x[a] (use), disp8 (SRL_1)

  or

  BNE x[a] (use), x0 (use), disp8 (RRL_1)
*/
void RV32_InstructionFactory::insertCBNEZ( const WIR::RV_RegV &xa,
                                           const WIR::WIR_BasicBlock &disp8,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CBNEZ, RV32IC::OperationFormat::SRL_1,
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new WIR_LabelParameter( disp8 ) } :
        WIR_Operation { RV32I::OpCode::BNE, RV32I::OperationFormat::RRL_1,
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new WIR_RegisterParameter( RV32::RV32_wirProc->x0(), WIR_Usage::use ),
          new WIR_LabelParameter( disp8 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.J instruction.

  Exact formats: C.J disp11 (SL_1)

  or

  JAL x0 (def), disp11 (RL_1)
*/
void RV32_InstructionFactory::insertCJ( const WIR::WIR_BasicBlock &disp11,
                                        const IR_Exp *exp,
                                        StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CJ, RV32IC::OperationFormat::SL_1,
          new WIR_LabelParameter( disp11 ) } :
        WIR_Operation { RV32I::OpCode::JAL, RV32I::OperationFormat::RL_1,
          new WIR_RegisterParameter( RV32::RV32_wirProc->x0(), WIR_Usage::def ),
          new WIR_LabelParameter( disp11 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.JAL instruction.

  Exact formats:

  C.JAL disp11 (SL_1)

  or

  JAL x1 (def), disp11 (RL_1)
*/
void RV32_InstructionFactory::insertCJAL( const WIR::WIR_BasicBlock &disp11,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CJAL, RV32IC::OperationFormat::SL_1,
          new WIR_LabelParameter( disp11 ) } :
        WIR_Operation { RV32I::OpCode::JAL, RV32I::OperationFormat::RL_1,
          new WIR_RegisterParameter( RV32::RV32_wirProc->RA(), WIR_Usage::def ),
          new WIR_LabelParameter( disp11 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.JALR instruction.

  Exact formats:

  C.JALR x[a] (use) (SR_1)

  or

  JALR x1 (def), x[a] (use), 0 (RRC12_1)
*/
void RV32_InstructionFactory::insertCJALR( const WIR::RV_RegV &xa,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CJALR, RV32IC::OperationFormat::SR_1,
          new WIR_RegisterParameter( xa, WIR_Usage::use ) } :
        WIR_Operation { RV32I::OpCode::JALR, RV32I::OperationFormat::RRC12_1,
          new WIR_RegisterParameter( RV32::RV32_wirProc->RA(), WIR_Usage::def ),
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new RV_Const12_Signed( 0 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.JR instruction.

  Exact formats:

  C.JR x[a] (use) (SR_1)

  or

  JALR x0 (def), x[a] (use), 0 (RRC12_1)
*/
void RV32_InstructionFactory::insertCJR( const WIR::RV_RegV &xa,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CJR, RV32IC::OperationFormat::SR_1,
          new WIR_RegisterParameter( xa, WIR_Usage::use ) } :
        WIR_Operation { RV32I::OpCode::JALR, RV32I::OperationFormat::RRC12_1,
          new WIR_RegisterParameter( RV32::RV32_wirProc->x0(), WIR_Usage::def ),
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new RV_Const12_Signed( 0 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.LI instruction.

  Exact formats:

  C.LI x[a] (def), const6 (SRC6_1)

  or

  ADDI x[a] (def), x0 (use), const6 (RRC12_1)
*/
void RV32_InstructionFactory::insertCLI( const WIR::RV_RegV &xa, int const6,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CLI, RV32IC::OperationFormat::SRC6_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const6_Signed( const6 ) } :
        WIR_Operation { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( RV32::RV32_wirProc->x0(), WIR_Usage::use ),
          new RV_Const12_Signed( const6 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.LUI instruction.

  Exact formats:

  C.LUI x[a] (def), uconst6 (SRC6_2)

  or

  LUI x[a] (def), sign_extend( uconst6 ) (RC20_1)
*/
void RV32_InstructionFactory::insertCLUI( const WIR::RV_RegV &xa,
                                          unsigned int uconst6,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( m16BitOperations && ( uconst6 != 0 ) ) {
    auto &i = RV32_wirBB->pushBackInstruction(
      { { RV32IC::OpCode::CLUI, RV32IC::OperationFormat::SRC6_2,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const6_Unsigned( uconst6 ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    // Extract least-significant 6 bits from uconst6.
    int val = uconst6 & 0x3F;

    // Get sign bit from position 5.
    int sign = uconst6 & 0x20;

    // Sign-extend most-significant bit positions.
    for ( unsigned int i = 6; i < sizeof( int ) * 8; ++i ) {
      sign <<= 1;
      val |= sign;
    }

    // Generate LUI instruction.
    auto &i = RV32_wirBB->pushBackInstruction(
      { { RV32I::OpCode::LUI, RV32I::OperationFormat::RC20_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const20_Unsigned( val ) } } );

    ADDDEBUGINFO( i, exp, type );
  }
};


/*
  Inserts a C.LW instruction.

  Exact formats:

  C.LW x[a] (def), uconst5(x[b] (use)) (SRC5R_1)

  or

  LW x[a] (def), uconst5(x[b] (use)) (RC12R_1)
*/
void RV32_InstructionFactory::insertCLW( const WIR::RV_RegV &xa,
                                         unsigned int uconst5,
                                         const WIR::RV_RegV &xb,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CLW, RV32IC::OperationFormat::SRC5R_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const5_Unsigned( uconst5 ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } :
        WIR_Operation { RV32I::OpCode::LW, RV32I::OperationFormat::RC12R_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const12_Signed( uconst5 ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.LWSP instruction.

  Exact formats:

  C.LWSP x[a] (def), uconst6(sp (use)) (SRC6R_1)

  or

  LW x[a] (def), uconst6(sp (use)) (RC12R_1)
*/
void RV32_InstructionFactory::insertCLWSP( const WIR::RV_RegV &xa,
                                           unsigned int uconst6,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CLWSP, RV32IC::OperationFormat::SRC6R_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const6_Unsigned( uconst6 ),
          new WIR_RegisterParameter(
            RV32::RV32_wirProc->SP(), WIR_Usage::use ) } :
        WIR_Operation { RV32I::OpCode::LW, RV32I::OperationFormat::RC12R_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const12_Signed( uconst6 ),
          new WIR_RegisterParameter(
            RV32::RV32_wirProc->SP(), WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.MV instruction.

  Exact formats:

  C.MV x[a] (def), x[b] (use) (SRR_1)

  or

  ADDI x[a] (def), x[b] (use), 0 (RR_1)
*/
void RV32_InstructionFactory::insertCMV( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertCMV(const RV_RegV&, const RV_RegV&, "
    "const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CMV, RV32IC::OperationFormat::SRR_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } :
        WIR_Operation { RV32IMC::OpCode::MOV, RV32I::OperationFormat::RR_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.NOP instruction.

  Exact formats:

  C.NOP (SNULL_1)

  or

  ADDI x0 (def), x0 (use), 0 (RRC12_1)
*/
void RV32_InstructionFactory::insertCNOP( const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation {
          RV32IC::OpCode::CNOP, RV32IC::OperationFormat::SNULL_1 } :
        WIR_Operation { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
          new WIR_RegisterParameter( RV32::RV32_wirProc->x0(), WIR_Usage::def ),
          new WIR_RegisterParameter( RV32::RV32_wirProc->x0(), WIR_Usage::use ),
          new RV_Const12_Signed( 0 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.OR instruction.

  Exact formats:

  C.OR x[a] (defuse), x[b] (use) (SRR_2)

  or

  OR x[a] (def), x[a] (use), x[b] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertCOR( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::COR, RV32IC::OperationFormat::SRR_2,
          new WIR_RegisterParameter( xa, WIR_Usage::defuse ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } :
        WIR_Operation { RV32I::OpCode::OR, RV32I::OperationFormat::RRR_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.SLLI instruction.

  Exact formats:

  C.SLLI x[a] (defuse), uconst5 (SRC5_1)

  or

  SLLI x[a] (def), x[a] (use), uconst5 (RRC5_1)
*/
void RV32_InstructionFactory::insertCSLLI( const WIR::RV_RegV &xa,
                                           unsigned int uconst5,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CSLLI, RV32IC::OperationFormat::SRC5_1,
          new WIR_RegisterParameter( xa, WIR_Usage::defuse ),
          new RV_Const5_Unsigned( uconst5 ) } :
        WIR_Operation { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new RV_Const5_Unsigned( uconst5 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.SRAI instruction.

  Exact formats:

  C.SRAI x[a] (defuse), uconst5 (SRC5_1)

  or

  SRAI x[a] (def), x[a] (use), uconst5 (RRC5_1)
*/
void RV32_InstructionFactory::insertCSRAI( const WIR::RV_RegV &xa,
                                           unsigned int uconst5,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CSRAI, RV32IC::OperationFormat::SRC5_1,
          new WIR_RegisterParameter( xa, WIR_Usage::defuse ),
          new RV_Const5_Unsigned( uconst5 ) } :
        WIR_Operation { RV32I::OpCode::SRAI, RV32I::OperationFormat::RRC5_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new RV_Const5_Unsigned( uconst5 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.SRLI instruction.

  Exact formats:

  C.SRLI x[a] (defuse), uconst5 (SRC5_1)

  or

  SRLI x[a] (def), x[a] (use), uconst5 (RRC5_1)
*/
void RV32_InstructionFactory::insertCSRLI( const WIR::RV_RegV &xa,
                                           unsigned int uconst5,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CSRLI, RV32IC::OperationFormat::SRC5_1,
          new WIR_RegisterParameter( xa, WIR_Usage::defuse ),
          new RV_Const5_Unsigned( uconst5 ) } :
        WIR_Operation { RV32I::OpCode::SRLI, RV32I::OperationFormat::RRC5_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new RV_Const5_Unsigned( uconst5 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.SUB instruction.

  Exact formats:

  C.SUB x[a] (defuse), x[b] (use) (SRR_2)

  or

  SUB x[a] (def), x[a] (use), x[d] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertCSUB( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CSUB, RV32IC::OperationFormat::SRR_2,
          new WIR_RegisterParameter( xa, WIR_Usage::defuse ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } :
        WIR_Operation { RV32IMC::OpCode::SUB, RV32IMC::OperationFormat::RRR_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.SW instruction.

  Exact formats:

  C.SW x[a] (use), uconst5(x[b] (use)) (SRC5R_2)

  or

  SW x[a] (use), uconst5(x[b] (use)) (RC12R_2)
*/
void RV32_InstructionFactory::insertCSW( const WIR::RV_RegV &xa,
                                         unsigned int uconst5,
                                         const WIR::RV_RegV &xb,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CSW, RV32IC::OperationFormat::SRC5R_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const5_Unsigned( uconst5 ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } :
        WIR_Operation { RV32I::OpCode::SW, RV32I::OperationFormat::RC12R_2,
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new RV_Const12_Signed( uconst5 ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.SWSP instruction.

  Exact formats:

  C.SWSP x[a] (use), uconst6(sp (use)) (SRC6R_2)

  or

  SW x[a] (use), uconst6(sp (use)) (RC12R_2)
*/
void RV32_InstructionFactory::insertCSWSP( const WIR::RV_RegV &xa,
                                           unsigned int uconst6,
                                           const IR_Exp *exp,
                                           StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CSWSP, RV32IC::OperationFormat::SRC6R_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new RV_Const6_Unsigned( uconst6 ),
          new WIR_RegisterParameter(
            RV32::RV32_wirProc->SP(), WIR_Usage::use ) } :
        WIR_Operation { RV32I::OpCode::SW, RV32I::OperationFormat::RC12R_2,
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new RV_Const12_Signed( uconst6 ),
          new WIR_RegisterParameter(
            RV32::RV32_wirProc->SP(), WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a C.XOR instruction.

  Exact formats:

  C.XOR x[a] (defuse), x[b] (use) (SRR_2)

  or

  XOR x[a] (def), x[a] (use), x[b] (use) (RRR_1)
*/
void RV32_InstructionFactory::insertCXOR( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    RV32_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { RV32IC::OpCode::CXOR, RV32IC::OperationFormat::SRR_2,
          new WIR_RegisterParameter( xa, WIR_Usage::defuse ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } :
        WIR_Operation { RV32IMC::OpCode::XOR, RV32IMC::OperationFormat::RRR_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( xa, WIR_Usage::use ),
          new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a BGT pseudo-instruction.

  Exact format: BLT x[b] (use), x[a] (use), disp12 (RRL_1)
*/
void RV32_InstructionFactory::insertBGT( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::WIR_BasicBlock &disp12,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertBGT(const RV_RegV&, const RV_RegV&, "
    "const WIR_BasicBlock&, const IR_Exp*, RV32_InstructionFactory::StmtType) "
    "const" );

  RV32_InstructionFactory::insertBLT( xb, xa, disp12, exp, type );
};


/*
  Inserts a BGTU pseudo-instruction.

  Exact format: BLTU x[b] (use), x[a] (use), disp12 (RRL_1)
*/
void RV32_InstructionFactory::insertBGTU( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const WIR::WIR_BasicBlock &disp12,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  RV32_InstructionFactory::insertBLTU( xb, xa, disp12, exp, type );
};


/*
  Inserts a BLE pseudo-instruction.

  Exact format: BGE x[b] (use), x[a] (use), disp12 (RRL_1)
*/
void RV32_InstructionFactory::insertBLE( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const WIR::WIR_BasicBlock &disp12,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertBLE(const RV_RegV&, const RV_RegV&, "
    "const WIR_BasicBlock&, const IR_Exp*, RV32_InstructionFactory::StmtType) "
    "const" );

  RV32_InstructionFactory::insertBGE( xb, xa, disp12, exp, type );
};


/*
  Inserts a BLEU pseudo-instruction.

  Exact format: BGEU x[b] (use), x[a] (use), disp12 (RRL_1)
*/
void RV32_InstructionFactory::insertBLEU( const WIR::RV_RegV &xa,
                                          const WIR::RV_RegV &xb,
                                          const WIR::WIR_BasicBlock &disp12,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  RV32_InstructionFactory::insertBGEU( xb, xa, disp12, exp, type );
};


/*
  Inserts a J pseudo-instruction.

  Exact format: JAL x0 (def), disp20 (RL_1)
*/
void RV32_InstructionFactory::insertJ( const WIR::WIR_BasicBlock &disp20,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertJ(const WIR_BasicBlock&, const "
    "IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::J, RV32I::OperationFormat::L_1,
        new WIR_LabelParameter( disp20 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a JR pseudo-instruction.

  Exact format: JALR x[0], x[a] (use), 0 (RRC12_1)
*/
void RV32_InstructionFactory::insertJR( const WIR::RV_RegV &xa,
                                        const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto const &x0 = createReg();
  RV32::RV32_wirFct->insertPrecolor( x0, RV32::RV32_wirProc->x0() );
  RVINSTRUCTIONS.insertJALR( x0, xa, 0, exp, type );
};


/*
  Inserts a MOV pseudo-instruction.

  Exact format: ADDI x[a] (def), x[b] (use), 0 (RR_1)
*/
void RV32_InstructionFactory::insertMOV( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertMOV(const RV_RegV&, const RV_RegV&, "
    "const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32IMC::OpCode::MOV, RV32I::OperationFormat::RR_1,
        new WIR_RegisterParameter( xa, WIR_Usage::def ),
        new WIR_RegisterParameter( xb, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a NOP pseudo-instruction.

  Exact format: ADDI x[0] (def), x[0] (use), 0 (RRC12_1)
*/
void RV32_InstructionFactory::insertNOP( const IR_Exp *exp,
                                         StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &x0 = createReg();
  RV32::RV32_wirFct->insertPrecolor( x0, RV32::RV32_wirProc->x0() );
  RV32_InstructionFactory::insertADDI( x0, x0, 0, exp, type );
};


/*
  Inserts a NOT pseudo-instruction.

  Exact format: XORI x[a] (def), x[b] (use), -1 (RRC12_1)
*/
void RV32_InstructionFactory::insertNOT( const WIR::RV_RegV &xa,
                                         const WIR::RV_RegV &xb,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertNOT(const RV_RegV&, const RV_RegV&, "
    "const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  RV32_InstructionFactory::insertXORI( xa, xb, -1, exp, type );
};


/*
  Inserts a RET pseudo-instruction.

  Exact format: JALR x[0], x[1], 0 (RRC12_1)
*/
void RV32_InstructionFactory::insertRETURN( const IR_Exp *exp,
                                            StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertRETURN(const IR_Exp*, "
    "RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::JALR, RV32I::OperationFormat::RRC12_1,
        new WIR_RegisterParameter( RV32::RV32_wirProc->x0(), WIR_Usage::def ),
        new WIR_RegisterParameter( RV32::RV32_wirProc->RA() , WIR_Usage::use ),
        new RV_Const12_Signed( 0 ) } } );

  // Add implicit parameters for proper def-use analysis.
  i.begin()->get().pushBackParameter(
    new WIR_RegisterParameter( RV32::RV32_wirProc->RA(),
                               WIR_Usage::use, true ) );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a RET pseudo-instruction.

  Exact format: JALR x[0], x[1], 0 (RRC12_1)
*/
void RV32_InstructionFactory::insertRET( const WIR::RV_RegV &x10,
                                         const IR_Exp *exp,
                                         StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertRET( const WIR::RV_RegV &x10,"
    "const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto &i = RV32_wirBB->pushBackInstruction(
    { { RV32I::OpCode::JALR, RV32I::OperationFormat::RRC12_1,
        new WIR_RegisterParameter( RV32::RV32_wirProc->x0(), WIR_Usage::def ),
        new WIR_RegisterParameter( RV32::RV32_wirProc->RA() , WIR_Usage::use ),
        new RV_Const12_Signed( 0 ) } } );

  // Add implicit parameters for proper def-use analysis.
  i.begin()->get().pushBackParameter(
    new WIR_RegisterParameter( x10, WIR_Usage::use, true ) );
  i.begin()->get().pushBackParameter(
    new WIR_RegisterParameter( RV32::RV32_wirProc->RA(),
                               WIR_Usage::use, true ) );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts instructions to move a constant into a register.

  The exact formats depend on the constant's value.

  For getMinSignedValue( 12 ) <= const32 <= getMaxSignedValue( 12 ):

  ADDI x[a] (def), x0 (use), const32 (RRC12_1)

  else

  LUI x[a] (def), <upper 20 bits of const32> (RC20_1) \n
  ADDI x[a] (def), x[a] (use), <lower 12 bits of const32> (RRC12_1)
*/
void RV32_InstructionFactory::insertMOVConstant( const WIR::RV_RegV &xa,
                                                 int const32,
                                                 const IR_Exp *exp,
                                                 StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertMOVConstant(const RV_RegV&, int, "
    "const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  if ( ( const32 >= RV_Const12_Signed::getMinValue( 12 ) ) &&
       ( const32 <= RV_Const12_Signed::getMaxValue( 12 ) ) ) {
    auto &i = RV32_wirBB->pushBackInstruction(
      { { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
          new WIR_RegisterParameter( xa, WIR_Usage::def ),
          new WIR_RegisterParameter( RV32::RV32_wirProc->x0(), WIR_Usage::use ),
          new RV_Const12_Signed( const32 ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else
    insertLUI_ADDI( xa, const32, exp, type );
};


/*
  getMOVConstantCost returns the costs of instructions to move a constant into a
  register.

  The exact costs depend on the constant's value.

  For getMinSignedValue( 12 ) <= const32 <= getMaxSignedValue( 12 ):

  ADDI x[a] (def), x0 (use), const32 (RRC12_1)

  else

  LUI x[a] (def), <upper 20 bits of const32> (RC20_1) \n
  ADDI x[a] (def), x[a] (use), <lower 12 bits of const32> (RRC12_1)
*/
COST RV32_InstructionFactory::getMOVConstantCost( int const32 ) const
{
  DSTART( "COST RV32_InstructionFactory::getMOVConstantCost(int) const" );

  if ( ( const32 >= RV_Const12_Signed::getMinValue( 12 ) ) &&
       ( const32 <= RV_Const12_Signed::getMaxValue( 12 ) ) )
    return( RV32I::OperationFormat::RRC12_1.getSize() );
  else
    return( getLUI_ADDICost( const32 ) );
};


/*
  Inserts a series of LUI and ADDI instructions.

  Exact formats:

  LUI x[a] (def), <upper 20 bits of const32> (RC20_1) \n
  ADDI x[a] (def), x[a] (use), <lower 12 bits of const32> (RRC12_1)
*/
void RV32_InstructionFactory::insertLUI_ADDI( const WIR::RV_RegV &xa,
                                              int const32,
                                              const IR_Exp *exp,
                                              StmtType type ) const
{
  DSTART(
    "void RV32_InstructionFactory::insertLUI_ADDI(const RV_RegV&, int, "
    "const IR_Exp*, RV32_InstructionFactory::StmtType) const" );

  auto p = splitOffset( const32 );

cout << const32 << endl;
cout << p.first << endl;
cout << p.second << endl;

  // Generate LUI instruction.
  insertLUI( xa, p.first, exp, type );

  // Generate ADDI instruction.
  if ( p.second != 0 )
    insertADDI( xa, xa, p.second, exp, type );
};


/*
  getLUI_ADDICost returns the costs of instructions to move a constant into a
  register.

  Exact formats:

  LUI x[a] (def), <upper 20 bits of const32> (RC20_1) \n
  ADDI x[a] (def), x[a] (use), <lower 12 bits of const32> (RRC12_1)
*/
COST RV32_InstructionFactory::getLUI_ADDICost( int const32 ) const
{
  DSTART( "COST RV32_InstructionFactory::getLUI_ADDICost(int) const" );

  auto p = splitOffset( const32 );

  return(
    ( p.second == 0 ) ?
      RV32I::OperationFormat::RC20_1.getSize() :
      RV32I::OperationFormat::RC20_1.getSize() +
        RV32I::OperationFormat::RRC12_1.getSize() );
};


/*
  getCMVCost returns the costs of instructions for register-register moves.
*/
COST RV32_InstructionFactory::getCMVCost( void ) const
{
  DSTART( "COST RV32_InstructionFactory::getCMVCost() const" );

  return(
    m16BitOperations ?
      RV32IC::OperationFormat::SRR_1.getSize() :
      RV32I::OperationFormat::RR_1.getSize() );
};


/*
  createReg creates a new virtual RISC-V register.
*/
RV_RegV &RV32_InstructionFactory::createReg( void ) const
{
  DSTART( "RV_RegV& RV32::RV32_InstructionFactory::createReg() const" );

  auto *res = new RV_RegV();
  RV32_wirFct->pushBackVirtualRegister( res );

  return( *res );
};


/*
  addDebugInfo generates debug information in the form of a WIR comment and
  attaches it to the specified WIR instruction.
*/
void RV32_InstructionFactory::addDebugInfo( WIR::WIR_Instruction &i,
                                            const IR_Stmt *s,
                                            enum StmtType t ) const
{
  DSTART(
    "void RV32_InstructionFactory::addDebugInfo(WIR_Instruction&, const "
    "IR_Stmt*, RV32_InstructionFactory::StmtType) const" );

  if ( mConfig.getGenerateDebugFlag() ) {
    ufAssert( s );
    unsigned int codeLine = s->getFileContext().getLine();

    stringstream codeLineSStr;
    codeLineSStr << codeLine;
    string codeLineString;

    switch ( t ) {
      case EXP_STMT: {
        codeLineString = "Simple expression, ";
        break;
      }

      case IF_STMT: {
        codeLineString = "If statement, ";
        break;
      }

      case IFELSE_STMT: {
        codeLineString = "If-else statement, ";
        break;
      }

      case FOR_STMT: {
        codeLineString = "For-loop statement, ";
        break;
      }

      case WHILE_STMT: {
        codeLineString = "While-loop statement, ";
        break;
      }

      case DOWHILE_STMT: {
        codeLineString = "Do-while-loop statement, ";
        break;
      }

      case JUMP_STMT: {
        codeLineString = "Unconditional jump statement, ";
        break;
      }

      case SWITCH_STMT: {
        codeLineString = "Switch statement, ";
        break;
      }

      case VOID_STMT: {
        codeLineString = "Return-void statement, ";
        break;
      }

      case RETURN_STMT: {
        codeLineString = "Non-void return, ";
        break;
      }

      case ASM_STMT: {
        codeLineString = "Inline assembler statement, ";
        break;
      }

      case NOT_DEFINED:
        break;

      default: {
        ufAssertT( false, "Unknown statement type for debugging." );
        break;
      }
    }

    codeLineString += "source line number: " + codeLineSStr.str();

    i.insertContainer( WIR_Comment( codeLineString ) );
  }
};


/*
  splitOffset splits the given address offset into its most- and
  least-significant 20- and 12-bit parts, resp.
*/
std::pair<int, int> RV32_InstructionFactory::splitOffset( const int o )
{
  DSTART( "static pair<int, int> RV32_InstructionFactory::splitOffset(int)" );

  // Extract most-significant 20 bits from constant. This involves adding a
  // carry from bit position 12 to 13 (+ 0x800) as well as right-shifting by 12
  // bits (/ 0x1000).
  int hi = ( ( o + 0x800 ) / (unsigned int) 0x1000 ) & 0xFFFFF;

  // Extract least-significant 12 bits from constant.
  int lo = o & 0xFFF;

  // Since the least-significant 12 bits are assumed to be signed and
  // represented in 2's-complement, wrap the lo-value around the 12-bits
  // boundary if it does not fit.
  if ( lo > RV_Const12_Signed::getMaxValue( 12 ) )
    lo =
      RV_Const12_Signed::getMinValue( 12 ) - 1 +
      ( lo - RV_Const12_Signed::getMaxValue( 12 ) );
  else

  if ( lo < RV_Const12_Signed::getMinValue( 12 ) )
    lo =
      RV_Const12_Signed::getMaxValue( 12 ) +
      ( lo - RV_Const12_Signed::getMinValue( 12 ) );

  return( make_pair( hi, lo ) );
};

}       // namespace RV32
