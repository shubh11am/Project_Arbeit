/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2005 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include standard headers
#include <map>
#include <string>
#include <utility>

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>
#include <arch/tricore/asmparser/tcasmparser.h>

// Include ICD headers
#include <arch/PROC/archinfo.h>

// Include back-annotation headers
#include <backannotation/backannotation.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include local headers
#include "auxfuncs.h"
#include "cs_tc179x.h"
#include "registrar.h"
#include "instructionfactory.h"

// Include assembly parser headers
#include <arch/IO/PARSER/parserinterface.h>
#include <arch/IO/PARSER/argument.h>

// Include misc headers
#include <misc/misc.h>


//
// Preprocessor macros
//

// This flag determines whether float-to-int conversions are carried out in
// software or hardware.
#define NO_SOFT_FLOAT_TO_INT_CONVERSION 1

// Useful defines
#define PHYSICAL 0
#define VIRTUAL 1

#define LAST_LLIR_BB mCodesel.getLastLLIRBB()
#define LAST_LLIR_FUNCTION mCodesel.getLastLLIRFunction()

#define ADDDEBUGINFO( __i, __e, __t )                                          \
  if ( __e )                                                                   \
    addDebugInfo( __i, &__e->getStmt(), __t );


//
// Code section
//

using namespace std;
using namespace WIR;


//
// Global variable declaration section
//

extern TC13 *TC179x_wirProc;
extern WIR_Function *TC179x_wirFct;
extern WIR_BasicBlock *TC179x_wirBB;


//
// Public class methods
//

/*
  Constructor with parameter Configuration and BackAnnotation.
*/
InstructionFactory::InstructionFactory( Configuration &config,
                                        TC179x_CodeSelector &codesel ) :
  mConfig( config ),
  mCodesel( codesel ),
  m16BitOperations { mConfig.getParam( Configuration::GENERATE_16BIT_OPS ) }
{
  mCurrentInstr = nullptr;
};


/*
  Destructor.
*/
InstructionFactory::~InstructionFactory( void )
{
};


/*
  setCurrentInstr sets the reference to the instruction lastly generated during
  code selection.
*/
void InstructionFactory::setCurrentInstruction( LLIR_Instruction *i )
{
  mCurrentInstr = i;
};


/*
  getCurrentInstr retrieves the reference to the instruction lastly generated
  during code selection.
*/
LLIR_Instruction *InstructionFactory::getCurrentInstruction( void ) const
{
  return( mCurrentInstr );
};


/*
  addDebugInfo generates debug information in the form of a LLIR comment and
  attaches it to the specified LLIR instruction.
*/
void InstructionFactory::addDebugInfo( const IR_Stmt *stm,
                                       LLIR_Instruction *ins,
                                       enum StmtType type )
{
  DSTART(
    "void InstructionFactory::addDebugInfo(const IR_Stmt*, LLIR_Instruction*, InstructionFactory::StmtType)" );

  ufAssert( stm );
  unsigned int codeLine = stm->getFileContext().getLine();

  stringstream codeLineSStr;
  codeLineSStr << codeLine;
  string codeLineString;

  switch ( type ) {
    case EXP_STMT:
      codeLineString = "Simple expression, ";
      break;
    case IF_STMT:
      codeLineString = "If statement, ";
      break;
    case IFELSE_STMT:
      codeLineString = "If-else statement, ";
      break;
    case FOR_STMT:
      codeLineString = "For-loop statement, ";
      break;
    case WHILE_STMT:
      codeLineString = "While-loop statement, ";
      break;
    case DOWHILE_STMT:
      codeLineString = "Do-while-loop statement, ";
      break;
    case JUMP_STMT:
      codeLineString = "Unconditional jump statement, ";
      break;
    case SWITCH_STMT:
      codeLineString = "Switch statement, ";
      break;
    case VOID_STMT:
      codeLineString = "Return-void statement, ";
      break;
    case RETURN_STMT:
      codeLineString = "Non-void return, ";
      break;
    case ASM_STMT:
      codeLineString = "Inline assembler statement, ";
      break;
    case NOT_DEFINED:
      break;
    default:
        ufAssertT( 0, "Unknown statement type for debugging." );
        break;
  }

  codeLineString += "source line number: " + codeLineSStr.str();

  /* If the passed instruction is 0, no other instructions have been generated
   * previously. */
  ins->AddPragma( new LLIR_Pragma( codeLineString.c_str(), true ) );
};


/*
  addDebugInfo generates debug information in the form of a WIR comment and
  attaches it to the specified WIR instruction.
*/
void InstructionFactory::addDebugInfo( WIR::WIR_Instruction &i,
                                       const IR_Stmt *s, enum StmtType t ) const
{
  DSTART(
    "void InstructionFactory::addDebugInfo(WIR_Instruction&, const IR_Stmt*, InstructionFactory::StmtType) const" );

  if ( mConfig.getGenerateDebugFlag() ) {
    ufAssert( s );
    unsigned int codeLine = s->getFileContext().getLine();

    stringstream codeLineSStr;
    codeLineSStr << codeLine;
    string codeLineString;

    switch ( t ) {
      case EXP_STMT:
        codeLineString = "Simple expression, ";
        break;
      case IF_STMT:
        codeLineString = "If statement, ";
        break;
      case IFELSE_STMT:
        codeLineString = "If-else statement, ";
        break;
      case FOR_STMT:
        codeLineString = "For-loop statement, ";
        break;
      case WHILE_STMT:
        codeLineString = "While-loop statement, ";
        break;
      case DOWHILE_STMT:
        codeLineString = "Do-while-loop statement, ";
        break;
      case JUMP_STMT:
        codeLineString = "Unconditional jump statement, ";
        break;
      case SWITCH_STMT:
        codeLineString = "Switch statement, ";
        break;
      case VOID_STMT:
        codeLineString = "Return-void statement, ";
        break;
      case RETURN_STMT:
        codeLineString = "Non-void return, ";
        break;
      case ASM_STMT:
        codeLineString = "Inline assembler statement, ";
        break;
      case NOT_DEFINED:
        break;
      default:
          ufAssertT( 0, "Unknown statement type for debugging." );
          break;
    }

    codeLineString += "source line number: " + codeLineSStr.str();

    i.insertContainer( WIR_Comment( codeLineString ) );
  }
};


//
// Native TriCore ISA instructions.
//

void InstructionFactory::insertABS( LLIR_Register *r0, LLIR_Register *r1,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insABS( r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ABS instruction.

  Exact format: ABS D[c] (def), D[b] (use) (DD)
*/
void InstructionFactory::insertABS( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Db,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertABS(const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::ABS, TC13::OperationFormat::DD,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertADD( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insADD( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ADD instruction.

  Exact format: ADD D[c] (def), D[a] (use), const9 (DDC9_1)
*/
void InstructionFactory::insertADD( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da, int const9,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertADD(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::ADD, TC13::OperationFormat::DDC9_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertADD( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insADD( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ADD instruction.

  Exact format: ADD D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertADD( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da,
                                    const WIR::TC_DRegV &Db,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertADD(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertADD( LLIR_Register *r0, int c0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insADD( r0, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ADD instruction.

  Exact format: ADD D[a] (defuse), const4 (SDC4_2)
*/
void InstructionFactory::insertADD( const WIR::TC_DRegV &Da, int const4,
                                    const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { TC13::OpCode::ADD, TC13::OperationFormat::SDC4_2,
          new WIR_RegisterParameter( Da, WIR_Usage::defuse ),
          new TC_Const4_Signed( const4 ) } :
        WIR_Operation { TC13::OpCode::ADD, TC13::OperationFormat::DDC9_1,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ),
          new TC_Const9_Signed( const4 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertADD_A( LLIR_Register *r0, int c0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insADD_A( r0, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ADD.A instruction.

  Exact format: ADD.A A[a] (defuse), const4 (SAC4_2)
*/
void InstructionFactory::insertADD_A( const WIR::TC_ARegV &Aa, int const4,
                                      const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { TC13::OpCode::ADD_A, TC13::OperationFormat::SAC4_2,
          new WIR_RegisterParameter( Aa, WIR_Usage::defuse ),
          new TC_Const4_Signed( const4 ) } :
        WIR_Operation { TC13::OpCode::LEA, TC13::OperationFormat::AAC10BOA,
          new WIR_RegisterParameter( Aa, WIR_Usage::def ),
          new WIR_RegisterParameter( Aa, WIR_Usage::use ),
          new TC_Const10_Signed( const4 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertADDC( LLIR_Register *r0, LLIR_Register *r1,
                                     int c0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insADDC( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ADDC instruction.

  Exact format: ADDC D[c] (def), D[a] (use), const9, PSW.C (defuse) (DDC9PSW_2)
*/
void InstructionFactory::insertADDC( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da, int const9,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertADDC(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const");

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::ADDC, TC13::OperationFormat::DDC9PSW_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ),
        new
          WIR_RegisterParameter(
            TC179x_wirProc->PSW_C(), WIR_Usage::defuse ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertADDC( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insADDC( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ADDC instruction.

  Exact format:
  ADDC D[c] (def), D[a] (use), D[b] (use), PSW.C (defuse) (DDDPSW_2)
*/
void InstructionFactory::insertADDC( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertADDC(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::ADDC, TC13::OperationFormat::DDDPSW_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new
          WIR_RegisterParameter(
            TC179x_wirProc->PSW_C(), WIR_Usage::defuse ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertADDI( LLIR_Register *r0, LLIR_Register *r1,
                                     int c0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insADDI( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ADDI instruction.

  Exact format: ADDI D[c] (def), D[a] (use), const16 (DDC16_1)
*/
void InstructionFactory::insertADDI( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da, int const16,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertADDI(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::ADDI, TC13::OperationFormat::DDC16_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const16_Signed( const16 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertADDIH_A( LLIR_Register *r0, LLIR_Register *r1,
                                        int c0,
                                        const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insADDIH_A( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ADDIH.A instruction.

  Exact format: ADDIH.A A[c] (def), A[a] (use), const16 (AAC16)
*/
void InstructionFactory::insertADDIH_A( const WIR::TC_ARegV &Ac,
                                        const WIR::TC_ARegV &Aa,
                                        unsigned int const16,
                                        const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertADDIH_A(const TC_ARegV&, const TC_ARegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
        new WIR_RegisterParameter( Ac, WIR_Usage::def ),
        new WIR_RegisterParameter( Aa, WIR_Usage::use ),
        new TC_Const16_Unsigned( const16 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertADDSC_A( LLIR_Register *r0, LLIR_Register *r1,
                                        LLIR_Register *r2, int c0,
                                        const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insADDSC_A( r0, r1, r2, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ADDSC.A instruction.

  Exact format: ADDSC.A A[c] (def), A[b] (use), D[a] (use), const2 (AADC2)
*/
void InstructionFactory::insertADDSC_A( const WIR::TC_ARegV &Ac,
                                        const WIR::TC_ARegV &Ab,
                                        const WIR::TC_DRegV &Da,
                                        unsigned int const2,
                                        const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertADDSC_A(const TC_ARegV&, const TC_ARegV&, const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::ADDSC_A, TC13::OperationFormat::AADC2,
        new WIR_RegisterParameter( Ac, WIR_Usage::def ),
        new WIR_RegisterParameter( Ab, WIR_Usage::use ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const2_Unsigned( const2 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertADDX( LLIR_Register *r0, LLIR_Register *r1,
                                     int c0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insADDX( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ADDX instruction.

  Exact format: ADDX D[c] (def), D[a] (use), const9, PSW.C (def) (DDC9PSW_1)
*/
void InstructionFactory::insertADDX( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da, int const9,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertADDX(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::ADDX, TC13::OperationFormat::DDC9PSW_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ),
        new
          WIR_RegisterParameter(
            TC179x_wirProc->PSW_C(), WIR_Usage::def ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertADDX( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insADDX( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ADDX instruction.

  Exact format: ADDX D[c] (def), D[a] (use), D[b] (use), PSW.C (def) (DDDPSW_1)
*/
void InstructionFactory::insertADDX( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertADDX(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::ADDX, TC13::OperationFormat::DDDPSW_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new
          WIR_RegisterParameter(
            TC179x_wirProc->PSW_C(), WIR_Usage::def ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertAND( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insAND( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an AND instruction.

  Exact format: AND D[c] (def), D[a] (use), const9 (DDC9_2)
*/
void InstructionFactory::insertAND( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da,
                                    unsigned int const9,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertAND(const TC_DRegV&, const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::AND, TC13::OperationFormat::DDC9_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Unsigned( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertAND( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insAND( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an AND instruction.

  Exact format: AND D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertAND( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da,
                                    const WIR::TC_DRegV &Db,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertAND(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::AND, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertAND_EQ( LLIR_Register *r0, LLIR_Register *r1,
                                       int c0,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insAND_EQ( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an AND.EQ instruction.

  Exact format: AND.EQ D[c] (defuse), D[a] (use), const9 (DDC9_3)
*/
void InstructionFactory::insertAND_EQ( const WIR::TC_DRegV &Dc,
                                       const WIR::TC_DRegV &Da, int const9,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertAND_EQ(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::AND_EQ, TC13::OperationFormat::DDC9_3,
        new WIR_RegisterParameter( Dc, WIR_Usage::defuse ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertAND_EQ( LLIR_Register *r0, LLIR_Register *r1,
                                       LLIR_Register *r2,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insAND_EQ( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an AND.EQ instruction.

  Exact format: AND.EQ D[c] (defuse), D[a] (use), D[b] (use) (DDD_2)
*/
void InstructionFactory::insertAND_EQ( const WIR::TC_DRegV &Dc,
                                       const WIR::TC_DRegV &Da,
                                       const WIR::TC_DRegV &Db,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertAND_EQ(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::AND_EQ, TC13::OperationFormat::DDD_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::defuse ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertAND_GE_U( LLIR_Register *r0, LLIR_Register *r1,
                                         LLIR_Register *r2,
                                         const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insAND_GE_U( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an AND.GE.U instruction.

  Exact format: AND.GE.U D[c] (defuse), D[a] (use), D[b] (use) (DDD_2)
*/
void InstructionFactory::insertAND_GE_U( const WIR::TC_DRegV &Dc,
                                         const WIR::TC_DRegV &Da,
                                         const WIR::TC_DRegV &Db,
                                         const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertAND_GE_U(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::AND_GE_U, TC13::OperationFormat::DDD_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::defuse ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertAND_LT_U( LLIR_Register *r0, LLIR_Register *r1,
                                         LLIR_Register *r2,
                                         const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insAND_LT_U( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an AND.LT.U instruction.

  Exact format: AND.LT.U D[c] (defuse), D[a] (use), D[b] (use) (DDD_2)
*/
void InstructionFactory::insertAND_LT_U( const WIR::TC_DRegV &Dc,
                                         const WIR::TC_DRegV &Da,
                                         const WIR::TC_DRegV &Db,
                                         const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertAND_LT_U(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::AND_LT_U, TC13::OperationFormat::DDD_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::defuse ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertANDN( LLIR_Register *r0, LLIR_Register *r1,
                                     int c0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insANDN( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ANDN instruction.

  Exact format: ANDN D[c] (def), D[a] (use), const9 (DDC9_2)
*/
void InstructionFactory::insertANDN( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     unsigned int const9,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertANDN(const TC_DRegV&, const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::ANDN, TC13::OperationFormat::DDC9_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Unsigned( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertCALL( const std::string &l0,
                                     std::deque<LLIR_Register *> *regs,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insCALL( l0, regs, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a CALL instruction.

  Exact format: CALL disp8 (SL)
*/
void InstructionFactory::insertCALL( const IR_FunctionSymbol &disp8,
                                     const std::list<std::reference_wrapper<WIR::WIR_BaseRegister>> &args,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertCALL(const IR_FunctionSymbol&, const list<reference_wrapper<WIR_BaseRegister> >&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( disp8.getFunction() == nullptr ) {
    // The called IR function is an incomplete function with a declaration only
    // but no implementation in the IR.
    string label = disp8.getName();
    WIR_System &sys = mCodesel.getSystem();

    if ( !mCodesel.containsWIRFunction( disp8 ) ) {
      // The WIR system does not yet contain a symbol and WIR function for the
      // current IR function. So, create one.
      WIR_Function &f =
        sys.begin()->get().pushBackFunction( WIR_Function( label ) );
      mCodesel.setWIRFunction( disp8, f );
      auto &sym = sys.findSymbol( f );
      sym.setExtern();

      // Determine if the IR function is static.
      bool fctIsStatic =
        ( disp8.getType().getStorageClass() == IR_Type::STATIC );

      // Add "global" directive only if function is not static.
      sym.setGlobal( !fctIsStatic );
    }

    // Finally generate the CALL to the external WIR function.
    auto &f = mCodesel.getWIRFunction( disp8 );
    insertCALL( f, args, exp, type );

    // Add implicit parameters for proper def-use analysis.
    auto &sym = sys.findSymbol( f );
    if ( sym.isExtern() ) {
      auto &i = TC179x_wirBB->getInstructions().back().get();
      auto &o = i.getOperations().back().get();
      o.pushBackParameter(
        new WIR_RegisterParameter(
          TC179x_wirProc->A2(), WIR_Usage::def, true ) );
      o.pushBackParameter(
        new WIR_RegisterParameter(
          TC179x_wirProc->A3(), WIR_Usage::def, true ) );
    }
  } else
    insertCALL(
      mCodesel.getWIRFunction( disp8.getFunction() ), args, exp, type );
};


/*
  Inserts a CALL instruction.

  Exact format: CALL disp8 (SL)
*/
void InstructionFactory::insertCALL( const WIR::WIR_Function &disp8,
                                     const std::list<std::reference_wrapper<WIR::WIR_BaseRegister>> &args,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertCALL(const WIR_Function&, const list<reference_wrapper<WIR_BaseRegister> >&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::CALL,
          m16BitOperations ?
            TC13::OperationFormat::SL : TC13::OperationFormat::L,
          new WIR_LabelParameter( disp8 ) } } );

  // Add implicit parameters for proper def-use analysis.
  WIR_Operation &callOp = i.begin()->get();
  for ( WIR_BaseRegister &r : args )
    callOp.pushBackParameter(
      new WIR_RegisterParameter( r, WIR_Usage::defuse, true ) );

  callOp.pushBackParameter(
    new WIR_RegisterParameter( TC179x_wirProc->A10(), WIR_Usage::use, true ) );
  callOp.pushBackParameter(
    new WIR_RegisterParameter( TC179x_wirProc->A11(), WIR_Usage::use, true ) );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertCALLI( LLIR_Register *r0,
                                      std::deque<LLIR_Register *> *regs,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insCALLI( r0, regs, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a CALLI instruction.

  Exact format: CALLI A[a] (A)
*/
void InstructionFactory::insertCALLI( const WIR::TC_ARegV &Aa,
                                      const std::list<std::reference_wrapper<WIR::WIR_BaseRegister>> &args,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertCALLI(const TC_ARegV&, const list<reference_wrapper<WIR_BaseRegister> >&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::CALLI, TC13::OperationFormat::A,
        new WIR_RegisterParameter( Aa, WIR_Usage::use ) } } );

  // Add implicit parameters for proper def-use analysis.
  WIR_Operation &callOp = i.begin()->get();
  for ( WIR_BaseRegister &r : args )
    callOp.pushBackParameter(
      new WIR_RegisterParameter( r, WIR_Usage::defuse, true ) );

  callOp.pushBackParameter(
    new WIR_RegisterParameter( TC179x_wirProc->A10(), WIR_Usage::use, true ) );
  callOp.pushBackParameter(
    new WIR_RegisterParameter( TC179x_wirProc->A11(), WIR_Usage::use, true ) );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertDEXTR( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2, int c0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insDEXTR( r0, r1, r2, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a DEXTR instruction.

  Exact format: DEXTR D[c] (def), D[a] (use), D[b] (use), pos (DDDC5)
*/
void InstructionFactory::insertDEXTR( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db,
                                      unsigned int pos,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDEXTR(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::DEXTR, TC13::OperationFormat::DDDC5,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new TC_Const5_Unsigned( pos ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertDEXTR( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2, LLIR_Register *r3,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insDEXTR( r0, r1, r2, r3, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a DEXTR instruction.

  Exact format: DEXTR D[c] (def), D[a] (use), D[b] (use), D[d] (use) (DDDD)
*/
void InstructionFactory::insertDEXTR( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db,
                                      const WIR::TC_DRegV &Dd,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDEXTR(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::DEXTR, TC13::OperationFormat::DDDD,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new WIR_RegisterParameter( Dd, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertDIV_B( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2, bool isUnsigned,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *e0 = CreateERegister( "" );

  // Generate initializing DVINIT instruction.
  if ( isUnsigned )
    mCurrentInstr = insDVINIT_BU( e0, r0, r1, LAST_LLIR_BB, mCurrentInstr );
  else
    mCurrentInstr = insDVINIT_B( e0, r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // Generate DVSTEP instruction.
  if ( isUnsigned )
    mCurrentInstr = insDVSTEP_U( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );
  else
    mCurrentInstr = insDVSTEP( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );

  // Generate finalizing DVADJ instruction.
  if ( !isUnsigned )
    mCurrentInstr = insDVADJ( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

  // MOV result to target register.
  mCurrentInstr =
    insMOV( r2, e0->GetFirstChild(), LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a series of instructions for signed/unsigned byte division.

  Exact formats:

  DVINIT.B E[x], D[a], D[b] (EDD)
  DVSTEP E[x], E[x], D[b] (EED)
  DVADJ E[x], E[x], D[b] (EED)
  MOV D[c], Even register of E[x] (MOV_RR SDD_1)

  resp.

  DVINIT.BU E[x], D[a], D[b] (EDD)
  DVSTEP.U E[x], E[x], D[b] (EED)
  MOV D[c], Even register of E[x] (MOV_RR SDD_1)
*/
void InstructionFactory::insertDIV_B( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db, bool u,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDIV_B(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, bool, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &Ex = createEReg();

  // Generate initializing DVINIT.B[U] instruction.
  auto &i1 = TC179x_wirBB->pushBackInstruction(
    { { ( u ? TC13::OpCode::DVINIT_BU : TC13::OpCode::DVINIT_B ),
        TC13::OperationFormat::EDD,
        new WIR_RegisterParameter( Ex, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
  ADDDEBUGINFO( i1, exp, type );

  // Generate DVSTEP instruction.
  auto &i2 = TC179x_wirBB->pushBackInstruction(
    { { ( u ? TC13::OpCode::DVSTEP_U : TC13::OpCode::DVSTEP ),
        TC13::OperationFormat::EED,
        new WIR_RegisterParameter( Ex, WIR_Usage::def ),
        new WIR_RegisterParameter( Ex, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
  ADDDEBUGINFO( i2, exp, type );

  // Generate finalizing DVADJ instruction.
  if ( !u ) {
    auto &i3 = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::DVADJ, TC13::OperationFormat::EED,
          new WIR_RegisterParameter( Ex, WIR_Usage::def ),
          new WIR_RegisterParameter( Ex, WIR_Usage::use ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
    ADDDEBUGINFO( i3, exp, type );
  }

  // Move result to target register.
  auto &i4 =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::MOV_RR,
          m16BitOperations ?
            TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter(
            dynamic_cast<WIR_VirtualRegister &>( Ex ).begin()->get(),
            WIR_Usage::use ) } } );
  ADDDEBUGINFO( i4, exp, type );
};


void InstructionFactory::insertDIV_H( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2, bool isUnsigned,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *e0 = CreateERegister( "" );

  // Generate initializing DVINIT instruction.
  if ( isUnsigned )
    mCurrentInstr = insDVINIT_HU( e0, r0, r1, LAST_LLIR_BB, mCurrentInstr );
  else
    mCurrentInstr = insDVINIT_H( e0, r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // Generate 2 DVSTEP instructions.
  for ( int i = 0; i < 2; i++ )
    if ( isUnsigned )
      mCurrentInstr = insDVSTEP_U( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );
    else
      mCurrentInstr = insDVSTEP( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );

  // Generate finalizing DVADJ instruction.
  if ( !isUnsigned )
    mCurrentInstr = insDVADJ( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

  // MOV result to target register.
  mCurrentInstr =
    insMOV( r2, e0->GetFirstChild(), LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a series of instructions for signed/unsigned halfword division.

  Exact formats:

  DVINIT.H E[x], D[a], D[b] (EDD)
  DVSTEP E[x], E[x], D[b] (EED)
  DVSTEP E[x], E[x], D[b] (EED)
  DVADJ E[x], E[x], D[b] (EED)
  MOV D[c], Even register of E[x] (MOV_RR SDD_1)

  resp.

  DVINIT.HU E[x], D[a], D[b] (EDD)
  DVSTEP.U E[x], E[x], D[b] (EED)
  DVSTEP.U E[x], E[x], D[b] (EED)
  MOV D[c], Even register of E[x] (MOV_RR SDD_1)
*/
void InstructionFactory::insertDIV_H( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db, bool u,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDIV_H(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, bool, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &Ex = createEReg();

  // Generate initializing DVINIT.H[U] instruction.
  auto &i1 = TC179x_wirBB->pushBackInstruction(
    { { ( u ? TC13::OpCode::DVINIT_HU : TC13::OpCode::DVINIT_H ),
        TC13::OperationFormat::EDD,
        new WIR_RegisterParameter( Ex, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
  ADDDEBUGINFO( i1, exp, type );

  // Generate two DVSTEP instructions.
  for ( int i = 0; i < 2; ++i ) {
    auto &i2 = TC179x_wirBB->pushBackInstruction(
      { { ( u ? TC13::OpCode::DVSTEP_U : TC13::OpCode::DVSTEP ),
          TC13::OperationFormat::EED,
          new WIR_RegisterParameter( Ex, WIR_Usage::def ),
          new WIR_RegisterParameter( Ex, WIR_Usage::use ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
    ADDDEBUGINFO( i2, exp, type );
  }

  // Generate finalizing DVADJ instruction.
  if ( !u ) {
    auto &i3 = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::DVADJ, TC13::OperationFormat::EED,
          new WIR_RegisterParameter( Ex, WIR_Usage::def ),
          new WIR_RegisterParameter( Ex, WIR_Usage::use ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
    ADDDEBUGINFO( i3, exp, type );
  }

  // Move result to target register.
  auto &i4 =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::MOV_RR,
          m16BitOperations ?
            TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter(
            dynamic_cast<WIR_VirtualRegister &>( Ex ).begin()->get(),
            WIR_Usage::use ) } } );
  ADDDEBUGINFO( i4, exp, type );
};


void InstructionFactory::insertDIV_W( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2, bool isUnsigned,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *e0 = CreateERegister( "" );

  // Generate initializing DVINIT instruction.
  if ( isUnsigned )
    mCurrentInstr = insDVINIT_U( e0, r0, r1, LAST_LLIR_BB, mCurrentInstr );
  else
    mCurrentInstr = insDVINIT( e0, r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // Generate 4 DVSTEP instructions.
  for ( int i = 0; i < 4; i++ )
    if ( isUnsigned )
      mCurrentInstr = insDVSTEP_U( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );
    else
      mCurrentInstr = insDVSTEP( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );

  // Generate finalizing DVADJ instruction.
  if ( !isUnsigned )
    mCurrentInstr = insDVADJ( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

  // MOV result to target register.
  mCurrentInstr =
    insMOV( r2, e0->GetFirstChild(), LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a series of instructions for signed/unsigned integer division.

  Exact formats:

  DVINIT E[x], D[a], D[b] (EDD)
  DVSTEP E[x], E[x], D[b] (EED)
  DVSTEP E[x], E[x], D[b] (EED)
  DVSTEP E[x], E[x], D[b] (EED)
  DVSTEP E[x], E[x], D[b] (EED)
  DVADJ E[x], E[x], D[b] (EED)
  MOV D[c], Even register of E[x] (MOV_RR SDD_1)

  resp.

  DVINIT.U E[x], D[a], D[b] (EDD)
  DVSTEP.U E[x], E[x], D[b] (EED)
  DVSTEP.U E[x], E[x], D[b] (EED)
  DVSTEP.U E[x], E[x], D[b] (EED)
  DVSTEP.U E[x], E[x], D[b] (EED)
  MOV D[c], Even register of E[x] (MOV_RR SDD_1)
*/
void InstructionFactory::insertDIV_W( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db, bool u,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDIV_W(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, bool, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &Ex = createEReg();

  // Generate initializing DVINIT[.U] instruction.
  auto &i1 = TC179x_wirBB->pushBackInstruction(
    { { ( u ? TC13::OpCode::DVINIT_U : TC13::OpCode::DVINIT ),
        TC13::OperationFormat::EDD,
        new WIR_RegisterParameter( Ex, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
  ADDDEBUGINFO( i1, exp, type );

  // Generate four DVSTEP instructions.
  for ( int i = 0; i < 4; ++i ) {
    auto &i2 = TC179x_wirBB->pushBackInstruction(
      { { ( u ? TC13::OpCode::DVSTEP_U : TC13::OpCode::DVSTEP ),
          TC13::OperationFormat::EED,
          new WIR_RegisterParameter( Ex, WIR_Usage::def ),
          new WIR_RegisterParameter( Ex, WIR_Usage::use ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
    ADDDEBUGINFO( i2, exp, type );
  }

  // Generate finalizing DVADJ instruction.
  if ( !u ) {
    auto &i3 = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::DVADJ, TC13::OperationFormat::EED,
          new WIR_RegisterParameter( Ex, WIR_Usage::def ),
          new WIR_RegisterParameter( Ex, WIR_Usage::use ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
    ADDDEBUGINFO( i3, exp, type );
  }

  // Move result to target register.
  auto &i4 =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::MOV_RR,
          m16BitOperations ?
            TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter(
            dynamic_cast<WIR_VirtualRegister &>( Ex ).begin()->get(),
            WIR_Usage::use ) } } );
  ADDDEBUGINFO( i4, exp, type );
};


void InstructionFactory::insertEQ( LLIR_Register *r0, LLIR_Register *r1,
                                   int c0,
                                   const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insEQ( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an EQ instruction.

  Exact format: EQ D[c] (def), D[a] (use), const9 (DDC9_1)
*/
void InstructionFactory::insertEQ( const WIR::TC_DRegV &Dc,
                                   const WIR::TC_DRegV &Da, int const9,
                                   const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertEQ(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::EQ, TC13::OperationFormat::DDC9_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertEQ( LLIR_Register *r0, LLIR_Register *r1,
                                   LLIR_Register *r2,
                                   const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insEQ( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an EQ instruction.

  Exact format: EQ D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertEQ( const WIR::TC_DRegV &Dc,
                                   const WIR::TC_DRegV &Da,
                                   const WIR::TC_DRegV &Db,
                                   const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertEQ(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::EQ, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertEQ_A( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insEQ_A( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an EQ.A instruction.

  Exact format: EQ.A D[c] (def), A[a] (use), A[b] (use) (DAA)
*/
void InstructionFactory::insertEQ_A( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_ARegV &Aa,
                                     const WIR::TC_ARegV &Ab,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertEQ_A(const TC_DRegV&, const TC_ARegV&, const TC_ARegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::EQ_A, TC13::OperationFormat::DAA,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Aa, WIR_Usage::use ),
        new WIR_RegisterParameter( Ab, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertEQZ_A( LLIR_Register *r0, LLIR_Register *r1,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insEQZ_A( r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an EQZ.A instruction.

  Exact format: EQZ.A D[c] (def), A[a] (use) (DA)
*/
void InstructionFactory::insertEQZ_A( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_ARegV &Aa,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertEQZ_A(const TC_DRegV&, const TC_ARegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::EQZ_A, TC13::OperationFormat::DA,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Aa, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertEXTR( LLIR_Register *r0, LLIR_Register *r1,
                                     int c0, int c1,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insEXTR( r0, r1, c0, c1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an EXTR instruction.

  Exact format: EXTR D[c] (def), D[a] (use), pos width (DDC5C5)
*/
void InstructionFactory::insertEXTR( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     unsigned int pos, unsigned int width,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertEXTR(const TC_DRegV&, const TC_DRegV&, unsigned int, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::EXTR, TC13::OperationFormat::DDC5C5,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const5_Unsigned( pos ),
        new TC_Const5_Unsigned( width ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertEXTR_U( LLIR_Register *r0, LLIR_Register *r1,
                                       int c0, int c1,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insEXTR_U( r0, r1, c0, c1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an EXTR.U instruction.

  Exact format: EXTR.U D[c] (def), D[a] (use), pos width (DDC5C5)
*/
void InstructionFactory::insertEXTR_U( const WIR::TC_DRegV &Dc,
                                       const WIR::TC_DRegV &Da,
                                       unsigned int pos, unsigned int width,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertEXTR_U(const TC_DRegV&, const TC_DRegV&, unsigned int, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::EXTR_U, TC13::OperationFormat::DDC5C5,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const5_Unsigned( pos ),
        new TC_Const5_Unsigned( width ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertGE( LLIR_Register *r0, LLIR_Register *r1, int c0,
                                   const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insGE( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a GE instruction.

  Exact format: GE D[c] (def), D[a] (use), const9 (DDC9_1)
*/
void InstructionFactory::insertGE( const WIR::TC_DRegV &Dc,
                                   const WIR::TC_DRegV &Da, int const9,
                                   const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertGE(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::GE, TC13::OperationFormat::DDC9_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertGE( LLIR_Register *r0, LLIR_Register *r1,
                                   LLIR_Register *r2,
                                   const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insGE( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a GE instruction.

  Exact format: GE D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertGE( const WIR::TC_DRegV &Dc,
                                   const WIR::TC_DRegV &Da,
                                   const WIR::TC_DRegV &Db,
                                   const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertGE(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::GE, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertGE_A( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insGE_A( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a GE.A instruction.

  Exact format: GE.A D[c] (def), A[a] (use), A[b] (use) (DAA)
*/
void InstructionFactory::insertGE_A( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_ARegV &Aa,
                                     const WIR::TC_ARegV &Ab,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertGE_A(const TC_DRegV&, const TC_ARegV&, const TC_ARegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::GE_A, TC13::OperationFormat::DAA,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Aa, WIR_Usage::use ),
        new WIR_RegisterParameter( Ab, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertGE_U( LLIR_Register *r0, LLIR_Register *r1,
                                     int c0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insGE_U( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a GE.U instruction.

  Exact format: GE.U D[c] (def), D[a] (use), const9 (DDC9_2)
*/
void InstructionFactory::insertGE_U( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     unsigned int const9,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertGE_U(const TC_DRegV&, const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::GE_U, TC13::OperationFormat::DDC9_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Unsigned( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertGE_U( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insGE_U( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a GE.U instruction.

  Exact format: GE.U D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertGE_U( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertGE_U(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::GE_U, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertIMASK( LLIR_Register *r0, int c0, int c1, int c2,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insIMASK( r0, c0, c1, c2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an IMASK instruction.

  Exact format: IMASK E[c], const4, pos, width (EC4C5C5)
*/
void InstructionFactory::insertIMASK( const WIR::TC_ERegV &Ec,
                                      unsigned int const4,
                                      unsigned int pos, unsigned int width,
                                      const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::IMASK, TC13::OperationFormat::EC4C5C5,
        new WIR_RegisterParameter( Ec, WIR_Usage::def ),
        new TC_Const4_Unsigned( const4 ),
        new TC_Const5_Unsigned( pos ),
        new TC_Const5_Unsigned( width ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertIMASK( LLIR_Register *r0, int c0,
                                      LLIR_Register *r1, int c1,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insIMASK( r0, c0, r1, c1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an IMASK instruction.

  Exact format: IMASK E[c], const4, D[d] (use), width (EC4DC5)
*/
void InstructionFactory::insertIMASK( const WIR::TC_ERegV &Ec,
                                      unsigned int const4,
                                      const WIR::TC_DRegV &Dd,
                                      unsigned int width,
                                      const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::IMASK, TC13::OperationFormat::EC4DC5,
        new WIR_RegisterParameter( Ec, WIR_Usage::def ),
        new TC_Const4_Unsigned( const4 ),
        new WIR_RegisterParameter( Dd, WIR_Usage::use ),
        new TC_Const5_Unsigned( width ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertIMASK( LLIR_Register *r0, LLIR_Register *r1,
                                      int c0, int c1,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insIMASK( r0, r1, c0, c1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an IMASK instruction.

  Exact format: IMASK E[c], D[b] (use), pos, width (EDC5C5)
*/
void InstructionFactory::insertIMASK( const WIR::TC_ERegV &Ec,
                                      const WIR::TC_DRegV &Db,
                                      unsigned int pos,
                                      unsigned int width,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertIMASK(const TC_ERegV&, const TC_DRegV&, unsigned int, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::IMASK, TC13::OperationFormat::EDC5C5,
        new WIR_RegisterParameter( Ec, WIR_Usage::def ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new TC_Const5_Unsigned( pos ),
        new TC_Const5_Unsigned( width ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertIMASK( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2, int c0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insIMASK( r0, r1, r2, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an IMASK instruction.

  Exact format: IMASK E[c], D[b] (use), D[d] (use), width (EDDC5)
*/
void InstructionFactory::insertIMASK( const WIR::TC_ERegV &Ec,
                                      const WIR::TC_DRegV &Db,
                                      const WIR::TC_DRegV &Dd,
                                      unsigned int width,
                                      const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::IMASK, TC13::OperationFormat::EDDC5,
        new WIR_RegisterParameter( Ec, WIR_Usage::def ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new WIR_RegisterParameter( Dd, WIR_Usage::use ),
        new TC_Const5_Unsigned( width ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertINSERT( LLIR_Register *r0, LLIR_Register *r1,
                                       int c0, int c1, int c2,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insINSERT( r0, r1, c0, c1, c2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an INSERT instruction.

  Exact format: INSERT D[c] (def), D[a] (use), const4, pos, width (DDC4C5C5)
*/
void InstructionFactory::insertINSERT( const WIR::TC_DRegV &Dc,
                                       const WIR::TC_DRegV &Da,
                                       unsigned int const4,
                                       unsigned int pos, unsigned int width,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertINSERT(const TC_DRegV&, const TC_DRegV&, unsigned int, unsigned int, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::INSERT, TC13::OperationFormat::DDC4C5C5,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const4_Unsigned( const4 ),
        new TC_Const5_Unsigned( pos ),
        new TC_Const5_Unsigned( width ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertINSERT( LLIR_Register *r0, LLIR_Register *r1,
                                       LLIR_Register *r2, int c0, int c1,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insINSERT( r0, r1, r2, c0, c1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an INSERT instruction.

  Exact format: INSERT D[c] (def), D[a] (use), D[b] (use), pos, width (DDDC5C5)
*/
void InstructionFactory::insertINSERT( const WIR::TC_DRegV &Dc,
                                       const WIR::TC_DRegV &Da,
                                       const WIR::TC_DRegV &Db,
                                       unsigned int pos, unsigned int width,
                                       const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::INSERT, TC13::OperationFormat::DDDC5C5,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new TC_Const5_Unsigned( pos ),
        new TC_Const5_Unsigned( width ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJ( const std::string &l0,
                                  const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJ( l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a J instruction.

  Exact format: J disp8 (SL)
*/
void InstructionFactory::insertJ( const WIR::WIR_BasicBlock &disp8,
                                  const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJ(const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::J,
          m16BitOperations ?
            TC13::OperationFormat::SL : TC13::OperationFormat::L,
          new WIR_LabelParameter( disp8 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJEQ( LLIR_Register *r0, int c0,
                                    const std::string &l0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJEQ( r0, c0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JEQ instruction.

  Exact format: JEQ D[a] (use), const4, disp15 (DC4L_1)
*/
void InstructionFactory::insertJEQ( const WIR::TC_DRegV &Da, int const4,
                                    const WIR::WIR_BasicBlock &disp15,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJEQ(const TC_DRegV&, int, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JEQ, TC13::OperationFormat::DC4L_1,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const4_Signed( const4 ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJEQ( LLIR_Register *r0, LLIR_Register *r1,
                                    const std::string &l0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJEQ( r0, r1, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JEQ instruction.

  Exact format: JEQ D[a] (use), D[b] (use), disp15 (DDL_1)
*/
void InstructionFactory::insertJEQ( const WIR::TC_DRegV &Da,
                                    const WIR::TC_DRegV &Db,
                                    const WIR::WIR_BasicBlock &disp15,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJEQ(const TC_DRegV&, const TC_DRegV&, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JEQ, TC13::OperationFormat::DDL_1,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJEQ_A( LLIR_Register *r0, LLIR_Register *r1,
                                      const std::string &l0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJEQ_A( r0, r1, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JEQ.A instruction.

  Exact format: JEQ.A A[a] (use), A[b] (use), disp15 (AAL)
*/
void InstructionFactory::insertJEQ_A( const WIR::TC_ARegV &Aa,
                                      const WIR::TC_ARegV &Ab,
                                      const WIR::WIR_BasicBlock &disp15,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJEQ_A(const TC_ARegV&, const TC_ARegV&, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JEQ_A, TC13::OperationFormat::AAL,
        new WIR_RegisterParameter( Aa, WIR_Usage::use ),
        new WIR_RegisterParameter( Ab, WIR_Usage::use ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJGE( LLIR_Register *r0, int c0,
                                    const std::string &l0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJGE( r0, c0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JGE instruction.

  Exact format: JGE D[a] (use), const4, disp15 (DC4L_1)
*/
void InstructionFactory::insertJGE( const WIR::TC_DRegV &Da, int const4,
                                    const WIR::WIR_BasicBlock &disp15,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJGE(const TC_DRegV&, int, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JGE, TC13::OperationFormat::DC4L_1,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const4_Signed( const4 ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJGE( LLIR_Register *r0, LLIR_Register *r1,
                                    const std::string &l0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJGE( r0, r1, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JGE instruction.

  Exact format: JGE D[a] (use), D[b] (use), disp15 (DDL_1)
*/
void InstructionFactory::insertJGE( const WIR::TC_DRegV &Da,
                                    const WIR::TC_DRegV &Db,
                                    const WIR::WIR_BasicBlock &disp15,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJGE(const TC_DRegV&, const TC_DRegV&, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JGE, TC13::OperationFormat::DDL_1,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJGE_U( LLIR_Register *r0, int c0,
                                      const std::string &l0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJGE_U( r0, c0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JGE_U instruction.

  Exact format: JGE_U D[a] (use), const4, disp15 (DC4L_2)
*/
void InstructionFactory::insertJGE_U( const WIR::TC_DRegV &Da,
                                      unsigned int const4,
                                      const WIR::WIR_BasicBlock &disp15,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJGE_U(const TC_DRegV&, unsigned int, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JGE_U, TC13::OperationFormat::DC4L_2,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const4_Unsigned( const4 ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJGE_U( LLIR_Register *r0, LLIR_Register *r1,
                                      const std::string &l0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJGE_U( r0, r1, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JGE_U instruction.

  Exact format: JGE.U D[a] (use), D[b] (use), disp15 (DDL_1)
*/
void InstructionFactory::insertJGE_U( const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db,
                                      const WIR::WIR_BasicBlock &disp15,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJGE_U(const TC_DRegV&, const TC_DRegV&, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JGE_U, TC13::OperationFormat::DDL_1,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJGEZ( LLIR_Register *r0, const std::string &l0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJGEZ( r0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JGEZ instruction.

  Exact format: JGEZ D[b] (use), disp4 (SDL)
*/
void InstructionFactory::insertJGEZ( const WIR::TC_DRegV &Db,
                                     const WIR::WIR_BasicBlock &disp4,
                                     const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { TC13::OpCode::JGEZ, TC13::OperationFormat::SDL,
          new WIR_RegisterParameter( Db, WIR_Usage::use ),
          new WIR_LabelParameter( disp4 ) } :
        WIR_Operation { TC13::OpCode::JGE, TC13::OperationFormat::DC4L_1,
          new WIR_RegisterParameter( Db, WIR_Usage::use ),
          new TC_Const4_Signed( 0 ),
          new WIR_LabelParameter( disp4 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJGTZ( LLIR_Register *r0, const std::string &l0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJGTZ( r0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JGTZ instruction.

  Exact format: JGTZ D[b] (use), disp4 (SDL)
*/
void InstructionFactory::insertJGTZ( const WIR::TC_DRegV &Db,
                                     const WIR::WIR_BasicBlock &disp4,
                                     const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { TC13::OpCode::JGTZ, TC13::OperationFormat::SDL,
          new WIR_RegisterParameter( Db, WIR_Usage::use ),
          new WIR_LabelParameter( disp4 ) } :
        WIR_Operation { TC13::OpCode::JGE, TC13::OperationFormat::DC4L_1,
          new WIR_RegisterParameter( Db, WIR_Usage::use ),
          new TC_Const4_Signed( 1 ),
          new WIR_LabelParameter( disp4 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJLEZ( LLIR_Register *r0, const std::string &l0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJLEZ( r0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JLEZ instruction.

  Exact format: JLEZ D[b] (use), disp4 (SDL)
*/
void InstructionFactory::insertJLEZ( const WIR::TC_DRegV &Db,
                                     const WIR::WIR_BasicBlock &disp4,
                                     const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { TC13::OpCode::JLEZ, TC13::OperationFormat::SDL,
          new WIR_RegisterParameter( Db, WIR_Usage::use ),
          new WIR_LabelParameter( disp4 ) } :
        WIR_Operation { TC13::OpCode::JLT, TC13::OperationFormat::DC4L_1,
          new WIR_RegisterParameter( Db, WIR_Usage::use ),
          new TC_Const4_Signed( 1 ),
          new WIR_LabelParameter( disp4 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJLT( LLIR_Register *r0, int c0,
                                    const std::string &l0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJLT( r0, c0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JLT instruction.

  Exact format: JLT D[a] (use), const4, disp15 (DC4L_1)
*/
void InstructionFactory::insertJLT( const WIR::TC_DRegV &Da, int const4,
                                    const WIR::WIR_BasicBlock &disp15,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJLT(const TC_DRegV&, int, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JLT, TC13::OperationFormat::DC4L_1,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const4_Signed( const4 ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJLT( LLIR_Register *r0, LLIR_Register *r1,
                                    const std::string &l0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJLT( r0, r1, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JLT instruction.

  Exact format: JLT D[a] (use), D[b] (use), disp15 (DDL_1)
*/
void InstructionFactory::insertJLT( const WIR::TC_DRegV &Da,
                                    const WIR::TC_DRegV &Db,
                                    const WIR::WIR_BasicBlock &disp15,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJLT(const TC_DRegV&, const TC_DRegV&, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JLT, TC13::OperationFormat::DDL_1,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJLT_U( LLIR_Register *r0, int c0,
                                      const std::string &l0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJLT_U( r0, c0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JLT_U instruction.

  Exact format: JLT_U D[a] (use), const4, disp15 (DC4L_2)
*/
void InstructionFactory::insertJLT_U( const WIR::TC_DRegV &Da,
                                      unsigned int const4,
                                      const WIR::WIR_BasicBlock &disp15,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJLT_U(const TC_DRegV&, unsigned int, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JLT_U, TC13::OperationFormat::DC4L_2,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const4_Unsigned( const4 ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJLT_U( LLIR_Register *r0, LLIR_Register *r1,
                                      const std::string &l0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJLT_U( r0, r1, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JLT_U instruction.

  Exact format: JLT.U D[a] (use), D[b] (use), disp15 (DDL_1)
*/
void InstructionFactory::insertJLT_U( const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db,
                                      const WIR::WIR_BasicBlock &disp15,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJLT_U(const TC_DRegV&, const TC_DRegV&, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JLT_U, TC13::OperationFormat::DDL_1,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJLTZ( LLIR_Register *r0, const std::string &l0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJLTZ( r0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JLTZ instruction.

  Exact format: JLTZ D[b] (use), disp4 (SDL)
*/
void InstructionFactory::insertJLTZ( const WIR::TC_DRegV &Db,
                                     const WIR::WIR_BasicBlock &disp4,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJLTZ(const TC_DRegV&, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { TC13::OpCode::JLTZ, TC13::OperationFormat::SDL,
          new WIR_RegisterParameter( Db, WIR_Usage::use ),
          new WIR_LabelParameter( disp4 ) } :
        WIR_Operation { TC13::OpCode::JLT, TC13::OperationFormat::DC4L_1,
          new WIR_RegisterParameter( Db, WIR_Usage::use ),
          new TC_Const4_Signed( 0 ),
          new WIR_LabelParameter( disp4 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJNE( LLIR_Register *r0, int c0,
                                    const std::string &l0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJNE( r0, c0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JNE instruction.

  Exact format: JNE D[a] (use), const4, disp15 (DC4L_1)
*/
void InstructionFactory::insertJNE( const WIR::TC_DRegV &Da, int const4,
                                    const WIR::WIR_BasicBlock &disp15,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJNE(const TC_DRegV&, int, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JNE, TC13::OperationFormat::DC4L_1,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const4_Signed( const4 ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJNE( LLIR_Register *r0, LLIR_Register *r1,
                                    const std::string &l0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJNE( r0, r1, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JNE instruction.

  Exact format: JNE D[a] (use), D[b] (use), disp15 (DDL_1)
*/
void InstructionFactory::insertJNE( const WIR::TC_DRegV &Da,
                                    const WIR::TC_DRegV &Db,
                                    const WIR::WIR_BasicBlock &disp15,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJNE(const TC_DRegV&, const TC_DRegV&, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JNE, TC13::OperationFormat::DDL_1,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJNE_A( LLIR_Register *r0, LLIR_Register *r1,
                                      const std::string &l0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJNE_A( r0, r1, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JNE.A instruction.

  Exact format: JNE.A A[a] (use), A[b] (use), disp15 (AAL)
*/
void InstructionFactory::insertJNE_A( const WIR::TC_ARegV &Aa,
                                      const WIR::TC_ARegV &Ab,
                                      const WIR::WIR_BasicBlock &disp15,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJNE_A(const TC_ARegV&, const TC_ARegV&, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JNE_A, TC13::OperationFormat::AAL,
        new WIR_RegisterParameter( Aa, WIR_Usage::use ),
        new WIR_RegisterParameter( Ab, WIR_Usage::use ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJNED( LLIR_Register *r0, int c0,
                                     const std::string &l0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJNED( r0, c0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JNED instruction.

  Exact format: JNED D[a] (defuse), const4, disp15 (DC4L_3)
*/
void InstructionFactory::insertJNED( const WIR::TC_DRegV &Da, int const4,
                                     const WIR::WIR_BasicBlock &disp15,
                                     const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JNED, TC13::OperationFormat::DC4L_3,
        new WIR_RegisterParameter( Da, WIR_Usage::defuse ),
        new TC_Const4_Signed( const4 ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJNED( LLIR_Register *r0, LLIR_Register *r1,
                                     const std::string &l0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJNED( r0, r1, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JNED instruction.

  Exact format: JNED D[a] (defuse), D[b] (use), disp15 (DDL_1)
*/
void InstructionFactory::insertJNED( const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const WIR::WIR_BasicBlock &disp15,
                                     const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JNED, TC13::OperationFormat::DDL_2,
        new WIR_RegisterParameter( Da, WIR_Usage::defuse ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJNEI( LLIR_Register *r0, int c0,
                                     const std::string &l0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJNEI( r0, c0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JNEI instruction.

  Exact format: JNEI D[a] (defuse), const4, disp15 (DC4L_3)
*/
void InstructionFactory::insertJNEI( const WIR::TC_DRegV &Da, int const4,
                                     const WIR::WIR_BasicBlock &disp15,
                                     const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JNEI, TC13::OperationFormat::DC4L_3,
        new WIR_RegisterParameter( Da, WIR_Usage::defuse ),
        new TC_Const4_Signed( const4 ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJNEI( LLIR_Register *r0, LLIR_Register *r1,
                                     const std::string &l0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJNEI( r0, r1, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JNEI instruction.

  Exact format: JNEI D[a] (defuse), D[b] (use), disp15 (DDL_1)
*/
void InstructionFactory::insertJNEI( const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const WIR::WIR_BasicBlock &disp15,
                                     const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JNEI, TC13::OperationFormat::DDL_2,
        new WIR_RegisterParameter( Da, WIR_Usage::defuse ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJNZ( LLIR_Register *r0, const std::string &l0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJNZ( r0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JNZ instruction.

  Exact format: JNZ D[b] (use), disp4 (SDL)
*/
void InstructionFactory::insertJNZ( const WIR::TC_DRegV &Db,
                                    const WIR::WIR_BasicBlock &disp4,
                                    const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { TC13::OpCode::JNZ, TC13::OperationFormat::SDL,
          new WIR_RegisterParameter( Db, WIR_Usage::use ),
          new WIR_LabelParameter( disp4 ) } :
        WIR_Operation { TC13::OpCode::JNE, TC13::OperationFormat::DC4L_1,
          new WIR_RegisterParameter( Db, WIR_Usage::use ),
          new TC_Const4_Signed( 0 ),
          new WIR_LabelParameter( disp4 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJNZ_A( LLIR_Register *r0, const std::string &l0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJNZ_A( r0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JNZ.A instruction.

  Exact format: JNZ.A A[a] (use), disp4 (SAL_1)
*/
void InstructionFactory::insertJNZ_A( const WIR::TC_ARegV &Aa,
                                      const WIR::WIR_BasicBlock &disp4,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJNZ_A(const TC_ARegV&, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::JNZ_A,
          m16BitOperations ?
            TC13::OperationFormat::SAL_1 : TC13::OperationFormat::AL_2,
          new WIR_RegisterParameter( Aa, WIR_Usage::use ),
          new WIR_LabelParameter( disp4 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJNZ_T( LLIR_Register *r0, int n,
                                      const std::string &l0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJNZ_T( r0, n, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JNZ.T instruction.

  Exact format: JNZ.T D[a] (use), n, disp15 (DC5L)
*/
void InstructionFactory::insertJNZ_T( const WIR::TC_DRegV &Da, unsigned int n,
                                      const WIR::WIR_BasicBlock &disp15,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJNZ_T(const TC_DRegV&, unsigned int, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JNZ_T, TC13::OperationFormat::DC5L,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const5_Unsigned( n ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJZ( LLIR_Register *r0, const std::string &l0,
                                   const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJZ( r0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JZ instruction.

  Exact format: JZ D[b] (use), disp4 (SDL)
*/
void InstructionFactory::insertJZ( const WIR::TC_DRegV &Db,
                                   const WIR::WIR_BasicBlock &disp4,
                                   const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJZ(const TC_DRegV&, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { TC13::OpCode::JZ, TC13::OperationFormat::SDL,
          new WIR_RegisterParameter( Db, WIR_Usage::use ),
          new WIR_LabelParameter( disp4 ) } :
        WIR_Operation { TC13::OpCode::JEQ, TC13::OperationFormat::DC4L_1,
          new WIR_RegisterParameter( Db, WIR_Usage::use ),
          new TC_Const4_Signed( 0 ),
          new WIR_LabelParameter( disp4 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertJZ_A( LLIR_Register *r0, const std::string &l0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJZ_A( r0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JZ.A instruction.

  Exact format: JZ.A A[a] (use), disp4 (SAL_1)
*/
void InstructionFactory::insertJZ_A( const WIR::TC_ARegV &Aa,
                                     const WIR::WIR_BasicBlock &disp4,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJZ_A(const TC_ARegV&, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( m16BitOperations ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::JZ_A, TC13::OperationFormat::SAL_1,
          new WIR_RegisterParameter( Aa, WIR_Usage::use ),
          new WIR_LabelParameter( disp4 ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    auto &Ax = createAReg();

    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LEA, TC13::OperationFormat::AC18ABSA,
          new WIR_RegisterParameter( Ax, WIR_Usage::def ),
          new TC_Const18_Unsigned( 0 ) } } );

    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::JEQ_A, TC13::OperationFormat::AAL,
          new WIR_RegisterParameter( Aa, WIR_Usage::use ),
          new WIR_RegisterParameter( Ax, WIR_Usage::use ),
          new WIR_LabelParameter( disp4 ) } } );

    ADDDEBUGINFO( i, exp, type );
  }
};


void InstructionFactory::insertJZ_T( LLIR_Register *r0, int n,
                                     const std::string &l0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insJZ_T( r0, n, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a JZ.T instruction.

  Exact format: JZ.T D[a] (use), n, disp15 (DC5L)
*/
void InstructionFactory::insertJZ_T( const WIR::TC_DRegV &Da, unsigned int n,
                                     const WIR::WIR_BasicBlock &disp15,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertJZ_T(const TC_DRegV&, unsigned int, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::JZ_T, TC13::OperationFormat::DC5L,
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const5_Unsigned( n ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertLD_A( LLIR_Register *r0, const std::string &o0,
                                     LLIR_Register *r1, int c0,
                                     const IR_Exp *exp, StmtType type )
{
  // TODO: The handling of these address offsets should be handled in the
  //       'ins...' functions because some parts of the code selector use
  //       them directly instead of using the 'insert...' functions.
  //       Additionally this offset fixing code should be centralized in
  //       a single function instead of duplicating it in any ST or LD
  //       instruction.
  if ( ( c0 >= minSignedConst10Value ) && ( c0 <= maxSignedConst10Value ) ) {
    mCurrentInstr = insLD_A( r0, o0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

    handleDataAccess( mCurrentInstr, exp );
  } else {
    if ( o0 == OPER_PREINC ) {
      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );

      insertLD_A( r0, OPER_BASE, r1, 0, exp );
    } else

    if ( o0 == OPER_POSTINC ) {
      insertLD_A( r0, OPER_BASE, r1, 0, exp );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );
    } else {
      if ( ( c0 >= minSignedConst16Value ) &&
           ( c0 <= maxSignedConst16Value ) ) {
        mCurrentInstr = insLD_A( r0, o0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

        // If exp defined, insert assembler debug information.
        if ( mConfig.getGenerateDebugFlag() && exp )
          addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

        handleDataAccess( mCurrentInstr, exp );
      } else {
        LLIR_Register *reg1 = CreateRegister( "", true );

        handleLargeOffset( reg1, OPER_BASE, c0, r1 );

        insertLD_A( r0, o0, reg1, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.A instruction.

  Exact format: LD.A A[a] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is included. Exact formats:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.A A[a] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertLD_A( const WIR::TC_ARegV &Aa,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_A(const TC_ARegV&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_A, TC13::OperationFormat::AAC16BOA,
          new WIR_RegisterParameter( Aa, WIR_Usage::def ),
          new WIR_RegisterParameter( Ab, WIR_Usage::use ),
          new TC_Const16_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else {
    auto &Ax = createAReg();
    auto p = splitOffset( off );

    insertADDIH_A( Ax, Ab, p.first, exp, type );
    insertLD_A( Aa, Ax, p.second, exp, type );
  }
};


/*
  Inserts a LD.A instruction.

  Exact formats:

  LD.A A[a] (def), [+A[b] (defuse)]off (AAC10PIA)
  LD.A A[a] (def), [A[b] (defuse)+]off (AAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)

  Exact formats for post-increment:

  LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.A A[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (AAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)

  Exact formats for post-increment either:

  LD.A A[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (AAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertLD_A( const WIR::TC_ARegV &Aa,
                                     const WIR::TC13::AddressingMode &m,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_A(const TC_ARegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_A, TC13::OperationFormat::AAC10PIA,
          new WIR_RegisterParameter( Aa, WIR_Usage::def ),
          new WIR_AddressingModeParameter( m ),
          new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
          new TC_Const10_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      insertLEA( Ab, Ab, off, exp, type );
      insertLD_A( Aa, Ab, 0, exp, type );
    } else {
      insertLD_A( Aa, Ab, 0, exp, type );
      insertLEA( Ab, Ab, off, exp, type );
    }
  } else {
    auto p = splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertADDIH_A( Ab, Ab, p.first, exp, type );
        insertLD_A( Aa, m, Ab, p.second, exp, type );
      } else {
        handleLargeOffset( Ab, Ab, off, exp, type );
        insertLD_A( Aa, Ab, 0, exp, type );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertLD_A( Aa, m, Ab, p.second, exp, type );
        insertADDIH_A( Ab, Ab, p.first, exp, type );
      } else {
        insertLD_A( Aa, Ab, 0, exp, type );
        handleLargeOffset( Ab, Ab, off, exp, type );
      }
    }
  }
};


void InstructionFactory::insertLD_A( LLIR_Register *r0, const std::string &o0,
                                     LLIR_Register *r1, const std::string &l0,
                                     const std::string &l1,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLD_A( r0, o0, r1, l0, l1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

  handleDataAccess( mCurrentInstr, exp );
};


/*
  Inserts a LD.A instruction.

  Exact format: LD.A A[a] (def), [A[b] (use)] LO:label (AALBOA)
*/
void InstructionFactory::insertLD_A( const WIR::TC_ARegV &Aa,
                                     const WIR::TC_ARegV &Ab,
                                     const WIR::WIR_Data &d,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_A(const TC_ARegV&, const TC_ARegV&, const WIR_Data&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::LD_A, TC13::OperationFormat::AALC16BOA,
        new WIR_RegisterParameter( Aa, WIR_Usage::def ),
        new WIR_RegisterParameter( Ab, WIR_Usage::use ),
        new WIR_LabelParameter( d ), new TC_Const16_Signed( 0 ) } } );

  ADDDEBUGINFO( i, exp, type );
  handleDataAccess( i, exp );
};


void InstructionFactory::insertLD_B( LLIR_Register *r0, const std::string &o0,
                                     LLIR_Register *r1, int c0,
                                     const IR_Exp *exp, StmtType type )
{
  if ( ( c0 >= minSignedConst10Value ) && ( c0 <= maxSignedConst10Value ) ) {
    mCurrentInstr = insLD_B( r0, o0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

    handleDataAccess( mCurrentInstr, exp );
 } else {
    if ( o0 == OPER_PREINC ) {
      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );

      insertLD_B( r0, OPER_BASE, r1, 0, exp );
    } else

    if ( o0 == OPER_POSTINC ) {
      insertLD_B( r0, OPER_BASE, r1, 0, exp );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );
    } else {
      LLIR_Register *reg1 = CreateRegister( "", true );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( reg1, o0, r1, c0 );
      else
        handleLargeOffset( reg1, OPER_BASE, c0, r1 );

      insertLD_B( r0, o0, reg1, 0, exp );
    }
  }
};


/*
  Inserts a LD.B instruction.

  Exact format: LD.B D[a] (def), [A[b] (use)]off (DAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact formats
  either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.B D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.B D[a] (def), [A[x] (use)]0 (DAC10BOA)
*/
void InstructionFactory::insertLD_B( const WIR::TC_DRegV &Da,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_B(const TC_DRegV&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_B, TC13::OperationFormat::DAC10BOA,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_RegisterParameter( Ab, WIR_Usage::use ),
          new TC_Const10_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else {
    auto &Ax = createAReg();
    auto p = splitOffset( off );

    insertADDIH_A( Ax, Ab, p.first, exp, type );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
      insertLD_B( Da, Ax, p.second, exp, type );
    else {
      insertLEA( Ax, Ax, p.second, exp, type );
      insertLD_B( Da, Ax, 0, exp, type );
    }
  }
};


/*
  Inserts a LD.B instruction.

  Exact formats:

  LD.B D[a] (def), [+A[b] (defuse)]off (DAC10PIA)
  LD.B D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment:

  LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.B D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment either:

  LD.B D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertLD_B( const WIR::TC_DRegV &Da,
                                     const WIR::TC13::AddressingMode &m,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_B(const TC_DRegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_B, TC13::OperationFormat::DAC10PIA,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_AddressingModeParameter( m ),
          new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
          new TC_Const10_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      insertLEA( Ab, Ab, off, exp, type );
      insertLD_B( Da, Ab, 0, exp, type );
    } else {
      insertLD_B( Da, Ab, 0, exp, type );
      insertLEA( Ab, Ab, off, exp, type );
    }
  } else {
    auto p = splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertADDIH_A( Ab, Ab, p.first, exp, type );
        insertLD_B( Da, m, Ab, p.second, exp, type );
      } else {
        handleLargeOffset( Ab, Ab, off, exp, type );
        insertLD_B( Da, Ab, 0, exp, type );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertLD_B( Da, m, Ab, p.second, exp, type );
        insertADDIH_A( Ab, Ab, p.first, exp, type );
      } else {
        insertLD_B( Da, Ab, 0, exp, type );
        handleLargeOffset( Ab, Ab, off, exp, type );
      }
    }
  }
};


void InstructionFactory::insertLD_BU( LLIR_Register *r0, const std::string &o0,
                                      LLIR_Register *r1, int c0,
                                      const IR_Exp *exp, StmtType type )
{
  if ( ( c0 >= minSignedConst10Value ) && ( c0 <= maxSignedConst10Value ) ) {
    mCurrentInstr = insLD_BU( r0, o0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

    handleDataAccess( mCurrentInstr, exp );
 } else {
    if ( o0 == OPER_PREINC ) {
      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );

      insertLD_BU( r0, OPER_BASE, r1, 0, exp );
    } else

    if ( o0 == OPER_POSTINC ) {
      insertLD_BU( r0, OPER_BASE, r1, 0, exp );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );
    } else {
      LLIR_Register *reg1 = CreateRegister( "", true );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( reg1, o0, r1, c0 );
      else
        handleLargeOffset( reg1, OPER_BASE, c0, r1 );

      insertLD_BU( r0, o0, reg1, 0, exp );
    }
  }
};


/*
  Inserts a LD.BU instruction.

  Exact format: LD.BU D[a] (def), [A[b] (use)]off (DAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact formats
  either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.BU D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.BU D[a] (def), [A[x] (use)]0 (DAC10BOA)
*/
void InstructionFactory::insertLD_BU( const WIR::TC_DRegV &Da,
                                      const WIR::TC_ARegV &Ab, int off,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_BU(const TC_DRegV&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_BU, TC13::OperationFormat::DAC10BOA,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_RegisterParameter( Ab, WIR_Usage::use ),
          new TC_Const10_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else {
    auto &Ax = createAReg();
    auto p = splitOffset( off );

    insertADDIH_A( Ax, Ab, p.first, exp, type );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
      insertLD_BU( Da, Ax, p.second, exp, type );
    else {
      insertLEA( Ax, Ax, p.second, exp, type );
      insertLD_BU( Da, Ax, 0, exp, type );
    }
  }
};


/*
  Inserts a LD.BU instruction.

  Exact formats:

  LD.BU D[a] (def), [+A[b] (defuse)]off (DAC10PIA)
  LD.BU D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment:

  LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.BU D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment either:

  LD.BU D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertLD_BU( const WIR::TC_DRegV &Da,
                                      const WIR::TC13::AddressingMode &m,
                                      const WIR::TC_ARegV &Ab, int off,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_BU(const TC_DRegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_BU, TC13::OperationFormat::DAC10PIA,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_AddressingModeParameter( m ),
          new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
          new TC_Const10_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      insertLEA( Ab, Ab, off, exp, type );
      insertLD_BU( Da, Ab, 0, exp, type );
    } else {
      insertLD_BU( Da, Ab, 0, exp, type );
      insertLEA( Ab, Ab, off, exp, type );
    }
  } else {
    auto p = splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertADDIH_A( Ab, Ab, p.first, exp, type );
        insertLD_BU( Da, m, Ab, p.second, exp, type );
      } else {
        handleLargeOffset( Ab, Ab, off, exp, type );
        insertLD_BU( Da, Ab, 0, exp, type );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertLD_BU( Da, m, Ab, p.second, exp, type );
        insertADDIH_A( Ab, Ab, p.first, exp, type );
      } else {
        insertLD_BU( Da, Ab, 0, exp, type );
        handleLargeOffset( Ab, Ab, off, exp, type );
      }
    }
  }
};


void InstructionFactory::insertLD_D( LLIR_Register *r0, const std::string &o0,
                                     LLIR_Register *r1, int c0,
                                     const IR_Exp *exp, StmtType type )
{
  if ( ( c0 >= minSignedConst10Value ) && ( c0 <= maxSignedConst10Value ) ) {
    mCurrentInstr = insLD_D( r0, o0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

    handleDataAccess( mCurrentInstr, exp );
 } else {
    if ( o0 == OPER_PREINC ) {
      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );

      insertLD_D( r0, OPER_BASE, r1, 0, exp );
    } else

    if ( o0 == OPER_POSTINC ) {
      insertLD_D( r0, OPER_BASE, r1, 0, exp );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );
    } else {
      LLIR_Register *reg1 = CreateRegister( "", true );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( reg1, o0, r1, c0 );
      else
        handleLargeOffset( reg1, OPER_BASE, c0, r1 );

      insertLD_D( r0, o0, reg1, 0, exp );
    }
  }
};


/*
  Inserts a LD.D instruction.

  Exact format: LD.D E[a] (def), [A[b] (use)]off (EAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact formats
  either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.D E[a] (def), [A[x] (use)]<lower 10 bits of off> (EAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.D E[a] (def), [A[x] (use)]0 (EAC10BOA)
*/
void InstructionFactory::insertLD_D( const WIR::TC_ERegV &Ea,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_D(const TC_ERegV&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_D, TC13::OperationFormat::EAC10BOA,
          new WIR_RegisterParameter( Ea, WIR_Usage::def ),
          new WIR_RegisterParameter( Ab, WIR_Usage::use ),
          new TC_Const10_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else {
    auto &Ax = createAReg();
    auto p = splitOffset( off );

    insertADDIH_A( Ax, Ab, p.first, exp, type );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
      insertLD_D( Ea, Ax, p.second, exp, type );
    else {
      insertLEA( Ax, Ax, p.second, exp, type );
      insertLD_D( Ea, Ax, 0, exp, type );
    }
  }
};


/*
  Inserts a LD.D instruction.

  Exact formats:

  LD.D E[a] (def), [+A[b] (defuse)]off (EAC10PIA)
  LD.D E[a] (def), [A[b] (defuse)+]off (EAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA)

  Exact formats for post-increment:

  LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.D E[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (EAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA)

  Exact formats for post-increment either:

  LD.D E[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (EAC10PIA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertLD_D( const WIR::TC_ERegV &Ea,
                                     const WIR::TC13::AddressingMode &m,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_D(const TC_ERegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_D, TC13::OperationFormat::EAC10PIA,
          new WIR_RegisterParameter( Ea, WIR_Usage::def ),
          new WIR_AddressingModeParameter( m ),
          new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
          new TC_Const10_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      insertLEA( Ab, Ab, off, exp, type );
      insertLD_D( Ea, Ab, 0, exp, type );
    } else {
      insertLD_D( Ea, Ab, 0, exp, type );
      insertLEA( Ab, Ab, off, exp, type );
    }
  } else {
    auto p = splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertADDIH_A( Ab, Ab, p.first, exp, type );
        insertLD_D( Ea, m, Ab, p.second, exp, type );
      } else {
        handleLargeOffset( Ab, Ab, off, exp, type );
        insertLD_D( Ea, Ab, 0, exp, type );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertLD_D( Ea, m, Ab, p.second, exp, type );
        insertADDIH_A( Ab, Ab, p.first, exp, type );
      } else {
        insertLD_D( Ea, Ab, 0, exp, type );
        handleLargeOffset( Ab, Ab, off, exp, type );
      }
    }
  }
};


void InstructionFactory::insertLD_H( LLIR_Register *r0, const std::string &o0,
                                     LLIR_Register *r1, int c0,
                                     const IR_Exp *exp, StmtType type )
{
  if ( ( c0 >= minSignedConst10Value ) && ( c0 <= maxSignedConst10Value ) ) {
    mCurrentInstr = insLD_H( r0, o0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

    handleDataAccess( mCurrentInstr, exp );
 } else {
    if ( o0 == OPER_PREINC ) {
      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );

      insertLD_H( r0, OPER_BASE, r1, 0, exp );
    } else

    if ( o0 == OPER_POSTINC ) {
      insertLD_H( r0, OPER_BASE, r1, 0, exp );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );
    } else {
      LLIR_Register *reg1 = CreateRegister( "", true );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( reg1, o0, r1, c0 );
      else
        handleLargeOffset( reg1, OPER_BASE, c0, r1 );

      insertLD_H( r0, o0, reg1, 0, exp );
    }
  }
};


/*
  Inserts a LD.H instruction.

  Exact format: LD.H D[a] (def), [A[b] (use)]off (DAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact formats
  either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.H D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.H D[a] (def), [A[x] (use)]0 (DAC10BOA)
*/
void InstructionFactory::insertLD_H( const WIR::TC_DRegV &Da,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_H(const TC_DRegV&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_H, TC13::OperationFormat::DAC10BOA,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_RegisterParameter( Ab, WIR_Usage::use ),
          new TC_Const10_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else {
    auto &Ax = createAReg();
    auto p = splitOffset( off );

    insertADDIH_A( Ax, Ab, p.first, exp, type );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
      insertLD_H( Da, Ax, p.second, exp, type );
    else {
      insertLEA( Ax, Ax, p.second, exp, type );
      insertLD_H( Da, Ax, 0, exp, type );
    }
  }
};


/*
  Inserts a LD.H instruction.

  Exact formats:

  LD.H D[a] (def), [+A[b] (defuse)]off (DAC10PIA)
  LD.H D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment:

  LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.H D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment either:

  LD.H D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertLD_H( const WIR::TC_DRegV &Da,
                                     const WIR::TC13::AddressingMode &m,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_H(const TC_DRegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_H, TC13::OperationFormat::DAC10PIA,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_AddressingModeParameter( m ),
          new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
          new TC_Const10_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      insertLEA( Ab, Ab, off, exp, type );
      insertLD_H( Da, Ab, 0, exp, type );
    } else {
      insertLD_H( Da, Ab, 0, exp, type );
      insertLEA( Ab, Ab, off, exp, type );
    }
  } else {
    auto p = splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertADDIH_A( Ab, Ab, p.first, exp, type );
        insertLD_H( Da, m, Ab, p.second, exp, type );
      } else {
        handleLargeOffset( Ab, Ab, off, exp, type );
        insertLD_H( Da, Ab, 0, exp, type );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertLD_H( Da, m, Ab, p.second, exp, type );
        insertADDIH_A( Ab, Ab, p.first, exp, type );
      } else {
        insertLD_H( Da, Ab, 0, exp, type );
        handleLargeOffset( Ab, Ab, off, exp, type );
      }
    }
  }
};


void InstructionFactory::insertLD_HU( LLIR_Register *r0, const std::string &o0,
                                      LLIR_Register *r1, int c0,
                                      const IR_Exp *exp, StmtType type )
{
  if ( ( c0 >= minSignedConst10Value ) && ( c0 <= maxSignedConst10Value ) ) {
    mCurrentInstr = insLD_HU( r0, o0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

    handleDataAccess( mCurrentInstr, exp );
 } else {
    if ( o0 == OPER_PREINC ) {
      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );

      insertLD_HU( r0, OPER_BASE, r1, 0, exp );
    } else

    if ( o0 == OPER_POSTINC ) {
      insertLD_HU( r0, OPER_BASE, r1, 0, exp );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );
    } else {
      LLIR_Register *reg1 = CreateRegister( "", true );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( reg1, o0, r1, c0 );
      else
        handleLargeOffset( reg1, OPER_BASE, c0, r1 );

      insertLD_HU( r0, o0, reg1, 0, exp );
    }
  }
};


/*
  Inserts a LD.HU instruction.

  Exact format: LD.HU D[a] (def), [A[b] (use)]off (DAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact formats
  either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.HU D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.HU D[a] (def), [A[x] (use)]0 (DAC10BOA)
*/
void InstructionFactory::insertLD_HU( const WIR::TC_DRegV &Da,
                                      const WIR::TC_ARegV &Ab, int off,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_HU(const TC_DRegV&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_HU, TC13::OperationFormat::DAC10BOA,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_RegisterParameter( Ab, WIR_Usage::use ),
          new TC_Const10_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else {
    auto &Ax = createAReg();
    auto p = splitOffset( off );

    insertADDIH_A( Ax, Ab, p.first, exp, type );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
      insertLD_HU( Da, Ax, p.second, exp, type );
    else {
      insertLEA( Ax, Ax, p.second, exp, type );
      insertLD_HU( Da, Ax, 0, exp, type );
    }
  }
};


/*
  Inserts a LD.HU instruction.

  Exact formats:

  LD.HU D[a] (def), [+A[b] (defuse)]off (DAC10PIA)
  LD.HU D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment:

  LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.HU D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment either:

  LD.HU D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertLD_HU( const WIR::TC_DRegV &Da,
                                      const WIR::TC13::AddressingMode &m,
                                      const WIR::TC_ARegV &Ab, int off,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_HU(const TC_DRegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_HU, TC13::OperationFormat::DAC10PIA,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_AddressingModeParameter( m ),
          new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
          new TC_Const10_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      insertLEA( Ab, Ab, off, exp, type );
      insertLD_HU( Da, Ab, 0, exp, type );
    } else {
      insertLD_HU( Da, Ab, 0, exp, type );
      insertLEA( Ab, Ab, off, exp, type );
    }
  } else {
    auto p = splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertADDIH_A( Ab, Ab, p.first, exp, type );
        insertLD_HU( Da, m, Ab, p.second, exp, type );
      } else {
        handleLargeOffset( Ab, Ab, off, exp, type );
        insertLD_HU( Da, Ab, 0, exp, type );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertLD_HU( Da, m, Ab, p.second, exp, type );
        insertADDIH_A( Ab, Ab, p.first, exp, type );
      } else {
        insertLD_HU( Da, Ab, 0, exp, type );
        handleLargeOffset( Ab, Ab, off, exp, type );
      }
    }
  }
};


void InstructionFactory::insertLD_W( LLIR_Register *r0, const std::string &o0,
                                     LLIR_Register *r1, int c0,
                                     const IR_Exp *exp, StmtType type )
{
  if ( ( c0 >= minSignedConst10Value ) && ( c0 <= maxSignedConst10Value ) ) {
    mCurrentInstr = insLD_W( r0, o0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

    handleDataAccess( mCurrentInstr, exp );
  } else {
    if ( o0 == OPER_PREINC ) {
      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );

      insertLD_W( r0, OPER_BASE, r1, 0, exp );
    } else

    if ( o0 == OPER_POSTINC ) {
      insertLD_W( r0, OPER_BASE, r1, 0, exp );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r1, OPER_BASE, r1, c0 );
      else
        handleLargeOffset( r1, OPER_BASE, c0 );
    } else {
      if ( ( c0 >= minSignedConst16Value ) &&
           ( c0 <= maxSignedConst16Value ) ) {
        mCurrentInstr = insLD_W( r0, o0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

        // If exp defined, insert assembler debug information.
        if ( mConfig.getGenerateDebugFlag() && exp )
          addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

        handleDataAccess( mCurrentInstr, exp );
      } else {
        LLIR_Register *reg1 = CreateRegister( "", true );

        handleLargeOffset( reg1, OPER_BASE, c0, r1 );

        insertLD_W( r0, o0, reg1, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.W instruction.

  Exact format: LD.W D[a] (def), [A[b] (use)]off (DAC16BOA)

  Handling of address offsets beyond signed 16 bits is included. Exact formats:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.A A[a] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertLD_W( const WIR::TC_DRegV &Da,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_W(const TC_DRegV&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_W, TC13::OperationFormat::DAC16BOA,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_RegisterParameter( Ab, WIR_Usage::use ),
          new TC_Const16_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else {
    auto &Ax = createAReg();
    auto p = splitOffset( off );

    insertADDIH_A( Ax, Ab, p.first, exp, type );
    insertLD_W( Da, Ax, p.second, exp, type );
  }
};


/*
  Inserts a LD.W instruction.

  Exact formats:

  LD.W D[a] (def), [+A[b] (defuse)]off (DAC10PIA)
  LD.W D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA)

  Exact formats for post-increment:

  LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.W D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA)

  Exact formats for post-increment either:

  LD.W D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertLD_W( const WIR::TC_DRegV &Da,
                                     const WIR::TC13::AddressingMode &m,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_W(const TC_DRegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LD_W, TC13::OperationFormat::DAC10PIA,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_AddressingModeParameter( m ),
          new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
          new TC_Const10_Signed( off ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      insertLEA( Ab, Ab, off, exp, type );
      insertLD_W( Da, Ab, 0, exp, type );
    } else {
      insertLD_W( Da, Ab, 0, exp, type );
      insertLEA( Ab, Ab, off, exp, type );
    }
  } else {
    auto p = splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertADDIH_A( Ab, Ab, p.first, exp, type );
        insertLD_W( Da, m, Ab, p.second, exp, type );
      } else {
        handleLargeOffset( Ab, Ab, off, exp, type );
        insertLD_W( Da, Ab, 0, exp, type );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertLD_W( Da, m, Ab, p.second, exp, type );
        insertADDIH_A( Ab, Ab, p.first, exp, type );
      } else {
        insertLD_W( Da, Ab, 0, exp, type );
        handleLargeOffset( Ab, Ab, off, exp, type );
      }
    }
  }
};


void InstructionFactory::insertLD_W( LLIR_Register *r0, const std::string &o0,
                                     LLIR_Register *r1, const std::string &l0,
                                     const std::string &l1,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLD_W( r0, o0, r1, l0, l1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

  handleDataAccess( mCurrentInstr, exp );
};


/*
  Inserts a LD.W instruction.

  Exact format: LD.W D[a] (def), [A[b] (use)] LO:label (DALBOA)
*/
void InstructionFactory::insertLD_W( const WIR::TC_DRegV &Da,
                                     const WIR::TC_ARegV &Ab,
                                     const WIR::WIR_Data &d,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLD_W(const TC_DRegV&, const TC_ARegV&, const WIR_Data&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::LD_W, TC13::OperationFormat::DALC16BOA,
        new WIR_RegisterParameter( Da, WIR_Usage::def ),
        new WIR_RegisterParameter( Ab, WIR_Usage::use ),
        new WIR_LabelParameter( d ), new TC_Const16_Signed( 0 ) } } );

  ADDDEBUGINFO( i, exp, type );
  handleDataAccess( i, exp );
};


void InstructionFactory::insertLDMST( const std::string &o0, LLIR_Register *r0,
                                      int c0, LLIR_Register *r1,
                                      const IR_Exp *exp, StmtType type )
{
  if ( ( c0 >= minSignedConst10Value ) && ( c0 <= maxSignedConst10Value ) ) {
    mCurrentInstr = insLDMST( o0, r0, c0, r1, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

    handleDataAccess( mCurrentInstr, exp );
  } else {
    if ( o0 == OPER_PREINC ) {
      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r0, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( r0, OPER_BASE, c0 );

      insertLDMST( OPER_BASE, r0, 0, r1, exp );
    } else

    if ( o0 == OPER_POSTINC ) {
      insertLDMST( OPER_BASE, r0, 0, r1, exp );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r0, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( r0, OPER_BASE, c0 );
    } else {
      LLIR_Register *reg1 = CreateRegister( "", true );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( reg1, o0, r0, c0 );
      else
        handleLargeOffset( reg1, OPER_BASE, c0, r0 );

      insertLDMST( o0, r0, 0, reg1, exp );
    }
  }
};


/*
  Inserts a LDMST instruction.

  Exact format: LDMST [A[b] (use)]off, Ea (use) (AC10EBOA)

  Handling of address offsets beyond signed 10 bits is included. Exact formats
  either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LDMST [A[x] (use)]<lower 10 bits of off>, Ea (use) (AC10EBOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
  LDMST [A[x] (use)]0, Ea (use) (AC10EBOA)
*/
void InstructionFactory::insertLDMST( const WIR::TC_ARegV &Ab, int off,
                                      const WIR::TC_ERegV &Ea,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLDMST(const TC_ARegV&, int, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LDMST, TC13::OperationFormat::AC10EBOA,
          new WIR_RegisterParameter( Ab, WIR_Usage::use ),
          new TC_Const10_Signed( off ),
          new WIR_RegisterParameter( Ea, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else {
    auto &Ax = createAReg();
    auto p = splitOffset( off );

    insertADDIH_A( Ax, Ab, p.first, exp, type );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
      insertLDMST( Ax, p.second, Ea, exp, type );
    else {
      insertLEA( Ax, Ax, p.second, exp, type );
      insertLDMST( Ax, 0, Ea, exp, type );
    }
  }
};


/*
  Inserts a LDMST instruction.

  Exact formats:

  LDMST [+A[b] (defuse)]off, E[a] (use) (AC10EPIA)
  LDMST [A[b] (defuse)+]off, E[a] (use) (AC10EPIA)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  LDMST [A[b] (use)]0, E[a] (use) (AC10EBOA)

  Exact formats for post-increment:

  LDMST [A[b] (use)]0, E[a] (use) (AC10EBOA)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LDMST [+A[b] (defuse)]<lower 10 bits of off>, E[a] (use) (AC10EPIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  LDMST [A[b] (use)]0, E[a] (use) (AC10EBOA)

  Exact formats for post-increment either:

  LDMST [A[b] (defuse)+]<lower 10 bits of off>, E[a] (use) (AC10EPIA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LDMST [A[b] (use)]0, E[a] (use) (AC10EBOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertLDMST( const WIR::TC13::AddressingMode &m,
                                      const WIR::TC_ARegV &Ab, int off,
                                      const WIR::TC_ERegV &Ea,
                                      const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LDMST, TC13::OperationFormat::AC10EPIA,
          new WIR_AddressingModeParameter( m ),
          new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
          new TC_Const10_Signed( off ),
          new WIR_RegisterParameter( Ea, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      insertLEA( Ab, Ab, off, exp, type );
      insertLDMST( Ab, 0, Ea, exp, type );
    } else {
      insertLDMST( Ab, 0, Ea, exp, type );
      insertLEA( Ab, Ab, off, exp, type );
    }
  } else {
    auto p = splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertADDIH_A( Ab, Ab, p.first, exp, type );
        insertLDMST( m, Ab, p.second, Ea, exp, type );
      } else {
        handleLargeOffset( Ab, Ab, off, exp, type );
        insertLDMST( Ab, 0, Ea, exp, type );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertLDMST( m, Ab, p.second, Ea, exp, type );
        insertADDIH_A( Ab, Ab, p.first, exp, type );
      } else {
        insertLDMST( Ab, 0, Ea, exp, type );
        handleLargeOffset( Ab, Ab, off, exp, type );
      }
    }
  }
};


void InstructionFactory::insertLEA( LLIR_Register *r0, long long c0,
                                    const IR_Exp *exp, StmtType type )
{
  bool isUnsignedConst18 =
    ( c0 >= 0 ) && ( ( c0 <= (int) TC_Const18_Unsigned::getMaxValue( 18 ) ) );

  // Extract bit positions 14:27 from c0.
  unsigned int bits1427 = ( c0 >> 14 ) & 0x3FFF;
  bool isOffset18 = isUnsignedConst18 && ( bits1427 == 0 );

  if ( isOffset18 ) {
    mCurrentInstr = insLEA( r0, c0, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
  } else {
    auto p = splitOffset( c0 );

    // Generate the operation (see TriCore Architecture Manual, page 5-19).
    insertMOVH_A( r0, p.first );
    insertLEA( r0, OPER_BASE, r0, p.second );
  }
};


/*
  Inserts a LEA instruction.

  Exact format: LEA A[a] (def), off (AC18ABSA)

  Handling of address offsets incompatible with TriCore's offset18 format is
  included. Exact formats:

  MOVH.A A[a] (def), <upper 16 bits of off> (AC16)
  LEA A[a] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertLEA( const WIR::TC_ARegV &Aa, int off,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLEA(const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  bool isUnsignedConst18 =
    ( off >= 0 ) && ( off <= (int) TC_Const18_Unsigned::getMaxValue( 18 ) );

  // Extract bit positions 14:27 from off.
  unsigned int bits1427 = ( off >> 14 ) & 0x3FFF;
  bool isOffset18 = isUnsignedConst18 && ( bits1427 == 0 );

  if ( isOffset18 ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::LEA, TC13::OperationFormat::AC18ABSA,
          new WIR_RegisterParameter( Aa, WIR_Usage::def ),
          new TC_Const18_Unsigned( off ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    auto p = splitOffset( off );

    // Generate the operation (see TriCore Architecture Manual, page 5-19).
    insertMOVH_A( Aa, p.first, exp, type );
    insertLEA( Aa, Aa, p.second, exp, type );
  }
};


void InstructionFactory::insertLEA( LLIR_Register *r0, const std::string &o0,
                                    LLIR_Register *r1, int c0,
                                    const IR_Exp *exp, StmtType type )
{
  string firstReg( r0->GetName() );
  string secondReg( r1->GetName() );

  // If both registers are equal and the offset is zero, the generated
  // instruction has no effect and can be skipped.
  if ( !( ( firstReg == secondReg ) && ( c0 == 0 ) ) ) {
    if ( ( c0 >= minSignedConst16Value ) &&
         ( c0 <= maxSignedConst16Value ) ) {
      mCurrentInstr = insLEA( r0, o0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

      // If exp defined, insert assembler debug information.
      if ( mConfig.getGenerateDebugFlag() && exp )
        addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
    } else {
      insertMOV_AA( r0, r1 );
      handleLargeOffset( r0, OPER_BASE, c0 );
    }
  }
};


/*
  Inserts a LEA instruction.

  Exact format: LEA A[a] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is included. Exact formats:

  MOV.AA A[a] (def), A[b] (use) (SAA_1)
  ADDIH.A A[a] (def), A[a] (use), <upper 16 bits of off> (AAC16)
  LEA A[a] (def), [A[a] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertLEA( const WIR::TC_ARegV &Aa,
                                    const WIR::TC_ARegV &Ab, int off,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLEA(const TC_ARegV&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  // If both registers are the same and the offset is 0, the LEA is omitted
  // since it has no effect.
  if ( ( Aa != Ab ) || ( off != 0 ) ) {
    if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
         ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
      auto &i = TC179x_wirBB->pushBackInstruction(
        { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
            new WIR_RegisterParameter( Aa, WIR_Usage::def ),
            new WIR_RegisterParameter( Ab, WIR_Usage::use ),
            new TC_Const16_Signed( off ) } } );

      ADDDEBUGINFO( i, exp, type );
    } else {
      insertMOV_AA( Aa, Ab, exp, type );
      handleLargeOffset( Aa, Aa, off, exp, type );
    }
  }
};


void InstructionFactory::insertLEA( LLIR_Register *r0, const std::string &o0,
                                    LLIR_Register *r1, const std::string &l0,
                                    const std::string &l1,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLEA( r0, o0, r1, l0, l1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a LEA instruction.

  Exact format: LEA A[a] (def), [A[b] (use)] LO:label (AALBOA)
*/
void InstructionFactory::insertLEA( const WIR::TC_ARegV &Aa,
                                    const WIR::TC_ARegV &Ab,
                                    const WIR::WIR_BasicBlock &b,
                                    const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::LEA, TC13::OperationFormat::AALC16BOA,
        new WIR_RegisterParameter( Aa, WIR_Usage::def ),
        new WIR_RegisterParameter( Ab, WIR_Usage::use ),
        new WIR_LabelParameter( b ), new TC_Const16_Signed( 0 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a LEA instruction.

  Exact format: LEA A[a] (def), [A[b] (use)] LO:label (AALBOA)
*/
void InstructionFactory::insertLEA( const WIR::TC_ARegV &Aa,
                                    const WIR::TC_ARegV &Ab,
                                    const WIR::WIR_Data &d,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLEA(const TC_ARegV&, const TC_ARegV&, const WIR_Data&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::LEA, TC13::OperationFormat::AALC16BOA,
        new WIR_RegisterParameter( Aa, WIR_Usage::def ),
        new WIR_RegisterParameter( Ab, WIR_Usage::use ),
        new WIR_LabelParameter( d ), new TC_Const16_Signed( 0 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a LEA instruction.

  Exact format: LEA A[a] (def), [A[b] (use)] LO:label (AALBOA)
*/
void InstructionFactory::insertLEA( const WIR::TC_ARegV &Aa,
                                    const WIR::TC_ARegV &Ab,
                                    const WIR::WIR_Function &f,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLEA(const TC_ARegV&, const TC_ARegV&, const WIR_Function&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::LEA, TC13::OperationFormat::AALC16BOA,
        new WIR_RegisterParameter( Aa, WIR_Usage::def ),
        new WIR_RegisterParameter( Ab, WIR_Usage::use ),
        new WIR_LabelParameter( f ), new TC_Const16_Signed( 0 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertLOOP( LLIR_Register *r0, const std::string &l0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLOOP( r0, l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a LOOP instruction.

  Exact format: LOOP A[b] (defuse), disp15 (AL_3)
*/
void InstructionFactory::insertLOOP( const WIR::TC_ARegV &Ab,
                                     const WIR::WIR_BasicBlock &disp15,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLOOP(const TC_ARegV&, const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::LOOP, TC13::OperationFormat::AL_3,
        new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertLOOPU( const std::string &l0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLOOPU( l0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a LOOPU instruction.

  Exact format: LOOPU disp15 (L)
*/
void InstructionFactory::insertLOOPU( const WIR::WIR_BasicBlock &disp15,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLOOPU(const WIR_BasicBlock&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::LOOPU, TC13::OperationFormat::L,
        new WIR_LabelParameter( disp15 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertLT( LLIR_Register *r0, LLIR_Register *r1, int c0,
                                   const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLT( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a LT instruction.

  Exact format: LT D[c] (def), D[a] (use), const9 (DDC9_1)
*/
void InstructionFactory::insertLT( const WIR::TC_DRegV &Dc,
                                   const WIR::TC_DRegV &Da, int const9,
                                   const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLT(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::LT, TC13::OperationFormat::DDC9_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertLT( LLIR_Register *r0, LLIR_Register *r1,
                                   LLIR_Register *r2,
                                   const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLT( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a LT instruction.

  Exact format: LT D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertLT( const WIR::TC_DRegV &Dc,
                                   const WIR::TC_DRegV &Da,
                                   const WIR::TC_DRegV &Db,
                                   const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLT(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::LT, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertLT_A( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLT_A( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a LT.A instruction.

  Exact format: LT.A D[c] (def), A[a] (use), A[b] (use) (DAA)
*/
void InstructionFactory::insertLT_A( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_ARegV &Aa,
                                     const WIR::TC_ARegV &Ab,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLT_A(const TC_DRegV&, const TC_ARegV&, const TC_ARegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::LT_A, TC13::OperationFormat::DAA,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Aa, WIR_Usage::use ),
        new WIR_RegisterParameter( Ab, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertLT_U( LLIR_Register *r0, LLIR_Register *r1,
                                     int c0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLT_U( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a LT.U instruction.

  Exact format: LT.U D[c] (def), D[a] (use), const9 (DDC9_2)
*/
void InstructionFactory::insertLT_U( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     unsigned int const9,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLT_U(const TC_DRegV&, const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::LT_U, TC13::OperationFormat::DDC9_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Unsigned( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertLT_U( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLT_U( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a LT.U instruction.

  Exact format: LT.U D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertLT_U( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLT_U(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::LT_U, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMADD( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2, int c0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMADD( r0, r1, r2, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MADD instruction.

  Exact format: MADD D[c] (def), D[d] (use), D[a] (use), const9 (DDDC9_1)
*/
void InstructionFactory::insertMADD( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Dd,
                                     const WIR::TC_DRegV &Da, int const9,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMADD(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MADD, TC13::OperationFormat::DDDC9_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Dd, WIR_Usage::use ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMADD( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2, LLIR_Register *r3,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMADD( r0, r1, r2, r3, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MADD instruction.

  Exact format: MADD D[c] (def), D[d] (use), D[a] (use), D[b] (use) (DDDD)
*/
void InstructionFactory::insertMADD( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Dd,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMADD(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MADD, TC13::OperationFormat::DDDD,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Dd, WIR_Usage::use ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMADD_U( LLIR_Register *r0, LLIR_Register *r1,
                                       LLIR_Register *r2, int c0,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMADD_U( r0, r1, r2, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MADD.U instruction.

  Exact format: MADD.U E[c] (def), E[d] (use), D[a] (use), const9 (EEDC9_2)
*/
void InstructionFactory::insertMADD_U( const WIR::TC_ERegV &Ec,
                                       const WIR::TC_ERegV &Ed,
                                       const WIR::TC_DRegV &Da,
                                       unsigned int const9,
                                       const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MADD_U, TC13::OperationFormat::EEDC9_2,
        new WIR_RegisterParameter( Ec, WIR_Usage::def ),
        new WIR_RegisterParameter( Ed, WIR_Usage::use ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Unsigned( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMADD_U( LLIR_Register *r0, LLIR_Register *r1,
                                       LLIR_Register *r2, LLIR_Register *r3,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMADD_U( r0, r1, r2, r3, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MADD.U instruction.

  Exact format: MADD.U E[c] (def), E[d] (use), D[a] (use), D[b] (use) (EEDD)
*/
void InstructionFactory::insertMADD_U( const WIR::TC_ERegV &Ec,
                                       const WIR::TC_ERegV &Ed,
                                       const WIR::TC_DRegV &Da,
                                       const WIR::TC_DRegV &Db,
                                       const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MADD_U, TC13::OperationFormat::EEDD,
        new WIR_RegisterParameter( Ec, WIR_Usage::def ),
        new WIR_RegisterParameter( Ed, WIR_Usage::use ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMAX( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMAX( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MAX instruction.

  Exact format: MAX D[c] (def), D[a] (use), const9 (DDC9_1)
*/
void InstructionFactory::insertMAX( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da, int const9,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMAX(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MAX, TC13::OperationFormat::DDC9_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMAX( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMAX( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MAX instruction.

  Exact format: MAX D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertMAX( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da,
                                    const WIR::TC_DRegV &Db,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMAX(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MAX, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMAX_U( LLIR_Register *r0, LLIR_Register *r1,
                                      int c0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMAX_U( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MAX.U instruction.

  Exact format: MAX.U D[c] (def), D[a] (use), const9 (DDC9_2)
*/
void InstructionFactory::insertMAX_U( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      unsigned int const9,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMAX_U(const TC_DRegV&, const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MAX_U, TC13::OperationFormat::DDC9_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Unsigned( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMAX_U( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMAX_U( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MAX.U instruction.

  Exact format: MAX.U D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertMAX_U( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMAX_U(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MAX_U, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMIN( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMIN( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MIN instruction.

  Exact format: MIN D[c] (def), D[a] (use), const9 (DDC9_1)
*/
void InstructionFactory::insertMIN( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da, int const9,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMIN(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MIN, TC13::OperationFormat::DDC9_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMIN( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMIN( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MIN instruction.

  Exact format: MIN D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertMIN( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da,
                                    const WIR::TC_DRegV &Db,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMIN(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MIN, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMIN_U( LLIR_Register *r0, LLIR_Register *r1,
                                      int c0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMIN_U( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MIN.U instruction.

  Exact format: MIN.U D[c] (def), D[a] (use), const9 (DDC9_2)
*/
void InstructionFactory::insertMIN_U( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      unsigned int const9,
                                      const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MIN_U, TC13::OperationFormat::DDC9_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Unsigned( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMIN_U( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMIN_U( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MIN.U instruction.

  Exact format: MIN.U D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertMIN_U( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMIN_U(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MIN_U, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMOD_B( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2, bool isUnsigned,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *e0 = CreateERegister( "" );

  // Generate initializing DVINIT instruction.
  if ( isUnsigned )
    mCurrentInstr = insDVINIT_BU( e0, r0, r1, LAST_LLIR_BB, mCurrentInstr );
  else
    mCurrentInstr = insDVINIT_B( e0, r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // Generate DVSTEP instruction.
  if ( isUnsigned )
    mCurrentInstr = insDVSTEP_U( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );
  else
    mCurrentInstr = insDVSTEP( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );

  // Generate finalizing DVADJ instruction.
  if ( !isUnsigned )
    mCurrentInstr = insDVADJ( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

  // MOV result to target register.
  mCurrentInstr =
    insMOV(
      r2, e0->GetNextChild( e0->GetFirstChild() ), LAST_LLIR_BB,
      mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a series of instructions for signed/unsigned byte modulo.

  Exact formats:

  DVINIT.B E[x], D[a], D[b] (EDD)
  DVSTEP E[x], E[x], D[b] (EED)
  DVADJ E[x], E[x], D[b] (EED)
  MOV D[c], Odd register of E[x] (MOV_RR SDD_1)

  resp.

  DVINIT.BU E[x], D[a], D[b] (EDD)
  DVSTEP.U E[x], E[x], D[b] (EED)
  MOV D[c], Odd register of E[x] (MOV_RR SDD_1)
*/
void InstructionFactory::insertMOD_B( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db, bool u,
                                      const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &Ex = createEReg();

  // Generate initializing DVINIT.B[U] instruction.
  auto &i1 = TC179x_wirBB->pushBackInstruction(
    { { ( u ? TC13::OpCode::DVINIT_BU : TC13::OpCode::DVINIT_B ),
        TC13::OperationFormat::EDD,
        new WIR_RegisterParameter( Ex, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
  ADDDEBUGINFO( i1, exp, type );

  // Generate DVSTEP instruction.
  auto &i2 = TC179x_wirBB->pushBackInstruction(
    { { ( u ? TC13::OpCode::DVSTEP_U : TC13::OpCode::DVSTEP ),
        TC13::OperationFormat::EED,
        new WIR_RegisterParameter( Ex, WIR_Usage::def ),
        new WIR_RegisterParameter( Ex, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
  ADDDEBUGINFO( i2, exp, type );

  // Generate finalizing DVADJ instruction.
  if ( !u ) {
    auto &i3 = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::DVADJ, TC13::OperationFormat::EED,
          new WIR_RegisterParameter( Ex, WIR_Usage::def ),
          new WIR_RegisterParameter( Ex, WIR_Usage::use ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
    ADDDEBUGINFO( i3, exp, type );
  }

  // Move result to target register.
  auto &i4 = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MOV_RR,
        m16BitOperations ?
          TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter(
          dynamic_cast<WIR_VirtualRegister &>( Ex ).rbegin()->get(),
          WIR_Usage::use ) } } );
  ADDDEBUGINFO( i4, exp, type );
};


void InstructionFactory::insertMOD_H( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2, bool isUnsigned,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *e0 = CreateERegister( "" );

  // Generate initializing DVINIT instruction.
  if ( isUnsigned )
    mCurrentInstr = insDVINIT_HU( e0, r0, r1, LAST_LLIR_BB, mCurrentInstr );
  else
    mCurrentInstr = insDVINIT_H( e0, r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // Generate 2 DVSTEP instructions.
  for ( int i = 0; i < 2; ++i )
    if ( isUnsigned )
      mCurrentInstr = insDVSTEP_U( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );
    else
      mCurrentInstr = insDVSTEP( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );

  // Generate finalizing DVADJ instruction.
  if ( !isUnsigned )
    mCurrentInstr = insDVADJ( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

  // MOV result to target register.
  mCurrentInstr =
    insMOV(
      r2, e0->GetNextChild( e0->GetFirstChild() ), LAST_LLIR_BB,
      mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a series of instructions for signed/unsigned halfword modulo.

  Exact formats:

  DVINIT.H E[x], D[a], D[b] (EDD)
  DVSTEP E[x], E[x], D[b] (EED)
  DVSTEP E[x], E[x], D[b] (EED)
  DVADJ E[x], E[x], D[b] (EED)
  MOV D[c], Odd register of E[x] (MOV_RR SDD_1)

  resp.

  DVINIT.HU E[x], D[a], D[b] (EDD)
  DVSTEP.U E[x], E[x], D[b] (EED)
  DVSTEP.U E[x], E[x], D[b] (EED)
  MOV D[c], Odd register of E[x] (MOV_RR SDD_1)
*/
void InstructionFactory::insertMOD_H( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db, bool u,
                                      const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &Ex = createEReg();

  // Generate initializing DVINIT.H[U] instruction.
  auto &i1 = TC179x_wirBB->pushBackInstruction(
    { { ( u ? TC13::OpCode::DVINIT_HU : TC13::OpCode::DVINIT_H ),
        TC13::OperationFormat::EDD,
        new WIR_RegisterParameter( Ex, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
  ADDDEBUGINFO( i1, exp, type );

  // Generate two DVSTEP instructions.
  for ( int i = 0; i < 2; ++i ) {
    auto &i2 = TC179x_wirBB->pushBackInstruction(
      { { ( u ? TC13::OpCode::DVSTEP_U : TC13::OpCode::DVSTEP ),
          TC13::OperationFormat::EED,
          new WIR_RegisterParameter( Ex, WIR_Usage::def ),
          new WIR_RegisterParameter( Ex, WIR_Usage::use ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
    ADDDEBUGINFO( i2, exp, type );
  }

  // Generate finalizing DVADJ instruction.
  if ( !u ) {
    auto &i3 = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::DVADJ, TC13::OperationFormat::EED,
          new WIR_RegisterParameter( Ex, WIR_Usage::def ),
          new WIR_RegisterParameter( Ex, WIR_Usage::use ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
    ADDDEBUGINFO( i3, exp, type );
  }

  // Move result to target register.
  auto &i4 =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::MOV_RR,
          m16BitOperations ?
            TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter(
            dynamic_cast<WIR_VirtualRegister &>( Ex ).rbegin()->get(),
            WIR_Usage::use ) } } );
  ADDDEBUGINFO( i4, exp, type );
};


void InstructionFactory::insertMOD_W( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2, bool isUnsigned,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *e0 = CreateERegister( "" );

  // Generate initializing DVINIT instruction.
  if ( isUnsigned )
    mCurrentInstr = insDVINIT_U( e0, r0, r1, LAST_LLIR_BB, mCurrentInstr );
  else
    mCurrentInstr = insDVINIT( e0, r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // Generate 4 DVSTEP instructions.
  for ( int i = 0; i < 4; ++i )
    if ( isUnsigned )
      mCurrentInstr = insDVSTEP_U( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );
    else
      mCurrentInstr = insDVSTEP( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );

  // Generate finalizing DVADJ instruction.
  if ( !isUnsigned )
    mCurrentInstr = insDVADJ( e0, e0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

  // MOV result to target register.
  mCurrentInstr =
    insMOV(
      r2, e0->GetNextChild( e0->GetFirstChild() ), LAST_LLIR_BB,
      mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a series of instructions for signed/unsigned integer modulo.

  Exact formats:

  DVINIT E[x], D[a], D[b] (EDD)
  DVSTEP E[x], E[x], D[b] (EED)
  DVSTEP E[x], E[x], D[b] (EED)
  DVSTEP E[x], E[x], D[b] (EED)
  DVSTEP E[x], E[x], D[b] (EED)
  DVADJ E[x], E[x], D[b] (EED)
  MOV D[c], Odd register of E[x] (MOV_RR SDD_1)

  resp.

  DVINIT.U E[x], D[a], D[b] (EDD)
  DVSTEP.U E[x], E[x], D[b] (EED)
  DVSTEP.U E[x], E[x], D[b] (EED)
  DVSTEP.U E[x], E[x], D[b] (EED)
  DVSTEP.U E[x], E[x], D[b] (EED)
  MOV D[c], Odd register of E[x] (MOV_RR SDD_1)
*/
void InstructionFactory::insertMOD_W( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db, bool u,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOD_W(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, bool, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &Ex = createEReg();

  // Generate initializing DVINIT[.U] instruction.
  auto &i1 = TC179x_wirBB->pushBackInstruction(
    { { ( u ? TC13::OpCode::DVINIT_U : TC13::OpCode::DVINIT ),
        TC13::OperationFormat::EDD,
        new WIR_RegisterParameter( Ex, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
  ADDDEBUGINFO( i1, exp, type );

  // Generate four DVSTEP instructions.
  for ( int i = 0; i < 4; ++i ) {
    auto &i2 = TC179x_wirBB->pushBackInstruction(
      { { ( u ? TC13::OpCode::DVSTEP_U : TC13::OpCode::DVSTEP ),
          TC13::OperationFormat::EED,
          new WIR_RegisterParameter( Ex, WIR_Usage::def ),
          new WIR_RegisterParameter( Ex, WIR_Usage::use ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
    ADDDEBUGINFO( i2, exp, type );
  }

  // Generate finalizing DVADJ instruction.
  if ( !u ) {
    auto &i3 = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::DVADJ, TC13::OperationFormat::EED,
          new WIR_RegisterParameter( Ex, WIR_Usage::def ),
          new WIR_RegisterParameter( Ex, WIR_Usage::use ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
    ADDDEBUGINFO( i3, exp, type );
  }

  // Move result to target register.
  auto &i4 =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::MOV_RR,
          m16BitOperations ?
            TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter(
            dynamic_cast<WIR_VirtualRegister &>( Ex ).rbegin()->get(),
            WIR_Usage::use ) } } );
  ADDDEBUGINFO( i4, exp, type );
};


void InstructionFactory::insertMOV( LLIR_Register *r0, int c0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMOV( r0, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MOV instruction.

  Exact formats either:

  MOV D[c] (def), const16 (DC16_1)

  or:

  MOV D[c] (def), const4 (SDC4_1)
*/
void InstructionFactory::insertMOV( const WIR::TC_DRegV &Dc, int c,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOV(const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( c >= TC_Const4_Signed::getMinValue( 4 ) ) &&
       ( c <= TC_Const4_Signed::getMaxValue( 4 ) ) && m16BitOperations ) {
    auto &i =
      TC179x_wirBB->pushBackInstruction(
        { { TC13::OpCode::MOV, TC13::OperationFormat::SDC4_1,
            new WIR_RegisterParameter( Dc, WIR_Usage::def ),
            new TC_Const4_Signed( c ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    auto &i =
      TC179x_wirBB->pushBackInstruction(
        { { TC13::OpCode::MOV, TC13::OperationFormat::DC16_1,
            new WIR_RegisterParameter( Dc, WIR_Usage::def ),
            new TC_Const16_Signed( c ) } } );

    ADDDEBUGINFO( i, exp, type );
  }
};


void InstructionFactory::insertMOV( LLIR_Register *r0, LLIR_Register *r1,
                                    const IR_Exp *exp, StmtType type )
{
  if ( isEReg( r0 ) && isEReg( r1 ) ) {
    mCurrentInstr = insMOV(
      r0->GetFirstChild(),
      r1->GetFirstChild(),
      LAST_LLIR_BB, mCurrentInstr );
    mCurrentInstr = insMOV(
      r0->GetNextChild( r0->GetFirstChild() ),
      r1->GetNextChild( r1->GetFirstChild() ),
      LAST_LLIR_BB, mCurrentInstr );
  } else
    mCurrentInstr = insMOV( r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MOV instruction.

  Exact format: MOV D[c] (def), D[b] (use) (MOV_RR SDD_1)
*/
void InstructionFactory::insertMOV( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Db,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOV(const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::MOV_RR,
          m16BitOperations ?
            TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );
  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a MOV instruction.

  Exact formats:

  MOV <even register of E[c] (def)>, <even register of E[b] (use)> (MOV_RR SDD_1)
  MOV <odd register of E[c] (def)>, <odd register of E[b] (use)> (MOV_RR SDD_1)
*/
void InstructionFactory::insertMOV( const WIR::TC_ERegV &Ec,
                                    const WIR::TC_ERegV &Eb,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOV(const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::MOV_RR,
          m16BitOperations ?
            TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
          new WIR_RegisterParameter(
            dynamic_cast<const WIR_VirtualRegister &>( Ec ).begin()->get(),
            WIR_Usage::def ),
          new WIR_RegisterParameter(
            dynamic_cast<const WIR_VirtualRegister &>( Eb ).begin()->get(),
            WIR_Usage::use ) } } );
  ADDDEBUGINFO( i, exp, type );

  auto &j =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::MOV_RR,
          m16BitOperations ?
            TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
          new WIR_RegisterParameter(
            dynamic_cast<const WIR_VirtualRegister &>( Ec ).rbegin()->get(),
            WIR_Usage::def ),
          new WIR_RegisterParameter(
            dynamic_cast<const WIR_VirtualRegister &>( Eb ).rbegin()->get(),
            WIR_Usage::use ) } } );
  ADDDEBUGINFO( j, exp, type );
};


void InstructionFactory::insertMOV_A( LLIR_Register *r0, LLIR_Register *r1,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMOV_A( r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MOV.A instruction.

  Exact format: MOV.A A[c] (def), D[b] (use) (SAD_1)
*/
void InstructionFactory::insertMOV_A( const WIR::TC_ARegV &Ac,
                                      const WIR::TC_DRegV &Db,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOV_A(const TC_ARegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::MOV_A,
          m16BitOperations ?
            TC13::OperationFormat::SAD_1 : TC13::OperationFormat::AD,
          new WIR_RegisterParameter( Ac, WIR_Usage::def ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMOV_AA( LLIR_Register *r0, LLIR_Register *r1,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMOV_AA( r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MOV.AA instruction.

  Exact format: MOV.AA A[a] (def), A[b] (use) (SAA_1)
*/
void InstructionFactory::insertMOV_AA( const WIR::TC_ARegV &Aa,
                                       const WIR::TC_ARegV &Ab,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOV_AA(const TC_ARegV&, const TC_ARegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::MOV_AA,
          m16BitOperations ?
            TC13::OperationFormat::SAA_1 : TC13::OperationFormat::AA,
          new WIR_RegisterParameter( Aa, WIR_Usage::def ),
          new WIR_RegisterParameter( Ab, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMOV_D( LLIR_Register *r0, LLIR_Register *r1,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMOV_D( r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MOV.D instruction.

  Exact format: MOV.D D[a] (def), A[b] (use) (SDA_1)
*/
void InstructionFactory::insertMOV_D( const WIR::TC_DRegV &Da,
                                      const WIR::TC_ARegV &Ab,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOV_D(const TC_DRegV&, const TC_ARegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::MOV_D,
          m16BitOperations ?
            TC13::OperationFormat::SDA_1 : TC13::OperationFormat::DA,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_RegisterParameter( Ab, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMOV_U( LLIR_Register *r0, int c0,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMOV_U( r0, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MOV.U instruction.

  Exact format: MOV.U D[c] (def), const16 (DC16_2)
*/
void InstructionFactory::insertMOV_U( const WIR::TC_DRegV &Dc,
                                      unsigned int const16,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOV_U(const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MOV_U, TC13::OperationFormat::DC16_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new TC_Const16_Unsigned( const16 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMOVH( LLIR_Register *r0, int c0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMOVH( r0, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MOVH instruction.

  Exact format: MOVH D[c] (def), const16 (DC16_2)
*/
void InstructionFactory::insertMOVH( const WIR::TC_DRegV &Dc,
                                     unsigned int const16,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOVH(const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MOVH, TC13::OperationFormat::DC16_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new TC_Const16_Unsigned( const16 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMOVH_A( LLIR_Register *r0, int c0,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMOVH_A( r0, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MOVH.A instruction.

  Exact format: MOVH.A A[c] (def), const16 (AC16)
*/
void InstructionFactory::insertMOVH_A( const WIR::TC_ARegV &Ac,
                                       unsigned int const16,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOVH_A(const TC_ARegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MOVH_A, TC13::OperationFormat::AC16,
        new WIR_RegisterParameter( Ac, WIR_Usage::def ),
        new TC_Const16_Unsigned( const16 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMOVH_A( LLIR_Register *r0, const std::string &l0,
                                       const std::string &l1,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMOVH_A( r0, l0, l1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MOVH.A instruction.

  Exact format: MOVH.A A[c] (def), HI:label (AL_1)
*/
void InstructionFactory::insertMOVH_A( const WIR::TC_ARegV &Ac,
                                       const WIR::WIR_BasicBlock &b,
                                       const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MOVH_A, TC13::OperationFormat::AL_1,
        new WIR_RegisterParameter( Ac, WIR_Usage::def ),
        new WIR_LabelParameter( b ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a MOVH.A instruction.

  Exact format: MOVH.A A[c] (def), HI:label (AL_1)
*/
void InstructionFactory::insertMOVH_A( const WIR::TC_ARegV &Ac,
                                       const WIR::WIR_Data &d,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOVH_A(const TC_ARegV&, const WIR_Data&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MOVH_A, TC13::OperationFormat::AL_1,
        new WIR_RegisterParameter( Ac, WIR_Usage::def ),
        new WIR_LabelParameter( d ) } } );

  ADDDEBUGINFO( i, exp, type );
};


/*
  Inserts a MOVH.A instruction.

  Exact format: MOVH.A A[c] (def), HI:label (AL_1)
*/
void InstructionFactory::insertMOVH_A( const WIR::TC_ARegV &Ac,
                                       const WIR::WIR_Function &f,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOVH_A(const TC_ARegV&, const WIR_Function&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MOVH_A, TC13::OperationFormat::AL_1,
        new WIR_RegisterParameter( Ac, WIR_Usage::def ),
        new WIR_LabelParameter( f ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMSUB( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2, int c0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMSUB( r0, r1, r2, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MSUB instruction.

  Exact format: MSUB D[c] (def), D[d] (use), D[a] (use), const9 (DDDC9_1)
*/
void InstructionFactory::insertMSUB( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Dd,
                                     const WIR::TC_DRegV &Da, int const9,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMSUB(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MSUB, TC13::OperationFormat::DDDC9_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Dd, WIR_Usage::use ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMSUB( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2, LLIR_Register *r3,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMSUB( r0, r1, r2, r3, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MSUB instruction.

  Exact format: MSUB D[c] (def), D[d] (use), D[a] (use), D[b] (use) (DDDD)
*/
void InstructionFactory::insertMSUB( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Dd,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMSUB(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MSUB, TC13::OperationFormat::DDDD,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Dd, WIR_Usage::use ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMSUB_U( LLIR_Register *r0, LLIR_Register *r1,
                                       LLIR_Register *r2, int c0,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMSUB_U( r0, r1, r2, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MSUB.U instruction.

  Exact format: MSUB.U E[c] (def), E[d] (use), D[a] (use), const9 (EEDC9_2)
*/
void InstructionFactory::insertMSUB_U( const WIR::TC_ERegV &Ec,
                                       const WIR::TC_ERegV &Ed,
                                       const WIR::TC_DRegV &Da,
                                       unsigned int const9,
                                       const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MSUB_U, TC13::OperationFormat::EEDC9_2,
        new WIR_RegisterParameter( Ec, WIR_Usage::def ),
        new WIR_RegisterParameter( Ed, WIR_Usage::use ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Unsigned( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMSUB_U( LLIR_Register *r0, LLIR_Register *r1,
                                       LLIR_Register *r2, LLIR_Register *r3,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMSUB_U( r0, r1, r2, r3, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MSUB.U instruction.

  Exact format: MSUB.U E[c] (def), E[d] (use), D[a] (use), D[b] (use) (EEDD)
*/
void InstructionFactory::insertMSUB_U( const WIR::TC_ERegV &Ec,
                                       const WIR::TC_ERegV &Ed,
                                       const WIR::TC_DRegV &Da,
                                       const WIR::TC_DRegV &Db,
                                       const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MSUB_U, TC13::OperationFormat::EEDD,
        new WIR_RegisterParameter( Ec, WIR_Usage::def ),
        new WIR_RegisterParameter( Ed, WIR_Usage::use ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMUL( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMUL( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MUL instruction.

  Exact format: MUL D[c] (def), D[a] (use), const9 (DDC9_1)
*/
void InstructionFactory::insertMUL( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da,
                                    int const9,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMUL(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MUL, TC13::OperationFormat::DDC9_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMUL( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMUL( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MUL instruction.

  Exact format: MUL D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertMUL( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da,
                                    const WIR::TC_DRegV &Db,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMUL(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MUL, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMUL( LLIR_Register *r0, LLIR_Register *r1,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMUL( r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MUL instruction.

  Exact format: MUL D[a] (defuse), D[b] (use) (SDD_2)
*/
void InstructionFactory::insertMUL( const WIR::TC_DRegV &Da,
                                    const WIR::TC_DRegV &Db,
                                    const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { TC13::OpCode::MUL, TC13::OperationFormat::SDD_2,
          new WIR_RegisterParameter( Da, WIR_Usage::defuse ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } :
        WIR_Operation { TC13::OpCode::MUL, TC13::OperationFormat::DDD_1,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMUL_U( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMUL_U( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MUL.U instruction.

  Exact format: MUL.U E[c] (def), D[a] (use), D[b] (use) (EDD)
*/
void InstructionFactory::insertMUL_U( const WIR::TC_ERegV &Ec,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMUL_U(const TC_ERegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MUL_U, TC13::OperationFormat::EDD,
        new WIR_RegisterParameter( Ec, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertNE( LLIR_Register *r0, LLIR_Register *r1, int c0,
                                   const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insNE( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a NE instruction.

  Exact format: NE D[c] (def), D[a] (use), const9 (DDC9_1)
*/
void InstructionFactory::insertNE( const WIR::TC_DRegV &Dc,
                                   const WIR::TC_DRegV &Da, int const9,
                                   const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertNE(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::NE, TC13::OperationFormat::DDC9_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertNE( LLIR_Register *r0, LLIR_Register *r1,
                                   LLIR_Register *r2,
                                   const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insNE( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a NE instruction.

  Exact format: NE D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertNE( const WIR::TC_DRegV &Dc,
                                   const WIR::TC_DRegV &Da,
                                   const WIR::TC_DRegV &Db,
                                   const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertNE(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::NE, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertNE_A( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insNE_A( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a NE.A instruction.

  Exact format: NE.A D[c] (def), A[a] (use), A[b] (use) (DAA)
*/
void InstructionFactory::insertNE_A( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_ARegV &Aa,
                                     const WIR::TC_ARegV &Ab,
                                     const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::NE_A, TC13::OperationFormat::DAA,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Aa, WIR_Usage::use ),
        new WIR_RegisterParameter( Ab, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertNEZ_A( LLIR_Register *r0, LLIR_Register *r1,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insNEZ_A( r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a NEZ.A instruction.

  Exact format: NEZ.A D[c] (def), A[a] (use) (DA)
*/
void InstructionFactory::insertNEZ_A( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_ARegV &Aa,
                                      const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::NEZ_A, TC13::OperationFormat::DA,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Aa, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertNOR( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insNOR( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a NOR instruction.

  Exact format: NOR D[c] (def), D[a] (use), const9 (DDC9_2)
*/
void InstructionFactory::insertNOR( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da,
                                    unsigned int const9,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertNOR(const TC_DRegV&, const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::NOR, TC13::OperationFormat::DDC9_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Unsigned( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertOR( LLIR_Register *r0, LLIR_Register *r1, int c0,
                                   const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insOR( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an OR instruction.

  Exact format: OR D[c] (def), D[a] (use), const9 (DDC9_2)
*/
void InstructionFactory::insertOR( const WIR::TC_DRegV &Dc,
                                   const WIR::TC_DRegV &Da,
                                   unsigned int const9,
                                   const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertOR(const TC_DRegV&, const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::OR, TC13::OperationFormat::DDC9_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Unsigned( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertOR( LLIR_Register *r0, LLIR_Register *r1,
                                   LLIR_Register *r2,
                                   const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insOR( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an OR instruction.

  Exact format: OR D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertOR( const WIR::TC_DRegV &Dc,
                                   const WIR::TC_DRegV &Da,
                                   const WIR::TC_DRegV &Db,
                                   const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertOR(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::OR, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertOR_EQ( LLIR_Register *r0, LLIR_Register *r1,
                                      int p1,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insOR_EQ( r0, r1, p1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an OR.EQ instruction.

  Exact format: OR.EQ D[c] (defuse), D[a] (use), const9 (DDC9_3)
*/
void InstructionFactory::insertOR_EQ( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da, int const9,
                                      const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::OR_EQ, TC13::OperationFormat::DDC9_3,
        new WIR_RegisterParameter( Dc, WIR_Usage::defuse ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertOR_LT( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insOR_LT( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an OR.LT instruction.

  Exact format: OR.LT D[c] (defuse), D[a] (use), D[b] (use) (DDD_2)
*/
void InstructionFactory::insertOR_LT( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertOR_LT(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::OR_LT, TC13::OperationFormat::DDD_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::defuse ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertOR_LT_U( LLIR_Register *r0, LLIR_Register *r1,
                                        LLIR_Register *r2,
                                        const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insOR_LT_U( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an OR.LT.U instruction.

  Exact format: OR.LT.U D[c] (defuse), D[a] (use), D[b] (use) (DDD_2)
*/
void InstructionFactory::insertOR_LT_U( const WIR::TC_DRegV &Dc,
                                        const WIR::TC_DRegV &Da,
                                        const WIR::TC_DRegV &Db,
                                        const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertOR_LT_U(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::OR_LT_U, TC13::OperationFormat::DDD_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::defuse ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertOR_NE( LLIR_Register *r0, LLIR_Register *r1,
                                      int p1,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insOR_NE( r0, r1, p1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an OR.NE instruction.

  Exact format: OR.NE D[c] (defuse), D[a] (use), const9 (DDC9_3)
*/
void InstructionFactory::insertOR_NE( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da, int const9,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertOR_NE(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::OR_NE, TC13::OperationFormat::DDC9_3,
        new WIR_RegisterParameter( Dc, WIR_Usage::defuse ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertOR_NE( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insOR_NE( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an OR.NE instruction.

  Exact format: OR.NE D[c] (defuse), D[a] (use), D[b] (use) (DDD_2)
*/
void InstructionFactory::insertOR_NE( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertOR_NE(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::OR_NE, TC13::OperationFormat::DDD_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::defuse ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertOR_T( LLIR_Register *r0, LLIR_Register *r1,
                                     int p1, LLIR_Register *r2, int p2,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insOR_T( r0, r1, p1, r2, p2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an OR.T instruction.

  Exact format: OR.T D[c] (def), D[a] (use), p1, D[b] (use), p2 (DDC5DC5_1)
*/
void InstructionFactory::insertOR_T( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     unsigned int p1,
                                     const WIR::TC_DRegV &Db,
                                     unsigned int p2,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertOR_T(const TC_DRegV&, const TC_DRegV&, unsigned int, const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::OR_T, TC13::OperationFormat::DDC5DC5_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const5_Unsigned( p1 ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new TC_Const5_Unsigned( p2 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertORN( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insORN( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an ORN instruction.

  Exact format: ORN D[c] (def), D[a] (use), const9 (DDC9_2)
*/
void InstructionFactory::insertORN( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da,
                                    unsigned int const9,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertORN(const TC_DRegV&, const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::ORN, TC13::OperationFormat::DDC9_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Unsigned( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertRET( const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insRET( LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a RET instruction.

  Exact format: RET PSW.C (def) (SPSW)
*/
void InstructionFactory::insertRETURN( const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertRETURN(const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::RET,
          m16BitOperations ?
            TC13::OperationFormat::SPSW : TC13::OperationFormat::PSW,
          new
            WIR_RegisterParameter(
              TC179x_wirProc->PSW_C(), WIR_Usage::def ) } } );

  // Add implicit parameters for proper def-use analysis.
  i.begin()->get().pushBackParameter(
    new WIR_RegisterParameter(
      TC179x_wirProc->A11(), WIR_Usage::use, true ) );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertRET( LLIR_Register *r0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insRET( r0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a RET instruction.

  Exact format: RET PSW.C (def) (SPSW)
*/
void InstructionFactory::insertRET( const WIR::WIR_BaseRegister &arg,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertRET(const WIR_BaseRegister&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::RET,
          m16BitOperations ?
            TC13::OperationFormat::SPSW : TC13::OperationFormat::PSW,
          new
            WIR_RegisterParameter(
              TC179x_wirProc->PSW_C(), WIR_Usage::def ) } } );

  // Add implicit parameters for proper def-use analysis.
  WIR_Operation &retOp = i.begin()->get();
  retOp.pushBackParameter(
    new WIR_RegisterParameter( arg, WIR_Usage::use, true ) );
  retOp.pushBackParameter(
    new WIR_RegisterParameter(
      TC179x_wirProc->A11(), WIR_Usage::use, true ) );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertRSUB( LLIR_Register *r0, LLIR_Register *r1,
                                     int c0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insRSUB( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a RSUB instruction.

  Exact format: RSUB D[c] (def), D[a] (use), const9 (DDC9_1)
*/
void InstructionFactory::insertRSUB( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da, int const9,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertRSUB(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::RSUB, TC13::OperationFormat::DDC9_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertRSUB( LLIR_Register *r0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insRSUB( r0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a RSUB instruction.

  Exact format: RSUB D[a] (defuse) (SD)
*/
void InstructionFactory::insertRSUB( const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i =
    TC179x_wirBB->pushBackInstruction(
      { m16BitOperations ?
        WIR_Operation { TC13::OpCode::RSUB, TC13::OperationFormat::SD,
          new WIR_RegisterParameter( Da, WIR_Usage::defuse ) } :
        WIR_Operation { TC13::OpCode::RSUB, TC13::OperationFormat::DDC9_1,
          new WIR_RegisterParameter( Da, WIR_Usage::def ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ),
          new TC_Const9_Signed( 0 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertSH( LLIR_Register *r0, LLIR_Register *r1, int c0,
                                   const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSH( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a SH instruction.

  Exact format: SH D[c] (def), D[a] (use), const9 (DDC9_1)
*/
void InstructionFactory::insertSH( const WIR::TC_DRegV &Dc,
                                   const WIR::TC_DRegV &Da, int const9,
                                   const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertSH(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::SH, TC13::OperationFormat::DDC9_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertSH( LLIR_Register *r0, LLIR_Register *r1,
                                   LLIR_Register *r2,
                                   const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSH( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a SH instruction.

  Exact format: SH D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertSH( const WIR::TC_DRegV &Dc,
                                   const WIR::TC_DRegV &Da,
                                   const WIR::TC_DRegV &Db,
                                   const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertSH(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::SH, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertSHA( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSHA( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a SHA instruction.

  Exact format: SHA D[c] (def), D[a] (use), const9, PSW.C (def) (DDC9PSW_1)
*/
void InstructionFactory::insertSHA( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da, int const9,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertSHA(const TC_DRegV&, const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::SHA, TC13::OperationFormat::DDC9PSW_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Signed( const9 ),
        new
          WIR_RegisterParameter(
            TC179x_wirProc->PSW_C(), WIR_Usage::def ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertSHA( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSHA( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a SHA instruction.

  Exact format: SHA D[c] (def), D[a] (use), D[b] (use), PSW.C (def) (DDDPSW_1)
*/
void InstructionFactory::insertSHA( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da,
                                    const WIR::TC_DRegV &Db,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertSHA(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::SHA, TC13::OperationFormat::DDDPSW_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new
          WIR_RegisterParameter(
            TC179x_wirProc->PSW_C(), WIR_Usage::def ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertST_A( const std::string &o0, LLIR_Register *r0,
                                     int c0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  if ( ( c0 >= minSignedConst10Value ) && ( c0 <= maxSignedConst10Value ) ) {
    mCurrentInstr = insST_A( o0, r0, c0, r1, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

    handleDataAccess( mCurrentInstr, exp );
  } else {
    if ( o0 == OPER_PREINC ) {
      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r0, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( r0, OPER_BASE, c0 );

      insertST_A( OPER_BASE, r0, 0, r1, exp );
    } else

    if ( o0 == OPER_POSTINC ) {
      insertST_A( OPER_BASE, r0, 0, r1, exp );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r0, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( r0, OPER_BASE, c0 );
    } else {
      LLIR_Register *reg1 = CreateRegister( "", true );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( reg1, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( reg1, OPER_BASE, c0, r0 );

      insertST_A( o0, reg1, 0, r1, exp );
    }
  }
};


/*
  Inserts a ST.A instruction.

  Exact format: ST.A [A[b] (use)]off, A[a] (use) (AC10ABOA)

  Handling of address offsets beyond signed 10 bits is included. Exact formats
  either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  ST.A [A[x] (use)]<lower 10 bits of off>, A[a] (use) (AC10ABOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
  ST.A [A[x] (use)]0, A[a] (use) (AC10ABOA)
*/
void InstructionFactory::insertST_A( const WIR::TC_ARegV &Ab, int off,
                                     const WIR::TC_ARegV &Aa,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertST_A(const TC_ARegV&, int, const TC_ARegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::ST_A, TC13::OperationFormat::AC10ABOA,
          new WIR_RegisterParameter( Ab, WIR_Usage::use ),
          new TC_Const10_Signed( off ),
          new WIR_RegisterParameter( Aa, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else {
    auto &Ax = createAReg();
    auto p = splitOffset( off );

    insertADDIH_A( Ax, Ab, p.first, exp, type );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
      insertST_A( Ax, p.second, Aa, exp, type );
    else {
      insertLEA( Ax, Ax, p.second, exp, type );
      insertST_A( Ax, 0, Aa, exp, type );
    }
  }
};


/*
  Inserts a ST.A instruction.

  Exact formats:

  ST.A [+A[b] (defuse)]off, A[a] (use) (AC10APIA)
  ST.A [A[b] (defuse)+]off, A[a] (use) (AC10APIA)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  ST.A [A[b] (use)]0, A[a] (use) (AC10ABOA)

  Exact formats for post-increment:

  ST.A [A[b] (use)]0, A[a] (use) (AC10ABOA)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  ST.A [+A[b] (defuse)]<lower 10 bits of off>, A[a] (use) (AC10APIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  ST.A [A[b] (use)]0, A[a] (use) (AC10ABOA)

  Exact formats for post-increment either:

  ST.A [A[b] (defuse)+]<lower 10 bits of off>, A[a] (use) (AC10APIA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  ST.A [A[b] (use)]0, A[a] (use) (AC10ABOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertST_A( const WIR::TC13::AddressingMode &m,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const WIR::TC_ARegV &Aa,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertST_A(const TC13::AddressingMode&, const TC_ARegV&, int, const TC_ARegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::ST_A, TC13::OperationFormat::AC10APIA,
          new WIR_AddressingModeParameter( m ),
          new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
          new TC_Const10_Signed( off ),
          new WIR_RegisterParameter( Aa, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      insertLEA( Ab, Ab, off, exp, type );
      insertST_A( Ab, 0, Aa, exp, type );
    } else {
      insertST_A( Ab, 0, Aa, exp, type );
      insertLEA( Ab, Ab, off, exp, type );
    }
  } else {
    auto p = splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertADDIH_A( Ab, Ab, p.first, exp, type );
        insertST_A( m, Ab, p.second, Aa, exp, type );
      } else {
        handleLargeOffset( Ab, Ab, off, exp, type );
        insertST_A( Ab, 0, Aa, exp, type );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertST_A( m, Ab, p.second, Aa, exp, type );
        insertADDIH_A( Ab, Ab, p.first, exp, type );
      } else {
        insertST_A( Ab, 0, Aa, exp, type );
        handleLargeOffset( Ab, Ab, off, exp, type );
      }
    }
  }
};


void InstructionFactory::insertST_B( const std::string &o0, LLIR_Register *r0,
                                     int c0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  if ( ( c0 >= minSignedConst10Value ) && ( c0 <= maxSignedConst10Value ) ) {
    mCurrentInstr = insST_B( o0, r0, c0, r1, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

    handleDataAccess( mCurrentInstr, exp );
  } else {
    if ( o0 == OPER_PREINC ) {
      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r0, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( r0, OPER_BASE, c0 );

      insertST_B( OPER_BASE, r0, 0, r1, exp );
    } else

    if ( o0 == OPER_POSTINC ) {
      insertST_B( OPER_BASE, r0, 0, r1, exp );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r0, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( r0, OPER_BASE, c0 );
    } else {
      LLIR_Register *reg1 = CreateRegister( "", true );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( reg1, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( reg1, OPER_BASE, c0, r0 );

      insertST_B( o0, reg1, 0, r1, exp );
    }
  }
};


/*
  Inserts a ST.B instruction.

  Exact format: ST.B [A[b] (use)]off, D[a] (use) (AC10DBOA_1)

  Handling of address offsets beyond signed 10 bits is included. Exact formats
  either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  ST.B [A[x] (use)]<lower 10 bits of off>, D[a] (use) (AC10DBOA_1)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
  ST.B [A[x] (use)]0, D[a] (use) (AC10DBOA_1)
*/
void InstructionFactory::insertST_B( const WIR::TC_ARegV &Ab, int off,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertST_B(const TC_ARegV&, int, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::ST_B, TC13::OperationFormat::AC10DBOA_1,
          new WIR_RegisterParameter( Ab, WIR_Usage::use ),
          new TC_Const10_Signed( off ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else {
    auto &Ax = createAReg();
    auto p = splitOffset( off );

    insertADDIH_A( Ax, Ab, p.first, exp, type );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
      insertST_B( Ax, p.second, Da, exp, type );
    else {
      insertLEA( Ax, Ax, p.second, exp, type );
      insertST_B( Ax, 0, Da, exp, type );
    }
  }
};


/*
  Inserts a ST.B instruction.

  Exact formats:

  ST.B [+A[b] (defuse)]off, D[a] (use) (AC10DPIA_1)
  ST.B [A[b] (defuse)+]off, D[a] (use) (AC10DPIA_1)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  ST.B [A[b] (use)]0, D[a] (use) (AC10DBOA_1)

  Exact formats for post-increment:

  ST.B [A[b] (use)]0, D[a] (use) (AC10DBOA_1)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  ST.B [+A[b] (defuse)]<lower 10 bits of off>, D[a] (use) (AC10DPIA_1)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  ST.B [A[b] (use)]0, D[a] (use) (AC10DBOA_1)

  Exact formats for post-increment either:

  ST.B [A[b] (defuse)+]<lower 10 bits of off>, D[a] (use) (AC10DPIA_1)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  ST.B [A[b] (use)]0, D[a] (use) (AC10DBOA_1)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertST_B( const WIR::TC13::AddressingMode &m,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertST_B(const TC13::AddressingMode&, const TC_ARegV&, int, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::ST_B, TC13::OperationFormat::AC10DPIA_1,
          new WIR_AddressingModeParameter( m ),
          new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
          new TC_Const10_Signed( off ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      insertLEA( Ab, Ab, off, exp, type );
      insertST_B( Ab, 0, Da, exp, type );
    } else {
      insertST_B( Ab, 0, Da, exp, type );
      insertLEA( Ab, Ab, off, exp, type );
    }
  } else {
    auto p = splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertADDIH_A( Ab, Ab, p.first, exp, type );
        insertST_B( m, Ab, p.second, Da, exp, type );
      } else {
        handleLargeOffset( Ab, Ab, off, exp, type );
        insertST_B( Ab, 0, Da, exp, type );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertST_B( m, Ab, p.second, Da, exp, type );
        insertADDIH_A( Ab, Ab, p.first, exp, type );
      } else {
        insertST_B( Ab, 0, Da, exp, type );
        handleLargeOffset( Ab, Ab, off, exp, type );
      }
    }
  }
};


void InstructionFactory::insertST_D( const std::string &o0, LLIR_Register *r0,
                                     int c0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  if ( ( c0 >= minSignedConst10Value ) && ( c0 <= maxSignedConst10Value ) ) {
    mCurrentInstr = insST_D( o0, r0, c0, r1, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

    handleDataAccess( mCurrentInstr, exp );
  } else {
    if ( o0 == OPER_PREINC ) {
      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r0, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( r0, OPER_BASE, c0 );

      insertST_D( OPER_BASE, r0, 0, r1, exp );
    } else

    if ( o0 == OPER_POSTINC ) {
      insertST_D( OPER_BASE, r0, 0, r1, exp );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r0, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( r0, OPER_BASE, c0 );
    } else {
      LLIR_Register *reg1 = CreateRegister( "", true );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( reg1, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( reg1, OPER_BASE, c0, r0 );

      insertST_D( o0, reg1, 0, r1, exp );
    }
  }
};


/*
  Inserts a ST.D instruction.

  Exact format: ST.D [A[b] (use)]off, E[a] (use) (AC10EBOA)

  Handling of address offsets beyond signed 10 bits is included. Exact formats
  either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  ST.D [A[x] (use)]<lower 10 bits of off>, E[a] (use) (AC10EBOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
  ST.D [A[x] (use)]0, E[a] (use) (AC10EBOA)
*/
void InstructionFactory::insertST_D( const WIR::TC_ARegV &Ab, int off,
                                     const WIR::TC_ERegV &Ea,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertST_D(const TC_ARegV&, int, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EBOA,
          new WIR_RegisterParameter( Ab, WIR_Usage::use ),
          new TC_Const10_Signed( off ),
          new WIR_RegisterParameter( Ea, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else {
    auto &Ax = createAReg();
    auto p = splitOffset( off );

    insertADDIH_A( Ax, Ab, p.first, exp, type );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
      insertST_D( Ax, p.second, Ea, exp, type );
    else {
      insertLEA( Ax, Ax, p.second, exp, type );
      insertST_D( Ax, 0, Ea, exp, type );
    }
  }
};


/*
  Inserts a ST.D instruction.

  Exact formats:

  ST.D [+A[b] (defuse)]off, E[a] (use) (AC10EPIA)
  ST.D [A[b] (defuse)+]off, E[a] (use) (AC10EPIA)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  ST.D [A[b] (use)]0, E[a] (use) (AC10EBOA)

  Exact formats for post-increment:

  ST.D [A[b] (use)]0, E[a] (use) (AC10EBOA)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  ST.D [+A[b] (defuse)]<lower 10 bits of off>, E[a] (use) (AC10EPIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  ST.D [A[b] (use)]0, E[a] (use) (AC10EBOA)

  Exact formats for post-increment either:

  ST.D [A[b] (defuse)+]<lower 10 bits of off>, E[a] (use) (AC10EPIA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  ST.D [A[b] (use)]0, E[a] (use) (AC10EBOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertST_D( const WIR::TC13::AddressingMode &m,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const WIR::TC_ERegV &Ea,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertST_D(const TC13::AddressingMode&, const TC_ARegV&, int, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EPIA,
          new WIR_AddressingModeParameter( m ),
          new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
          new TC_Const10_Signed( off ),
          new WIR_RegisterParameter( Ea, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      insertLEA( Ab, Ab, off, exp, type );
      insertST_D( Ab, 0, Ea, exp, type );
    } else {
      insertST_D( Ab, 0, Ea, exp, type );
      insertLEA( Ab, Ab, off, exp, type );
    }
  } else {
    auto p = splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertADDIH_A( Ab, Ab, p.first, exp, type );
        insertST_D( m, Ab, p.second, Ea, exp, type );
      } else {
        handleLargeOffset( Ab, Ab, off, exp, type );
        insertST_D( Ab, 0, Ea, exp, type );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertST_D( m, Ab, p.second, Ea, exp, type );
        insertADDIH_A( Ab, Ab, p.first, exp, type );
      } else {
        insertST_D( Ab, 0, Ea, exp, type );
        handleLargeOffset( Ab, Ab, off, exp, type );
      }
    }
  }
};


void InstructionFactory::insertST_H( const std::string &o0, LLIR_Register *r0,
                                     int c0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  if ( ( c0 >= minSignedConst10Value ) && ( c0 <= maxSignedConst10Value ) ) {
    mCurrentInstr = insST_H( o0, r0, c0, r1, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

    handleDataAccess( mCurrentInstr, exp );
  } else {
    if ( o0 == OPER_PREINC ) {
      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r0, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( r0, OPER_BASE, c0 );

      insertST_H( OPER_BASE, r0, 0, r1, exp );
    } else

    if ( o0 == OPER_POSTINC ) {
      insertST_H( OPER_BASE, r0, 0, r1, exp );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r0, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( r0, OPER_BASE, c0 );
    } else {
      LLIR_Register *reg1 = CreateRegister( "", true );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( reg1, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( reg1, OPER_BASE, c0, r0 );

      insertST_H( o0, reg1, 0, r1, exp );
    }
  }
};


/*
  Inserts a ST.H instruction.

  Exact format: ST.H [A[b] (use)]off, D[a] (use) (AC10DBOA_1)

  Handling of address offsets beyond signed 10 bits is included. Exact formats
  either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  ST.H [A[x] (use)]<lower 10 bits of off>, D[a] (use) (AC10DBOA_1)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
  ST.H [A[x] (use)]0, D[a] (use) (AC10DBOA_1)
*/
void InstructionFactory::insertST_H( const WIR::TC_ARegV &Ab, int off,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertST_H(const TC_ARegV&, int, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::ST_H, TC13::OperationFormat::AC10DBOA_1,
          new WIR_RegisterParameter( Ab, WIR_Usage::use ),
          new TC_Const10_Signed( off ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else {
    auto &Ax = createAReg();
    auto p = splitOffset( off );

    insertADDIH_A( Ax, Ab, p.first, exp, type );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) )
      insertST_H( Ax, p.second, Da, exp, type );
    else {
      insertLEA( Ax, Ax, p.second, exp, type );
      insertST_H( Ax, 0, Da, exp, type );
    }
  }
};


/*
  Inserts a ST.H instruction.

  Exact formats:

  ST.H [+A[b] (defuse)]off, D[a] (use) (AC10DPIA_1)
  ST.H [A[b] (defuse)+]off, D[a] (use) (AC10DPIA_1)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  ST.H [A[b] (use)]0, D[a] (use) (AC10DBOA_1)

  Exact formats for post-increment:

  ST.H [A[b] (use)]0, D[a] (use) (AC10DBOA_1)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  ST.H [+A[b] (defuse)]<lower 10 bits of off>, D[a] (use) (AC10DPIA_1)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  ST.H [A[b] (use)]0, D[a] (use) (AC10DBOA_1)

  Exact formats for post-increment either:

  ST.H [A[b] (defuse)+]<lower 10 bits of off>, D[a] (use) (AC10DPIA_1)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  ST.H [A[b] (use)]0, D[a] (use) (AC10DBOA_1)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertST_H( const WIR::TC13::AddressingMode &m,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertST_H(const TC13::AddressingMode&, const TC_ARegV&, int, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::ST_H, TC13::OperationFormat::AC10DPIA_1,
          new WIR_AddressingModeParameter( m ),
          new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
          new TC_Const10_Signed( off ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      insertLEA( Ab, Ab, off, exp, type );
      insertST_H( Ab, 0, Da, exp, type );
    } else {
      insertST_H( Ab, 0, Da, exp, type );
      insertLEA( Ab, Ab, off, exp, type );
    }
  } else {
    auto p = splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertADDIH_A( Ab, Ab, p.first, exp, type );
        insertST_H( m, Ab, p.second, Da, exp, type );
      } else {
        handleLargeOffset( Ab, Ab, off, exp, type );
        insertST_H( Ab, 0, Da, exp, type );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertST_H( m, Ab, p.second, Da, exp, type );
        insertADDIH_A( Ab, Ab, p.first, exp, type );
      } else {
        insertST_H( Ab, 0, Da, exp, type );
        handleLargeOffset( Ab, Ab, off, exp, type );
      }
    }
  }
};


void InstructionFactory::insertST_W( const std::string &o0, LLIR_Register *r0,
                                     int c0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  if ( ( c0 >= minSignedConst10Value ) && ( c0 <= maxSignedConst10Value ) ) {
    mCurrentInstr = insST_W( o0, r0, c0, r1, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

    handleDataAccess( mCurrentInstr, exp );
  } else {
    if ( o0 == OPER_PREINC ) {
      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r0, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( r0, OPER_BASE, c0 );

      insertST_W( OPER_BASE, r0, 0, r1, exp );
    } else

    if ( o0 == OPER_POSTINC ) {
      insertST_W( OPER_BASE, r0, 0, r1, exp );

      if ( ( c0 >= minSignedConst16Value ) && ( c0 <= maxSignedConst16Value ) )
        insertLEA( r0, OPER_BASE, r0, c0 );
      else
        handleLargeOffset( r0, OPER_BASE, c0 );
    } else {
      if ( ( c0 >= minSignedConst16Value ) &&
           ( c0 <= maxSignedConst16Value ) ) {
        mCurrentInstr = insST_W( o0, r0, c0, r1, LAST_LLIR_BB, mCurrentInstr );

        // If exp defined, insert assembler debug information.
        if ( mConfig.getGenerateDebugFlag() && exp )
          addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

        handleDataAccess( mCurrentInstr, exp );
      } else {
        LLIR_Register *reg1 = CreateRegister( "", true );

        handleLargeOffset( reg1, OPER_BASE, c0, r0 );

        insertST_W( o0, reg1, 0, r1, exp );
      }
    }
  }
};


/*
  Inserts a ST.W instruction.

  Exact format: ST.W [A[b] (use)]off, D[a] (use) (AC16DBOA)

  Handling of address offsets beyond signed 16 bits is included. Exact formats:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  ST.W [A[x] (use)]<lower 16 bits of off>, D[a] (use) (AC16DBOA)
*/
void InstructionFactory::insertST_W( const WIR::TC_ARegV &Ab, int off,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertST_W(const TC_ARegV&, int, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::ST_W, TC13::OperationFormat::AC16DBOA,
          new WIR_RegisterParameter( Ab, WIR_Usage::use ),
          new TC_Const16_Signed( off ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else {
    auto &Ax = createAReg();
    auto p = splitOffset( off );

    insertADDIH_A( Ax, Ab, p.first, exp, type );
    insertST_W( Ax, p.second, Da, exp, type );
  }
};


/*
  Inserts a ST.W instruction.

  Exact formats:

  ST.W [+A[b] (defuse)]off, D[a] (use) (AC10DPIA_1)
  ST.W [A[b] (defuse)+]off, D[a] (use) (AC10DPIA_1)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  ST.W [A[b] (use)]0, D[a] (use) (AC10DBOA_1)

  Exact formats for post-increment:

  ST.W [A[b] (use)]0, D[a] (use) (AC10DBOA_1)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  ST.W [+A[b] (defuse)]<lower 10 bits of off>, D[a] (use) (AC10DPIA_1)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  ST.W [A[b] (use)]0, D[a] (use) (AC16DBOA)

  Exact formats for post-increment either:

  ST.W [A[b] (defuse)+]<lower 10 bits of off>, D[a] (use) (AC10DPIA_1)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  ST.W [A[b] (use)]0, D[a] (use) (AC16DBOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::insertST_W( const WIR::TC13::AddressingMode &m,
                                     const WIR::TC_ARegV &Ab, int off,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertST_W(const TC13::AddressingMode&, const TC_ARegV&, int, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::ST_W, TC13::OperationFormat::AC10DPIA_1,
          new WIR_AddressingModeParameter( m ),
          new WIR_RegisterParameter( Ab, WIR_Usage::defuse ),
          new TC_Const10_Signed( off ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
    handleDataAccess( i, exp );
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      insertLEA( Ab, Ab, off, exp, type );
      insertST_W( Ab, 0, Da, exp, type );
    } else {
      insertST_W( Ab, 0, Da, exp, type );
      insertLEA( Ab, Ab, off, exp, type );
    }
  } else {
    auto p = splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertADDIH_A( Ab, Ab, p.first, exp, type );
        insertST_W( m, Ab, p.second, Da, exp, type );
      } else {
        handleLargeOffset( Ab, Ab, off, exp, type );
        insertST_W( Ab, 0, Da, exp, type );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        insertST_W( m, Ab, p.second, Da, exp, type );
        insertADDIH_A( Ab, Ab, p.first, exp, type );
      } else {
        insertST_W( Ab, 0, Da, exp, type );
        handleLargeOffset( Ab, Ab, off, exp, type );
      }
    }
  }
};


void InstructionFactory::insertST_W( const std::string &o0, LLIR_Register *r0,
                                     const std::string &l0,
                                     const std::string &l1, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insST_W( o0, r0, l0, l1, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );

  handleDataAccess( mCurrentInstr, exp );
};


/*
  Inserts a ST.W instruction.

  Exact format: ST.W [A[b] (use)] LO:label, D[a] (use) (ALDBOA)
*/
void InstructionFactory::insertST_W( const WIR::TC_ARegV &Ab,
                                     const WIR::WIR_Data &d,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertST_W(const TC_ARegV&, const WIR_Data&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::ST_W, TC13::OperationFormat::ALC16DBOA,
        new WIR_RegisterParameter( Ab, WIR_Usage::use ),
        new WIR_LabelParameter( d ), new TC_Const16_Signed( 0 ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
  handleDataAccess( i, exp );
};


void InstructionFactory::insertSUB( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSUB( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a SUB instruction.

  Exact format: SUB D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertSUB( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da,
                                    const WIR::TC_DRegV &Db,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertSUB(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::SUB, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertSUB_A( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSUB_A( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a SUB.A instruction.

  Exact format: SUB.A A[c] (def), A[a] (use), A[b] (use) (AAA)
*/
void InstructionFactory::insertSUB_A( const WIR::TC_ARegV &Ac,
                                      const WIR::TC_ARegV &Aa,
                                      const WIR::TC_ARegV &Ab,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertSUB_A(const TC_ARegV&, const TC_ARegV&, const TC_ARegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::SUB_A, TC13::OperationFormat::AAA,
        new WIR_RegisterParameter( Ac, WIR_Usage::def ),
        new WIR_RegisterParameter( Aa, WIR_Usage::use ),
        new WIR_RegisterParameter( Ab, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertSUBC( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSUBC( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a SUBC instruction.

  Exact format:
  SUBC D[c] (def), D[a] (use), D[b] (use), PSW.C (defuse) (DDDPSW_2)
*/
void InstructionFactory::insertSUBC( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertSUBC(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::SUBC, TC13::OperationFormat::DDDPSW_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new
          WIR_RegisterParameter(
            TC179x_wirProc->PSW_C(), WIR_Usage::defuse ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertSUBX( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSUBX( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a SUBX instruction.

  Exact format: SUBX D[c] (def), D[a] (use), D[b] (use), PSW.C (def) (DDDPSW_1)
*/
void InstructionFactory::insertSUBX( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertSUBX(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::SUBX, TC13::OperationFormat::DDDPSW_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ),
        new
          WIR_RegisterParameter(
            TC179x_wirProc->PSW_C(), WIR_Usage::def ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertXNOR( LLIR_Register *r0, LLIR_Register *r1,
                                     int c0,
                                     const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insXNOR( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an XNOR instruction.

  Exact format: XNOR D[c] (def), D[a] (use), const9 (DDC9_2)
*/
void InstructionFactory::insertXNOR( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     unsigned int const9,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertXNOR(const TC_DRegV&, const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::XNOR, TC13::OperationFormat::DDC9_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Unsigned( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertXOR( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insXOR( r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an XOR instruction.

  Exact format: XOR D[c] (def), D[a] (use), const9 (DDC9_2)
*/
void InstructionFactory::insertXOR( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da,
                                    unsigned int const9,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertXOR(const TC_DRegV&, const TC_DRegV&, unsigned int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::XOR, TC13::OperationFormat::DDC9_2,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new TC_Const9_Unsigned( const9 ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertXOR( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2,
                                    const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insXOR( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts an XOR instruction.

  Exact format: XOR D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertXOR( const WIR::TC_DRegV &Dc,
                                    const WIR::TC_DRegV &Da,
                                    const WIR::TC_DRegV &Db,
                                    const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertXOR(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::XOR, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


//
// Native TriCore floating-point instructions or library calls.
//

void InstructionFactory::insertADD_F( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  if ( mConfig.getEmitFpuInstructions() ) {
    mCurrentInstr = insADDF( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
  } else {
    LLIR_Register *reg0 = CreateRegister( "" );
    LLIR_Register *reg1 = CreateRegister( "" );
    LLIR_Register *reg2 = CreateRegister( "" );
    deque<LLIR_Register *> regs;

    insertMOV( reg1, r1 );
    LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
    regs.push_back( reg1 );
    insertMOV( reg2, r2 );
    LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateRegister( PHREG_D5 ) );
    regs.push_back( reg2 );

    string functionName = getSoftFloatSymbol( SoftFloatSymbol::ADDF );

    insertCALL( functionName, &regs, exp, type );

    // Setup proper DEF/USE attributes.
    LLIR_Instruction *insCall = getCurrentInstruction();
    LLIR_Operation *opCall = insCall->GetFirstOp();
    LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
    opCall->AddParameter( implicitParam );

    // A call delimits a basic block.
    mCodesel.beginNewLLIRBasicBlock();

    LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
    insertMOV( r0, reg0 );
  }
};


/*
  Inserts an ADD.F instruction.

  Exact format: ADD.F D[c] (def), D[d] (use), D[a] (use) (DDD_1)
*/
void InstructionFactory::insertADD_F( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Dd,
                                      const WIR::TC_DRegV &Da,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertADD_F(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( mConfig.getEmitFpuInstructions() ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::ADD_F, TC13::OperationFormat::DDD_1,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter( Dd, WIR_Usage::use ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    // Create pre-colored virtual registers for argument/return value passing.
    auto &reg0 = createDReg();
    auto &reg1 = createDReg();
    auto &reg2 = createDReg();

    TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
    TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );
    TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->D5() );

    // Move arguments and generate the CALL.
    list<reference_wrapper<WIR_BaseRegister>> regs;
    regs.push_back( reg1 );
    regs.push_back( reg2 );

    insertMOV( reg1, Dd, exp, type );
    insertMOV( reg2, Da, exp, type );

    insertCALL(
      getSoftFloatFunction( SoftFloatSymbol::ADDF ), regs, exp, type );

    // Add DEFined implicit parameter for function result.
    WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
    callOp.pushBackParameter(
      new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

    // Start new basic block after the CALL and move return value.
    mCodesel.startNewBasicBlock();
    insertMOV( Dc, reg0, exp, type );
  }
};


void InstructionFactory::insertCMP_F( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insCMPF( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a CMP.F instruction.

  Exact format: CMP.F D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertCMP_F( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertCMP_F(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::CMP_F, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertDIV_F( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  if ( mConfig.getEmitFpuInstructions() ) {
    mCurrentInstr = insDIVF( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
  } else {
    LLIR_Register *reg0 = CreateRegister( "" );
    LLIR_Register *reg1 = CreateRegister( "" );
    LLIR_Register *reg2 = CreateRegister( "" );
    deque<LLIR_Register *> regs;

    insertMOV( reg1, r1 );
    LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
    regs.push_back( reg1 );
    insertMOV( reg2, r2 );
    LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateRegister( PHREG_D5 ) );
    regs.push_back( reg2 );

    string functionName = getSoftFloatSymbol( SoftFloatSymbol::DIVF );

    insertCALL( functionName, &regs, exp, type );

    // Setup proper DEF/USE attributes
    LLIR_Instruction *insCall = getCurrentInstruction();
    LLIR_Operation *opCall = insCall->GetFirstOp();
    LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
    opCall->AddParameter( implicitParam );

    // A call delimits a basic block.
    mCodesel.beginNewLLIRBasicBlock();

    LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
    insertMOV( r0, reg0 );
  }
};


/*
  Inserts an DIV.F instruction.

  Exact format: DIV.F D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertDIV_F( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDIV_F(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( mConfig.getEmitFpuInstructions() ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::DIV_F, TC13::OperationFormat::DDD_1,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    // Create pre-colored virtual registers for argument/return value passing.
    auto &reg0 = createDReg();
    auto &reg1 = createDReg();
    auto &reg2 = createDReg();

    TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
    TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );
    TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->D5() );

    // Move arguments and generate the CALL.
    list<reference_wrapper<WIR_BaseRegister>> regs;
    regs.push_back( reg1 );
    regs.push_back( reg2 );

    insertMOV( reg1, Da, exp, type );
    insertMOV( reg2, Db, exp, type );

    insertCALL(
      getSoftFloatFunction( SoftFloatSymbol::DIVF ), regs, exp, type );

    // Add DEFined implicit parameter for function result.
    WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
    callOp.pushBackParameter(
      new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

    // Start new basic block after the CALL and move return value.
    mCodesel.startNewBasicBlock();
    insertMOV( Dc, reg0, exp, type );
  }
};


void InstructionFactory::insertFTOI( LLIR_Register *r0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  if ( mConfig.getEmitFpuInstructions() ) {
    mCurrentInstr = insFTOI( r0, r1, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
  } else {
    LLIR_Register *reg0 = CreateRegister( "" );
    LLIR_Register *reg1 = CreateRegister( "" );
    deque<LLIR_Register *> regs;

    insertMOV( reg1, r1 );
    LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
    regs.push_back( reg1 );

    string functionName = getSoftFloatSymbol( SoftFloatSymbol::FTOI );

    insertCALL( functionName, &regs, exp, type );

    // Setup proper DEF/USE attributes
    LLIR_Instruction *insCall = getCurrentInstruction();
    LLIR_Operation *opCall = insCall->GetFirstOp();
    LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
    opCall->AddParameter( implicitParam );

    // A call delimits a basic block
    mCodesel.beginNewLLIRBasicBlock();

    LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
    insertMOV( r0, reg0 );
  }
};


/*
  Inserts a FTOI instruction.

  Exact format: FTOI D[c] (def), D[a] (use) (DD)
*/
void InstructionFactory::insertFTOI( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertFTOI(const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( mConfig.getEmitFpuInstructions() ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::FTOI, TC13::OperationFormat::DD,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    // Create pre-colored virtual registers for argument/return value passing.
    auto &reg0 = createDReg();
    auto &reg1 = createDReg();

    TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
    TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );

    // Move arguments and generate the CALL.
    list<reference_wrapper<WIR_BaseRegister>> regs;
    regs.push_back( reg1 );

    insertMOV( reg1, Da, exp, type );

    insertCALL(
      getSoftFloatFunction( SoftFloatSymbol::FTOI ), regs, exp, type );

    // Add DEFined implicit parameter for function result.
    WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
    callOp.pushBackParameter(
      new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

    // Start new basic block after the CALL and move return value.
    mCodesel.startNewBasicBlock();
    insertMOV( Dc, reg0, exp, type );
  }
};


void InstructionFactory::insertFTOQ31( LLIR_Register *r0, LLIR_Register *r1,
                                       LLIR_Register *r2,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insFTOQ31( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a FTOQ31 instruction.

  Exact format: FTOQ31 D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertFTOQ31( const WIR::TC_DRegV &Dc,
                                       const WIR::TC_DRegV &Da,
                                       const WIR::TC_DRegV &Db,
                                       const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::FTOQ31, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertFTOU( LLIR_Register *r0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  if ( mConfig.getEmitFpuInstructions() ) {
    mCurrentInstr = insFTOU( r0, r1, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
  } else {
    LLIR_Register *reg0 = CreateRegister( "" );
    LLIR_Register *reg1 = CreateRegister( "" );
    deque<LLIR_Register *> regs;

    insertMOV( reg1, r1 );
    LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
    regs.push_back( reg1 );

    string functionName = getSoftFloatSymbol( SoftFloatSymbol::FTOU );

    insertCALL( functionName, &regs, exp, type );

    // Setup proper DEF/USE attributes
    LLIR_Instruction *insCall = getCurrentInstruction();
    LLIR_Operation *opCall = insCall->GetFirstOp();
    LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
    opCall->AddParameter( implicitParam );

    // A call delimits a basic block.
    mCodesel.beginNewLLIRBasicBlock();

    LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
    insertMOV( r0, reg0 );
  }
};


/*
  Inserts a FTOU instruction.

  Exact format: FTOU D[c] (def), D[a] (use) (DD)
*/
void InstructionFactory::insertFTOU( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertFTOU(const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( mConfig.getEmitFpuInstructions() ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::FTOU, TC13::OperationFormat::DD,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    // Create pre-colored virtual registers for argument/return value passing.
    auto &reg0 = createDReg();
    auto &reg1 = createDReg();

    TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
    TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );

    // Move arguments and generate the CALL.
    list<reference_wrapper<WIR_BaseRegister>> regs;
    regs.push_back( reg1 );

    insertMOV( reg1, Da, exp, type );

    insertCALL(
      getSoftFloatFunction( SoftFloatSymbol::FTOU ), regs, exp, type );

    // Add DEFined implicit parameter for function result.
    WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
    callOp.pushBackParameter(
      new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

    // Start new basic block after the CALL and move return value.
    mCodesel.startNewBasicBlock();
    insertMOV( Dc, reg0, exp, type );
  }
};


void InstructionFactory::insertITOF( LLIR_Register *r0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  if ( mConfig.getEmitFpuInstructions() ) {
    mCurrentInstr = insITOF( r0, r1, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
  } else {
    LLIR_Register *reg0 = CreateRegister( "" );
    LLIR_Register *reg1 = CreateRegister( "" );
    deque<LLIR_Register *> regs;

    insertMOV( reg1, r1 );
    LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
    regs.push_back( reg1 );

    string functionName = getSoftFloatSymbol( SoftFloatSymbol::ITOF );

    insertCALL( functionName, &regs, exp, type );

    // Setup proper DEF/USE attributes
    LLIR_Instruction *insCall = getCurrentInstruction();
    LLIR_Operation *opCall = insCall->GetFirstOp();
    LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
    opCall->AddParameter( implicitParam );

    // A call delimits a basic block.
    mCodesel.beginNewLLIRBasicBlock();

    LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
    insertMOV( r0, reg0 );
  }
};


/*
  Inserts an ITOF instruction.

  Exact format: ITOF D[c] (def), D[a] (use) (DD)
*/
void InstructionFactory::insertITOF( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertITOF(const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( mConfig.getEmitFpuInstructions() ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::ITOF, TC13::OperationFormat::DD,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    // Create pre-colored virtual registers for argument/return value passing.
    auto &reg0 = createDReg();
    auto &reg1 = createDReg();

    TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
    TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );

    // Move arguments and generate the CALL.
    list<reference_wrapper<WIR_BaseRegister>> regs;
    regs.push_back( reg1 );

    insertMOV( reg1, Da, exp, type );

    insertCALL(
      getSoftFloatFunction( SoftFloatSymbol::ITOF ), regs, exp, type );

    // Add DEFined implicit parameter for function result.
    WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
    callOp.pushBackParameter(
      new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

    // Start new basic block after the CALL and move return value.
    mCodesel.startNewBasicBlock();
    insertMOV( Dc, reg0, exp, type );
  }
};


void InstructionFactory::insertMADD_F( LLIR_Register *r0, LLIR_Register *r1,
                                       LLIR_Register *r2, LLIR_Register *r3,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMADDF( r0, r1, r2, r3, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MADD.F instruction.

  Exact format: MADD.F D[c] (def), D[d] (use), D[a] (use), D[b] (use) (DDDD)
*/
void InstructionFactory::insertMADD_F( const WIR::TC_DRegV &Dc,
                                       const WIR::TC_DRegV &Dd,
                                       const WIR::TC_DRegV &Da,
                                       const WIR::TC_DRegV &Db,
                                       const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MADD_F, TC13::OperationFormat::DDDD,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Dd, WIR_Usage::use ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMSUB_F( LLIR_Register *r0, LLIR_Register *r1,
                                       LLIR_Register *r2, LLIR_Register *r3,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMSUBF( r0, r1, r2, r3, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a MSUB.F instruction.

  Exact format: MSUB.F D[c] (def), D[d] (use), D[a] (use), D[b] (use) (DDDD)
*/
void InstructionFactory::insertMSUB_F( const WIR::TC_DRegV &Dc,
                                       const WIR::TC_DRegV &Dd,
                                       const WIR::TC_DRegV &Da,
                                       const WIR::TC_DRegV &Db,
                                       const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::MSUB_F, TC13::OperationFormat::DDDD,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Dd, WIR_Usage::use ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertMUL_F( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  if ( mConfig.getEmitFpuInstructions() ) {
    mCurrentInstr = insMULF( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
  } else {
    LLIR_Register *reg0 = CreateRegister( "" );
    LLIR_Register *reg1 = CreateRegister( "" );
    LLIR_Register *reg2 = CreateRegister( "" );
    deque<LLIR_Register *> regs;

    insertMOV( reg1, r1 );
    LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
    regs.push_back( reg1 );
    insertMOV( reg2, r2 );
    LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateRegister( PHREG_D5 ) );
    regs.push_back( reg2 );

    string functionName = getSoftFloatSymbol( SoftFloatSymbol::MULF );

    insertCALL( functionName, &regs, exp, type );

    // Setup proper DEF/USE attributes
    LLIR_Instruction *insCall = getCurrentInstruction();
    LLIR_Operation *opCall = insCall->GetFirstOp();
    LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
    opCall->AddParameter( implicitParam );

    // A call delimits a basic block.
    mCodesel.beginNewLLIRBasicBlock();

    LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
    insertMOV( r0, reg0 );
  }
};


/*
  Inserts a MUL.F instruction.

  Exact format: MUL.F D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertMUL_F( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Da,
                                      const WIR::TC_DRegV &Db,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMUL_F(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( mConfig.getEmitFpuInstructions() ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::MUL_F, TC13::OperationFormat::DDD_1,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ),
          new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    // Create pre-colored virtual registers for argument/return value passing.
    auto &reg0 = createDReg();
    auto &reg1 = createDReg();
    auto &reg2 = createDReg();

    TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
    TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );
    TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->D5() );

    // Move arguments and generate the CALL.
    list<reference_wrapper<WIR_BaseRegister>> regs;
    regs.push_back( reg1 );
    regs.push_back( reg2 );

    insertMOV( reg1, Da, exp, type );
    insertMOV( reg2, Db, exp, type );

    insertCALL(
      getSoftFloatFunction( SoftFloatSymbol::MULF ), regs, exp, type );

    // Add DEFined implicit parameter for function result.
    WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
    callOp.pushBackParameter(
      new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

    // Start new basic block after the CALL and move return value.
    mCodesel.startNewBasicBlock();
    insertMOV( Dc, reg0, exp, type );
  }
};


void InstructionFactory::insertQ31TOF( LLIR_Register *r0, LLIR_Register *r1,
                                       LLIR_Register *r2,
                                       const IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insQ31TOF( r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
};


/*
  Inserts a Q31TOF instruction.

  Exact format: Q31TOF D[c] (def), D[a] (use), D[b] (use) (DDD_1)
*/
void InstructionFactory::insertQ31TOF( const WIR::TC_DRegV &Dc,
                                       const WIR::TC_DRegV &Da,
                                       const WIR::TC_DRegV &Db,
                                       const IR_Exp *exp, StmtType type ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &i = TC179x_wirBB->pushBackInstruction(
    { { TC13::OpCode::Q31TOF, TC13::OperationFormat::DDD_1,
        new WIR_RegisterParameter( Dc, WIR_Usage::def ),
        new WIR_RegisterParameter( Da, WIR_Usage::use ),
        new WIR_RegisterParameter( Db, WIR_Usage::use ) } } );

  ADDDEBUGINFO( i, exp, type );
};


void InstructionFactory::insertSUB_F( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  if ( mConfig.getEmitFpuInstructions() ) {
    mCurrentInstr = insSUBF(r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
  } else {
    LLIR_Register *reg0 = CreateRegister( "" );
    LLIR_Register *reg1 = CreateRegister( "" );
    LLIR_Register *reg2 = CreateRegister( "" );
    deque<LLIR_Register *> regs;

    insertMOV( reg1, r1 );
    LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
    regs.push_back( reg1 );
    insertMOV( reg2, r2 );
    LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateRegister( PHREG_D5 ) );
    regs.push_back( reg2 );

    string functionName = getSoftFloatSymbol( SoftFloatSymbol::SUBF );

    insertCALL( functionName, &regs, exp, type );

    // Setup proper DEF/USE attributes
    LLIR_Instruction *insCall = getCurrentInstruction();
    LLIR_Operation *opCall = insCall->GetFirstOp();
    LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
    opCall->AddParameter( implicitParam );

    // A call delimits a basic block.
    mCodesel.beginNewLLIRBasicBlock();

    LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
    insertMOV( r0, reg0 );
  }
};


/*
  Inserts a SUB.F instruction.

  Exact format: SUB.F D[c] (def), D[d] (use), D[a] (use) (DDD_1)
*/
void InstructionFactory::insertSUB_F( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_DRegV &Dd,
                                      const WIR::TC_DRegV &Da,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertSUB_F(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( mConfig.getEmitFpuInstructions() ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::SUB_F, TC13::OperationFormat::DDD_1,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter( Dd, WIR_Usage::use ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    // Create pre-colored virtual registers for argument/return value passing.
    auto &reg0 = createDReg();
    auto &reg1 = createDReg();
    auto &reg2 = createDReg();

    TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
    TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );
    TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->D5() );

    // Move arguments and generate the CALL.
    list<reference_wrapper<WIR_BaseRegister>> regs;
    regs.push_back( reg1 );
    regs.push_back( reg2 );

    insertMOV( reg1, Dd, exp, type );
    insertMOV( reg2, Da, exp, type );

    insertCALL(
      getSoftFloatFunction( SoftFloatSymbol::SUBF ), regs, exp, type );

    // Add DEFined implicit parameter for function result.
    WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
    callOp.pushBackParameter(
      new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

    // Start new basic block after the CALL and move return value.
    mCodesel.startNewBasicBlock();
    insertMOV( Dc, reg0, exp, type );
  }
};


void InstructionFactory::insertUTOF( LLIR_Register *r0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  if ( mConfig.getEmitFpuInstructions() ) {
    mCurrentInstr = insUTOF( r0, r1, LAST_LLIR_BB, mCurrentInstr );

    // If exp defined, insert assembler debug information.
    if ( mConfig.getGenerateDebugFlag() && exp )
      addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
  } else {
    LLIR_Register *reg0 = CreateRegister( "" );
    LLIR_Register *reg1 = CreateRegister( "" );
    deque<LLIR_Register *> regs;

    insertMOV( reg1, r1 );
    LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
    regs.push_back( reg1 );

    string functionName = getSoftFloatSymbol( SoftFloatSymbol::UTOF );

    insertCALL( functionName, &regs, exp, type );

    // Setup proper DEF/USE attributes
    LLIR_Instruction *insCall = getCurrentInstruction();
    LLIR_Operation *opCall = insCall->GetFirstOp();
    LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
    opCall->AddParameter( implicitParam );

    // A call delimits a basic block.
    mCodesel.beginNewLLIRBasicBlock();

    LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
    insertMOV( r0, reg0 );
  }
};


/*
  Inserts an UTOF instruction.

  Exact format: UTOF D[c] (def), D[a] (use) (DD)
*/
void InstructionFactory::insertUTOF( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertUTOF(const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( mConfig.getEmitFpuInstructions() ) {
    auto &i = TC179x_wirBB->pushBackInstruction(
      { { TC13::OpCode::UTOF, TC13::OperationFormat::DD,
          new WIR_RegisterParameter( Dc, WIR_Usage::def ),
          new WIR_RegisterParameter( Da, WIR_Usage::use ) } } );

    ADDDEBUGINFO( i, exp, type );
  } else {
    // Create pre-colored virtual registers for argument/return value passing.
    auto &reg0 = createDReg();
    auto &reg1 = createDReg();

    TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
    TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );

    // Move arguments and generate the CALL.
    list<reference_wrapper<WIR_BaseRegister>> regs;
    regs.push_back( reg1 );

    insertMOV( reg1, Da, exp, type );

    insertCALL(
      getSoftFloatFunction( SoftFloatSymbol::UTOF ), regs, exp, type );

    // Add DEFined implicit parameter for function result.
    WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
    callOp.pushBackParameter(
      new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

    // Start new basic block after the CALL and move return value.
    mCodesel.startNewBasicBlock();
    insertMOV( Dc, reg0, exp, type );
  }
};


void InstructionFactory::insertEQ_F( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateRegister( "" );
  LLIR_Register *reg2 = CreateRegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateRegister( PHREG_D5 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::FEQ );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-float equality comparison.

  Assumed format: EQ.F D[c] (def), D[a] (use), D[b] (use)  (DDD_1)
*/
void InstructionFactory::insertEQ_F( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertEQ_F(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createDReg();
  auto &reg2 = createDReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->D5() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Da, exp, type );
  insertMOV( reg2, Db, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::FEQ ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertGE_F( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateRegister( "" );
  LLIR_Register *reg2 = CreateRegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateRegister( PHREG_D5 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::FGE );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-float greater-equal comparison.

  Assumed format: GE.F D[c] (def), D[a] (use), D[b] (use)  (DDD_1)
*/
void InstructionFactory::insertGE_F( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertGE_F(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createDReg();
  auto &reg2 = createDReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->D5() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Da, exp, type );
  insertMOV( reg2, Db, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::FGE ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertGT_F( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateRegister( "" );
  LLIR_Register *reg2 = CreateRegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateRegister( PHREG_D5 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::FGT );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-float greater-than comparison.

  Assumed format: GT.F D[c] (def), D[a] (use), D[b] (use)  (DDD_1)
*/
void InstructionFactory::insertGT_F( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertGT_F(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createDReg();
  auto &reg2 = createDReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->D5() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Da, exp, type );
  insertMOV( reg2, Db, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::FGT ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertLE_F( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateRegister( "" );
  LLIR_Register *reg2 = CreateRegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateRegister( PHREG_D5 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::FLE );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-float less-equal comparison.

  Assumed format: LE.F D[c] (def), D[a] (use), D[b] (use)  (DDD_1)
*/
void InstructionFactory::insertLE_F( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLE_F(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createDReg();
  auto &reg2 = createDReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->D5() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Da, exp, type );
  insertMOV( reg2, Db, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::FLE ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertLT_F( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateRegister( "" );
  LLIR_Register *reg2 = CreateRegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateRegister( PHREG_D5 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::FLT );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-float less-than comparison.

  Assumed format: LT.F D[c] (def), D[a] (use), D[b] (use)  (DDD_1)
*/
void InstructionFactory::insertLT_F( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLT_F(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createDReg();
  auto &reg2 = createDReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->D5() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Da, exp, type );
  insertMOV( reg2, Db, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::FLT ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertNE_F( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateRegister( "" );
  LLIR_Register *reg2 = CreateRegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateRegister( PHREG_D4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateRegister( PHREG_D5 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::FNEQ );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-float inequality comparison.

  Assumed format: NE.F D[c] (def), D[a] (use), D[b] (use)  (DDD_1)
*/
void InstructionFactory::insertNE_F( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_DRegV &Da,
                                     const WIR::TC_DRegV &Db,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertNE_F(const TC_DRegV&, const TC_DRegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createDReg();
  auto &reg2 = createDReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->D5() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Da, exp, type );
  insertMOV( reg2, Db, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::FNEQ ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertADD_D( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DADD );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateERegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-double addition.

  Assumed format: ADD.D E[c] (def), E[a] (use), E[b] (use)  (EEE)
*/
void InstructionFactory::insertADD_D( const WIR::TC_ERegV &Ec,
                                      const WIR::TC_ERegV &Ea,
                                      const WIR::TC_ERegV &Eb,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertADD_D(const TC_ERegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DADD ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertDIV_D( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DDIV );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateERegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-double division.

  Assumed format: DIV.D E[c] (def), E[a] (use), E[b] (use)  (EEE)
*/
void InstructionFactory::insertDIV_D( const WIR::TC_ERegV &Ec,
                                      const WIR::TC_ERegV &Ea,
                                      const WIR::TC_ERegV &Eb,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDIV_D(const TC_ERegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DDIV ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertEQ_D( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DEQ );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // Start new basic block after the CALL and move return value, properly
  // normalized to the logical values '0' or '1' according to the ANSI-C
  // standard.
  mCodesel.beginNewLLIRBasicBlock();
  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );

  insertNE( r0, reg0, 0, exp, type );
};


/*
  Inserts a library call for soft-double equality comparison.

  Assumed format: EQ.D D[c] (def), E[a] (use), E[b] (use)  (DEE)
*/
void InstructionFactory::insertEQ_D( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_ERegV &Ea,
                                     const WIR::TC_ERegV &Eb,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertEQ_D(const TC_DRegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DEQ ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value, properly
  // normalized to the logical values '0' or '1' according to the ANSI-C
  // standard.
  mCodesel.startNewBasicBlock();
  insertNE( Dc, reg0, 0, exp, type );
};


void InstructionFactory::insertGE_D( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DGE );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-double greater-equal comparison.

  Assumed format: GE.D D[c] (def), E[a] (use), E[b] (use)  (DEE)
*/
void InstructionFactory::insertGE_D( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_ERegV &Ea,
                                     const WIR::TC_ERegV &Eb,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertGE_D(const TC_DRegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DGE ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertGT_D( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DGT );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-double greater-than comparison.

  Assumed format: GT.D D[c] (def), E[a] (use), E[b] (use)  (DEE)
*/
void InstructionFactory::insertGT_D( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_ERegV &Ea,
                                     const WIR::TC_ERegV &Eb,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertGT_D(const TC_DRegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DGT ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertLE_D( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DLE );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-double less-equal comparison.

  Assumed format: LE.D D[c] (def), E[a] (use), E[b] (use)  (DEE)
*/
void InstructionFactory::insertLE_D( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_ERegV &Ea,
                                     const WIR::TC_ERegV &Eb,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLE_D(const TC_DRegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DLE ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertLT_D( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DLT );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-double less-than comparison.

  Assumed format: LT.D D[c] (def), E[a] (use), E[b] (use)  (DEE)
*/
void InstructionFactory::insertLT_D( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_ERegV &Ea,
                                     const WIR::TC_ERegV &Eb,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLT_D(const TC_DRegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DLT ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertMUL_D( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DMUL );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateERegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-double multiplication.

  Assumed format: MUL.D E[c] (def), E[a] (use), E[b] (use)  (EEE)
*/
void InstructionFactory::insertMUL_D( const WIR::TC_ERegV &Ec,
                                      const WIR::TC_ERegV &Ea,
                                      const WIR::TC_ERegV &Eb,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMUL_D(const TC_ERegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DMUL ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertNE_D( LLIR_Register *r0, LLIR_Register *r1,
                                     LLIR_Register *r2,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DNEQ );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-double inequality comparison.

  Assumed format: NE.D D[c] (def), E[a] (use), E[b] (use)  (DEE)
*/
void InstructionFactory::insertNE_D( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_ERegV &Ea,
                                     const WIR::TC_ERegV &Eb,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertNE_D(const TC_DRegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DNEQ ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertSUB_D( LLIR_Register *r0, LLIR_Register *r1,
                                      LLIR_Register *r2,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DSUB );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateERegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-double subtraction.

  Assumed format: SUB.D E[c] (def), E[a] (use), E[b] (use)  (EEE)
*/
void InstructionFactory::insertSUB_D( const WIR::TC_ERegV &Ec,
                                      const WIR::TC_ERegV &Ea,
                                      const WIR::TC_ERegV &Eb,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertSUB_D(const TC_ERegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DSUB ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertDIV_LL( LLIR_Register *r0, LLIR_Register *r1,
                                       LLIR_Register *r2,
                                       const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftLongLongSymbol( SoftLongLongSymbol::DIV );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateERegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-long long division.

  Assumed format: DIV.LL E[c] (def), E[a] (use), E[b] (use)  (EEE)
*/
void InstructionFactory::insertDIV_LL( const WIR::TC_ERegV &Ec,
                                       const WIR::TC_ERegV &Ea,
                                       const WIR::TC_ERegV &Eb,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDIV_LL(const TC_ERegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftLongLongFunction( SoftLongLongSymbol::DIV ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertDIV_ULL( LLIR_Register *r0, LLIR_Register *r1,
                                        LLIR_Register *r2,
                                        const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftLongLongSymbol( SoftLongLongSymbol::UDIV );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateERegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-unsigned long long division.

  Assumed format: DIV.ULL E[c] (def), E[a] (use), E[b] (use)  (EEE)
*/
void InstructionFactory::insertDIV_ULL( const WIR::TC_ERegV &Ec,
                                        const WIR::TC_ERegV &Ea,
                                        const WIR::TC_ERegV &Eb,
                                        const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDIV_ULL(const TC_ERegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftLongLongFunction( SoftLongLongSymbol::UDIV ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertMOD_LL( LLIR_Register *r0, LLIR_Register *r1,
                                       LLIR_Register *r2,
                                       const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftLongLongSymbol( SoftLongLongSymbol::MOD );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateERegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-long long modulo.

  Assumed format: MOD.LL E[c] (def), E[a] (use), E[b] (use)  (EEE)
*/
void InstructionFactory::insertMOD_LL( const WIR::TC_ERegV &Ec,
                                       const WIR::TC_ERegV &Ea,
                                       const WIR::TC_ERegV &Eb,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOD_LL(const TC_ERegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftLongLongFunction( SoftLongLongSymbol::MOD ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertMOD_ULL( LLIR_Register *r0, LLIR_Register *r1,
                                        LLIR_Register *r2,
                                        const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  LLIR_Register *reg2 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );
  insertMOV( reg2, r2 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E6 ) );
  regs.push_back( reg2 );

  string functionName = getSoftLongLongSymbol( SoftLongLongSymbol::UMOD );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateERegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for soft-unsigned long long modulo.

  Assumed format: MOD.ULL E[c] (def), E[a] (use), E[b] (use)  (EEE)
*/
void InstructionFactory::insertMOD_ULL( const WIR::TC_ERegV &Ec,
                                        const WIR::TC_ERegV &Ea,
                                        const WIR::TC_ERegV &Eb,
                                        const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOD_ULL(const TC_ERegV&, const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createEReg();
  auto &reg2 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E6() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );
  regs.push_back( reg2 );

  insertMOV( reg1, Ea, exp, type );
  insertMOV( reg2, Eb, exp, type );

  insertCALL(
    getSoftLongLongFunction( SoftLongLongSymbol::UMOD ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertDTOF( LLIR_Register *r0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DTOF );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for double to float conversion.

  Assumed format: DTOF D[c] (def), E[a] (use) (DE)
*/
void InstructionFactory::insertDTOF( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_ERegV &Ea,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDTOF(const TC_DRegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );

  insertMOV( reg1, Ea, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DTOF ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertDTOI( LLIR_Register *r0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DTOI );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for double to integer conversion.

  Assumed format: DTOI D[c] (def), E[a] (use) (DE)
*/
void InstructionFactory::insertDTOI( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_ERegV &Ea,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDTOI(const TC_DRegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );

  insertMOV( reg1, Ea, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DTOI ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertDTOLL( LLIR_Register *r0, LLIR_Register *r1,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DTOLL );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for double to long long conversion.

  Assumed format: DTOLL E[c] (def), E[a] (use) (EE)
*/
void InstructionFactory::insertDTOLL( const WIR::TC_ERegV &Ec,
                                      const WIR::TC_ERegV &Ea,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDTOLL(const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );

  insertMOV( reg1, Ea, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DTOLL ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertDTOU( LLIR_Register *r0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DTOU );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for double to unsigned integer conversion.

  Assumed format: DTOU D[c] (def), E[a] (use) (DE)
*/
void InstructionFactory::insertDTOU( const WIR::TC_DRegV &Dc,
                                     const WIR::TC_ERegV &Ea,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDTOU(const TC_DRegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );

  insertMOV( reg1, Ea, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DTOU ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertDTOULL( LLIR_Register *r0, LLIR_Register *r1,
                                       const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::DTOULL );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for double to unsigned long long conversion.

  Assumed format: DTOULL E[c] (def), E[a] (use) (EE)
*/
void InstructionFactory::insertDTOULL( const WIR::TC_ERegV &Ec,
                                       const WIR::TC_ERegV &Ea,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertDTOULL(const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );

  insertMOV( reg1, Ea, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DTOULL ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertFTOD( LLIR_Register *r0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateRegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_D4 ) );
  regs.push_back( reg1 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::FTOD );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for float to double conversion.

  Assumed format: FTOD E[c] (def), D[a] (use) (ED)
*/
void InstructionFactory::insertFTOD( const WIR::TC_ERegV &Ec,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertFTOD(const TC_ERegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createDReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );

  insertMOV( reg1, Da, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::FTOD ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertFTOLL( LLIR_Register *r0, LLIR_Register *r1,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateRegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_D4 ) );
  regs.push_back( reg1 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::FTOLL );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for float to long long conversion.

  Assumed format: FTOLL E[c] (def), D[a] (use) (ED)
*/
void InstructionFactory::insertFTOLL( const WIR::TC_ERegV &Ec,
                                      const WIR::TC_DRegV &Da,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertFTOLL(const TC_ERegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createDReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );

  insertMOV( reg1, Da, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::FTOLL ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertFTOULL( LLIR_Register *r0, LLIR_Register *r1,
                                       const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateRegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_D4 ) );
  regs.push_back( reg1 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::FTOULL );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for float to unsigned long long conversion.

  Assumed format: FTOULL E[c] (def), D[a] (use) (ED)
*/
void InstructionFactory::insertFTOULL( const WIR::TC_ERegV &Ec,
                                       const WIR::TC_DRegV &Da,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertFTOULL(const TC_ERegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createDReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );

  insertMOV( reg1, Da, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::FTOULL ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertITOD( LLIR_Register *r0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateRegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_D4 ) );
  regs.push_back( reg1 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::ITOD );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for integer to double conversion.

  Assumed format: ITOD E[c] (def), D[a] (use) (ED)
*/
void InstructionFactory::insertITOD( const WIR::TC_ERegV &Ec,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertITOD(const TC_ERegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createDReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );

  insertMOV( reg1, Da, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::ITOD ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertLLTOD( LLIR_Register *r0, LLIR_Register *r1,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::LLTOD );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for long long to double conversion.

  Assumed format: LLTOD E[c] (def), E[a] (use) (EE)
*/
void InstructionFactory::insertLLTOD( const WIR::TC_ERegV &Ec,
                                      const WIR::TC_ERegV &Ea,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLLTOD(const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );

  insertMOV( reg1, Ea, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::LLTOD ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertLLTOF( LLIR_Register *r0, LLIR_Register *r1,
                                      const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::LLTOF );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for long long to float conversion.

  Assumed format: LLTOF D[c] (def), E[a] (use) (DE)
*/
void InstructionFactory::insertLLTOF( const WIR::TC_DRegV &Dc,
                                      const WIR::TC_ERegV &Ea,
                                      const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertLLTOF(const TC_DRegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );

  insertMOV( reg1, Ea, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::LLTOF ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertULLTOD( LLIR_Register *r0, LLIR_Register *r1,
                                       const IR_Exp *exp, StmtType type )
{
  // Generate labels for internal basic blocks.
  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();

  // Generate result register.
  LLIR_Register *reg0 = CreateERegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_E2 ) );

  // Insert conditional jump.
  insertJLTZ( r1->GetNextChild( r1->GetFirstChild() ), label0, exp, type );
  mCodesel.beginNewLLIRBasicBlock();

  // Generate THEN branch.
  deque<LLIR_Register *> regs;
  LLIR_Register *reg1 = CreateERegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );

  insertMOV( reg1->GetFirstChild(), r1->GetFirstChild(), exp, type );
  insertMOV(
    reg1->GetNextChild( reg1->GetFirstChild() ),
    r1->GetNextChild( r1->GetFirstChild() ), exp, type );
  insertCALL( getSoftFloatSymbol( SoftFloatSymbol::ULLTOD ), &regs, exp, type );

  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  mCodesel.beginNewLLIRBasicBlock();
  regs.clear();
  insertJ( label1, exp, type );

  mCodesel.beginNewLLIRBasicBlock( label0.c_str() );

  // Generate ELSE branch.
  LLIR_Register *reg2 = CreateERegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( reg2, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg2 );

  insertAND( reg2->GetFirstChild(), r1->GetFirstChild(), 1, exp, type );
  insertMOV( reg2->GetNextChild( reg2->GetFirstChild() ), 0, exp, type );

  LLIR_Register *reg3 = CreateRegister( "" );
  insertDEXTR(
    reg3, r1->GetNextChild( r1->GetFirstChild() ), r1->GetFirstChild(), 31,
    exp, type );
  LLIR_Register *reg4 = CreateRegister( "" );
  insertSH( reg4, r1->GetNextChild( r1->GetFirstChild() ), -1, exp, type );
  insertOR( reg2->GetFirstChild(), reg2->GetFirstChild(), reg3, exp, type );
  insertOR(
    reg2->GetNextChild( reg2->GetFirstChild() ),
    reg2->GetNextChild( reg2->GetFirstChild() ), reg4, exp, type );
  insertCALL( getSoftFloatSymbol( SoftFloatSymbol::ULLTOD ), &regs, exp, type );

  LLIR_Register *reg5 = CreateERegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( reg5, CreateRegister( PHREG_E2 ) );
  insCall = getCurrentInstruction();
  opCall = insCall->GetFirstOp();
  implicitParam = new LLIR_Parameter( reg5, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  mCodesel.beginNewLLIRBasicBlock();
  regs.clear();

  LLIR_Register *reg6 = CreateERegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( reg6, CreateERegister( PHREG_E4 ) );
  insertMOV( reg6, reg5 );
  LLIR_Register *reg7 = CreateERegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( reg7, CreateERegister( PHREG_E6 ) );
  insertMOV( reg7, reg5 );
  regs.push_back( reg6 );
  regs.push_back( reg7 );
  insertCALL( getSoftFloatSymbol( SoftFloatSymbol::DADD ), &regs, exp, type );

  insCall = getCurrentInstruction();
  opCall = insCall->GetFirstOp();
  implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  mCodesel.beginNewLLIRBasicBlock( label1.c_str() );
  regs.clear();

  // Move the final result to the destination register.
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for unsigned long long to double conversion.

  Assumed format: ULLTOD E[c] (def), E[a] (use) (EE)
*/
void InstructionFactory::insertULLTOD( const WIR::TC_ERegV &Ec,
                                       const WIR::TC_ERegV &Ea,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertULLTOD(const TC_ERegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored target register.
  auto &reg0 = createEReg();
  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );

  // Create temporary basic blocks.
  auto *currentBB = TC179x_wirBB;
  auto &b1 = mCodesel.startNewBasicBlock();
  auto &b2 = mCodesel.startNewBasicBlock();
  auto &b3 = mCodesel.startNewBasicBlock();
  auto &b4 = mCodesel.startNewBasicBlock();
  auto &b5 = mCodesel.startNewBasicBlock();
  TC179x_wirBB = currentBB;

  list<reference_wrapper<WIR_BaseRegister>> regs;

  // Insert conditional jump.
  insertJLTZ(
    dynamic_cast<TC_DRegV &>( Ea.getChilds().back().get() ), b3, exp, type );
  TC179x_wirBB = &b1;

  // Generate THEN branch.
  auto &reg1 = createEReg();
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );
  insertMOV( reg1, Ea, exp, type );

  regs.push_back( reg1 );
  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::ULLTOD ), regs, exp, type );
  regs.clear();
  WIR_Operation &call1 = TC179x_wirBB->rbegin()->get().begin()->get();
  call1.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );
  TC179x_wirBB = &b2;

  insertJ( b5, exp, type );
  TC179x_wirBB = &b3;

  // Generate ELSE branch.
  auto &reg2 = createEReg();
  TC179x_wirFct->insertPrecolor( reg2, TC179x_wirProc->E4() );

  insertAND(
    dynamic_cast<TC_DRegV &>( reg2.getChilds().front().get() ),
    dynamic_cast<TC_DRegV &>( Ea.getChilds().front().get() ), 1, exp, type );
  insertMOV(
    dynamic_cast<TC_DRegV &>( reg2.getChilds().back().get() ), 0, exp, type );

  auto &reg3 = createDReg();
  insertDEXTR(
    reg3, dynamic_cast<TC_DRegV &>( Ea.getChilds().back().get() ),
    dynamic_cast<TC_DRegV &>( Ea.getChilds().front().get() ), 31, exp, type );

  auto &reg4 = createDReg();
  insertSH(
    reg4, dynamic_cast<TC_DRegV &>( Ea.getChilds().back().get() ), -1, exp,
    type );

  insertOR(
    dynamic_cast<TC_DRegV &>( reg2.getChilds().front().get() ),
    dynamic_cast<TC_DRegV &>( reg2.getChilds().front().get() ), reg3, exp,
    type );
  insertOR(
    dynamic_cast<TC_DRegV &>( reg2.getChilds().back().get() ),
    dynamic_cast<TC_DRegV &>( reg2.getChilds().back().get() ), reg4, exp,
    type );

  regs.push_back( reg2 );
  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::ULLTOD ), regs, exp, type );
  regs.clear();
  WIR_Operation &call2 = TC179x_wirBB->rbegin()->get().begin()->get();
  auto &reg5 = createEReg();
  TC179x_wirFct->insertPrecolor( reg5, TC179x_wirProc->E2() );
  call2.pushBackParameter(
    new WIR_RegisterParameter( reg5, WIR_Usage::def, true ) );
  TC179x_wirBB = &b4;

  auto &reg6 = createEReg();
  TC179x_wirFct->insertPrecolor( reg6, TC179x_wirProc->E4() );
  insertMOV( reg6, reg5, exp, type );
  auto &reg7 = createEReg();
  TC179x_wirFct->insertPrecolor( reg7, TC179x_wirProc->E6() );
  insertMOV( reg7, reg5, exp, type );

  regs.push_back( reg6 );
  regs.push_back( reg7 );
  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::DADD ), regs, exp, type );
  regs.clear();
  WIR_Operation &call3 = TC179x_wirBB->rbegin()->get().begin()->get();
  call3.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );
  TC179x_wirBB = &b5;

  // Move the final result to the destination register.
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertULLTOF( LLIR_Register *r0, LLIR_Register *r1,
                                       const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  LLIR_Register *reg1 = CreateERegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_E4 ) );
  regs.push_back( reg1 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::ULLTOF );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_D2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for unsigned long long to float conversion.

  Assumed format: ULLTOF D[c] (def), E[a] (use) (DE)
*/
void InstructionFactory::insertULLTOF( const WIR::TC_DRegV &Dc,
                                       const WIR::TC_ERegV &Ea,
                                       const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertULLTOF(const TC_DRegV&, const TC_ERegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createDReg();
  auto &reg1 = createEReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->D2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->E4() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );

  insertMOV( reg1, Ea, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::ULLTOF ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Dc, reg0, exp, type );
};


void InstructionFactory::insertUTOD( LLIR_Register *r0, LLIR_Register *r1,
                                     const IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateERegister( "" );
  LLIR_Register *reg1 = CreateRegister( "" );
  deque<LLIR_Register *> regs;

  insertMOV( reg1, r1 );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1, CreateERegister( PHREG_D4 ) );
  regs.push_back( reg1 );

  string functionName = getSoftFloatSymbol( SoftFloatSymbol::UTOD );

  insertCALL( functionName, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_E2 ) );
  insertMOV( r0, reg0 );
};


/*
  Inserts a library call for unsigned integer to double conversion.

  Assumed format: UTOD E[c] (def), D[a] (use) (ED)
*/
void InstructionFactory::insertUTOD( const WIR::TC_ERegV &Ec,
                                     const WIR::TC_DRegV &Da,
                                     const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertUTOD(const TC_ERegV&, const TC_DRegV&, const IR_Exp*, InstructionFactory::StmtType) const" );

  // Create pre-colored virtual registers for argument/return value passing.
  auto &reg0 = createEReg();
  auto &reg1 = createDReg();

  TC179x_wirFct->insertPrecolor( reg0, TC179x_wirProc->E2() );
  TC179x_wirFct->insertPrecolor( reg1, TC179x_wirProc->D4() );

  // Move arguments and generate the CALL.
  list<reference_wrapper<WIR_BaseRegister>> regs;
  regs.push_back( reg1 );

  insertMOV( reg1, Da, exp, type );

  insertCALL(
    getSoftFloatFunction( SoftFloatSymbol::UTOD ), regs, exp, type );

  // Add DEFined implicit parameter for function result.
  WIR_Operation &callOp = TC179x_wirBB->rbegin()->get().begin()->get();
  callOp.pushBackParameter(
    new WIR_RegisterParameter( reg0, WIR_Usage::def, true ) );

  // Start new basic block after the CALL and move return value.
  mCodesel.startNewBasicBlock();
  insertMOV( Ec, reg0, exp, type );
};


void InstructionFactory::insertMOVConstant( LLIR_Register *r0, int c0,
                                            const IR_Exp *exp, StmtType type )
{
  int bitWidth = getBitWidth( c0 );

  if ( bitWidth < 17 )
    insertMOV( r0, c0, exp, type );
  else

  if ( ( bitWidth == 17 ) && ( c0 >= 0 ) )
    insertMOV_U( r0, c0, exp, type );
  else

  if ( ( bitWidth >= 17 ) && ( bitWidth <= 33 ) )
    insertMOVH_ADDI( r0, c0, exp, type );
};


/*
  Inserts instructions to move a constant into a register.

  The exact formats depend on the constant's value.

  For getMinSignedValue( 16 ) <= c <= getMaxSignedValue( 16 ):

  MOV D[c] (def), c (DC16_1)

  else for 0 <= c <= getMaxUnsignedValue( 16 ):

  MOV.U D[c] (def), c (DC16_2)

  else

  MOVH D[c] (def), <upper 16 bits of c> (DC16_2)
  ADDI D[c] (def), D[c] (use), <lower 16 bits of c> (DDC16_1)
*/
void InstructionFactory::insertMOVConstant( const WIR::TC_DRegV &Dc, int c,
                                            const IR_Exp *exp,
                                            StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOVConstant(const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  if ( ( c >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( c <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    insertMOV( Dc, c, exp, type );
  else

  if ( ( c >= 0 ) && ( c <= (int) TC_Const16_Unsigned::getMaxValue( 16 ) ) )
    insertMOV_U( Dc, (unsigned int) c, exp, type );
  else
    insertMOVH_ADDI( Dc, c, exp, type );
};


void InstructionFactory::insertMOVConstantLL( LLIR_Register *r0,
                                              const IR_Integer *c0,
                                              const IR_Exp *exp, StmtType type )
{
  Integer low = getLowerLongLongWord( *c0 );
  Integer high = getUpperLongLongWord( *c0 );

  LLIR_Register *moveTo = r0->GetFirstChild();

  insertMOVConstant( moveTo, low, exp, type );
  moveTo = r0->GetNextChild( moveTo );
  insertMOVConstant( moveTo, high, exp, type );
};


/*
  Inserts a series of instructions to move a 64-bit constant into an extended
  data register.

  insertMOVConstant calls insertMOVConstant above for the lower and the upper
  words of the 64-bit constant.
*/
void InstructionFactory::insertMOVConstant( const WIR::TC_ERegV &Ec,
                                            const IR_Integer &c,
                                            const IR_Exp *exp,
                                            StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOVConstant(const TC_ERegV&, const IR_Integer&, const IR_Exp*, InstructionFactory::StmtType) const" );

  insertMOVConstant(
    dynamic_cast<TC_DRegV &>( Ec.begin()->get() ), getLowerLongLongWord( c ),
    exp, type );
  insertMOVConstant(
    dynamic_cast<TC_DRegV &>( Ec.rbegin()->get() ), getUpperLongLongWord( c ),
    exp, type );
};


void InstructionFactory::insertMOVH_ADDI( LLIR_Register *r0, int c0,
                                          const IR_Exp *exp, StmtType type )
{
  int low, high;

  // Extract lowest 16 bits from constant, use 2-complement representation.
  low = c0 & 0x0000FFFF;

  if ( low > maxSignedConst16Value )
    low = minSignedConst16Value + ( low - maxSignedConst16Value ) - 1;

  // Extract upper part of constant.
  high = c0 - low;

  // We don't use '>> 16' here since C does not specify whether >> performs
  // shifting with or without sign extension.
  for ( int i = 0; i < 16; ++i )
    high /= 2;

  if ( high < 0 )
    high += maxUnsignedConst16Value + 1;

  // Generate MOVH instruction.
  insertMOVH( r0, high, exp, type );

  // Generate ADDI instruction.
  if ( low != 0 )
    insertADDI( r0, r0, low, exp, type );
};


/*
  Inserts a series of MOVH and ADDI instructions.

  Exact formats:

  MOVH D[c] (def), <upper 16 bits of c> (DC16_2)
  ADDI D[c] (def), D[c] (use), <lower 16 bits of c> (DDC16_1)
*/
void InstructionFactory::insertMOVH_ADDI( const WIR::TC_DRegV &Dc, int c,
                                          const IR_Exp *exp,
                                          StmtType type ) const
{
  DSTART(
    "void InstructionFactory::insertMOVH_ADDI(const TC_DRegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  int low, high;

  // Extract lowest 16 bits from constant, use 2-complement representation.
  low = c & 0xFFFF;

  if ( low > TC_Const16_Signed::getMaxValue( 16 ) )
    low =
      TC_Const16_Signed::getMinValue( 16 ) +
      ( low - TC_Const16_Signed::getMaxValue( 16 ) ) - 1;

  // Extract upper part of constant.
  high = c - low;

  // We don't use '>> 16' here, since C does not specify whether >> performs
  // shifting with or without sign extension.
  for ( int i = 0; i < 16; ++i )
    high /= 2;

  if ( high < 0 )
    high += TC_Const16_Unsigned::getMaxValue( 16 ) + 1;

  // Generate MOVH instruction.
  insertMOVH( Dc, high, exp, type );

  // Generate ADDI instruction.
  if ( low != 0 )
    insertADDI( Dc, Dc, low, exp, type );
};


void InstructionFactory::insertInlineAsm( const IR_AsmStmt &asmStmt,
                                          const std::vector<Tricap::Argument *> &arguments )
{
  // Parse the instructions in the asm string.
  vector<LLIR_BB *> bbs;

  try {
    bbs =
      Tricap::parseASM( asmStmt.getAsmTemplate(), arguments, *LAST_LLIR_BB );
  } catch ( invalid_argument &ia ) {
    throw
      ufFatalError(
        asmStmt.getFileContext().getFilename(),
        asmStmt.getFileContext().getLine(), string( ia.what() ) );
  }

  for ( auto it( ++bbs.begin() ); it != bbs.end(); ++it ) {
    // Add back annotation mappings for new basic blocks.
    mCodesel.getBackAnnotation()->addNewMapping( *it, asmStmt.getBasicBlock() );
    // Add new blocks to the code selector.
    mCodesel.insertBB( *it );
  }

   mCurrentInstr = bbs.back()->GetLastIns();
   mCodesel.setCurrentInstruction( mCurrentInstr );

   // If activated, insert assembler debug information.
  if ( mConfig.getGenerateDebugFlag() )
    for ( auto *b : bbs )
      for ( auto *i = b->GetFirstIns(); i != nullptr; i = i->GetSucc() )
        addDebugInfo( &asmStmt, i, InstructionFactory::ASM_STMT );
};


/*
  Inserts assembly instructions for a given GNU inline assembly statement.
*/
void InstructionFactory::insertInlineAsm( const IR_AsmStmt &s,
                                          const std::vector<std::unique_ptr<WIR::TC_AsmArgument>> &args ) const
{
  DSTART(
    "void InstructionFactory::insertInlineAsm(const IR_AsmStmt&, const vector<unique_ptr<TC_AsmArgument> >&) const" );

  TC_AsmParser parser;
  parser.setGenerate16BitOperations( m16BitOperations );
  auto &bbs =
    parser.run(
      s.getAsmTemplate(), args, *TC179x_wirBB,
      s.getFileContext().getFilename() );

  // TODO: Back-annotation mapping!

  TC179x_wirBB = &(bbs.rbegin()->get());

  // Insert debug information.
  for ( WIR_BasicBlock &b : bbs )
    for ( WIR_Instruction &i : b )
      if ( !i.containsContainers( WIR_Comment::getContainerTypeID() ) )
        addDebugInfo( i, &s, InstructionFactory::ASM_STMT );
};


/*
  getSoftFloatSymbol returns a string containing the library function name to be
  called for the specified soft-float symbol.

  getSoftFloatSymbol distinguishes between WCC's internal soft-float library and
  the one from GCC.
*/
const std::string &InstructionFactory::getSoftFloatSymbol( SoftFloatSymbol sfs ) const
{
  DSTART(
    "const string& InstructionFactory::getSoftFloatSymbol(InstructionFactory::SoftFloatSymbol) const" );

  // libwcc symbols.
  static const map<SoftFloatSymbol, string> smap_libwcc
    { { SoftFloatSymbol::ADDF,   "__wcc_addsf3"      },
      { SoftFloatSymbol::DIVF,   "__wcc_divsf3"      },
      { SoftFloatSymbol::MULF,   "__wcc_mulsf3"      },
      { SoftFloatSymbol::SUBF,   "__wcc_subsf3"      },
      { SoftFloatSymbol::FEQ,    "__wcc_eqsf2"       },
      { SoftFloatSymbol::FNEQ,   "__wcc_nesf2"       },
      { SoftFloatSymbol::FLT,    "__wcc_ltsf2"       },
      { SoftFloatSymbol::FLE,    "__wcc_lesf2"       },
      { SoftFloatSymbol::FGT,    "__wcc_gtsf2"       },
      { SoftFloatSymbol::FGE,    "__wcc_gesf2"       },
      { SoftFloatSymbol::DTOF,   "__wcc_truncdfsf2"  },
      { SoftFloatSymbol::DTOI,   "__wcc_fixdfsi"     },
      { SoftFloatSymbol::DTOU,   "__wcc_fixunsdfsi"  },
      { SoftFloatSymbol::FTOD,   "__wcc_extendsfdf2" },
      { SoftFloatSymbol::FTOI,   "__wcc_fixsfsi"     },
      { SoftFloatSymbol::FTOU,   "__wcc_fixunssfsi"  },
      { SoftFloatSymbol::ITOD,   "__wcc_floatsidf"   },
      { SoftFloatSymbol::ITOF,   "__wcc_floatsisf"   },
      { SoftFloatSymbol::UTOD,   "__wcc_floatusidf"  },
      { SoftFloatSymbol::UTOF,   "__wcc_floatusisf"  },
      { SoftFloatSymbol::LLTOF,  "__wcc_floatdisf"   },
      { SoftFloatSymbol::LLTOD,  "__wcc_floatdidf"   },
      { SoftFloatSymbol::ULLTOF, "__wcc_floatdisf"   },
      { SoftFloatSymbol::ULLTOD, "__wcc_floatdidf"   },
      { SoftFloatSymbol::FTOLL,  "__wcc_fixsfdi"     },
      { SoftFloatSymbol::DTOLL,  "__wcc_fixdfdi"     },
      { SoftFloatSymbol::FTOULL, "__wcc_fixunssfdi"  },
      { SoftFloatSymbol::DTOULL, "__wcc_fixunsdfdi"  },
      { SoftFloatSymbol::DADD,   "__wcc_adddf3"      },
      { SoftFloatSymbol::DSUB,   "__wcc_subdf3"      },
      { SoftFloatSymbol::DMUL,   "__wcc_muldf3"      },
      { SoftFloatSymbol::DDIV,   "__wcc_divdf3"      },
      { SoftFloatSymbol::DEQ,    "__wcc_eqdf2"       },
      { SoftFloatSymbol::DNEQ,   "__wcc_nedf2"       },
      { SoftFloatSymbol::DLT,    "__wcc_ltdf2"       },
      { SoftFloatSymbol::DLE,    "__wcc_ledf2"       },
      { SoftFloatSymbol::DGT,    "__wcc_gtdf2"       },
      { SoftFloatSymbol::DGE,    "__wcc_gedf2"       } };

  // libgccopt symbols.
  static const map<SoftFloatSymbol, string> smap_libgccopt
    { { SoftFloatSymbol::ADDF,   "__f_add"      },
      { SoftFloatSymbol::DIVF,   "__f_div"      },
      { SoftFloatSymbol::MULF,   "__f_mul"      },
      { SoftFloatSymbol::SUBF,   "__f_sub"      },
      { SoftFloatSymbol::FEQ,    "__eqsf2"      },
      { SoftFloatSymbol::FNEQ,   "__nesf2"      },
      { SoftFloatSymbol::FLT,    "__ltsf2"      },
      { SoftFloatSymbol::FLE,    "__lesf2"      },
      { SoftFloatSymbol::FGT,    "__gtsf2"      },
      { SoftFloatSymbol::FGE,    "__gesf2"      },
      { SoftFloatSymbol::DTOF,   "__d_dtof"     },
      { SoftFloatSymbol::DTOI,   "__d_dtoi"     },
      { SoftFloatSymbol::DTOU,   "__d_dtoui"    },
      { SoftFloatSymbol::FTOD,   "__f_ftod"     },
      { SoftFloatSymbol::FTOI,   "__f_ftoi"     },
      { SoftFloatSymbol::FTOU,   "__f_ftoui"    },
      { SoftFloatSymbol::ITOD,   "__d_itod"     },
      { SoftFloatSymbol::ITOF,   "__f_itof"     },
      { SoftFloatSymbol::UTOD,   "__d_uitod"    },
      { SoftFloatSymbol::UTOF,   "__f_uitof"    },
      { SoftFloatSymbol::LLTOF,  "__floatdisf"  },
      { SoftFloatSymbol::LLTOD,  "__floatdidf"  },
      { SoftFloatSymbol::ULLTOF, "__floatdisf"  },
      { SoftFloatSymbol::ULLTOD, "__floatdidf"  },
      { SoftFloatSymbol::FTOLL,  "__fixsfdi"    },
      { SoftFloatSymbol::DTOLL,  "__fixdfdi"    },
      { SoftFloatSymbol::FTOULL, "__fixunssfdi" },
      { SoftFloatSymbol::DTOULL, "__fixunsdfdi" },
      { SoftFloatSymbol::DADD,   "__d_add"      },
      { SoftFloatSymbol::DSUB,   "__d_sub"      },
      { SoftFloatSymbol::DMUL,   "__d_mul"      },
      { SoftFloatSymbol::DDIV,   "__d_div"      },
      { SoftFloatSymbol::DEQ,    "__eqdf2"      },
      { SoftFloatSymbol::DNEQ,   "__nedf2"      },
      { SoftFloatSymbol::DLT,    "__ltdf2"      },
      { SoftFloatSymbol::DLE,    "__ledf2"      },
      { SoftFloatSymbol::DGT,    "__gtdf2"      },
      { SoftFloatSymbol::DGE,    "__gedf2"      } };

  if ( mConfig.getUseStandardSoftFloats() )
    return( smap_libgccopt.at( sfs ) );
  else
    return( smap_libwcc.at( sfs ) );
};


/*
  getSoftFloatFunction returns the WIR library function that corresponds to the
  specified soft-float symbol.

  If the WIR system for which code is currently generated does not contain the
  requested function, a novel empty, external WIR function is created and
  returned.
*/
WIR_Function &InstructionFactory::getSoftFloatFunction( SoftFloatSymbol sfs ) const
{
  DSTART(
    "WIR_Function& InstructionFactory::getSoftFloatFunction(InstructionFactory::SoftFloatSymbol) const" );

  auto &name = getSoftFloatSymbol( sfs );

  // Check whether a function with the given name already exists in the current
  // WIR system.
  WIR_System &sys = mCodesel.getSystem();
  for ( WIR_Symbol &sym : sys.getSymbols() )
    if ( sym.getType() == WIR_SymbolType::function ) {
      WIR_Function &f = sym.getFunction();

      if ( f.getName() == name )
        return( f );
    }

  // The requested function does not yet exist, we have to create it.
  // The new function will be the first one in the system's first compilation
  // unit. It will be empty (no basic blocks) and its function symbol will be
  // set to external.
  WIR_Function &f =
    sys.begin()->get().pushFrontFunction( WIR_Function( name ) );
  WIR_Symbol &sym = sys.findSymbol( f );
  sym.setExtern();
  f.setDontOptimize();

  return( f );
};


const std::string &InstructionFactory::getSoftLongLongSymbol( SoftLongLongSymbol slls ) const
{
  DSTART(
    "const string& InstructionFactory::getSoftLongLongSymbol(InstructionFactory::SoftLongLongSymbol) const" );

  // libwcc symbols.
  static const map<SoftLongLongSymbol, string> smap_libwcc
    { { SoftLongLongSymbol::DIV,   "__wcc_divdi3"  },
      { SoftLongLongSymbol::UDIV,  "__wcc_udivdi3" },
      { SoftLongLongSymbol::MOD,   "__wcc_moddi3"  },
      { SoftLongLongSymbol::UMOD,  "__wcc_umoddi3" } };

  // libgccopt symbols
  static const map<SoftLongLongSymbol, string> smap_libgccopt
    { { SoftLongLongSymbol::DIV,   "__ll_div64"  },
      { SoftLongLongSymbol::UDIV,  "__ll_udiv64" },
      { SoftLongLongSymbol::MOD,   "__ll_rem64"  },
      { SoftLongLongSymbol::UMOD,  "__ll_urem64" } };

  if ( mConfig.getUseStandardSoftFloats() )
    return( smap_libgccopt.at( slls ) );
  else
    return( smap_libwcc.at( slls ) );
};


/*
  getSoftLongLongFunction returns the WIR library function that corresponds to
  the specified soft-long long symbol.

  If the WIR system for which code is currently generated does not contain the
  requested function, a novel empty, external WIR function is created and
  returned.
*/
WIR_Function &InstructionFactory::getSoftLongLongFunction( SoftLongLongSymbol slls ) const
{
  DSTART(
    "WIR_Function& InstructionFactory::getSoftLongLongFunction(InstructionFactory::SoftLongLongSymbol) const" );

  auto &name = getSoftLongLongSymbol( slls );

  // Check whether a function with the given name already exists in the current
  // WIR system.
  WIR_System &sys = mCodesel.getSystem();
  for ( WIR_Symbol &sym : sys.getSymbols() )
    if ( sym.getType() == WIR_SymbolType::function ) {
      WIR_Function &f = sym.getFunction();

      if ( f.getName() == name )
        return( f );
    }

  // The requested function does not yet exist, we have to create it.
  // The new function will be the first one in the system's first compilation
  // unit. It will be empty (no basic blocks) and its function symbol will be
  // set to external.
  WIR_Function &f =
    sys.begin()->get().pushFrontFunction( WIR_Function( name ) );
  WIR_Symbol &sym = sys.findSymbol( f );
  sym.setExtern();
  f.setDontOptimize();

  return( f );
};


LLIR_Register *InstructionFactory::CreateRegister( std::string target,
                                                   bool isAddrReg )
{
  int regtype = 0;

  if ( isVirtual( target ) )
    regtype = VIRTUAL;
  else {
    if ( target.empty() ) {
      stringstream reg_stream;

      reg_stream << ( LLIR_Register::getVirtualRegID() );

      if ( !isAddrReg )
        target = VREG_D + reg_stream.str();
      else
        target = VREG_A + reg_stream.str();
      regtype = VIRTUAL;
    } else
      regtype = PHYSICAL;
  }

  return(
    LLIR_Register::Create( LAST_LLIR_FUNCTION, target.c_str(), regtype ) );
};


LLIR_Register *InstructionFactory::CreateERegister( std::string target,
                                                    LLIR_Register *r1,
                                                    LLIR_Register *r2 )
{
  LLIR_Register *res = nullptr;
  int regtype = 0;
  string originalTarget = target;

  // Determine information about the extended register to be created.
  if ( isVirtual( target ) )
    regtype = VIRTUAL;
  else {
    if ( target.empty() ) {
      stringstream reg_stream;

      reg_stream << ( LLIR_Register::getVirtualRegID() );

      target = VREG_E + reg_stream.str();
      regtype = VIRTUAL;
    } else
      regtype = PHYSICAL;
  }

  res = LLIR_Register::Create( LAST_LLIR_FUNCTION, target.c_str(), regtype );

  // Build register hierarchy if virtual extended register was created.
  if ( ( regtype == VIRTUAL ) && ( res->GetNumberOfChildren() == 0 ) ) {
    if ( r1 == nullptr )
      r1 = CreateRegister( "" );
    if ( r2 == nullptr )
      r2 = CreateRegister( "" );

    res->AddChild( r1 );
    res->AddChild( r2 );
  }

  return( res );
};


/*
  createAReg creates a new virtual TriCore address register.
*/
TC_ARegV &InstructionFactory::createAReg( void ) const
{
  DSTART( "TC_ARegV& InstructionFactory::createAReg() const" );

  auto *res = new TC_ARegV();
  TC179x_wirFct->pushBackVirtualRegister( res );
  return( *res );
};


/*
  createDReg creates a new virtual TriCore data register.
*/
TC_DRegV &InstructionFactory::createDReg( void ) const
{
  DSTART( "TC_DRegV& InstructionFactory::createDReg() const" );

  auto *res = new TC_DRegV();
  TC179x_wirFct->pushBackVirtualRegister( res );
  return( *res );
};


/*
  createEReg creates a new virtual TriCore extended data register.
*/
TC_ERegV &InstructionFactory::createEReg( void ) const
{
  DSTART( "TC_ERegV& InstructionFactory::createEReg() const" );

  auto *res = new TC_ERegV();
  TC179x_wirFct->pushBackVirtualRegister( res );
  return( *res );
};


/*
  createPReg creates a new virtual TriCore extended address register.
*/
TC_PRegV &InstructionFactory::createPReg( void ) const
{
  DSTART( "TC_PRegV& InstructionFactory::createPReg() const" );

  auto *res = new TC_PRegV();
  TC179x_wirFct->pushBackVirtualRegister( res );
  return( *res );
};


/*
  splitOffset splits the given address offset into its most- and least-
  significant 16-bit parts.
*/
std::pair<int, int> InstructionFactory::splitOffset( const int o )
{
  DSTART( "static pair<int, int> InstructionFactory::splitOffset(int)" );

  // Extract most-significant 16 bits from constant. This involves adding a
  // carry from bit position 16 to 17 (+ 0x8000) as well as right-shifting by
  // 16 bits (/ 0x10000).
  int hi = ( ( o + 0x8000 ) / (unsigned int) 0x10000 ) & 0xFFFF;

  // Extract least-significant 16 bits from constant.
  int lo = o & 0xFFFF;

  // Since the least-significant 16 bits are assumed to be signed and
  // represented in 2's-complement, wrap the lo-value around the 16-bits
  // boundary if it does not fit.
  if ( lo > TC_Const16_Signed::getMaxValue( 16 ) )
    lo =
      TC_Const16_Signed::getMinValue( 16 ) - 1 +
      ( lo - TC_Const16_Signed::getMaxValue( 16 ) );
  else

  if ( lo < TC_Const16_Signed::getMinValue( 16 ) )
    lo =
      TC_Const16_Signed::getMaxValue( 16 ) +
      ( lo - TC_Const16_Signed::getMinValue( 16 ) );

  return( make_pair( hi, lo ) );
};


//
// Private class methods
//

void InstructionFactory::handleLargeOffset( LLIR_Register *r0,
                                            const std::string &o0, int c0,
                                            LLIR_Register *r1 )
{
  if ( r1 == nullptr )
    r1 = r0;

  auto p = splitOffset( c0 );

  // Generate the operation (see TriCore Architecture Manual, page 5-19).
  insertADDIH_A( r0, r1, p.first );
  insertLEA( r0, o0, r0, p.second );
};


/*
  handleLargeOffset inserts instructions in order to handle address offsets
  beyond signed 16 bits.

  Exact formats:

  ADDIH.A A[c] (def), A[a] (use), <upper 16 bits of off> (AAC16)
  LEA A[c] (def), [A[c] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void InstructionFactory::handleLargeOffset( const WIR::TC_ARegV &Ac,
                                            const WIR::TC_ARegV &Aa, int off,
                                            const IR_Exp *exp, StmtType type ) const
{
  DSTART(
    "void InstructionFactory::handleLargeOffset(const TC_ARegV&, const TC_ARegV&, int, const IR_Exp*, InstructionFactory::StmtType) const" );

  auto p = splitOffset( off );

  // Generate the operation (see TriCore Architecture Manual, page 5-19).
  insertADDIH_A( Ac, Aa, p.first, exp, type );
  insertLEA( Ac, Ac, p.second, exp, type );
};


/*
  handleDataAccess adds data access meta-information to assembly instructions.

  handleDataAccess adds a LLIR pragma to the given LLIR instruction. This
  pragma carries the string content "DATA ACCESS: _objectname".
*/
void InstructionFactory::handleDataAccess( LLIR_Instruction *ins,
                                           const IR_Exp *exp, bool deref )
{
  if ( !exp || !mConfig.getParam( Configuration::RUN_WCETANALYSIS ) )
    return;

  // Skip instructions that access the stack pointer.
  LLIR_Parameter *param = ins->GetFirstOp()->GetFirstParam();
  for ( ; param; param = ins->GetFirstOp()->GetNextParam( param ) )
    if ( param->GetRegister() && isStackPointerRegister ( param ) ) {
      string pragmaString = "STACK ACCESS";

      // Take care that the pragma is created only once.
      LLIR_Pragma *p = ins->GetFirstPragma();
      while( p ) {
        if ( pragmaString.find( p->GetText() ) != string::npos )
          return;
        p = ins->GetNextPragma( p );
      }
      ins->AddPragma( new LLIR_Pragma( pragmaString.c_str(), true ) );
      return;
    }

  auto *compExp = dynamic_cast<const IR_ComponentAccessExp *>( exp );
  auto *indexExp = dynamic_cast<const IR_IndexExp *>( exp );
  auto *binExp = dynamic_cast<const IR_BinaryExp *>( exp );
  auto *unExp = dynamic_cast<const IR_UnaryExp *>( exp );
  auto *symExp = dynamic_cast<const IR_SymbolExp *>( exp );
  auto *assignExp = dynamic_cast<const IR_AssignExp *>( exp );

  // Recurse for some expression types.
  // There is no need to handle all expression types here, because only the
  // following are passed as the exp parameter. IR_BinaryExp and IR_AssignExp
  // are handled, because there is an IR optimization that generates these.
  if ( compExp ) {
    // Component access expressions that occur via pointers cannot be resolved
    // here, because we would need an alias analysis to determine the access
    // target.
    if ( !dynamic_cast<IR_PointerType *>(
           &compExp->getObjectExp().getType() ) ) {
      handleDataAccess( ins, &(compExp->getObjectExp()) );
    }
    return;
  } else

  if ( indexExp ) {
    handleDataAccess( ins, &(indexExp->getBaseExp()), true );
    return;
  } else

  if ( binExp ) {
    handleDataAccess( ins, &(binExp->getOp1()) );
    handleDataAccess( ins, &(binExp->getOp2()) );
    return;
  } else

  if ( assignExp ) {
    handleDataAccess( ins, &(assignExp->getLHS()) );
    handleDataAccess( ins, &(assignExp->getRHS()) );
    return;
  }

  // Dereference pointer if needed.
  if ( unExp && ( unExp->getOperator() == IR_UnaryExp::DEREF ) ) {
    symExp = dynamic_cast<IR_SymbolExp *>( &(unExp->getOp()) );
    deref = true;
  }

  set<IR_Symbol *> accessed;

  if ( symExp ) {
    IR_Symbol *sym = &(symExp->getSymbol());

    // Use alias analysis if available.
    #ifdef HAVE_ALIAS_ANALYSIS
    IR_AliasAnalysis * const aliases = mCodesel.getAliasAnalysis();
    if ( aliases && deref )
      accessed = aliases->getPointsTo( sym );
    #endif

    // Add the symbol itself to the set of accessed symbols.
    accessed.insert( sym );
  }

  // Iterate the set of accessed symbols and create data access pragmas for
  // global and static symbols.
  for ( IR_Symbol *sym : accessed ) {
    string name = "";

    if ( sym->getType().getStorageClass() == IR_Type::STATIC )
      name = mCodesel.getStaticName( sym );
    else

    if ( sym->isGlobal() )
      name = sym->getWrittenName();

    if ( name.size() ) {
      string pragmaString = "DATA ACCESS: " + name;

      // Take care that the pragma is created only once.
      LLIR_Pragma *p = ins->GetFirstPragma();
      while ( p ) {
        if ( pragmaString.find( p->GetText() ) != string::npos )
          return;
        p = ins->GetNextPragma( p );
      }

      ins->AddPragma( new LLIR_Pragma( pragmaString.c_str(), true ) );
    }
  }
};


/*
  handleDataAccess adds data access meta-information to assembly instructions.

  handleDataAccess adds a DataAccess container to the WIR operation inside the
  given WIR instruction.
*/
void InstructionFactory::handleDataAccess( WIR::WIR_Instruction &i,
                                           const IR_Exp *exp, bool deref ) const
{
  DSTART(
    "void InstructionFactory::handleDataAccess(WIR_Instruction&, const IR_Exp*, bool) const" );

  if ( !exp )
    return;

  // Skip instructions that access the stack pointer.
  for ( WIR_Parameter &p : i.begin()->get() )
    if ( p.getType() == WIR_ParameterType::reg )
      if ( TC13::isSP(
             dynamic_cast<WIR_RegisterParameter &>( p ).getRegister() ) )
        return;

  auto *compExp = dynamic_cast<const IR_ComponentAccessExp *>( exp );
  auto *indexExp = dynamic_cast<const IR_IndexExp *>( exp );
  auto *binExp = dynamic_cast<const IR_BinaryExp *>( exp );
  auto *unExp = dynamic_cast<const IR_UnaryExp *>( exp );
  auto *symExp = dynamic_cast<const IR_SymbolExp *>( exp );
  auto *assignExp = dynamic_cast<const IR_AssignExp *>( exp );

  // Recurse for some expression types.
  // There is no need to handle all expression types here, because only the
  // following are passed as the exp parameter. IR_BinaryExp and IR_AssignExp
  // are handled, because there is an IR optimization that generates these.
  if ( compExp ) {
    // Component access expressions that occur via pointers cannot be resolved
    // here, because we would need an alias analysis to determine the access
    // target.
    if ( !dynamic_cast<IR_PointerType *>(
           &compExp->getObjectExp().getType() ) )
      handleDataAccess( i, &(compExp->getObjectExp()) );
    return;
  } else

  if ( indexExp ) {
    handleDataAccess( i, &(indexExp->getBaseExp()), true );
    return;
  } else

  if ( binExp ) {
    handleDataAccess( i, &(binExp->getOp1()) );
    handleDataAccess( i, &(binExp->getOp2()) );
    return;
  } else

  if ( assignExp ) {
    handleDataAccess( i, &(assignExp->getLHS()) );
    handleDataAccess( i, &(assignExp->getRHS()) );
    return;
  }

  // Dereference pointer if needed.
  if ( unExp && ( unExp->getOperator() == IR_UnaryExp::DEREF ) ) {
    symExp = dynamic_cast<IR_SymbolExp *>( &(unExp->getOp()) );
    deref = true;
  }

  set<IR_Symbol *> accessed;

  if ( symExp ) {
    IR_Symbol *sym = &(symExp->getSymbol());

    // Use alias analysis if available.
    #ifdef HAVE_ALIAS_ANALYSIS
    IR_AliasAnalysis * const aliases = mCodesel.getAliasAnalysis();
    if ( aliases && deref )
      accessed = aliases->getPointsTo( sym );
    #endif

    // Add the symbol itself to the set of accessed symbols.
    accessed.insert( sym );
  }

  // Iterate the set of accessed IR symbols and collect WIR data objects.
  WIR_DataSet d;

  for ( IR_Symbol *sym : accessed ) {
    string name = "";

    if ( sym->getType().getStorageClass() == IR_Type::STATIC )
      name = mCodesel.getStaticName( sym );
    else

    if ( sym->isGlobal() )
      name = sym->getWrittenName();

    if ( name.size() ) {
      d.insert( mCodesel.getWIRData( *sym ) );
    }
  }

  if ( !d.empty() )
    // Attach a fresh data access container to the current operation.
    i.begin()->get().insertContainer(
      new WIR_DataAccess( std::move( d ) ) );
};
