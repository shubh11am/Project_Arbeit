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

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/wir.h>
#include <arch/arm/armv6.h>

// Include ICD headers
#include <arch/PROC/archinfo.h>

// Include local headers
#include "cs_arm7.h"
#include "registrar.h"
#include "instructionfactory.h"

// Include misc headers
#include <misc/misc.h>


//
// Preprocessor macros
//

// This flag determines whether float-to-int conversions
// are carried out in software or hardware.
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

extern WIR_Function *ARM7_wirFct;
extern WIR_BasicBlock *ARM7_wirBB;


//
// Public class methods
//

InstructionFactory::InstructionFactory( Configuration &config,
                                        ARM7_CodeSelector &codesel ) :
  mConfig( config ), mCodesel( codesel )
{
  mCurrentInstr = 0;
}


InstructionFactory::~InstructionFactory()
{
}

/*
  setCurrentInstr sets the reference to the instruction lastly generated
  during code selection.
*/
void InstructionFactory::setCurrentInstruction( LLIR_Instruction *i )
{
  mCurrentInstr = i;
}


/*
  getCurrentInstr retrieves the reference to the instruction lastly
  generated during code selection.
*/
LLIR_Instruction *InstructionFactory::getCurrentInstruction() const
{
  return mCurrentInstr;
}


const std::string &InstructionFactory::getSoftLongLongSymbol( SoftLongLongSymbol slls ) const
{
  DSTART( "const string& InstructionFactory::getSoftLongLongSymbol( InstructionFactory::SoftLongLongSymbol ) const" );

  // libwcc symbols.
  static const map<SoftLongLongSymbol, string> smap_libwcc
    { { SoftLongLongSymbol::LLDIV,   "__wcc_divdi3"  },
      { SoftLongLongSymbol::ULLDIV,  "__wcc_udivdi3" },
      { SoftLongLongSymbol::LLMOD,   "__wcc_moddi3"  },
      { SoftLongLongSymbol::ULLMOD,  "__wcc_umoddi3" },
      { SoftLongLongSymbol::LLSHL,   "__wcc_ashldi3" },
      { SoftLongLongSymbol::LLSHR,   "__wcc_ashrdi3" },
      { SoftLongLongSymbol::ULLSHR,   "__wcc_lshrdi3" } };

  // libgccopt symbols
  static const map<SoftLongLongSymbol, string> smap_libgccopt
    { { SoftLongLongSymbol::LLDIV,   "__divdi3"  },
      { SoftLongLongSymbol::ULLDIV,  "__udivdi3" },
      { SoftLongLongSymbol::LLMOD,   "__moddi3"  },
      { SoftLongLongSymbol::ULLMOD,  "__umoddi3" },
      { SoftLongLongSymbol::LLSHL,   "__ashldi3" },
      { SoftLongLongSymbol::LLSHR,   "__ashrdi3" },
      { SoftLongLongSymbol::ULLSHR,   "__lshrdi3" } };

  if ( mConfig.getUseStandardSoftFloats() )
    return( smap_libgccopt.at( slls ) );
  else
    return( smap_libwcc.at( slls ) );
};


LLIR_Register *InstructionFactory::CreateRegister( string target, bool )
{
  int regtype = 0;

  if ( isVirtual( target ) )
    regtype = VIRTUAL;
  else {
    if ( target.empty() ) {
      stringstream reg_stream;

      reg_stream << ( LLIR_Register::getVirtualRegID() );
      target = VREG_P + reg_stream.str();
      regtype = VIRTUAL;
    } else {
      regtype = PHYSICAL;
    }
  }

  LLIR_Register* newReg = LLIR_Register::Create( LAST_LLIR_FUNCTION,
                                                 target.c_str(), regtype );
  assert( newReg );
  return newReg;
}

LLIR_Register *InstructionFactory::CreateERegister( std::string target,
                                                    LLIR_Register *r1,
                                                    LLIR_Register *r2 )
{
  // The ARM7 does not have actually hierarchy registers.
  // Yet, we use them to indicate that the registers have to be allocated
  // together.

  LLIR_Register *res = nullptr;
  int regtype = 0;

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

/*!
  The function addDebugInfo generates debug information in the form of a LLIR
  pragma and attaches it to the first instruction. The second parameter defines
  the predecessor of the instruction to be extended by the debug information,
  the third argument defines the type of the statement.
*/
void InstructionFactory::addDebugInfo( const IR_Stmt *stm, LLIR_Instruction *ins,
                                       enum StmtType type )
{
  DSTART( "addDebugInfo( const IR_Stmt *, LLIR_Instruction *, enum StmtType )" );

  ufAssert( stm );
  unsigned int codeLine = stm->getFileContext().getLine();

  std::stringstream codeLineSStr;
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
}


/*
  addDebugInfo generates debug information in the form of a WIR comment and
  attaches it to the specified WIR instruction.
*/
void InstructionFactory::addDebugInfo( WIR::WIR_Instruction &i,
                                       const IR_Stmt *s, enum StmtType t ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

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



void InstructionFactory::insertADD( LLIR_Register *r0,  LLIR_Register *r1,
                                    LLIR_Register *r2, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insADD( OPER_AL, "", r0, r1, r2, LAST_LLIR_BB,
        mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertADD( LLIR_Register *r0,  LLIR_Register *r1,
                                    int c0, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insADD( OPER_AL, "",  r0, r1, c0, LAST_LLIR_BB,
        mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertADD( const std::string &cond,
                                    const std::string &sbit, LLIR_Register* r0,
                                    LLIR_Register* r1, LLIR_Register* r2,
                                    IR_Exp * exp, StmtType type )
{
  mCurrentInstr = insADD( cond, sbit, r0, r1, r2, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertADD( const std::string &cond,
                                    const std::string &sbit, LLIR_Register* r0,
                                    LLIR_Register* r1, int c0,
                                    IR_Exp * exp, StmtType type )
{
  mCurrentInstr = insADD( cond, sbit, r0, r1, c0, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertADD(	const string &cond, LLIR_Register * r0,
                                    LLIR_Register * r1, int c0, IR_Exp * exp,
                                    StmtType type)
{
  mCurrentInstr = insADD( cond, "", r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertADD(	const string & cond, LLIR_Register * r0,
                                    LLIR_Register * r1, LLIR_Register *r2,
                                    IR_Exp * exp, StmtType type)
{
  mCurrentInstr = insADD( cond, "", r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertADD( const string & cond, LLIR_Register * r0,
                                    LLIR_Register * r1, LLIR_Register * r2,
                                    string shift, int c0, IR_Exp * exp,
                                    StmtType type )
{
  mCurrentInstr = insADD( cond, "", r0, r1, r2, shift, c0, LAST_LLIR_BB,
                          mCurrentInstr);

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertADD( const string & cond, LLIR_Register *r0,
                                    LLIR_Register * r1, LLIR_Register * r2,
                                    string shift, LLIR_Register *r3,
                                    IR_Exp * exp, StmtType type )
{
  mCurrentInstr = insADD( cond, "", r0, r1, r2, shift, r3, LAST_LLIR_BB,
                          mCurrentInstr);

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertADC( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insADC( OPER_AL, "", r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertADC( const string &cond, LLIR_Register *r0,
                                    LLIR_Register *r1, LLIR_Register *r2,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insADC( cond, "", r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertADC( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insADC( OPER_AL, "", r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertAND( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insAND( OPER_AL, "", r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertAND( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insAND( OPER_AL, "", r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertB( const string& l0, int c0, IR_Exp *exp,
          StmtType type )
{
  mCurrentInstr = insB( l0, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertB( const string& l0, const string& l1,
          IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insB( l0, l1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertB( int c0, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insB( c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertBIC( const string& l0, const string& l1, LLIR_Register* r0, LLIR_Register* r1, LLIR_Register *r2, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insBIC( l0, l1, r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertBL( const string& l0, int c0,
                                   deque<LLIR_Register *> * regs, IR_Exp *exp,
                                   StmtType type )
{
  mCurrentInstr = insBL( l0, c0, LAST_LLIR_BB, mCurrentInstr, regs );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertBL( const string& l0, const string& l1,
                                   deque<LLIR_Register *> * regs, IR_Exp *exp,
                                   StmtType type )
{
  mCurrentInstr = insBL( l0, l1, LAST_LLIR_BB, mCurrentInstr, regs );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertBL( int c0, deque<LLIR_Register *> * regs,
                                   IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insBL( c0, LAST_LLIR_BB, mCurrentInstr, regs );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertBX( const string& l0, LLIR_Register* r0,
                                   deque<LLIR_Register *> * regs, IR_Exp *exp,
                                   StmtType type )
{
  mCurrentInstr = insBX( l0, r0, LAST_LLIR_BB, mCurrentInstr );
  ufAssert( mCurrentInstr );

  // Add implicit parameters if given.
  if ( regs ) {
    for ( deque<LLIR_Register *>::const_iterator it = regs->begin();
          it != regs->end(); ++it ) {
      LLIR_Parameter* impl = new LLIR_Parameter( *it, USAGE_USE, 1 );
      mCurrentInstr->GetFirstOp()->AddParameter( impl );
    }
  }

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertBX( LLIR_Register* r0,
                                   deque<LLIR_Register *> * regs, IR_Exp *exp,
                                   StmtType type )
{
  mCurrentInstr = insBX( r0, LAST_LLIR_BB, mCurrentInstr );
  ufAssert( mCurrentInstr );

  // Add implicit parameters if given.
  if ( regs ) {
    for ( deque<LLIR_Register *>::const_iterator it = regs->begin();
          it != regs->end(); ++it ) {
      LLIR_Parameter* impl = new LLIR_Parameter( *it, USAGE_USE, 1 );
      mCurrentInstr->GetFirstOp()->AddParameter( impl );
    }
  }

 // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertCMP( const string& l0, LLIR_Register *r0,
                                    LLIR_Register *r1, IR_Exp *exp,
                                    StmtType type )
{
  // insCMP( condition, lhsReg, rhsReg,.. )
  mCurrentInstr = insCMP( l0, r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertCMP( const string& l0, LLIR_Register *r0, int c0,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insCMP( l0, r0, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertEOR( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insEOR( OPER_AL, "", r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertEOR( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insEOR( OPER_AL, "", r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDM( const string& l0, const string& l1,
            LLIR_Register *r0, const string& l2,
            deque< LLIR_Register* > *d0, IR_Exp *exp,
            StmtType type )
{
  mCurrentInstr = insLDM( l0, l1, r0, l2, d0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}


void InstructionFactory::insertLDR( const string& l0, LLIR_Register *r0,
                                    LLIR_Register *r1, int c0, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insLDR( OPER_AL, l0, r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDR( const string& l0, LLIR_Register *r0,
                                    LLIR_Register *r1, LLIR_Register *r2,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLDR( OPER_AL, l0, r0, r1, r2, LAST_LLIR_BB, mCurrentInstr);

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDR( const string& l0, LLIR_Register *r0,
                                    LLIR_Register* r1, const string& l1,
                                    LLIR_Register* r2, IR_Exp* exp,
                                    StmtType type )
{
  mCurrentInstr = insLDR( OPER_AL, l0, r0, r1, l1, r2, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDR( const string& l0, LLIR_Register *r0,
                                    LLIR_Register *r1, LLIR_Register *r2,
                                    string l1, int c0, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insLDR( OPER_AL, l0, r0, r1, r2, l1, c0, LAST_LLIR_BB,
        mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDR( const string &l0, LLIR_Register *r0,
    LLIR_Register *r1, const string &l1, LLIR_Register *r2, string l2, int c0,
    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLDR( OPER_AL, l0, r0, r1, l1, r2, l2, c0, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDR( const string& l0, LLIR_Register* r0,
                                    bool b0, const string l1, IR_Exp* exp,
                                    StmtType type)
{
  mCurrentInstr = insLDR( l0, r0, b0, l1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}


void InstructionFactory::insertLDRB( const string& l0, LLIR_Register *r0,
                                     LLIR_Register *r1, int c0, IR_Exp *exp,
                                     StmtType type )
{
  mCurrentInstr = insLDRB( OPER_AL, l0, r0, r1, c0, LAST_LLIR_BB,
         mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDRB( const string& l0, LLIR_Register *r0,
                                     LLIR_Register *r1, LLIR_Register *r2,
                                     IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLDRB( OPER_AL, l0, r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDRB( const string& l0, LLIR_Register *r0,
                                    LLIR_Register* r1, const string& l1,
                                    LLIR_Register* r2, IR_Exp* exp,
                                    StmtType type )
{
  mCurrentInstr = insLDRB( OPER_AL, l0, r0, r1, l1, r2, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDRB( const string& l0, LLIR_Register *r0,
                                     LLIR_Register *r1, LLIR_Register *r2,
                                     string l1, int c0, IR_Exp *exp,
                                     StmtType type )
{
  mCurrentInstr = insLDRB( OPER_AL, l0, r0, r1, r2, l1, c0, LAST_LLIR_BB,
         mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDRB( const string &l0, LLIR_Register *r0,
    LLIR_Register *r1, const string &l1, LLIR_Register *r2, string l2, int c0,
    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLDRB( OPER_AL, l0, r0, r1, l1, r2, l2, c0, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDRH( const string& l0, LLIR_Register *r0,
                                     LLIR_Register *r1, int c0, IR_Exp *exp,
                                     StmtType type )
{
  mCurrentInstr = insLDRH( OPER_AL, l0, r0, r1, c0, LAST_LLIR_BB,
         mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDRH( const string& l0, LLIR_Register *r0,
                                     LLIR_Register *r1, LLIR_Register *r2,
                                     IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLDRH( OPER_AL, l0, r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDRH( const string& l0, LLIR_Register *r0,
                                    LLIR_Register* r1, const string& l1,
                                    LLIR_Register* r2, IR_Exp* exp,
                                    StmtType type )
{
  mCurrentInstr = insLDRH( OPER_AL, l0, r0, r1, l1, r2, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDRSB( const string& l0, LLIR_Register *r0,
                                      LLIR_Register *r1, int c0, IR_Exp *exp,
                                      StmtType type )
{
  mCurrentInstr = insLDRSB( OPER_AL, l0, r0, r1, c0, LAST_LLIR_BB,
          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDRSB( const string& l0, LLIR_Register *r0,
                                     LLIR_Register *r1, LLIR_Register *r2,
                                     IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLDRSB( OPER_AL, l0, r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDRSB( const string& l0, LLIR_Register *r0,
                                    LLIR_Register* r1, const string& l1,
                                    LLIR_Register* r2, IR_Exp* exp,
                                    StmtType type )
{
  mCurrentInstr = insLDRSB( OPER_AL, l0, r0, r1, l1, r2, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDRSH( const string& l0, LLIR_Register *r0,
                                      LLIR_Register *r1, int c0, IR_Exp *exp,
                                      StmtType type )
{
  mCurrentInstr = insLDRSH( OPER_AL, l0, r0, r1, c0, LAST_LLIR_BB,
          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDRSH( const string& l0, LLIR_Register *r0,
                                     LLIR_Register *r1, LLIR_Register *r2,
                                     IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insLDRSH( OPER_AL, l0, r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertLDRSH( const string& l0, LLIR_Register *r0,
                                    LLIR_Register* r1, const string& l1,
                                    LLIR_Register* r2, IR_Exp* exp,
                                    StmtType type )
{
  mCurrentInstr = insLDRSH( OPER_AL, l0, r0, r1, l1, r2, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMUL( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insMUL( OPER_AL, "", r0, r1, r2, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMUL( const string &l0,
                                    LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insMUL( l0, "", r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMLA( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, LLIR_Register *r3,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMLA( OPER_AL, "", r0, r1, r2, r3, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMLA( const string &l0,
                                    LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, LLIR_Register *r3,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMLA( l0, "", r0, r1, r2, r3, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertUMULL( LLIR_Register *low, LLIR_Register *high,
                                      LLIR_Register *r0, LLIR_Register *r1,
                                      IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insUMULL( OPER_AL, "", low, high, r0, r1, LAST_LLIR_BB,
                            mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertUMULL( const string &l0, LLIR_Register *low,
                                      LLIR_Register *high, LLIR_Register *r0,
                                      LLIR_Register *r1, IR_Exp *exp,
                                      StmtType type )
{
  mCurrentInstr = insUMULL( l0, "", low, high, r0, r1, LAST_LLIR_BB,
                            mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertUMLAL( LLIR_Register *low, LLIR_Register *high,
                                      LLIR_Register *r0, LLIR_Register *r1,
                                      IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insUMLAL( OPER_AL, "", low, high, r0, r1, LAST_LLIR_BB,
                            mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertUMLAL( const string &l0, LLIR_Register *low,
                                      LLIR_Register *high, LLIR_Register *r0,
                                      LLIR_Register *r1, IR_Exp *exp,
                                      StmtType type )
{
  mCurrentInstr = insUMLAL( l0, "", low, high, r0, r1, LAST_LLIR_BB,
                            mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSMULL( LLIR_Register *low, LLIR_Register *high,
                                      LLIR_Register *r0, LLIR_Register *r1,
                                      IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSMULL( OPER_AL, "", low, high, r0, r1, LAST_LLIR_BB,
                            mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSMULL( const string &l0, LLIR_Register *low,
                                      LLIR_Register *high, LLIR_Register *r0,
                                      LLIR_Register *r1, IR_Exp *exp,
                                      StmtType type )
{
  mCurrentInstr = insSMULL( l0, "", low, high, r0, r1, LAST_LLIR_BB,
                            mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSMLAL( LLIR_Register *low, LLIR_Register *high,
                                      LLIR_Register *r0, LLIR_Register *r1,
                                      IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSMLAL( OPER_AL, "", low, high, r0, r1, LAST_LLIR_BB,
                            mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSMLAL( const string &l0, LLIR_Register *low,
                                      LLIR_Register *high, LLIR_Register *r0,
                                      LLIR_Register *r1, IR_Exp *exp,
                                      StmtType type )
{
  mCurrentInstr = insSMLAL( l0, "", low, high, r0, r1, LAST_LLIR_BB,
                            mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}


void InstructionFactory::insertMOV( LLIR_Register *r0, int c0,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMOV( OPER_AL, "",  r0, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMOV( LLIR_Register *r0,  LLIR_Register *r1,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMOV( OPER_AL, "",  r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMOV( LLIR_Register *r0, LLIR_Register *r1,
                                    string l0, int c0, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insMOV( OPER_AL, "", r0, r1, l0, c0, LAST_LLIR_BB,
        mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMOV( const string& cond, LLIR_Register *r0,
                                    LLIR_Register *r1, const string& l0, int c0,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMOV( cond, "", r0, r1, l0, c0, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMOV( const string &cond, LLIR_Register *r0,
                                    int c0, IR_Exp *exp, StmtType type)
{
  mCurrentInstr = insMOV( cond, "",  r0, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMOV( const std::string &cond,
                                    const std::string &s, LLIR_Register* r0,
                                    LLIR_Register* r1,
                                    const std::string &shift, LLIR_Register* r2,
                                    IR_Exp * exp, StmtType type )
{
  mCurrentInstr = insMOV( cond, s,  r0, r1, shift, r2, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMOV( const std::string& cond,
  const std::string& s, LLIR_Register* r0, LLIR_Register* r1,
  const std::string& shift, int c0, IR_Exp * exp, StmtType type )
{
  mCurrentInstr = insMOV( cond, s,  r0, r1, shift, c0, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMVN( const string &cond, LLIR_Register *r0,
                                    LLIR_Register *r1, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insMVN( cond, "", r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMVN( const string &cond, LLIR_Register *r0,
                                    int c0, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insMVN( cond, "", r0, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertORR( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, IR_Exp *exp,
            StmtType type )
{
  mCurrentInstr = insORR( OPER_AL, "", r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertORR( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insORR( OPER_AL, "", r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertORR( const string& cond, LLIR_Register *r0,
                                    LLIR_Register *r1, LLIR_Register *r2,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insORR( cond, "", r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertORR( const string& cond, LLIR_Register *r0,
                                    LLIR_Register *r1, int c0, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insORR( cond, "", r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertRSB( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, IR_Exp *exp,
            StmtType type )
{
  mCurrentInstr = insRSB( OPER_AL, "", r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertRSB( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insRSB( OPER_AL, "", r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertRSB( const string& cond, LLIR_Register *r0,
                                    LLIR_Register *r1, LLIR_Register *r2,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insRSB( cond, "", r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertRSB( const string& cond, LLIR_Register *r0,
                                    LLIR_Register *r1, int c0, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insRSB( cond, "", r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertRSC( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, IR_Exp *exp,
            StmtType type )
{
  mCurrentInstr = insRSC( OPER_AL, "", r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertRSC( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insRSC( OPER_AL, "", r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}


void InstructionFactory::insertSBC( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insSBC( OPER_AL, "", r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSBC( LLIR_Register *r0, LLIR_Register *r1,
                                    int c0, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSBC( OPER_AL, "", r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSBC( const string &l0, LLIR_Register *r0,
                                    LLIR_Register *r1, LLIR_Register *r2,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSBC( l0, "", r0, r1, r2, LAST_LLIR_BB,
                          mCurrentInstr );
  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}


void InstructionFactory::insertSTM( const string& l0, const string& l1,
            LLIR_Register *r0, const string& l2,
            deque< LLIR_Register* > *d0, IR_Exp *exp,
            StmtType type )
{
  mCurrentInstr = insSTM( l0, l1, r0, l2, d0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSTR( const string& l0, const string& l1,
            LLIR_Register *r0, LLIR_Register *r1,
            int c0, IR_Exp *exp, StmtType type )
{
 mCurrentInstr = insSTR( l0, l1, r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

 // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSTR( const string &l0, const string& l1,
                                    LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSTR( l0, l1, r0, r1, r2, LAST_LLIR_BB, mCurrentInstr);

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSTR( const string& l0, const string &l1,
                                    LLIR_Register *r0,
                                    LLIR_Register* r1, const string& l2,
                                    LLIR_Register* r2, IR_Exp* exp,
                                    StmtType type )
{
  mCurrentInstr = insSTR( l0, l1, r0, r1, l2, r2, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSTR( const string& l0, const string& l1,
                                    LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, const string& l2, int c0,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSTR( l0, l1, r0, r1, r2, l2, c0, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSTR( const string &l0, const string &l1,
    LLIR_Register *r0, LLIR_Register *r1, const string &l2, LLIR_Register *r2,
    const string& l3, int c0, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSTR( l0, l1, r0, r1, l2, r2, l3, c0, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSTRB( const string &l0, const string &l1,
                                     LLIR_Register *r0, LLIR_Register *r1,
                                     int c0, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSTRB( l0, l1, r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSTRB( const string &l0, const string& l1,
                                    LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSTRB( l0, l1, r0, r1, r2, LAST_LLIR_BB, mCurrentInstr);

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSTRB( const string& l0, const string &l1,
                                    LLIR_Register *r0,
                                    LLIR_Register* r1, const string& l2,
                                    LLIR_Register* r2, IR_Exp* exp,
                                    StmtType type )
{
  mCurrentInstr = insSTRB( l0, l1, r0, r1, l2, r2, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSTRB( const string& l0, const string& l1,
                                    LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, const string& l2, int c0,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSTRB( l0, l1, r0, r1, r2, l2, c0, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSTRB( const string &l0, const string &l1,
    LLIR_Register *r0, LLIR_Register *r1, const string &l2, LLIR_Register *r2,
    const string& l3, int c0, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSTRB( l0, l1, r0, r1, l2, r2, l3, c0, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSTRH( const string &l0, const string &l1,
                                     LLIR_Register *r0, LLIR_Register *r1,
                                     int c0, IR_Exp *exp, StmtType type )
{
 mCurrentInstr = insSTRH( l0, l1, r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

 // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSTRH( const string &l0, const string& l1,
                                    LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSTRH( l0, l1, r0, r1, r2, LAST_LLIR_BB, mCurrentInstr);

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSTRH( const string& l0, const string &l1,
                                    LLIR_Register *r0,
                                    LLIR_Register* r1, const string& l2,
                                    LLIR_Register* r2, IR_Exp* exp,
                                    StmtType type )
{
  mCurrentInstr = insSTRH( l0, l1, r0, r1, l2, r2, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSUB( LLIR_Register *r0,  LLIR_Register *r1,
                                    int c0, IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insSUB( OPER_AL, "", r0, r1, c0, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSUB( LLIR_Register *r0, LLIR_Register *r1,
                                    LLIR_Register *r2, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insSUB( OPER_AL, "", r0, r1, r2, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSUB( const std::string &cond,
                                    const std::string &sbit, LLIR_Register* r0,
                                    LLIR_Register* r1, int c0,
                                    IR_Exp * exp, StmtType type )
{
  mCurrentInstr = insSUB( cond, sbit, r0, r1, c0, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSUB( const std::string &cond,
                                    const std::string &sbit, LLIR_Register* r0,
                                    LLIR_Register* r1, LLIR_Register* r2,
                                    IR_Exp * exp, StmtType type )
{
  mCurrentInstr = insSUB( cond, sbit, r0, r1, r2, LAST_LLIR_BB,
                          mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSUB(	const string & cond, LLIR_Register * r0,
                                    LLIR_Register * r1, int c0, IR_Exp * exp,
                                    StmtType type)
{
  mCurrentInstr = insSUB( cond, "", r0, r1, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSUB(	const string & cond, LLIR_Register * r0,
                                    LLIR_Register * r1, LLIR_Register *r2,
                                    IR_Exp * exp, StmtType type)
{
  mCurrentInstr = insSUB( cond, "", r0, r1, r2, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSUB( const string & cond, LLIR_Register * r0,
                                    LLIR_Register * r1, LLIR_Register * r2,
                                    string shift, int c0, IR_Exp * exp,
                                    StmtType type )
{
  mCurrentInstr = insSUB( cond, "", r0, r1, r2, shift, c0, LAST_LLIR_BB,
                          mCurrentInstr);

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSUB( const string & cond, LLIR_Register * r0,
                                    LLIR_Register * r1, LLIR_Register * r2,
                                    string shift, LLIR_Register * r3,
      IR_Exp * exp, StmtType type )
{
  mCurrentInstr = insSUB( cond, "", r0, r1, r2, shift, r3, LAST_LLIR_BB,
                          mCurrentInstr);

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertTEQ( LLIR_Register *r0, LLIR_Register *r1,
                                    IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insTEQ( OPER_AL, r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertTEQ( LLIR_Register *r0, int c0, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insTEQ( OPER_AL, r0, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertTEQ( const string &cond, LLIR_Register *r0,
                                    LLIR_Register *r1, IR_Exp *exp,
                                    StmtType type )
{
  mCurrentInstr = insTEQ( cond, r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertASMWordDir( const string& label, LLIR_BB* bb,
                                           IR_Exp* exp, StmtType type )
{
  mCurrentInstr = insASM_DIR_WORD( label, 0, bb );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMOV_ORR( LLIR_Register* reg, const int constant,
                                        IR_Exp* irExp, StmtType stmtType ) {

  // Generate the operation. We split the constant into chunks of 8 bit.
  int bitMask = 0xFF;
  for ( unsigned int windowPos = 0; windowPos < 4; ++windowPos ) {
    int curData = constant & ( bitMask << ( windowPos * 8 ) );
    // Always insert the first mov.
    if ( windowPos == 0 ) {
      // First generate a MOV.
      insertMOV( reg, curData, irExp );
    } else {
      if ( curData != 0 ) {
        // Otherwise generate OR.
        insertORR( reg, reg, curData, irExp );
      }
    }
  }
  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && irExp )
    addDebugInfo( &irExp->getStmt(), mCurrentInstr, stmtType );
}


COST InstructionFactory::insertMOV_ORR_Cost( const int constant ) {

  COST c = 0;

  // We split the constant into chunks of 8 bit.
  int bitMask = 0xFF;
  for ( unsigned int windowPos = 0; windowPos < 4; ++windowPos ) {
    int curData = constant & ( bitMask << ( windowPos * 8 ) );
    // Always insert the first mov.
    if ( windowPos == 0) {
      c += CT( INS_MOV_32 );
    } else {
      if ( curData != 0 ) {
        // Otherwise generate OR.
        c += CT( INS_ORR_32 );
      }
    }
  }

  return c;
}

void InstructionFactory::insertFloatOperation( LLIR_Register *reg,
                                               const SoftFloatSymbol &sym,
                                               IR_Exp *exp, StmtType type )
{
  LLIR_Register *reg0 = CreateRegister( "" );
  deque<LLIR_Register* > regs;

  insertMOV(reg0,reg,exp,type);
  LAST_LLIR_FUNCTION->AddPrecolor( reg0, CreateRegister( PHREG_R0 ) );
  regs.push_back( reg0 );
  string functionName = getSoftFloatSymbol( sym );

  insertBL( OPER_AL,functionName, &regs, exp, type );
  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( reg0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();
  insertMOV( reg, reg0, exp );
}

ARM7_CodeSelector *InstructionFactory::getCodeSelector()
{
  return &mCodesel;
};

void InstructionFactory::handleDataAccess( LLIR_Instruction *ins, IR_Exp *exp,
                                           bool )
{
  if ( !exp || !mConfig.getParam( Configuration::RUN_WCETANALYSIS ) )
    return;

  //Skip instruction that access the stack pointer
  LLIR_Parameter* param = ins->GetFirstOp()->GetFirstParam();
  for ( ; param; param = ins->GetFirstOp()->GetNextParam( param ) )
    if ( param->GetRegister() && isStackPointerRegister ( param ) ) {
      string pragmaString = "STACK ACCESS";

      //take care that the pragma is created only once
      LLIR_Pragma *p = ins->GetFirstPragma();
      while( p ) {
        if ( pragmaString.find(p->GetText() ) != std::string::npos )
          return;
        p = ins->GetNextPragma(p);
      }
      ins->AddPragma( new LLIR_Pragma( pragmaString.c_str(), true ) );
      return;
    }

  IR_ComponentAccessExp *compExp = dynamic_cast<IR_ComponentAccessExp*>(exp);
  IR_IndexExp  *indexExp  = dynamic_cast<IR_IndexExp*>(exp);
  IR_BinaryExp *binExp    = dynamic_cast<IR_BinaryExp*>(exp);
  IR_UnaryExp  *unExp     = dynamic_cast<IR_UnaryExp*>(exp);
  IR_SymbolExp *symExp    = dynamic_cast<IR_SymbolExp*>(exp);
  IR_AssignExp *assignExp = dynamic_cast<IR_AssignExp*>(exp);

  //recurse for some expression types
  //there is no need to handle all expression types
  //here, because only the following are passed as the exp parameter
  //IR_BinaryExp and IR_AssignExp are handled because there is a
  //IR optimization that generates these
  if (compExp) {
    // Component access expressions that occur via pointers cannot be resolved
    // here, because we would need an alias analysis to determine the access
    // target.
    if ( !dynamic_cast<IR_PointerType*>( &compExp->getObjectExp().getType() ) ) {
      handleDataAccess( ins, &(compExp->getObjectExp()) );
    }
    return;
  } else if (indexExp) {
    handleDataAccess( ins, &(indexExp->getBaseExp()), true );
    return;
  } else if (binExp) {
    handleDataAccess( ins, &(binExp->getOp1()) );
    handleDataAccess( ins, &(binExp->getOp2()) );
    return;
  } else if (assignExp) {
    handleDataAccess( ins, &(assignExp->getLHS()) );
    handleDataAccess( ins, &(assignExp->getRHS()) );
    return;
  }

  //dereference pointer if needed
  if (unExp && unExp->getOperator() == IR_UnaryExp::DEREF) {
    symExp = dynamic_cast<IR_SymbolExp*>( &(unExp->getOp()) );
    //deref = true;
  }

  std::set<IR_Symbol*> accessed;

  if (symExp) {
    IR_Symbol *sym = &(symExp->getSymbol());


    //Add the symbol itself to the set of accessed symbols
    accessed.insert(sym);
  }

  //Iterate the set of accessed symbols and create data access pragma
  //for global and static symbols.
  std::set<IR_Symbol*>::iterator sit = accessed.begin();
  for (; sit != accessed.end(); ++sit) {
    IR_Symbol *sym = (*sit);
    string name = "";
    if ( sym->getType().getStorageClass() == IR_Type::STATIC )
      name = mCodesel.getStaticName( sym );
    else if ( sym->isGlobal() ) {
      name = sym->getWrittenName();
    }
    if ( name.size() ) {
      string pragmaString = "DATA ACCESS: " + name;

      //take care that the pragma is created only once
      LLIR_Pragma *p = ins->GetFirstPragma();
      while (p) {
        if ( pragmaString.find(p->GetText() ) != std::string::npos )
          return;
        p = ins->GetNextPragma(p);
      }
      ins->AddPragma( new LLIR_Pragma( pragmaString.c_str(), true ) );
    }
  }
}
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
      { SoftFloatSymbol::DGE,    "__wcc_gedf2"       },
      { SoftFloatSymbol::SDIVSI,  "__wcc_divsi3"     },
      { SoftFloatSymbol::UDIVSI, "__wcc_udivsi3"     } };

  // libgccopt symbols.
  static const map<SoftFloatSymbol, string> smap_libgccopt
    { { SoftFloatSymbol::ADDF,   "__addsf3"     },
      { SoftFloatSymbol::DIVF,   "__divsf3"     },
      { SoftFloatSymbol::MULF,   "__mulsf3"     },
      { SoftFloatSymbol::SUBF,   "__subsf3"     },
      { SoftFloatSymbol::FEQ,    "__eqsf2"      },
      { SoftFloatSymbol::FNEQ,   "__nesf2"      },
      { SoftFloatSymbol::FLT,    "__ltsf2"      },
      { SoftFloatSymbol::FLE,    "__lesf2"      },
      { SoftFloatSymbol::FGT,    "__gtsf2"      },
      { SoftFloatSymbol::FGE,    "__gesf2"      },
      { SoftFloatSymbol::DTOF,   "__truncdfsf2" },
      { SoftFloatSymbol::DTOI,   "__fixdfsi"    },
      { SoftFloatSymbol::DTOU,   "__fixunsdfsi" },
      { SoftFloatSymbol::FTOD,   "__extendsfdf2"},
      { SoftFloatSymbol::FTOI,   "__fixsfsi"    },
      { SoftFloatSymbol::FTOU,   "__fixunssfsi" },
      { SoftFloatSymbol::ITOD,   "__floatsidf"  },
      { SoftFloatSymbol::ITOF,   "__floatsisf"  },
      { SoftFloatSymbol::UTOD,   "__floatunsidf"},
      { SoftFloatSymbol::UTOF,   "__floatunsisf"},
      { SoftFloatSymbol::LLTOF,  "__floatdisf"  },
      { SoftFloatSymbol::LLTOD,  "__floatdidf"  },
      { SoftFloatSymbol::ULLTOF, "__floatundisf"},
      { SoftFloatSymbol::ULLTOD, "__floatundidf"},
      { SoftFloatSymbol::FTOLL,  "__fixsfdi"    },
      { SoftFloatSymbol::DTOLL,  "__fixdfdi"    },
      { SoftFloatSymbol::FTOULL, "__fixunssfdi" },
      { SoftFloatSymbol::DTOULL, "__fixunsdfdi" },
      { SoftFloatSymbol::DADD,   "__adddf3"     },
      { SoftFloatSymbol::DSUB,   "__subdf3"     },
      { SoftFloatSymbol::DMUL,   "__muldf3"     },
      { SoftFloatSymbol::DDIV,   "__divdf3"     },
      { SoftFloatSymbol::DEQ,    "__eqdf2"      },
      { SoftFloatSymbol::DNEQ,   "__nedf2"      },
      { SoftFloatSymbol::DLT,    "__ltdf2"      },
      { SoftFloatSymbol::DLE,    "__ledf2"      },
      { SoftFloatSymbol::DGT,    "__gtdf2"      },
      { SoftFloatSymbol::DGE,    "__gedf2"      },
      { SoftFloatSymbol::SDIVSI,  "__divsi3"     },
      { SoftFloatSymbol::UDIVSI, "__udivsi3"    } };

  if ( mConfig.getUseStandardSoftFloats() )
    return( smap_libgccopt.at( sfs ) );
  else
    return( smap_libwcc.at( sfs ) );
};

WIR_Function &InstructionFactory::getSoftFloatFunction( SoftFloatSymbol sfs ) const
{
  DSTART(
    "LLIR_Function& InstructionFactory::getSoftFloatFunction(InstructionFactory::SoftFloatSymbol) const" );

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

void InstructionFactory::insertADD_F( const string& cond, LLIR_Register* result,
  LLIR_Register* reg0, LLIR_Register* reg1, IR_Exp* exp, StmtType type ) {

  DSTART( "InstructionFactory::insertADD_F" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register* p0 = CreateRegister( "" );
  LLIR_Register* p1 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  mCurrentInstr = insMOV( cond, "", p0, reg0, LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1, reg1, LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( p0 );
  regs.push_back( p1 );
  const string& label = getSoftFloatSymbol( SoftFloatSymbol::ADDF );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( p0, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB, mCurrentInstr );
}

void InstructionFactory::insertADD_D( const string &cond,
                                      LLIR_Register *eresult,
                                      LLIR_Register *ereg0,
                                      LLIR_Register *ereg1, IR_Exp *exp,
                                      StmtType type ) {

  DSTART( "InstructionFactory::insertADD_D" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *p0 = CreateRegister( "" );
  LLIR_Register *p1 = CreateRegister( "" );
  LLIR_Register *p2 = CreateRegister( "" );
  LLIR_Register *p3 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p2, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p3, CreateRegister( PHREG_R3 ) );
  mCurrentInstr = insMOV( cond, "", p0, ereg0->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p2, ereg1->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p3,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );
  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( p0 );
  eregs.push_back( p1 );
  eregs.push_back( p2 );
  eregs.push_back( p3 );
  const string &label = getSoftFloatSymbol( SoftFloatSymbol::DADD );
  insertBL( cond, label, &eregs, exp, type );

  // Set DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( p0, USAGE_DEF, 1 );
  LLIR_Parameter *implicitParam1 = new LLIR_Parameter( p1, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );
  opCall->AddParameter( implicitParam1 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", eresult->GetFirstChild(), p0, LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "",
                          eresult->GetNextChild( eresult->GetFirstChild()), p1,
                          LAST_LLIR_BB, mCurrentInstr );
}

void InstructionFactory::insertADD_LL( const string &cond,
                                       LLIR_Register *eresult,
                                       LLIR_Register *ereg0,
                                       LLIR_Register *ereg1, IR_Exp *exp,
                                       StmtType type ) {

  DSTART( "InstructionFactory::insertADD_LL" );

  // Add first children (least significant) of ereg0 and ereg1 and set S-bit to
  // update condition flags.
  insertADD( cond, OPER_SBIT, eresult->GetFirstChild(), ereg0->GetFirstChild(),
             ereg1->GetFirstChild(), exp, type );
  // Add second children (most significant) of ereg0 and ereg1 and carry.
  insertADC( cond, eresult->GetNextChild( eresult->GetFirstChild() ),
             ereg0->GetNextChild( ereg0->GetFirstChild() ),
             ereg1->GetNextChild( ereg1->GetFirstChild() ), exp, type );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSUB_F( const string& cond, LLIR_Register* result,
  LLIR_Register* reg0, LLIR_Register* reg1, IR_Exp* exp, StmtType type ) {

  DSTART( "InstructionFactory::insertSUB_F" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register* p0 = CreateRegister( "" );
  LLIR_Register* p1 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  mCurrentInstr = insMOV( cond, "", p0, reg0, LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1, reg1, LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( p0 );
  regs.push_back( p1 );
  const string& label = getSoftFloatSymbol( SoftFloatSymbol::SUBF );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( p0, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB, mCurrentInstr );
}

void InstructionFactory::insertSUB_D( const string &cond,
                                      LLIR_Register *eresult,
                                      LLIR_Register *ereg0,
                                      LLIR_Register *ereg1,
                                      IR_Exp *exp, StmtType type ) {

  DSTART( "InstructionFactory::insertSUB_D" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *p0 = CreateRegister( "" );
  LLIR_Register *p1 = CreateRegister( "" );
  LLIR_Register *p2 = CreateRegister( "" );
  LLIR_Register *p3 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p2, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p3, CreateRegister( PHREG_R3 ) );
  mCurrentInstr = insMOV( cond, "", p0, ereg0->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p2, ereg1->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p3,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( p0 );
  eregs.push_back( p1 );
  eregs.push_back( p2 );
  eregs.push_back( p3 );
  const string &label = getSoftFloatSymbol( SoftFloatSymbol::DSUB );
  insertBL( cond, label, &eregs, exp, type );

  // Set DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( p0, USAGE_DEF, 1 );
  LLIR_Parameter *implicitParam1 = new LLIR_Parameter( p1, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );
  opCall->AddParameter( implicitParam1 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", eresult->GetFirstChild(), p0, LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", eresult->GetNextChild(
                          eresult->GetFirstChild()), p1, LAST_LLIR_BB,
                          mCurrentInstr );
}

void InstructionFactory::insertSUB_LL( const string &cond,
                                       LLIR_Register *eresult,
                                       LLIR_Register *ereg0,
                                       LLIR_Register *ereg1, IR_Exp *exp,
                                       StmtType type ) {

  DSTART( "InstructionFactory::insertSUB_LL" );

  // Subtract first children (least significant) of ereg0 and ereg1 and set
  // S-bit to update condition flags.
  insertSUB( cond, OPER_SBIT, eresult->GetFirstChild(), ereg0->GetFirstChild(),
             ereg1->GetFirstChild(), exp, type );
  // Subtract second children (most significant) of ereg0 and ereg1 and carry.
  insertSBC( cond, eresult->GetNextChild( eresult->GetFirstChild() ),
             ereg0->GetNextChild( ereg0->GetFirstChild() ),
             ereg1->GetNextChild( ereg1->GetFirstChild() ), exp, type );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMUL_F( const string& cond, LLIR_Register* result,
  LLIR_Register* reg0, LLIR_Register* reg1, IR_Exp* exp, StmtType type ) {

  DSTART( "InstructionFactory::insertSUB_F" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register* p0 = CreateRegister( "" );
  LLIR_Register* p1 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  mCurrentInstr = insMOV( cond, "", p0, reg0, LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1, reg1, LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( p0 );
  regs.push_back( p1 );
  const string& label = getSoftFloatSymbol( SoftFloatSymbol::MULF );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( p0, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB, mCurrentInstr );
}

void InstructionFactory::insertMUL_D( const string &cond,
                                      LLIR_Register *eresult,
                                      LLIR_Register *ereg0,
                                      LLIR_Register *ereg1, IR_Exp *exp,
                                      StmtType type ) {

  DSTART( "InstructionFactory::insertMUL_D" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *p0 = CreateRegister( "" );
  LLIR_Register *p1 = CreateRegister( "" );
  LLIR_Register *p2 = CreateRegister( "" );
  LLIR_Register *p3 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p2, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p3, CreateRegister( PHREG_R3 ) );
  mCurrentInstr = insMOV( cond, "", p0, ereg0->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p2, ereg1->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p3,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( p0 );
  eregs.push_back( p1 );
  eregs.push_back( p2 );
  eregs.push_back( p3 );
  const string &label = getSoftFloatSymbol( SoftFloatSymbol::DMUL );
  insertBL( cond, label, &eregs, exp, type );

  // Set DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( p0, USAGE_DEF, 1 );
  LLIR_Parameter *implicitParam1 = new LLIR_Parameter( p1, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );
  opCall->AddParameter( implicitParam1 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", eresult->GetFirstChild(), p0, LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", eresult->GetNextChild(
                          eresult->GetFirstChild()), p1, LAST_LLIR_BB,
                          mCurrentInstr );
}

void InstructionFactory::insertMUL_LL( const string &cond,
                                       LLIR_Register *eresult,
                                       LLIR_Register *lhsEReg,
                                       LLIR_Register *rhsEReg, IR_Exp *exp,
                                       StmtType type ) {

  DSTART( "InstructionFactory::insertMUL_LL" );

  // Sanity checks.
  ufAssert( lhsEReg->GetNumberOfChildren() == 2 );
  ufAssert( rhsEReg->GetNumberOfChildren() == 2 );
  ufAssert( eresult->GetNumberOfChildren() == 2 );

  // Create extended result register.
  LLIR_Register *eres = CreateERegister( "" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *ereg0LSB = CreateRegister( "" );
  LLIR_Register *ereg0MSB = CreateRegister( "" );
  LLIR_Register *ereg1LSB = CreateRegister( "" );
  LLIR_Register *ereg1MSB = CreateRegister( "" );

  mCurrentInstr = insMOV( cond, "", ereg0LSB, lhsEReg->GetFirstChild(),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg0MSB,
                          lhsEReg->GetNextChild( lhsEReg->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg1LSB, rhsEReg->GetFirstChild(),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg1MSB,
                          rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );

  // Multiply and accumulate first-high and second-low and first-low and
  // second-high child registers.
  insertMUL( cond, eres->GetNextChild( eres->GetFirstChild() ), ereg0LSB,
             ereg1MSB, exp );
  insertMLA( cond, eres->GetNextChild( eres->GetFirstChild() ), ereg0MSB,
             ereg1LSB, eres->GetNextChild( eres->GetFirstChild() ), exp );
  // Multiply least significant children and add the result. First-high *
  // second-high is omitted since the result doesn't contribute to the 64
  // least significant bits.
  LLIR_Register *tpmHigh = CreateRegister( "" );
  insertUMULL( cond, eres->GetFirstChild(), tpmHigh, ereg0LSB, ereg1LSB,
               exp );
  insertADD( cond, eres->GetNextChild( eres->GetFirstChild() ),
             eres->GetNextChild( eres->GetFirstChild() ), tpmHigh );

  // And move the result.
  mCurrentInstr = insMOV( cond, "", eresult->GetFirstChild(),
                          eres->GetFirstChild(), LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "",
                          eresult->GetNextChild( eresult->GetFirstChild()),
                          eres->GetNextChild( eres->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertDIV_F( const string& cond, LLIR_Register* result,
  LLIR_Register* reg0, LLIR_Register* reg1, IR_Exp* exp, StmtType type ) {

  DSTART( "InstructionFactory::insertDIV_F" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register* p0 = CreateRegister( "" );
  LLIR_Register* p1 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  mCurrentInstr = insMOV( cond, "", p0, reg0, LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1, reg1, LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( p0 );
  regs.push_back( p1 );
  const string& label = getSoftFloatSymbol( SoftFloatSymbol::DIVF );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( p0, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB, mCurrentInstr );
}

void InstructionFactory::insertDIV_D( const string &cond,
                                      LLIR_Register *eresult,
                                      LLIR_Register *ereg0,
                                      LLIR_Register *ereg1,
                                      IR_Exp *exp,
                                      StmtType type ) {

  DSTART( "InstructionFactory::insertDIV_D" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *p0 = CreateRegister( "" );
  LLIR_Register *p1 = CreateRegister( "" );
  LLIR_Register *p2 = CreateRegister( "" );
  LLIR_Register *p3 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p2, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p3, CreateRegister( PHREG_R3 ) );
  mCurrentInstr = insMOV( cond, "", p0, ereg0->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p2, ereg1->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p3,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( p0 );
  eregs.push_back( p1 );
  eregs.push_back( p2 );
  eregs.push_back( p3 );
  const string &label = getSoftFloatSymbol( SoftFloatSymbol::DDIV );
  insertBL( cond, label, &eregs, exp, type );

 // Set DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( p0, USAGE_DEF, 1 );
  LLIR_Parameter *implicitParam1 = new LLIR_Parameter( p1, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );
  opCall->AddParameter( implicitParam1 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", eresult->GetFirstChild(), p0, LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", eresult->GetNextChild(
                          eresult->GetFirstChild()), p1, LAST_LLIR_BB,
                          mCurrentInstr );
}

void InstructionFactory::insertDIV_LL( const string &cond,
                                       LLIR_Register *eresult,
                                       LLIR_Register *ereg0,
                                       LLIR_Register *ereg1, IR_Exp *exp,
                                       StmtType type ) {

  DSTART( "InstructionFactory::insertDIV_LL" );

  // Sanity checks.
  ufAssert( ereg0 );
  ufAssert( ereg1 );
  ufAssert( eresult );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *ereg0LSB = CreateRegister( "" );
  LLIR_Register *ereg0MSB = CreateRegister( "" );
  LLIR_Register *ereg1LSB = CreateRegister( "" );
  LLIR_Register *ereg1MSB = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( ereg0LSB, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg0MSB, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg1LSB, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg1MSB, CreateRegister( PHREG_R3 ) );

  mCurrentInstr = insMOV( cond, "", ereg0LSB, ereg0->GetFirstChild(),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg0MSB,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg1LSB, ereg1->GetFirstChild(),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg1MSB,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( ereg0LSB );
  eregs.push_back( ereg0MSB );
  eregs.push_back( ereg1LSB );
  eregs.push_back( ereg1MSB );

  const string &label = getSoftLongLongSymbol( SoftLongLongSymbol::LLDIV );
  insertBL( cond, label, &eregs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( ereg0LSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );
  implicitParam = new LLIR_Parameter( ereg0MSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", eresult->GetFirstChild(), ereg0LSB,
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "",
                          eresult->GetNextChild( eresult->GetFirstChild()),
                          ereg0MSB, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertDIV_ULL( const string &cond,
                                        LLIR_Register *eresult,
                                        LLIR_Register *ereg0,
                                        LLIR_Register *ereg1, IR_Exp *exp,
                                        StmtType type ) {

  DSTART( "InstructionFactory::insertDIV_ULL" );

  // Sanity checks.
  ufAssert( ereg0 );
  ufAssert( ereg1 );
  ufAssert( eresult );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *ereg0LSB = CreateRegister( "" );
  LLIR_Register *ereg0MSB = CreateRegister( "" );
  LLIR_Register *ereg1LSB = CreateRegister( "" );
  LLIR_Register *ereg1MSB = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( ereg0LSB, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg0MSB, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg1LSB, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg1MSB, CreateRegister( PHREG_R3 ) );

  mCurrentInstr = insMOV( cond, "", ereg0LSB, ereg0->GetFirstChild(),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg0MSB,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg1LSB, ereg1->GetFirstChild(),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg1MSB,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( ereg0LSB );
  eregs.push_back( ereg0MSB );
  eregs.push_back( ereg1LSB );
  eregs.push_back( ereg1MSB );

  const string &label = getSoftLongLongSymbol( SoftLongLongSymbol::ULLDIV );
  insertBL( cond, label, &eregs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( ereg0LSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );
  implicitParam = new LLIR_Parameter( ereg0MSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", eresult->GetFirstChild(), ereg0LSB,
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "",
                          eresult->GetNextChild( eresult->GetFirstChild()),
                          ereg0MSB, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSHL_LL( const string &cond,
                                       LLIR_Register *eresult,
                                       LLIR_Register *ereg0,
                                       LLIR_Register *reg1, IR_Exp *exp,
                                       StmtType type ) {

  DSTART( "InstructionFactory::insertSHL_LL" );

  // Sanity checks.
  ufAssert( ereg0 );
  ufAssert( reg1 );
  ufAssert( eresult );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *ereg0LSB = CreateRegister( "" );
  LLIR_Register *ereg0MSB = CreateRegister( "" );
  LLIR_Register *reg1LSB = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( ereg0LSB, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg0MSB, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1LSB, CreateRegister( PHREG_R2 ) );

  mCurrentInstr = insMOV( cond, "", ereg0LSB, ereg0->GetFirstChild(),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg0MSB,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", reg1LSB, reg1, LAST_LLIR_BB,
                          mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( ereg0LSB );
  regs.push_back( ereg0MSB );
  regs.push_back( reg1LSB );

  const string &label = getSoftLongLongSymbol( SoftLongLongSymbol::LLSHL );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( ereg0LSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );
  implicitParam = new LLIR_Parameter( ereg0MSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", eresult->GetFirstChild(), ereg0LSB,
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "",
                          eresult->GetNextChild( eresult->GetFirstChild()),
                          ereg0MSB, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSHR_LL( const string &cond,
                                       LLIR_Register *eresult,
                                       LLIR_Register *ereg0,
                                       LLIR_Register *reg1, IR_Exp *exp,
                                       StmtType type ) {

  DSTART( "InstructionFactory::insertSHR_LL" );

  // Sanity checks.
  ufAssert( ereg0 );
  ufAssert( reg1 );
  ufAssert( eresult );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *ereg0LSB = CreateRegister( "" );
  LLIR_Register *ereg0MSB = CreateRegister( "" );
  LLIR_Register *reg1LSB = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( ereg0LSB, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg0MSB, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1LSB, CreateRegister( PHREG_R2 ) );

  mCurrentInstr = insMOV( cond, "", ereg0LSB, ereg0->GetFirstChild(),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg0MSB,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", reg1LSB, reg1, LAST_LLIR_BB,
                          mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( ereg0LSB );
  regs.push_back( ereg0MSB );
  regs.push_back( reg1LSB );

  const string &label = getSoftLongLongSymbol( SoftLongLongSymbol::LLSHR );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( ereg0LSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );
  implicitParam = new LLIR_Parameter( ereg0MSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", eresult->GetFirstChild(), ereg0LSB,
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "",
                          eresult->GetNextChild( eresult->GetFirstChild()),
                          ereg0MSB, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSHR_ULL( const string &cond,
                                        LLIR_Register *eresult,
                                        LLIR_Register *ereg0,
                                        LLIR_Register *reg1, IR_Exp *exp,
                                        StmtType type ) {

  DSTART( "InstructionFactory::insertSHR_ULL" );

  // Sanity checks.
  ufAssert( ereg0 );
  ufAssert( reg1 );
  ufAssert( eresult );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *ereg0LSB = CreateRegister( "" );
  LLIR_Register *ereg0MSB = CreateRegister( "" );
  LLIR_Register *reg1LSB = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( ereg0LSB, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg0MSB, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( reg1LSB, CreateRegister( PHREG_R2 ) );

  mCurrentInstr = insMOV( cond, "", ereg0LSB, ereg0->GetFirstChild(),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg0MSB,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", reg1LSB, reg1, LAST_LLIR_BB,
                          mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( ereg0LSB );
  regs.push_back( ereg0MSB );
  regs.push_back( reg1LSB );

  const string &label = getSoftLongLongSymbol( SoftLongLongSymbol::ULLSHR );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( ereg0LSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );
  implicitParam = new LLIR_Parameter( ereg0MSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", eresult->GetFirstChild(), ereg0LSB,
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "",
                          eresult->GetNextChild( eresult->GetFirstChild()),
                          ereg0MSB, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMOD_LL( const string &cond,
                                       LLIR_Register *eresult,
                                       LLIR_Register *ereg0,
                                       LLIR_Register *ereg1, IR_Exp *exp,
                                       StmtType type ) {

  DSTART( "InstructionFactory::insertMOD_LL" );

  // Sanity checks.
  ufAssert( ereg0 );
  ufAssert( ereg1 );
  ufAssert( eresult );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *ereg0LSB = CreateRegister( "" );
  LLIR_Register *ereg0MSB = CreateRegister( "" );
  LLIR_Register *ereg1LSB = CreateRegister( "" );
  LLIR_Register *ereg1MSB = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( ereg0LSB, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg0MSB, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg1LSB, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg1MSB, CreateRegister( PHREG_R3 ) );

  mCurrentInstr = insMOV( cond, "", ereg0LSB, ereg0->GetFirstChild(),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg0MSB,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg1LSB, ereg1->GetFirstChild(),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg1MSB,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( ereg0LSB );
  eregs.push_back( ereg0MSB );
  eregs.push_back( ereg1LSB );
  eregs.push_back( ereg1MSB );

  const string &label = getSoftLongLongSymbol( SoftLongLongSymbol::LLMOD );
  insertBL( cond, label, &eregs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( ereg0LSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );
  implicitParam = new LLIR_Parameter( ereg0MSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", eresult->GetFirstChild(), ereg0LSB,
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "",
                          eresult->GetNextChild( eresult->GetFirstChild()),
                          ereg0MSB, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertMOD_ULL( const string &cond,
                                        LLIR_Register *eresult,
                                        LLIR_Register *ereg0,
                                        LLIR_Register *ereg1, IR_Exp *exp,
                                        StmtType type ) {

  DSTART( "InstructionFactory::insertMOD_ULL" );

  // Sanity checks.
  ufAssert( ereg0 );
  ufAssert( ereg1 );
  ufAssert( eresult );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *ereg0LSB = CreateRegister( "" );
  LLIR_Register *ereg0MSB = CreateRegister( "" );
  LLIR_Register *ereg1LSB = CreateRegister( "" );
  LLIR_Register *ereg1MSB = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( ereg0LSB, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg0MSB, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg1LSB, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( ereg1MSB, CreateRegister( PHREG_R3 ) );

  mCurrentInstr = insMOV( cond, "", ereg0LSB, ereg0->GetFirstChild(),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg0MSB,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg1LSB, ereg1->GetFirstChild(),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", ereg1MSB,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( ereg0LSB );
  eregs.push_back( ereg0MSB );
  eregs.push_back( ereg1LSB );
  eregs.push_back( ereg1MSB );

  const string &label = getSoftLongLongSymbol( SoftLongLongSymbol::ULLMOD );
  insertBL( cond, label, &eregs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( ereg0LSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );
  implicitParam = new LLIR_Parameter( ereg0MSB, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", eresult->GetFirstChild(), ereg0LSB,
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "",
                          eresult->GetNextChild( eresult->GetFirstChild()),
                          ereg0MSB, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertSDIVSI( const string& cond, LLIR_Register* result,
  LLIR_Register* reg0, LLIR_Register* reg1, IR_Exp* exp, StmtType type ) {

  DSTART( "InstructionFactory::insertDIV" );

  ufAssert( reg0 );
  ufAssert( reg1 );
  ufAssert( result );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register* p0 = CreateRegister( "" );
  LLIR_Register* p1 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  mCurrentInstr = insMOV( cond, "", p0, reg0, LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1, reg1, LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( p0 );
  regs.push_back( p1 );
  const string& label = getSoftFloatSymbol( SoftFloatSymbol::SDIVSI );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( p0, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB, mCurrentInstr );
}

void InstructionFactory::insertUDIVSI( const string& cond, LLIR_Register* result,
  LLIR_Register* reg0, LLIR_Register* reg1, IR_Exp* exp, StmtType type ) {

  DSTART( "InstructionFactory::insertDIV" );

  ufAssert( reg0 );
  ufAssert( reg1 );
  ufAssert( result );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register* p0 = CreateRegister( "" );
  LLIR_Register* p1 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  mCurrentInstr = insMOV( cond, "", p0, reg0, LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1, reg1, LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( p0 );
  regs.push_back( p1 );
  const string& label = getSoftFloatSymbol( SoftFloatSymbol::UDIVSI );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( p0, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB, mCurrentInstr );
}

void InstructionFactory::insertDTOF( const std::string &cond,
                                     LLIR_Register *result,
                                     LLIR_Register *ereg, IR_Exp *exp,
                                     StmtType type )
{
  DSTART( "InstructionFactory::insertDTOF" );

  // Sanity checks.
  ufAssert( result );
  ufAssert( ereg );
  ufAssert( !result->IsHierarchical() );
  ufAssert( ereg->IsHierarchical() && ereg->GetNumberOfChildren() == 2 );

  // Create pre-colored registers r0 and r1.
  LLIR_Register* r0 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r0, CreateRegister( PHREG_R0 ) );
  LLIR_Register* r1 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r1, CreateRegister( PHREG_R1 ) );

  // Move the source ereg. First child (MSB) in r0 and second child (LSB) in r1.
  insertMOV( cond, r0, ereg->GetFirstChild(), OPER_LSL, 0, exp, type );
  insertMOV( cond, r1, ereg->GetNextChild( ereg->GetFirstChild() ), OPER_LSL,
             0, exp, type );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( r0 );
  regs.push_back( r1 );
  insertBL( cond, getSoftFloatSymbol( SoftFloatSymbol::DTOF ), &regs, exp,
            type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( r0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();
  insertMOV( cond, result, r0, OPER_LSL, 0, exp, type );
}

void InstructionFactory::insertFTOD( const std::string &cond,
                                     LLIR_Register *eresult,
                                     LLIR_Register *reg, IR_Exp *exp,
                                     StmtType type )
{
  DSTART( "InstructionFactory::insertFTOD" );

  // Sanity checks.
  ufAssert( eresult );
  ufAssert( reg );
  ufAssert( eresult->IsHierarchical() && eresult->GetNumberOfChildren() == 2 );
  ufAssert( !reg->IsHierarchical() );

  // Create pre-colored physical registers r0 and r1.
  LLIR_Register* r0 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r0, CreateRegister( PHREG_R0 ) );
  LLIR_Register* r1 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r1, CreateRegister( PHREG_R1 ) );

  // Move the source register into the physical register r0.
  insertMOV( cond, r0, reg, OPER_LSL, 0, exp, type );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( r0 );
  const string& label = getSoftFloatSymbol( SoftFloatSymbol::FTOD );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( r0, USAGE_DEF, 1 );
  LLIR_Parameter *implicitParam1 = new LLIR_Parameter( r1, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );
  opCall->AddParameter( implicitParam1 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  //Move the result.
  insertMOV( cond, eresult->GetFirstChild(), r0, OPER_LSL, 0, exp, type );
  insertMOV( cond, eresult->GetNextChild( eresult->GetFirstChild() ), r1,
             OPER_LSL, 0, exp, type );
}

void InstructionFactory::insertDTOI( const std::string &cond,
                                     LLIR_Register *result,
                                     LLIR_Register *ereg0, IR_Exp *exp,
                                     StmtType type )
{
  DSTART( "InstructionFactory::insertDTOI" );

  // Sanity checks.
  ufAssert( result );
  ufAssert( ereg0 );
  ufAssert( !result->IsHierarchical() );
  ufAssert( ereg0->IsHierarchical() && ereg0->GetNumberOfChildren() == 2 );

  // Create pre-colored registers r0 and r1.
  LLIR_Register* r0 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r0, CreateRegister( PHREG_R0 ) );
  LLIR_Register* r1 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r1, CreateRegister( PHREG_R1 ) );

  // Move the source ereg into the pre-colored registers.
  // First child (MSB) in r0 and second child (LSB) in r1
  insertMOV( cond, r0, ereg0->GetFirstChild(), OPER_LSL, 0, exp, type );
  insertMOV( cond, r1, ereg0->GetNextChild( ereg0->GetFirstChild() ), OPER_LSL,
             0, exp, type );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( r0 );
  regs.push_back( r1 );
  insertBL( cond, getSoftFloatSymbol( SoftFloatSymbol::DTOI ), &regs, exp,
            type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( r0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();
  insertMOV( cond, result, r0, OPER_LSL, 0, exp, type );
}

void InstructionFactory::insertDTOU( const std::string &cond,
                                     LLIR_Register *result,
                                     LLIR_Register *ereg0,
                                     IR_Exp *exp, StmtType type )
{
  DSTART( "InstructionFactory::insertDTOU" );

  // Sanity checks.
  ufAssert( result );
  ufAssert( ereg0 );
  ufAssert( !result->IsHierarchical() );
  ufAssert( ereg0->IsHierarchical() && ereg0->GetNumberOfChildren() == 2 );

  // First, move the source ereg into the pre-colored registers r0 and r1.
  LLIR_Register* r0 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r0, CreateRegister( PHREG_R0 ) );
  LLIR_Register* r1 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r1, CreateRegister( PHREG_R1 ) );

  // Move them.
  // First child (MSB) in r0 and second child (LSB) in r1
  insertMOV( cond, r0, ereg0->GetFirstChild(), OPER_LSL, 0, exp, type );
  insertMOV( cond, r1, ereg0->GetNextChild( ereg0->GetFirstChild() ), OPER_LSL,
             0, exp, type );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( r0 );
  regs.push_back( r1 );
  insertBL( cond, getSoftFloatSymbol( SoftFloatSymbol::DTOU ), &regs, exp,
            type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( r0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();
  insertMOV( cond, result, r0, OPER_LSL, 0, exp, type );
}

void InstructionFactory::insertDTOLL( const std::string &cond,
                                      LLIR_Register *eresult,
                                      LLIR_Register *ereg, IR_Exp *exp,
                                      StmtType type )
{
  DSTART( "InstructionFactory::insertDTOLL" );

  // Sanity checks.
  ufAssert( eresult );
  ufAssert( ereg );
  ufAssert( eresult->IsHierarchical() && eresult->GetNumberOfChildren() == 2 );
  ufAssert( ereg->IsHierarchical() && ereg->GetNumberOfChildren() == 2 );

  // Create pre-colored physical registers r0 and r1.
  LLIR_Register* r0 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r0, CreateRegister( PHREG_R0 ) );
  LLIR_Register* r1 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r1, CreateRegister( PHREG_R1 ) );

  // Move the source ereg into the physical registers.
  // First child (MSB) in r0 and second child (LSB) in r1.
  insertMOV( cond, r0, ereg->GetFirstChild(), OPER_LSL, 0, exp, type );
  insertMOV( cond, r1, ereg->GetNextChild( ereg->GetFirstChild() ), OPER_LSL,
             0, exp, type );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( r0 );
  regs.push_back( r1 );

  insertBL( cond, getSoftFloatSymbol( SoftFloatSymbol::DTOLL ), &regs, exp,
            type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( r0, USAGE_DEF, 1 );
  LLIR_Parameter *implicitParam1 = new LLIR_Parameter( r1, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );
  opCall->AddParameter( implicitParam1 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();
  // Move the result.
  insertMOV( cond, eresult->GetFirstChild(), r0, OPER_LSL, 0, exp, type );
  insertMOV( cond, eresult->GetNextChild( eresult->GetFirstChild() ), r1,
             OPER_LSL, 0, exp, type );
}

void InstructionFactory::insertLLTOD( const std::string &cond,
                                      LLIR_Register *eresult,
                                      LLIR_Register *ereg, IR_Exp *exp,
                                      StmtType type )
{
  DSTART( "InstructionFactory::insertLLTOD" );

  // Sanity checks.
  ufAssert( eresult );
  ufAssert( ereg );
  ufAssert( eresult->IsHierarchical() && eresult->GetNumberOfChildren() == 2 );
  ufAssert( ereg->IsHierarchical() && ereg->GetNumberOfChildren() == 2 );

  // Create pre-colored physical registers r0 and r1.
  LLIR_Register* r0 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r0, CreateRegister( PHREG_R0 ) );
  LLIR_Register* r1 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r1, CreateRegister( PHREG_R1 ) );

  // Move the source ereg into the physical registers.
  // First child (MSB) in r0 and second child (LSB) in r1.
  insertMOV( cond, r0, ereg->GetFirstChild(), OPER_LSL, 0, exp, type );
  insertMOV( cond, r1, ereg->GetNextChild( ereg->GetFirstChild() ), OPER_LSL,
             0, exp, type );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( r0 );
  regs.push_back( r1 );

  insertBL( cond, getSoftFloatSymbol( SoftFloatSymbol::LLTOD ), &regs, exp,
            type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( r0, USAGE_DEF, 1 );
  LLIR_Parameter *implicitParam1 = new LLIR_Parameter( r1, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );
  opCall->AddParameter( implicitParam1 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();
  // Move the result.
  insertMOV( cond, eresult->GetFirstChild(), r0, OPER_LSL, 0, exp, type );
  insertMOV( cond, eresult->GetNextChild( eresult->GetFirstChild() ), r1,
             OPER_LSL, 0, exp, type );
}

void InstructionFactory::insertDTOULL( const std::string &cond,
                                       LLIR_Register *eresult,
                                       LLIR_Register *ereg, IR_Exp *exp,
                                       StmtType type )
{
  DSTART( "InstructionFactory::insertDTOULL" );

  // Sanity checks.
  ufAssert( eresult );
  ufAssert( ereg );
  ufAssert( eresult->IsHierarchical() && eresult->GetNumberOfChildren() == 2 );
  ufAssert( ereg->IsHierarchical() && ereg->GetNumberOfChildren() == 2 );

  // Create pre-colored physical registers r0 and r1.
  LLIR_Register* r0 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r0, CreateRegister( PHREG_R0 ) );
  LLIR_Register* r1 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r1, CreateRegister( PHREG_R1 ) );

  // Move the source ereg into the physical registers.
  // First child (MSB) in r0 and second child (LSB) in r1.
  insertMOV( cond, r0, ereg->GetFirstChild(), OPER_LSL, 0, exp, type );
  insertMOV( cond, r1, ereg->GetNextChild( ereg->GetFirstChild() ), OPER_LSL,
             0, exp, type );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( r0 );
  regs.push_back( r1 );

  insertBL( cond, getSoftFloatSymbol( SoftFloatSymbol::DTOULL ), &regs, exp,
            type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( r0, USAGE_DEF, 1 );
  LLIR_Parameter *implicitParam1 = new LLIR_Parameter( r1, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );
  opCall->AddParameter( implicitParam1 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();
  // Move the result.
  insertMOV( cond, eresult->GetFirstChild(), r0, OPER_LSL, 0, exp, type );
  insertMOV( cond, eresult->GetNextChild( eresult->GetFirstChild() ), r1,
             OPER_LSL, 0, exp, type );
}

void InstructionFactory::insertULLTOD( const std::string &cond,
                                       LLIR_Register *eresult,
                                       LLIR_Register *ereg, IR_Exp *exp,
                                       StmtType type )
{
  DSTART( "InstructionFactory::insertULLTOD" );

  // Sanity checks.
  ufAssert( eresult );
  ufAssert( ereg );
  ufAssert( eresult->IsHierarchical() && eresult->GetNumberOfChildren() == 2 );
  ufAssert( ereg->IsHierarchical() && ereg->GetNumberOfChildren() == 2 );

  // Create pre-colored physical registers r0 and r1.
  LLIR_Register* r0 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r0, CreateRegister( PHREG_R0 ) );
  LLIR_Register* r1 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r1, CreateRegister( PHREG_R1 ) );

  // Move the source ereg into the physical registers.
  // First child (MSB) in r0 and second child (LSB) in r1.
  insertMOV( cond, r0, ereg->GetFirstChild(), OPER_LSL, 0, exp, type );
  insertMOV( cond, r1, ereg->GetNextChild( ereg->GetFirstChild() ), OPER_LSL,
             0, exp, type );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( r0 );
  regs.push_back( r1 );

  insertBL( cond, getSoftFloatSymbol( SoftFloatSymbol::ULLTOD ), &regs, exp,
            type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( r0, USAGE_DEF, 1 );
  LLIR_Parameter *implicitParam1 = new LLIR_Parameter( r1, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );
  opCall->AddParameter( implicitParam1 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();
  // Move the result.
  insertMOV( cond, eresult->GetFirstChild(), r0, OPER_LSL, 0, exp, type );
  insertMOV( cond, eresult->GetNextChild( eresult->GetFirstChild() ), r1,
             OPER_LSL, 0, exp, type );
}

void InstructionFactory::insertGT_D( const string &cond,
                                     LLIR_Register *result,
                                     LLIR_Register *ereg0,
                                     LLIR_Register *ereg1, IR_Exp *exp,
                                     StmtType type ) {

  DSTART( "InstructionFactory::insertGT_D" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *p0 = CreateRegister( "" );
  LLIR_Register *p1 = CreateRegister( "" );
  LLIR_Register *p2 = CreateRegister( "" );
  LLIR_Register *p3 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p2, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p3, CreateRegister( PHREG_R3 ) );

  // Move arguments.
  mCurrentInstr = insMOV( cond, "", p0, ereg0->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p2, ereg1->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p3,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );
  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( p0 );
  eregs.push_back( p1 );
  eregs.push_back( p2 );
  eregs.push_back( p3 );
  const string &label = getSoftFloatSymbol( SoftFloatSymbol::DGT );
  insertBL( cond, label, &eregs, exp, type );

  // Set DEF/USE attributes.
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( p0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB,
                          mCurrentInstr );
}

void InstructionFactory::insertLT_D( const string &cond,
                                     LLIR_Register *result,
                                     LLIR_Register *ereg0,
                                     LLIR_Register *ereg1, IR_Exp *exp,
                                     StmtType type ) {

  DSTART( "InstructionFactory::insertLT_D" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *p0 = CreateRegister( "" );
  LLIR_Register *p1 = CreateRegister( "" );
  LLIR_Register *p2 = CreateRegister( "" );
  LLIR_Register *p3 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p2, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p3, CreateRegister( PHREG_R3 ) );

  // Move arguments.
  mCurrentInstr = insMOV( cond, "", p0, ereg0->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p2, ereg1->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p3,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );
  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( p0 );
  eregs.push_back( p1 );
  eregs.push_back( p2 );
  eregs.push_back( p3 );
  const string &label = getSoftFloatSymbol( SoftFloatSymbol::DLT );
  insertBL( cond, label, &eregs, exp, type );

  // Set DEF/USE attributes.
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( p0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB,
                          mCurrentInstr );
}

void InstructionFactory::insertGEQ_D( const string &cond,
                                     LLIR_Register *result,
                                     LLIR_Register *ereg0,
                                     LLIR_Register *ereg1, IR_Exp *exp,
                                     StmtType type ) {

  DSTART( "InstructionFactory::insertGEQ_D" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *p0 = CreateRegister( "" );
  LLIR_Register *p1 = CreateRegister( "" );
  LLIR_Register *p2 = CreateRegister( "" );
  LLIR_Register *p3 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p2, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p3, CreateRegister( PHREG_R3 ) );

  // Move arguments.
  mCurrentInstr = insMOV( cond, "", p0, ereg0->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p2, ereg1->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p3,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );
  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( p0 );
  eregs.push_back( p1 );
  eregs.push_back( p2 );
  eregs.push_back( p3 );
  const string &label = getSoftFloatSymbol( SoftFloatSymbol::DGE );
  insertBL( cond, label, &eregs, exp, type );

  // Set DEF/USE attributes.
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( p0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB,
                          mCurrentInstr );
}

void InstructionFactory::insertLEQ_D( const string &cond,
                                     LLIR_Register *result,
                                     LLIR_Register *ereg0,
                                     LLIR_Register *ereg1, IR_Exp *exp,
                                     StmtType type ) {

  DSTART( "InstructionFactory::insertLEQ_D" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *p0 = CreateRegister( "" );
  LLIR_Register *p1 = CreateRegister( "" );
  LLIR_Register *p2 = CreateRegister( "" );
  LLIR_Register *p3 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p2, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p3, CreateRegister( PHREG_R3 ) );

  // Move arguments.
  mCurrentInstr = insMOV( cond, "", p0, ereg0->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p2, ereg1->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p3,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );
  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( p0 );
  eregs.push_back( p1 );
  eregs.push_back( p2 );
  eregs.push_back( p3 );
  const string &label = getSoftFloatSymbol( SoftFloatSymbol::DLE );
  insertBL( cond, label, &eregs, exp, type );

  // Set DEF/USE attributes.
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( p0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB,
                          mCurrentInstr );
}

void InstructionFactory::insertEQ_D( const string &cond,
                                     LLIR_Register *result,
                                     LLIR_Register *ereg0,
                                     LLIR_Register *ereg1, IR_Exp *exp,
                                     StmtType type ) {

  DSTART( "InstructionFactory::insertEQ_D" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *p0 = CreateRegister( "" );
  LLIR_Register *p1 = CreateRegister( "" );
  LLIR_Register *p2 = CreateRegister( "" );
  LLIR_Register *p3 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p2, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p3, CreateRegister( PHREG_R3 ) );

  // Move arguments.
  mCurrentInstr = insMOV( cond, "", p0, ereg0->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p2, ereg1->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p3,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );
  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( p0 );
  eregs.push_back( p1 );
  eregs.push_back( p2 );
  eregs.push_back( p3 );
  const string &label = getSoftFloatSymbol( SoftFloatSymbol::DEQ );
  insertBL( cond, label, &eregs, exp, type );

  // Set DEF/USE attributes.
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( p0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB,
                          mCurrentInstr );
}

void InstructionFactory::insertNEQ_D( const string &cond,
                                      LLIR_Register *result,
                                      LLIR_Register *ereg0,
                                      LLIR_Register *ereg1, IR_Exp *exp,
                                      StmtType type ) {

  DSTART( "InstructionFactory::insertNEQ_D" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register *p0 = CreateRegister( "" );
  LLIR_Register *p1 = CreateRegister( "" );
  LLIR_Register *p2 = CreateRegister( "" );
  LLIR_Register *p3 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p2, CreateRegister( PHREG_R2 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p3, CreateRegister( PHREG_R3 ) );

  // Move arguments.
  mCurrentInstr = insMOV( cond, "", p0, ereg0->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1,
                          ereg0->GetNextChild( ereg0->GetFirstChild()),
                          LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p2, ereg1->GetFirstChild(), LAST_LLIR_BB,
                          mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p3,
                          ereg1->GetNextChild( ereg1->GetFirstChild() ),
                          LAST_LLIR_BB, mCurrentInstr );
  // Insert the call.
  deque< LLIR_Register* > eregs;
  eregs.push_back( p0 );
  eregs.push_back( p1 );
  eregs.push_back( p2 );
  eregs.push_back( p3 );
  const string &label = getSoftFloatSymbol( SoftFloatSymbol::DNEQ );
  insertBL( cond, label, &eregs, exp, type );

  // Set DEF/USE attributes.
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( p0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB,
                          mCurrentInstr );
}

void InstructionFactory::insertTST( const string& l0, LLIR_Register *r0,
  LLIR_Register *r1, IR_Exp *exp, StmtType type )
{
  // insTST( condition, lhsReg, rhsReg,.. )
  mCurrentInstr = insTST( l0, r0, r1, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertTST( const string& l0, LLIR_Register *r0, int c0,
  IR_Exp *exp, StmtType type )
{
  mCurrentInstr = insTST( l0, r0, c0, LAST_LLIR_BB, mCurrentInstr );

  // If exp defined, insert assembler debug information.
  if( mConfig.getGenerateDebugFlag() && exp )
    addDebugInfo( &exp->getStmt(), mCurrentInstr, type );
}

void InstructionFactory::insertUMODSI( const string& cond, LLIR_Register* r0,
  LLIR_Register* r1, LLIR_Register* r2, IR_Exp* exp, StmtType type )
{
  DSTART( "InstructionFactory::insertUMODSI" );

  // First calculate r1 / r2.
  insertUDIVSI( cond, r0, r1, r2, exp, type );

  // And multiply it with r2.
  // BEWARE: We explicitely want to insert the multiplication
  // mul r0, r2, r0
  // ...and NOT
  // mul r0, r0, r2
  // ...as the ARM architecture reads:
  // "Specifying the same register for <Rd> and <Rm> was previously described as
  // producing UNPREDICTABLE results..."
  // When using ARM GDB for simulating, it will indeed end up in weird results.
  insertMUL( cond, r0, r2, r0, exp, type );

  // And finally, subtract that from r1.
  insertSUB( cond, r0, r1, r0, exp, type );
}

void InstructionFactory::insertSMODSI( const string& cond, LLIR_Register* r0,
  LLIR_Register* r1, LLIR_Register* r2, IR_Exp* exp, StmtType type )
{
  DSTART( "InstructionFactory::insertSMODSI" );

  // First calculate r1 / r2.
  insertSDIVSI( cond, r0, r1, r2, exp, type );

  // And multiply it with r2.
  // BEWARE: We explicitely want to insert the multiplication
  // mul r0, r2, r0
  // ...and NOT
  // mul r0, r0, r2
  // ...as the ARM architecture reads:
  // "Specifying the same register for <Rd> and <Rm> was previously described as
  // producing UNPREDICTABLE results..."
  // When using ARM GDB for simulating, it will indeed end up in weird results.
  insertMUL( cond, r0, r2, r0, exp, type );

  // And finally, subtract that from r1.
  insertSUB( cond, r0, r1, r0, exp, type );
}

void InstructionFactory::insertLT_F( const string& cond, LLIR_Register* result,
  LLIR_Register* reg0, LLIR_Register* reg1, IR_Exp* exp, StmtType type ) {

  DSTART( "InstructionFactory::insertLT_F" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register* p0 = CreateRegister( "" );
  LLIR_Register* p1 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  mCurrentInstr = insMOV( cond, "", p0, reg0, LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1, reg1, LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( p0 );
  regs.push_back( p1 );
  const string& label = getSoftFloatSymbol( SoftFloatSymbol::FLT );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( p0, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB, mCurrentInstr );
}

void InstructionFactory::insertLE_F( const string& cond, LLIR_Register* result,
  LLIR_Register* reg0, LLIR_Register* reg1, IR_Exp* exp, StmtType type ) {

  DSTART( "InstructionFactory::insertLE_F" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register* p0 = CreateRegister( "" );
  LLIR_Register* p1 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  mCurrentInstr = insMOV( cond, "", p0, reg0, LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1, reg1, LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( p0 );
  regs.push_back( p1 );
  const string& label = getSoftFloatSymbol( SoftFloatSymbol::FLE );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( p0, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB, mCurrentInstr );
}

void InstructionFactory::insertGT_F( const string& cond, LLIR_Register* result,
  LLIR_Register* reg0, LLIR_Register* reg1, IR_Exp* exp, StmtType type ) {

  DSTART( "InstructionFactory::insertGT_F" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register* p0 = CreateRegister( "" );
  LLIR_Register* p1 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  mCurrentInstr = insMOV( cond, "", p0, reg0, LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1, reg1, LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( p0 );
  regs.push_back( p1 );
  const string& label = getSoftFloatSymbol( SoftFloatSymbol::FGT );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( p0, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB, mCurrentInstr );
}

void InstructionFactory::insertGE_F( const string& cond, LLIR_Register* result,
  LLIR_Register* reg0, LLIR_Register* reg1, IR_Exp* exp, StmtType type ) {

  DSTART( "InstructionFactory::insertGE_F" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register* p0 = CreateRegister( "" );
  LLIR_Register* p1 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  mCurrentInstr = insMOV( cond, "", p0, reg0, LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1, reg1, LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( p0 );
  regs.push_back( p1 );
  const string& label = getSoftFloatSymbol( SoftFloatSymbol::FGE );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( p0, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB, mCurrentInstr );
}

void InstructionFactory::insertEQ_F( const string& cond, LLIR_Register* result,
  LLIR_Register* reg0, LLIR_Register* reg1, IR_Exp* exp, StmtType type ) {

  DSTART( "InstructionFactory::insertEQ_F" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register* p0 = CreateRegister( "" );
  LLIR_Register* p1 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  mCurrentInstr = insMOV( cond, "", p0, reg0, LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1, reg1, LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( p0 );
  regs.push_back( p1 );
  const string& label = getSoftFloatSymbol( SoftFloatSymbol::FEQ );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( p0, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB, mCurrentInstr );
}

void InstructionFactory::insertNE_F( const string& cond, LLIR_Register* result,
  LLIR_Register* reg0, LLIR_Register* reg1, IR_Exp* exp, StmtType type ) {

  DSTART( "InstructionFactory::insertNE_F" );

  // First, move the LLIR_Registers into the corresponding registers.
  LLIR_Register* p0 = CreateRegister( "" );
  LLIR_Register* p1 = CreateRegister( "" );

  LAST_LLIR_FUNCTION->AddPrecolor( p0, CreateRegister( PHREG_R0 ) );
  LAST_LLIR_FUNCTION->AddPrecolor( p1, CreateRegister( PHREG_R1 ) );
  mCurrentInstr = insMOV( cond, "", p0, reg0, LAST_LLIR_BB, mCurrentInstr );
  mCurrentInstr = insMOV( cond, "", p1, reg1, LAST_LLIR_BB, mCurrentInstr );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( p0 );
  regs.push_back( p1 );
  const string& label = getSoftFloatSymbol( SoftFloatSymbol::FNEQ );
  insertBL( cond, label, &regs, exp, type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam = new LLIR_Parameter( p0, USAGE_DEF, true );
  opCall->AddParameter( implicitParam );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();

  // And move the result.
  mCurrentInstr = insMOV( cond, "", result, p0, LAST_LLIR_BB, mCurrentInstr );
}

void InstructionFactory::insertULLTOF( const std::string &cond,
                                       LLIR_Register *res,
                                       LLIR_Register *ereg, IR_Exp *exp,
                                       StmtType type )
{
  DSTART( "InstructionFactory::insertULLTOF" );

  // Sanity checks.
  ufAssert( res );
  ufAssert( ereg );
  ufAssert( ereg->IsHierarchical() && ereg->GetNumberOfChildren() == 2 );

  // Create pre-colored physical registers r0 and r1.
  LLIR_Register* r0 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r0, CreateRegister( PHREG_R0 ) );
  LLIR_Register* r1 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r1, CreateRegister( PHREG_R1 ) );

  // Move the source ereg into the physical registers.
  // First child (MSB) in r0 and second child (LSB) in r1.
  insertMOV( cond, r0, ereg->GetFirstChild(), OPER_LSL, 0, exp, type );
  insertMOV( cond, r1, ereg->GetNextChild( ereg->GetFirstChild() ), OPER_LSL,
             0, exp, type );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( r0 );
  regs.push_back( r1 );

  insertBL( cond, getSoftFloatSymbol( SoftFloatSymbol::ULLTOF ), &regs, exp,
            type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( r0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();
  // Move the result.
  insertMOV( cond, res, r0, OPER_LSL, 0, exp, type );
}

void InstructionFactory::insertLLTOF( const std::string &cond,
                                       LLIR_Register *res,
                                       LLIR_Register *ereg, IR_Exp *exp,
                                       StmtType type )
{
  DSTART( "InstructionFactory::insertULLTOF" );

  // Sanity checks.
  ufAssert( res );
  ufAssert( ereg );
  ufAssert( ereg->IsHierarchical() && ereg->GetNumberOfChildren() == 2 );

  // Create pre-colored physical registers r0 and r1.
  LLIR_Register* r0 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r0, CreateRegister( PHREG_R0 ) );
  LLIR_Register* r1 = CreateRegister( "" );
  LAST_LLIR_FUNCTION->AddPrecolor( r1, CreateRegister( PHREG_R1 ) );

  // Move the source ereg into the physical registers.
  // First child (MSB) in r0 and second child (LSB) in r1.
  insertMOV( cond, r0, ereg->GetFirstChild(), OPER_LSL, 0, exp, type );
  insertMOV( cond, r1, ereg->GetNextChild( ereg->GetFirstChild() ), OPER_LSL,
             0, exp, type );

  // Insert the call.
  deque< LLIR_Register* > regs;
  regs.push_back( r0 );
  regs.push_back( r1 );

  insertBL( cond, getSoftFloatSymbol( SoftFloatSymbol::LLTOF ), &regs, exp,
            type );

  // Setup proper DEF/USE attributes
  LLIR_Instruction *insCall = getCurrentInstruction();
  LLIR_Operation *opCall = insCall->GetFirstOp();
  LLIR_Parameter *implicitParam0 = new LLIR_Parameter( r0, USAGE_DEF, 1 );
  opCall->AddParameter( implicitParam0 );

  // A call delimits a basic block.
  mCodesel.beginNewLLIRBasicBlock();
  // Move the result.
  insertMOV( cond, res, r0, OPER_LSL, 0, exp, type );
}
