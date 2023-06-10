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


using namespace std;
using namespace WIR;


// Macro COST_LESS just serves for compatibility with good-old icd-cg.
#define COST_LESS(x,y) CodeSelector::COST_LESS( x, y )

/* Evaluates to the costs a floating-point instruction would cause, taking
    into consideration the overhead of invoking external library routines. */
#define FP_COST(I)                                          \
  ( ( ARMCODESEL->getConfig()->getEmitFpuInstructions() )      \
    ? ( 2 * CT( INS_MOV_32) + CT( INS_BL_32 ) + CT( I ) ) \
    : ( CT( I ) )                                           \
  )

/*
   The strings logOrEndLabel and logAndEndLabel contain the names of a label
   that should be used within the tpm_BinaryExpLOGAND/tpm_BinaryExpLOGOR rules
   to generate a conditional jump if the left-hand side of these logical
   operators already evaluates to false or true, resp.
*/
static string logOrEndLabel = "";
static string logAndEndLabel = "";

/*!
   DEBUG_RULE_ACTION should be invoked right at the beginning of the action part
   of each rule. If debugging is enabled, it prints out which action part is
   entered, for which C expression. If debugging is disabled, DEBUG_RULE_ACTION
   does nothing .

   This function is named in uppercase letters against the usual convention,
   which would force us to use lowercase letters, because it mimics a debugmacro
   functionality and all the debugmacro stuff is written in uppercase letters.
*/
inline void DEBUG_RULE_ACTION( const string& ruleSignature, NODEPTR treeElem )
{
  #ifdef DEBUG_WCC
  stringstream ss;

  DSTART_EXT(
    const_cast<char *>( ruleSignature.c_str() ),
    "CodeSelector::RulesetActions" );

  if ( treeElem->getExp() ) {
    treeElem->getExp()->write( ss );

    // If debug-output is written to cout, the expression is printed in bold.
    if ( string( "cout" ).compare( CODESEL_DBG_TARGET ) == 0 ) {
      DOUT( "\33[1m[ " << ss.str() << " ]\33[0m" << endl );
    } else {
      DOUT( "[ " << ss.str() << " ]" << endl );
    }
  } else

  if ( treeElem->getStmt() ) {
    treeElem->getStmt()->write( ss );

    // If debug-output is written to cout, the statment is printed in bold.
    if ( string( "cout" ).compare( CODESEL_DBG_TARGET ) == 0 ) {
      DOUT( "\33[1m" << ss.str() << "\33[0m" << endl );
    } else {
      DOUT( ss.str() << endl );
    }
  }
  #endif
}

#ifdef DEBUGMACROS
/*!
  Debug output helper function.
 */
static string getExpString( const IR_Exp& exp )
{
  stringstream out;
  out << "'";
  exp.write( out );
  out << "' (" << exp.getStmt().getFileContext().getFilename() << ":" <<
                  exp.getStmt().getFileContext().getLine() << ")";
  char line[255];
  out.getline( line, 255 );
  return line;
}
#endif

/*!
  Returns a new LLIR_Register object for register 'target'
  matching the type 'type' of the LHS of an assignment expression.
 */
LLIR_Register *getNewLHSRegister( string target, const IR_Type & )
{
    return ARMINSTRUCTIONS.CreateRegister( target, true );
}


/*!
  Returns a new LLIR_Register object for register 'target'
  that matches the type of the given symbol expression.
 */
LLIR_Register *getNewRegister( string target, const IR_SymbolExp &symExp )
{
  IR_Type &type = symExp.getType();

  if ( type.bitSize() <= 32 )
    return ARMINSTRUCTIONS.CreateRegister( target );
  else
    return ARMINSTRUCTIONS.CreateERegister( target );
}

/*!
  Returns the cost for using loadRegisterSymbol( symExp ).
 */
COST loadRegisterSymbolCost( const IR_SymbolExp &symExp )
{
  if ( isRegType( symExp ) ) {
    return CT( INS_MOV_32 );
  } else {
    return ( ( symExp.getType().bitSize() <= 32 ) ? 1 : 2 ) * CT( INS_MOV_32 );
  }
}

/*!
   This function binds the given register to the given physical register number,
   no matter whether it is a data register, an address register or an extended
   register.
 */
void bindToPHREG( LLIR_Register &reg, unsigned int phregNumber )
{
  LLIR_Function * const lf = ARMCODESEL->getLastLLIRFunction();

  if ( !isEReg( reg.GetName() ) ) {
      switch ( phregNumber ) {
        case  0: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R0 ) ); break;
        case  1: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R1 ) ); break;
        case  2: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R2 ) ); break;
        case  3: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R3 ) ); break;
        case  4: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R4 ) ); break;
        case  5: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R5 ) ); break;
        case  6: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R6 ) ); break;
        case  7: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R7 ) ); break;
        case  8: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R8 ) ); break;
        case  9: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R9 ) ); break;
        case 10: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R10 ) ); break;
        case 11: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R11 ) ); break;
        case 12: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R12 ) ); break;
        case 13: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R13 ) ); break;
        case 14: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R14 ) ); break;
        case 15: lf->AddPrecolor( &reg, ARMINSTRUCTIONS.CreateRegister( PHREG_R15 ) ); break;
        default: ufAssertT( 0, "Invalid register index!" );
      }
  } else {
    switch ( phregNumber ) {
        case  0:
          // We do not color the pseudo extended register itself, only the
          // children.
          lf->AddPrecolor( reg.GetFirstChild(),
            ARMINSTRUCTIONS.CreateRegister( PHREG_R0 ) );
          lf->AddPrecolor( reg.GetNextChild( reg.GetFirstChild() ),
            ARMINSTRUCTIONS.CreateRegister( PHREG_R1 ) );
          break;
        case  2:
          // We do not color the pseudo extended register itself, only the
          // children.
          lf->AddPrecolor( reg.GetFirstChild(),
            ARMINSTRUCTIONS.CreateRegister( PHREG_R2 ) );
          lf->AddPrecolor( reg.GetNextChild( reg.GetFirstChild() ),
            ARMINSTRUCTIONS.CreateRegister( PHREG_R3 ) );
          break;
        case  4:
          // We do not color the pseudo extended register itself, only the
          // children.
          lf->AddPrecolor( reg.GetFirstChild(),
            ARMINSTRUCTIONS.CreateRegister( PHREG_R4 ) );
          lf->AddPrecolor( reg.GetNextChild( reg.GetFirstChild() ),
            ARMINSTRUCTIONS.CreateRegister( PHREG_R5 ) );
          break;
        case  6:
          // We do not color the pseudo extended register itself, only the
          // children.
          lf->AddPrecolor( reg.GetFirstChild(),
            ARMINSTRUCTIONS.CreateRegister( PHREG_R6 ) );
          lf->AddPrecolor( reg.GetNextChild( reg.GetFirstChild() ),
            ARMINSTRUCTIONS.CreateRegister( PHREG_R7 ) );
          break;
        case  8:
          // We do not color the pseudo extended register itself, only the
          // children.
          lf->AddPrecolor( reg.GetFirstChild(),
            ARMINSTRUCTIONS.CreateRegister( PHREG_R8 ) );
          lf->AddPrecolor( reg.GetNextChild( reg.GetFirstChild() ),
            ARMINSTRUCTIONS.CreateRegister( PHREG_R9 ) );
          break;
        default: ufAssertT( 0, "Invalid register index!" );
    }
  }
}

/*!
  Loads the non-stack, non-global symbol 'sym' into it's appropriate register,
  as determined by the settings in the Stack class and then returns the register.
 */
LLIR_Register *loadRegisterSymbol( IR_SymbolExp &symExp )
{
  IR_Symbol &sym = symExp.getSymbol();
  SymbolInfo * const symbolInfo = ARMCODESEL->getStack()->getSymbolInfo( &sym );
  assert( symbolInfo != 0 && "Missing symbol info!" );

  LLIR_Register *reg0 = 0;

  if ( symbolInfo->getSymbolType() == SymbolInfo::D_ARGUMENT ||
       symbolInfo->getSymbolType() == SymbolInfo::A_ARGUMENT ) {

    // Current symbol is a function argument.
    const int myRegNumber = ARMCODESEL->getStack()->isPassedThroughRegister( sym );
    if ( myRegNumber != -1 ) {
      LLIR_BB * const theBB = ARMCODESEL->getLastLLIRFunction()->GetFirstBB();
      LLIR_Register   *vreg = 0;

      const string symReg = ARMCODESEL->getStack()->getSymbolReg( &sym );
      if ( symReg != "" )
        vreg  = getNewRegister( symReg, symExp );
      else {
        vreg  = getNewRegister( "", symExp );
        ARMCODESEL->getStack()->setSymbolReg( &sym, string( vreg->GetName() ) );
      }

      // Generate Constraints for Register Allocator w.r.t. LC Registers.
      bindToPHREG( *vreg, myRegNumber );

      // If not existing, generate a new function internal VREG for this
      // argument, else use the existing one.
      const string viSym = ARMCODESEL->getStack()->getInternalVReg( &sym );
      if ( viSym != "" ) {
        reg0 = getNewRegister( viSym, symExp );
      } else {
        // Add a MOV from the external to the internal VREG at the very
        // beginning of the current LLIR function, to save the argument
        // into the upper context. Use a plain new register as the internal
        // reg.
        reg0 = getNewRegister( "", symExp );
        ARMCODESEL->getStack()->setInternalVReg( &sym, reg0->GetName() );
        LLIR_Instruction* mov = nullptr;
        if ( sym.getType().bitSize() <= 32 ||
             isArrayType( sym ) ) {
          ufAssert( reg0 );
          ufAssert( vreg );
          mov = insMOV( OPER_AL, "", reg0, vreg );
          theBB->InsertIns( mov );

        } else {
          // Move the two registers.
          mov = insMOV( OPER_AL, "", reg0->GetFirstChild(),
                        vreg->GetFirstChild() );
          theBB->InsertIns( mov );
          mov = insMOV( OPER_AL, "",
                        reg0->GetNextChild( reg0->GetFirstChild() ),
                        vreg->GetNextChild( vreg->GetFirstChild() ) );
          theBB->InsertIns( mov );

        }

        if ( !ARMCODESEL->getCurrentInstruction()
             && ARMCODESEL->getLastLLIRFunction()->GetNumberOfBB() == 1 )
          ARMCODESEL->setCurrentInstruction( mov );
      }
    } else {
      ufAssertT( 0, "Cost function should prevent this" );
    }

  } else
  if ( symbolInfo->getSymbolType() == SymbolInfo::LOCAL_VAR ) {

    // If no virtual register has been assigned to the symbol, assign a new
    // virtual register, else use the existing one
    const string symReg = ARMCODESEL->getStack()->getSymbolReg( &sym );
    if ( symReg != "" ) {
      reg0 = getNewRegister( symReg, symExp );
    } else {
      reg0 = getNewRegister( "", symExp );
      ARMCODESEL->getStack()->setSymbolReg( &sym, reg0->GetName() );
    }

  } else {
    ufAssertT( 0, "Cost function should prevent this" );
  }
  ufAssertT( reg0, "Internal error: Did not create symbol register!" );

  return reg0;
}


/*!
  Returns the cost for using loadStackSymbol( symExp ).
 */
COST loadStackSymbolCost( const IR_SymbolExp &symExp )
{
  COST cost = 0;

  switch ( symExp.getType().getType() ) {
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::BOOL:
      cost = CT( INS_LDRB_32 );
      break;
    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT:
      cost = CT( INS_LDRH_32 );
      break;
    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::POINTER:
    case IR_Type::FLOAT:
      cost = CT( INS_LDR_32 );
      break;
    case IR_Type::LONG_LONG:
    case IR_Type::UNSIGNED_LONG_LONG:
    case IR_Type::DOUBLE:
    case IR_Type::LONG_DOUBLE:
      cost = 2 * CT( INS_LDR_32 );
      break;
    case IR_Type::ARRAY:
      cost = CT( INS_LDR_32 );
      break;
    default:
      ufAssertT( 0, "Uncovered case!" );
      break;
  }

  return cost;
}


/*!
  Loads the local stack symbol 'sym' into it's appropriate register, as
  determined by the settings in the Stack class and then returns the symbol
  info.

  'loadResult' specifies whether the result should be loaded into the target
               register, or if just the address should be loaded.
 */
ARM_LValue loadStackSymbol( IR_SymbolExp &symExp, bool loadResult )
{
  DSTART( "ARM_LValue loadStackSymbol(IR_SymbolExp&, bool)" );

  IR_Symbol &sym = symExp.getSymbol();

  SymbolInfo * const symbolInfo = ARMCODESEL->getStack()->getSymbolInfo( &sym );
  ufAssertT( symbolInfo != 0, "Missing symbol info!" );

  // Check if we need an ereg.
  LLIR_Register * reg0;
  if ( !isERegType( sym.getType() ) ) {
    reg0 = ARMINSTRUCTIONS.CreateRegister( "", false);
  } else {
    reg0 = ARMINSTRUCTIONS.CreateERegister( "" );
  }

  LLIR_Register * const sp   = ARMINSTRUCTIONS.CreateRegister( PHREG_SP, true );
  const int offset           = ARMCODESEL->getStack()->getSymbolOffset( &sym );

  // First do some administration work to keep the Stack object up to date.
  // We don' load the symbol yet, we let the control flow pass in all branches
  // where the symbol must be loaded and load it in the end. This centralizes
  // the code for loading at the end of this function. All branches which are
  // illegal, and which should be blocked by the cost part, are marked with
  // assertions, so we don't run into the load part accidentally.
  if ( symbolInfo->getSymbolType() == SymbolInfo::D_ARGUMENT ||
       symbolInfo->getSymbolType() == SymbolInfo::A_ARGUMENT ) {

    // Current symbol is a function argument.
    const int myRegNumber = ARMCODESEL->getStack()->isPassedThroughRegister( sym );
    if ( myRegNumber != -1 ) {
      LLIR_BB * const theBB = ARMCODESEL->getLastLLIRFunction()->GetFirstBB();
      LLIR_Register   *vreg = 0;

      const string symReg = ARMCODESEL->getStack()->getSymbolReg( &sym );
      if ( symReg != "" )
        vreg  = getNewRegister( symReg, symExp );
      else {
        vreg  = getNewRegister( "", symExp );
        ARMCODESEL->getStack()->setSymbolReg( &sym, string( vreg->GetName() ) );
      }

      // Generate Constraints for Register Allocator w.r.t. LC Registers.
      bindToPHREG( *vreg, myRegNumber );

      if ( ARMCODESEL->getStack()->getAddressTaken( &sym ) ) {

        if ( !ARMCODESEL->getStack()->getStoreInstructionsGenerated( &sym ) ) {
          // If the address of a function argument passed in a data register
          // is taken, we need to store the argument at the already reserved
          // stack position so that its address can be taken legally.
          LLIR_Instruction *insSt = 0;

          switch ( sym.getType().getType() ) {
            case IR_Type::CHAR:
            case IR_Type::UNSIGNED_CHAR:
            case IR_Type::BOOL:
              insSt = insSTRB( OPER_AL, OPER_IMMOFF, sp, vreg, offset );
              break;
            case IR_Type::SHORT:
            case IR_Type::UNSIGNED_SHORT:
              insSt = insSTRH( OPER_AL, OPER_IMMOFF, sp, vreg, offset );
              break;
            case IR_Type::INT:
            case IR_Type::UNSIGNED_INT:
            case IR_Type::LONG:
            case IR_Type::UNSIGNED_LONG:
            case IR_Type::FLOAT:
            case IR_Type::POINTER:
              insSt = insSTR( OPER_AL, OPER_IMMOFF, sp, vreg, offset );
              break;
            case IR_Type::LONG_LONG:
            case IR_Type::UNSIGNED_LONG_LONG:
            case IR_Type::DOUBLE:
            case IR_Type::LONG_DOUBLE:
              // A few sanity checks.
              ufAssert( vreg->IsHierarchical() );
              ufAssert( vreg->GetNumberOfChildren() == 2 );
              // Genererate the first instruction, store the LSB.
              insSTR( OPER_AL, OPER_IMMOFF, sp, vreg->GetFirstChild(),
                offset );
              // And the second one, store the MSB.
              insSt = insSTR( OPER_AL, OPER_IMMOFF,
                vreg->GetNextChild( vreg->GetFirstChild() ), sp, offset - 4 );
              break;
            case IR_Type::ARRAY:
              // no need to store, array is on the stack anyways
              break;
            default:
              ufAssertT( 0, "Uncovered case!" );
              break;
          }

          if ( insSt ) {
            theBB->InsertIns( insSt );
            if ( !ARMCODESEL->getCurrentInstruction() &&
                 ARMCODESEL->getLastLLIRFunction()->GetNumberOfBB() == 1 )
              ARMCODESEL->setCurrentInstruction( insSt );
          }

          ARMCODESEL->getStack()->setStoreInstructionsGenerated( &sym, true );
        }

        ; // Load result later ...

      } else {
        ufAssertT( 0, "Cost function should exclude this" );
      }

    } else { /* if arg # >= 5 */

      // The symbol was passed on the stack in this case.

      // TODO: We could load it each time it is accessed, but it is more
      //       intelligent to only load it once and then use the loaded version
      //       for further accesses. This only works if the argument's address
      //       has not been taken. The problem with the current setup is, that
      //       we can't do this at the moment, because for such a stack-passed
      //       argument this function is called inevitably by the rules.
      //       Unfortunately this function just returns an lvalue, whereas
      //       we'd need to return a string with the proper register. So
      //       what we need are new rules which produce a 'reg'/'areg'/...
      //       for stack-passed arguments like depicted above.

      ; // Load result later ...
      ARMCODESEL->getStack()->setSymbolReg( &sym, reg0->GetName() );
    }
  } else
  if ( symbolInfo->getSymbolType() == SymbolInfo::LOCAL_STACK_VAR ) {

    ; // Load result later ...

  } else {
    ufAssertT( 0, "Cost function should exclude this!" );
  }


  // Now finally do the access (the location is always the same)
  if ( loadResult ) {

    // For the sake of efficiency, use different load instructions
    // depending on the type of the variable.
    switch ( sym.getType().getType() ) {
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL:
        ARMINSTRUCTIONS.insertLDRB( OPER_IMMOFF, reg0, sp, offset, &symExp );
        break;
      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT:
        ARMINSTRUCTIONS.insertLDRH( OPER_IMMOFF, reg0, sp, offset, &symExp );
        break;
      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::POINTER:
      case IR_Type::FLOAT:
        ARMINSTRUCTIONS.insertLDR( OPER_IMMOFF, reg0, sp, offset, &symExp );
        break;
      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE:
        // A few sanity checks.
        ufAssert( reg0->IsHierarchical() );
        ufAssert( reg0->GetNumberOfChildren() == 2 );
        // Genererate the first instruction, load the LSB.
        ARMINSTRUCTIONS.insertLDR( OPER_IMMOFF, reg0->GetFirstChild(), sp,
          offset, &symExp );
        // And the second one, load the MSB.
        ARMINSTRUCTIONS.insertLDR( OPER_IMMOFF,
          reg0->GetNextChild( reg0->GetFirstChild() ), sp, offset - 4, &symExp );
        break;
      case IR_Type::ARRAY:
        ARMINSTRUCTIONS.insertLDR( OPER_IMMOFF, reg0, sp, offset, &symExp );
        break;
      default:
        ufAssertT( 0, "Uncovered case!" );
        break;
    }
  }

  return(
    ARM_LValue {
      loadResult ? reg0 : nullptr,
      ARM_AddressModification {
        sp, offset, &sym.getType(), AddressModification::ModTime::NONE,
        AddressModification::ModOper::ADD, true } } );
};


/*!
  Return the dry run version of the lvalue, only containing the stack offset
  of the symbol expression and not the address register.
*/
ARM_LValue loadStackSymbolDryRun( IR_SymbolExp &symExp ) {
  DSTART( "ARM_LValue loadStackSymbolDryRun(IR_SymbolExp&)" );

  // Acquire the stack offset in bytes.
  IR_Symbol &sym = symExp.getSymbol();
  const int offset = ARMCODESEL->getStack()->getSymbolOffset( &sym );

  // And bundle it together with the baseType in a dry-run lvalue.
  return(
    ARM_LValue {
      nullptr,
      ARM_AddressModification {
        static_cast<LLIR_Register *>( nullptr ), offset, &sym.getType(),
        AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
        true, true },
      true } );
};


/*!
  Returns the cost for using loadGlobalSymbol( symExp ).
 */
COST loadGlobalSymbolCost( [[maybe_unused]] const IR_SymbolExp &symExp )
{
  COST cost = CT( INS_LDR_32 );
  return cost;
};

//! Creates a new literal pool for a symbol.
LLIR_BB* createLiteralPool( std::string lab, IR_Exp* exp = nullptr )
{
  // Save the current instruction.
  LLIR_Instruction* curIns = ARMCODESEL->getCurrentInstruction();
  LLIR_Function *const func = ARMCODESEL->getLastLLIRFunction();
  // Create new basic block
  string bbLabel = LLIR::getUniqueLabel();
  LLIR_BB *addrBB = new LLIR_BB( bbLabel.c_str() );

  // Insert it into the function.
  LLIR_BB * const lastBB = func->GetLastBB();
  func->InsertBB( addrBB, lastBB );

  // Insert the word.
  ARMINSTRUCTIONS.insertASMWordDir( lab, addrBB, exp);

  // Restore the current instruction.
  ARMCODESEL->setCurrentInstruction( curIns );

  return addrBB;
}


/*!
  Loads the global symbol 'sym' into it's appropriate register, as determined
  by the settings in the Stack class and then returns the symbol info.

  'loadResult' specifies whether the result should be loaded into the target
               register, or if just the address should be loaded.
 */
ARM_LValue loadGlobalSymbol( IR_SymbolExp &symExp, bool loadResult )
{
  DSTART( "ARM_LValue loadGlobalSymbol(IR_SymbolExp&, bool)" );

  IR_Symbol &sym = symExp.getSymbol();

  LLIR_Register * reg0 = nullptr;
  LLIR_Register *iaddr = ARMINSTRUCTIONS.CreateRegister( "", true );

  // Load the allocated address register with the symbol's address.
  string varName;
  if ( sym.getType().getStorageClass() == IR_Type::STATIC )
    varName = ARMCODESEL->getStaticName( &sym );
  else {
    varName = sym.getWrittenName();
  }

  LLIR_Function *const func = ARMCODESEL->getLastLLIRFunction();
  string label = func->getDataSectionAddressLabel( varName );

  if( label.empty() )
  {
    DOUT( "no label found for " << varName <<
          ", inserting Block at end of function with new label: " );

    LLIR_BB* addrBB = createLiteralPool( varName, &symExp );
    ARMINSTRUCTIONS.insertLDR( OPER_IMMOFF, iaddr, false, addrBB->GetLabel(),
                             &symExp );
  }
  else
  {
    DOUT( "label " << label << " found for " << varName <<
          ", inserting no extra Block at end of function" << endl );
    ARMINSTRUCTIONS.insertLDR( OPER_IMMOFF, iaddr, false, label, &symExp );
  }

  IR_Type* baseType = &symExp.getType();

  if ( loadResult ) {
    reg0 = ARMINSTRUCTIONS.CreateRegister( "", false );
    // For the sake of efficiency, use different load instructions depending
    // on the type of the global variable.
    switch ( baseType->getType() ) {

      case IR_Type::CHAR:
        ARMINSTRUCTIONS.insertLDRSB( OPER_IMMOFF, reg0, iaddr, 0, &symExp );
        break;
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL:
        ARMINSTRUCTIONS.insertLDRB( OPER_IMMOFF, reg0, iaddr, 0, &symExp );
        break;
      case IR_Type::SHORT:
        ARMINSTRUCTIONS.insertLDRSH( OPER_IMMOFF, reg0, iaddr, 0, &symExp );
        break;
      case IR_Type::UNSIGNED_SHORT:
        ARMINSTRUCTIONS.insertLDRH( OPER_IMMOFF, reg0, iaddr, 0, &symExp );
        break;
      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT:
      case IR_Type::ARRAY:
      case IR_Type::POINTER:
      case IR_Type::FUNCTION:
        ARMINSTRUCTIONS.insertLDR( OPER_IMMOFF, reg0, iaddr, 0, &symExp );
        break;
      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE:
      default:
        ufAssertT( 0, "Uncovered case!" );
        break;

    }
  }

  // Assemble an ARM_AddressModification with no offset.
  ARM_AddressModification amod( iaddr, baseType );

  // Return  the result structure.
  return( ARM_LValue { reg0, amod } );
};


/*! This macro generates a wrapper function for a given constant-format-check function. */
#define IS_CONSTANT_EXP_WRAPPER( resultType, funcName )                       \
static inline resultType funcName ( const IR_Exp &exp )                       \
{                                                                             \
  const IR_IntConstExp *icExp = dynamic_cast<const IR_IntConstExp*>( &exp );  \
  if ( icExp ) {                                                              \
    return funcName ( icExp->getValue(), icExp->getType() );                  \
  } else {                                                                    \
    return 0;                                                                 \
  }                                                                           \
}

/*! Returns whether the given expression is a constant expression that can be
    represented by the 'addrOffset' nonterminal. */
static inline bool isAddrOffset( const IR_Integer &, const IR_Type &type )
{
  return type.bitSize() <= 32;
}

IS_CONSTANT_EXP_WRAPPER( bool, isAddrOffset );

/*! Returns whether the given value 'value' of type 'type' denotes an integer
    constant whose type does not occupy more than 32 bits, whose value is inside
    the interval [minValue, maxValue] and which is signed (if isSigned=true) or
    unsigned (if isSigned=false). */
static bool isBounded32BitConstant( const IR_Integer &value, const IR_Type &type,
                                    int minValue, int maxValue, bool isSigned )
{
  return value.getIntValue() >= minValue &&
         value.getIntValue() <= maxValue &&
         ( (  isSigned && type.isSignedType()   ) ||
           ( !isSigned && type.isUnsignedType() ) ) &&
         type.bitSize() <= 32;
}

/*! Returns whether the given expression is a constant expression that can be
    represented by the 'const8' nonterminal. */
static inline bool isConst8( const IR_Integer &value, const IR_Type & )
{
  return isValidARMImmediate( value.getIntValue() );
}IS_CONSTANT_EXP_WRAPPER( bool, isConst8 );

static inline bool isNegConst8( const IR_Integer &value )
{
  return isValidARMImmediate( -value.getIntValue() );
}

/*!
   computeSizeOf computes the size of an IR Type in bytes.
*/
int computeSizeOf( IR_Type *type )
{
  DSTART( "computeSizeOf( IR_Type * )");

  int bytesize = 0;
  const enum IR_Type::Type theType = type->getType();

  IR_PointerType *thePointerType = dynamic_cast<IR_PointerType *>( type );
  IR_ArrayType *theArrayType = dynamic_cast<IR_ArrayType *>( type );

  const bool isPointerType = ( thePointerType != 0 ) && ( theArrayType == 0 );
  const bool isArrayType = ( thePointerType != 0 ) && ( theArrayType != 0 );

  if ( ( theType == IR_Type::CHAR ) || ( theType == IR_Type::UNSIGNED_CHAR ) ||
       ( theType == IR_Type::BOOL ) )
    bytesize = charBytes;
  else
  if ( theType == IR_Type::SHORT || theType == IR_Type::UNSIGNED_SHORT )
    bytesize = shortBytes;
  else
  if ( theType == IR_Type::INT || theType == IR_Type::UNSIGNED_INT )
    bytesize = intBytes;
  else
  if ( theType == IR_Type::LONG || theType == IR_Type::UNSIGNED_LONG )
    bytesize = longBytes;
  else
  if ( theType == IR_Type::LONG_LONG || theType == IR_Type::UNSIGNED_LONG_LONG )
    bytesize = longLongBytes;
  else
  if ( theType == IR_Type::DOUBLE || theType == IR_Type::LONG_DOUBLE )
    bytesize = doubleBytes;
  else
  if ( theType == IR_Type::FLOAT )
    bytesize = floatBytes;
  else
  if ( isPointerType )
    bytesize = pointerBytes;
  else
  if ( isArrayType )
    bytesize = theArrayType->bitSize() / ARMIR_CONFIGURATION->bitwidthAddressable;
  else
  if ( theType == IR_Type::STRUCT || theType == IR_Type::UNION )
    bytesize = Stack::getStackSize( type );

  ufAssertT( bytesize != 0, "Non supported type passed to sizeof." );

  return( bytesize );
}


/*!
   computeSizeOf computes the size of a tree element in bytes.
*/
int computeSizeOf( NODEPTR treeElem )
{
  DSTART( "computeSizeOf( NODEPTR )" );

  int bytesize = 0;
  IR_SizeOfExp *theExp = dynamic_cast<IR_SizeOfExp *>( treeElem->getExp() );


  if ( theExp != 0 )
    bytesize = computeSizeOf( &theExp->getBaseType() );
  else
    bytesize = computeSizeOf( &treeElem->getExp()->getType() );

  return( bytesize );
}

/*!
   Generates a new basic block and inserts it into the current function and
   updates the BackAnnotation mappings by mapping the new block to the given
   IR basic block
 */
static LLIR_BB &beginNewLLIRBasicBlock( const char *name, IR_BasicBlock &bb )
{
  DSTART( "beginNewLLIRBasicBlock( const char *name, const IR_BasicBlock &bb )" );
  return( ARMCODESEL->beginNewLLIRBasicBlock( name, bb ) );
}
/*!
   Wrapper for "beginNewLLIRBasicBlock( const char *name, IR_BasicBlock &bb )"
   that always uses a new unique block name.
 */
static LLIR_BB &beginNewLLIRBasicBlock( IR_BasicBlock &bb )
{
  return ARMCODESEL->beginNewLLIRBasicBlock( bb );
}

/*!
   Generates a new basic block and inserts it into the current function and
   updates the BackAnnotation mappings by inserting a join mapping that marks
   the new basic block and the last LLIR BB as joined together (mapped to the
   same IR BB).
 */
static LLIR_BB &beginNewLLIRBasicBlock( const char *name )
{
  DSTART( "beginNewLLIRBasicBlock( const char *name )" );
  return( ARMCODESEL->beginNewLLIRBasicBlock( name ) );
}
/*!
   Wrapper for "beginNewLLIRBasicBlock( const char *name )"
   that always uses a new unique block name.
 */
static LLIR_BB &beginNewLLIRBasicBlock( void )
{
  return ARMCODESEL->beginNewLLIRBasicBlock();
}


/*!
   Computes whether 'e' is an expression that denotes a memory location where
   the result of an operation must be written to. This f.e. true for cases like

   - 'a[i]' in 'a[i] = 0'
   - '*p' in '*p += 1'
   - 'c->comp' in 'c->comp++'
   - any assignment to global / local stack symbols
 */
static bool isMemoryWriteLocation( const IR_Exp &e )
{
  DSTART( "static bool isMemoryWriteLocation( const IR_Exp &e )" );
  const IR_IndexExp           *iexp = dynamic_cast<const IR_IndexExp*>( &e );
  const IR_UnaryExp           *uexp = dynamic_cast<const IR_UnaryExp*>( &e );
  const IR_ComponentAccessExp *cexp =
    dynamic_cast<const IR_ComponentAccessExp*>( &e );
  const IR_SymbolExp          *sexp = dynamic_cast<const IR_SymbolExp*>( &e );

  const bool isMemoryAccess =
    iexp ||
    cexp ||
    ( uexp && uexp->getOperator() == IR_UnaryExp::DEREF ) ||
    ( sexp && ( sexp->getSymbol().isGlobal() ||
                ARMCODESEL->getStack()->getSymbolOffset(
                  &sexp->getSymbol() ) >= 0 ) );

  DOUT( e.isDef() && isMemoryAccess );
  return e.isDef() && isMemoryAccess;
}


/*!
  Returns the lower and upper 16 bit of the given constant in the
  given pointer output parameters.
 */
void splitUpConstant( int constant, int *out_low, int *out_high )
{
  int const_hi  = ( ( constant + 0x8000 ) >> 16 ) & 0xFFFF;
  int const_low = constant & 0xFFFF;

  if ( const_low > maxSignedConst16Value )
    const_low = minSignedConst16Value - 1 +
      ( const_low - maxSignedConst16Value );
  else
    if ( const_low < minSignedConst16Value )
      const_low = maxSignedConst16Value +
        ( const_low - minSignedConst16Value );

  *out_low  = const_low;
  *out_high = const_hi;
};


/*!
  Loads the memory location that is described by the given lvalue into an
  address register and returns this register.

  'baseReg' holds the base register of the memory access
  'offset' holds the offset (bytes) of the memory access
  'target' should be the name of the reg that should be loaded with the address
           (may be the empty string)
  'loadExp' should be the expression in which the load is performed (if any)
*/
LLIR_Register &loadAccessLocationToReg( LLIR_Register *reg_base, int offset,
                                        string target, IR_Exp *loadExp )
{
  DSTART( "loadAccessLocationToReg" );
  LLIR_Register *reg_target = ARMINSTRUCTIONS.CreateRegister( target, true );

  if ( !offset ) {
    return *reg_base;
  } else {
    if ( offset >= minSignedConst12Value &&
         offset <= maxSignedConst12Value ) {
      ARMINSTRUCTIONS.insertADD( reg_target, reg_base, offset, loadExp );
    } else {
      ARMINSTRUCTIONS.insertMOV_ORR( reg_target, offset, loadExp );
      ARMINSTRUCTIONS.insertADD( reg_target, reg_target, reg_base, loadExp );
    }

    return *reg_target;
  }
}

/*!
  Returns the cost of the instructions that are generated by
  'loadRegisterRelativeAddress' for a given element size.
 */
COST loadRegisterRelativeAddressCost()
{
  COST cost = CT( INS_MOV_32 ) + CT( INS_MUL_32 ) + CT( INS_ADD_32 );
  return cost;
}

/*!
   Loads the address that is denoted by the given parameters into a new
   virtual address register.

   'baseReg' is an address register that holds the base address
   'offsetReg' is a data register that holds the offset from the base address
               in multiples of 'elementByteSize'
   'elementByteSize' is the size of a single element
   'loadExp' should be the expression in which the load is performed (if any)
 */
LLIR_Register *loadRegisterRelativeAddress( LLIR_Register *baseReg,
                                            LLIR_Register *offsetReg,
                                            int elementByteSize, IR_Exp *loadExp )
{
  ufAssertT( elementByteSize >= 0, "Negative size is illegal!" );

  //LLIR_Register *iaddr = ARMINSTRUCTIONS.CreateRegister( "", true );

  LLIR_Register *reg1 = ARMINSTRUCTIONS.CreateRegister( "", false );
  ARMINSTRUCTIONS.insertMOV( reg1, elementByteSize, loadExp );
  ARMINSTRUCTIONS.insertMUL( reg1, offsetReg, reg1, loadExp );
  ARMINSTRUCTIONS.insertADD( reg1, reg1, baseReg, loadExp );

  return reg1;
}

/* Returns an inverted OPER-code, depending on the Tree Element given.
   It gets the label of the treeElem. In case of a binary expression, it returns
   the inverted condition (e.g., tpm_BinaryExpEQ -> OPER_NE ). */
string getInvOper( IR_TreeElem *treeElem, bool sgn )
{
  string condition = OPER_AL;
  int index = treeElem->getLabel();

  switch( index ) {
    case tpm_BinaryExpLT:
      // jump if greater than or equal
      // Check if signed or unsigned.
      if ( sgn )
        condition = OPER_GE;
      else
        condition = OPER_HS;
      break;
    case tpm_BinaryExpGT:
      // jump if less than or equal
      // Check if signed or unsigned.
      if ( sgn )
        condition = OPER_LE;
      else
        condition = OPER_LS;
      break;
    case tpm_BinaryExpLEQ:
      // jump if greater than
      // Check if signed or unsigned.
      if ( sgn )
        condition = OPER_GT;
      else
        condition = OPER_HI;
      break;
    case tpm_BinaryExpGEQ:
      // jump if less than
      // Check if signed or unsigned.
      if ( sgn )
        condition = OPER_LT;
      else
        condition = OPER_LO;
      break;
    case tpm_BinaryExpEQ:
      // the jump should be performed, when the condition is not fullfilled,
      // so not equal
      condition = OPER_NE;
      break;
    case tpm_BinaryExpNEQ:
      // jump, if equal
      condition = OPER_EQ;
      break;
    case tpm_SymbolExp:
      // Comparison against zero.
      condition = OPER_EQ;
      break;
    default:
      // This should also be a comparison against zero.
      condition = OPER_EQ;
      break;
  }

  return condition;
}

/* Returns an OPER-code, depending on the Tree Element given.
   It gets the label of the treeElem. In case of a binary expression, it returns
   the non-inverted condition (e.g., tpm_BinaryExpEQ -> OPER_EQ ). */
string getOper( IR_TreeElem *treeElem, bool sgn )
{
  string condition = OPER_AL;
  int index = treeElem->getLabel();

  switch( index ) {
    case tpm_BinaryExpLT:
      // jump if greater than or equal
      // Check if signed or unsigned.
      if ( sgn )
        condition = OPER_LT;
      else
        condition = OPER_LO;
      break;
    case tpm_BinaryExpGT:
      // jump if less than or equal
      // Check if signed or unsigned.
      if ( sgn )
        condition = OPER_GT;
      else
        condition = OPER_HI;
      break;
    case tpm_BinaryExpLEQ:
      // jump if greater than
      // Check if signed or unsigned.
      if ( sgn )
        condition = OPER_LE;
      else
        condition = OPER_LS;
      break;
    case tpm_BinaryExpGEQ:
      // jump if less than
      // Check if signed or unsigned.
      if ( sgn )
        condition = OPER_GE;
      else
        condition = OPER_HS;
      break;
    case tpm_BinaryExpEQ:
      // jump, if equal
      condition = OPER_EQ;
      break;
    case tpm_BinaryExpNEQ:
      // jump, if not equal
      condition = OPER_NE;
      break;
    case tpm_SymbolExp:
      // Comparison against zero.
      condition = OPER_NE;
      break;
    default:
      // This should also be a comparison against zero.
      condition = OPER_NE;
      break;
  }

   return condition;
}

/*!
   If the function argument was already assigned to a register, then that
   register is returned, else a new register of matching type is created
   and registered as the representation of that argument. The new register
   is then returned.

   'sym' is the function parameter symbol
 */
LLIR_Register &getFunctionArgumentRegister( const IR_Symbol &sym )
{
  DSTART( "LLIR_Register &getFunctionArgumentRegister( const IR_Symbol &sym )" );
  ufAssertT( sym.getSymbolTable().getFunction(),
    "'sym' was no function parameter!" );

  LLIR_Register *result = nullptr;
  string existing_reg = ARMCODESEL->getStack()->getSymbolReg(
    const_cast<IR_Symbol*>( &sym ) );

  if ( existing_reg != "" ) {
    // The function argument symbol was already processed, hence use the
    // virtual register already assigned to the argument symbol.
    if ( isRegType( sym ) || isArrayType( sym ) || isPointerType( sym ) ) {
      result = ARMINSTRUCTIONS.CreateRegister( existing_reg );
    } else if ( isComposedType( sym ) ) {
      result = ARMINSTRUCTIONS.CreateRegister( existing_reg, true );
    } else if ( isERegType( sym ) ) {
      result = ARMINSTRUCTIONS.CreateERegister( existing_reg );
    } else {
      ufAssertT( 0, "Uncovered case!" );
    }

  } else {
    // The function argument symbol was not yet processed, hence declare a
    // new virtual register and assign it to the argument symbol.
    if ( isRegType( sym ) || isArrayType( sym ) || isPointerType( sym ) ) {
      result = ARMINSTRUCTIONS.CreateRegister( "" );
    } else if ( isComposedType( sym ) ) {
      result = ARMINSTRUCTIONS.CreateRegister( "", true );
    } else if ( isERegType( sym ) ) {
      result = ARMINSTRUCTIONS.CreateERegister( "" );
    } else {
      ufAssertT( 0, "Uncovered case!" );
    }
    ARMCODESEL->getStack()->setSymbolReg( const_cast<IR_Symbol*>( &sym ),
                                       result->GetName() );

  }

  // Bind the result to the appropriate physical register
  const int argPos = Stack::isPassedThroughRegister( sym );
  DOUT( "Passed through register: " << argPos << endl );
  ufAssertT( argPos >= 0 && argPos <= 3,
             "Stack class computed invalid argument position!" );
  bindToPHREG( *result, argPos );
  DOUT( "Precolor " << result->GetName() << " with " << argPos << endl );

  return *result;
}

/*!
   If the function argument was already assigned to a register, then that
   register is returned, else a new register of matching type is created
   and registered as the representation of that argument. The new register
   is then returned.

   'funcCall' is the function call expression whose arguments are analyzed
   'argumentIndex' is the index of the argument in the arg. list of 'funcCall'
   'phRegIndex' is the index of the physical register to which the argument
                should be bound (only used for function pointer calls).
 */
LLIR_Register &getFunctionArgumentRegister( const IR_CallExp &funcCall,
                                            unsigned int argumentIndex,
                                            unsigned int phRegIndex )
{
  DSTART( "getFunctionArgumentRegister" );
  LLIR_Register *result = 0;

  IR_Function * const theFunc = ( funcCall.getFunctionSymbol() ?
    funcCall.getFunctionSymbol()->getFunction() : 0 );

  if ( theFunc ) {
    DOUT( "function symbol found" << endl );
    // The function to be called is fully known within ICD-C. Hence, get
    // its argument symbols.
    list<IR_Symbol *> funcArgs = theFunc->functionArguments.getSymbols();
    list<IR_Symbol *>::iterator theArg = funcArgs.begin();
    for ( unsigned int i = 0; i < argumentIndex; ++i, ++theArg ) ;

    result = &getFunctionArgumentRegister( **theArg );

  } else {
    DOUT( "function symbol not found" << endl );
    // When the function that is called is not known (this happens for indirect
    // calls through function pointers), then we must extract the argument type,
    // create an appropriate register and bind it to the respective PHREG.
    const list<IR_Type*> &funcArgTypes =
      funcCall.getFunctionType().getArgumentTypes();
    list<IR_Type*>::const_iterator theArgType = funcArgTypes.begin();
    for ( unsigned int i = 0; i < argumentIndex; ++i, ++theArgType ) ;
    const IR_Type &argumentType = **theArgType;

    if ( isRegType( argumentType ) ) {
      result = ARMINSTRUCTIONS.CreateRegister( "" );
    } else if ( isComposedType( argumentType )
             || isArrayType( argumentType )
             || isPointerType( argumentType ) ) {
       result = ARMINSTRUCTIONS.CreateRegister( "", true );
    } else if ( isERegType( argumentType ) ) {
      result = ARMINSTRUCTIONS.CreateERegister( "" );
    } else {
      ufAssertT( 0, "Uncovered case!" );
    }

    // Bind the result to the appropriate physical register
    bindToPHREG( *result, phRegIndex );
  }

  return *result;
}

/*! For a given pointer/array type, returns the base type, else returns 0. */
static inline IR_Type *getBaseType( const IR_Type &t )
{
  const IR_PointerType *ptype = dynamic_cast<const IR_PointerType*>( &t );
  const IR_ArrayType   *atype = dynamic_cast<const IR_ArrayType*>( &t );

  if ( ptype ) {
    return &ptype->getBaseType();
  } else
  if ( atype ) {
    return &atype->getBaseType();
  } else {
    return 0;
  }
}

/*! For a given pointer/array exp, returns the base type, else returns 0. */
static inline IR_Type *getBaseType( const IR_Exp &exp )
{
  return getBaseType( exp.getType() );
}

/*!
   This function returns the constant integer value associated with the
   IR_TreeElem passed as argument. It returns VALUE_INVALID on error.

   The function should only be used from within the cost computations, in the
   action parts, the appropriate action should be called to obtain the integer
   constant.
*/
long long getConstIntValue( NODEPTR treeElem )
{
  DSTART( "getConstIntValue( NODEPTR )" );

  // Get the constant expression's value considering
  // unary minus and cast expressions, etc.
  ConstCastIntegralResult * const result =
    Cast::constantCastToIntegral( *treeElem->getExp() );
  const long long longlongresult = result
    ? tolonglong( result->value ) : VALUE_INVALID;
  delete result;

  return( longlongresult );
}


//######################################################################
//
// Utility methods
//
//######################################################################

/*
  If the given expression ia a symbol expression that hold a function argument
  symbol, then this function returns 'true', else 'false'.
*/
bool isFunctionArgument( const IR_Exp &exp )
{
  auto *symExp = dynamic_cast<const IR_SymbolExp *>( &exp );
  return(
    symExp && symExp->getSymbol().getSymbolTable().getFunction() != nullptr );
};


/*
  getLowerLongLongWord extracts the lower word from a long long.
*/
ICDInt64 getLowerLongLongWord( const IR_Integer &ll )
{
  DSTART( "getLowerLongLongWord" );
  DOUT( "Getting lower word for " << ll << " ("
    << ( ll.getSign() == IR_Integer::SIGNED ? "signed" : "unsigned" )
    << ")" << endl );

  ICDInt64 result( 0 );

  if ( ll.getSign() == IR_Integer::SIGNED ) {
    ICDInt64 temp = ll.getRawValue( 64, IR_Integer::SIGNED );
    ICDInt64 mask( 0xFFFFFFFF );
    result = temp & mask;
  } else {
    ICDUInt64 temp = ll.getRawValue( 64, IR_Integer::UNSIGNED );
    ICDUInt64 mask( 0xFFFFFFFF );
    result = temp & mask;
  }

  DOUT( "Result: " << result << endl );
  return( result );
};


/*
  getUpperLongLongWord extracts the upper word from a long long.
*/
ICDInt64 getUpperLongLongWord( const IR_Integer &ll )
{
  DSTART( "getUpperLongLongWord" );
  DOUT( "Getting upper word for " << ll << " ("
    << ( ll.getSign() == IR_Integer::SIGNED ? "signed" : "unsigned" )
    << ")" << endl );

  ICDInt64 result( 0 );

  if ( ll.getSign() == IR_Integer::SIGNED ) {

    const bool negativeSign = ll < 0;
    const ICDInt64 lowerword = getLowerLongLongWord( ll );

    ICDInt64 temp = ll.getRawValue( 64, IR_Integer::SIGNED );
    // We don't use '>> 32' here since >> is invalid for negative numbers.
    for ( int i = 0; i < 32; i++ ) {
      temp /= 2;
    }

    // Negative numbers must be treated as a single 64-bit number which is
    // represented in two's complement. As such a number like
    // 0000 0001 0000 0003 (hexadecimal)
    // would become
    // FFFF FFFE FFFF FFFD (hexadecimal)
    // which is:
    //        -2        -3
    // in decimal when regarded as two separate two's complement numbers.
    // So, for any long long which has a nonzero lower word we will get an
    // upper word that is (normal upper word - 1).
    if ( negativeSign && lowerword != 0 ) {
      temp--;
    }
    result = temp;
  } else {
    ICDUInt64 temp = ll.getRawValue( 64, IR_Integer::UNSIGNED );
    result = temp >> 32;
  }

  DOUT( "Result: " << result << endl );
  return( result );
};

/*
  derefCost computes the cost of store instructions that save the result of the
  given expression to a memory location with available memory address.
*/
COST derefCost( const IR_Exp *exp )
{
  DSTART( "COST derefCost(const IR_Exp*)" );

  auto &t = exp->getType();
  COST cost = 0;

  // TODO: The costs for label-based write-back are missing here, but it seems
  //       almost impossible to determine in the costs part whether a label-
  //       based write-back has to be performed, as this is only clear when the
  //       real DerefInfo struct was passed to the action part.
  if ( dynamic_cast<IR_BitfieldType *>( &t ) )
    ufAssertT( 0, "Bitfields not yet implemented!" );
  else {
    // Just decide between 32 bit or 64 bit, all have the same cost otherwise.
    switch ( t.getType() ) {
      case IR_Type::BOOL:
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT:
      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: {
        cost = CT( INS_STR_32 );
        break;
      }

      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE: {
        cost = 2 * CT( INS_STR_32 );
        break;
      }

      case IR_Type::ARRAY: {
        ufAssertT(
          dynamic_cast<const IR_SymbolExp *>(
            exp )->getSymbol(). getSymbolTable().getFunction(),
          "Internal error: Arrays can only be assigned to if they are function "
          "parameters." );
        cost = CT( INS_STR_32 );
        break;
      }

      case IR_Type::POINTER: {
        cost = CT( INS_STR_32 );
        break;
      }

      default:
        break;

    }
  }

  return( cost );
}

