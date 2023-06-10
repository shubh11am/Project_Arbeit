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
  @file rv32_prolog.m4
  @brief This file implements various helper functions used by the RISC-V RV32
         code selector within its tree grammar.
*/


using namespace std;
using namespace WIR;


namespace RV32 {

//
// Preprocessor macros section
//

/*!
  @brief Macro COST_LESS just serves for compatibility with good-old icd-cg.
*/
#define COST_LESS(x,y) CodeSelector::COST_LESS( x, y )


/*!
  @brief Macro IS_CONSTANT_EXP_WRAPPER generates a wrapper function for a given
         constant-format-check function.

  @param[in] resultType The type returned by the generated wrapper function.
  @param[in] funcName The name of the generated wrapper function.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
#define IS_CONSTANT_EXP_WRAPPER( resultType, funcName )                        \
  static inline resultType funcName( const IR_Exp &exp )                       \
  {                                                                            \
    auto *icExp = dynamic_cast<const IR_IntConstExp *>( &exp );                \
    if ( icExp )                                                               \
      return( funcName( icExp->getValue(), icExp->getType() ) );               \
    else                                                                       \
      return( 0 );                                                             \
  };


//
// Global variables section
//

/*!
  @brief RV32_wirProc points to the %WIR processor for which code is currently
         generated.
*/
extern RV32IMC *RV32_wirProc;


/*!
  @brief RV32_wirFct points to the %WIR function within which code is currently
         generated.
*/
extern WIR_Function *RV32_wirFct;


/*!
  @brief RV32_wirBB points to the %WIR basic block within which code is
         currently generated.
*/
extern WIR_BasicBlock *RV32_wirBB;


/*!
  @brief dummyRegV is just for the initialization of reg non-terminals only.
*/
RV_RegV dummyRegV;


/*!
  @brief logAndEndLabel stores the name of a label that is used by the
         tpm_BinaryExpLOGAND rules to generate a conditional jump if the
         left-hand side of the logical operator already evaluates to false.
*/
static string logAndEndLabel = "";


/*!
  @brief logAndEndBlock points to a %WIR basic block to be used by the
         tpm_BinaryExpLOGAND rules in order to generate a conditional jump if
         the left-hand side of the logical operator already evaluates to false.
*/
static WIR_BasicBlock *logAndEndBlock = nullptr;


/*!
  @brief logOrEndLabel stores the name of a label that is used by the
         tpm_BinaryExpLOGOR rules to generate a conditional jump if the
         left-hand side of the logical operator already evaluates to true.
*/
static string logOrEndLabel = "";


/*!
  @brief logOrEndBlock points to a %WIR basic block to be used by the
         tpm_BinaryExpLOGOR rules in order to generate a conditional jump if the
         left-hand side of the logical operator already evaluates to true.
*/
static WIR_BasicBlock *logOrEndBlock = nullptr;


//
// Code section
//

/*
  DEBUG_RULE_ACTION prints out which action part of which rule is entered, and
  for which C expression. If debugging of WCC is disabled, DEBUG_RULE_ACTION
  does nothing.

  DEBUG_RULE_ACTION should be invoked right at the beginning of the action part
  of each rule.

  This function is named in uppercase letters against the usual convention,
  which would force us to use lowercase letters, because it mimics a debugmacro
  functionality and all the debugmacro stuff is written in uppercase letters.
*/
void DEBUG_RULE_ACTION( const string &ruleSignature, NODEPTR treeElem )
{
  #ifdef DEBUG_WCC
  stringstream ss;

  DSTART_EXT(
    const_cast<char *>( ruleSignature.c_str() ),
    "CodeSelector::RulesetActions" );

  if ( treeElem->getExp() ) {
    treeElem->getExp()->write( ss );

    // If debug output is written to cout, the expression is printed in bold.
    if ( string( "cout" ).compare( CODESEL_DBG_TARGET ) == 0 )
      DOUT( "\33[1m[ " << ss.str() << " ]\33[0m" << endl );
    else
      DOUT( "[ " << ss.str() << " ]" << endl );
  } else

  if ( treeElem->getStmt() ) {
    treeElem->getStmt()->write( ss );

    // If debug output is written to cout, the statment is printed in bold.
    if ( string( "cout" ).compare( CODESEL_DBG_TARGET ) == 0 )
      DOUT( "\33[1m" << ss.str() << "\33[0m" << endl );
    else
      DOUT( ss.str() << endl );
  }
  #endif
};


/*
  isBounded32BitConstant determines whether the given IR value of the given type
  denotes an integer constant of at most 32 bits width, whose value is in the
  interval [minValue, maxValue] and whose signedness is equivalent to isSigned.
*/
bool isBounded32BitConstant( const IR_Integer &v, const IR_Type &t,
                             int minValue, int maxValue, bool isSigned )
{
  DSTART(
    "bool isBounded32BitConstant(const IR_Integer&, const IR_Type&, int, int, "
    "bool)" );

  return(
    ( v.getIntValue() >= minValue ) && ( v.getIntValue() <= maxValue ) &&
    ( ( isSigned && t.isSignedType() ) ||
      ( !isSigned && t.isUnsignedType() ) ) &&
    ( t.bitSize() <= 32 ) );
};


/*
  isUConst5 determines whether the given IR integer is a constant that can be
  represented by the 'uconst5' nonterminal.
*/
bool isUConst5( const IR_Integer &v, const IR_Type &t )
{
  DSTART( "bool isUConst5(const IR_Integer&, const IR_Type&)" );

  return(
    isBounded32BitConstant(
      v, t, 0, RV_Const5_Unsigned::getMaxValue( 5 ), false ) );
};

IS_CONSTANT_EXP_WRAPPER( bool, isUConst5 );


/*
  isConst6 determines whether the given IR integer is a constant that can be
  represented by the 'const6' nonterminal.
*/
bool isConst6( const IR_Integer &v, const IR_Type &t )
{
  DSTART( "bool isConst6(const IR_Integer&, const IR_Type&)" );

  return(
    isBounded32BitConstant(
      v, t, RV_Const6_Signed::getMinValue( 6 ),
      RV_Const6_Signed::getMaxValue( 6 ), true ) );
};

IS_CONSTANT_EXP_WRAPPER( bool, isConst6 );


/*
  isUConst6 determines whether the given IR integer is a constant that can be
  represented by the 'uconst6' nonterminal.
*/
bool isUConst6( const IR_Integer &v, const IR_Type &t )
{
  DSTART( "bool isUConst6(const IR_Integer&, const IR_Type&)" );

  return(
    isBounded32BitConstant(
      v, t, 0, RV_Const6_Unsigned::getMaxValue( 6 ), false ) );
};

IS_CONSTANT_EXP_WRAPPER( bool, isUConst6 );


/*
  isUConst8 determines whether the given IR integer is a constant that can be
  represented by the 'uconst8' nonterminal.
*/
bool isUConst8( const IR_Integer &v, const IR_Type &t )
{
  DSTART( "bool isUConst8(const IR_Integer&, const IR_Type&)" );

  return(
    isBounded32BitConstant(
      v, t, 0, RV_Const8_Unsigned::getMaxValue( 8 ), false ) );
};

IS_CONSTANT_EXP_WRAPPER( bool, isUConst8 );


/*
  isConst12 determines whether the given IR integer is a constant that can be
  represented by the 'const12' nonterminal.
*/
bool isConst12( const IR_Integer &v, const IR_Type &t )
{
  DSTART( "bool isConst12(const IR_Integer&, const IR_Type&)" );

  return(
    isBounded32BitConstant(
      v, t, RV_Const12_Signed::getMinValue( 12 ),
      RV_Const12_Signed::getMaxValue( 12 ), true ) );
}

IS_CONSTANT_EXP_WRAPPER( bool, isConst12 );


/*
  isUConst20 determines whether the given IR integer is a constant that can be
  represented by the 'uconst20' nonterminal.
*/
bool isUConst20( const IR_Integer &v, const IR_Type &t )
{
  DSTART( "bool isUConst20(const IR_Integer&, const IR_Type&)" );

  return(
    isBounded32BitConstant(
      v, t, 0, RV_Const20_Unsigned::getMaxValue( 20 ), false ) );
};

IS_CONSTANT_EXP_WRAPPER( bool, isUConst20 );


/*
  computeSizeOf computes the size of an IR type in bytes.
*/
int computeSizeOf( const IR_Type *t )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  int bytesize = 0;
  auto theType = t->getType();

  auto *ptrType = dynamic_cast<const IR_PointerType *>( t );
  auto *arrType = dynamic_cast<const IR_ArrayType *>( t );

  const bool isPtrType = ( ptrType != nullptr ) && ( arrType == nullptr );
  const bool isArrType = ( ptrType != nullptr ) && ( arrType != nullptr );

  if ( theType == IR_Type::BOOL )
    bytesize = RV32I::boolBytes;
  else

  if ( ( theType == IR_Type::CHAR ) || ( theType == IR_Type::UNSIGNED_CHAR ) )
    bytesize = RV32I::charBytes;
  else

  if ( ( theType == IR_Type::SHORT ) || ( theType == IR_Type::UNSIGNED_SHORT ) )
    bytesize = RV32I::shortBytes;
  else

  if ( ( theType == IR_Type::INT ) || ( theType == IR_Type::UNSIGNED_INT ) )
    bytesize = RV32I::intBytes;
  else

  if ( ( theType == IR_Type::LONG ) || ( theType == IR_Type::UNSIGNED_LONG ) )
    bytesize = RV32I::longBytes;
  else

  if ( ( theType == IR_Type::LONG_LONG ) ||
       ( theType == IR_Type::UNSIGNED_LONG_LONG ) )
    bytesize = RV32I::longLongBytes;
  else

  if ( theType == IR_Type::FLOAT )
    bytesize = RV32I::floatBytes;
  else

  if ( theType == IR_Type::DOUBLE )
    bytesize = RV32I::doubleBytes;
  else

  if ( theType == IR_Type::LONG_DOUBLE )
    bytesize = RV32I::longDoubleBytes;
  else

  if ( isPtrType )
    bytesize = RV32I::pointerBytes;
  else

  if ( isArrType )
    bytesize = arrType->bitSize() / RVIR_CONFIGURATION->bitwidthAddressable;
  else
    ufAssertT( bytesize != 0, "Unsupported type passed to sizeof." );

  return( bytesize );
};


/*
  computeSizeOf computes the size of a tree element in bytes.
*/
int computeSizeOf( const NODEPTR treeElem )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  int bytesize = 0;
  auto *e = const_cast<NODEPTR>( treeElem );
  auto *exp = dynamic_cast<IR_SizeOfExp *>( e->getExp() );

  if ( exp != nullptr )
    bytesize = computeSizeOf( &exp->getBaseType() );
  else
    bytesize = computeSizeOf( &e->getExp()->getType() );

  return( bytesize );
};


/*
  getFctArgReg determines the pre-colored virtual WIR register to be used for
  passing an IR symbol as function argument.

  If the function argument symbol was already assigned to a register previously,
  that register is returned. Otherwise, a new register of matching type is
  created, pre-colored and registered as the representative of the given
  function argument. The new register is then returned.
*/
WIR_VirtualRegister &getFctArgReg( const IR_Symbol &sym )
{
  DSTART( BOOST_CURRENT_FUNCTION );
  if ( RVCODESEL.getStack().isSymbolRegSet( sym ) ) {
    auto &r = RVCODESEL.getStack().getSymbolReg( sym );

    if ( r.getFunction() == *RV32::RV32_wirFct )
      // The function argument symbol was already processed. Hence, use the
      // virtual register already assigned to the argument symbol.
      return( RVCODESEL.getStack().getSymbolReg( sym ) );
  }

  // The function argument symbol was not yet processed. Hence, create a new
  // virtual register and assign it to the argument symbol.
  WIR_VirtualRegister &r = RVINSTRUCTIONS.createReg();

  RVCODESEL.getStack().setSymbolReg( sym, r ); 

  // Pre-color the result register with the appropriate physical register.
  bindToPHREG( r, RVCODESEL.getStack().isPassedThroughRegister( sym ) );

  return( r );
};


/*
  getFctArgReg determines the pre-colored virtual WIR register to be used for
  passing a function argument.

  If the function argument symbol was already assigned to a register previously,
  that register is returned. Otherwise, a new register of matching type is
  created, pre-colored and registered as the representative of the given
  function argument. The new register is then returned.
*/
WIR_VirtualRegister &getFctArgReg( const IR_CallExp &exp, unsigned int argIdx,
                                   unsigned int phRegIdx )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  IR_Function *f =
    exp.getFunctionSymbol() ?
      exp.getFunctionSymbol()->getFunction() : nullptr;

  if ( f ) {
    // The function to be called is fully known within ICD-C. Hence, get its
    // argument symbols.
    list<IR_Symbol *> funcArgs = f->functionArguments.getSymbols();
    list<IR_Symbol *>::iterator theArg = funcArgs.begin();
    for ( unsigned int i = 0; i < argIdx; ++i, ++theArg ) ;

    return( getFctArgReg( **theArg ) );
  }

  // If the called IR function is not known (this happens for indirect calls via
  // function pointers), we must extract the argument type, create an
  // appropriate register and bind it to the respective PHREG.
  const list<IR_Type *> &funcArgTypes =
    exp.getFunctionType().getArgumentTypes();
  list<IR_Type *>::const_iterator theArgType = funcArgTypes.begin();
  for ( unsigned int i = 0;
        ( i < argIdx ) && ( i < funcArgTypes.size() - 1 );
        ++i, ++theArgType ) ;

  WIR_VirtualRegister &r = RVINSTRUCTIONS.createReg();

  // Pre-color the result register with the appropriate physical register.
  bindToPHREG( r, phRegIdx );

  return( r );
};


/*
  bindToPHREG precolors the given virtual WIR register with a physical RISC-V
  register.
*/
void bindToPHREG( const WIR_VirtualRegister &r, unsigned int phregNumber )
{
  DSTART( "void bindToPHREG(const WIR_VirtualRegister&, unsigned int)" );

  // Precolor register.
  switch ( phregNumber ) {
    case 0: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x0() );
      break;
    }

    case 1: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x1() );
      break;
    }

    case 2: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x2() );
      break;
    }

    case 3: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x3() );
      break;
    }

    case 4: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x4() );
      break;
    }

    case 5: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x5() );
      break;
    }

    case 6: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x6() );
      break;
    }

    case 7: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x7() );
      break;
    }

    case 8: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x8() );
      break;
    }

    case 9: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x9() );
      break;
    }

    case 10: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x10() );
      break;
    }

    case 11: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x11() );
      break;
    }

    case 12: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x12() );
      break;
    }

    case 13: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x13() );
      break;
    }

    case 14: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x14() );
      break;
    }

    case 15: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x15() );
      break;
    }

    case 16: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x16() );
      break;
    }

    case 17: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x17() );
      break;
    }

    case 18: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x18() );
      break;
    }

    case 19: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x19() );
      break;
    }

    case 20: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x20() );
      break;
    }

    case 21: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x21() );
      break;
    }

    case 22: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x22() );
      break;
    }

    case 23: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x23() );
      break;
    }

    case 24: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x24() );
      break;
    }

    case 25: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x25() );
      break;
    }

    case 26: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x26() );
      break;
    }

    case 27: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x27() );
      break;
    }

    case 28: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x28() );
      break;
    }

    case 29: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x29() );
      break;
    }

    case 30: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x30() );
      break;
    }

    case 31: {
      RV32::RV32_wirFct->insertPrecolor( r, RV32::RV32_wirProc->x31() );
      break;
    }

    default: {
      ufAssertT( 0, "Invalid register index." );
      break;
    }
  }
};


/*
  getNewRegister creates a new virtual RISC-V WIR register for the given IR
  type.
*/
WIR_VirtualRegister &getNewRegister( const IR_Type &t )
{
  DSTART( "WIR_VirtualRegister& RV32::getNewRegister(const IR_Type&)" );

  return( static_cast<WIR_VirtualRegister &>( RVINSTRUCTIONS.createReg() ) );
};


/*
  getNewRegister creates a new virtual RISC-V WIR register for the given IR
  symbol expression.
*/
WIR_VirtualRegister &getNewRegister( const IR_SymbolExp &symExp )
{
  DSTART( "WIR_VirtualRegister& RV32::getNewRegister(const IR_SymbolExp&)" );

  return( getNewRegister( symExp.getType() ) );
};


/*
  Computes whether 'e' is an expression that denotes a memory location where the
  result of an operation must be written to. This is true for cases like, e.g.,

  - 'a[i]' in 'a[i] = 0'
  - '*p' in '*p += 1'
  - 'c->comp' in 'c->comp++'
  - any assignment to global / local stack symbols
*/
bool isMemoryWriteLocation( const IR_Exp &e )
{
  auto *iexp = dynamic_cast<const IR_IndexExp *>( &e );
  auto *uexp = dynamic_cast<const IR_UnaryExp *>( &e );
  auto *cexp = dynamic_cast<const IR_ComponentAccessExp *>( &e );
  auto *sexp = dynamic_cast<const IR_SymbolExp *>( &e );

  const bool isMemoryAccess =
    iexp ||
    cexp ||
    ( uexp && ( uexp->getOperator() == IR_UnaryExp::DEREF ) ) ||
    ( sexp &&
      ( sexp->getSymbol().isGlobal() ||
        RVCODESEL.getStack().getSymbolOffset( sexp->getSymbol() ) >= 0 ) );

  return( e.isDef() && isMemoryAccess );
};


/*
  loadGlobalSymbolCost computes the costs for loading a global symbol into an
  appropriate register.
*/
COST loadGlobalSymbolCost( const IR_SymbolExp &symExp )
{
  DSTART( "COST loadGlobalSymbolCost(const IR_SymbolExp&)" );

  COST cost = RV32I::OperationFormat::RL_2.getSize();

  switch( symExp.getType().getType() ) {
    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::FLOAT:
    case IR_Type::POINTER:
    case IR_Type::ARRAY: {
      cost += RV32I::OperationFormat::RC12R_1.getSize();
      break;
    }

    default: {
      cost = COST_INFINITY;
      break;
    }
  }

  return( cost );
};


/*
  loadGlobalSymbol generates code that loads a global symbol into its
  appropriate register.
*/

RV32_LValue loadGlobalSymbol( const IR_SymbolExp &symExp, bool loadResult)
{
  DSTART( "RV32_LValue loadGlobalSymbol(const IR_SymbolExp&, bool)" );

  // Determine the symbol's label.
  IR_Symbol &sym = symExp.getSymbol();
  WIR_VirtualRegister *r = nullptr;
  auto &tmpReg = RVINSTRUCTIONS.createReg();

  string label =
    ( sym.getType().getStorageClass() == IR_Type::STATIC ) ?
      RVCODESEL.getStaticName( &sym ) : sym.getWrittenName();

  if ( sym.getType().getType() == IR_Type::FUNCTION ) {
    auto &f = RVCODESEL.getWIRFunction( sym );
    auto lp = WIR_Data(f.getName());

    // Load the symbol's address.
    if(loadResult){
      r = &(getNewRegister( symExp ));
      

      RVINSTRUCTIONS.insertLUI( tmpReg, lp, &symExp );
      RVINSTRUCTIONS.insertLW(
        dynamic_cast<RV_RegV &>( *r ), dynamic_cast<WIR_Data &>( f ), tmpReg, &symExp );
    }
    return(
      RV32::RV32_LValue( r, f, &sym.getType() ) );
  }

  else {
    auto &d = RVCODESEL.getWIRData( sym );

    // Load the symbol's address.
    if(loadResult){
      r = &(getNewRegister( symExp ));
      auto &tmpReg = RVINSTRUCTIONS.createReg();

      RVINSTRUCTIONS.insertLUI( tmpReg, d, &symExp );

      switch ( sym.getType().getType() ) {
        case IR_Type::INT:
        case IR_Type::UNSIGNED_INT:
        case IR_Type::LONG:
        case IR_Type::UNSIGNED_LONG:
        case IR_Type::FLOAT:
        case IR_Type::POINTER:
        case IR_Type::ARRAY: {
          RVINSTRUCTIONS.insertLW(
            dynamic_cast<RV_RegV &>( *r ), d, tmpReg, &symExp );
          break;
        }

        default:
          ufAssertT( false, "Unsupported global symbol type!" );
          break;
      }
    }

    // Assemble an ARM_AddressModification with no offset.
    //IR_Type* baseType = &symExp.getType();
    //RV32::RV32_AddressModification amod( tmpReg, 0, baseType, false );

    //return(
    //  RV32::RV32_LValue { r, amod } );
    //return(
    //  TC_LValue { loadResult ? reg : nullptr, label, r, d, &sym.getType() } );

    return(
    RV32::RV32_LValue { label, r, d, &sym.getType() });
  }
};
/*
  loadGlobalSymbol generates code that loads a global symbol into its
  appropriate register.
*/
/*
RV32_LValue loadGlobalSymbol( const IR_SymbolExp &symExp, bool loadResult )
{
  DSTART( "RV32_LValue loadGlobalSymbol(const IR_SymbolExp&, bool)" );

  // Determine the symbol's label.
  IR_Symbol &sym = symExp.getSymbol();
  string label =
    ( sym.getType().getStorageClass() == IR_Type::STATIC ) ?
      RVCODESEL.getStaticName( &sym ) : sym.getWrittenName();

  // LLIR
  //LLIR_Register * const reg = getNewRegister( "", symExp );
  //LLIR_Register *iaddr = nullptr;

  //if ( RVCODESEL.getStack().getAddrReg( sym ) != "" ) {
  //  auto addrReg = RVCODESEL.getStack().getAddrReg( sym );
    //iaddr = TCINSTRUCTIONS.CreateRegister( addrReg, true );
  //}

  // WIR
  WIR_VirtualRegister *r = nullptr;

  if ( sym.getType().getType() == IR_Type::FUNCTION ) {
    auto &f = RVCODESEL.getWIRFunction( sym );
    auto lp = WIR_Data(f.getName());

    // Load the symbol's address.
    if(loadResult){
      r = &(getNewRegister( symExp ));
      auto &tmpReg = RVINSTRUCTIONS.createReg();

      RVINSTRUCTIONS.insertLUI( tmpReg, lp, &symExp );
      RVINSTRUCTIONS.insertLW(
        dynamic_cast<RV_RegV &>( *r ), dynamic_cast<WIR_Data &>( f ), tmpReg, &symExp );
    }
    return(
      RV32::RV32_LValue( r, f, &sym.getType() ) );
  } else {
    auto &d = RVCODESEL.getWIRData( sym );

    // Load the symbol's address.
    if ( loadResult ) {
      r = &(getNewRegister( symExp ));
      auto &tmpReg = RVINSTRUCTIONS.createReg();

      RVINSTRUCTIONS.insertLUI( tmpReg, d, &symExp );

      switch ( sym.getType().getType() ) {
        case IR_Type::INT:
        case IR_Type::UNSIGNED_INT:
        case IR_Type::LONG:
        case IR_Type::UNSIGNED_LONG:
        case IR_Type::FLOAT:
        case IR_Type::POINTER: {
          RVINSTRUCTIONS.insertLW(
            dynamic_cast<RV_RegV &>( *r ), d, tmpReg, &symExp );
          break;
        }

        default:
          ufAssertT( false, "Unsupported global symbol type!" );
          break;
      }
    }

    return(
      RV32::RV32_LValue { label, r, d, &sym.getType() } );
  }
};*/

/*
  loadStackSymbolCost computes the costs for loading a local stack symbol into
  its appropriate register.
*/
COST loadStackSymbolCost( const IR_SymbolExp &symExp )
{
  DSTART( "COST loadStackSymbolCost(const IR_SymbolExp&)" );

  COST cost = 0;

  switch ( symExp.getType().getType() ) {
    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::FLOAT: 
    case IR_Type::POINTER:{
      cost += RV32I::OperationFormat::RC12R_1.getSize();
      break;
    }

    default: {
      cost = COST_INFINITY;
      break;
    }
  }

  return( cost );
};


/*
  loadStackSymbol generates code that loads a local stack symbol into its
  appropriate register.
*/
RV32_LValue loadStackSymbol( const IR_SymbolExp &symExp, bool loadResult )
{
  DSTART( "RV32_LValue loadStackSymbol(const IR_SymbolExp&, bool)" );

  IR_Symbol &sym = symExp.getSymbol();
  auto *symbolInfo = RVCODESEL.getStack().getSymbolInfo( sym );
  ufAssertT( symbolInfo != nullptr, "Missing symbol info!" );
  const int off = RVCODESEL.getStack().getSymbolOffset( sym );

  // Calculate frame pointer relative offset using the stack frame size
  // and the offset given in relation to the stack pointer

  IR_Function *irFunc = sym.getSymbolTable().getFunction();
  if (irFunc == nullptr){
    irFunc = &sym.getSymbolTable().getCompoundStmt()->getFunction();
  }
  ufAssertT( irFunc != nullptr, "Missing symbol IR function!" );
  int stackFrameSize = RVCODESEL.getStack().getStackFrameSize( *irFunc );
  int paramFrameSize = RVCODESEL.getStack().getParameterStackFrameSize(
                        irFunc->getSymbol().getType() );
  int stackAdjOffset = stackFrameSize - paramFrameSize;
  int fpOffset = off - stackAdjOffset + 8;

  auto &r = getNewRegister( symExp );
  auto &fp = RVINSTRUCTIONS.createReg();
  RV32::RV32_wirFct->insertPrecolor( fp, RV32::RV32_wirProc->x8() );

  // First do some administration work to keep the Stack object up to date. We
  // don't load the symbol yet, we let the control flow pass in all branches
  // where the symbol must be loaded and load it in the end. This centralizes
  // the code for loading at the end of this function.
  if ( symbolInfo->getSymbolType() == RV32_SymbolInfo::Type::R_ARGUMENT ) {
    // Current symbol is a function argument.
    const int myRegNumber = RVCODESEL.getStack().isPassedThroughRegister( sym );
    if ( myRegNumber != -1) {
      // Retrieve/create the function argument symbol's WIR register.
      if ( !symbolInfo->isSymbolRegSet() )
        symbolInfo->setSymbolReg( getNewRegister( symExp ) );
      auto &tmpReg = symbolInfo->getSymbolReg();

      // Generate constraints for register allocator w.r.t. LC registers.
      bindToPHREG( tmpReg, myRegNumber );

      if ( RVCODESEL.getStack().isAddressTaken( sym ) ) {
        if ( !RVCODESEL.getStack().areStoreInstructionsGenerated( sym ) ) {
          // If the address of a function argument passed in a register is
          // taken, we need to store the argument at the already reserved
          // stack position so that its address can be taken legally.

          auto &b = RV32::RV32_wirFct->begin()->get();

          switch ( sym.getType().getType() ) {
            case IR_Type::CHAR:
            case IR_Type::UNSIGNED_CHAR:
            case IR_Type::BOOL:
            case IR_Type::SHORT:
            case IR_Type::UNSIGNED_SHORT:
            case IR_Type::INT:
            case IR_Type::UNSIGNED_INT:
            case IR_Type::LONG:
            case IR_Type::UNSIGNED_LONG:
            case IR_Type::FLOAT: 
            case IR_Type::POINTER:{
              b.pushBackInstruction(
                { { RV32I::OpCode::SW, RV32I::OperationFormat::RC12R_2,
                    new WIR_RegisterParameter( fp, WIR_Usage::use ),
                    new RV_Const12_Signed( fpOffset ),
                    new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
              break;
            }

            default:
              break;
          }

          RVCODESEL.getStack().setStoreInstructionsGenerated( sym, true );
        }
      } else {
        ufAssertT( 0, "Cost function should exclude this" );
      }
    } else {
      // The symbol is passed via the stack.
      // TODO: We could load it each time it is accessed, but it is more clever
      //       to only load it once and then to use the loaded version for
      //       further accesses. This only works if the argument's address has
      //       not been taken. The problem with the current setup is that we can
      //       not do this at the moment, because for such a stack-passed
      //       argument, this function is called inevitably by the rules.
      //       Unfortunately, this function just returns a RV_LValue, whereas
      //       we'd need to return the proper register. So, what we need are new
      //       rules which produce a 'reg' for stack-passed
      //       arguments as described above.

      RVCODESEL.getStack().setSymbolReg( sym, r );
    }
  }

  // Now finally do the access (the location is always the same).
  if(loadResult){
    // For the sake of efficiency, use different load instructions depending on
    // the type of the variable.

    // WIR
    switch ( sym.getType().getType() ) {
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL:
      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT:
      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: 
      case IR_Type::POINTER:{
        RVINSTRUCTIONS.insertLW(
          dynamic_cast<RV_RegV &>( r ), (int)fpOffset, fp, &symExp );
        break;
      }

      default:
        break;
    }
  }

  return(
    RV32::RV32_LValue( loadResult ? &r : nullptr,
      RV32_AddressModification { fp, fpOffset, &sym.getType(), true } ) );
};


/*
  loadRegisterSymbolCost computes the cost for using loadRegSym( symExp ).
*/
COST loadRegisterSymbolCost( const IR_SymbolExp &symExp )
{
  DSTART( "COST loadRegisterSymbolCost(const IR_SymbolExp&)" );

  return( RV32I::OperationFormat::RR_1.getSize() );
};


/*
  loadRegSym loads a non-stack, non-global symbol into its appropriate register,
  as determined by the settings in the Stack class and then returns the
  register.
*/
WIR_BaseRegister &loadRegSym( const IR_SymbolExp &symExp )
{
  DSTART( "WIR_BaseRegister& RV32::loadRegSym(const IR_SymbolExp&)" );

  IR_Symbol &sym = symExp.getSymbol();
  auto *symbolInfo = RVCODESEL.getStack().getSymbolInfo( sym );
  ufAssertT( symbolInfo != nullptr, "Missing symbol info!" );

  if ( symbolInfo->getSymbolType() == RV32_SymbolInfo::Type::R_ARGUMENT ) {
    // Current symbol is a function argument.
    const int myRegNumber = RVCODESEL.getStack().isPassedThroughRegister( sym );
    #ifdef DEBUG_WCC
    ufAssertT( myRegNumber != -1, "Cost function should prevent this." );
    #endif

    // Retrieve/create the function argument symbol's WIR register.
    if ( symbolInfo->isSymbolRegSet() ) {
      auto &r = symbolInfo->getSymbolReg();

      if ( r.getFunction() != *RV32_wirFct )
        symbolInfo->setSymbolReg( getNewRegister( symExp ) );
    } else
      symbolInfo->setSymbolReg( getNewRegister( symExp ) );
    WIR_VirtualRegister &vreg = symbolInfo->getSymbolReg();

    // Generate constraints for register allocator.
    bindToPHREG( vreg, myRegNumber );

    // Annotate the pre-colored VREG as function input.
    vreg.getFunction().addFunctionInput( vreg.getPrecolor() );

    // If not yet existing, create the function-internal virtual WIR register.
    if ( !symbolInfo->isInternalVRegSet() ) {
      symbolInfo->setInternalVReg( getNewRegister( symExp ) );
      WIR_VirtualRegister &reg0 = symbolInfo->getInternalVReg();

      // Add a MOV from the external to the internal VREG at the very beginning
      // of the current WIR function in order to save the argument.
      WIR_BasicBlock &b = RV32_wirFct->begin()->get();

      b.pushBackInstruction(
        { { RV32I::OpCode::MOV,
            RV32I::OperationFormat::RR_1,
            new WIR_RegisterParameter( reg0, WIR_Usage::def ),
            new WIR_RegisterParameter( vreg, WIR_Usage::use ) } } );

      return( reg0 );
    }

    return( symbolInfo->getInternalVReg() );
  }

  #ifdef DEBUG_WCC
  ufAssert( symbolInfo->getSymbolType() == RV32_SymbolInfo::Type::LOCAL_VAR );
  #endif

  // Retrieve/create the function argument symbol's WIR register.
  if ( !symbolInfo->isSymbolRegSet() )
    symbolInfo->setSymbolReg( getNewRegister( symExp ) );
  return( symbolInfo->getSymbolReg() );
};


/*
  isFunctionType returns whether a given type is a function type.
*/
bool isFunctionType( const IR_Type &t )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  return(
    ( t.getType() == IR_Type::FUNCTION ) ||
    // The IR produces wrong type objects with invalid type IDs from time to
    // time. So, we must try to cast the object as well, because in those cases,
    // this is the only thing that works.
    dynamic_cast<const IR_FunctionType *>( &t ) );
};

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
  isPointerType returns whether a given type is a pointer type.
*/
bool isPointerType( const IR_Type &t )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  return(
    ( t.getType() == IR_Type::POINTER ) ||
    // The IR produces wrong type objects with invalid type IDs from time to
    // time. So, we must try to cast the object as well, because in those cases,
    // this is the only thing that works.
    (  dynamic_cast<const IR_PointerType*>( &t ) &&
             !dynamic_cast<const IR_ArrayType*>(   &t ) ) );
};

/*! Returns whether the given type is an array type. */
bool isArrayType( const IR_Type &t )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  return ( t.getType() == IR_Type::ARRAY ||
           // The IR produces wrong type objects with invalid type ids from
           // time to time, so we must try to cast the object as well because
           // in those cases this is the only thing that works
           dynamic_cast<const IR_ArrayType*>(   &t ) );
}

/*
  Returns whether the given 'exp' is an address operator expression that must be
  handled without emitting any code, like, e.g., &*a, &a[i] or the like (see
  ANSI C sec. 6.5.3.2.).
*/
bool isZeroOpADDR( const IR_Exp &exp )
{
  auto *uexp = dynamic_cast<const IR_UnaryExp *>( &exp );
  if ( uexp &&
       ( uexp->getOperator() == IR_UnaryExp::ADDR ) ) {
    auto *op_uexp = dynamic_cast<IR_UnaryExp *>( &uexp->getOp() );
    auto *op_iexp = dynamic_cast<IR_IndexExp *>( &uexp->getOp() );

    return(
      op_iexp ||
      ( op_uexp && ( op_uexp->getOperator() == IR_UnaryExp::DEREF ) ) );
  }

  return( false );
};

/*
  Returns whether the given type is a type that can be represented by
  nonterminal 'dreg'.
*/
bool isDRegType( const IR_Type &t )
{
  return(
    ( t.getType() == IR_Type::VOID ) || ( t.getType() == IR_Type::CHAR ) ||
    ( t.getType() == IR_Type::UNSIGNED_CHAR ) ||
    ( t.getType() == IR_Type::SHORT ) ||
    ( t.getType() == IR_Type::UNSIGNED_SHORT ) ||
    ( t.getType() == IR_Type::INT ) ||
    ( t.getType() == IR_Type::UNSIGNED_INT ) ||
    ( t.getType() == IR_Type::LONG ) ||
    ( t.getType() == IR_Type::UNSIGNED_LONG ) ||
    ( t.getType() == IR_Type::FLOAT ) || ( t.getType() == IR_Type::BOOL ) );
};

/*! Returns whether the given type needs an address register. */
bool isARegType( const IR_Type &t )
{
  return(
    ( t.getType() == IR_Type::POINTER ) ||
    ( t.getType() == IR_Type::ARRAY ) ||
    // The IR produces wrong type objects with invalid type ids from time to
    // time. So, we must try to cast the object as well, because in those cases,
    // this is the only thing that works.
    dynamic_cast<const IR_PointerType *>( &t ) );
};

/*! Returns whether the given type is a composed type. */
bool isComposedType( const IR_Type &type )
{
  return ( type.getType() == IR_Type::STRUCT ||
           type.getType() == IR_Type::UNION  ||
           // The IR produces wrong type objects with invalid type ids from
           // time to time, so we must try to cast the object as well because
           // in those cases this is the only thing that works
           dynamic_cast<const IR_ComposedType*>( &type ) );
}

/*! Returns whether the given type is a type that can be
 * represented by nonterminal 'ereg'. */
bool isERegType( const IR_Type &type )
{
  return ( type.getType() == IR_Type::DOUBLE             ||
           type.getType() == IR_Type::LONG_DOUBLE        ||
           type.getType() == IR_Type::LONG_LONG          ||
           type.getType() == IR_Type::UNSIGNED_LONG_LONG );
}

bool isAddrOffset( const IR_Type &t )
{
  return( t.bitSize() <= 32 );
};

IR_Type *getBaseType( const IR_Type &t )
{
  const IR_PointerType *ptype = dynamic_cast<const IR_PointerType*>( &t );
  const IR_ArrayType   *atype = dynamic_cast<const IR_ArrayType*>( &t );

  if ( ptype ) {
    return( &ptype->getBaseType() );
  } else

  if ( atype ) {
    return( &atype->getBaseType() );
  } else {
    return( nullptr );
  }
};


/*!
  Return the dry run version of the lvalue, only containing the stack offset
  of the symbol expression and not the address register.
*/
RV32::RV32_LValue loadStackSymbolDryRun( IR_SymbolExp &symExp ) {
  DSTART( "RV32_LValue loadStackSymbolDryRun(IR_SymbolExp&)" );

  // Acquire the stack offset in bytes.
  IR_Symbol &sym = symExp.getSymbol();
  const int offset = RVCODESEL.getStack().getSymbolOffset( sym );

  // And bundle it together with the baseType in a dry-run lvalue.
  RV32::RV32_AddressModification temp = RV32::RV32_AddressModification(
        nullptr, offset, &sym.getType(),
        true);
  RV32::RV32_LValue dryInstance = RV32::RV32_LValue(
      nullptr,
      temp);

  return(
     dryInstance );
};

}       // namespace RV32
