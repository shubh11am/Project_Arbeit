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


using namespace std;
using namespace WIR;


//
// Global variable declaration section
//

extern TC13 *TC179x_wirProc;
extern WIR_Function *TC179x_wirFct;
extern WIR_BasicBlock *TC179x_wirBB;

TC_ARegV dummyARegV;
TC_DRegV dummyDRegV;
TC_ERegV dummyERegV;


// Macro COST_LESS just serves for compatibility with good-old icd-cg.
#define COST_LESS(x,y) CodeSelector::COST_LESS( x, y )

/*
  The strings logOrEndLabel and logAndEndLabel contain the names of a label that
  should be used within the tpm_BinaryExpLOGAND/tpm_BinaryExpLOGOR rules to
  generate a conditional jump if the left-hand side of these logical operators
  already evaluates to false or true, resp.
*/
static string logOrEndLabel = "";
static string logAndEndLabel = "";

/*!
  logAndEndBlock points to a %WIR basic block to be used within the
  tpm_BinaryExpLOGAND rules in order to generate a conditional jump if the left-
  hand side of this logical operator already evaluates to false.
*/
static WIR_BasicBlock *logAndEndBlock = nullptr;

/*!
  logOrEndBlock points to a %WIR basic block to be used within the
  tpm_BinaryExpLOGOR rules in order to generate a conditional jump if the left-
  hand side of this logical operator already evaluates to true.
*/
static WIR_BasicBlock *logOrEndBlock = nullptr;

/*
  A RegisterPair contains a tuple of equivalent LLIR and WIR registers that are
  passed between the rules' action parts.
*/
using RegisterPair = pair<LLIR_Register *, WIR_BaseRegister *>;

/*
  An ARegPair contains a tuple of equivalent virtual LLIR and WIR address
  registers that are passed between the rules' action parts.
*/
using ARegPair = pair<LLIR_Register *, reference_wrapper<TC_ARegV>>;

/*
  A DRegPair contains a tuple of equivalent virtual LLIR and WIR data registers
  that are passed between the rules' action parts.
*/
using DRegPair = pair<LLIR_Register *, reference_wrapper<TC_DRegV>>;

/*
  An ERegPair contains a tuple of equivalent virtual LLIR and WIR extended data
  registers that are passed between the rules' action parts.
*/
using ERegPair = pair<LLIR_Register *, reference_wrapper<TC_ERegV>>;


#ifdef DEBUGMACROS
/*!
  Debug output helper function.
*/
static string getExpString( const IR_Exp &exp )
{
  stringstream out;
  out << "'";
  exp.write( out );
  out << "' (" << exp.getStmt().getFileContext().getFilename() << ":"
      << exp.getStmt().getFileContext().getLine() << ")";
  return( out.str() );
}
#endif


//#######################################################################
//
// Helper struct method definitions
//
//#######################################################################

/*
  generateOutputOperand converts an output operand of an inline assembly
  statement to an argument for the TriCore assembly parser.
*/
std::pair<Tricap::Argument *, WIR::TC_AsmArgument *> generateOutputOperand( IR_AsmOperand *op,
                                                                            vector<TC_LValue> &lvalues )
{
  DSTART(
    "pair<Tricap::Argument*, TC_AsmArgument*> generateOutputOperand(IR_AsmOperand*, vector<TC_LValue>&)" );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( op->getExpression() );

  int modifier =
    accumulate(
      op->getArgumentType().begin(), op->getArgumentType().end(), 0,
      addModifier );
  Constraint constraint = readConstraint( op->getArgumentType() );

  if ( !op->getSymbolicName().empty() )
    operandParseError(
      op->getExpression(),
      "Symbolic names are not supported yet (Argument ", ")" );

  if ( !symExp )
    operandParseError(
      op->getExpression(),
      "Output operands can only be bound to symbol expressions: ",
      " is not a symbol expression." );

  // Load global or local symbols from memory.
  TC_LValue info;
  bool deref = false;
  bool loadValue = modifier & static_cast<char>( Modifier::READWRITE );

  if ( symExp->getSymbol().isGlobal() ) {
    info = loadGlobalSymbol( *symExp, loadValue );
    deref = true;
  } else

  if ( TCCODESEL->getStack()->getSymbolOffset( &symExp->getSymbol() ) >= 0 ) {
    info = loadStackSymbol( *symExp, loadValue );
    deref = true;
  }

  // Generate the actual arguments for the assembly code parser.
  switch ( constraint ) {

    case Constraint::AREG: {
      LLIR_Register *reg;
      WIR_BaseRegister *rReg;

      if ( deref ) {
        if ( info.getResultRegister() == nullptr )
          info.setResultRegister( TCINSTRUCTIONS.CreateRegister( "", true ) );
        if ( info.getResultReg() == nullptr )
          info.setResultReg( &(TCINSTRUCTIONS.createAReg()) );
        reg = info.getResultRegister();
        rReg = info.getResultReg();
        lvalues.push_back( info );
      } else {
        reg = loadRegisterSymbol( *symExp );
        rReg = &(loadRegSym( *symExp ));
      }

      auto &r = *rReg;

      if ( r.getType() == TC13::RegisterType::aReg )
        return(
          make_pair(
            new Tricap::Register( Tricap::Argument::AREG, reg ),
            new TC_AsmRegister( r, TC_AsmArgument::Type::AREG ) ) );
      else

      if ( r.getType() == TC13::RegisterType::dReg ) {
        // LLIR
        LLIR_Register *areg = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_A( areg, reg );

        // WIR
        auto &aReg = TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_A( aReg, dynamic_cast<TC_DRegV &>( r ) );

        return(
          make_pair(
            new Tricap::Register( Tricap::Argument::AREG, areg ),
            new TC_AsmRegister( aReg, TC_AsmArgument::Type::AREG ) ) );
      } else

      if ( r.getType() == TC13::RegisterType::eReg ) {
        // LLIR
        LLIR_Register *areg = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_A( areg, reg->GetFirstChild() );

        // WIR
        auto &aReg = TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_A(
          aReg,
          dynamic_cast<TC_DRegV &>(
            dynamic_cast<TC_ERegV &>( r ).begin()->get() ) );

        return(
          make_pair(
            new Tricap::Register( Tricap::Argument::AREG, areg ),
            new TC_AsmRegister( aReg, TC_AsmArgument::Type::AREG ) ) );
      } else
        return(
          make_pair(
            new Tricap::Register(
              Tricap::Argument::AREG, reg->GetFirstChild() ),
            new TC_AsmRegister(
              dynamic_cast<TC_PRegV &>( r ).begin()->get(),
              TC_AsmArgument::Type::AREG ) ) );
    }

    case Constraint::DREG: {
      LLIR_Register *reg;
      WIR_BaseRegister *rReg;

      if ( deref ) {
        if ( info.getResultRegister() == nullptr )
          info.setResultRegister( TCINSTRUCTIONS.CreateRegister( "", false ) );
        if ( info.getResultReg() == nullptr )
          info.setResultReg( &(TCINSTRUCTIONS.createDReg()) );
        reg = info.getResultRegister();
        rReg = info.getResultReg();
        lvalues.push_back( info );
      } else {
        reg = loadRegisterSymbol( *symExp );
        rReg = &(loadRegSym( *symExp ));
      }

      auto &r = *rReg;

      if ( r.getType() == TC13::RegisterType::aReg ) {
        // LLIR
        LLIR_Register *dreg = TCINSTRUCTIONS.CreateRegister( "" );
        TCINSTRUCTIONS.insertMOV_D( dreg, reg );

        // WIR
        auto &dReg = TCINSTRUCTIONS.createDReg();
        TCINSTRUCTIONS.insertMOV_D( dReg, dynamic_cast<TC_ARegV &>( r ) );

        return(
          make_pair(
            new Tricap::Register( Tricap::Argument::DREG, dreg ),
            new TC_AsmRegister( dReg, TC_AsmArgument::Type::DREG ) ) );
      } else

      if ( r.getType() == TC13::RegisterType::dReg )
        return(
          make_pair(
            new Tricap::Register( Tricap::Argument::DREG, reg ),
            new TC_AsmRegister( r, TC_AsmArgument::Type::DREG ) ) );
      else

      if ( r.getType() == TC13::RegisterType::eReg )
        return(
          make_pair(
            new Tricap::Register( Tricap::Argument::EREG, reg ),
            new TC_AsmRegister( r, TC_AsmArgument::Type::EREG ) ) );
      else {
        // LLIR
        LLIR_Register *dreg = TCINSTRUCTIONS.CreateRegister( "" );
        TCINSTRUCTIONS.insertMOV_D( dreg, reg->GetFirstChild() );

        // WIR
        auto &dReg = TCINSTRUCTIONS.createDReg();
        TCINSTRUCTIONS.insertMOV_D(
          dReg,
          dynamic_cast<TC_ARegV &>(
            dynamic_cast<TC_PRegV &>( r ).begin()->get() ) );

        return(
          make_pair(
            new Tricap::Register( Tricap::Argument::DREG, dreg ),
            new TC_AsmRegister( dReg, TC_AsmArgument::Type::DREG ) ) );
      }
    }

    case Constraint::MEMORY: {
      if ( info.getAddress().containsAReg() )
        return(
          make_pair(
            new Tricap::Address(
              info.getAddress().getARegister(), info.getOffset() ),
            new TC_AsmAddress(
              info.getAddress().getAReg(), info.getOffset() ) ) );
      else
        //TODO
        break;
    }

    case Constraint::CONST:
      operandParseError(
        op->getExpression(),
        "Constant cannot be used as constraint for an output operand. ", "" );

    case Constraint::PREVIOUS:
      operandParseError(
        op->getExpression(),
        "Numbered constraints can only be used for input operands. ", "" );

  }

  return(
    make_pair(
      (Tricap::Argument *) nullptr, (WIR::TC_AsmArgument *) nullptr ) );
};


/*
  generateInputOperand converts an input operand of an inline assembly statement
  to an argument for the TriCore assembly parser.
*/
std::pair<Tricap::Argument *, WIR::TC_AsmArgument *> generateInputOperand( IR_AsmOperand *op,
                                                                           const vector<Tricap::Argument *> &arguments,
                                                                           const vector<unique_ptr<TC_AsmArgument>> &args )
{
  DSTART(
    "pair<Tricap::Argument*, TC_AsmArgument*> generateInputOperand(IR_AsmOperand*, const vector<Tricap::Argument*>&, const vector<unique_ptr<TC_AsmArgument> >&)" );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( op->getExpression() );
  auto *intConst = dynamic_cast<IR_IntConstExp *>( op->getExpression() );

  int modifier =
    accumulate(
      op->getArgumentType().begin(), op->getArgumentType().end(), 0,
      addModifier );
  Constraint constraint = readConstraint( op->getArgumentType() );

  if ( !op->getSymbolicName().empty() )
    operandParseError(
      op->getExpression(),
      "Symbolic names are not supported yet (Argument ", ")" );

  // Handle casts of constants to read folded pointers.
  auto *unaryExp = dynamic_cast<IR_UnaryExp *>( op->getExpression() );

  if ( unaryExp && ( unaryExp->getOperator() == IR_UnaryExp::CAST ) ) {
    symExp = dynamic_cast<IR_SymbolExp *>( &unaryExp->getOp() );
    intConst = dynamic_cast<IR_IntConstExp *>( &unaryExp->getOp() );
  }

  if ( modifier &
       ( static_cast<char>( Modifier::WRITE ) |
         static_cast<char>( Modifier::READWRITE ) |
         static_cast<char>( Modifier::EARLYCLOB ) ) )
    operandParseError(
      op->getExpression(),
      "The modifiers '=', '+' and '&' can only be applied to output "
        "operands. ", "" );

  switch ( constraint ) {

    case Constraint::AREG: {
      if ( symExp ) {
        LLIR_Register *reg = loadRegister( symExp );
        WIR_BaseRegister &r = loadReg( *symExp );
        string regname( reg->GetName() );

        if ( r.getType() == TC13::RegisterType::aReg )
          return(
            make_pair(
              new Tricap::Register( Tricap::Argument::AREG, reg ),
              new TC_AsmRegister( r, TC_AsmArgument::Type::AREG ) ) );
        else

        if ( r.getType() == TC13::RegisterType::dReg ) {
          // LLIR
          LLIR_Register *areg = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_A( areg, reg );

          // WIR
          auto &aReg = TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_A( aReg, dynamic_cast<TC_DRegV &>( r ) );

          return(
            make_pair(
              new Tricap::Register( Tricap::Argument::AREG, areg ),
              new TC_AsmRegister( aReg, TC_AsmArgument::Type::AREG ) ) );
        } else

        if ( r.getType() == TC13::RegisterType::eReg ) {
          // LLIR
          LLIR_Register *areg = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_A( areg, reg->GetFirstChild() );

          // WIR
          auto &aReg = TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_A(
            aReg,
            dynamic_cast<TC_DRegV &>(
              dynamic_cast<TC_ERegV &>( r ).begin()->get() ) );

          return(
            make_pair(
              new Tricap::Register( Tricap::Argument::AREG, areg ),
              new TC_AsmRegister( aReg, TC_AsmArgument::Type::AREG ) ) );
        } else
          return(
            make_pair(
              new Tricap::Register(
                Tricap::Argument::AREG, reg->GetFirstChild() ),
              new TC_AsmRegister(
                dynamic_cast<TC_PRegV &>( r ).begin()->get(),
                TC_AsmArgument::Type::AREG ) ) );
      } else

      if ( intConst ) {
        int val = intConst->getValue().getIntValue();

        // LLIR
        LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertLEA( reg, val );

        // WIR
        auto &aReg = TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertLEA( aReg, val );

        return(
          make_pair(
            new Tricap::Register( Tricap::Argument::AREG, reg ),
            new TC_AsmRegister( aReg, TC_AsmArgument::Type::AREG ) ) );
      } else
        operandParseError(
          op->getExpression(),
          "Only symbol expressions can be bound to an address register: ",
          " is not a symbol expression" );

      break;
    }

    case Constraint::DREG:
      return(
        make_pair(
          new Tricap::TemplateRegister(
            *(new GenerateInputRegister( &TCINSTRUCTIONS, *op ) ) ),
          new TC_AsmTemplateRegister(
            new AsmRegisterInitializer( *op, TCINSTRUCTIONS ) ) ) );

    case Constraint::MEMORY: {
      if ( symExp ) {
        auto info =
          symExp->getSymbol().isGlobal() ?
            loadGlobalSymbol( *symExp, false ) :
            loadStackSymbol( *symExp, false );

        if ( info.getAddress().containsAReg() )
          return(
            make_pair(
              new Tricap::Address(
                info.getAddress().getARegister(), info.getOffset() ),
              new TC_AsmAddress(
                info.getAddress().getAReg(), info.getOffset() ) ) );
        else {
          //TODO
        }
      } else

      if ( intConst ) {
        // TODO put value into global
      } else
        operandParseError(
          op->getExpression(),
          "Only symbol expressions or integer constants can be bound to "
            "memory: ",
          " is neither." );

      break;
    }

    case Constraint::CONST: {

      // The following assertion is required for the clang static analyzer.
      ufAssert( symExp != nullptr );

      if ( !symExp->isConstant() )
        operandParseError(
          op->getExpression(),
          "Only constants can be bound to a constant.", "" );

      IR_Exp &folded = symExp->fold();

      auto *integer = dynamic_cast<IR_IntConstExp *>( &folded );
      if ( integer )
        return(
          make_pair(
            new Tricap::Constant( integer->getValue().getIntValue() ),
            new TC_AsmConstant(
              (signed long long) integer->getValue().getIntValue() ) ) );

      auto *sizeOf = dynamic_cast<IR_SizeOfExp *>( &folded );
      if ( sizeOf )
        return(
          make_pair(
            new Tricap::Constant( sizeOf->getBaseType().bitSize() ),
            new TC_AsmConstant(
              (unsigned long long) sizeOf->getBaseType().bitSize() ) ) );

      operandParseError(
        op->getExpression(),
        "Only constants can be bound to a constant: ",
        " is not a constant." );

      break;
    }

    case Constraint::PREVIOUS: {
      istringstream str( op->getArgumentType() );
      size_t i = numeric_limits<size_t>::max();
      str >> i;

      if ( i < args.size() )
        return( make_pair( arguments.at( i ), args.at( i ).get() ) );
      else {
        ostringstream errmsg;
        errmsg << "The constraint '" << i << "' is reffering to a "
          "template operand which has not been defined yet.";
        operandParseError( op->getExpression(), errmsg.str(), "" );
      }
    }

  }

  return(
    make_pair(
      (Tricap::Argument *) nullptr, (WIR::TC_AsmArgument *) nullptr ) );
};


//######################################################################
//
// Utility methods
//
//######################################################################

/*
  Returns whether 'exp' is a LHS of an assignment expression.
*/
bool isLHSOfAssignment( const IR_Exp &exp )
{
  auto *aexp = dynamic_cast<IR_AssignExp *>( exp.getParent() );
  return( aexp && ( &aexp->getLHS() == &exp ) );
};


/*
  Returns whether 'exp' is a RHS of an assignment expression.
*/
bool isRHSOfAssignment( const IR_Exp &exp )
{
  auto *aexp = dynamic_cast<IR_AssignExp *>( exp.getParent() );
  return( aexp && ( &aexp->getRHS() == &exp ) );
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


/*!
  @brief IS_CONSTANT_EXP_WRAPPER generates a wrapper function for a given
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


/*
  isBounded32BitConstant determines whether the given IR value of the given type
  denotes an integer constant of at most 32 bits width, whose value is in the
  interval [minValue, maxValue] and whose signedness is equivalent to isSigned.
*/
bool isBounded32BitConstant( const IR_Integer &v, const IR_Type &t,
                             int minValue, int maxValue, bool isSigned )
{
  return(
    ( v.getIntValue() >= minValue ) && ( v.getIntValue() <= maxValue ) &&
    ( ( isSigned && t.isSignedType() ) ||
      ( !isSigned && t.isUnsignedType() ) ) &&
    ( t.bitSize() <= 32 ) );
};


/*
  is32BitConstantValue determines whether the given IR value of the given type
  denotes an integer constant of at most 32 bits width and whose value is 'val'.
*/
bool is32BitConstantValue( const IR_Integer &v, const IR_Type &t, int val )
{
  return( ( v.getIntValue() == val ) && ( t.bitSize() <= 32 ) );
};


/*
  Returns whether the given expression is a constant expression that can be
  represented by the 'addrOffset' nonterminal.
*/
bool isAddrOffset( const IR_Integer &v, const IR_Type &t )
{
  (void) v;
  return( t.bitSize() <= 32 );
};

IS_CONSTANT_EXP_WRAPPER( bool, isAddrOffset );


/*
  isConst4 determines whether the given IR integer is a constant that can be
  represented by the 'const4' nonterminal.
*/
bool isConst4( const IR_Integer &v, const IR_Type &t )
{
  return(
    isBounded32BitConstant(
      v, t, TC_Const4_Signed::getMinValue( 4 ),
      TC_Const4_Signed::getMaxValue( 4 ), true ) );
};

IS_CONSTANT_EXP_WRAPPER( bool, isConst4 );


/*
  isUConst4 determines whether the given IR integer is a constant that can be
  represented by the 'uconst4' nonterminal.
*/
bool isUConst4( const IR_Integer &v, const IR_Type &t )
{
  return(
    isBounded32BitConstant(
      v, t, 0, TC_Const4_Unsigned::getMaxValue( 4 ), false ) );
};

IS_CONSTANT_EXP_WRAPPER( bool, isUConst4 );


/*
  isConst9 determines whether the given IR integer is a constant that can be
  represented by the 'const9' nonterminal.
*/
bool isConst9( const IR_Integer &v, const IR_Type &t )
{
  return(
    isBounded32BitConstant(
      v, t, TC_Const9_Signed::getMinValue( 9 ),
      TC_Const9_Signed::getMaxValue( 9 ), true ) );
};

IS_CONSTANT_EXP_WRAPPER( bool, isConst9 );


/*
  isUConst9 determines whether the given IR integer is a constant that can be
  represented by the 'uconst9' nonterminal.
*/
bool isUConst9( const IR_Integer &v, const IR_Type &t )
{
  return(
    isBounded32BitConstant(
      v, t, 0, TC_Const9_Unsigned::getMaxValue( 9 ), false ) );
};

IS_CONSTANT_EXP_WRAPPER( bool, isUConst9 );


/*
  isConst16 determines whether the given IR integer is a constant that can be
  represented by the 'const16' nonterminal.
*/
bool isConst16( const IR_Integer &v, const IR_Type &t )
{
  return(
    isBounded32BitConstant(
      v, t, TC_Const16_Signed::getMinValue( 16 ),
      TC_Const16_Signed::getMaxValue( 16 ), true ) );
}

IS_CONSTANT_EXP_WRAPPER( bool, isConst16 );


/*
  isConstant0 determines whether the given IR integer is a constant that can be
  represented by the 'constant0' nonterminal.
*/
bool isConstant0( const IR_Integer &v, const IR_Type &t )
{
  return( is32BitConstantValue( v, t, 0 ) );
};

IS_CONSTANT_EXP_WRAPPER( bool, isConstant0 );


/*
  isConstant8 determines whether the given IR integer is a constant that can be
  represented by the 'constant8' nonterminal.
*/
bool isConstant8( const IR_Integer &v, const IR_Type &t )
{
  return( is32BitConstantValue( v, t, 8 ) );
};

IS_CONSTANT_EXP_WRAPPER( bool, isConstant8 );


/*
  isConstant256 determines whether the given IR integer is a constant that can
  be represented by the 'constant256' nonterminal.
*/
bool isConstant256( const IR_Integer &v, const IR_Type &t )
{
  return( is32BitConstantValue( v, t, 256 ) );
};

IS_CONSTANT_EXP_WRAPPER( bool, isConstant256 );


/*
  isPowerOfTwo determines whether the given IR integer is a constant that can be
  represented by the 'powerOfTwo' nonterminal.
*/
int isPowerOfTwo( const IR_Integer &v, const IR_Type &t )
{
  if ( t.bitSize() <= 32 ) {

    int constValue = v.getIntValue();
    int powerVal = 2;

    for ( int i = 1; i < 32; ++i, powerVal *= 2 )
      if ( constValue == powerVal )
        return( i );
  }

  return( 0 );
};

IS_CONSTANT_EXP_WRAPPER( int, isPowerOfTwo );


/*
  isNegativePowerOfTwo determines whether the given IR integer is a constant
  that can be represented by the 'negPowerOfTwo' nonterminal.
*/
int isNegativePowerOfTwo( const IR_Integer &v, const IR_Type &t )
{
  if ( t.bitSize() <= 32 ) {

    int constValue = v.getIntValue();
    int powerVal = -2;

    for ( int i = 1; i < 32; ++i, powerVal *= 2 )
      if ( constValue == powerVal )
        return( i );
  }

  return( 0 );
};


/*
  isConst9Neg determines whether the given IR integer is a constant that can be
  represented by the 'const9_neg' nonterminal.
*/
bool isConst9Neg( const IR_Integer &v, const IR_Type &t )
{
  return(
    isBounded32BitConstant(
      v, t,
      TC_Const9_Unsigned::getMaxValue( 32 ) -
        TC_Const9_Signed::getMaxValue( 9 ),
      TC_Const9_Unsigned::getMaxValue( 32 ), false ) );
};

IS_CONSTANT_EXP_WRAPPER( bool, isConst9Neg );


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
  computeSizeOf computes the size of an IR type in bytes.
*/
int computeSizeOf( const IR_Type *t )
{
  DSTART( "int computeSizeOf(const IR_Type*)" );

  int bytesize = 0;
  const IR_Type::Type theType = t->getType();

  auto *ptrType = dynamic_cast<const IR_PointerType *>( t );
  auto *arrType = dynamic_cast<const IR_ArrayType *>( t );

  const bool isPtrType = ( ptrType != nullptr ) && ( arrType == nullptr );
  const bool isArrType = ( ptrType != nullptr ) && ( arrType != nullptr );

  if ( theType == IR_Type::BOOL )
    bytesize = boolBytes;
  else
  if ( ( theType == IR_Type::CHAR ) || ( theType == IR_Type::UNSIGNED_CHAR ) )
    bytesize = charBytes;
  else
  if ( ( theType == IR_Type::SHORT ) || ( theType == IR_Type::UNSIGNED_SHORT ) )
    bytesize = shortBytes;
  else
  if ( ( theType == IR_Type::INT ) || ( theType == IR_Type::UNSIGNED_INT ) )
    bytesize = intBytes;
  else
  if ( ( theType == IR_Type::LONG ) || ( theType == IR_Type::UNSIGNED_LONG ) )
    bytesize = longBytes;
  else
  if ( ( theType == IR_Type::LONG_LONG ) ||
       ( theType == IR_Type::UNSIGNED_LONG_LONG ) )
    bytesize = longLongBytes;
  else
  if ( ( theType == IR_Type::DOUBLE ) || ( theType == IR_Type::LONG_DOUBLE ) )
    bytesize = doubleBytes;
  else
  if ( theType == IR_Type::FLOAT )
    bytesize = floatBytes;
  else
  if ( isPtrType )
    bytesize = pointerBytes;
  else
  if ( isArrType )
    bytesize = arrType->bitSize() / TCIR_CONFIGURATION->bitwidthAddressable;
  else
  if ( ( theType == IR_Type::STRUCT ) || ( theType == IR_Type::UNION ) )
    bytesize = Stack::getStackSize( t );
  else
    ufAssertT( bytesize != 0, "Unsupported type passed to sizeof." );

  return( bytesize );
};


/*
  computeSizeOf computes the size of a tree element in bytes.
*/
int computeSizeOf( const NODEPTR treeElem )
{
  DSTART( "int computeSizeOf(const IR_TreeElem*)" );

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
  This function binds the given register to the given physical register number,
  no matter whether it is a data register, an address register or an extended
  register.
*/
void bindToPHREG( LLIR_Register &reg, unsigned int phregNumber )
{
  LLIR_Function * const lf = TCCODESEL->getLastLLIRFunction();

  if ( isDReg( reg.GetName() ) || isAReg( reg.GetName() ) ) {
    // Create data/address register

    if ( isDReg( reg.GetName() ) ) {
      switch ( phregNumber ) {
        case  0: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D0 ) ); break;
        case  1: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D1 ) ); break;
        case  2: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D2 ) ); break;
        case  3: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D3 ) ); break;
        case  4: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D4 ) ); break;
        case  5: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D5 ) ); break;
        case  6: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D6 ) ); break;
        case  7: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D7 ) ); break;
        case  8: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D8 ) ); break;
        case  9: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D9 ) ); break;
        case 10: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D10 ) ); break;
        case 11: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D11 ) ); break;
        case 12: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D12 ) ); break;
        case 13: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D13 ) ); break;
        case 14: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D14 ) ); break;
        case 15: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_D15 ) ); break;
        default: ufAssertT( 0, "Invalid register index!" );
      }
    } else
    if ( isAReg( reg.GetName() ) ) {
      switch ( phregNumber ) {
        case  0: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A0 ) ); break;
        case  1: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A1 ) ); break;
        case  2: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A2 ) ); break;
        case  3: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A3 ) ); break;
        case  4: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A4 ) ); break;
        case  5: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A5 ) ); break;
        case  6: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A6 ) ); break;
        case  7: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A7 ) ); break;
        case  8: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A8 ) ); break;
        case  9: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A9 ) ); break;
        case 10: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A10 ) ); break;
        case 11: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A11 ) ); break;
        case 12: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A12 ) ); break;
        case 13: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A13 ) ); break;
        case 14: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A14 ) ); break;
        case 15: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateRegister( PHREG_A15 ) ); break;
        default: ufAssertT( 0, "Invalid register index!" );
      }
    }

  } else {
    // Create extended data register

    if ( isEReg( reg.GetName() ) ) {
      switch ( phregNumber ) {
        case  0: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateERegister( PHREG_E0 ) ); break;
        case  2: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateERegister( PHREG_E2 ) ); break;
        case  4: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateERegister( PHREG_E4 ) ); break;
        case  6: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateERegister( PHREG_E6 ) ); break;
        case  8: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateERegister( PHREG_E8 ) ); break;
        case 10: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateERegister( PHREG_E10 ) ); break;
        case 12: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateERegister( PHREG_E12 ) ); break;
        case 14: lf->AddPrecolor( &reg, TCINSTRUCTIONS.CreateERegister( PHREG_E14 ) ); break;
        default: ufAssertT( 0, "Invalid register index!" );
      }
    }
  }
}


/*
  bindToPHREG precolors the given virtual %WIR register with a physical TriCore
  register.

  Depending on the virtual register's type, bindtoPHREG assigns r to either a
  data register, an address register or an extended data register.
*/
void bindToPHREG( const WIR_VirtualRegister &r, unsigned int phregNumber )
{
  DSTART( "void bindToPHREG(const WIR_VirtualRegister&, unsigned int)" );

  if ( r.getType() == TC13::RegisterType::dReg ) {
    // Precolor data register.
    switch ( phregNumber ) {
      case 0: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D0() );
        break;
      }
      case 1: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D1() );
        break;
      }
      case 2: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D2() );
        break;
      }
      case 3: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D3() );
        break;
      }
      case 4: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D4() );
        break;
      }
      case 5: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D5() );
        break;
      }
      case 6: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D6() );
        break;
      }
      case 7: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D7() );
        break;
      }
      case 8: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D8() );
        break;
      }
      case 9: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D9() );
        break;
      }
      case 10: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D10() );
        break;
      }
      case 11: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D11() );
        break;
      }
      case 12: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D12() );
        break;
      }
      case 13: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D13() );
        break;
      }
      case 14: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D14() );
        break;
      }
      case 15: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->D15() );
        break;
      }
      default: {
        ufAssertT( 0, "Invalid register index." );
        break;
      }
    }
  } else

  if ( r.getType() == TC13::RegisterType::aReg ) {
    // Precolor address register.
    switch ( phregNumber ) {
      case 0: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A0() );
        break;
      }
      case 1: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A1() );
        break;
      }
      case 2: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A2() );
        break;
      }
      case 3: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A3() );
        break;
      }
      case 4: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A4() );
        break;
      }
      case 5: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A5() );
        break;
      }
      case 6: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A6() );
        break;
      }
      case 7: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A7() );
        break;
      }
      case 8: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A8() );
        break;
      }
      case 9: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A9() );
        break;
      }
      case 10: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A10() );
        break;
      }
      case 11: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A11() );
        break;
      }
      case 12: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A12() );
        break;
      }
      case 13: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A13() );
        break;
      }
      case 14: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A14() );
        break;
      }
      case 15: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->A15() );
        break;
      }
      default: {
        ufAssertT( 0, "Invalid register index." );
        break;
      }
    }
  } else

  if ( r.getType() == TC13::RegisterType::eReg ) {
    // Precolor extended data register.
    switch ( phregNumber ) {
      case 0: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->E0() );
        break;
      }
      case 2: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->E2() );
        break;
      }
      case 4: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->E4() );
        break;
      }
      case 6: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->E6() );
        break;
      }
      case 8: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->E8() );
        break;
      }
      case 10: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->E10() );
        break;
      }
      case 12: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->E12() );
        break;
      }
      case 14: {
        TC179x_wirFct->insertPrecolor( r, TC179x_wirProc->E14() );
        break;
      }
      default: {
        ufAssertT( 0, "Invalid register index." );
        break;
      }
    }
  }
};


/*
   If the function argument was already assigned to a register, then that
   register is returned, else a new register of matching type is created
   and registered as the representation of that argument. The new register
   is then returned.

   'sym' is the function parameter symbol
 */
LLIR_Register &getFunctionArgumentRegister( const IR_Symbol &sym )
{
  ufAssertT(
    sym.getSymbolTable().getFunction(), "'sym' was no function parameter." );

  LLIR_Register *result = nullptr;
  string existing_reg =
    TCCODESEL->getStack()->getSymbolReg( const_cast<IR_Symbol *>( &sym ) );

  if ( existing_reg != "" ) {

    // The function argument symbol was already processed, hence use the virtual
    // register already assigned to the argument symbol.
    if ( isDRegType( sym ) || isARegType( sym ) )
      result = TCINSTRUCTIONS.CreateRegister( existing_reg );
    else
    if ( isERegType( sym ) )
      result = TCINSTRUCTIONS.CreateERegister( existing_reg );
    else
    if ( isComposedType( sym ) ) {
      const int composedObjSize = Stack::getStackSize( &sym.getType() );
      if ( composedObjSize <= 4 )
        result = TCINSTRUCTIONS.CreateRegister( existing_reg );
      else
      if ( composedObjSize <= 8 )
        result = TCINSTRUCTIONS.CreateERegister( existing_reg );
      else
        result = TCINSTRUCTIONS.CreateRegister( existing_reg, true );
    }

  } else {

    // The function argument symbol was not yet processed, hence declare a new
    // virtual register and assign it to the argument symbol.
    if ( isDRegType( sym ) )
      result = TCINSTRUCTIONS.CreateRegister( "" );
    else
    if ( isARegType( sym ) )
      result = TCINSTRUCTIONS.CreateRegister( "", true );
    else
    if ( isERegType( sym ) )
      result = TCINSTRUCTIONS.CreateERegister( "" );
    else
    if ( isComposedType( sym ) ) {
      const int composedObjSize = Stack::getStackSize( &sym.getType() );
      if ( composedObjSize <= 4 )
        result = TCINSTRUCTIONS.CreateRegister( "" );
      else
      if ( composedObjSize <= 8 )
        result = TCINSTRUCTIONS.CreateERegister( "" );
      else
        result = TCINSTRUCTIONS.CreateRegister( "", true );
    }

    // The following assertion is required for the clang static analyzer.
    ufAssert( result != nullptr );

    TCCODESEL->getStack()->setSymbolReg(
      const_cast<IR_Symbol *>( &sym ), result->GetName() );

  }

  // The following assertion is required for the clang static analyzer.
  ufAssert( result != nullptr );

  // Bind the result to the appropriate physical register.
  bindToPHREG( *result, Stack::isPassedThroughRegister( sym ) );

  return( *result );
};


/*
  getFctArgReg determines the pre-colored virtual %WIR register to be used for
  passing an IR symbol as function argument.

  If the function argument symbol was already assigned to a register previously,
  that register is returned. Otherwise, a new register of matching type is
  created, pre-colored and registered as the representative of the given
  function argument. The new register is then returned.
*/
WIR_VirtualRegister &getFctArgReg( const IR_Symbol &sym )
{
  DSTART( "WIR_VirtualRegister& getFctArgReg(const IR_Symbol&)" );

  if ( TCCODESEL->getStack()->isSymbolRegSet( sym ) ) {
    auto &r = TCCODESEL->getStack()->getSymbolReg( sym );

    if ( r.getFunction() == *TC179x_wirFct )
      // The function argument symbol was already processed. Hence, use the
      // virtual register already assigned to the argument symbol.
      return( TCCODESEL->getStack()->getSymbolReg( sym ) );
  }

  WIR_VirtualRegister *r = nullptr;

  // The function argument symbol was not yet processed. Hence, create a new
  // virtual register and assign it to the argument symbol.
  if ( isDRegType( sym ) )
    r = &(TCINSTRUCTIONS.createDReg());
  else
  if ( isARegType( sym ) )
    r = &(TCINSTRUCTIONS.createAReg());
  else
  if ( isERegType( sym ) )
    r = &(TCINSTRUCTIONS.createEReg());
  else
  if ( isComposedType( sym ) ) {
    const int composedObjSize = Stack::getStackSize( &sym.getType() );
    if ( composedObjSize <= 4 )
      r = &(TCINSTRUCTIONS.createDReg());
    else
    if ( composedObjSize <= 8 )
      r = &(TCINSTRUCTIONS.createEReg());
    else
      r = &(TCINSTRUCTIONS.createAReg());
  }

  TCCODESEL->getStack()->setSymbolReg( sym, *r );

  // Pre-color the result register with the appropriate physical register.
  bindToPHREG( *r, Stack::isPassedThroughRegister( sym ) );

  return( *r );
};


/*
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
  LLIR_Register *result = nullptr;

  IR_Function *theFunc =
    funcCall.getFunctionSymbol() ?
      funcCall.getFunctionSymbol()->getFunction() : nullptr;

  if ( theFunc ) {
    // The function to be called is fully known within ICD-C. Hence, get its
    // argument symbols.
    list<IR_Symbol *> funcArgs = theFunc->functionArguments.getSymbols();
    list<IR_Symbol *>::iterator theArg = funcArgs.begin();
    for ( unsigned int i = 0; i < argumentIndex; ++i, ++theArg ) ;

    result = &getFunctionArgumentRegister( **theArg );
  } else {
    // If the called IR function is not known (this happens for indirect calls
    // through function pointers), we must extract the argument type, create an
    // appropriate register and bind it to the respective PHREG.
    const list<IR_Type *> &funcArgTypes =
      funcCall.getFunctionType().getArgumentTypes();
    list<IR_Type *>::const_iterator theArgType = funcArgTypes.begin();
    for ( unsigned int i = 0;
          ( i < argumentIndex ) && ( i < funcArgTypes.size() - 1 );
          ++i, ++theArgType ) ;

    IR_Type *argumentType = *theArgType;

    if ( (*theArgType)->getType() == IR_Type::ELLIPSIS ) {
      // The current function argument is represented by an ellipsis '...'
      // whose type is unclear. We thus have to take the type of the actual
      // parameter of the function call, instead of the type of the argument of
      // the called function.
      auto &args = funcCall.getArguments();
      auto argIt = args.begin();
      std::advance( argIt, argumentIndex );
      IR_Exp *exp = *argIt;
      argumentType = &(exp->getType());
    }

    if ( isDRegType( *argumentType ) )
      result = TCINSTRUCTIONS.CreateRegister( "" );
    else
    if ( isARegType( *argumentType ) )
      result = TCINSTRUCTIONS.CreateRegister( "", true );
    else
    if ( isERegType( *argumentType ) )
      result = TCINSTRUCTIONS.CreateERegister( "" );
    else
    if ( isComposedType( *argumentType ) ) {
      const int composedObjSize = Stack::getStackSize( argumentType );
      if ( composedObjSize <= 4 )
        result = TCINSTRUCTIONS.CreateRegister( "" );
      else
      if ( composedObjSize <= 8 )
        result = TCINSTRUCTIONS.CreateERegister( "" );
      else
        result = TCINSTRUCTIONS.CreateRegister( "", true );
    }

    // Bind the result to the appropriate physical register.
    bindToPHREG( *result, phRegIndex );
  }

  return( *result );
};


/*
  getFctArgReg determines the pre-colored virtual %WIR register to be used for
  passing a function argument.

  If the function argument symbol was already assigned to a register previously,
  that register is returned. Otherwise, a new register of matching type is
  created, pre-colored and registered as the representative of the given
  function argument. The new register is then returned.
*/
WIR_VirtualRegister &getFctArgReg( const IR_CallExp &exp, unsigned int argIdx,
                                   unsigned int phRegIdx )
{
  DSTART(
    "WIR_VirtualRegister& getFctArgReg(const IR_CallExp&, unsigned int, unsigned int)" );

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

  // If the called IR function is not known (this happens for indirect calls
  // through function pointers), we must extract the argument type, create an
  // appropriate register and bind it to the respective PHREG.
  const list<IR_Type *> &funcArgTypes =
    exp.getFunctionType().getArgumentTypes();
  list<IR_Type *>::const_iterator theArgType = funcArgTypes.begin();
  for ( unsigned int i = 0;
        ( i < argIdx ) && ( i < funcArgTypes.size() - 1 );
        ++i, ++theArgType ) ;

  IR_Type *argumentType = *theArgType;

  if ( (*theArgType)->getType() == IR_Type::ELLIPSIS ) {
    // The current function argument is represented by an ellipsis '...'
    // whose type is unclear. We thus have to take the type of the actual
    // parameter of the function call, instead of the type of the argument of
    // the called function.
    auto &args = exp.getArguments();
    auto argIt = args.begin();
    std::advance( argIt, argIdx );
    argumentType = &((*argIt)->getType());
  }

  WIR_VirtualRegister *r = nullptr;

  if ( isDRegType( *argumentType ) )
    r = &(TCINSTRUCTIONS.createDReg());
  else
  if ( isARegType( *argumentType ) )
    r = &(TCINSTRUCTIONS.createAReg());
  else
  if ( isERegType( *argumentType ) )
    r = &(TCINSTRUCTIONS.createEReg());
  else
  if ( isComposedType( *argumentType ) ) {
    const int composedObjSize = Stack::getStackSize( argumentType );
    if ( composedObjSize <= 4 )
      r = &(TCINSTRUCTIONS.createDReg());
    else
    if ( composedObjSize <= 8 )
      r = &(TCINSTRUCTIONS.createEReg());
    else
      r = &(TCINSTRUCTIONS.createAReg());
  }

  // Pre-color the result register with the appropriate physical register.
  bindToPHREG( *r, phRegIdx );

  return( *r );
};


/*
  Generates a new basic block and inserts it into the current function and
  updates the BackAnnotation mappings by mapping the new block to the given IR
  basic block.
*/
LLIR_BB &beginNewLLIRBasicBlock( const char *name, IR_BasicBlock &bb )
{
  DSTART( "beginNewLLIRBasicBlock( const char *name, const IR_BasicBlock &bb )" );
  return( TCCODESEL->beginNewLLIRBasicBlock( name, bb ) );
};


/*
  Wrapper for "beginNewLLIRBasicBlock( const char *name, IR_BasicBlock &bb )"
  that always uses a new unique block name.
*/
LLIR_BB &beginNewLLIRBasicBlock( IR_BasicBlock &bb )
{
  return( TCCODESEL->beginNewLLIRBasicBlock( bb ) );
};


/*
  Generates a new basic block and inserts it into the current function and
  updates the BackAnnotation mappings by inserting a join mapping that marks the
  new basic block and the last LLIR BB as joined together (mapped to the same IR
  BB).
*/
LLIR_BB &beginNewLLIRBasicBlock( const char *name )
{
  DSTART( "beginNewLLIRBasicBlock( const char *name )" );
  return( TCCODESEL->beginNewLLIRBasicBlock( name ) );
};


/*
  Wrapper for "beginNewLLIRBasicBlock( const char *name )" that always uses a
  new unique block name.
*/
LLIR_BB &beginNewLLIRBasicBlock( void )
{
  return( TCCODESEL->beginNewLLIRBasicBlock() );
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
        TCCODESEL->getStack()->getSymbolOffset( &sexp->getSymbol() ) >= 0 ) );

  return( e.isDef() && isMemoryAccess );
};


/*
  Loads the memory location that is described by the given lvalue into an
  address register and returns this register.

  'baseReg' holds the base register of the memory access
  'offset' holds the offset (bytes) of the memory access
  'target' should be the name of the areg that should be loaded with the address
          (may be the empty string)
  'loadExp' should be the expression in which the load is performed (if any)
*/
LLIR_Register &loadAccessLocationToAReg( LLIR_Register *baseReg, int offset,
                                         const string &target,
                                         const IR_Exp *loadExp )
{
  LLIR_Register *reg_base   = baseReg;
  LLIR_Register *reg_target = TCINSTRUCTIONS.CreateRegister( target, true );

  string regName( baseReg->GetName() );
  if ( !offset ) {

    /*
     * always insert a MOV instruction for the stack pointer, otherwise we end
     * may end up with instructions like "add.a %a12, %a12, %a10" which are not
     * handled by the register allocator when it adjusts the stack pointer
     */
    if ( ( target != "" && target != regName ) || regName == PHREG_SP ) {
      TCINSTRUCTIONS.insertMOV_AA( reg_target, reg_base, loadExp );
      return *reg_target;
    } else {
      return *reg_base;
    }

  } else {

    if ( offset >= minSignedConst16Value &&
         offset <= maxSignedConst16Value ) {
      TCINSTRUCTIONS.insertLEA( reg_target, OPER_BASE, reg_base, offset, loadExp );
    } else {
      auto p = TCINSTRUCTIONS.splitOffset( offset );

      TCINSTRUCTIONS.insertMOV_AA( reg_target, reg_base, loadExp );
      TCINSTRUCTIONS.insertADDIH_A( reg_target, reg_target, p.first, loadExp );
      TCINSTRUCTIONS.insertLEA( reg_target, OPER_BASE, reg_target, p.second, loadExp );
    }

    return *reg_target;
  }
};


/*
  loadRegisterRelativeAddressCost computes the cost of instructions that load a
  register-relative address into an address register.
*/
COST loadRegisterRelativeAddressCost( unsigned int byteSize )
{
  DSTART( "COST loadRegisterRelativeAddressCost(unsigned int)" );

  COST cost = TC13::OperationFormat::AADC2.getSize();
  const int bitWidth = getBitWidth( byteSize );

  if ( ( byteSize >= TC_Const9_Signed::getMinValue( 9 ) ) &&
       ( byteSize <= TC_Const9_Signed::getMaxValue( 9 ) ) )
    cost += TC13::OperationFormat::DDC9_1.getSize();
  else
  if ( ( byteSize >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( byteSize <= TC_Const16_Signed::getMinValue( 16 ) ) )
    cost +=
      TC13::OperationFormat::DC16_1.getSize() +
      TC13::OperationFormat::DDC9_1.getSize();
  else
  if ( bitWidth == 17 )
    cost +=
      TC13::OperationFormat::DC16_2.getSize() +
      TC13::OperationFormat::DDC9_1.getSize();
  else
    cost +=
      TC13::OperationFormat::DC16_2.getSize() +
      TC13::OperationFormat::DDC16_1.getSize() +
      TC13::OperationFormat::DDC9_1.getSize();

  return( cost );
};


/*
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
                                            int elementByteSize,
                                            const IR_Exp *loadExp )
{
  ufAssertT( elementByteSize >= 0, "Negative size is illegal!" );

  const int bitWidth = getBitWidth( elementByteSize );
  LLIR_Register *iaddr = TCINSTRUCTIONS.CreateRegister( "", true );

  // Compute address pointer.
  if ( elementByteSize == 1 || elementByteSize == 2 ||
       elementByteSize == 4 || elementByteSize == 8 ) {

    int shiftAmount = 0;

    switch( elementByteSize ) {
      case 1: shiftAmount = 0; break;
      case 2: shiftAmount = 1; break;
      case 4: shiftAmount = 2; break;
      case 8: shiftAmount = 3; break;
      default: ufAssertT( 0, "Unsupported byte size!" ); break;
    }

    TCINSTRUCTIONS.insertADDSC_A( iaddr, baseReg, offsetReg, shiftAmount, loadExp );

  } else
  if ( elementByteSize >= minSignedConst9Value &&
       elementByteSize <= maxSignedConst9Value ) {

    LLIR_Register *reg1 = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertMUL( reg1, offsetReg, elementByteSize, loadExp );
    TCINSTRUCTIONS.insertADDSC_A( iaddr, baseReg, reg1, 0, loadExp );

  } else
  if ( elementByteSize >= minSignedConst16Value &&
       elementByteSize <= maxSignedConst16Value ) {

    LLIR_Register *reg1 = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertMOV( reg1, elementByteSize, loadExp );
    TCINSTRUCTIONS.insertMUL( reg1, reg1, offsetReg, loadExp );
    TCINSTRUCTIONS.insertADDSC_A( iaddr, baseReg, reg1, 0, loadExp );

  } else
  if ( bitWidth == 17 && elementByteSize >= 0 ) {

    LLIR_Register *reg1 = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertMOV_U( reg1, elementByteSize, loadExp );
    TCINSTRUCTIONS.insertMUL( reg1, reg1, offsetReg, loadExp );
    TCINSTRUCTIONS.insertADDSC_A( iaddr, baseReg, reg1, 0, loadExp );

  } else {

    LLIR_Register *reg1 = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertMOVH_ADDI( reg1, elementByteSize, loadExp );
    TCINSTRUCTIONS.insertMUL( reg1, reg1, offsetReg, loadExp );
    TCINSTRUCTIONS.insertADDSC_A( iaddr, baseReg, reg1, 0, loadExp );

  }

  return iaddr;
}


/*
  loadRegRelativeAddr generates code that loads a register-relative address into
  an address register.
*/
TC_ARegV &loadRegRelativeAddr( const TC_ARegV &a, const TC_DRegV &d,
                               unsigned int byteSize,
                               const IR_Exp *exp )
{
  DSTART(
    "TC_ARegV& loadRegRelativeAddr(const TC_ARegV&, const TC_DRegV&, unsigned int, const IR_Exp*)" );

  auto &r = TCINSTRUCTIONS.createAReg();

  if ( ( byteSize == 1 ) || ( byteSize == 2 ) || ( byteSize == 4 ) ||
       ( byteSize == 8 ) ) {
    unsigned int shiftAmount = 0;

    switch( byteSize ) {
      case 1: {
        shiftAmount = 0;
        break;
      }
      case 2: {
        shiftAmount = 1;
        break;
      }
      case 4: {
        shiftAmount = 2;
        break;
      }
      default: {
        shiftAmount = 3;
        break;
      }
    }

    TCINSTRUCTIONS.insertADDSC_A( r, a, d, shiftAmount, exp );
  } else

  if ( ( byteSize >= TC_Const9_Signed::getMinValue( 9 ) ) &&
       ( byteSize <= TC_Const9_Signed::getMaxValue( 9 ) ) ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();
    TCINSTRUCTIONS.insertMUL( tmpReg, d, byteSize, exp );
    TCINSTRUCTIONS.insertADDSC_A( r, a, tmpReg, 0, exp );
  } else {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();
    TCINSTRUCTIONS.insertMOVConstant( tmpReg, byteSize, exp );
    TCINSTRUCTIONS.insertMUL( tmpReg, tmpReg, d, exp );
    TCINSTRUCTIONS.insertADDSC_A( r, a, tmpReg, 0, exp );
  }

  return( r );
};


/*
  transformToLOOPLoopCost computes the costs for a do-while loop using the LOOP
  instruction.
*/
COST transformToLOOPLoopCost( const IR_DoWhileStmt &doWhileStmt )
{
  DSTART( "COST transformToLOOPLoopCost(const IR_DoWhileStmt&)" );

  COST res = TC13::OperationFormat::AL_3.getSize();

  int v = TCCODESEL->getLOOPIterationCounts( &doWhileStmt ) - 1;
  bool isUnsignedConst18 =
    ( v >= 0 ) && ( v <= (int) TC_Const18_Unsigned::getMaxValue( 18 ) );
  unsigned int bits1427 = ( v >> 14 ) & 0x3FFF;
  bool isOffset18 = isUnsignedConst18 && ( bits1427 == 0 );

  if ( isOffset18 )
    res += TC13::OperationFormat::AC18ABSA.getSize();
  else
    res +=
      TC13::OperationFormat::AC16.getSize() +
      TC13::OperationFormat::AAC16BOA.getSize();

  return( res );
};


/*
  transformToLOOPLoop generates code for a do-while loop using the LOOP
  instruction.

  transformToLOOPLoop does just the conversion, it does not check whether the
  prerequisites for doing so are fulfilled.
*/
void transformToLOOPLoop( const IR_DoWhileStmt &doWhileStmt,
                          const IR_Exp *exp )
{
  DSTART( "void transformToLOOPLoop(const IR_DoWhileStmt&, const IR_Exp*)" );

  // Retrieve loop count.
  int loopCounterValue = TCCODESEL->getLOOPIterationCounts( &doWhileStmt ) - 1;

  // LLIR
  // Get first block within the loop body.
  stringstream ss;
  ss << doWhileStmt.getBasicBlock()->getSuccs()[ 0 ];
  string bb = TCCODESEL->getBlockLabel( ss.str() );
  LLIR_BB *oldLoopBody = TCCODESEL->getLastLLIRFunction()->FindBB( bb.c_str() );

  // Create a new basic block that is placed after the current first loop
  // block. The idea is to move all instructions from the old loop block to
  // the new block block and place the LEA instruction with the address
  // register required for LOOP in oldLoopBody. Thus, the CFG is still
  // preserved, i.e. all path entering the old loop structure will pass
  // the LEA initialization before entering the actual loop.
  LLIR_BB *newLoopBody = new LLIR_BB( LLIR::getUniqueLabel().c_str() );
  TCCODESEL->getLastLLIRFunction()->InsertBB( newLoopBody, oldLoopBody );
  TCCODESEL->insertBB( newLoopBody );

  // BackAnnotation (New body replaces old one)
  TCCODESEL->getBackAnnotation()->addNewMapping( newLoopBody,
    TCCODESEL->getBackAnnotation()->getMapping( oldLoopBody ) );

  // Move instructions of oldLoopBody to newLoopBody.
  for ( LLIR_Instruction *i = oldLoopBody->GetFirstIns(); i;
        i = oldLoopBody->GetFirstIns() ) {
    oldLoopBody->MoveIns( i, newLoopBody->GetLastIns(), newLoopBody );
  }

  // Move pragmas from oldLoopBody to newLoopBody.
  oldLoopBody->CopyPragmas( newLoopBody );
  oldLoopBody->DeletePragmas();

  // Create a new address register that is initialized with the loop iteration
  // counts for the subsequent LOOP instruction.
  LLIR_Register *loopAReg = TCINSTRUCTIONS.CreateRegister( "", true );

  // Insert LEA instruction holding the address register initialization
  // for the LOOP instruction.
  if ( loopCounterValue <= maxUnsignedConst14Value )
    insLEA( loopAReg, loopCounterValue, oldLoopBody );
  else {
    stringstream str;
    str << loopCounterValue;
    int low;

    // Extract lowest 16 bits from loop counter, use 2-complement
    // representation.
    low = loopCounterValue & 0x0000FFFF;

    if ( low >= 32768 )
      // 2-complement.
      low = -(65536 - low);

    // Generate MOVH.A instruction.
    // See also TriCore ISA Manual, section 1.7 on address arithmetic.
    LLIR_Instruction *movha =
      insMOVH_A( loopAReg, OPER_LAB_HI, str.str(), oldLoopBody );

    // Generate LEA instruction.
    if ( low != 0 )
      insLEA(
        loopAReg, OPER_BASE, loopAReg, OPER_LAB_LO, str.str(), oldLoopBody,
        movha );
  }

  // Insert LOOP instruction.
  TCCODESEL->setCurrentInstruction( insLOOP( loopAReg, newLoopBody->GetLabel(),
    TCCODESEL->getLastLLIRFunction()->GetLastBB(),
    TCCODESEL->getLastLLIRFunction()->GetLastBB()->GetLastIns() ) );
  // Move flow facts from oldLoopBody to newLoopBody.
  if ( oldLoopBody->getHandler().hasObjective( LLIR_Flowfactref::getID() ) ) {
    LLIR_Flowfactupdater ffupdater;
    ffupdater.flowfactsExchangeBB(     oldLoopBody, newLoopBody );
    ffupdater.flowfactsMoveLoopbounds( oldLoopBody, newLoopBody );
  }

  TCCODESEL->getLastLLIRFunction()->GetLastBB()->AddPragma(
  new LLIR_Pragma( "Loop condition: DOWHILE", true ) );

  // WIR
  // Determine first block of the loop body.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->getWIRBlock( doWhileStmt.getBasicBlock()->getSuccs()[ 0 ] );

  // Create a new basic block b2 immediately after b1. The idea is to move all
  // instructions from b1 to b2 and to place the LEA instruction with the
  // address register required for LOOP in b1. Thus, the CFG is still preserved,
  // i.e., all paths entering the old loop structure will pass the LEA
  // initialization before entering the actual loop.
  TC179x_wirBB = &b1;
  auto &b2 = TCCODESEL->startNewBasicBlock();
  // TODO: Back-annotation mapping!

  // Move all instructions from b1 to b2.
  for ( WIR_Instruction &i : b1 )
    b2.pushBackInstruction( i );

  for ( WIR_Instruction &i : b1 )
    for ( WIR_Operation &o : i )
      for ( WIR_Parameter &p : o )
        p.setDontOptimize( false );
  b1.clearInstructions();

  // Generate LEA in basic block b1.
  TC179x_wirBB = &b1;
  auto &lc = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertLEA(
    lc, loopCounterValue, exp, InstructionFactory::DOWHILE_STMT );

  // Generate LOOP to b2 in the current basic block.
  TC179x_wirBB = currentBB;
  TCINSTRUCTIONS.insertLOOP( lc, b2, exp, InstructionFactory::DOWHILE_STMT );
};


/*
  This function determines for a given symbol expression whether it is a
  function symbol or an array that is not a function parameter. Both cases must
  be handled separately by the loadSymbol functions, because in each case, they
  must then only load the address of the denoted symbol.
*/
bool isTrueArrayOrFunctionSymbol( const IR_SymbolExp &symExp )
{
  auto *parentAssign = dynamic_cast<IR_AssignExp *>( symExp.getParent() );
  auto *parentAddress = dynamic_cast<IR_UnaryExp *>( symExp.getParent() );
  if ( parentAddress && ( parentAddress->getOperator() != IR_UnaryExp::ADDR ) )
    parentAddress = nullptr;

  return(
    ( isFunctionType( symExp ) && ( parentAssign || parentAddress ) ) ||
    ( isArrayType( symExp ) && !isFunctionArgument( symExp ) ) );
};


/*
  Returns a new LLIR_Register object for register 'target' that matches the type
  of the given symbol expression.
*/
LLIR_Register *getNewRegister( const string &target,
                               const IR_SymbolExp &symExp )
{
  IR_Type &type = symExp.getType();

  if ( isARegType( type ) || isTrueArrayOrFunctionSymbol( symExp ) )
    return( TCINSTRUCTIONS.CreateRegister( target, true ) );
  else
    return(
      type.bitSize() <= 32 ?
        TCINSTRUCTIONS.CreateRegister( target ) :
        TCINSTRUCTIONS.CreateERegister( target ) );
};


/*
  getNewRegister creates a new virtual TriCore WIR register for the given IR
  type.

  Depending on the type, getNewRegister generates either a data register, an
  address register or an extended data register.
*/
WIR_VirtualRegister &getNewRegister( const IR_Type &t )
{
  DSTART( "WIR_VirtualRegister& getNewRegister(const IR_Type&)" );

  if ( isARegType( t ) )
    return( TCINSTRUCTIONS.createAReg() );
  else
    return(
      t.bitSize() <= 32 ?
        static_cast<WIR_VirtualRegister &>( TCINSTRUCTIONS.createDReg() ) :
        static_cast<WIR_VirtualRegister &>( TCINSTRUCTIONS.createEReg() ) );
};


/*
  getNewRegister creates a new virtual TriCore WIR register for the given IR
  symbol expression.

  Depending on the symbol expression's type, getNewRegister generates either a
  data register, an address register or an extended data register.
*/
WIR_VirtualRegister &getNewRegister( const IR_SymbolExp &symExp )
{
  DSTART( "WIR_VirtualRegister& getNewRegister(const IR_SymbolExp&)" );

  if ( isTrueArrayOrFunctionSymbol( symExp ) )
    return( TCINSTRUCTIONS.createAReg() );
  return( getNewRegister( symExp.getType() ) );
};


/*
  loadGlobalSymbolCost computes the costs for loading a global symbol into an
  appropriate register.
*/
COST loadGlobalSymbolCost( const IR_SymbolExp &symExp )
{
  DSTART( "COST loadGlobalSymbolCost(const IR_SymbolExp&)" );

  COST cost = TC13::OperationFormat::AL_1.getSize();

  switch( symExp.getType().getType() ) {
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::BOOL:
    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT: {
      cost +=
        TC13::OperationFormat::AALC16BOA.getSize() +
        TC13::OperationFormat::DAC10BOA.getSize();
      break;
    }

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::FLOAT: {
      cost += TC13::OperationFormat::DALC16BOA.getSize();
      break;
    }

    case IR_Type::LONG_LONG:
    case IR_Type::UNSIGNED_LONG_LONG:
    case IR_Type::DOUBLE:
    case IR_Type::LONG_DOUBLE: {
      cost +=
        TC13::OperationFormat::AALC16BOA.getSize() +
        TC13::OperationFormat::EAC10BOA.getSize();
      break;
    }

    case IR_Type::ARRAY:
    case IR_Type::FUNCTION:
    case IR_Type::POINTER: {
      cost += TC13::OperationFormat::AALC16BOA.getSize();
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
TC_LValue loadGlobalSymbol( const IR_SymbolExp &symExp, bool loadResult )
{
  DSTART( "TC_LValue loadGlobalSymbol(const IR_SymbolExp&, bool)" );

  // Determine the symbol's label.
  IR_Symbol &sym = symExp.getSymbol();
  string label =
    ( sym.getType().getStorageClass() == IR_Type::STATIC ) ?
      TCCODESEL->getStaticName( &sym ) : sym.getWrittenName();

  // LLIR
  LLIR_Register * const reg = getNewRegister( "", symExp );
  LLIR_Register *iaddr = nullptr;

  if ( TCCODESEL->getStack()->getAddrReg( &sym ) != "" ) {
    string addrReg = TCCODESEL->getStack()->getAddrReg( &sym );
    iaddr = TCINSTRUCTIONS.CreateRegister( addrReg, true );
  } else
    // If no virtual register has been assigned to the symbol, assign a new
    // virtual register.
    iaddr = TCINSTRUCTIONS.CreateRegister( "", true );

  // Load the allocated address register with the symbol's address.
  if ( loadResult ) {
    TCINSTRUCTIONS.insertMOVH_A( iaddr, OPER_LAB_HI, label, &symExp );

    switch ( sym.getType().getType() ) {

      case IR_Type::CHAR: {
        TCINSTRUCTIONS.insertLEA(
          iaddr, OPER_BASE, iaddr, OPER_LAB_LO, label, &symExp );
        TCINSTRUCTIONS.insertLD_B( reg, OPER_BASE, iaddr, 0, &symExp );
        break;
      }

      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        TCINSTRUCTIONS.insertLEA(
          iaddr, OPER_BASE, iaddr, OPER_LAB_LO, label, &symExp );
        TCINSTRUCTIONS.insertLD_BU( reg, OPER_BASE, iaddr, 0, &symExp );
        break;
      }

      case IR_Type::SHORT: {
        TCINSTRUCTIONS.insertLEA(
          iaddr, OPER_BASE, iaddr, OPER_LAB_LO, label, &symExp );
        TCINSTRUCTIONS.insertLD_H( reg, OPER_BASE, iaddr, 0, &symExp );
        break;
      }

      case IR_Type::UNSIGNED_SHORT: {
        TCINSTRUCTIONS.insertLEA(
          iaddr, OPER_BASE, iaddr, OPER_LAB_LO, label, &symExp );
        TCINSTRUCTIONS.insertLD_HU( reg, OPER_BASE, iaddr, 0, &symExp );
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: {
        TCINSTRUCTIONS.insertLD_W(
          reg, OPER_BASE, iaddr, OPER_LAB_LO, label, &symExp );
        break;
      }

      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE: {
        TCINSTRUCTIONS.insertLEA(
          iaddr, OPER_BASE, iaddr, OPER_LAB_LO, label, &symExp );
        TCINSTRUCTIONS.insertLD_D( reg, OPER_BASE, iaddr, 0, &symExp );
        break;
      }

      case IR_Type::ARRAY: {
        TCINSTRUCTIONS.insertLEA(
          reg, OPER_BASE, iaddr, OPER_LAB_LO, label, &symExp );
        break;
      }

      case IR_Type::POINTER: {
        TCINSTRUCTIONS.insertLD_A(
          reg, OPER_BASE, iaddr, OPER_LAB_LO, label, &symExp );
        break;
      }

      case IR_Type::FUNCTION: {
        TCINSTRUCTIONS.insertLEA(
          reg, OPER_BASE, iaddr, OPER_LAB_LO, label, &symExp );
        break;
      }

      default:
        break;
    }
  }

  // WIR
  WIR_VirtualRegister *r = nullptr;

  if ( sym.getType().getType() == IR_Type::FUNCTION ) {
    auto &f = TCCODESEL->getWIRFunction( sym );

    // Load the symbol's address.
    if ( loadResult ) {
      r = &(getNewRegister( symExp ));
      auto &tmpReg = TCINSTRUCTIONS.createAReg();

      TCINSTRUCTIONS.insertMOVH_A( tmpReg, f, &symExp );
      TCINSTRUCTIONS.insertLEA(
        dynamic_cast<TC_ARegV &>( *r ), tmpReg, f, &symExp );
    }

    return(
      TC_LValue { loadResult ? reg : nullptr, label, r, f, &sym.getType() } );
  } else {
    auto &d = TCCODESEL->getWIRData( sym );

    // Load the symbol's address.
    if ( loadResult ) {
      r = &(getNewRegister( symExp ));
      auto &tmpReg = TCINSTRUCTIONS.createAReg();

      TCINSTRUCTIONS.insertMOVH_A( tmpReg, d, &symExp );

      switch ( sym.getType().getType() ) {

        case IR_Type::CHAR: {
          TCINSTRUCTIONS.insertLEA( tmpReg, tmpReg, d, &symExp );
          TCINSTRUCTIONS.insertLD_B(
            dynamic_cast<TC_DRegV &>( *r ), tmpReg, 0, &symExp );
          break;
        }

        case IR_Type::UNSIGNED_CHAR:
        case IR_Type::BOOL: {
          TCINSTRUCTIONS.insertLEA( tmpReg, tmpReg, d, &symExp );
          TCINSTRUCTIONS.insertLD_BU(
            dynamic_cast<TC_DRegV &>( *r ), tmpReg, 0, &symExp );
          break;
        }

        case IR_Type::SHORT: {
          TCINSTRUCTIONS.insertLEA( tmpReg, tmpReg, d, &symExp );
          TCINSTRUCTIONS.insertLD_H(
            dynamic_cast<TC_DRegV &>( *r ), tmpReg, 0, &symExp );
          break;
        }

        case IR_Type::UNSIGNED_SHORT: {
          TCINSTRUCTIONS.insertLEA( tmpReg, tmpReg, d, &symExp );
          TCINSTRUCTIONS.insertLD_HU(
            dynamic_cast<TC_DRegV &>( *r ), tmpReg, 0, &symExp );
          break;
        }

        case IR_Type::INT:
        case IR_Type::UNSIGNED_INT:
        case IR_Type::LONG:
        case IR_Type::UNSIGNED_LONG:
        case IR_Type::FLOAT: {
          TCINSTRUCTIONS.insertLD_W(
            dynamic_cast<TC_DRegV &>( *r ), tmpReg, d, &symExp );
          break;
        }

        case IR_Type::LONG_LONG:
        case IR_Type::UNSIGNED_LONG_LONG:
        case IR_Type::DOUBLE:
        case IR_Type::LONG_DOUBLE: {
          TCINSTRUCTIONS.insertLEA( tmpReg, tmpReg, d, &symExp );
          TCINSTRUCTIONS.insertLD_D(
            dynamic_cast<TC_ERegV &>( *r ), tmpReg, 0, &symExp );
          break;
        }

        case IR_Type::ARRAY: {
          TCINSTRUCTIONS.insertLEA(
            dynamic_cast<TC_ARegV &>( *r ), tmpReg, d, &symExp );
          break;
        }

        case IR_Type::POINTER: {
          TCINSTRUCTIONS.insertLD_A(
            dynamic_cast<TC_ARegV &>( *r ), tmpReg, d, &symExp );
          break;
        }

        default:
          break;
      }
    }

    return(
      TC_LValue { loadResult ? reg : nullptr, label, r, d, &sym.getType() } );
  }
};


/*
  loadStackSymbolCost computes the costs for loading a local stack symbol into
  its appropriate register.
*/
COST loadStackSymbolCost( const IR_SymbolExp &symExp )
{
  DSTART( "COST loadStackSymbolCost(const IR_SymbolExp&)" );

  COST cost = 0;

  switch ( symExp.getType().getType() ) {
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::BOOL:
    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT: {
      cost += TC13::OperationFormat::DAC10BOA.getSize();
      break;
    }

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::FLOAT: {
      cost += TC13::OperationFormat::DAC16BOA.getSize();
      break;
    }

    case IR_Type::LONG_LONG:
    case IR_Type::UNSIGNED_LONG_LONG:
    case IR_Type::DOUBLE:
    case IR_Type::LONG_DOUBLE: {
      cost += TC13::OperationFormat::EAC10BOA.getSize();
      break;
    }

    case IR_Type::ARRAY:
    case IR_Type::POINTER: {
      cost += TC13::OperationFormat::AAC16BOA.getSize();
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
TC_LValue loadStackSymbol( const IR_SymbolExp &symExp, bool loadResult )
{
  DSTART( "TC_LValue loadStackSymbol(const IR_SymbolExp&, bool)" );

  IR_Symbol &sym = symExp.getSymbol();
  auto *symbolInfo = TCCODESEL->getStack()->getSymbolInfo( &sym );
  ufAssertT( symbolInfo != nullptr, "Missing symbol info!" );
  const int off = TCCODESEL->getStack()->getSymbolOffset( &sym );

  // LLIR
  LLIR_Register *reg = getNewRegister( "", symExp );
  LLIR_Register *stackp = TCINSTRUCTIONS.CreateRegister( PHREG_SP, true );

  // WIR
  auto &r = getNewRegister( symExp );
  auto &sp = TCINSTRUCTIONS.createAReg();
  TC179x_wirFct->insertPrecolor( sp, TC179x_wirProc->SP() );

  // First do some administration work to keep the Stack object up to date. We
  // don't load the symbol yet, we let the control flow pass in all branches
  // where the symbol must be loaded and load it in the end. This centralizes
  // the code for loading at the end of this function.
  if ( ( symbolInfo->getSymbolType() == SymbolInfo::D_ARGUMENT ) ||
       ( symbolInfo->getSymbolType() == SymbolInfo::A_ARGUMENT ) ) {

    // Current symbol is a function argument.
    const int myRegNumber = TCCODESEL->getStack()->isPassedThroughRegister( sym );
    if ( myRegNumber ) {
      // LLIR
      LLIR_BB *theBB = TCCODESEL->getLastLLIRFunction()->GetFirstBB();
      LLIR_Register *vreg = nullptr;

      const string symReg = TCCODESEL->getStack()->getSymbolReg( &sym );
      if ( symReg != "" )
        vreg = getNewRegister( symReg, symExp );
      else {
        vreg = getNewRegister( "", symExp );
        TCCODESEL->getStack()->setSymbolReg( &sym, string( vreg->GetName() ) );
      }

      // Generate constraints for register allocator w.r.t. LC registers.
      bindToPHREG( *vreg, myRegNumber );

      // WIR
      // Retrieve/create the function argument symbol's WIR register.
      if ( !symbolInfo->isSymbolRegSet() )
        symbolInfo->setSymbolReg( getNewRegister( symExp ) );
      auto &tmpReg = symbolInfo->getSymbolReg();

      // Generate constraints for register allocator w.r.t. LC registers.
      bindToPHREG( tmpReg, myRegNumber );

      if ( !TCCODESEL->getStack()->getStoreInstructionsGenerated( &sym ) ) {
        // If the address of a function argument passed in a register is taken,
        // we need to store the argument at the already reserved stack position
        // so that its address can be taken legally.

        // LLIR
        LLIR_Instruction *insSt = nullptr;

        switch ( sym.getType().getType() ) {
          case IR_Type::CHAR:
          case IR_Type::UNSIGNED_CHAR:
          case IR_Type::BOOL: {
            insSt = insST_B( OPER_BASE, stackp, off, vreg );
            break;
          }

          case IR_Type::SHORT:
          case IR_Type::UNSIGNED_SHORT: {
            insSt = insST_H( OPER_BASE, stackp, off, vreg );
            break;
          }

          case IR_Type::INT:
          case IR_Type::UNSIGNED_INT:
          case IR_Type::LONG:
          case IR_Type::UNSIGNED_LONG:
          case IR_Type::FLOAT: {
            insSt = insST_W( OPER_BASE, stackp, off, vreg );
            break;
          }

          case IR_Type::LONG_LONG:
          case IR_Type::UNSIGNED_LONG_LONG:
          case IR_Type::DOUBLE:
          case IR_Type::LONG_DOUBLE: {
            insSt = insST_D( OPER_BASE, stackp, off, vreg );
            break;
          }

          case IR_Type::ARRAY: {
            if ( isFunctionArgument( symExp ) ) {
              insSt = insST_A( OPER_BASE, stackp, off, vreg );
            } else {
              // No need to store, the array is on the stack anyways
            }
            break;
          }

          case IR_Type::POINTER: {
            insSt = insST_A( OPER_BASE, stackp, off, vreg );
            break;
          }

          default: {
            ufAssertT( 0, "Uncovered case!" );
            break;
          }
        }

        if ( insSt ) {
          theBB->InsertIns( insSt );
          if ( !TCCODESEL->getCurrentInstruction() &&
                TCCODESEL->getLastLLIRFunction()->GetNumberOfBB() == 1 )
            TCCODESEL->setCurrentInstruction( insSt );
        }

        // WIR
        auto &b = TC179x_wirFct->begin()->get();

        switch ( sym.getType().getType() ) {
          case IR_Type::CHAR:
          case IR_Type::UNSIGNED_CHAR:
          case IR_Type::BOOL: {
            b.pushFrontInstruction(
              { { TC13::OpCode::ST_B, TC13::OperationFormat::AC10DBOA_1,
                  new WIR_RegisterParameter( sp, WIR_Usage::use ),
                  new TC_Const10_Signed( off ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
            break;
          }

          case IR_Type::SHORT:
          case IR_Type::UNSIGNED_SHORT: {
            b.pushFrontInstruction(
              { { TC13::OpCode::ST_H, TC13::OperationFormat::AC10DBOA_1,
                  new WIR_RegisterParameter( sp, WIR_Usage::use ),
                  new TC_Const10_Signed( off ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
            break;
          }

          case IR_Type::INT:
          case IR_Type::UNSIGNED_INT:
          case IR_Type::LONG:
          case IR_Type::UNSIGNED_LONG:
          case IR_Type::FLOAT: {
            b.pushFrontInstruction(
              { { TC13::OpCode::ST_W, TC13::OperationFormat::AC16DBOA,
                  new WIR_RegisterParameter( sp, WIR_Usage::use ),
                  new TC_Const16_Signed( off ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
            break;
          }

          case IR_Type::LONG_LONG:
          case IR_Type::UNSIGNED_LONG_LONG:
          case IR_Type::DOUBLE:
          case IR_Type::LONG_DOUBLE: {
            b.pushFrontInstruction(
              { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EBOA,
                  new WIR_RegisterParameter( sp, WIR_Usage::use ),
                  new TC_Const10_Signed( off ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
            break;
          }

          case IR_Type::ARRAY: {
            if ( isFunctionArgument( symExp ) )
              b.pushFrontInstruction(
                { { TC13::OpCode::ST_A, TC13::OperationFormat::AC10ABOA,
                    new WIR_RegisterParameter( sp, WIR_Usage::use ),
                    new TC_Const10_Signed( off ),
                    new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
            break;
          }

          case IR_Type::POINTER: {
            b.pushFrontInstruction(
              { { TC13::OpCode::ST_A, TC13::OperationFormat::AC10ABOA,
                  new WIR_RegisterParameter( sp, WIR_Usage::use ),
                  new TC_Const10_Signed( off ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ) } } );
            break;
          }

          default:
            break;
        }

        TCCODESEL->getStack()->setStoreInstructionsGenerated( &sym, true );
      }

    } else {
      // The symbol is passed via the stack.

      // TODO: We could load it each time it is accessed, but it is more clever
      //       to only load it once and then to use the loaded version for
      //       further accesses. This only works if the argument's address has
      //       not been taken. The problem with the current setup is that we can
      //       not do this at the moment, because for such a stack-passed
      //       argument, this function is called inevitably by the rules.
      //       Unfortunately, this function just returns a TC_LValue, whereas
      //       we'd need to return the proper register. So, what we need are new
      //       rules which produce a 'dreg'/'areg'/... for stack-passed
      //       arguments as described above.

      TCCODESEL->getStack()->setSymbolReg( &sym, reg->GetName() );
      TCCODESEL->getStack()->setSymbolReg( sym, r );
    }
  }

  // Now finally do the access (the location is always the same).
  if ( loadResult ) {
    // For the sake of efficiency, use different load instructions depending on
    // the type of the variable.

    // LLIR
    switch ( sym.getType().getType() ) {
      case IR_Type::CHAR: {
        TCINSTRUCTIONS.insertLD_B( reg, OPER_BASE, stackp, off, &symExp );
        break;
      }

      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        TCINSTRUCTIONS.insertLD_BU( reg, OPER_BASE, stackp, off, &symExp );
        break;
      }

      case IR_Type::SHORT: {
        TCINSTRUCTIONS.insertLD_H( reg, OPER_BASE, stackp, off, &symExp );
        break;
      }

      case IR_Type::UNSIGNED_SHORT: {
        TCINSTRUCTIONS.insertLD_HU( reg, OPER_BASE, stackp, off, &symExp );
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: {
        TCINSTRUCTIONS.insertLD_W( reg, OPER_BASE, stackp, off, &symExp );
        break;
      }

      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE: {
        TCINSTRUCTIONS.insertLD_D( reg, OPER_BASE, stackp, off, &symExp );
        break;
      }

      case IR_Type::ARRAY: {
        if ( isFunctionArgument( symExp ) ) {
          TCINSTRUCTIONS.insertLD_A( reg, OPER_BASE, stackp, off, &symExp );
        } else {
          TCINSTRUCTIONS.insertLEA( reg, OPER_BASE, stackp, off, &symExp );
        }
        break;
      }

      case IR_Type::POINTER: {
        TCINSTRUCTIONS.insertLD_A( reg, OPER_BASE, stackp, off, &symExp );
        break;
      }

      default:
        break;
    }

    // WIR
    switch ( sym.getType().getType() ) {
      case IR_Type::CHAR: {
        TCINSTRUCTIONS.insertLD_B(
          dynamic_cast<TC_DRegV &>( r ), sp, off, &symExp );
        break;
      }

      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        TCINSTRUCTIONS.insertLD_BU(
          dynamic_cast<TC_DRegV &>( r ), sp, off, &symExp );
        break;
      }

      case IR_Type::SHORT: {
        TCINSTRUCTIONS.insertLD_H(
          dynamic_cast<TC_DRegV &>( r ), sp, off, &symExp );
        break;
      }

      case IR_Type::UNSIGNED_SHORT: {
        TCINSTRUCTIONS.insertLD_HU(
          dynamic_cast<TC_DRegV &>( r ), sp, off, &symExp );
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: {
        TCINSTRUCTIONS.insertLD_W(
          dynamic_cast<TC_DRegV &>( r ), sp, off, &symExp );
        break;
      }

      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE: {
        TCINSTRUCTIONS.insertLD_D(
          dynamic_cast<TC_ERegV &>( r ), sp, off, &symExp );
        break;
      }

      case IR_Type::ARRAY: {
        if ( isFunctionArgument( symExp ) )
          TCINSTRUCTIONS.insertLD_A(
            dynamic_cast<TC_ARegV &>( r ), sp, off, &symExp );
        else
          TCINSTRUCTIONS.insertLEA(
            dynamic_cast<TC_ARegV &>( r ), sp, off, &symExp );
        break;
      }

      case IR_Type::POINTER: {
        TCINSTRUCTIONS.insertLD_A(
          dynamic_cast<TC_ARegV &>( r ), sp, off, &symExp );
        break;
      }

      default:
        break;
    }
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, loadResult ? &r : nullptr,
      TC_AddressModification { stackp, sp, off, &sym.getType(), true } } );
};


/*
  loadRegisterSymbolCost computes the cost for using loadRegSym( symExp ).
*/
COST loadRegisterSymbolCost( const IR_SymbolExp &symExp )
{
  DSTART( "COST loadRegisterSymbolCost(const IR_SymbolExp&)" );

  if ( isARegType( symExp ) )
    return( TC13::OperationFormat::SAA_1.getSize() );
  else
    return(
      ( ( symExp.getType().bitSize() <= 32 ) ? 1 : 2 ) *
      TC13::OperationFormat::SDD_1.getSize() );
};


/*
  Loads the non-stack, non-global symbol 'sym' into it's appropriate register,
  as determined by the settings in the Stack class and then returns the
  register.
*/
LLIR_Register *loadRegisterSymbol( IR_SymbolExp &symExp )
{
  IR_Symbol &sym = symExp.getSymbol();
  auto *symbolInfo = TCCODESEL->getStack()->getSymbolInfo( &sym );
  ufAssertT( symbolInfo != nullptr, "Missing symbol info!" );

  LLIR_Register *reg0 = nullptr;

  if ( ( symbolInfo->getSymbolType() == SymbolInfo::D_ARGUMENT ) ||
       ( symbolInfo->getSymbolType() == SymbolInfo::A_ARGUMENT ) ) {

    // Current symbol is a function argument.
    const int myRegNumber = TCCODESEL->getStack()->isPassedThroughRegister( sym );
    if ( myRegNumber ) {
      LLIR_BB *theBB = TCCODESEL->getLastLLIRFunction()->GetFirstBB();
      LLIR_Register *vreg = nullptr;

      const string symReg = TCCODESEL->getStack()->getSymbolReg( &sym );
      if ( symReg != "" )
        vreg = getNewRegister( symReg, symExp );
      else {
        vreg = getNewRegister( "", symExp );
        TCCODESEL->getStack()->setSymbolReg( &sym, string( vreg->GetName() ) );
      }

      // Generate constraints for register allocator w.r.t. LC registers.
      bindToPHREG( *vreg, myRegNumber );

      // If not existing, generate a new function internal VREG for this
      // argument, else use the existing one.
      const string viSym = TCCODESEL->getStack()->getInternalVReg( &sym );
      if ( viSym != "" )
        reg0 = getNewRegister( viSym, symExp );
      else {
        // Add a MOV from the external to the internal VREG at the very
        // beginning of the current LLIR function, to save the argument into the
        // upper context. Use a plain new register as the internal reg.
        reg0 = getNewRegister( "", symExp );
        TCCODESEL->getStack()->setInternalVReg( &sym, reg0->GetName() );

        LLIR_Instruction *insMov = nullptr;
        if ( isARegType( sym ) ) {
          insMov = insMOV_AA( reg0, vreg );
          theBB->InsertIns( insMov );
        } else

        if ( sym.getType().bitSize() <= 32 ) {
          insMov = insMOV( reg0, vreg );
          theBB->InsertIns( insMov );
        } else {
          insMov = insMOV( reg0->GetFirstChild(), vreg->GetFirstChild() );
          theBB->InsertIns( insMov );
          insMov =
            insMOV(
              reg0->GetNextChild( reg0->GetFirstChild() ),
              vreg->GetNextChild( vreg->GetFirstChild() ) );
          theBB->InsertIns( insMov );
        }

        if ( !TCCODESEL->getCurrentInstruction() &&
             ( TCCODESEL->getLastLLIRFunction()->GetNumberOfBB() == 1 ) )
          TCCODESEL->setCurrentInstruction( insMov );
      }
    } else
      ufAssertT( 0, "Cost function should prevent this." );

  } else

  if ( symbolInfo->getSymbolType() == SymbolInfo::LOCAL_VAR ) {
    // If no virtual register has been assigned to the symbol, assign a new
    // virtual register, else use the existing one.
    const string symReg = TCCODESEL->getStack()->getSymbolReg( &sym );

    if ( symReg != "" )
      reg0 = getNewRegister( symReg, symExp );
    else {
      reg0 = getNewRegister( "", symExp );
      TCCODESEL->getStack()->setSymbolReg( &sym, reg0->GetName() );
    }
  } else
    ufAssertT( 0, "Cost function should prevent this." );

  ufAssertT( reg0, "Internal error: Failed to create symbol register!" );

  return( reg0 );
};


/*
  loadRegSym loads the non-stack, non-global symbol 'sym' into its appropriate
  register, as determined by the settings in the Stack class and then returns
  the register.
*/
WIR_BaseRegister &loadRegSym( const IR_SymbolExp &symExp )
{
  DSTART( "WIR_BaseRegister& loadRegSym(const IR_SymbolExp&)" );

  IR_Symbol &sym = symExp.getSymbol();
  auto *symbolInfo = TCCODESEL->getStack()->getSymbolInfo( &sym );
  ufAssertT( symbolInfo != nullptr, "Missing symbol info!" );

  if ( ( symbolInfo->getSymbolType() == SymbolInfo::D_ARGUMENT ) ||
       ( symbolInfo->getSymbolType() == SymbolInfo::A_ARGUMENT ) ) {

    // Current symbol is a function argument.
    const int myRegNumber = TCCODESEL->getStack()->isPassedThroughRegister( sym );
    #ifdef DEBUG_WCC
    ufAssertT( myRegNumber != 0, "Cost function should prevent this." );
    #endif

    // Retrieve/create the function argument symbol's WIR register.
    if ( symbolInfo->isSymbolRegSet() ) {
      auto &r = symbolInfo->getSymbolReg();

      if ( r.getFunction() != *TC179x_wirFct )
        symbolInfo->setSymbolReg( getNewRegister( symExp ) );
    } else
      symbolInfo->setSymbolReg( getNewRegister( symExp ) );
    WIR_VirtualRegister &vreg = symbolInfo->getSymbolReg();

    // Generate constraints for register allocator w.r.t. LC registers.
    bindToPHREG( vreg, myRegNumber );

    // Annotate the pre-colored VREG as function input.
    vreg.getFunction().addFunctionInput( vreg.getPrecolor() );

    // If not yet existing, create the function-internal virtual WIR register.
    if ( !symbolInfo->isInternalVRegSet() ) {
      symbolInfo->setInternalVReg( getNewRegister( symExp ) );
      WIR_VirtualRegister &reg0 = symbolInfo->getInternalVReg();

      // Add a MOV from the external to the internal VREG at the very beginning
      // of the current WIR function in order to save the argument in an upper-
      // context register.
      WIR_BasicBlock &b = TC179x_wirFct->begin()->get();

      if ( isARegType( sym ) )
        b.pushFrontInstruction(
          { { TC13::OpCode::MOV_AA,
              TCCODESEL->getGenerate16BitOperations() ?
                TC13::OperationFormat::SAA_1 : TC13::OperationFormat::AA,
              new WIR_RegisterParameter( reg0, WIR_Usage::def ),
              new WIR_RegisterParameter( vreg, WIR_Usage::use ) } } );
      else

      if ( sym.getType().bitSize() <= 32 )
        b.pushFrontInstruction(
          { { TC13::OpCode::MOV_RR,
              TCCODESEL->getGenerate16BitOperations() ?
                TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
              new WIR_RegisterParameter( reg0, WIR_Usage::def ),
              new WIR_RegisterParameter( vreg, WIR_Usage::use ) } } );
      else {
        b.pushFrontInstruction(
          { { TC13::OpCode::MOV_RR,
              TCCODESEL->getGenerate16BitOperations() ?
                TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
              new WIR_RegisterParameter( reg0.rbegin()->get(), WIR_Usage::def ),
              new WIR_RegisterParameter(
                vreg.rbegin()->get(), WIR_Usage::use ) } } );
        b.pushFrontInstruction(
          { { TC13::OpCode::MOV_RR,
              TCCODESEL->getGenerate16BitOperations() ?
                TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
              new WIR_RegisterParameter( reg0.begin()->get(), WIR_Usage::def ),
              new WIR_RegisterParameter(
                vreg.begin()->get(), WIR_Usage::use ) } } );
      }

      return( reg0 );
    }

    return( symbolInfo->getInternalVReg() );

  }

  #ifdef DEBUG_WCC
  ufAssert( symbolInfo->getSymbolType() == SymbolInfo::LOCAL_VAR );
  #endif

  // Retrieve/create the function argument symbol's WIR register.
  if ( !symbolInfo->isSymbolRegSet() )
    symbolInfo->setSymbolReg( getNewRegister( symExp ) );
  return( symbolInfo->getSymbolReg() );
};


int addModifier( int acc, char c )
{
  switch ( c ) {
    case '=':
      return( acc | static_cast<char>( Modifier::WRITE ) );
    case '+':
      return( acc | static_cast<char>( Modifier::READWRITE ) );
    case '&':
      return( acc | static_cast<char>( Modifier::EARLYCLOB ) );
    default:
      return( acc );
  }
};


Constraint readConstraint( const string &s )
{
  ufAssertT( s.length() > 0, "Constraint string may not be empty" );

  char last = s.at( s.length() - 1 );
  if ( isdigit( last ) )
    return( Constraint::PREVIOUS );

  switch ( last ) {
    case 'a':
      return( Constraint::AREG );
    case 'd':
      return( Constraint::DREG );
    case 'i':
      return( Constraint::CONST );
    case 'm':
      return( Constraint::MEMORY );
    default: {
      ostringstream errmsg;
      errmsg << "Unknown operand constraint: " << last;
      throw ufFatalError( errmsg.str() );
      // Get rid of compiler warning.
      return( Constraint::AREG );
    }
  }
};


LLIR_Register *loadRegister( IR_SymbolExp *symExp )
{
  if ( !symExp )
    return( nullptr );

  if ( symExp->getSymbol().isGlobal() )
    return( loadGlobalSymbol( *symExp, true ).getResultRegister() );

  if ( TCCODESEL->getStack()->getSymbolOffset( &symExp->getSymbol() ) >= 0 )
    return( loadStackSymbol( *symExp, true ).getResultRegister() );

  return( loadRegisterSymbol( *symExp ) );
};


/*
  loadRegs emits code to load the given symbol expression into a TriCore
  register.
*/
WIR::WIR_BaseRegister &loadReg( const IR_SymbolExp &e )
{
  DSTART( "WIR_BaseRegister& loadReg(const IR_SymbolExp&)" );

  if ( e.getSymbol().isGlobal() )
    return( *(loadGlobalSymbol( e, true ).getResultReg()) );

  if ( TCCODESEL->getStack()->getSymbolOffset( &(e.getSymbol()) ) >= 0 )
    return( *(loadStackSymbol( e, true ).getResultReg()) );

  return( loadRegSym( e ) );
};


void operandParseError( IR_Exp *exp, const string &err1, const string &err2 )
{
  ostringstream str;
  exp->write( str );
  IR_Stmt &stmt = exp->getStmt();

  throw
    ufFatalError(
      stmt.getFileContext().getFilename(), stmt.getFileContext().getLine(),
      err1 + str.str() + err2 );
};


/*
  Returns whether the given type is a pointer type.
*/
bool isPointerType( const IR_Type &t )
{
  return(
    ( t.getType() == IR_Type::POINTER ) ||
    // The IR produces wrong type objects with invalid type ids from time to
    // time. So, we must try to cast the object as well, because in those cases,
    // this is the only thing that works.
    ( dynamic_cast<const IR_PointerType *>( &t ) &&
      !dynamic_cast<const IR_ArrayType *>( &t ) ) );
};


/*
  Returns whether the given type is an array type.
*/
bool isArrayType( const IR_Type &t )
{
  return(
    ( t.getType() == IR_Type::ARRAY ) ||
    // The IR produces wrong type objects with invalid type ids from time to
    // time. So, we must try to cast the object as well, because in those cases,
    // this is the only thing that works.
    dynamic_cast<const IR_ArrayType *>( &t ) );
};


/*
  Returns whether the given type is a composed type.
*/
bool isComposedType( const IR_Type &t )
{
  return(
    ( t.getType() == IR_Type::STRUCT ) ||
    ( t.getType() == IR_Type::UNION ) ||
    // The IR produces wrong type objects with invalid type ids from time to
    // time. So, we must try to cast the object as well, because in those cases,
    // this is the only thing that works.
    dynamic_cast<const IR_ComposedType *>( &t ) );
};


/*
  Returns whether the given type is a function type.
*/
bool isFunctionType( const IR_Type &t )
{
  return(
    ( t.getType() == IR_Type::FUNCTION ) ||
    // The IR produces wrong type objects with invalid type ids from time to
    // time. So, we must try to cast the object as well, because in those cases,
    // this is the only thing that works.
    dynamic_cast<const IR_FunctionType *>( &t ) );
};


/*
  Returns whether the given type is a bitfield type.
*/
bool isBitfieldType( const IR_Type &t )
{
  return( dynamic_cast<const IR_BitfieldType *>( &t ) );
};


/*
  Returns whether the given type is a long long type.
*/
bool isLongLongType( const IR_Type &t )
{
  return(
    ( t.getType() == IR_Type::LONG_LONG ) ||
    ( t.getType() == IR_Type::UNSIGNED_LONG_LONG ) );
};


/*
  Returns whether the given int const expression has long long type.
*/
bool isLongLongConstant( const IR_IntConstExp &constExp )
{
  return( isLongLongType( constExp ) );
};


/*
  Returns whether the given type is a double type.
*/
bool isDoubleType( const IR_Type &t )
{
  return(
    ( t.getType() == IR_Type::DOUBLE ) ||
    ( t.getType() == IR_Type::LONG_DOUBLE ) );
};


/*
  Returns whether the given type is float (no double/long double).
*/
bool isFloatType( const IR_Type &t )
{
  return( t.getType() == IR_Type::FLOAT );
};


/*
  Returns whether the given type is a character type.
*/
bool isCharType( const IR_Type &t )
{
  return(
    ( t .getType() == IR_Type::CHAR ) ||
    ( t.getType() == IR_Type::UNSIGNED_CHAR ) );
};


/*
  Returns whether the given type is a short integer type.
*/
bool isShortType( const IR_Type &t )
{
  return(
    ( t.getType() == IR_Type::SHORT ) ||
    ( t.getType() == IR_Type::UNSIGNED_SHORT ) );
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


/*
  Returns whether the given type is a type that can be represented by
  nonterminal 'areg'.
*/
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


/*
  Returns whether the given type is a type that must be stored in an extended
  register.
*/
bool isERegType( const IR_Type &t )
{
  return(
    ( t.getType() == IR_Type::DOUBLE ) ||
    ( t.getType() == IR_Type::LONG_DOUBLE ) ||
    ( t.getType() == IR_Type::LONG_LONG ) ||
    ( t.getType() == IR_Type::UNSIGNED_LONG_LONG ) );
};
