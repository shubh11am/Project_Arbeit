/*
  Verbatim C code section for the C++ header file
*/

%code requires {
/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2015 - 2022, Heiko Falk.

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


//
// Include section
//

// Include standard headers
#include <memory>
#include <string>
#include <vector>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>


//
// Class forward declarations
//

namespace WIR {

class TC_AsmAddress;
class TC_AsmArgument;
class TC_AsmConstant;
class TC_AsmParser;
class TC_AsmRegister;

/*!
  @brief This enum represents optional specifiers for 16 or 32 bits wide
         operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class TC_AsmOperationWidth : unsigned char
{
  //! A 16 bits wide operation.
  CODE16,

  //! A 32 bits wide operation.
  CODE32,

  //! An operation with unspecified bit width.
  UNSPECIFIED
};

/*!
  @brief This struct serves to store a TriCore mnemonic along with an optional
         bit width specified.
*/
struct MnemonicData
{
  TC_AsmOperationWidth w;
  const WIR_BaseProcessor::OpCode *m;
};

}

}


/*
  Verbatim C code section for the C++ file
*/

%code {
/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2015 - 2022, Heiko Falk.

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <sstream>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/stringtools.h>

// Include local headers
#include <arch/tricore/asmparser/tcasmaddress.h>
#include <arch/tricore/asmparser/tcasmargument.h>
#include <arch/tricore/asmparser/tcasmconstant.h>
#include <arch/tricore/asmparser/tcasmcontext.h>
#include <arch/tricore/asmparser/tcasmlabel.h>
#include <arch/tricore/asmparser/tcasmlex.h>
#include <arch/tricore/asmparser/tcasmparser.h>
#include <arch/tricore/asmparser/tcasmregister.h>
#include <arch/tricore/asmparser/tcasmtemplateregister.h>
#include "location.hh"

/*
  The following lines 'connect' the bison parser to the flex scanner object. It
  defines the yylex() function call to pull the next token from the current
  lexer object of the driver context.
*/
#undef yylex
#define yylex driver.mScanner->lex

//! The following macro serves as little helper to push back operations.
#define PB_OP driver.mContext->pushBackOperation


using namespace std;

}

/*
  Bison options section
*/

/* Require bison 3.0.4 or later. */
%require "3.0.4"

/* Enable parser to generate debug output. */
%debug

/* Start symbol is named 'operations'. */
%start operations

/* Use newer C++ skeleton file. */
%skeleton "lalr1.cc"

/* Namespace to enclose parser in. */
%define api.namespace {WIR}

/* Set the parser's class identifier. */
%define parser_class_name {TC_AsmYacc}

/* Keep track of the current position within the input. */
%locations

/* Initialize location object. */
%initial-action
{
  @$.begin.filename = @$.end.filename = &driver.mStreamName;
};

/*
  The driver is passed by reference to the parser and to the scanner. This
  provides a simple but effective pure interface, not relying on global
  variables.
*/
%parse-param { TC_AsmParser &driver }

/* Verbose error messages. */
%define parse.error verbose


/*
  The semantic type / yylval.
*/

%union
{
  const WIR_BaseProcessor::OpCode *mnemonic;
  const TC_ARegP *aReg;
  const TC_DRegP *dReg;
  const TC_ERegP *eReg;
  signed long long intVal;
  unsigned long long uIntVal;
  char *stringVal;

  MnemonicData mnemonicInfo;
  std::vector<TC_AsmArgument *> *arguments;
  TC_AsmArgument *argument;
  TC_AsmRegister *regArg;
  TC_AsmAddress *addressArg;
  TC_AsmConstant *constantArg;
};


/*
  Tokens section
*/

%token END 0 "end of file";
%token COMMA COLON SEMICOLON;
%token NEWLINE BREV CIRC PLUS LBRACKET RBRACKET HI LO CODE16 CODE32;
%token <mnemonic> MNEMONIC;
%token <uIntVal> TPLARG;
%token <uIntVal> TPLARGMODA;
%token <uIntVal> TPLARGMODH;
%token <uIntVal> TPLARGMODL;
%token <stringVal> STRING;
%token <aReg> AREG;
%token <aReg> AREG10;
%token <aReg> AREG15;
%token <dReg> DREG;
%token <dReg> DREG15;
%token <dReg> DREGL;
%token <dReg> DREGLL;
%token <dReg> DREGLU;
%token <dReg> DREGU;
%token <dReg> DREGUL;
%token <dReg> DREGUU;
%token <eReg> EREG;
%token <intVal> IMMEDIATE_S;
%token <uIntVal> IMMEDIATE_U;
%token <uIntVal> TPLARGL;
%token <uIntVal> TPLARGLL;
%token <uIntVal> TPLARGLU;
%token <uIntVal> TPLARGU;
%token <uIntVal> TPLARGUL;
%token <uIntVal> TPLARGUU;


/*
  Types / non-terminals section
*/

%type <stringVal> label;
%type <mnemonicInfo> mnemonic;
%type <arguments> args;
%type <argument> arg;
%type <addressArg> address;
%type <constantArg> constant;
%type <regArg> reg;
%type <regArg> iareg;
%type <regArg> iaregbrev;
%type <regArg> iaregcirc;
%type <regArg> iaregpostinc;
%type <regArg> iaregpreinc;
%type <intVal> offset;
%type <regArg> indreg;

%%


/*
  Rules section
*/

operations
  : operation
  | label
    {
      driver.mContext->insertBasicBlock( trim( string( $1 ) ) );
      delete( $1 );
    }
  | operations operation
  | operations label
    {
      driver.mContext->insertBasicBlock( trim( string( $2 ) ) );
      delete( $2 );
    }
  ;

/* Parse TriCore operations. */
operation
  : mnemonic args eol
    {
      bool res = driver.mContext->pushBackOperation( $1, *$2 );
      delete( $2 );

      if ( !res ) {
        stringstream str;
        str << "Parameters incompatible with ";
        if ( $1.w == TC_AsmOperationWidth::CODE16 )
          str << "16-bit ";
        else
        if ( $1.w == TC_AsmOperationWidth::CODE32 )
          str << "32-bit ";
        str << "opcode '" << $1.m->getName() << "'";
        error( @$, str.str() );
        YYERROR;
      }
    }
  | mnemonic eol
    {
      bool res = driver.mContext->pushBackOperation( $1, {} );

      if ( !res ) {
        stringstream str;
        str << "Parameters incompatible with ";
        if ( $1.w == TC_AsmOperationWidth::CODE16 )
          str << "16-bit ";
        else
        if ( $1.w == TC_AsmOperationWidth::CODE32 )
          str << "32-bit ";
        str << "opcode '" << $1.m->getName() << "'";
        error( @$, str.str() );
        YYERROR;
      }
    }
  ;

label
  : STRING COLON
    {
      $$ = $1;
    }
  | STRING COLON NEWLINE
    {
      $$ = $1;
    }
  ;

eol
  : SEMICOLON
  | NEWLINE
  | END
  ;

mnemonic
  : CODE16 MNEMONIC
    {
      $$ = { TC_AsmOperationWidth::CODE16, $2 };
    }
  | CODE16 NEWLINE MNEMONIC
    {
      $$ = { TC_AsmOperationWidth::CODE16, $3 };
    }
  | CODE32 MNEMONIC
    {
      $$ = { TC_AsmOperationWidth::CODE32, $2 };
    }
  | CODE32 NEWLINE MNEMONIC
    {
      $$ = { TC_AsmOperationWidth::CODE32, $3 };
    }
  | MNEMONIC
    {
      $$ = { TC_AsmOperationWidth::UNSPECIFIED, $1 };
    }
  ;

args
  : arg
    {
      $$ = new vector<TC_AsmArgument *>;
      $$->push_back( $1 );
    }
  | args COMMA arg
    {
      $$ = $1;
      $$->push_back( $3 );
    }
  ;

arg
  : address
    {
      $$ = $1;
    }
  | constant
    {
      $$ = $1;
    }
  | reg
    {
      $$ = $1;
    }
  | STRING
    {
      $$ = new TC_AsmLabel( string( $1 ) );
      delete( $1 );
    }
  | TPLARG
    {
      $$ = driver.mContext->getTemplateArgument( $1 );
    }
  | TPLARGMODA
    {
      auto *arg = driver.mContext->getTemplateArgument( $1 );
      auto *reg = dynamic_cast<TC_AsmTemplateRegister *>( arg );

      if ( !reg ) {
        auto *r = dynamic_cast<TC_AsmRegister *>( arg );
        if ( r && ( r->getType() == TC_AsmArgument::Type::EREG ) )
          $$ = r;
        else {
          stringstream sstr;
          sstr << "Template argument '%A" << $1 << "' does not represent an "
               << "extended register.";
          error( @$, sstr.str() );
        }
      } else {
        reg->setEReg();
        $$ = reg;
      }
    }
  | TPLARGMODH
    {
      auto *arg = driver.mContext->getTemplateArgument( $1 );
      auto *reg = dynamic_cast<TC_AsmTemplateRegister *>( arg );

      if ( !reg ) {
        auto *r = dynamic_cast<TC_AsmRegister *>( arg );
        if ( r && ( r->getType() == TC_AsmArgument::Type::EREG ) ) {
          auto &eReg = dynamic_cast<const TC_ERegV &>( r->getRegister() );
          $$ =
            new TC_AsmRegister(
              eReg.rbegin()->get(), TC_AsmArgument::Type::DREG );
        } else {
          stringstream sstr;
          sstr << "Template argument '%H" << $1 << "' does not represent an "
               << "extended register.";
          error( @$, sstr.str() );
        }
      } else
        $$ = reg->getChildRegister( 1 );
    }
  | TPLARGMODL
    {
      auto *arg = driver.mContext->getTemplateArgument( $1 );
      auto *reg = dynamic_cast<TC_AsmTemplateRegister *>( arg );

      if ( !reg ) {
        auto *r = dynamic_cast<TC_AsmRegister *>( arg );
        if ( r && ( r->getType() == TC_AsmArgument::Type::EREG ) ) {
          auto &eReg = dynamic_cast<const TC_ERegV &>( r->getRegister() );
          $$ =
            new TC_AsmRegister(
              eReg.begin()->get(), TC_AsmArgument::Type::DREG );
        } else {
          stringstream sstr;
          sstr << "Template argument '%L" << $1 << "' does not represent an "
               << "extended register.";
          error( @$, sstr.str() );
        }
      } else
        $$ = reg->getChildRegister( 0 );
    }
  ;

address
  : iareg
    {
      $$ = new TC_AsmAddress( $1, 0, TC_AsmArgument::Type::AMODE_REGI );
    }
  | iareg offset
    {
      try {
        $$ = new TC_AsmAddress( $1, $2, TC_AsmArgument::Type::AMODE_BASE );
      } catch ( invalid_argument &e ) {
        stringstream str;
        str << e.what() << " Incompatible with base+offset addressing";
        error( @$, str.str() );
        YYERROR;
      }
    }
  | iaregbrev
    {
      try {
        $$ = new TC_AsmAddress( $1, 0, TC_AsmArgument::Type::AMODE_BREV );
      } catch ( invalid_argument &e ) {
        stringstream str;
        str << e.what() << " Incompatible with bit-reverse addressing";
        error( @$, str.str() );
        YYERROR;
      }
    }
  | iaregcirc offset
    {
      try {
        $$ = new TC_AsmAddress( $1, $2, TC_AsmArgument::Type::AMODE_CIRC );
      } catch ( invalid_argument &e ) {
        stringstream str;
        str << e.what() << " Incompatible with circular addressing";
        error( @$, str.str() );
        YYERROR;
      }
    }
  | iaregpostinc
    {
      $$ = new TC_AsmAddress( $1, 0, TC_AsmArgument::Type::AMODE_POSTINCCNST );
    }
  | iaregpostinc offset
    {
      try {
        $$ = new TC_AsmAddress( $1, $2, TC_AsmArgument::Type::AMODE_POSTINC );
      } catch ( invalid_argument &e ) {
        stringstream str;
        str << e.what() << " Incompatible with post-increment addressing";
        error( @$, str.str() );
        YYERROR;
      }
    }
  | iaregpreinc offset
    {
      try {
        $$ = new TC_AsmAddress( $1, $2, TC_AsmArgument::Type::AMODE_PREINC );
      } catch ( invalid_argument &e ) {
        stringstream str;
        str << e.what() << " Incompatible with pre-increment addressing";
        error( @$, str.str() );
        YYERROR;
      }
    }
  | HI STRING
    {
      $$ =
        new TC_AsmAddress( nullptr, TC_AsmArgument::Type::AMODE_BASEHILAB, $2 );
      delete( $2 );
    }
  | iareg LO STRING
    {
      $$ = new TC_AsmAddress( $1, TC_AsmArgument::Type::AMODE_BASELOLAB, $3 );
      delete( $3 );
    }
  ;

constant
  : IMMEDIATE_S
    {
      $$ = new TC_AsmConstant( static_cast<signed long long>( $1 ) );
    }
  | IMMEDIATE_U
    {
      $$ = new TC_AsmConstant( static_cast<unsigned long long>( $1 ) );
    }
  ;

reg
  : AREG
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::AREG );
    }
  | AREG10
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::AREG_A10 );
    }
  | AREG15
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::AREG_A15 );
    }
  | DREG
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::DREG );
    }
  | DREG15
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::DREG_D15 );
    }
  | DREGL
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::DREG_L );
    }
  | DREGLL
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::DREG_LL );
    }
  | DREGLU
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::DREG_LU );
    }
  | DREGU
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::DREG_U );
    }
  | DREGUL
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::DREG_UL );
    }
  | DREGUU
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::DREG_UU );
    }
  | EREG
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::EREG );
    }
  | TPLARGL
    {
      $$ =
        dynamic_cast<TC_AsmRegister *>(
          driver.mContext->getTemplateArgument( $1 ) );
      $$->setType( TC_AsmArgument::Type::DREG_L );
      if ( $$ == nullptr )
        YYERROR;
    }
  | TPLARGLL
    {
      $$ =
        dynamic_cast<TC_AsmRegister *>(
          driver.mContext->getTemplateArgument( $1 ) );
      $$->setType( TC_AsmArgument::Type::DREG_LL );
      if ( $$ == nullptr )
        YYERROR;
    }
  | TPLARGLU
    {
      $$ =
        dynamic_cast<TC_AsmRegister *>(
          driver.mContext->getTemplateArgument( $1 ) );
      $$->setType( TC_AsmArgument::Type::DREG_LU );
      if ( $$ == nullptr )
        YYERROR;
    }
  | TPLARGU
    {
      $$ =
        dynamic_cast<TC_AsmRegister *>(
          driver.mContext->getTemplateArgument( $1 ) );
      $$->setType( TC_AsmArgument::Type::DREG_U );
      if ( $$ == nullptr )
        YYERROR;
    }
  | TPLARGUL
    {
      $$ =
        dynamic_cast<TC_AsmRegister *>(
          driver.mContext->getTemplateArgument( $1 ) );
      $$->setType( TC_AsmArgument::Type::DREG_UL );
      if ( $$ == nullptr )
        YYERROR;
    }
  | TPLARGUU
    {
      $$ =
        dynamic_cast<TC_AsmRegister *>(
          driver.mContext->getTemplateArgument( $1 ) );
      $$->setType( TC_AsmArgument::Type::DREG_UU );
      if ( $$ == nullptr )
        YYERROR;
    }
  ;

iareg
  : LBRACKET indreg RBRACKET
    {
      $$ = $2;
    }
  ;

iaregbrev
  : LBRACKET indreg BREV RBRACKET
    {
      $$ = $2;
    }
  ;

iaregcirc
  : LBRACKET indreg CIRC RBRACKET
    {
      $$ = $2;
    }
  ;

iaregpostinc
  : LBRACKET indreg PLUS RBRACKET
    {
      $$ = $2;
    }
  ;

iaregpreinc
  : LBRACKET PLUS indreg RBRACKET
    {
      $$ = $3;
    }
  ;

offset
  : IMMEDIATE_S
    {
      $$ = $1;
    }
  | IMMEDIATE_U
    {
      $$ = $1;
    }
  ;

indreg
  : AREG
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::AREG );
    }
  | AREG10
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::AREG_A10 );
    }
  | AREG15
    {
      $$ = new TC_AsmRegister( *$1, TC_AsmArgument::Type::AREG_A15 );
    }
  | TPLARG
    {
      $$ =
        dynamic_cast<TC_AsmRegister *>(
          driver.mContext->getTemplateArgument( $1 ) );
      if ( $$ == nullptr ) {
        stringstream str;
        str << "Template argument '%" << $1 << "' does not refer to a register";
        error( @$, str.str() );
        YYERROR;
      }
    }
  ;


%%


/*
  C++ code section
*/

namespace WIR {


using namespace std;


void TC_AsmYacc::error( const TC_AsmYacc::location_type &l, const string &m )
{
  DSTART(
    "virtual void TC_AsmYacc::error(const location_type&, const string&)" );

  driver.mContext->setError( l, m );
};

}       // namespace WIR
