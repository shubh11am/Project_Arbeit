/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2017 - 2022, Heiko Falk.

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
#include <sstream>

// Include ICD headers
#include <icd-c.h>
#include <llir3/llir3.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include misc headers
#include <misc/misc.h>

// Include local headers
#include <tc179x/auxfuncs.h>
#include <tc179x/cs_tc179x.h>
#include <tc179x/cast.h>
#include <tc179x/registrar.h>
#include <tc179x/stack.h>


using namespace std;


/*
  DEBUG_RULE_ACTION should be invoked right at the beginning of the action part
  of each rule. If debugging is enabled, it prints out which action part is
  entered, for which C expression. If debugging is disabled, DEBUG_RULE_ACTION
  does nothing.

  This function is named in uppercase letters against the usual convention,
  which would force us to use lowercase letters, because it mimics a debugmacro
  functionality and all the debugmacro stuff is written in uppercase letters.
*/
void DEBUG_RULE_ACTION( const std::string &ruleSignature,
                        IR_TreeElem *treeElem )
{
  (void) ruleSignature;
  (void) treeElem;

  #ifdef DEBUG_WCC
  stringstream ss;


  DSTART_EXT(
    const_cast<char *>( ruleSignature.c_str() ),
    "CodeSelector::RulesetActions" );

  if ( treeElem->getExp() ) {
    treeElem->getExp()->write( ss );

    // If debug output is written to cout, the expression is printed in bold.
    if ( strcmp( CODESEL_DBG_TARGET, "cout" ) == 0 ) {
      DOUT( "\33[1m[ " << ss.str() << " ]\33[0m" << endl );
    } else {
      DOUT( "[ " << ss.str() << " ]" << endl );
    }
  } else

  if ( treeElem->getStmt() ) {
    treeElem->getStmt()->write( ss );

    // If debug output is written to cout, the statment is printed in bold.
    if ( strcmp( CODESEL_DBG_TARGET, "cout" ) == 0 ) {
      DOUT( "\33[1m" << ss.str() << "\33[0m" << endl );
    } else {
      DOUT( ss.str() << endl );
    }
  }
  #endif
};


/*
  getConstIntValue returns the constant integer value associated with the
  IR_TreeElem passed as argument. It returns VALUE_INVALID on error.

  The function should only be used from within the cost computations. In the
  action parts, the appropriate action should be called to obtain the integer
  constant.
*/
long long getConstIntValue( IR_TreeElem *treeElem )
{
  DSTART( "getConstIntValue( NODEPTR )" );

  // Get the constant expression's value considering unary minus and cast
  // expressions, etc.
  ConstCastIntegralResult *const result =
    Cast::constantCastToIntegral( *treeElem->getExp() );
  const long long longlongresult =
    result ? tolonglong( result->value ) : VALUE_INVALID;
  delete result;

  return( longlongresult );
};


/*
  For a given pointer/array type, getBaseType returns the base type, else
  returns 0.
*/
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


/*
  For a given pointer/array exp, getBaseType returns the base type, else returns
  0.
*/
IR_Type *getBaseType( const IR_Exp &exp )
{
  return( getBaseType( exp.getType() ) );
};


/*
  getLowerLongLongWord extracts the lower word from a long long.
*/
Integer getLowerLongLongWord( const IR_Integer &ll )
{
  DSTART( "getLowerLongLongWord" );
  DOUT( "Getting lower word for " << ll << " ("
    << ( ll.getSign() == IR_Integer::SIGNED ? "signed" : "unsigned" )
    << ")" << endl );

  Integer result( 0 );

  if ( ll.getSign() == IR_Integer::SIGNED ) {
    Integer temp = ll.getRawValue( 64, IR_Integer::SIGNED );
    Integer mask( 0xFFFFFFFF );
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
Integer getUpperLongLongWord( const IR_Integer &ll )
{
  DSTART( "getUpperLongLongWord" );
  DOUT( "Getting upper word for " << ll << " ("
    << ( ll.getSign() == IR_Integer::SIGNED ? "signed" : "unsigned" )
    << ")" << endl );

  Integer result( 0 );

  if ( ll.getSign() == IR_Integer::SIGNED ) {

    const bool negativeSign = ll < 0;
    const Integer lowerword = getLowerLongLongWord( ll );

    Integer temp = ll.getRawValue( 64, IR_Integer::SIGNED );
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
