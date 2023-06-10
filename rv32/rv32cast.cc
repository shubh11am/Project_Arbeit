/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2007 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/



// this class requires further implementation to be operational beyond pointer casts
// to make this easier, all left out functionality is just commented out



//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include standard headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <functional>

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32imc.h>

// Include ICD headers
#include <icd-c.h>
#include <llir3/llir3.h>

// Include local headers
#include "rv32cast.h"
#include <rv32/rv32addressmodification.h>
#include <codesel/addresswithoffset.h>
#include <rv32/rv32lvalue.h>
#include <rv32/rv32instructionfactory.h>
#include <rv32/rv32registrar.h>
#include <rv32/rv32_incl.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include misc headers
#include <misc/misc.h>

#include "rv32_incl.h"
//#include "casting_incl.h"


//
// Code section
//

using namespace std;
using namespace WIR;

namespace RV32 {
#ifdef DEBUG_WCC
// Debug output function
string getTypeString( const IR_Type& t )
{
  stringstream out;
  t.write( out );
  char line[255];
  out.getline( line, 255 );
  return line;
}
#endif


int Cast::castingCost( NODEPTR dstTreeElem, NODEPTR srcTreeElem )
{
  return castingCost( dstTreeElem->getExp()->getType(),
                      srcTreeElem->getExp()->getType() );
}


/*
  castingCost determines the cost of a type conversion.
*/
int Cast::castingCost( const IR_Type &dstType, const IR_Type &srcType )
{
  int cost = 0;
  /*
  const bool dstIsERegType = typeIsStoredInEReg( dstType );
  const bool srcIsERegType = typeIsStoredInEReg( srcType );
  const bool dstIsBoolType = ( dstType.getType() == IR_Type::BOOL );
  const bool dstIsIntegralType =
    ( dstType.isIntegralType() && ! dstIsBoolType );
  const bool dstIsPointerType =
    ( ( dstType.getType() == IR_Type::POINTER ) ||
      ( dstType.getType() == IR_Type::ARRAY ) );
  const bool srcIsPointerType =
    ( ( srcType.getType() == IR_Type::POINTER ) ||
      ( srcType.getType() == IR_Type::ARRAY ) );

  // ## 1 ## Casting of integral types to integral types or Bool.
  if ( ( dstIsIntegralType || dstIsBoolType ) && srcType.isIntegralType() )
    cost = truncationCost( dstType, srcType );
  else

  // ## 2 ## Casting of real types to integral types.
  if ( dstIsIntegralType && srcType.isRealType() ) {

    if ( dstIsERegType ) {
      cost =
        TC13::OperationFormat::L.getSize() +
        ( srcType.getType() == IR_Type::FLOAT ? 3 : 4 ) *
          TC13::OperationFormat::SDD_1.getSize();
    } else {

      if ( srcType.getType() == IR_Type::FLOAT )
        cost = FLOAT_COST1( TC13::OperationFormat::DD );
      else
      if ( srcIsERegType )
        cost = SOFTFLOAT_COST;

      // Workaround to obtain an integer type object.
      IR_SourceSymbol intSymbol(
        "x",
        *new IR_Type(
          dstType.isSignedType() ? IR_Type::INT : IR_Type::UNSIGNED_INT ) );
      cost += truncationCost( dstType, intSymbol.getType() );
    }
  } else

  // ## 3 ## Casting of integral types to real types.
  if ( dstType.isRealType() && srcType.isIntegralType() ) {

    if ( srcIsERegType ) {
      if ( dstType.getType() == IR_Type::FLOAT )
        cost = SOFTFLOAT_COST;
      else
      if ( dstIsERegType ) {
        if ( srcType.isSignedType() )
          cost = SOFTFLOAT_COST + TC13::OperationFormat::SDD_1.getSize();
        else
          cost =
            TC13::OperationFormat::SDL.getSize() +
            8 * TC13::OperationFormat::SDD_1.getSize() +
            4 * TC13::OperationFormat::L.getSize() +
            TC13::OperationFormat::DDC9_2.getSize() +
            TC13::OperationFormat::SDC4_1.getSize() +
            TC13::OperationFormat::DDDC5.getSize() +
            TC13::OperationFormat::DDC9_1.getSize() +
            2 * TC13::OperationFormat::DDD_1.getSize();
      }
    } else {
      // Workaround to obtain an integer type object.
      IR_SourceSymbol intSymbol(
        "x",
        *new IR_Type(
          srcType.isSignedType() ? IR_Type::INT : IR_Type::UNSIGNED_INT ) );
      cost = truncationCost( intSymbol.getType(), srcType );

      if ( dstType.getType() == IR_Type::FLOAT )
        cost += FLOAT_COST1( TC13::OperationFormat::DD );
      else
      if ( dstIsERegType )
        cost += SOFTFLOAT_COST;
    }
  } else

  // ## 4 ## Casting of real types to real types.
  if ( dstType.isRealType() && srcType.isRealType() ) {

    if ( ( dstType.getType() != srcType.getType() ) &&
         dstIsERegType ^ srcIsERegType ) {
      if ( dstType.getType() == IR_Type::FLOAT ) {
        if ( srcIsERegType ) {
          cost = SOFTFLOAT_COST;
        } else
          cost = COST_INFINITY;
      } else
      if ( dstIsERegType ) {
        if ( srcType.getType() == IR_Type::FLOAT )
          cost = SOFTFLOAT_COST;
        else
          cost = COST_INFINITY;
      } else
        cost = COST_INFINITY;
    }
  } else

  // ## 5 ## Casting to void.
  if ( dstType.getType() == IR_Type::VOID )
    // Do nothing.
    cost = 0;
  else

  // ## 6 ## Casting to pointers.
  if ( dstIsPointerType && srcType.isIntegralType() ) {

    cost = TC13::OperationFormat::SAD_1.getSize();
    if ( srcIsERegType ) {
      // Workaround to obtain an integer type object.
      IR_SourceSymbol intSymbol( "x", *new IR_Type( IR_Type::UNSIGNED_INT ) );
      cost += truncationCost( intSymbol.getType(), srcType );
    }
  } else

  // ## 7 ## Casting from pointers.
  if ( srcIsPointerType && dstIsIntegralType ) {

    cost = TC13::OperationFormat::SDA_1.getSize();

    if ( dstIsERegType ) {
      // Workaround to obtain an integer type object.
      IR_SourceSymbol intSymbol( "x", *new IR_Type( IR_Type::UNSIGNED_INT ) );
      cost += truncationCost( dstType, intSymbol.getType() );
    }
  } else

  // ## 8 ## Casting between pointers.
  if ( srcIsPointerType && dstIsPointerType )
    // Do nothing.
    cost = 0;
  else

  // ## 9 ## Casting real types to Bool.
  if ( dstIsBoolType && srcType.isRealType() ) {

    cost =
      TC13::OperationFormat::DDC5C5.getSize() +
      TC13::OperationFormat::DDC9_1.getSize();
    if ( srcIsERegType )
      cost += TC13::OperationFormat::DDD_1.getSize();
  } else

  // ## 10 ## Casting pointers to Bool.
  if ( dstIsBoolType && srcIsPointerType )
    cost = TC13::OperationFormat::DA.getSize();
  else
    cost = COST_INFINITY;

  return( cost );*/
};


/*
  doCasting performs a type conversion between two compatible types.
*/
WIR::WIR_VirtualRegister &Cast::doCasting( const IR_Type &dstType,
                                           const IR_Type &srcType,
                                           const WIR::WIR_VirtualRegister &r,
                                           const IR_Exp *exp )
{
  DSTART(
    "static WIR_VirtualRegister& Cast::doCasting(const IR_Type&, const IR_Type&, const WIR_VirtualRegister&, const IR_Exp*)" );

  DACTION(
    cout << "dstType = ";
    dstType.write( cout );
    cout << endl << "srcType = ";
    srcType.write( cout );
    cout << endl << "r = " << r.getName() << endl; );

  //const bool dstIsERegType = typeIsStoredInEReg( dstType );
  //const bool srcIsERegType = typeIsStoredInEReg( srcType );
  const bool dstIsBoolType = ( dstType.getType() == IR_Type::BOOL );
  const bool dstIsIntegralType =
    ( dstType.isIntegralType() && ! dstIsBoolType );
  const bool dstIsPointerType =
    ( ( dstType.getType() == IR_Type::POINTER ) ||
      ( dstType.getType() == IR_Type::ARRAY ) );
  const bool srcIsPointerType =
    ( ( srcType.getType() == IR_Type::POINTER ) ||
      ( srcType.getType() == IR_Type::ARRAY ) );

  // ## 1 ## Casting of integral types to integral types or Bool.
  if ( ( dstIsIntegralType || dstIsBoolType ) && srcType.isIntegralType() ) {
    DOUT( "Case ## 1 ##" << endl );
    return( doTruncation( dstType, srcType, r, exp ) );
  } else

  // ## 2 ## Casting of real types to integral types.
  if ( dstIsIntegralType && srcType.isRealType() ) {
    DOUT( "Case ## 2 ##" << endl );
    ufAssertT( false, "Case 2 not yet implemented");
    /*
    auto &d = static_cast<WIR_VirtualRegister &>( RVINSTRUCTIONS.createReg() );

    if ( dstIsERegType ) {

      if ( srcType.getType() == IR_Type::FLOAT ) {
        if ( dstType.isSignedType() )
          TCINSTRUCTIONS.insertFTOLL(
            dynamic_cast<TC_ERegV &>( d ),
            dynamic_cast<const TC_DRegV &>( r ), exp );
        else
          TCINSTRUCTIONS.insertFTOULL(
            dynamic_cast<TC_ERegV &>( d ),
            dynamic_cast<const TC_DRegV &>( r ), exp );
      } else
      if ( srcIsERegType ) {
        if ( dstType.isSignedType() )
          TCINSTRUCTIONS.insertDTOLL(
            dynamic_cast<TC_ERegV &>( d ),
            dynamic_cast<const TC_ERegV &>( r ), exp );
        else
          TCINSTRUCTIONS.insertDTOULL(
            dynamic_cast<TC_ERegV &>( d ),
            dynamic_cast<const TC_ERegV &>( r ), exp );
      } else
        ufAssertT( 0, "Uncovered case!" );
      return( d );

    } else {

      if ( srcType.getType() == IR_Type::FLOAT ) {
        if ( dstType.isSignedType() )
          TCINSTRUCTIONS.insertFTOI(
            dynamic_cast<TC_DRegV &>( d ),
            dynamic_cast<const TC_DRegV &>( r ), exp );
        else
          TCINSTRUCTIONS.insertFTOU(
            dynamic_cast<TC_DRegV &>( d ),
            dynamic_cast<const TC_DRegV &>( r ), exp );
      } else
      if ( srcIsERegType ) {
        if ( dstType.isSignedType() )
          TCINSTRUCTIONS.insertDTOI(
            dynamic_cast<TC_DRegV &>( d ),
            dynamic_cast<const TC_ERegV &>( r ), exp );
        else
          TCINSTRUCTIONS.insertDTOU(
            dynamic_cast<TC_DRegV &>( d ),
            dynamic_cast<const TC_ERegV &>( r ), exp );
      } else
        ufAssertT( 0, "Uncovered case!" );

      // Workaround to obtain an integer type object.
      IR_SourceSymbol intSymbol(
        "x",
        *new IR_Type(
          dstType.isSignedType() ? IR_Type::INT : IR_Type::UNSIGNED_INT ) );
      return( doTruncation( dstType, intSymbol.getType(), d, exp ) );
    }*/
  } else

  // ## 3 ## Casting of integral types to real types.
  if ( dstType.isRealType() && srcType.isIntegralType() ) {
    DOUT( "Case ## 3 ##" << endl );
    ufAssertT( false, "Case 3 not yet implemented");
    /*
    auto &d =
      ( dstIsERegType ?
        static_cast<WIR_VirtualRegister &>( TCINSTRUCTIONS.createEReg() ) :
        static_cast<WIR_VirtualRegister &>( TCINSTRUCTIONS.createDReg() ) );

    if ( srcIsERegType ) {

      if ( dstType.getType() == IR_Type::FLOAT ) {
        if ( srcType.isSignedType() )
          TCINSTRUCTIONS.insertLLTOF(
            dynamic_cast<TC_DRegV &>( d ),
            dynamic_cast<const TC_ERegV &>( r ), exp );
        else
          TCINSTRUCTIONS.insertULLTOF(
            dynamic_cast<TC_DRegV &>( d ),
            dynamic_cast<const TC_ERegV &>( r ), exp );
      } else
      if ( dstIsERegType ) {
        if ( srcType.isSignedType() )
          TCINSTRUCTIONS.insertLLTOD(
            dynamic_cast<TC_ERegV &>( d ),
            dynamic_cast<const TC_ERegV &>( r ), exp );
        else
          TCINSTRUCTIONS.insertULLTOD(
            dynamic_cast<TC_ERegV &>( d ),
            dynamic_cast<const TC_ERegV &>( r ), exp );
      } else
        ufAssertT( 0, "Uncovered case!" );
      return( d );

    } else {

      // Workaround to obtain an integer type object.
      IR_SourceSymbol intSymbol(
        "x",
        *new IR_Type(
          srcType.isSignedType() ? IR_Type::INT : IR_Type::UNSIGNED_INT ) );
      auto &truncReg = doTruncation( intSymbol.getType(), srcType, r, exp );

      if ( dstType.getType() == IR_Type::FLOAT ) {
        if ( srcType.isSignedType() )
          TCINSTRUCTIONS.insertITOF(
            dynamic_cast<TC_DRegV &>( d ),
            dynamic_cast<TC_DRegV &>( truncReg ), exp );
        else
          TCINSTRUCTIONS.insertUTOF(
            dynamic_cast<TC_DRegV &>( d ),
            dynamic_cast<TC_DRegV &>( truncReg ), exp );
      } else
      if ( dstIsERegType ) {
        if ( srcType.isSignedType() )
          TCINSTRUCTIONS.insertITOD(
            dynamic_cast<TC_ERegV &>( d ),
            dynamic_cast<const TC_DRegV &>( truncReg ), exp );
        else
          TCINSTRUCTIONS.insertUTOD(
            dynamic_cast<TC_ERegV &>( d ),
            dynamic_cast<const TC_DRegV &>( truncReg ), exp );
      } else
        ufAssertT( 0, "Uncovered case!" );
      return( d );

    }*/
  } else

  // ## 4 ## Casting of real types to real types.
  if ( dstType.isRealType() && srcType.isRealType() ) {
    DOUT( "Case ## 4 ##" << endl );

    ufAssertT( false, "Case 4 not yet implemented");
    /*
    if ( ( dstType.getType() != srcType.getType() ) &&
         dstIsERegType ^ srcIsERegType ) {
      if ( dstType.getType() == IR_Type::FLOAT ) {
        if ( srcIsERegType ) {
          auto &d =
            ( dstIsERegType ?
              static_cast<WIR_VirtualRegister &>( TCINSTRUCTIONS.createEReg() ) :
              static_cast<WIR_VirtualRegister &>( TCINSTRUCTIONS.createDReg() ) );
          TCINSTRUCTIONS.insertDTOF(
            dynamic_cast<TC_DRegV &>( d ),
            dynamic_cast<const TC_ERegV &>( r ), exp );
          return( d );
        } else
          ufAssertT( 0, "Unknown conversion." );

      } else
      if ( dstIsERegType ) {
        if ( srcType.getType() == IR_Type::FLOAT ) {
          auto &d = TCINSTRUCTIONS.createEReg();
          TCINSTRUCTIONS.insertFTOD(
            dynamic_cast<TC_ERegV &>( d ),
            dynamic_cast<const TC_DRegV &>( r ), exp );
          return( d );
        } else
          ufAssertT( 0, "Unknown conversion." );
      } else
        ufAssertT( 0, "Other real numbers not supported." );

    } else
      return( const_cast<WIR_VirtualRegister &>( r ) );*/
  } else

  // ## 5 ## Casting to void.
  if ( dstType.getType() == IR_Type::VOID ) {
    DOUT( "Case ## 5 ##" << endl );
    // Simply return an empty register.
    auto &d = static_cast<WIR_VirtualRegister &>( RVINSTRUCTIONS.createReg() );
    return( d );
  } else

  // ## 6 ## Casting to pointers.
  if ( dstIsPointerType && srcType.isIntegralType() ) {
    DOUT( "Case ## 6 ##" << endl );
    auto &a = RVINSTRUCTIONS.createReg();

    /* ERegs Not yet implemented
    if ( srcIsERegType ) {
      ufAssertT( false, "Case 6 not yet implemented");
      // Workaround to obtain an integer type object.
      IR_SourceSymbol intSymbol( "x", *new IR_Type( IR_Type::UNSIGNED_INT ) );
      auto &truncReg = doTruncation( intSymbol.getType(), srcType, r, exp );
      TCINSTRUCTIONS.insertMOV_A(
        a, dynamic_cast<TC_DRegV &>( truncReg ), exp );
    } else*/
      RVINSTRUCTIONS.insertMOV(
        a, dynamic_cast<const RV_RegV &>( r ), exp );

    return( a );
  } else

  // ## 7 ## Casting from pointers.
  if ( srcIsPointerType && dstIsIntegralType ) {
    DOUT( "Case ## 7 ##" << endl );
    auto &d = RVINSTRUCTIONS.createReg();
    RVINSTRUCTIONS.insertMOV(
      d, dynamic_cast<const RV_RegV &>( r ), exp );

    /* ERegs Not yet implemented
    if ( dstIsERegType ) {
      // Workaround to obtain an integer type object.
      IR_SourceSymbol intSymbol( "x", *new IR_Type( IR_Type::UNSIGNED_INT ) );
      return( doTruncation( dstType, intSymbol.getType(), d ) );
    }*/

    return( d );
  } else

  // ## 8 ## Casting between pointers.
  if ( srcIsPointerType && dstIsPointerType ) {
    DOUT( "Case ## 8 ##" << endl );
    // Do nothing.
    return( const_cast<WIR_VirtualRegister &>( r ) );
  } else

  // ## 9 ## Casting real types to Bool.
  if ( dstIsBoolType && srcType.isRealType() ) {
    DOUT( "Case ## 9 ##" << endl );
    ufAssertT( false, "Case 9 not yet implemented");
    /*
    auto &d = TCINSTRUCTIONS.createDReg();

    if ( srcIsERegType ) {
      TCINSTRUCTIONS.insertEXTR_U(
        d, dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), 0, 31, exp );
      TCINSTRUCTIONS.insertOR(
        d, d, dynamic_cast<TC_DRegV &>( r.begin()->get() ), exp );
    } else
      TCINSTRUCTIONS.insertEXTR_U(
        d, dynamic_cast<const TC_DRegV &>( r ), 0, 31, exp );
    TCINSTRUCTIONS.insertNE( d, d, 0, exp );

    return( d );*/
  } else

  // ## 10 ## Casting pointers to Bool.
  if ( dstIsBoolType && srcIsPointerType ) {
    DOUT( "Case ## 10 ##" << endl );
    
    /*auto &d = RVINSTRUCTIONS.createReg();
    TCINSTRUCTIONS.insertNEZ_A( d, dynamic_cast<const TC_ARegV &>( r ), exp );
    return( d );*/
  } else
    ufAssertT( 0, "Unknown Type!" );
  ufAssertT( false, "Case 10 not yet implemented");
  return( const_cast<WIR_VirtualRegister &>( r ) );
};


// // (DEPRECATED)
// int Cast::castingCost( NODEPTR srcTreeElem )
// {
//   // Workaround for getting an integer type object
//   IR_SourceSymbol intSymbol( "", *new IR_Type( IR_Type::INT ) );
//   return castingCost( intSymbol.getType(), srcTreeElem->getExp()->getType() );
// }


// int Cast::truncationCost( NODEPTR targetTypeElem, NODEPTR srcTypeElem )
// {
//   return truncationCost(
//     targetTypeElem->getExp()->getType(),
//     srcTypeElem->getExp()->getType() );
// }


// /*
//   truncationCost determines the cost of a truncation operation.
// */
// int Cast::truncationCost( const IR_Type &dstType, const IR_Type &srcType )
// {
//   DSTART( "static int Cast::truncationCost(const IR_Type&, const IR_Type&)" );

//   const bool dstIsLongLong =
//     ( dstType.getType() == IR_Type::LONG_LONG ) ||
//     ( dstType.getType() == IR_Type::UNSIGNED_LONG_LONG );
//   const bool srcIsLongLong =
//     ( srcType.getType() == IR_Type::LONG_LONG ) ||
//     ( srcType.getType() == IR_Type::UNSIGNED_LONG_LONG );

//   if ( srcIsLongLong && dstIsLongLong )
//     // Nothing has to be done for long long source and destination types. Even
//     // for signed/unsigned conversions, simply take over the value.
//     return( 0 );

//   switch ( dstType.getType() ) {
//     case IR_Type::CHAR:
//     case IR_Type::UNSIGNED_CHAR:
//     case IR_Type::SHORT:
//     case IR_Type::UNSIGNED_SHORT: {
//       return( TC13::OperationFormat::DDC5C5.getSize() );
//       break;
//     }

//     case IR_Type::BOOL: {
//       return( TC13::OperationFormat::DDC9_1.getSize() );
//       break;
//     }

//     default: {
//       if ( dstIsLongLong == srcIsLongLong )
//         // No conversion required.
//         return( 0 );
//       else {
//         int cost = 0;

//         if ( dstIsLongLong ) {
//           cost += TC13::OperationFormat::SDD_1.getSize();
//           if ( srcType.isSignedType() )
//             cost += TC13::OperationFormat::DDC5C5.getSize();
//           else
//             cost += TC13::OperationFormat::SDC4_1.getSize();
//         } else
//           cost += TC13::OperationFormat::SDD_1.getSize();
//         return( cost );
//       }

//       break;
//     }
//   }
// };




/*
  doTruncation performs the truncation of a virtual register's contents to the
  bitwidth of the supposed ICD-C data type.
*/
WIR::WIR_VirtualRegister &Cast::doTruncation( const IR_Type &dstType,
                                              const IR_Type &srcType,
                                              const WIR::WIR_VirtualRegister &r,
                                              const IR_Exp *exp )
{
  DSTART(
    "static WIR_VirtualRegister& Cast::doTruncation(const IR_Type&, const IR_Type&, const WIR_VirtualRegister&, const IR_Exp*)" );

  #ifdef DEBUG_WCC
  ufAssertT(
    dstType.isIntegralType() && srcType.isIntegralType(),
    "'doTruncation' only converts integral types." );
  #endif

  const IR_Configuration *conf = RVIR_CONFIGURATION;

  //const bool dstIsERegType = typeIsStoredInEReg( dstType );

  const bool dstIsLongLong =
    ( dstType.getType() == IR_Type::LONG_LONG ) ||
    ( dstType.getType() == IR_Type::UNSIGNED_LONG_LONG );
  const bool srcIsLongLong =
    ( srcType.getType() == IR_Type::LONG_LONG ) ||
    ( srcType.getType() == IR_Type::UNSIGNED_LONG_LONG );

  #ifdef DEBUG_WCC
  //const bool srcIsERegType = typeIsStoredInEReg( srcType );
  //ufAssertT( dstIsERegType == dstIsLongLong, "Invalid state." );
  //ufAssertT( srcIsERegType == srcIsLongLong, "Invalid state." );
  #endif

  if ( srcIsLongLong && dstIsLongLong )
    // Do nothing for long long source and destination types. Even for
    // signed/unsigned conversions, simply take over the value.
    return( const_cast<WIR_VirtualRegister &>( r ) );

  // If the cast reads a long long, the value is implicitly truncated by only
  // returning the lower register.
  auto &srcReg =
    dynamic_cast<const RV_RegV &>( srcIsLongLong ? r.begin()->get() : r );

  // Truncate/expand (further) to reach given bitwidth.
  switch ( dstType.getType() ) {
    case IR_Type::CHAR: {
      auto &d = RVINSTRUCTIONS.createReg();
      //TCINSTRUCTIONS.insertEXTR( d, srcReg, 0, conf->bitwidthChar, exp );
      RVINSTRUCTIONS.insertMOV( d, srcReg, exp );
      int neededShifts = 32 - conf->bitwidthChar;
      RVINSTRUCTIONS.insertSLLI( d, d, neededShifts, exp );
      RVINSTRUCTIONS.insertSRAI( d, d, neededShifts, exp );
      return( d );
      break;
    }

    case IR_Type::UNSIGNED_CHAR: {
      auto &d = RVINSTRUCTIONS.createReg();
      //TCINSTRUCTIONS.insertEXTR_U( d, srcReg, 0, conf->bitwidthChar, exp );
      RVINSTRUCTIONS.insertMOV( d, srcReg, exp ); 
      int neededShifts = 32 - conf->bitwidthChar; 
      RVINSTRUCTIONS.insertSLLI( d, d, neededShifts, exp ); 
      RVINSTRUCTIONS.insertSRLI( d, d, neededShifts, exp ); 
      return( d );
      break;
    }

    case IR_Type::SHORT: {
      auto &d = RVINSTRUCTIONS.createReg();
      //TCINSTRUCTIONS.insertEXTR( d, srcReg, 0, conf->bitwidthShort, exp );
      RVINSTRUCTIONS.insertMOV( d, srcReg, exp );
      int neededShifts = 32 - conf->bitwidthShort;
      RVINSTRUCTIONS.insertSLLI( d, d, neededShifts, exp );
      RVINSTRUCTIONS.insertSRAI( d, d, neededShifts, exp );
      return( d );
      break;
    }

    case IR_Type::UNSIGNED_SHORT: {
      auto &d = RVINSTRUCTIONS.createReg();
      //TCINSTRUCTIONS.insertEXTR_U( d, srcReg, 0, conf->bitwidthShort, exp );
      RVINSTRUCTIONS.insertMOV( d, srcReg, exp ); 
      int neededShifts = 32 - conf->bitwidthShort; 
      RVINSTRUCTIONS.insertSLLI( d, d, neededShifts, exp ); 
      RVINSTRUCTIONS.insertSRLI( d, d, neededShifts, exp ); 
      return( d );
      break;
    }

    case IR_Type::BOOL: {
      //auto &d = TCINSTRUCTIONS.createDReg();
      //TCINSTRUCTIONS.insertNE( d, srcReg, 0, exp );
      //return( d );
      ufAssertT( false, "Bool type not yet implemented");
      break;
    }

    default: {
      if ( dstIsLongLong == srcIsLongLong )
        // No conversion required.
        return( const_cast<WIR_VirtualRegister &>( r ) );
      else {
        /*auto &d =
          ( dstIsERegType ?
            static_cast<WIR_VirtualRegister &>( TCINSTRUCTIONS.createEReg() ) :
            static_cast<WIR_VirtualRegister &>( TCINSTRUCTIONS.createDReg() ) );*/
        auto &d = static_cast<WIR_VirtualRegister &>( RVINSTRUCTIONS.createReg() );

        // Does the cast write to a long long and read from a non long-long?
        if ( dstIsLongLong ) {
          /*// Carry over the lower word to the new ereg.
          RVINSTRUCTIONS.insertMOV(
            dynamic_cast<RV_RegV &>( d.begin()->get() ), srcReg, exp );

          // Fill the upper word with sign-extension or zero.
          if ( srcType.isSignedType() )
            TCINSTRUCTIONS.insertEXTR(
              dynamic_cast<TC_DRegV &>( d.rbegin()->get() ), srcReg, 31, 1,
              exp );
          else
            TCINSTRUCTIONS.insertMOV(
              dynamic_cast<TC_DRegV &>( d.rbegin()->get() ), 0, exp );*/
          ufAssertT( false, "dstLongLong type not yet implemented");
        } else
          // !dstIsLongLong && srcIsLongLong
          // Carry over a long long lower word into a normal register.
          RVINSTRUCTIONS.insertMOV( dynamic_cast<RV_RegV &>( d ), srcReg, exp );

        return( d );
      }
      break;
    }
  }
};


// /*
//   constantCastToIntegral returns the result of the casted constant expression to
//   an integral type.

//   The caller must delete the result.
// */
// ConstCastIntegralResult *Cast::constantCastToIntegral( const IR_Exp &exp )
// {
//   DSTART(
//     "static ConstCastIntegralResult* Cast::constantCastToIntegral(const IR_Exp&)" );

//   if ( effectiveType( exp ).isIntegralType() ) {
//     ConstCastResult *tempRes = constantCast( exp );

//     if ( tempRes ) {
//       auto *res =
//         new ConstCastIntegralResult
//           { tempRes->value.integral, *tempRes->type };
//       delete( tempRes );
//       return( res );
//     }
//   }

//   return( nullptr );
// };


// /*
//   constantCastToFloat returns the result of the casted constant expression to a
//   float type.

//   The caller must delete the result.
// */
// Float *Cast::constantCastToFloat( const IR_Exp &exp )
// {
//   DSTART( "static Float* Cast::constantCastToFloat(const IR_Exp&)" );

//   if ( isFloatType( effectiveType( exp ) ) ) {
//     ConstCastResult *tempRes = constantCast( exp );

//     if ( tempRes ) {
//       auto *res = new Float( tempRes->value.flt );
//       delete( tempRes );
//       return( res );
//     }
//   }

//   return( nullptr );
// };


// /*
//   constantCastToDouble returns the result of the casted constant expression to a
//   double type.

//   The caller must delete the result.
// */
// Double *Cast::constantCastToDouble( const IR_Exp &exp )
// {
//   DSTART( "static Double* Cast::constantCastToDouble(const IR_Exp&)" );

//   if ( isDoubleType( effectiveType( exp ) ) ) {
//     ConstCastResult *tempRes = constantCast( exp );

//     if ( tempRes ) {
//       auto *res = new Double( tempRes->value.dbl );
//       delete( tempRes );
//       return( res );
//     }
//   }

//   return( nullptr );
// };


// Cast::ConstCastResult *Cast::constantCast( const IR_Exp &exp )
// {
//   DSTART( "static Cast::ConstCastResult* Cast::constantCast(const IR_Exp&)" );

//   ConstCastResult *res = nullptr;

//   auto *icexp = dynamic_cast<const IR_IntConstExp *>( &exp );
//   auto *fcexp = dynamic_cast<const IR_FloatConstExp *>( &exp );
//   auto *uexp = dynamic_cast<const IR_UnaryExp *>( &exp );
//   auto *soexp = dynamic_cast<const IR_SizeOfExp *>( &exp );
//   auto *symexp = dynamic_cast<const IR_SymbolExp *>( &exp );

//   if ( icexp ) {
//     res = new ConstCastResult;
//     res->type = &icexp->getType();
//     res->value.integral.setBitwidth(
//       res->type->bitSize(), res->type->isSignedType() );
//     res->value.integral = icexp->getValue();
//   } else

//   if ( fcexp ) {
//     res = new ConstCastResult;
//     res->type = &fcexp->getType();

//     switch ( res->type->getType() ) {
//       case IR_Type::FLOAT: {
//         res->value.flt = fcexp->getValue().getSingleValue();
//         break;
//       }
//       case IR_Type::DOUBLE: {
//         res->value.dbl = fcexp->getValue().getDoubleValue();
//         break;
//       }
//       case IR_Type::LONG_DOUBLE: {
//         res->value.dbl = fcexp->getValue().getLongDoubleValue();
//         break;
//       }
//       default: {
//         ufAssertT( 0, "Invalid type!" );
//         break;
//       }
//     }
//   } else

//   if ( symexp && symexp->getSymbol().getEnumType() ) {
//     res = new ConstCastResult;
//     res->type = &symexp->getType();

//     IR_Symbol &sym = symexp->getSymbol();
//     res->value.integral.setBitwidth(
//       res->type->bitSize(), res->type->isSignedType() );
//     res->value.integral = sym.getEnumType()->getValue( &sym );
//   } else

//   if ( soexp ) {
//     res = new ConstCastResult;
//     res->type = &soexp->getType();
//     res->value.integral.setBitwidth(
//       res->type->bitSize(), res->type->isSignedType() );
//     res->value.integral = computeSizeOf( &soexp->getBaseType() );
//   } else

//   if ( uexp && ( uexp->getOperator() == IR_UnaryExp::SIZEOF ) ) {
//     res = new ConstCastResult;
//     res->type = &uexp->getType();
//     res->value.integral.setBitwidth(
//       res->type->bitSize(), res->type->isSignedType() );
//     res->value.integral = computeSizeOf( &uexp->getOp().getType() );
//   } else

//   if ( uexp && ( uexp->getOperator() == IR_UnaryExp::PLUS ) ) {
//     res = constantCast( uexp->getOp() );
//     if ( !res )
//       return( nullptr );

//     res->type = &uexp->getType();
//   } else

//   if ( uexp && ( uexp->getOperator() == IR_UnaryExp::MINUS ) ) {
//     res = constantCast( uexp->getOp() );
//     if ( !res )
//       return( nullptr );

//     res->type = &uexp->getType();

//     switch ( res->type->getType() ) {
//       case IR_Type::FLOAT: {
//         res->value.flt = -res->value.flt;
//         break;
//       }
//       case IR_Type::DOUBLE:
//       case IR_Type::LONG_DOUBLE: {
//         res->value.dbl = -res->value.dbl;
//         break;
//       }
//       default: {
//         res->value.integral.setBitwidth(
//           res->type->bitSize(), res->type->isSignedType() );
//         res->value.integral = -res->value.integral;
//         break;
//       }
//     }
//   } else

//   if ( uexp && ( uexp->getOperator() == IR_UnaryExp::CAST ) ) {
//     res = constantCast( uexp->getOp() );
//     if ( !res )
//       return( nullptr );

//     res->type = &uexp->getType();

//     IR_Type::Type sourceType = uexp->getOp().getType().getType();
//     switch ( res->type->getType() ) {
//       case IR_Type::FLOAT: {
//         switch ( sourceType ) {
//           case IR_Type::FLOAT: {
//             res->value.flt = constantCastToFloat( res->value.flt );
//             break;
//           }
//           case IR_Type::DOUBLE:
//           case IR_Type::LONG_DOUBLE: {
//             res->value.flt = constantCastToFloat( res->value.dbl );
//             break;
//           }
//           default: {
//             // TODO: The floating point cast results do not always match the
//             //       TriCore representation. Therefore, casting of integrals to
//             //       float is deactivated at the moment.
//             delete( res );
//             return( nullptr );
//             //result->value.flt = constantCastToFloat( result->value.integral );
//             //break;
//           }
//         }
//         break;
//       }

//       case IR_Type::DOUBLE:
//       case IR_Type::LONG_DOUBLE: {
//         switch ( sourceType ) {
//           case IR_Type::FLOAT: {
//             res->value.dbl = constantCastToDouble( res->value.flt );
//             break;
//           }
//           case IR_Type::DOUBLE:
//           case IR_Type::LONG_DOUBLE: {
//             res->value.dbl = constantCastToDouble( res->value.dbl );
//             break;
//           }
//           default: {
//             // TODO: The floating point cast results do not always match the
//             //       TriCore representation. Therefore, casting of integrals to
//             //       float is deactivated at the moment.
//             delete( res );
//             return( nullptr );
//             //result->value.dbl = constantCastToDouble( result->value.integral );
//             //break;
//           }
//         }
//         break;
//       }

//       default: {
//         res->value.integral.setBitwidth(
//           res->type->bitSize(), res->type->isSignedType() );

//         switch ( sourceType ) {
//           case IR_Type::FLOAT: {
//             res->value.integral =
//               constantCastToIntegral( res->value.flt, *res->type );
//             break;
//           }
//           case IR_Type::DOUBLE:
//           case IR_Type::LONG_DOUBLE: {
//             res->value.integral =
//               constantCastToIntegral( res->value.dbl, *res->type );
//             break;
//           }
//           default: {
//             res->value.integral =
//               constantCastToIntegral( res->value.integral, *res->type );
//             break;
//           }
//         }
//         break;
//       }
//     }
//   } else
//     return( nullptr );

//   // Handle the possible implicit cast.
//   IR_Type *implicitCastType = exp.getImplicitCastType();
//   if ( implicitCastType ) {
//     res->type = implicitCastType;

//     IR_Type::Type sourceType = exp.getType().getType();

//     switch ( res->type->getType() ) {
//       case IR_Type::FLOAT: {
//         switch ( sourceType ) {
//           case IR_Type::FLOAT: {
//             res->value.flt = constantCastToFloat( res->value.flt );
//             break;
//           }
//           case IR_Type::DOUBLE:
//           case IR_Type::LONG_DOUBLE: {
//             res->value.flt = constantCastToFloat( res->value.dbl );
//             break;
//           }
//           default: {
//             // TODO: The floating point cast results do not always match the
//             //       TriCore representation. Therefore, casting of integrals to
//             //       float is deactivated at the moment.
//             delete( res );
//             return( nullptr );
//             //result->value.flt = constantCastToFloat( result->value.integral );
//             //break;
//           }
//         }
//         break;
//       }

//       case IR_Type::DOUBLE:
//       case IR_Type::LONG_DOUBLE: {
//         switch ( sourceType ) {
//           case IR_Type::FLOAT: {
//             res->value.dbl = constantCastToDouble( res->value.flt );
//             break;
//           }
//           case IR_Type::DOUBLE:
//           case IR_Type::LONG_DOUBLE: {
//             res->value.dbl = constantCastToDouble( res->value.dbl );
//             break;
//           }
//           default: {
//             // TODO: The floating point cast results do not always match the
//             //       TriCore representation. Therefore, casting of integrals to
//             //       float is deactivated at the moment.
//             delete( res );
//             return( nullptr );
//             //result->value.dbl = constantCastToDouble( result->value.integral );
//             //break;
//           }
//         }
//         break;
//       }

//       default: {
//         res->value.integral.setBitwidth(
//           res->type->bitSize(), res->type->isSignedType() );

//         switch ( sourceType ) {
//           case IR_Type::FLOAT: {
//             res->value.integral =
//               constantCastToIntegral( res->value.flt, *res->type );
//             break;
//           }
//           case IR_Type::DOUBLE:
//           case IR_Type::LONG_DOUBLE: {
//             res->value.integral =
//               constantCastToIntegral( res->value.dbl, *res->type );
//             break;
//           }
//           default: {
//             res->value.integral =
//               constantCastToIntegral( res->value.integral, *res->type );
//             break;
//           }
//         }
//         break;
//       }
//     }
//   }

//   return( res );
// };


// Float Cast::constantCastToFloat( const Float &f )
// {
//   DSTART( "static Float Cast::constantCastToFloat(const Float&)" );

//   return( f );
// };


// Float Cast::constantCastToFloat( const Double &d )
// {
//   DSTART( "static Float Cast::constantCastToFloat(const Double&)" );

//   return( Float( d ) );
// };


// Float Cast::constantCastToFloat( const IR_Integer &i )
// {
// cout << BOOST_CURRENT_FUNCTION << endl;
//   DSTART( BOOST_CURRENT_FUNCTION );

//   return( Float( i.getRawValue() ) );
// };


// Double Cast::constantCastToDouble( const Float &f )
// {
//   DSTART( "static Double Cast::constantCastToDouble(const Float&)" );

//   return( Double( f ) );
// };


// Double Cast::constantCastToDouble( const Double &d )
// {
//   DSTART( "static Double Cast::constantCastToDouble(const Double&)" );

//   return( d );
// };


// Double Cast::constantCastToDouble( const IR_Integer &i )
// {
// cout << BOOST_CURRENT_FUNCTION << endl;
//   DSTART( BOOST_CURRENT_FUNCTION );

//   return( Double( i.getRawValue() ) );
// };


// IR_Integer Cast::constantCastToIntegral( const Float &f,
//                                          const IR_Type &targetType )
// {
//   DSTART(
//     "static IR_Integer Cast::constantCastToIntegral(const Float&, const IR_Type&)" );

//   ufAssertT( targetType.isScalarType(), "Invalid argument!" );

//   IR_Integer res( f, targetType.bitSize(), targetType.isSignedType() );
//   if ( targetType.getType() == IR_Type::BOOL )
//     res = ( f == 0 ? 0 : 1 );
//   return( res );
// };


// IR_Integer Cast::constantCastToIntegral( const Double &d,
//                                          const IR_Type &targetType )
// {
//   DSTART(
//     "static IR_Integer Cast::constantCastToIntegral(const Double&, const IR_Type&)" );

//   ufAssertT( targetType.isScalarType(), "Invalid argument!" );

//   IR_Integer res( d, targetType.bitSize(), targetType.isSignedType() );
//   if ( targetType.getType() == IR_Type::BOOL )
//     res = ( d == 0 ? 0 : 1 );
//   return( res );
// };


// IR_Integer Cast::constantCastToIntegral( const IR_Integer &i,
//                                          const IR_Type &targetType )
// {
//   DSTART(
//     "static IR_Integer Cast::constantCastToIntegral(const IR_Integer&, const IR_Type&)" );

//   ufAssertT( targetType.isScalarType(), "Invalid argument!" );

//   IR_Integer res( 0, targetType.bitSize(), targetType.isSignedType() );
//   if ( targetType.getType() == IR_Type::BOOL )
//     res = ( i == 0 ? 0 : 1 );
//   else
//     res = i;
//   return( res );
// };


// LLIR_Register *Cast::getDestinationRegister( LLIR_Register *sourceReg,
//                                              bool srcIsERegType,
//                                              bool dstIsERegType )
// {
//   DSTART( "Cast::getDestinationRegister" );

//   DOUT(
//     "Getting LLIR registers for source reg " << sourceReg->GetName() <<
//     " (Source is EReg: " << srcIsERegType << ", Dst Is EReg: " <<
//     dstIsERegType << endl );

//   // Avoid compiler warnings.
//   (void) sourceReg;
//   (void) srcIsERegType;

//   if ( dstIsERegType )
//     return( TCINSTRUCTIONS.CreateERegister( "" ) );
//   else
//     return( TCINSTRUCTIONS.CreateRegister( "" ) );
// };


// /*
//   typeIsStoredInEReg is a little helper function that returns whether a given IR
//   type is stored in a 64-bit extended data register.
// */
// bool Cast::typeIsStoredInEReg( const IR_Type &t )
// {
//   DSTART( "static bool Cast::typeIsStoredInEReg(const IR_Type&)" );

//   auto metaType = t.getType();
//   return(
//     ( metaType == IR_Type::DOUBLE ) || ( metaType == IR_Type::LONG_DOUBLE ) ||
//     ( metaType == IR_Type::LONG_LONG ) ||
//     ( metaType == IR_Type::UNSIGNED_LONG_LONG ) );
// };
}