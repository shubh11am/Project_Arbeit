/*

   This header file belongs to the

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


#ifndef _ARM7_INCL_H_
#define _ARM7_INCL_H_


//
// Include section
//

// Include standard headers
#include <deque>
#include <string>

// Include ICD headers
#include <icd-c.h>
#include <icdint/icdint.h>


//
// Class forward declarations
//

class IR_IntConstExp;
class IR_Integer;
class IR_Symbol;
class IR_SymbolExp;

class LLIR_Register;

class Registrar;
class ARM_LValue;


//
// Header section
//

//! A helper function to create an appropriate register for the given type.
LLIR_Register *getNewLHSRegister( std::string target, const IR_Type &type );

union AssemblyOperand {
  ARM_LValue *lvalue;
  LLIR_Register* reg;
};

//! We need this typedef due to technical reasons
//! (no '<' or '>' allowed in tc_decl.m4)
typedef std::deque<LLIR_Register*> regptr_list;

/*!
   This enum describes how a certain value is passed ( register / address
   register / extended register / no register at all ).
 */
enum RegType {
  DATA_REGISTER,
  ADDRESS_REGISTER,
  EXTENDED_REGISTER,
  NO_REGISTER
};


//######################################################################
//
// Utility methods
//
//######################################################################

#define HEADER_IS_TYPE_EXP_WRAPPER( funcName )       \
bool funcName ( const IR_Exp &exp );
#define HEADER_IS_TYPE_SYMBOL_WRAPPER( funcName )    \
bool funcName ( const IR_Symbol &sym );
#define HEADER_IS_POINTER_EXP_WRAPPER( funcName )    \
bool funcName ( const IR_Exp &exp );
#define HEADER_IS_POINTER_SYMBOL_WRAPPER( funcName ) \
bool funcName ( const IR_Symbol &sym );
#define HEADER_IS_ARRAY_EXP_WRAPPER( funcName )      \
bool funcName ( const IR_Exp &exp );
#define HEADER_IS_ARRAY_SYMBOL_WRAPPER( funcName )   \
bool funcName ( const IR_Symbol &sym );


/*! Returns whether the given type is a pointer type. */
bool isPointerType( const IR_Type &type );
HEADER_IS_TYPE_EXP_WRAPPER(    isPointerType );
HEADER_IS_TYPE_SYMBOL_WRAPPER( isPointerType );
HEADER_IS_ARRAY_EXP_WRAPPER(    isPointerArray );
HEADER_IS_ARRAY_SYMBOL_WRAPPER( isPointerArray );
HEADER_IS_POINTER_EXP_WRAPPER(    isPointerPointer );
HEADER_IS_POINTER_SYMBOL_WRAPPER( isPointerPointer );


/*! Returns whether the given type is an array type. */
bool isArrayType( const IR_Type &type );
HEADER_IS_TYPE_EXP_WRAPPER(    isArrayType );
HEADER_IS_TYPE_SYMBOL_WRAPPER( isArrayType );
HEADER_IS_ARRAY_EXP_WRAPPER(    isArrayArray );
HEADER_IS_ARRAY_SYMBOL_WRAPPER( isArrayArray );
HEADER_IS_POINTER_EXP_WRAPPER(    isArrayPointer );
HEADER_IS_POINTER_SYMBOL_WRAPPER( isArrayPointer );


/*! Returns whether the given type is a composed type. */
bool isComposedType( const IR_Type &type );
HEADER_IS_TYPE_EXP_WRAPPER(    isComposedType );
HEADER_IS_TYPE_SYMBOL_WRAPPER( isComposedType );
HEADER_IS_ARRAY_EXP_WRAPPER(    isComposedArray );
HEADER_IS_ARRAY_SYMBOL_WRAPPER( isComposedArray );
HEADER_IS_POINTER_EXP_WRAPPER(    isComposedPointer );
HEADER_IS_POINTER_SYMBOL_WRAPPER( isComposedPointer );


/*! Returns whether the given type is a function type. */
bool isFunctionType( const IR_Type &type );
HEADER_IS_TYPE_EXP_WRAPPER(    isFunctionType );
HEADER_IS_TYPE_SYMBOL_WRAPPER( isFunctionType );
HEADER_IS_ARRAY_EXP_WRAPPER(    isFunctionArray );
HEADER_IS_ARRAY_SYMBOL_WRAPPER( isFunctionArray );
HEADER_IS_POINTER_EXP_WRAPPER(    isFunctionPointer );
HEADER_IS_POINTER_SYMBOL_WRAPPER( isFunctionPointer );


/*! Returns whether the given type is a bitfield type. */
bool isBitfieldType( const IR_Type &type );
HEADER_IS_TYPE_EXP_WRAPPER(    isBitfieldType );
HEADER_IS_TYPE_SYMBOL_WRAPPER( isBitfieldType );
HEADER_IS_ARRAY_EXP_WRAPPER(    isBitfieldArray );
HEADER_IS_ARRAY_SYMBOL_WRAPPER( isBitfieldArray );
HEADER_IS_POINTER_EXP_WRAPPER(    isBitfieldPointer );
HEADER_IS_POINTER_SYMBOL_WRAPPER( isBitfieldPointer );


/*! Returns whether the given type is a long long type. */
bool isLongLongType( const IR_Type &type );
HEADER_IS_TYPE_EXP_WRAPPER(    isLongLongType );
HEADER_IS_TYPE_SYMBOL_WRAPPER( isLongLongType );
HEADER_IS_ARRAY_EXP_WRAPPER(    isLongLongArray );
HEADER_IS_ARRAY_SYMBOL_WRAPPER( isLongLongArray );
HEADER_IS_POINTER_EXP_WRAPPER(    isLongLongPointer );
HEADER_IS_POINTER_SYMBOL_WRAPPER( isLongLongPointer );


/*! Returns whether the given int const expression has long long type. */
bool isLongLongConstant( const IR_IntConstExp &constExp );


/*! Returns whether the given type is a double type. */
bool isDoubleType( const IR_Type &type );
HEADER_IS_TYPE_EXP_WRAPPER(    isDoubleType );
HEADER_IS_TYPE_SYMBOL_WRAPPER( isDoubleType );
HEADER_IS_ARRAY_EXP_WRAPPER(    isDoubleArray );
HEADER_IS_ARRAY_SYMBOL_WRAPPER( isDoubleArray );
HEADER_IS_POINTER_EXP_WRAPPER(    isDoublePointer );
HEADER_IS_POINTER_SYMBOL_WRAPPER( isDoublePointer );


/*! Returns whether the given type is float (no double/long double). */
bool isFloatType( const IR_Type &type );
HEADER_IS_TYPE_EXP_WRAPPER(    isFloatType );
HEADER_IS_TYPE_SYMBOL_WRAPPER( isFloatType );
HEADER_IS_ARRAY_EXP_WRAPPER(    isFloatArray );
HEADER_IS_ARRAY_SYMBOL_WRAPPER( isFloatArray );
HEADER_IS_POINTER_EXP_WRAPPER(    isFloatPointer );
HEADER_IS_POINTER_SYMBOL_WRAPPER( isFloatPointer );


/*! Returns whether the given type is a character type. */
bool isCharType( const IR_Type &type );
HEADER_IS_TYPE_EXP_WRAPPER(    isCharType );
HEADER_IS_TYPE_SYMBOL_WRAPPER( isCharType );
HEADER_IS_ARRAY_EXP_WRAPPER(    isCharArray );
HEADER_IS_ARRAY_SYMBOL_WRAPPER( isCharArray );
HEADER_IS_POINTER_EXP_WRAPPER(    isCharPointer );
HEADER_IS_POINTER_SYMBOL_WRAPPER( isCharPointer );


/*! Returns whether the given type is a short integer type. */
bool isShortType( const IR_Type &type );
HEADER_IS_TYPE_EXP_WRAPPER(    isShortType );
HEADER_IS_TYPE_SYMBOL_WRAPPER( isShortType );
HEADER_IS_ARRAY_EXP_WRAPPER(    isShortArray );
HEADER_IS_ARRAY_SYMBOL_WRAPPER( isShortArray );
HEADER_IS_POINTER_EXP_WRAPPER(    isShortPointer );
HEADER_IS_POINTER_SYMBOL_WRAPPER( isShortPointer );


/*! Returns whether the given type is a type that can be
 * represented by nonterminal 'reg'. */
bool isRegType( const IR_Type &type );
HEADER_IS_TYPE_EXP_WRAPPER(    isRegType );
HEADER_IS_TYPE_SYMBOL_WRAPPER( isRegType );
HEADER_IS_ARRAY_EXP_WRAPPER(    isRegArray );
HEADER_IS_ARRAY_SYMBOL_WRAPPER( isRegArray );
HEADER_IS_POINTER_EXP_WRAPPER(    isRegPointer );
HEADER_IS_POINTER_SYMBOL_WRAPPER( isRegPointer );

/*! Returns whether the given type is a type that can be
 * represented by nonterminal 'ereg'. */
bool isERegType( const IR_Type &type );
HEADER_IS_TYPE_EXP_WRAPPER(    isERegType );
HEADER_IS_TYPE_SYMBOL_WRAPPER( isERegType );
HEADER_IS_ARRAY_EXP_WRAPPER(    isERegArray );
HEADER_IS_ARRAY_SYMBOL_WRAPPER( isERegArray );
HEADER_IS_POINTER_EXP_WRAPPER(    isERegPointer );
HEADER_IS_POINTER_SYMBOL_WRAPPER( isERegPointer );

#undef HEADER_IS_TYPE_EXP_WRAPPER
#undef HEADER_IS_TYPE_SYMBOL_WRAPPER
#undef HEADER_IS_POINTER_EXP_WRAPPER
#undef HEADER_IS_POINTER_SYMBOL_WRAPPER
#undef HEADER_IS_ARRAY_EXP_WRAPPER
#undef HEADER_IS_ARRAY_SYMBOL_WRAPPER


//! Computes the size of an IR Type in bytes.
int computeSizeOf( IR_Type *type );
//! Computes the size of a tree element in bytes.
int computeSizeOf( NODEPTR treeElem );

/* Extracts the lower word from a long long. */
ICDInt64 getLowerLongLongWord( const IR_Integer &ll );

//! Extracts the upper word from a long long.
ICDInt64 getUpperLongLongWord( const IR_Integer &ll );

//! Returns a register which contains the value associated with the Symbol Exp
LLIR_Register *loadRegister( IR_SymbolExp* );


//######################################################################
//
// Definitions of public inline functions
//
//######################################################################


/*! This macro generates a wrapper function for a given type-check function. */
#define IS_TYPE_EXP_WRAPPER( funcName )             \
inline bool funcName ( const IR_Exp &exp )          \
{                                                   \
  return funcName ( exp.getType() );                \
}
/*! This macro generates a wrapper function for a given type-check function. */
#define IS_TYPE_SYMBOL_WRAPPER( funcName )             \
inline bool funcName ( const IR_Symbol &sym )          \
{                                                      \
  return funcName ( sym.getType() );                   \
}
/*! This macro generates a wrapper function for a given type-check function. */
#define IS_POINTER_EXP_WRAPPER( funcName, nonPointerFunc ) \
inline bool funcName ( const IR_Exp &exp )                 \
{                                                          \
  IR_PointerType *ptype = dynamic_cast<IR_PointerType *>(  \
                            &( exp.getType() ) );          \
  return ptype && nonPointerFunc ( ptype->getBaseType() ); \
}
/*! This macro generates a wrapper function for a given type-check function. */
#define IS_POINTER_SYMBOL_WRAPPER( funcName, nonPointerFunc ) \
inline bool funcName ( const IR_Symbol &sym )                 \
{                                                             \
  IR_PointerType *ptype = dynamic_cast<IR_PointerType *>(     \
                            &( sym.getType() ) );             \
  return ptype && nonPointerFunc ( ptype->getBaseType() );    \
}
/*! This macro generates a wrapper function for a given type-check function. */
#define IS_ARRAY_EXP_WRAPPER( funcName, nonPointerFunc )   \
inline bool funcName ( const IR_Exp &exp )                 \
{                                                          \
  IR_ArrayType *atype = dynamic_cast<IR_ArrayType *>(      \
                            &( exp.getType() ) );          \
  return atype && nonPointerFunc ( atype->getBaseType() ); \
}
/*! This macro generates a wrapper function for a given type-check function. */
#define IS_ARRAY_SYMBOL_WRAPPER( funcName, nonPointerFunc )   \
inline bool funcName ( const IR_Symbol &sym )                 \
{                                                             \
  IR_ArrayType *atype = dynamic_cast<IR_ArrayType *>(         \
                            &( sym.getType() ) );             \
  return atype && nonPointerFunc ( atype->getBaseType() );    \
}


/*! Returns whether the given type is a pointer type. */
inline bool isPointerType( const IR_Type &type )
{
  return ( type.getType() == IR_Type::POINTER  ||
           // The IR produces wrong type objects with invalid type ids from
           // time to time, so we must try to cast the object as well because
           // in those cases this is the only thing that works
           (  dynamic_cast<const IR_PointerType*>( &type ) &&
             !dynamic_cast<const IR_ArrayType*>(   &type ) ) );
}
IS_TYPE_EXP_WRAPPER(    isPointerType );
IS_TYPE_SYMBOL_WRAPPER( isPointerType );
IS_ARRAY_EXP_WRAPPER(    isPointerArray, isPointerType );
IS_ARRAY_SYMBOL_WRAPPER( isPointerArray, isPointerType );
IS_POINTER_EXP_WRAPPER(    isPointerPointer, isPointerType );
IS_POINTER_SYMBOL_WRAPPER( isPointerPointer, isPointerType );


/*! Returns whether the given type is an array type. */
inline bool isArrayType( const IR_Type &type )
{
  return ( type.getType() == IR_Type::ARRAY ||
           // The IR produces wrong type objects with invalid type ids from
           // time to time, so we must try to cast the object as well because
           // in those cases this is the only thing that works
           dynamic_cast<const IR_ArrayType*>( &type ) );
}
IS_TYPE_EXP_WRAPPER(    isArrayType );
IS_TYPE_SYMBOL_WRAPPER( isArrayType );
IS_ARRAY_EXP_WRAPPER(    isArrayArray, isArrayType );
IS_ARRAY_SYMBOL_WRAPPER( isArrayArray, isArrayType );
IS_POINTER_EXP_WRAPPER(    isArrayPointer, isArrayType );
IS_POINTER_SYMBOL_WRAPPER( isArrayPointer, isArrayType );


/*! Returns whether the given type is a composed type. */
inline bool isComposedType( const IR_Type &type )
{
  return ( type.getType() == IR_Type::STRUCT ||
           type.getType() == IR_Type::UNION  ||
           // The IR produces wrong type objects with invalid type ids from
           // time to time, so we must try to cast the object as well because
           // in those cases this is the only thing that works
           dynamic_cast<const IR_ComposedType*>( &type ) );
}
IS_TYPE_EXP_WRAPPER(    isComposedType );
IS_TYPE_SYMBOL_WRAPPER( isComposedType );
IS_ARRAY_EXP_WRAPPER(    isComposedArray, isComposedType );
IS_ARRAY_SYMBOL_WRAPPER( isComposedArray, isComposedType );
IS_POINTER_EXP_WRAPPER(    isComposedPointer, isComposedType );
IS_POINTER_SYMBOL_WRAPPER( isComposedPointer, isComposedType );


/*! Returns whether the given type is a function type. */
inline bool isFunctionType( const IR_Type &type )
{
  return ( type.getType() == IR_Type::FUNCTION  ||
           // The IR produces wrong type objects with invalid type ids from
           // time to time, so we must try to cast the object as well because
           // in those cases this is the only thing that works
           dynamic_cast<const IR_FunctionType*>( &type ) );
}
IS_TYPE_EXP_WRAPPER(    isFunctionType );
IS_TYPE_SYMBOL_WRAPPER( isFunctionType );
IS_ARRAY_EXP_WRAPPER(    isFunctionArray, isFunctionType );
IS_ARRAY_SYMBOL_WRAPPER( isFunctionArray, isFunctionType );
IS_POINTER_EXP_WRAPPER(    isFunctionPointer, isFunctionType );
IS_POINTER_SYMBOL_WRAPPER( isFunctionPointer, isFunctionType );


/*! Returns whether the given type is a bitfield type. */
inline bool isBitfieldType( const IR_Type &type )
{
  return ( dynamic_cast<const IR_BitfieldType *>( &type ) != nullptr );
}
IS_TYPE_EXP_WRAPPER(    isBitfieldType );
IS_TYPE_SYMBOL_WRAPPER( isBitfieldType );
IS_ARRAY_EXP_WRAPPER(    isBitfieldArray, isBitfieldType );
IS_ARRAY_SYMBOL_WRAPPER( isBitfieldArray, isBitfieldType );
IS_POINTER_EXP_WRAPPER(    isBitfieldPointer, isBitfieldType );
IS_POINTER_SYMBOL_WRAPPER( isBitfieldPointer, isBitfieldType );


/*! Returns whether the given type is a long long type. */
inline bool isLongLongType( const IR_Type &type )
{
  return ( type.getType() == IR_Type::LONG_LONG ||
           type.getType() == IR_Type::UNSIGNED_LONG_LONG );
}
IS_TYPE_EXP_WRAPPER(    isLongLongType );
IS_TYPE_SYMBOL_WRAPPER( isLongLongType );
IS_ARRAY_EXP_WRAPPER(    isLongLongArray, isLongLongType );
IS_ARRAY_SYMBOL_WRAPPER( isLongLongArray, isLongLongType );
IS_POINTER_EXP_WRAPPER(    isLongLongPointer, isLongLongType );
IS_POINTER_SYMBOL_WRAPPER( isLongLongPointer, isLongLongType );


/*! Returns whether the given int const expression has long long type. */
inline bool isLongLongConstant( const IR_IntConstExp &constExp )
{
  return isLongLongType( constExp );
}


/*! Returns whether the given type is a double type. */
inline bool isDoubleType( const IR_Type &type )
{
  return ( type.getType() == IR_Type::DOUBLE ||
           type.getType() == IR_Type::LONG_DOUBLE );
}
IS_TYPE_EXP_WRAPPER(    isDoubleType );
IS_TYPE_SYMBOL_WRAPPER( isDoubleType );
IS_ARRAY_EXP_WRAPPER(    isDoubleArray, isDoubleType );
IS_ARRAY_SYMBOL_WRAPPER( isDoubleArray, isDoubleType );
IS_POINTER_EXP_WRAPPER(    isDoublePointer, isDoubleType );
IS_POINTER_SYMBOL_WRAPPER( isDoublePointer, isDoubleType );


/*! Returns whether the given type is float (no double/long double). */
inline bool isFloatType( const IR_Type &type )
{
  return ( type.getType() == IR_Type::FLOAT );
}
IS_TYPE_EXP_WRAPPER(    isFloatType );
IS_TYPE_SYMBOL_WRAPPER( isFloatType );
IS_ARRAY_EXP_WRAPPER(    isFloatArray, isFloatType );
IS_ARRAY_SYMBOL_WRAPPER( isFloatArray, isFloatType );
IS_POINTER_EXP_WRAPPER(    isFloatPointer, isFloatType );
IS_POINTER_SYMBOL_WRAPPER( isFloatPointer, isFloatType );


/*! Returns whether the given type is a character type. */
inline bool isCharType( const IR_Type &type )
{
  return ( type.getType() == IR_Type::CHAR ||
           type.getType() == IR_Type::UNSIGNED_CHAR );
}
IS_TYPE_EXP_WRAPPER(    isCharType );
IS_TYPE_SYMBOL_WRAPPER( isCharType );
IS_ARRAY_EXP_WRAPPER(    isCharArray, isCharType );
IS_ARRAY_SYMBOL_WRAPPER( isCharArray, isCharType );
IS_POINTER_EXP_WRAPPER(    isCharPointer, isCharType );
IS_POINTER_SYMBOL_WRAPPER( isCharPointer, isCharType );


/*! Returns whether the given type is a short integer type. */
inline bool isShortType( const IR_Type &type )
{
  return ( type.getType() == IR_Type::SHORT ||
           type.getType() == IR_Type::UNSIGNED_SHORT );
}
IS_TYPE_EXP_WRAPPER(    isShortType );
IS_TYPE_SYMBOL_WRAPPER( isShortType );
IS_ARRAY_EXP_WRAPPER(    isShortArray, isShortType );
IS_ARRAY_SYMBOL_WRAPPER( isShortArray, isShortType );
IS_POINTER_EXP_WRAPPER(    isShortPointer, isShortType );
IS_POINTER_SYMBOL_WRAPPER( isShortPointer, isShortType );


/*! Returns whether the given type is a type that can be
 * represented by nonterminal 'reg'. */
inline bool isRegType( const IR_Type &type )
{
  return ( type.getType() == IR_Type::VOID           ||
           type.getType() == IR_Type::CHAR           ||
           type.getType() == IR_Type::UNSIGNED_CHAR  ||
           type.getType() == IR_Type::SHORT          ||
           type.getType() == IR_Type::UNSIGNED_SHORT ||
           type.getType() == IR_Type::INT            ||
           type.getType() == IR_Type::UNSIGNED_INT   ||
           type.getType() == IR_Type::LONG           ||
           type.getType() == IR_Type::UNSIGNED_LONG  ||
           type.getType() == IR_Type::FLOAT          ||
           type.getType() == IR_Type::BOOL );
}
IS_TYPE_EXP_WRAPPER(    isRegType );
IS_TYPE_SYMBOL_WRAPPER( isRegType );
IS_ARRAY_EXP_WRAPPER(    isRegArray, isRegType );
IS_ARRAY_SYMBOL_WRAPPER( isRegArray, isRegType );
IS_POINTER_EXP_WRAPPER(    isRegPointer, isRegType );
IS_POINTER_SYMBOL_WRAPPER( isRegPointer, isRegType );

/*! Returns whether the given type is a type that can be
 * represented by nonterminal 'ereg'. */
inline bool isERegType( const IR_Type &type )
{
  return ( type.getType() == IR_Type::DOUBLE             ||
           type.getType() == IR_Type::LONG_DOUBLE        ||
           type.getType() == IR_Type::LONG_LONG          ||
           type.getType() == IR_Type::UNSIGNED_LONG_LONG );
}
IS_TYPE_EXP_WRAPPER(    isERegType );
IS_TYPE_SYMBOL_WRAPPER( isERegType );
IS_ARRAY_EXP_WRAPPER(    isERegArray, isERegType );
IS_ARRAY_SYMBOL_WRAPPER( isERegArray, isERegType );
IS_POINTER_EXP_WRAPPER(    isERegPointer, isERegType );
IS_POINTER_SYMBOL_WRAPPER( isERegPointer, isERegType );

#endif  // _ARM7_INCL_H_
