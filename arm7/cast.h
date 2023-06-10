/*

   This header file belongs to the

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


#ifndef _ARM7_CAST_H
#define _ARM7_CAST_H


//
// Include section
//

// Include standard headers
#include <limits.h>

// Include ICD headers
#include <icd-c.h>
#include <icdint/icdint.h>
#include <icdint/float_type.h>

// Include local headers
#include <arm7/auxfuncs.h>


//
// Header section
//

/*! The result of casting a constant value. */
struct ConstCastIntegralResult
{
  IR_Integer value;
  IR_Type &type;

  ConstCastIntegralResult( IR_Integer pValue, IR_Type &pType ) :
    value( pValue ), type( pType ) {}
};


/*! A class that handles all casting and generates the code for it, too. */
class Cast
{

  public:

    //! Returns the casted constant expression.
    /*! If the type of the resulting constant is not integral, 0 is returned.
        The caller must delete the result. */
    static ConstCastIntegralResult *constantCastToIntegral( const IR_Exp &exp );

  static LLIR_Register* doCasting( const IR_Type& dstType, const IR_Type& srcType,
                            LLIR_Register* srcReg, IR_Exp* exp = nullptr );

  static LLIR_Register *castToBool( LLIR_Register *reg, IR_Exp *exp );
  static LLIR_Register *castToChar( LLIR_Register *reg, const IR_Type &sourcetype,
    const IR_Type &destinationtype, IR_Exp *exp );
  static LLIR_Register *castToShort( LLIR_Register *reg, const IR_Type &sourcetype,
    const IR_Type &destinationtype, IR_Exp *exp );
  static LLIR_Register *castToLong( LLIR_Register *reg, const IR_Type &sourcetype,
                             IR_Exp *exp );
  static LLIR_Register *castToPointer( LLIR_Register *reg );
  static LLIR_Register *castToFloat( LLIR_Register *reg, const IR_Type &sourcetype,
                              IR_Exp *exp );
  static LLIR_Register *floatToInt( LLIR_Register *reg,
    const IR_Type &destinationtype, IR_Exp * exp );
  static LLIR_Register* doubleToInt( LLIR_Register *ereg,
    const IR_Type &destinationtype,IR_Exp * exp );
  static LLIR_Register* castToLonglong( LLIR_Register *reg,
    const IR_Type &sourcetype, IR_Exp *exp );
  static LLIR_Register* castToDouble( LLIR_Register *reg,
    const IR_Type &sourcetype, IR_Exp *exp );
  static int castingCosts( const IR_Type &sourcetype, const IR_Type &destinationtype );
  static int castingCosts( const IR_Type::Type sourcetype,
                           const IR_Type::Type destinationtype, IR* ir );
  static void truncate( LLIR_Register* reg, LLIR_Register* res,
                        const IR_Type& sourcetype, IR_Exp *exp );
  static int truncateCosts( const IR_Type& sourcetype );

  private:

    //! Internal result type for constant casts
    struct ConstCastResult {
      struct ValueUnion {
        IR_Integer integral;
        Float flt;
        Double dbl;
      } value;
      IR_Type *type;
    };

    //! Returns the casted constant expression, if 'exp' is one.
    /*! The caller must delete the result. */
    static ConstCastResult *constantCast( const IR_Exp &exp );

    //! Casts the given input constant to single-precision float
    static Float constantCastToFloat( const Float &f );

    //! Casts the given input constant to single-precision float
    static Float constantCastToFloat( const Double &d );

    //! Casts the given input constant to single-precision float
    static Float constantCastToFloat( const IR_Integer &i );

    //! Casts the given input constant to double-precision float
    static Double constantCastToDouble( const Float &f );

    //! Casts the given input constant to double-precision float
    static Double constantCastToDouble( const Double &d );

    //! Casts the given input constant to double-precision float
    static Double constantCastToDouble( const IR_Integer &i );

    //! Casts the given input constant to an integer with the given width and sign
    static IR_Integer constantCastToIntegral( const Float &f,
                                              const IR_Type &targetType  );

    //! Casts the given input constant to an integer with the given width and sign
    static IR_Integer constantCastToIntegral( const Double &d,
                                              const IR_Type &targetType  );

    //! Casts the given input constant to an integer with the given width and sign
    static IR_Integer constantCastToIntegral( const IR_Integer &i,
                                              const IR_Type &targetType  );

};

#endif  // _ARM7_CAST_H
