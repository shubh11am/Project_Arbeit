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



// this class requires further implementation to be operational beyond pointer casts
// to make this easier, all left out functionality is just commented out



#ifndef _RV32_CAST_H
#define _RV32_CAST_H

// Include standard headers
#include <limits.h>

// Include local headers
#include <rv32/rv32codesel.h>


//
// Class forward declarations
//

namespace WIR {
class WIR_VirtualRegister;
}


//
// Header section
//

namespace RV32 {
/*!
  @brief struct ConstCastIntegralResult represents the result of casting a
         constant value.
*/
struct ConstCastIntegralResult
{
  IR_Integer value;
  IR_Type &type;
};

/*!
  @brief Class Cast handles all type-casting and generates the code for it, too.
*/
class Cast
{

  public:

    //! Calculates the local costs of performing a type-conversion.
    static int castingCost( NODEPTR, NODEPTR );

    /*!
      @brief castingCost determines the cost of a type conversion.

      @param[in] dstType A const reference to the destination IR type.
      @param[in] srcType A const reference to the virtual register's source IR
                         type.
      @return The costs to convert values from srcType to dstType.
    */
    static int castingCost( const IR_Type &, const IR_Type & );

    /*!
      @brief doCasting performs a type conversion between two compatible types.

      @param[in] dstType A const reference to the destination IR type.
      @param[in] srcType A const reference to the virtual register's source IR
                         type.
      @param[in] r A const reference to the virtual %WIR register to be
                   truncated.
      @param[in] exp A const pointer to an IR expression used to generate
                     assembler debug information.
      @return A reference to a newly created virtual register that holds the
              truncated value. If no code for the conversion is generated and
              thus no new virtual register was created, doTruncation returns r.
    */
    static WIR::WIR_VirtualRegister &doCasting( const IR_Type &,
                                                const IR_Type &,
                                                const WIR::WIR_VirtualRegister &,
                                                const IR_Exp * = nullptr );

    /*!
      (DEPRECATED)
      castingCost determines the local costs involved in casting a type into
      INT.
    */
    // static int castingCost( NODEPTR );

    // //! This function determines the cost of a truncation operation
    // static int truncationCost( NODEPTR, NODEPTR );

    // /*!
    //   @brief truncationCost determines the cost of a truncation operation.

    //   @param[in] dstType A const reference to the destination IR type.
    //   @param[in] srcType A const reference to the virtual register's source IR
    //                      type.
    //   @return The costs to truncate values from srcType's bitwidth to dstType's
    //           bitwidth.
    // */
    // static int truncationCost( const IR_Type &, const IR_Type & );

    /*!
      @brief doTruncation performs the truncation of a virtual register's
             contents to the bitwidth of the supposed ICD-C data type.

      @param[in] dstType A const reference to the destination IR type.
      @param[in] srcType A const reference to the virtual register's source IR
                         type.
      @param[in] r A const reference to the virtual %WIR register to be
                   truncated.
      @param[in] exp A const pointer to an IR expression used to generate
                     assembler debug information.
      @return A reference to a newly created virtual register that holds the
              truncated value. If no code for the conversion is generated and
              thus no new virtual register was created, doTruncation returns r.
    */
    static WIR::WIR_VirtualRegister &doTruncation( const IR_Type &,
                                                   const IR_Type &,
                                                   const WIR::WIR_VirtualRegister &,
                                                   const IR_Exp * = nullptr );

    // /*!
    //   @brief constantCastToIntegral returns the result of the casted constant
    //          expression to an integral type.

    //   @param[in] exp A const reference to a constant IR expression to be casted.
    //   @return The result of the cast to an integral type, or nulltr if the
    //           resulting constant is not integral.

    //   The caller must delete the result.
    // */
    // static ConstCastIntegralResult *constantCastToIntegral( const IR_Exp &exp );

    // /*!
    //   @brief constantCastToFloat returns the result of the casted constant
    //          expression to a float type.

    //   @param[in] exp A const reference to a constant IR expression to be casted.
    //   @return The result of the cast to a float type, or nulltr if the
    //           resulting constant is not float.

    //   The caller must delete the result.
    // */
    // static Float *constantCastToFloat( const IR_Exp & );

    // /*!
    //   @brief constantCastToDouble returns the result of the casted constant
    //          expression to a double type.

    //   @param[in] exp A const reference to a constant IR expression to be casted.
    //   @return The result of the cast to a double type, or nulltr if the
    //           resulting constant is not double.

    //   The caller must delete the result.
    // */
    // static Double *constantCastToDouble( const IR_Exp & );


  private:

    //! Internal result type for constant casts.
    // struct ConstCastResult
    // {
    //   struct ValueUnion
    //   {
    //     IR_Integer integral;
    //     Float flt;
    //     Double dbl;
    //   } value;

    //   IR_Type *type;
    // };

    // /*!
    //   @brief Returns the casted constant expression, if 'exp' is one.

    //   The caller must delete the result.
    // */
    // static ConstCastResult *constantCast( const IR_Exp & );

    // //! Casts the given input constant to single-precision float.
    // static Float constantCastToFloat( const Float & );

    // //! Casts the given input constant to single-precision float.
    // static Float constantCastToFloat( const Double & );

    // //! Casts the given input constant to single-precision float.
    // static Float constantCastToFloat( const IR_Integer & );

    // //! Casts the given input constant to double-precision float.
    // static Double constantCastToDouble( const Float & );

    // //! Casts the given input constant to double-precision float.
    // static Double constantCastToDouble( const Double & );

    // //! Casts the given input constant to double-precision float.
    // static Double constantCastToDouble( const IR_Integer & );

    // /*!
    //   @brief Casts the given input constant to an integer with the given width
    //          and sign.
    // */
    // static IR_Integer constantCastToIntegral( const Float &, const IR_Type & );

    // /*!
    //   @brief Casts the given input constant to an integer with the given width
    //          and sign.
    // */
    // static IR_Integer constantCastToIntegral( const Double &, const IR_Type & );

    // /*!
    //   @brief Casts the given input constant to an integer with the given width
    //          and sign.
    // */
    // static IR_Integer constantCastToIntegral( const IR_Integer &,
    //                                           const IR_Type & );

    // /*!
    //   @brief typeIsStoredInEReg is a little helper function that returns whether
    //          a given IR type is stored in a 64-bit extended data register.

    //   @param[in] t A const reference to an IR type.
    //   @return true iff the IR type is represented by extended data registers,
    //           false otherwise.
    // */
    // static bool typeIsStoredInEReg( const IR_Type & );

};
}
#endif  // _RV32_CAST_H