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

/*!
  @file armconst5rotateamount.h
  @brief This file provides the interface of unsigned 5 bits-wide immediate
         parameters for immediate data-processing operands.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _ARM_CONST5_ROTATEAMOUNT_H
#define _ARM_CONST5_ROTATEAMOUNT_H


//
// Include section
//

// Include WIR headers
#include <wir/wirunsignedimmediateparameter.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class ARM_Const5_RotateAmount is the representation of unsigned const5
         parameters.

  According to the ARM Architecture Reference Manual (section A5.1.3, page
  A5-6f.), immediate values are encoded as 8-bit immediate and 4-bit rotate
  amount. This class ARM_Const5_RotateAmount serves to represent only the 4-bit
  rotate amount. As the rotate amount is defined to be 2 * rotate_imm (cf. page
  A5-7), and rotate_imm is 4 bits wide, the rotate amount is only allowed to
  hold the even values between 0 and 30. Exactly this behavior is modeled by
  class ARM_Const5_RotateAmount.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class ARM_Const5_RotateAmount final : public WIR_UnsignedImmediateParameter<ARM_Const5_RotateAmount>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief No standard construction allowed, users must use
             ARM_Const5_RotateAmount( unsigned long long ) instead.
    */
    ARM_Const5_RotateAmount( void ) = delete;

    /*!
      @brief Default constructor for unsigned const5 parameters.

      @param[in] __i The immediate value.

      The constructor ensures that __i lies in the range of values that can be
      represented as rotate amounts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit ARM_Const5_RotateAmount( unsigned long long );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Const5_RotateAmount( const ARM_Const5_RotateAmount & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Const5_RotateAmount( ARM_Const5_RotateAmount && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~ARM_Const5_RotateAmount( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Const5_RotateAmount & operator = ( const ARM_Const5_RotateAmount & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Const5_RotateAmount & operator = ( ARM_Const5_RotateAmount && );


    //
    // Value handling.
    //

    /*!
      @brief setValue sets an unsigned immediate parameter's actual value.

      @param[in] i The parameter's new immediate value.

      setValue ensures that i lies in the range of values that can be
      represented as rotate amounts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void setValue( unsigned long long );

};

}       // namespace WIR

#endif  // _ARM_CONST5_ROTATEAMOUNT_H
