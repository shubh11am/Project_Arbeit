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
  @file armconst5unsigned0.h
  @brief This file provides the interface of unsigned 5 bits-wide immediate
         parameters, excluding the value '0'.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _ARM_CONST5_UNSIGNED0_H
#define _ARM_CONST5_UNSIGNED0_H


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
  @brief Class ARM_Const5_Unsigned0 is the representation of unsigned const5
         parameters without the value '0'.

  This class thus models immediate values form the interval [1, 31].

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class ARM_Const5_Unsigned0 final : public WIR_UnsignedImmediateParameter<ARM_Const5_Unsigned0>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief No standard construction allowed, users must use
             ARM_Const5_Unsigned0( unsigned long long ) instead.
    */
    ARM_Const5_Unsigned0( void ) = delete;

    /*!
      @brief Default constructor for unsigned const5 parameters.

      @param[in] __i The immediate value.

      The constructor ensures that __i lies in the range of values that can be
      represented with 5 bits (unsigned) and that __i is greater than 0.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit ARM_Const5_Unsigned0( unsigned long long );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Const5_Unsigned0( const ARM_Const5_Unsigned0 & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Const5_Unsigned0( ARM_Const5_Unsigned0 && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~ARM_Const5_Unsigned0( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Const5_Unsigned0 & operator = ( const ARM_Const5_Unsigned0 & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Const5_Unsigned0 & operator = ( ARM_Const5_Unsigned0 && );


    //
    // Value handling.
    //

    /*!
      @brief setValue sets an unsigned immediate parameter's actual value.

      @param[in] i The parameter's new immediate value.

      setValue ensures that __i lies in the range of values that can be
      represented with 5 bits (unsigned) and that __i is greater than 0.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void setValue( unsigned long long );

};

}       // namespace WIR

#endif  // _ARM_CONST5_UNSIGNED0_H
