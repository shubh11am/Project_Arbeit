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
  @file mipsimmediate16unsigned.h
  @brief This file provides the interface of unsigned 16-bit immediate
         parameters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _MIPS_IMMEDIATE16_UNSIGNED_H
#define _MIPS_IMMEDIATE16_UNSIGNED_H


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
  @brief Class MIPS_Immediate16_Unsigned is the representation of unsigned
         16-bit immediate parameters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class MIPS_Immediate16_Unsigned final : public WIR_UnsignedImmediateParameter<MIPS_Immediate16_Unsigned>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief No standard construction allowed, users must use
             MIPS_Immediate16_Unsigned( unsigned long long ) instead.
    */
    MIPS_Immediate16_Unsigned( void ) = delete;

    /*!
      @brief Default constructor for unsigned 16-bit immediate parameters.

      @param[in] __i The immediate value.

      The constructor ensures that __i lies in the range of values that can be
      represented with 16 bits (unsigned).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit MIPS_Immediate16_Unsigned( unsigned long long );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_Immediate16_Unsigned( const MIPS_Immediate16_Unsigned & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_Immediate16_Unsigned( MIPS_Immediate16_Unsigned && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~MIPS_Immediate16_Unsigned( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_Immediate16_Unsigned & operator = ( const MIPS_Immediate16_Unsigned & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_Immediate16_Unsigned & operator = ( MIPS_Immediate16_Unsigned && );

};

}       // namespace WIR

#endif  // _MIPS_IMMEDIATE16_UNSIGNED_H
