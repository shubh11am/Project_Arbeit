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
  @file mipsimmediate5shamt.h
  @brief This file provides the interface of 5-bit shift amount parameters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _MIPS_IMMEDIATE5_SHAMT_H
#define _MIPS_IMMEDIATE5_SHAMT_H


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
  @brief Class MIPS_Immediate5_Shamt is the representation of 5-bit shift
         amount parameters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class MIPS_Immediate5_Shamt final : public WIR_UnsignedImmediateParameter<MIPS_Immediate5_Shamt>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief No standard construction allowed, users must use
             MIPS_Immediate5_Shamt( unsigned long long ) instead.
    */
    MIPS_Immediate5_Shamt( void ) = delete;

    /*!
      @brief Default constructor for 5-bit shift amount parameters.

      @param[in] __i The immediate value.

      The constructor ensures that __i lies in the range of values that can be
      represented with 5 bits (unsigned).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit MIPS_Immediate5_Shamt( unsigned long long );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_Immediate5_Shamt( const MIPS_Immediate5_Shamt & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_Immediate5_Shamt( MIPS_Immediate5_Shamt && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~MIPS_Immediate5_Shamt( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_Immediate5_Shamt & operator = ( const MIPS_Immediate5_Shamt & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_Immediate5_Shamt & operator = ( MIPS_Immediate5_Shamt && );

};

}       // namespace WIR

#endif  // _MIPS_IMMEDIATE5_SHAMT_H
