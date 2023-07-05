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
  @file tcasmconstant.h
  @brief This file provides the interface of constant assembly arguments.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_ASMCONSTANT_H
#define _TC_ASMCONSTANT_H


//
// Include section
//

// Include local headers
#include "tcasmargument.h"


//
// Header section
//

namespace WIR {

/*!
  @brief Class TC_AsmConstant represents numeric constant arguments for an
         assembly operation.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_AsmConstant final : public TC_AsmArgument
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating a signed constant argument.

      @param[in] v A signed long long holding the constant's value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_AsmConstant( signed long long );

    /*!
      @brief Default constructor creating an unsigned constant argument.

      @param[in] v An unsigned long long holding the constant's value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_AsmConstant( unsigned long long );

    /*!
      @brief Copy constructor.

      @param[in] c A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmConstant( const TC_AsmConstant & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_AsmConstant( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmConstant & operator = ( const TC_AsmConstant & );


    //
    // Type management.
    //

    /*!
      @brief isCompatible returns whether a constant is compatible with a given
             argument type.

      @param[in] t A specifier denoting the argument's type.
      @return true if the constant is compatible with t's type, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isCompatible( Type ) const;


    //
    // Value management.
    //

    /*!
      @brief getSignedValue gets a constant's signed value.

      @return The constant's current signed value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    signed long long getSignedValue( void ) const;

    /*!
      @brief getUnsignedValue gets a constant's unsigned value.

      @return The constant's current unsigned value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long long getUnsignedValue( void ) const;

    /*!
      @brief isSigned returns whether a constant argument is signed or not.

      @return true if the constant is signed, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSigned( void ) const;

    /*!
      @brief isUnsigned returns whether a constant argument is unsigned or not.

      @return true if the constant is unsigned, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isUnsigned( void ) const;


  protected:

    /*!
      @brief clone creates a copy of a constant argument.

      @return A pointer to the newly created copy of this constant.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual TC_AsmConstant *clone( void ) const;


  private:

    /*!
      @brief No standard construction allowed, users must use
             TC_AsmConstant( long long )
             instead.
    */
    TC_AsmConstant( void ) = delete;

    /*!
      @brief determineSignedType determines the argument type for a given
             signed value.

      @param[in] v A signed long long holding the constant's value.
      @return An argument type appropriate for the given signed value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static TC_AsmArgument::Type determineSignedType( signed long long );

    /*!
      @brief determineUnsignedType determines the argument type for a given
             unsigned value.

      @param[in] v An unsigned long long holding the constant's value.
      @return An argument type appropriate for the given unsigned value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static TC_AsmArgument::Type determineUnsignedType( unsigned long long );

    //! mValue stores a constant argument's actual value.
    union
    {

      //! sVal stores a signed constant value.
      signed long long sVal;

      //! uVal stores an unsigned constant value.
      unsigned long long uVal;

    } mValue;

    //! mIsSigned stores whether a constant argument is signed or unsigned.
    bool mIsSigned;

};

}       // namespace WIR

#endif  // _TC_ASMCONSTANT_H
