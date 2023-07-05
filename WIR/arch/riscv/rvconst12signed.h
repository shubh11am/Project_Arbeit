/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file rvconst12signed.h
  @brief This file provides the interface of signed 12 bits-wide immediate
         parameters.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/


#ifndef _RV_CONST12_SIGNED_H
#define _RV_CONST12_SIGNED_H


//
// Include section
//

// Include WIR headers
#include <wir/wirsignedimmediateparameter.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class RV_Const12_Signed is the representation of signed const12
         parameters.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/
class RV_Const12_Signed final : public WIR_SignedImmediateParameter<RV_Const12_Signed>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief No standard construction allowed, users must use
             RV_Const12_Signed( signed long long ) instead.
    */
    RV_Const12_Signed( void ) = delete;

    /*!
      @brief Default constructor for signed const12 parameters.

      @param[in] __i The immediate value.

      The constructor ensures that __i lies in the range of values that can be
      represented with 12 bits, assuming two's-complement as underlying data
      format.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    explicit RV_Const12_Signed( signed long long );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV_Const12_Signed( const RV_Const12_Signed & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV_Const12_Signed( RV_Const12_Signed && );

    /*!
      @brief Destructor.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    virtual ~RV_Const12_Signed( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV_Const12_Signed & operator = ( const RV_Const12_Signed & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV_Const12_Signed & operator = ( RV_Const12_Signed && );

};

}       // namespace WIR

#endif  // _RV_CONST12_SIGNED_H
