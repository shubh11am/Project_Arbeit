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
  @file rvregvirtual.h
  @brief This file provides the interface of virtual RISC-V RV32I registers.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/


#ifndef _RV_REGVIRTUAL_H
#define _RV_REGVIRTUAL_H


//
// Include section
//

// Include WIR headers
#include <wir/wirvirtualregister.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class RV_RegV is the representation of virtual RV32I registers.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/
class RV_RegV final : public WIR_VirtualRegister
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for virtual registers.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV_RegV( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV_RegV( const RV_RegV & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV_RegV( RV_RegV && );

    /*!
      @brief Destructor.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    virtual ~RV_RegV( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV_RegV & operator = ( const RV_RegV & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be copied.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV_RegV & operator = ( RV_RegV && );


  protected:

    /*!
      @brief clone creates a copy of a virtual RV32I register.

      @return A pointer to the newly created virtual RV32I register.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    virtual RV_RegV *clone( void ) const;

};

}       // namespace WIR

#endif  // _RV_REGVIRTUAL_H
