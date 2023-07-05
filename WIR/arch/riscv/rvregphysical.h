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
  @file rvregphysical.h
  @brief This file provides the interface of physical RISC-V RV32I registers.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/


#ifndef _RV_REGPHYSICAL_H
#define _RV_REGPHYSICAL_H


//
// Include section
//

// Include standard headers
#include <string>

// Include WIR headers
#include <wir/wirphysicalregister.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class RV_RegP is the representation of physical RV32I registers.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/
class RV_RegP final : public WIR_PhysicalRegister
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Destructor.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    virtual ~RV_RegP( void );


  private:

    friend class WIR_BaseProcessor;
    friend class RV32I;
    friend class RV32IC;
    friend class RV32IM;
    friend class RV32IMC;

    /*!
      @brief No standard construction allowed, users must use
             RV_RegP( const std::string & ) instead.
    */
    RV_RegP( void ) = delete;

    /*!
      @brief Default constructor for physical RV32I registers.

      @param[in] __s A const reference to a string that holds the physical
                     register's name.
      @param[in] __sp A Boolean denoting whether the physical register is the
                      stack pointer.

      This constructor is private so that folks cannot create and mess around
      with additional physical registers. Construction of physical registers is
      only allowed for class RV32I.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    explicit RV_RegP( const std::string &, bool = false );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV_RegP( const RV_RegP & );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV_RegP & operator = ( const RV_RegP & );

    /*!
      @brief clone creates a copy of a physical RV32I register.

      @return A pointer to the newly created physical RV32I register.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    virtual RV_RegP *clone( void ) const;

};

}       // namespace WIR

#endif  // _RV_REGPHYSICAL_H
