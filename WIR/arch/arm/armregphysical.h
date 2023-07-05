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
  @file armregphysical.h
  @brief This file provides the interface of physical ARM general-purpose
         registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _ARM_REGPHYSICAL_H
#define _ARM_REGPHYSICAL_H


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
  @brief Class ARM_RegP is the representation of physical ARM general-purpose
         registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class ARM_RegP final : public WIR_PhysicalRegister
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~ARM_RegP( void );


  private:

    friend class WIR_BaseProcessor;
    friend class ARM_Base;

    /*!
      @brief No standard construction allowed, users must use
             ARM_RegP( const std::string & ) instead.
    */
    ARM_RegP( void ) = delete;

    /*!
      @brief Default constructor for physical general-purpose registers.

      @param[in] __s A const reference to a string that holds the physical
                     register's name (without its specific pre-/suffix as
                     determined by the register's type).
      @param[in] __sp A Boolean denoting whether the physical register is the
                      stack pointer.

      This constructor is private so that folks cannot create and mess around
      with additional physical registers. Construction of physical registers is
      only allowed for class ARM_Base.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit ARM_RegP( const std::string &, bool = false );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_RegP( const ARM_RegP & );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_RegP & operator = ( const ARM_RegP & );

    /*!
      @brief clone creates a copy of a physical ARM general-purpose register.

      @return A pointer to the newly created physical ARM register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ARM_RegP *clone( void ) const;

};

}       // namespace WIR

#endif  // _ARM_REGPHYSICAL_H
