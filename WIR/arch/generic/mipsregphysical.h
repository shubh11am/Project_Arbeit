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
  @file mipsregphysical.h
  @brief This file provides the interface of physical MIPS integer registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _MIPS_REGPHYSICAL_H
#define _MIPS_REGPHYSICAL_H


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
  @brief Class MIPS_RegP is the representation of physical MIPS integer
         registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class MIPS_RegP final : public WIR_PhysicalRegister
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~MIPS_RegP( void );


  private:

    friend class WIR_BaseProcessor;
    friend class MIPS;

    /*!
      @brief No standard construction allowed, users must use
             MIPS_RegP( const std::string & ) instead.
    */
    MIPS_RegP( void ) = delete;

    /*!
      @brief Default constructor for physical integer registers.

      @param[in] __s A const reference to a string that holds the physical
                     register's name (without its specific pre-/suffix as
                     determined by the register's type).
      @param[in] __sp A Boolean denoting whether the physical register is the
                      stack pointer.

      This constructor is private so that folks cannot create and mess around
      with additional physical registers. Construction of physical registers is
      only allowed for class MIPS.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit MIPS_RegP( const std::string &, bool = false );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_RegP( const MIPS_RegP & );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_RegP & operator = ( const MIPS_RegP & );

    /*!
      @brief clone creates a copy of a physical MIPS integer register.

      @return A pointer to the newly created physical MIPS register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual MIPS_RegP *clone( void ) const;

};

}       // namespace WIR

#endif  // _MIPS_REGPHYSICAL_H
