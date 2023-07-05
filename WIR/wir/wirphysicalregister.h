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
  @file wirphysicalregister.h
  @brief This file provides the interface of physical %WIR registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_PHYSICALREGISTER_H
#define _WIR_PHYSICALREGISTER_H


//
// Include section
//

// Include standard headers
#include <string>

// Include WIR headers
#include <wir/wirregister.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_PhysicalRegister is the generic representation of physical
         machine registers.

  It is simply a full specialization of the template
  WIR_Register<DerivedRegisterClass, false> with forwarding of constructors and
  assignment operators only. The key implementation issues of physical registers
  can thus be found in the specialized template
  WIR_Register<DerivedRegisterClass, false>,

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_PhysicalRegister : public WIR_Register<WIR_PhysicalRegister, false>
{

  public:

    //
    // Destructor.
    //

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_PhysicalRegister( void );


  protected:

    //
    // Constructors.
    //

    /*!
      @brief Default constructor for physical registers.

      @param[in] __r A const reference to a register type to be used.
      @param[in] __s A const reference to a string that holds the physical
                     register's name (without its specific pre-/suffix as
                     determined by the register's type).
      @param[in] __sp A Boolean denoting whether the physical register is the
                      stack pointer.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_PhysicalRegister( const WIR_BaseProcessor::RegisterType &,
                          const std::string &, const bool = false );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      When copying a register that is inserted in some %WIR processor, the
      resulting copy will not be inserted in a processor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_PhysicalRegister( const WIR_PhysicalRegister & );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When copying a register that is inserted in some %WIR processor, the
      resulting copy will not be inserted in a processor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_PhysicalRegister & operator = ( const WIR_PhysicalRegister & );


  private:

    friend class WIR_Register<WIR_PhysicalRegister, false>;

};

}       // namespace WIR

#endif  // _WIR_PHYSICALREGISTER_H
