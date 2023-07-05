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
  @file wirvirtualregister.h
  @brief This file provides the interface of virtual %WIR registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_VIRTUALREGISTER_H
#define _WIR_VIRTUALREGISTER_H


//
// Include section
//

// Include WIR headers
#include <wir/wirregister.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_VirtualRegister is the generic representation of virtual
         machine registers.

  It is simply a full specialization of the template
  WIR_Register<DerivedRegisterClass, true> with forwarding of constructors and
  assignment operators only. The key implementation issues of virtual registers
  can thus be found in the specialized template
  WIR_Register<DerivedRegisterClass, true>.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_VirtualRegister : public WIR_Register<WIR_VirtualRegister, true>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for virtual registers.

      @param[in] __r A const reference to a register type to be used.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_VirtualRegister( const WIR_BaseProcessor::RegisterType & );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      When copying a register that is inserted in some %WIR function, the
      resulting copy will not be inserted in a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_VirtualRegister( const WIR_VirtualRegister & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a register that is inserted in some %WIR function results
      in an assertion, since you are not allowed to move a register whose
      ownership is managed by a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_VirtualRegister( WIR_VirtualRegister && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_VirtualRegister( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When copying a register that is inserted in some %WIR function, the
      resulting copy will not be inserted in a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_VirtualRegister & operator = ( const WIR_VirtualRegister & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a register that is inserted in some %WIR function results
      in an assertion, since you are not allowed to move a register whose
      ownership is managed by a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_VirtualRegister & operator = ( WIR_VirtualRegister && );

};

}       // namespace WIR

#endif  // _WIR_VIRTUALREGISTER_H
