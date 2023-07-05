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
  @file mipsregvirtual.h
  @brief This file provides the interface of virtual MIPS integer registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _MIPS_REGVIRTUAL_H
#define _MIPS_REGVIRTUAL_H


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
  @brief Class MIPS_RegV is the representation of virtual MIPS integer
         registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class MIPS_RegV : public WIR_VirtualRegister
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for virtual integer registers.
    */
    MIPS_RegV( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_RegV( const MIPS_RegV & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_RegV( MIPS_RegV && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~MIPS_RegV( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_RegV & operator = ( const MIPS_RegV & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS_RegV & operator = ( MIPS_RegV && );


  protected:

    /*!
      @brief clone creates a copy of a virtual MIPS integer register.

      @return A pointer to the newly created virtual MIPS register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual MIPS_RegV *clone( void ) const;

};

}       // namespace WIR

#endif  // _MIPS_REGVIRTUAL_H
