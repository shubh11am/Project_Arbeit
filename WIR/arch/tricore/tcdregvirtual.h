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
  @file tcdregvirtual.h
  @brief This file provides the interface of virtual TriCore data registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_DREGVIRTUAL_H
#define _TC_DREGVIRTUAL_H


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
  @brief Class TC_DRegV is the representation of virtual TriCore data registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_DRegV : public WIR_VirtualRegister
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for virtual data registers.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_DRegV( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_DRegV( const TC_DRegV & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_DRegV( TC_DRegV && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_DRegV( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_DRegV & operator = ( const TC_DRegV & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_DRegV & operator = ( TC_DRegV && );


  protected:

    /*!
      @brief clone creates a copy of a virtual TriCore data register.

      @return A pointer to the newly created virtual TriCore register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual TC_DRegV *clone( void ) const;

};

}       // namespace WIR

#endif  // _TC_DREGVIRTUAL_H
