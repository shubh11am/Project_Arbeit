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
  @file tcpswbitphysical.h
  @brief This file provides the interface of physical TriCore Processor Status
         Word (PSW) bits.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_PSWBITPHYSICAL_H
#define _TC_PSWBITPHYSICAL_H


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
  @brief Class TC_PSWBit is the representation of physical TriCore Processor
         Status Word (PSW) bits.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_PSWBit final : public WIR_PhysicalRegister
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_PSWBit( void );


  private:

    friend class WIR_BaseProcessor;
    friend class TC13;

    /*!
      @brief No standard construction allowed, users must use
             TC_PSWBit( const std::string & ) instead.
    */
    TC_PSWBit( void ) = delete;

    /*!
      @brief Default constructor for physical Processor Status Word (PSW) bits.

      @param[in] __s A const reference to a string that holds the physical
                     register's name (without its specific pre-/suffix as
                     determined by the register's type).
      @param[in] __sp A Boolean denoting whether the physical register is the
                      stack pointer.

      This constructor is private so that folks cannot create and mess around
      with additional physical registers. Construction of physical registers is
      only allowed for class TC13.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_PSWBit( const std::string &, bool = false );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_PSWBit( const TC_PSWBit & );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_PSWBit & operator = ( const TC_PSWBit & );

    /*!
      @brief clone creates a copy of a physical TriCore Processor Status Word
             bit.

      @return A pointer to the newly created physical TriCore PSW bit.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual TC_PSWBit *clone( void ) const;

};

}       // namespace WIR

#endif  // _TC_PSWBITPHYSICAL_H
