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
  @file tcpregphysical.h
  @brief This file provides the interface of physical TriCore extended address
         registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_PREGPHYSICAL_H
#define _TC_PREGPHYSICAL_H


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
  @brief Class TC_PRegP is the representation of physical TriCore extended
         address registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_PRegP final : public WIR_PhysicalRegister
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_PRegP( void );


  private:

    friend class WIR_BaseProcessor;
    friend class TC13;

    /*!
      @brief No standard construction allowed, users must use
             TC_PRegP( const std::string & ) instead.
    */
    TC_PRegP( void ) = delete;

    /*!
      @brief Default constructor for physical extended address registers.

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
    explicit TC_PRegP( const std::string &, bool = false );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_PRegP( const TC_PRegP & );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_PRegP & operator = ( const TC_PRegP & );

    /*!
      @brief Upon creation of TriCore physical register pairs, addChilds adds
             the two specified physical registers as childs.

      @param[in] c1 A const reference to the first child.
      @param[in] c2 A const reference to the second child.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addChilds( const WIR_PhysicalRegister &,
                    const WIR_PhysicalRegister & );

    /*!
      @brief clone creates a copy of a physical TriCore extended address
             register.

      @return A pointer to the newly created physical TriCore register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual TC_PRegP *clone( void ) const;

};

}       // namespace WIR

#endif  // _TC_PREGPHYSICAL_H
