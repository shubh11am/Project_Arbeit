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
  @file wirliveout.h
  @brief This file provides the interface of a %WIR container representing sets
         of registers that are live-out at instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_LIVEOUT_H
#define _WIR_LIVEOUT_H


//
// Include section
//

// Include WIR headers
#include <wir/wircontainer.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseRegister;


/*!
  @brief Class WIR_LiveOut models sets of live-out registers for instructions.

  WIR_LiveOut containers are attached to WIR_Instruction objects during lifeness
  analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_LiveOut : public WIR_Container<WIR_LiveOut>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LiveOut( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LiveOut( const WIR_LiveOut & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LiveOut( WIR_LiveOut && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_LiveOut( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LiveOut & operator = ( const WIR_LiveOut & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LiveOut & operator = ( WIR_LiveOut && );

    /*!
      @brief isUnique returns whether live-out sets are unique, i.e., whether at
             most one instance of this container type can be attached to a %WIR
             class.

      @return Always true, live-out sets are unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;


    //
    // Register set handling.
    //

    /*!
      @brief insertRegister adds a new register to set mRegisterReferences.

      @param[in] o A const reference to the register to be added.

      @details A (wrapped) reference to o is added to the set.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertRegister( const WIR_BaseRegister & );

    /*!
      @brief eraseRegister removes the specified register from set
             mRegisterReferences.

      @param[in] o A const reference to the register to be erased.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void eraseRegister( const WIR_BaseRegister & );

    /*!
      @brief clearRegisters removes all elements from set mRegisterReferences.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearRegisters( void );

    /*!
      @brief getRegisters returns the set mRegisterReferences.

      @return A const reference to the set mRegisterReferences.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_RegisterSet &getRegisters( void ) const;

    /*!
      @brief begin returns an iterator to the first register of a live-out set.

      @return A const iterator pointing to the first register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterSet::const_iterator begin( void ) const;

    /*!
      @brief end returns an iterator to the end of a live-out set.

      @return A const iterator pointing to the position after the last register
              of a live-out set.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterSet::const_iterator end( void ) const;

    /*!
      @brief containsRegister returns whether set mRegisterReferences contains
             a register with the specified ID.

      @param[in] id An object's ID to be found.
      @return true if mRegisterReferences contains an object with the given ID,
              false otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            set's size. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsRegister( WIR_id_t ) const;

    /*!
      @brief containsRegister returns whether set mRegisterReferences contains
             the specified register.

      @param[in] o A const reference to the register to be found.
      @return true if mRegisterReferences contains the specified object, false
              otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            set's size. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsRegister( const WIR_BaseRegister & ) const;

    /*!
      @brief findRegister finds a register with the specified ID in set
             mRegisterReferences.

      @param[in] id An object's ID to be found.
      @return An iterator pointing to the found element with the specified ID,
              or the end() iterator otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            set's size. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterSet::const_iterator findRegister( WIR_id_t ) const;

    /*!
      @brief findRegister finds the specified register in set
             mRegisterReferences.

      @param[in] o A const reference to the register to be found.
      @return An iterator pointing to the found element, or the end() iterator
              otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            set's size. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterSet::const_iterator findRegister( const WIR_BaseRegister & ) const;


  private:

    //! mRegisterReferences holds the set of live-out registers.
    WIR_RegisterSet mRegisterReferences;

};

}       // namespace WIR

#endif  // _WIR_LIVEOUT_H
