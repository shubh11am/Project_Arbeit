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
  @file wirbaseregister.h
  @brief This file provides the basic interface of generic %WIR registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_BASEREGISTER_H
#define _WIR_BASEREGISTER_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <string>
#include <vector>

// Include WIR headers
#include <wir/API/wiridapi.h>
#include <wir/wirmisc.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_BaseRegister provides the basic functionality that each %WIR
         register must have.

  This class serves as virtual base class from which generic or actual registers
  are derived.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_BaseRegister : public WIR_ID_API
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for registers.

      @param[in] __r A const reference to a register type to be used.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_BaseRegister( const WIR_BaseProcessor::RegisterType & );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseRegister( const WIR_BaseRegister & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseRegister( WIR_BaseRegister && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_BaseRegister( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseRegister & operator = ( const WIR_BaseRegister & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o A const reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseRegister & operator = ( WIR_BaseRegister && );


    //
    // Type handling.
    //

    /*!
      @brief getType gets a register's actual type.

      @return A reference to the register's type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseProcessor::RegisterType &getType( void ) const;

    /*!
      @brief getBitWidth returns a register's bit width.

      @return The register's bit width.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getBitWidth( void ) const;


    //
    // Ownership handling.
    //

    /*!
      @brief isInserted returns whether this object is inserted into some
             %WIR object.

      @return true if the object is inserted, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isInserted( void ) const = 0;


    //
    // Virtual register handling.
    //

    /*!
      @brief isPhysical returns whether a register is physical.

      @return true if the register is physical, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isPhysical( void ) const;

    /*!
      @brief isVirtual returns whether a register is virtual.

      @return true if the register is virtual, false otherwise.

      Since the distinction between physical and virtual registers is realized
      in derived class, isVirtual is a pure virtual method.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isVirtual( void ) const = 0;


    //
    // Name handling.
    //

    /*!
      @brief getName returns a register's specific name.

      @return A string that holds the register's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getName( void ) const;


    //
    // Register hierarchy handling.
    //

    /*!
      @brief containsChild returns whether a register contains a %WIR child
             register with the specified ID.

      @param[in] id An object's ID to be found.
      @return true if a register contains an object with the given ID, false
              otherwise.

      Since the implementation details of this method depend on the distinction
      between physical and virtual registers, containsChild is a pure virtual
      method.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool containsChild( WIR_id_t id ) const = 0;

    /*!
      @brief hasChilds returns whether a register has child registers.

      @return true if the register has childs, false otherwise.

      Since the implementation details of this method depend on the distinction
      between physical and virtual registers, hasChilds is a pure virtual
      method.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool hasChilds( void ) const = 0;

    /*!
      @brief isChild returns whether a register is child of some parent
             register, i.e., whether it is part of a register hierarchy.

      @return true if the register is child of some parent, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isChild( void ) const;

    /*!
      @brief isChild returns whether this register is a child of the specified
             register in the register hierarchy.

      @param[in] r A const reference to another register.
      @return true if this register is child of the specified register, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isChildOf( const WIR_BaseRegister & ) const;

    /*!
      @brief isHierarchical returns whether a register is part of a register
             hierarchy, i.e., whether it has some childs or is a child.

      @return true if the register is hierarchical, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isHierarchical( void ) const;

    /*!
      @brief getParent returns a register's parent register.

      @return A reference to the parent register, or a reference to itself if it
              has no parent.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseRegister &getParent( void ) const;

    /*!
      @brief getRoot returns the root of a complex register hierarchy that this
             register is part of.

      @return A reference to the root register, or a reference to itself if it
              the root itself.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseRegister &getRoot( void ) const;

    /*!
      @brief getLeafs returns all leafs of the register hierarchy rooted by this
             object, i.e., all registers that have no childs.

      @return A vector containing all leaf registers in little-endian order. If
              the current register has no childs, the returned vector contains
              only the current register.

      Leaf registers are returned in little-endian order. I.e., the
      least-significant leaf register is the first element of the returned
      vector.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::vector<std::reference_wrapper<WIR_BaseRegister>> getLeafs( void ) const;


  protected:

    //! mParent points to the parent register that this one is child of.
    WIR_BaseRegister *mParent;

    /*!
      @brief mName holds a register's name (without its specific pre-/suffix as
             determined by the register's type).
    */
    std::string mName;


  private:

    friend class WIR_OperationFormat;

    /*!
      @brief No standard construction allowed, users must use
             WIR_BaseRegister( const WIR_BaseProcessor::RegisterType & )
             instead.
    */
    WIR_BaseRegister( void ) = delete;

    /*!
      @brief clone creates a copy of a %WIR register.

      @return A pointer to the newly created copy of this register.

      Since the implementation details of this method depend on the distinction
      between physical and virtual registers, clone is a pure virtual method.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseRegister *clone( void ) const = 0;

    //! mType points to a register's actual type.
    WIR_BaseProcessor::RegisterType *mType;

};

}       // namespace WIR

#endif  // _WIR_BASEREGISTER_H
