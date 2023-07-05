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
  @file tcasmaddress.h
  @brief This file provides the interface of address arguments.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_ASMADDRESS_H
#define _TC_ASMADDRESS_H


//
// Include section
//

// Include standard headers
#include <memory>
#include <string>

// Include local headers
#include "tcasmargument.h"
#include "tcasmregister.h"


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseRegister;


/*!
  @brief Class TC_AsmAddress represents addressing arguments for an assembly
         operation.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_AsmAddress final : public TC_AsmArgument
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an address argument for a given
             register argument, offset and type.

      @param[in] r A pointer to a register argument.
      @param[in] o A signed long long value denoting the address offset.
      @param[in] t A specifier denoting the argument's type.

      This constructor takes over the ownership of the given pointer. The object
      pointed to will automatically be deleted during destruction of this
      address argument.

      The constructor throws std::invalid_argument if the offset is too large
      for the given argument type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmAddress( TC_AsmRegister *, long long, Type );

    /*!
      @brief Default constructor creating an address argument of type AMODE_BASE
             for a given %WIR register and offset.

      @param[in] r A const reference to a %WIR register.
      @param[in] o A signed long long value denoting the address offset.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmAddress( const WIR_BaseRegister &, long long );

    /*!
      @brief Default constructor creating an address argument of types
             AMODE_BASEHILAB or AMODE_BASELOLAB for a given register argument
             and label.

      @param[in] r A pointer to a register argument.
      @param[in] t A specifier denoting the argument's type.
      @param[in] l A const reference to a label's name.

      This constructor takes over the ownership of the given pointer. The object
      pointed to will automatically be deleted during destruction of this
      address argument.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmAddress( TC_AsmRegister *, Type, const std::string & );

    /*!
      @brief Copy constructor.

      @param[in] a A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmAddress( const TC_AsmAddress & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_AsmAddress( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmAddress & operator = ( const TC_AsmAddress & );


    //
    // Type management.
    //

    /*!
      @brief isCompatible returns whether an address is compatible with a given
             argument type.

      @param[in] t A specifier denoting the argument's type.
      @return true if the address is compatible with t's type, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isCompatible( Type ) const;


    //
    // Register management.
    //

    /*!
      @brief getRegister returns the register represented by an assembly
             address argument.

      @return A reference to the actual %WIR register modeled by this class.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_BaseRegister &getRegister( void ) const;

    /*!
      @brief getOffset returns an assembly address argument's offset.

      @return A signed long long value denoting the address offset.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    long long getOffset( void ) const;


    //
    // Value management.
    //

    /*!
      @brief getName returns the label's name.

      @return The label's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getName( void ) const;


  protected:

    /*!
      @brief clone creates a copy of an address argument.

      @return A pointer to the newly created copy of this address.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual TC_AsmAddress *clone( void ) const;


  private:

    /*!
      @brief No standard construction allowed, users must use one of the other
             constructors above instead.
    */
    TC_AsmAddress( void ) = delete;

    /*!
      @brief mRegister1 holds a (smart) pointer to an address' first involved
             register.
    */
    std::unique_ptr<TC_AsmRegister> mRegister1;

    /*!
      @brief mRegister2 holds a (smart) pointer to an address' second involved
             register (only used for bit-reverse and circular addressing modes
             where pairs of neighboring address registers are used).
    */
    std::unique_ptr<TC_AsmRegister> mRegister2;

    //! mOffset stores the address offset.
    long long mOffset;

    //! mName holds a label name.
    std::string mName;

};

}       // namespace WIR

#endif  // _TC_ASMADDRESS_H
