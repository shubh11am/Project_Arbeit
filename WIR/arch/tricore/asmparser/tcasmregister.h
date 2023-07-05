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
  @file tcasmregister.h
  @brief This file provides the interface of register assembly arguments.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_ASMREGISTER_H
#define _TC_ASMREGISTER_H


//
// Include section
//

// Include local headers
#include "tcasmargument.h"


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseRegister;


/*!
  @brief Class TC_AsmRegister represents register arguments for an assembly
         operation.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_AsmRegister : public TC_AsmArgument
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty register argument.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmRegister( void );

    /*!
      @brief Default constructor creating an assembly argument for a given %WIR
             register and type.

      @param[in] r A const reference to a %WIR register.
      @param[in] t A specifier denoting the argument's type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmRegister( const WIR_BaseRegister &, Type );

    /*!
      @brief Copy constructor.

      @param[in] r A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmRegister( const TC_AsmRegister & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_AsmRegister( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmRegister & operator = ( const TC_AsmRegister & );


    //
    // Type management.
    //

    /*!
      @brief isCompatible returns whether a register is compatible with a given
             argument type.

      @param[in] t A specifier denoting the argument's type.
      @return true if the register is compatible with t's type, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isCompatible( Type ) const;


    //
    // Register management.
    //

    /*!
      @brief getRegister returns the register represented by an assembly
             operation argument.

      @return A reference to the actual %WIR register modeled by this class.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual const WIR_BaseRegister &getRegister( void ) const;


  protected:

    /*!
      @brief clone creates a copy of a register argument.

      @return A pointer to the newly created copy of this register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual TC_AsmRegister *clone( void ) const;


  private:

    //! mRegister points to the actual %WIR register represented by this class.
    const WIR_BaseRegister *mRegister;

};

}       // namespace WIR

#endif  // _TC_ASMREGISTER_H
