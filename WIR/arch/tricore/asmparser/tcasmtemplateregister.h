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
  @file tcasmtemplateregister.h
  @brief This file provides the interface of template register arguments.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_ASMTEMPLATEREGISTER_H
#define _TC_ASMTEMPLATEREGISTER_H


//
// Include section
//

// Include standard headers
#include <memory>

// Include local headers
#include "tcasmregister.h"


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseRegister;

class TC_AsmRegisterInitializer;


/*!
  @brief Class TC_AsmTemplateRegister represents template register arguments for
         an assembly operation.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_AsmTemplateRegister final : public TC_AsmRegister
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an assembly argument for a given %WIR
             register and type.

      @param[in] init A pointer to a register initializer.

      This constructor takes over the ownership of the given pointer. The object
      pointed to will automatically be deleted during destruction of this
      template register argument.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_AsmTemplateRegister( TC_AsmRegisterInitializer * );

    /*!
      @brief Copy constructor.

      @param[in] r A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmTemplateRegister( const TC_AsmTemplateRegister & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_AsmTemplateRegister( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmTemplateRegister & operator = ( const TC_AsmTemplateRegister & );


    //
    // Type management.
    //

    /*!
      @brief isCompatible returns whether a template register is compatible with
             a given argument type.

      @param[in] t A specifier denoting the argument's type.
      @return true if the template register is compatible with t's type, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isCompatible( Type ) const;


    //
    // Register management.
    //

    /*!
      @brief getRegister returns the %WIR register represented by a template
             assembly argument.

      @return A reference to the actual %WIR register modeled by this class.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual const WIR_BaseRegister &getRegister( void ) const;

    /*!
      @brief setEReg specifies that the %WIR register backing this template
             argument is an extended register.

      @param[in] e A Boolean defaulting to true specifying whether an extended
                   register will be used or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setEReg( bool = true );

    /*!
      @brief getChildRegister determines the ith child register of this template
             register argument.

      @param[in] i The index of the child register to be looked up.
      @return A pointer to a newly created assembly code register argument. The
              caller of getChildRegister is reponsible for the proper deletion
              of the object returned by getChildRegister.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmRegister *getChildRegister( unsigned int );


  protected:

    /*!
      @brief clone creates a copy of a template register argument.

      @return A pointer to the newly created copy of this template register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual TC_AsmTemplateRegister *clone( void ) const;


  private:

    struct templateRegisterData
    {

      /*!
        @brief Default constructor for template register data using a given
               register initializer.

        @param[in] init A pointer to a register initializer.

        This constructor takes over the ownership of the given pointer. The
        object pointed to will automatically be deleted during destruction of
        this struct.

        @author Heiko Falk <Heiko.Falk@tuhh.de>
      */
      explicit templateRegisterData( TC_AsmRegisterInitializer * );

      //! mIsEReg denotes whether a template register is an extended register.
      bool mIsEReg;

      /*!
        @brief mRegister points to the %WIR register backing this template
               register argument.
      */
      WIR_BaseRegister *mRegister;

      /*!
        @brief mRegisterInitializer holds a (smart) pointer to a %WIR register
               initializer.
      */
      std::unique_ptr<TC_AsmRegisterInitializer> mRegisterInitializer;

    };

    /*!
      @brief mTemplateRegisterData is a smart pointer to a template register's
             internal data that can be shared among various copies of a template
             register.
    */
    std::shared_ptr<templateRegisterData> mTemplateRegisterData;

};

}       // namespace WIR

#endif  // _TC_ASMTEMPLATEREGISTER_H
