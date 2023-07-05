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
  @file armv5tej.h
  @brief This file provides the specific interface of the ARMv5TEJ instruction
         set architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _ARMV5TEJ_H
#define _ARMV5TEJ_H


//
// Include section
//

// Include WIR headers
#include <arch/arm/armv5te.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class ARMv5TEJ models the %ARMv5TEJ instruction set architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class ARMv5TEJ : public ARMv5TE
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for ARMv5TEJ-based processor architectures.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5TEJ( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5TEJ( const ARMv5TEJ & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5TEJ( ARMv5TEJ && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~ARMv5TEJ( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5TEJ & operator = ( const ARMv5TEJ & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5TEJ & operator = ( ARMv5TEJ && );


    //
    // ARMv5TEJ-specific global initializations.
    //

    /*!
      @brief init performs some global initialization tasks for ARMv5TEJ-based
             processor architectures.

      This includes setting up the ARMv5TEJ machine operation formats and the
      assignment of valid operation formats to ARMv5TEJ opcodes.

      @note init shall be called globally by WIR_Init(). It shall only perform
            tasks that cannot be expressed as initializations of static class
            members (since the order of static initialization is unspecified in
            C++) and that thus require execution by active code.
    */
    static void init( void );


    //
    // Data structures used to model the ARMv5TEJ architecture.
    //

    /*!
      @brief The public members of class OpCode model the ARMv5TEJ opcodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OpCode : public ARMv5TE::OpCode
    {

      public:

        //! Branch and Change to Jazelle State
        static const OpCode BXJ;


      protected:

        // Inherit the Constructors from ARMv5TE::OpCode.
        using ARMv5TE::OpCode::OpCode;

    };


  private:

    /*!
      @brief clone creates a copy of an ARMv5TEJ processor.

      @return A pointer to the newly created ARMv5TEJ copy.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseProcessor *clone( void ) const;

};

}       // namespace WIR

#endif  // _ARMV5TEJ_H
