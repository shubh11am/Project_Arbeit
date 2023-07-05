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
  @file armv5t.h
  @brief This file provides the specific interface of the ARMv5T instruction set
         architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _ARMV5T_H
#define _ARMV5T_H


//
// Include section
//

// Include WIR headers
#include <arch/arm/armv4t.h>
#include <arch/arm/armconst16unsigned.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class ARMv5T models the %ARMv5T instruction set architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class ARMv5T : public ARMv4T
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for ARMv5T-based processor architectures.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5T( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5T( const ARMv5T & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5T( ARMv5T && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~ARMv5T( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5T & operator = ( const ARMv5T & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5T & operator = ( ARMv5T && );


    //
    // ARMv5T-specific global initializations.
    //

    /*!
      @brief init performs some global initialization tasks for ARMv5T-based
             processor architectures.

      This includes setting up the ARMv5T machine operation formats and the
      assignment of valid operation formats to ARMv5T opcodes.

      @note init shall be called globally by WIR_Init(). It shall only perform
            tasks that cannot be expressed as initializations of static class
            members (since the order of static initialization is unspecified in
            C++) and that thus require execution by active code.
    */
    static void init( void );


    //
    // Data structures used to model the ARMv5T architecture.
    //

    /*!
      @brief The public members of class OpCode model the ARMv5T opcodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OpCode : public ARMv4T::OpCode
    {

      public:

        //! Breakpoint
        static const OpCode BKPT;

        //! Branch with Link and Exchange
        static const OpCode BLX;

        //! Coprocessor Data Processing 2
        static const OpCode CDP2;

        //! Count Leading Zeros
        static const OpCode CLZ;

        //! Load Coprocessor 2
        static const OpCode LDC2;

        //! Move to Coprocessor from ARM Register 2
        static const OpCode MCR2;

        //! Store Coprocessor 2
        static const OpCode STC2;


      protected:

        // Inherit the Constructors from ARMv4T::OpCode.
        using ARMv4T::OpCode::OpCode;

    };

    /*!
      @brief The public members of class OperationFormat model the different
             formats of ARMv5T machine operations.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OperationFormat : public ARMv4T::OperationFormat
    {

      public:

        //
        // 32-bit ARM operation formats.
        //

        //! 32-bit addrmode (p0-p15), opcode1, reg (use), string, string, opcode2
        static const OperationFormat AORSSO_2;

        //! 32-bit addrmode (p0-p15), opcode1, string, string, string, opcode2
        static const OperationFormat AOSSSO;

        //! 32-bit addrmode (p0-p15), string, addrmode (pre, post) [reg (defuse), addrmode (+/-) const10_offset]
        static const OperationFormat ASARAC8_1;

        //! 32-bit L addrmode (p0-p15), string, addrmode (pre, post) [reg (defuse), addrmode (+/-) const10_offset]
        static const OperationFormat ASARAC8_2;

        //! 32-bit addrmode (p0-p15), string, [reg (use), addrmode (+/-) const10_offset]
        static const OperationFormat ASRAC8_1;

        //! 32-bit L addrmode (p0-p15), string, [reg (use), addrmode (+/-) const10_offset]
        static const OperationFormat ASRAC8_2;

        //! 32-bit addrmode (p0-p15), string, [reg (use)], { unsigned const8 }
        static const OperationFormat ASRC8_1;

        //! 32-bit L addrmode (p0-p15), string, [reg (use)], { unsigned const8 }
        static const OperationFormat ASRC8_2;

        //! 32-bit unsigned const16
        static const OperationFormat C16_1;

        //! 32-bit label
        static const OperationFormat L;


      protected:

        // Inherit the constructors from ARMv4T::OperationFormat.
        using ARMv4T::OperationFormat::OperationFormat;

    };


  private:

    /*!
      @brief clone creates a copy of an ARMv5T processor.

      @return A pointer to the newly created ARMv5T copy.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseProcessor *clone( void ) const;

};

}       // namespace WIR

#endif  // _ARMV5T_H
