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
  @file armv5te.h
  @brief This file provides the specific interface of the ARMv5TE instruction
         set architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _ARMV5TE_H
#define _ARMV5TE_H


//
// Include section
//

// Include WIR headers
#include <arch/arm/armv5t.h>
#include <arch/arm/armv5tepregphysical.h>
#include <arch/arm/armv5tepregvirtual.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class ARMv5TE models the %ARMv5TE instruction set architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class ARMv5TE : public ARMv5T
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for ARMv5TE-based processor architectures.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5TE( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5TE( const ARMv5TE & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5TE( ARMv5TE && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~ARMv5TE( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5TE & operator = ( const ARMv5TE & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv5TE & operator = ( ARMv5TE && );


    //
    // ARMv5TE-specific global initializations.
    //

    /*!
      @brief init performs some global initialization tasks for ARMv5TE-based
             processor architectures.

      This includes setting up the ARMv5TE machine operation formats and the
      assignment of valid operation formats to ARMv5TE opcodes.

      @note init shall be called globally by WIR_Init(). It shall only perform
            tasks that cannot be expressed as initializations of static class
            members (since the order of static initialization is unspecified in
            C++) and that thus require execution by active code.
    */
    static void init( void );


    //
    // Data structures used to model the ARMv5TE architecture.
    //

    /*!
      @brief The public members of class OpCode model the ARMv5TE opcodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OpCode : public ARMv5T::OpCode
    {

      public:

        //! Load Registers Doubleword
        static const OpCode LDRD;

        //! Move to Coprocessor from two ARM Registers
        static const OpCode MCRR;

        //! Move to two ARM Registers from Coprocessor
        static const OpCode MRRC;

        //! Preload Data
        static const OpCode PLD;

        //! Saturating Add
        static const OpCode QADD;

        //! Saturating Double and Add
        static const OpCode QDADD;

        //! Saturating Double and Subtract
        static const OpCode QDSUB;

        //! Saturating Subtract
        static const OpCode QSUB;

        //! Signed Multiply-Accumulate BB
        static const OpCode SMLABB;

        //! Signed Multiply-Accumulate BT
        static const OpCode SMLABT;

        //! Signed Multiply-Accumulate TB
        static const OpCode SMLATB;

        //! Signed Multiply-Accumulate TT
        static const OpCode SMLATT;

        //! Signed Multiply-Accumulate Long BB
        static const OpCode SMLALBB;

        //! Signed Multiply-Accumulate Long BT
        static const OpCode SMLALBT;

        //! Signed Multiply-Accumulate Long TB
        static const OpCode SMLALTB;

        //! Signed Multiply-Accumulate Long TT
        static const OpCode SMLALTT;

        //! Signed Multiply-Accumulate Word B
        static const OpCode SMLAWB;

        //! Signed Multiply-Accumulate Word T
        static const OpCode SMLAWT;

        //! Signed Multiply BB
        static const OpCode SMULBB;

        //! Signed Multiply BT
        static const OpCode SMULBT;

        //! Signed Multiply TB
        static const OpCode SMULTB;

        //! Signed Multiply TT
        static const OpCode SMULTT;

        //! Signed Multiply Word B
        static const OpCode SMULWB;

        //! Signed Multiply Word T
        static const OpCode SMULWT;

        //! Store Registers Doubleword
        static const OpCode STRD;


      protected:

        // Inherit the Constructors from ARMv5T::OpCode.
        using ARMv5T::OpCode::OpCode;

    };

    /*!
      @brief The public members of class RegisterType model the ARMv5TE register
             types.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class RegisterType : public ARMv5T::RegisterType
    {

      public:

        //! Register pairs.
        static const RegisterType pReg;


      protected:

        // Inherit the constructors from ARMv5T::RegisterType.
        using ARMv5T::RegisterType::RegisterType;

    };

    /*!
      @brief The public members of class OperationFormat model the different
             formats of ARMv5TE machine operations.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OperationFormat : public ARMv5T::OperationFormat
    {

      public:

        //
        // 32-bit ARM operation formats.
        //

        //! 32-bit cond, addrmode (p0-p15), opcode, reg (def), reg (def), string
        static const OperationFormat CAORRS_1;

        //! 32-bit cond, addrmode (p0-p15), opcode, reg (use), reg (use), string
        static const OperationFormat CAORRS_2;

        //! 32-bit cond, preg (def), addrmode (pre, post) [reg (defuse), addrmode (+/-) unsigned const8]
        static const OperationFormat CPARAC8_1;

        //! 32-bit cond, preg (use), addrmode (pre, post) [reg (defuse), addrmode (+/-) unsigned const8]
        static const OperationFormat CPARAC8_2;

        //! 32-bit cond, preg (def), addrmode (pre, post) [reg (defuse), addrmode (+/-) reg (use)]
        static const OperationFormat CPARAR_1;

        //! 32-bit cond, preg (use), addrmode (pre, post) [reg (defuse), addrmode (+/-) reg (use)]
        static const OperationFormat CPARAR_2;

        //! 32-bit cond, preg (def), [reg (use), addrmode (+/-) unsigned const8]
        static const OperationFormat CPRAC8_1;

        //! 32-bit cond, preg (use), [reg (use), addrmode (+/-) unsigned const8]
        static const OperationFormat CPRAC8_2;

        //! 32-bit cond, preg (def), [reg (use), addrmode (+/-) reg (use)]
        static const OperationFormat CPRAR_1;

        //! 32-bit cond, preg (use), [reg (use), addrmode (+/-) reg (use)]
        static const OperationFormat CPRAR_2;

        //! 32-bit [reg (use), addrmode (+/-) unsigned const12]
        static const OperationFormat RAC12_1;

        //! 32-bit [reg (use), addrmode (+/-) reg (use)]
        static const OperationFormat RAR_1;

        //! 32-bit [reg (use), addrmode (+/-) reg (use), rrx]
        static const OperationFormat RAR_2;

        //! 32-bit [reg (use), addrmode (+/-) reg (use), addrmode (lsr, asr) unsigned const6_0]
        static const OperationFormat RARAC60_1;

        //! 32-bit [reg (use), addrmode (+/-) reg (use), lsl unsigned const5]
        static const OperationFormat RARC5_1;

        //! 32-bit [reg (use), addrmode (+/-) reg (use), ror unsigned const5_0]
        static const OperationFormat RARC50_1;


      protected:

        // Inherit the constructors from ARMv5T::OperationFormat.
        using ARMv5T::OperationFormat::OperationFormat;

    };

    /*!
      @brief Access to physical register pair 0 (R0 + R1).
      @return A const reference to pair 0.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARMv5TE_PRegP &P0( void ) const;

    /*!
      @brief Access to physical register pair 2 (R2 + R3).
      @return A const reference to pair 2.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARMv5TE_PRegP &P2( void ) const;

    /*!
      @brief Access to physical register pair 4 (R4 + R5).
      @return A const reference to pair 4.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARMv5TE_PRegP &P4( void ) const;

    /*!
      @brief Access to physical register pair 6 (R6 + R7).
      @return A const reference to pair 6.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARMv5TE_PRegP &P6( void ) const;

    /*!
      @brief Access to physical register pair 8 (R8 + R9).
      @return A const reference to pair 8.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARMv5TE_PRegP &P8( void ) const;

    /*!
      @brief Access to physical register pair 10 (R10 + R11).
      @return A const reference to pair 10.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARMv5TE_PRegP &P10( void ) const;

    /*!
      @brief Access to physical register pair 12 (R12 + R13).
      @return A const reference to pair 12.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARMv5TE_PRegP &P12( void ) const;

    /*!
      @brief Access to physical register pair 14 (R14 + R15).
      @return A const reference to pair 14.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARMv5TE_PRegP &P14( void ) const;


  private:

    /*!
      @brief clone creates a copy of an ARMv5TE processor.

      @return A pointer to the newly created ARMv5TE copy.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseProcessor *clone( void ) const;

};

}       // namespace WIR

#endif  // _ARMV5TE_H
