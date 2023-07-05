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
  @file armv6.h
  @brief This file provides the specific interface of the ARMv6 instruction set
         architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _ARMV6_H
#define _ARMV6_H


//
// Include section
//

// Include WIR headers
#include <arch/arm/armv5tej.h>
#include <arch/arm/armconst5satpos.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class ARMv6 models the %ARMv6 instruction set architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class ARMv6 : public ARMv5TEJ
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for ARMv6-based processor architectures.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv6( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv6( const ARMv6 & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv6( ARMv6 && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~ARMv6( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv6 & operator = ( const ARMv6 & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv6 & operator = ( ARMv6 && );


    //
    // ARMv6-specific global initializations.
    //

    /*!
      @brief init performs some global initialization tasks for ARMv6-based
             processor architectures.

      This includes setting up the ARMv6 machine operation formats and the
      assignment of valid operation formats to ARMv6 opcodes.

      @note init shall be called globally by WIR_Init(). It shall only perform
            tasks that cannot be expressed as initializations of static class
            members (since the order of static initialization is unspecified in
            C++) and that thus require execution by active code.
    */
    static void init( void );


    //
    // Data structures used to model the ARMv6 architecture.
    //

    /*!
      @brief The public members of class OpCode model the ARMv6 opcodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OpCode : public ARMv5TEJ::OpCode
    {

      public:

        //! Change Processor State
        static const OpCode CPS;

        //! Change Processor State Interrupt Disable
        static const OpCode CPSID;

        //! Change Processor State Interrupt Enable
        static const OpCode CPSIE;

        //! Copy
        static const OpCode CPY;

        //! Load Register Exclusive
        static const OpCode LDREX;

        //! Move to Coprocessor from two ARM Registers 2
        static const OpCode MCRR2;

        //! Move to two ARM Registers from Coprocessor 2
        static const OpCode MRRC2;

        //! Pack Halfword Bottom Top
        static const OpCode PKHBT;

        //! Pack Halfword Top Bottom
        static const OpCode PKHTB;

        //! Saturating Add of two 16-bit integers
        static const OpCode QADD16;

        //! Saturating Add of two 8-bit integers
        static const OpCode QADD8;

        //! Saturating Add and Subtract with Exchange
        static const OpCode QADDSUBX;

        //! Saturating Subtraction of two 16-bit integers
        static const OpCode QSUB16;

        //! Saturating Subtraction of two 8-bit integers
        static const OpCode QSUB8;

        //! Saturating Subtract and Add with Exchange
        static const OpCode QSUBADDX;

        //! Byte-Reverse Word
        static const OpCode REV;

        //! Byte-Reverse Packed Halfword
        static const OpCode REV16;

        //! Byte-Reverse Signed Halfword
        static const OpCode REVSH;

        //! Return From Exception
        static const OpCode RFE;

        //! Signed Add of two 16-bit integers
        static const OpCode SADD16;

        //! Signed Add of four 8-bit integers
        static const OpCode SADD8;

        //! Signed Add and Subtract with Exchange
        static const OpCode SADDSUBX;

        //! Select
        static const OpCode SEL;

        //! Set Endianess
        static const OpCode SETEND;

        //! Signed Halving Add of two 16-bit integers
        static const OpCode SHADD16;

        //! Signed Halving Add of four 8-bit integers
        static const OpCode SHADD8;

        //! Signed Halving Add and Subtract with Exchange
        static const OpCode SHADDSUBX;

        //! Signed Halving Subtract of two 16-bit integers
        static const OpCode SHSUB16;

        //! Signed Halving Subtract of four 8-bit integers
        static const OpCode SHSUB8;

        //! Signed Halving Subtract and Add with Exchange
        static const OpCode SHSUBADDX;

        //! Signed Multiply Accumulate Dual
        static const OpCode SMLAD;

        //! X-Bit Signed Multiply Accumulate Dual
        static const OpCode SMLADX;

        //! Signed Multiply Accumulate Long Dual
        static const OpCode SMLALD;

        //! X-Bit Signed Multiply Accumulate Long Dual
        static const OpCode SMLALDX;

        //! Signed Multiply Subtract Accumulate Dual
        static const OpCode SMLSD;

        //! X-Bit Signed Multiply Subtract Accumulate Dual
        static const OpCode SMLSDX;

        //! Signed Multiply Subtract Accumulate Long Dual
        static const OpCode SMLSLD;

        //! X-Bit Signed Multiply Subtract Accumulate Long Dual
        static const OpCode SMLSLDX;

        //! Signed Most Significant Word Multiply Accumulate
        static const OpCode SMMLA;

        //! R-Bit Signed Most Significant Word Multiply Accumulate
        static const OpCode SMMLAR;

        //! Signed Most Significant Word Multiply Subtract
        static const OpCode SMMLS;

        //! R-Bit Signed Most Significant Word Multiply Subtract
        static const OpCode SMMLSR;

        //! Signed Most Significant Word Multiply
        static const OpCode SMMUL;

        //! R-Bit Signed Most Significant Word Multiply
        static const OpCode SMMULR;

        //! Signed Dual Multiply Add
        static const OpCode SMUAD;

        //! X-Bit Signed Dual Multiply Add
        static const OpCode SMUADX;

        //! Signed Dual Multiply Subtract
        static const OpCode SMUSD;

        //! X-Bit Signed Dual Multiply Subtract
        static const OpCode SMUSDX;

        //! Store Return State
        static const OpCode SRS;

        //! Signed Saturate
        static const OpCode SSAT;

        //! Signed Saturate of two 16-bit integers
        static const OpCode SSAT16;

        //! Signed Subtract of two 16-bit integers
        static const OpCode SSUB16;

        //! Signed Subtract of four 8-bit integers
        static const OpCode SSUB8;

        //! Signed Subtract and Add with Exchange
        static const OpCode SSUBADDX;

        //! Store Register Exclusive
        static const OpCode STREX;

        //! Extract 8-bit value from register and Add
        static const OpCode SXTAB;

        //! Extract two 8-bit values from register and Add
        static const OpCode SXTAB16;

        //! Extract 16-bit value from register and Add
        static const OpCode SXTAH;

        //! Extract 8-bit value from register
        static const OpCode SXTB;

        //! Extract two 8-bit values from register
        static const OpCode SXTB16;

        //! Extract 16-bit value from register
        static const OpCode SXTH;

        //! Unsigned Add of two 16-bit unsigned integers
        static const OpCode UADD16;

        //! Unsigned Add of four 8-bit unsigned integers
        static const OpCode UADD8;

        //! Unsigned Add and Subtract with Exchange
        static const OpCode UADDSUBX;

        //! Unsigned Halving Add of two 16-bit unsigned integers
        static const OpCode UHADD16;

        //! Unsigned Halving Add of four 8-bit unsigned integers
        static const OpCode UHADD8;

        //! Unsigned Halving Add and Subtract with Exchange
        static const OpCode UHADDSUBX;

        //! Unsigned Halving Subtract of two 16-bit unsigned integers
        static const OpCode UHSUB16;

        //! Unsigned Halving Subtract of four 8-bit unsigned integers
        static const OpCode UHSUB8;

        //! Unsigned Halving Subtract and Add with Exchange
        static const OpCode UHSUBADDX;

        //! Unsigned Multiply Accumulate Long
        static const OpCode UMAAL;

        //! Unsigned Saturating Add of two 16-bit unsigned integers
        static const OpCode UQADD16;

        //! Unsigned Saturating Add of four 8-bit unsigned integers
        static const OpCode UQADD8;

        //! Unsigned Saturating Add and Subtract with Exchange
        static const OpCode UQADDSUBX;

        //! Unsigned Saturating Subtract of two 16-bit unsigned integers
        static const OpCode UQSUB16;

        //! Unsigned Saturating Subtract of four 8-bit unsigned integers
        static const OpCode UQSUB8;

        //! Unsigned Saturating Subtract and Add with Exchange
        static const OpCode UQSUBADDX;

        //! Unsigned Sum of Absolute Differences of four 8-bit unsigned integers
        static const OpCode USAD8;

        //! Unsigned Sum of Absolute Differences and Accumulate of four 8-bit unsigned integers
        static const OpCode USADA8;

        //! Unsigned Saturate
        static const OpCode USAT;

        //! Unsigned Saturate of two 16-bit integers
        static const OpCode USAT16;

        //! Unsigned Subtract of two 16-bit unsigned integers
        static const OpCode USUB16;

        //! Unsigned Subtract of four 8-bit unsigned integers
        static const OpCode USUB8;

        //! Unsigned Subtract and Add with Exchange
        static const OpCode USUBADDX;

        //! Extract unsigned 8-bit value from register and Add
        static const OpCode UXTAB;

        //! Extract two unsigned 8-bit values from register and Add
        static const OpCode UXTAB16;

        //! Extract unsigned 16-bit value from register and Add
        static const OpCode UXTAH;

        //! Extract unsigned 8-bit value from register
        static const OpCode UXTB;

        //! Extract two unsigned 8-bit values from register
        static const OpCode UXTB16;

        //! Extract unsigned 16-bit value from register
        static const OpCode UXTH;


      protected:

        // Inherit the Constructors from ARMv5TEJ::OpCode.
        using ARMv5TEJ::OpCode::OpCode;

    };

    /*!
      @brief The public members of class OperationFormat model the different
             formats of ARMv5T machine operations.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OperationFormat : public ARMv5TEJ::OperationFormat
    {

      public:

        //
        // 32-bit ARM operation formats.
        //

        //! 32-bit addrmode (cpsra, cpsrf, cpsri, cpsraf, cpsrai, cpsrfi, cpsrafi)
        static const OperationFormat A_1;

        //! 32-bit addrmode (be, le)
        static const OperationFormat A_2;

        //! 32-bit addrmode (cpsra, cpsrf, cpsri, cpsraf, cpsrai, cpsrfi, cpsrafi), unsigned const5
        static const OperationFormat AC5_1;

        //! 32-bit addrmode (ia, ib, da, db, fd, ed, fa, ea), unsigned const5
        static const OperationFormat AC5_2;

        //! 32-bit addrmode (ia, ib, da, db, fd, ed, fa, ea), unsigned const5!
        static const OperationFormat AC5_3;

        //! 32-bit addrmode (p0-p15), opcode, reg (def), reg (def), string
        static const OperationFormat AORRS_1;

        //! 32-bit addrmode (p0-p15), opcode, reg (use), reg (use), string
        static const OperationFormat AORRS_2;

        //! 32-bit addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use)
        static const OperationFormat AR_1;

        //! 32-bit addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!
        static const OperationFormat AR_2;

        //! 32-bit unsigned const5
        static const OperationFormat C5_1;

        //! 32-bit cond, reg (def), unsigned const4, reg (use)
        static const OperationFormat CRC4R_1;

        //! 32-bit cond, reg (def), unsigned const5 satpos, reg (use)
        static const OperationFormat CRC5SPR_1;

        //! 32-bit cond, reg (def), unsigned const6 0, reg (use)
        static const OperationFormat CRC60R_1;

        //! 32-bit cond, reg (def), unsigned const5, reg (use), lsl unsigned const5
        static const OperationFormat CRC5RC5_1;

        //! 32-bit cond, reg (def), unsigned const5, reg (use), asr unsigned const6_0
        static const OperationFormat CRC5RC60_1;

        //! 32-bit cond, reg (def), unsigned const6_0, reg (use), lsl unsigned const5
        static const OperationFormat CRC60RC5_1;

        //! 32-bit cond, reg (def), unsigned const6_0, reg (use), asr unsigned const6_0
        static const OperationFormat CRC60RC60_1;

        //! 32-bit cond, reg (def), [reg (use)]
        static const OperationFormat CRR_7;

        //! 32-bit cond, reg (def), reg (use), addrmode (ror0, ror8, ror16, ror24)
        static const OperationFormat CRRA_1;

        //! 32-bit cond, reg (def), reg (use), reg (use), addrmode (ror0, ror8, ror16, ror24)
        static const OperationFormat CRRRA_1;

        //! 32-bit cond, reg (def), reg (use), reg (use), asr unsigned const5
        static const OperationFormat CRRRC5_3;

        //! 32-bit cond, reg (def), reg (def), reg (use), reg (use)
        static const OperationFormat CRRRR_5;


        //
        // 16-bit THUMB operation formats.
        //

        //! 16-bit addrmode (cpsra, cpsrf, cpsri, cpsraf, cpsrai, cpsrfi, cpsrafi)
        static const OperationFormat TA_1;

        //! 16-bit addrmode (be, le)
        static const OperationFormat TA_2;


      protected:

        // Inherit the constructors from ARMv5TEJ::OperationFormat.
        using ARMv5TEJ::OperationFormat::OperationFormat;

    };


  private:

    /*!
      @brief clone creates a copy of an ARMv6 processor.

      @return A pointer to the newly created ARMv6 copy.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseProcessor *clone( void ) const;

};

}       // namespace WIR

#endif  // _ARMV6_H
