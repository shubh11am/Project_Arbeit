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
  @file armbase.h
  @brief This file provides the interface that is common for all ARM-based
         architectures.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _ARMBASE_H
#define _ARMBASE_H


//
// Include section
//

// Include WIR headers
#include <wir/wirprocessor.h>

#include <arch/arm/armconst3coprocessoropcode.h>
#include <arch/arm/armconst3unsigned.h>
#include <arch/arm/armconst4coprocessoropcode.h>
#include <arch/arm/armconst5rotateamount.h>
#include <arch/arm/armconst5unsigned.h>
#include <arch/arm/armconst5unsigned0.h>
#include <arch/arm/armconst6unsigned0.h>
#include <arch/arm/armconst8unsigned.h>
#include <arch/arm/armconst10coprocessoroffset.h>
#include <arch/arm/armconst12unsigned.h>
#include <arch/arm/armconst24unsigned.h>
#include <arch/arm/armhiregphysical.h>
#include <arch/arm/armhiregvirtual.h>
#include <arch/arm/armio.h>
#include <arch/arm/armloregphysical.h>
#include <arch/arm/armloregvirtual.h>
#include <arch/arm/armregphysical.h>
#include <arch/arm/armregvirtual.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class ARM_Base models the general ARM architecture.

  Since the old architecture versions ARMv1x, ARMv2x and ARMv3x are deprecated,
  this class ARM_Base actually models the ARMv4 architecture.

  Specific other ARM variants like, e.g., the ARMv4T ISA are derived from this
  class by inheritance.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class ARM_Base : public WIR_Processor<ARM_Base>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for ARM-based processor architectures.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Base( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Base( const ARM_Base & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Base( ARM_Base && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~ARM_Base( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Base & operator = ( const ARM_Base & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Base & operator = ( ARM_Base && );


    //
    // ARM-specific global initializations.
    //

    /*!
      @brief init performs some global initialization tasks for ARM-based
             processor architectures.

      This includes setting up the common ARM machine operation formats and the
      assignment of valid operation formats to ARM opcodes.

      @note init shall be called globally by WIR_Init(). It shall only perform
            tasks that cannot be expressed as initializations of static class
            members (since the order of static initialization is unspecified in
            C++) and that thus require execution by active code.
    */
    static void init( void );


    //
    // Data structures used to model the general ARM architecture.
    //

    /*!
      @brief The public members of class AddressingMode model the ARM's general
             addressing modes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class AddressingMode : public WIR_Processor::AddressingMode
    {

      public:

        //! Logical shift left.
        static const AddressingMode lsl;

        //! Logical shift right.
        static const AddressingMode lsr;

        //! Arithmetic shift right.
        static const AddressingMode asr;

        //! Rotate right.
        static const AddressingMode ror;

        //! Address addition
        static const AddressingMode plus;

        //! Address subtraction
        static const AddressingMode minus;

        //! Pre-Indexed Addressing
        static const AddressingMode pre;

        //! Post-Indexed Addressing
        static const AddressingMode post;

        //! Increment After
        static const AddressingMode ia;

        //! Increment Before
        static const AddressingMode ib;

        //! Decrement After
        static const AddressingMode da;

        //! Decrement Before
        static const AddressingMode db;

        //! Full Descending
        static const AddressingMode fd;

        //! Empty Descending
        static const AddressingMode ed;

        //! Full Ascending
        static const AddressingMode fa;

        //! Empty Ascending
        static const AddressingMode ea;

        //! CPSR/SPSR Control Field
        static const AddressingMode c;

        //! CPSR/SPSR Extension Field
        static const AddressingMode x;

        //! CPSR/SPSR Status Field
        static const AddressingMode s;

        //! CPSR/SPSR Flags Field
        static const AddressingMode f;

        //! Coprocessor p0
        static const AddressingMode p0;

        //! Coprocessor p1
        static const AddressingMode p1;

        //! Coprocessor p2
        static const AddressingMode p2;

        //! Coprocessor p3
        static const AddressingMode p3;

        //! Coprocessor p4
        static const AddressingMode p4;

        //! Coprocessor p5
        static const AddressingMode p5;

        //! Coprocessor p6
        static const AddressingMode p6;

        //! Coprocessor p7
        static const AddressingMode p7;

        //! Coprocessor p8
        static const AddressingMode p8;

        //! Coprocessor p9
        static const AddressingMode p9;

        //! Coprocessor p10
        static const AddressingMode p10;

        //! Coprocessor p11
        static const AddressingMode p11;

        //! Coprocessor p12
        static const AddressingMode p12;

        //! Coprocessor p13
        static const AddressingMode p13;

        //! Coprocessor p14
        static const AddressingMode p14;

        //! Coprocessor p15
        static const AddressingMode p15;

        //! CPSR A Bit
        static const AddressingMode cpsra;

        //! CPSR F Bit
        static const AddressingMode cpsrf;

        //! CPSR I Bit
        static const AddressingMode cpsri;

        //! CPSR AF Bits
        static const AddressingMode cpsraf;

        //! CPSR AI Bits
        static const AddressingMode cpsrai;

        //! CPSR FI Bits
        static const AddressingMode cpsrfi;

        //! CPSR AFI Bits
        static const AddressingMode cpsrafi;

        //! Big Endianess
        static const AddressingMode be;

        //! Little Endianess
        static const AddressingMode le;

        //! Rotation Right by 0 bits
        static const AddressingMode ror0;

        //! Rotation Right by 8 bits
        static const AddressingMode ror8;

        //! Rotation Right by 16 bits
        static const AddressingMode ror16;

        //! Rotation Right by 24 bits
        static const AddressingMode ror24;

        /*!
          @brief getProcessorTypeName returns a string containing the C++-
                 mangled name of the ARM class to which an addressing mode
                 belongs.

          @return A string holding the ARM type name.

          This method is only used for comparing addressing modes of different
          processor architectures in WIR_Operation::checkParameters().

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual std::string getProcessorTypeName( void ) const;

        /*!
          @brief isCompatible returns whether one ARM addressing mode is
                 compatible with another.

          @param[in] t A const reference to the addressing mode whose
                       compatibility is to be checked.
          @param[in] f A const reference to the operation format whose
                       compatibility is to be checked.
          @param[in] o A const reference to the operation code whose
                       compatibility is to be checked.
          @return True iff two addressing modes are compatible, false otherwise.

          This compatibility check is used by WIR_Operation::checkParameters in
          order to verify that addressing mode parameters inserted into some
          %WIR operation actually match with the ARM operation formats.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isCompatible( const WIR_BaseProcessor::AddressingMode &,
                                   const WIR_BaseProcessor::OperationFormat &,
                                   const WIR_BaseProcessor::OpCode & ) const;


      protected:

        // Inherit the Constructors from WIR_Processor::AddressingMode.
        using WIR_Processor::AddressingMode::AddressingMode;

    };

    /*!
      @brief The public members of class Condition model the ARM's condition
             codes for predicated execution of machine operations.

      See also the ARM Architecture Reference Manual, Table A3-1, page A3-4.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class Condition : public WIR_Processor::Condition
    {

      public:

        /*!
          @brief Equal;
                 Z set.
        */
        static const Condition eq;

        /*!
          @brief Not equal;
                 Z clear.
        */
        static const Condition ne;

        /*!
          @brief Carry set/unsigned higher or same;
                 C set.
        */
        static const Condition hs;

        /*!
          @brief Carry clear/unsigned lower;
                 C clear.
        */
        static const Condition lo;

        /*!
          @brief Minus/negative;
                 N set
        */
        static const Condition mi;

        /*!
          @brief Plus/positive or zero;
                 N clear.
        */
        static const Condition pl;

        /*!
          @brief Overflow;
                 V set.
        */
        static const Condition vs;

        /*!
          @brief No overflow;
                 V clear.
        */
        static const Condition vc;

        /*!
          @brief Unsigned higher;
                 C set and Z clear.
        */
        static const Condition hi;

        /*!
          @brief Unsigned lower or same;
                 C clear or Z set.
        */
        static const Condition ls;

        /*!
          @brief Signed greater than or equal;
                 N set and V set, or N clear and V clear (N == V).
        */
        static const Condition ge;

        /*!
          @brief Signed less than;
                 N set and V clear, or N clear and V set (N != V).
        */
        static const Condition lt;

        /*!
          @brief Signed greater than;
                 Z clear, and either N set and V set, or N clear and V clear
                 (Z == 0, N == V).
        */
        static const Condition gt;

        /*!
          @brief Signed less than or equal;
                 Z set, or N set and V clear, or N clear and V set
                 (Z == 1 or N != V).
        */
        static const Condition le;

        /*!
          @brief Always (unconditional).
        */
        static const Condition al;

        /*!
          @brief getProcessorTypeName returns a string containing the C++-
                 mangled name of the ARM class to which a condition belongs.

          @return A string holding the ARM type name.

          This method is only used for comparing addressing modes of different
          processor architectures in WIR_Operation::checkParameters().

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual std::string getProcessorTypeName( void ) const;


      protected:

        // Inherit the Constructors from WIR_Processor::Condition.
        using WIR_Processor::Condition::Condition;

    };

    /*!
      @brief The public members of class OpCode model the ARM opcodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OpCode : public WIR_Processor::OpCode
    {

      public:

        //! Add with Carry
        static const OpCode ADC;

        //! Add
        static const OpCode ADD;

        //! And
        static const OpCode AND;

        //! Branch
        static const OpCode B;

        //! Branch and Link
        static const OpCode BL;

        //! Bit Clear
        static const OpCode BIC;

        //! Coprocessor Data Processing
        static const OpCode CDP;

        //! Compare Negative
        static const OpCode CMN;

        //! Compare
        static const OpCode CMP;

        //! Exclusive Or
        static const OpCode EOR;

        //! Load Coprocessor
        static const OpCode LDC;

        //! Load Multiple
        static const OpCode LDM;

        //! Load Register
        static const OpCode LDR;

        //! Load Register Byte
        static const OpCode LDRB;

        //! Load Register Byte with Translation
        static const OpCode LDRBT;

        //! Load Register Halfword
        static const OpCode LDRH;

        //! Load Register Signed Byte
        static const OpCode LDRSB;

        //! Load Register Signed Halfword
        static const OpCode LDRSH;

        //! Load Register with Translation
        static const OpCode LDRT;

        //! Move to Coprocessor from ARM Register
        static const OpCode MCR;

        //! Multiply Accumulate
        static const OpCode MLA;

        //! Move
        static const OpCode MOV;

        //! Move to ARM Register from Coprocessor
        static const OpCode MRC;

        //! Move PSR to General-Purpose Register
        static const OpCode MRS;

        //! Move to Status Register from ARM Register
        static const OpCode MSR;

        //! Multiply
        static const OpCode MUL;

        //! Move Not
        static const OpCode MVN;

        //! Logical Or
        static const OpCode ORR;

        //! Reverse Subtract
        static const OpCode RSB;

        //! Reverse Subtract with Carry
        static const OpCode RSC;

        //! Subtract with Carry
        static const OpCode SBC;

        //! Signed Multiply Accumulate Long
        static const OpCode SMLAL;

        //! Signed Multiply Long
        static const OpCode SMULL;

        //! Store Coprocessor
        static const OpCode STC;

        //! Store Register
        static const OpCode STR;

        //! Store Multiple
        static const OpCode STM;

        //! Store Register Byte
        static const OpCode STRB;

        //! Store Register Byte with Translation
        static const OpCode STRBT;

        //! Store Register Halfword
        static const OpCode STRH;

        //! Store Register with Translation
        static const OpCode STRT;

        //! Subtract
        static const OpCode SUB;

        //! Software Interrupt
        static const OpCode SWI;

        //! Swap word (deprecated from ARMv6 onwards)
        static const OpCode SWP;

        //! Swap byte (deprecated from ARMv6 onwards)
        static const OpCode SWPB;

        //! Test Equivalence
        static const OpCode TEQ;

        //! Test
        static const OpCode TST;

        //! Unsigned Multiply Accumulate Long
        static const OpCode UMLAL;

        //! Unsigned Multiply Long
        static const OpCode UMULL;

        /*!
          @brief isMove returns whether an opcode is a register-register move.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode is a move, false otherwise.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isMove( const WIR_Operation & ) const;

        /*!
          @brief isIndirectCall returns whether an opcode indirectly calls a
                 function.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode is an indirect function call, false
                  otherwise.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isIndirectCall( const WIR_Operation & ) const;

        /*!
          @brief isReturn returns whether an opcode returns from a function
                 call.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode is a function return, false otherwise.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isReturn( const WIR_Operation & ) const;

        /*!
          @brief isConditionalJump returns whether an opcode performs a
                 conditional jump.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode is a conditional jump, false otherwise.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isConditionalJump( const WIR_Operation & ) const;

        /*!
          @brief isUnconditionalJump returns whether an opcode performs an
                 unconditional jump.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode is an unconditional jump, false otherwise.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isUnconditionalJump( const WIR_Operation & ) const;

        /*!
          @brief isIndirectJump returns whether an opcode indirectly performs
                 a jump.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode is an indirect jump, false otherwise.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isIndirectJump( const WIR_Operation & ) const;


      protected:

        // Inherit the Constructors from WIR_Processor::OpCode.
        using WIR_Processor::OpCode::OpCode;

    };

    /*!
      @brief The public members of class RegisterType model the different types
             of ARM registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class RegisterType : public WIR_Processor::RegisterType
    {

      public:

        /*!
          @brief All general-purpose registers R0 - R15.

          The full register set is divided in two sub-groups, namely R0-R7 for
          the THUMB ISA and R8-R15. These sub-groups are defined below.
        */
        static const RegisterType reg;

        //! Low registers R0 - R7 for THUMB.
        static const RegisterType lo;

        //! High registers R8 - R15.
        static const RegisterType hi;

        /*!
          @brief isCompatible returns whether one ARM register type is
                 compatible with another.

          @param[in] t A const reference to the register type whose
                       compatibility is to be checked.
          @param[in] ra A const reference to an operation's actual register.
          @param[in] rf A const reference to a register as required by an
                        operation format.
          @return True iff two register types are compatible, false otherwise.

          This compatibility check is used by WIR_Operation::checkParameters. It
          ensures that lo or hi registers can be used as register parameters
          whenever the ARM operation format requires general registers. However,
          it prohibits the use of hi registers whenever lo registers are
          required, i.e., in THUMB operations, or vice versa.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isCompatible( const WIR_BaseProcessor::RegisterType &,
                                   const WIR_BaseRegister &,
                                   const WIR_BaseRegister & ) const;


      protected:

        // Inherit the constructors from WIR_Processor::RegisterType.
        using WIR_Processor::RegisterType::RegisterType;

    };

    /*!
      @brief The public members of class OperationFormat model the different
             formats of ARM machine operations.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OperationFormat : public WIR_Processor::OperationFormat
    {

      public:

        //
        // 32-bit ARM operation formats.
        //

        //! 32-bit cond, CPSR_, 4 x addrmode (c, x, s, f), unsigned const8, rotate_amount
        static const OperationFormat CAAAAC8RA_1;

        //! 32-bit cond, SPSR_, 4 x addrmode (c, x, s, f), unsigned const8, rotate_amount
        static const OperationFormat CAAAAC8RA_2;

        //! 32-bit cond, CPSR_, 4 x addrmode (c, x, s, f), reg (use)
        static const OperationFormat CAAAAR_1;

        //! 32-bit cond, SPSR_, 4 x addrmode (c, x, s, f), reg (use)
        static const OperationFormat CAAAAR_2;

        //! 32-bit cond, addrmode (p0-p15), opcode1, reg (def), string, string, opcode2
        static const OperationFormat CAORSSO_1;

        //! 32-bit cond, addrmode (p0-p15), opcode1, reg (use), string, string, opcode2
        static const OperationFormat CAORSSO_2;

        //! 32-bit cond, addrmode (p0-p15), opcode1, string, string, string, opcode2
        static const OperationFormat CAOSSSO;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def) }
        static const OperationFormat CARR1_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def) }
        static const OperationFormat CARR1_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def) (incl PC) }^
        static const OperationFormat CARR1_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def) (incl PC) }^
        static const OperationFormat CARR1_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def) (excl PC) }^
        static const OperationFormat CARR1_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use) }
        static const OperationFormat CARR1_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use) }
        static const OperationFormat CARR1_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use) }^
        static const OperationFormat CARR1_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def) }
        static const OperationFormat CARR2_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def) }
        static const OperationFormat CARR2_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR2_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR2_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR2_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use) }
        static const OperationFormat CARR2_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use) }
        static const OperationFormat CARR2_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use) }^
        static const OperationFormat CARR2_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def) }
        static const OperationFormat CARR3_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def) }
        static const OperationFormat CARR3_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR3_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR3_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR3_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use) }
        static const OperationFormat CARR3_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use) }
        static const OperationFormat CARR3_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR3_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR4_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def), reg (def) }
        static const OperationFormat CARR4_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR4_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR4_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR4_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR4_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR4_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR4_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR5_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR5_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR5_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR5_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR5_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR5_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR5_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR5_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR6_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR6_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR6_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR6_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR6_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR6_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR6_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR6_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR7_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR7_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR7_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR7_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR7_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR7_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR7_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR7_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR8_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR8_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR8_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR8_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR8_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR8_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR8_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR8_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR9_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR9_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR9_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR9_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR9_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR9_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR9_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR9_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR10_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR10_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR10_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR10_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR10_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR10_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR10_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR10_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR11_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR11_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR11_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR11_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR11_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR11_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR11_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR11_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR12_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR12_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR12_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR12_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR12_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR12_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR12_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR12_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR13_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR13_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR13_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR13_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR13_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR13_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR13_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR13_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR14_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR14_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR14_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR14_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR14_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR14_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR14_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR14_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR15_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR15_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR15_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR15_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (excl PC) }^
        static const OperationFormat CARR15_5;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR15_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR15_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR15_8;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR16_1;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), ref (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) }
        static const OperationFormat CARR16_2;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR16_3;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def), reg (def) (incl PC) }^
        static const OperationFormat CARR16_4;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR16_6;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (defuse)!, { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }
        static const OperationFormat CARR16_7;

        //! 32-bit cond, addrmode (ia, ib, da, db, fd, ed, fa, ea), reg (use), { reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use), reg (use) }^
        static const OperationFormat CARR16_8;

        //! 32-bit cond, addrmode (p0-p15), string, addrmode (pre, post) [reg (defuse), addrmode (+/-) const10_offset]
        static const OperationFormat CASARAC8_1;

        //! 32-bit L cond, addrmode (p0-p15), string, addrmode (pre, post) [reg (defuse), addrmode (+/-) const10_offset]
        static const OperationFormat CASARAC8_2;

        //! 32-bit cond, addrmode (p0-p15), string, [reg (use), addrmode (+/-) const10_offset]
        static const OperationFormat CASRAC8_1;

        //! 32-bit L cond, addrmode (p0-p15), string, [reg (use), addrmode (+/-) const10_offset]
        static const OperationFormat CASRAC8_2;

        //! 32-bit cond, addrmode (p0-p15), string, [reg (use)], { unsigned const8 }
        static const OperationFormat CASRC8_1;

        //! 32-bit L cond, addrmode (p0-p15), string, [reg (use)], { unsigned const8 }
        static const OperationFormat CASRC8_2;

        //! 32-bit cond, unsigned const24
        static const OperationFormat CC24;

        //! 32-bit cond, label
        static const OperationFormat CL;

        //! 32-bit cond, reg (def), CPSR
        static const OperationFormat CR_1;

        //! 32-bit cond, reg (def), SPSR
        static const OperationFormat CR_2;

        //! 32-bit cond, reg (def), addrmode (pre, post) [reg (defuse), addrmode (+/-) unsigned const12]
        static const OperationFormat CRARAC12_1;

        //! 32-bit cond, reg (use), addrmode (pre, post) [reg (defuse), addrmode (+/-) unsigned const12]
        static const OperationFormat CRARAC12_2;

        //! 32-bit cond, reg (def), addrmode (pre, post) [reg (defuse), addrmode (+/-) unsigned const8]
        static const OperationFormat CRARAC8_1;

        //! 32-bit cond, reg (use), addrmode (pre, post) [reg (defuse), addrmode (+/-) unsigned const8]
        static const OperationFormat CRARAC8_2;

        //! 32-bit cond, reg (def), addrmode (pre, post) [reg (defuse), addrmode (+/-) reg (use)]
        static const OperationFormat CRARAR_1;

        //! 32-bit cond, reg (def), addrmode (pre, post) [reg (defuse), addrmode (+/-) reg (use), rrx]
        static const OperationFormat CRARAR_2;

        //! 32-bit cond, reg (use), addrmode (pre, post) [reg (defuse), addrmode (+/-) reg (use)]
        static const OperationFormat CRARAR_3;

        //! 32-bit cond, reg (use), addrmode (pre, post) [reg (defuse), addrmode (+/-) reg (use), rrx]
        static const OperationFormat CRARAR_4;

        //! 32-bit cond, reg (def), addrmode (pre, post) [reg (defuse), addrmode (+/-) reg (use), addrmode (lsr, asr) unsigned const6_0]
        static const OperationFormat CRARARAC60_1;

        //! 32-bit cond, reg (use), addrmode (pre, post) [reg (defuse), addrmode (+/-) reg (use), addrmode (lsr, asr) unsigned const6_0]
        static const OperationFormat CRARARAC60_2;

        //! 32-bit cond, reg (def), addrmode (pre, post) [reg (defuse), addrmode (+/-) reg (use), ror unsigned const5_0]
        static const OperationFormat CRARARC50_1;

        //! 32-bit cond, reg (use), addrmode (pre, post) [reg (defuse), addrmode (+/-) reg (use), ror unsigned const5_0]
        static const OperationFormat CRARARC50_2;

        //! 32-bit cond, reg (def), addrmode (pre, post) [reg (defuse), addrmode (+/-) reg (use), lsl unsigned const5]
        static const OperationFormat CRARARC5_1;

        //! 32-bit cond, reg (use), addrmode (pre, post) [reg (defuse), addrmode (+/-) reg (use), lsl unsigned const5]
        static const OperationFormat CRARARC5_2;

        //! 32-bit cond, reg (def), unsigned const8, rotate_amount
        static const OperationFormat CRC8RA_1;

        //! 32-bit S cond, reg (def), unsigned const8, rotate_amount
        static const OperationFormat CRC8RA_2;

        //! 32-bit cond, reg (use), unsigned const8, rotate_amount
        static const OperationFormat CRC8RA_3;

        //! 32-bit cond, reg (def), reg (use)
        static const OperationFormat CRR_1;

        //! 32-bit S cond, reg (def), reg (use)
        static const OperationFormat CRR_2;

        //! 32-bit cond, reg (def), reg (use), rrx
        static const OperationFormat CRR_3;

        //! 32-bit S cond, reg (def), reg (use), rrx
        static const OperationFormat CRR_4;

        //! 32-bit cond, reg (use), reg (use)
        static const OperationFormat CRR_5;

        //! 32-bit cond, reg (use), reg (use), rrx
        static const OperationFormat CRR_6;

        //! 32-bit cond, reg (def), [reg (use), addrmode (+/-) unsigned const12]
        static const OperationFormat CRRAC12_1;

        //! 32-bit cond, reg (use), [reg (use), addrmode (+/-) unsigned const12]
        static const OperationFormat CRRAC12_2;

        //! 32-bit cond, reg (def), reg (use), addrmode (lsr, asr) unsigned const6_0
        static const OperationFormat CRRAC60_1;

        //! 32-bit S cond, reg (def), reg (use), addrmode (lsr, asr) unsigned const6_0
        static const OperationFormat CRRAC60_2;

        //! 32-bit cond, reg (use), reg (use), addrmode (lsr, asr) unsigned const6_0
        static const OperationFormat CRRAC60_3;

        //! 32-bit cond, reg (def), [reg (use), addrmode (+/-) unsigned const8]
        static const OperationFormat CRRAC8_1;

        //! 32-bit cond, reg (use), [reg (use), addrmode (+/-) unsigned const8]
        static const OperationFormat CRRAC8_2;

        //! 32-bit cond, reg (def), reg (use), addrmode (lsl, lsr, asr, ror) reg (use)
        static const OperationFormat CRRAR_1;

        //! 32-bit S cond, reg (def), reg (use), addrmode (lsl, lsr, asr, ror) reg (use)
        static const OperationFormat CRRAR_2;

        //! 32-bit cond, reg (def), [reg (use), addrmode (+/-) reg (use)]
        static const OperationFormat CRRAR_3;

        //! 32-bit cond, reg (def), [reg (use), addrmode (+/-) reg (use), rrx]
        static const OperationFormat CRRAR_4;

        //! 32-bit cond, reg (use), reg (use), addrmode (lsl, lsr, asr, ror) reg (use)
        static const OperationFormat CRRAR_5;

        //! 32-bit cond, reg (use), [reg (use), addrmode (+/-) reg (use)]
        static const OperationFormat CRRAR_6;

        //! 32-bit cond, reg (use), [reg (use), addrmode (+/-) reg (use), rrx]
        static const OperationFormat CRRAR_7;

        //! 32-bit cond, reg (def), [reg (use), addrmode (+/-) reg (use), addrmode (lsr, asr) unsigned const6_0]
        static const OperationFormat CRRARAC60_1;

        //! 32-bit cond, reg (use), [reg (use), addrmode (+/-) reg (use), addrmode (lsr, asr) unsigned const6_0]
        static const OperationFormat CRRARAC60_2;

        //! 32-bit cond, reg (def), [reg (use), addrmode (+/-) reg (use), ror unsigned const5_0]
        static const OperationFormat CRRARC50_1;

        //! 32-bit cond, reg (use), [reg (use), addrmode (+/-) reg (use), ror unsigned const5_0]
        static const OperationFormat CRRARC50_2;

        //! 32-bit cond, reg (def), [reg (use), addrmode (+/-) reg (use), lsl unsigned const5]
        static const OperationFormat CRRARC5_1;

        //! 32-bit cond, reg (use), [reg (use), addrmode (+/-) reg (use), lsl unsigned const5]
        static const OperationFormat CRRARC5_2;

        //! 32-bit cond, reg (def), reg (use), ror unsigned const5_0
        static const OperationFormat CRRC50_1;

        //! 32-bit S cond, reg (def), reg (use), ror unsigned const5_0
        static const OperationFormat CRRC50_2;

        //! 32-bit cond, reg (use), reg (use), ror unsigned const5_0
        static const OperationFormat CRRC50_3;

        //! 32-bit cond, reg (def), reg (use), lsl unsigned const5
        static const OperationFormat CRRC5_1;

        //! 32-bit S cond, reg (def), reg (use), lsl unsigned const5
        static const OperationFormat CRRC5_2;

        //! 32-bit cond, reg (use), reg (use), lsl unsigned const5
        static const OperationFormat CRRC5_3;

        //! 32-bit cond, reg (def), reg (use), unsigned const8, rotate_amount
        static const OperationFormat CRRC8RA_1;

        //! 32-bit S cond, reg (def), reg (use), unsigned const8, rotate_amount
        static const OperationFormat CRRC8RA_2;

        //! 32-bit cond, reg (def), reg (use), reg (use)
        static const OperationFormat CRRR_1;

        //! 32-bit S cond, reg (def), reg (use), reg (use)
        static const OperationFormat CRRR_2;

        //! 32-bit cond, reg (def), reg (use), reg (use), rrx
        static const OperationFormat CRRR_3;

        //! 32-bit S cond, reg (def), reg (use), reg (use), rrx
        static const OperationFormat CRRR_4;

        //! 32-bit cond, reg (def), reg (use), [ reg (use) ]
        static const OperationFormat CRRR_5;

        //! 32-bit cond, reg (def), reg (use), reg (use), addrmode (lsr, asr) unsigned const6_0
        static const OperationFormat CRRRAC60_1;

        //! 32-bit S cond, reg (def), reg (use), reg (use), addrmode (lsr, asr) unsigned const6_0
        static const OperationFormat CRRRAC60_2;

        //! 32-bit cond, reg (def), reg (use), reg (use), addrmode (lsl, lsr, asr, ror) reg (use)
        static const OperationFormat CRRRAR_1;

        //! 32-bit S cond, reg (def), reg (use), reg (use), addrmode (lsl, lsr, asr, ror) reg (use)
        static const OperationFormat CRRRAR_2;

        //! 32-bit cond, reg (def), reg (use), reg (use), ror unsigned const5_0
        static const OperationFormat CRRRC50_1;

        //! 32-bit S cond, reg (def), reg (use), reg (use), ror unsigned const5_0
        static const OperationFormat CRRRC50_2;

        //! 32-bit cond, reg (def), reg (use), reg (use), lsl unsigned const5
        static const OperationFormat CRRRC5_1;

        //! 32-bit S cond, reg (def), reg (use), reg (use), lsl unsigned const5
        static const OperationFormat CRRRC5_2;

        //! 32-bit cond, reg (def), reg (use), reg (use), reg (use)
        static const OperationFormat CRRRR_1;

        //! 32-bit S cond, reg (def), reg (use), reg (use), reg (use)
        static const OperationFormat CRRRR_2;

        //! 32-bit cond, reg (defuse), reg (defuse), reg (use), reg (use)
        static const OperationFormat CRRRR_3;

        //! 32-bit S cond, reg (defuse), reg (defuse), reg (use), reg (use)
        static const OperationFormat CRRRR_4;


      protected:

        // Inherit the constructors from WIR_Processor::OperationFormat.
        using WIR_Processor::OperationFormat::OperationFormat;

    };

    /*!
      @brief Access to low physical register R0.
      @return A const reference to R0.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_LoRegP &R0( void ) const;

    /*!
      @brief Access to low physical register R1.
      @return A const reference to R1.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_LoRegP &R1( void ) const;

    /*!
      @brief Access to low physical register R2.
      @return A const reference to R2.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_LoRegP &R2( void ) const;

    /*!
      @brief Access to low physical register R3.
      @return A const reference to R3.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_LoRegP &R3( void ) const;

    /*!
      @brief Access to low physical register R4.
      @return A const reference to R4.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_LoRegP &R4( void ) const;

    /*!
      @brief Access to low physical register R5.
      @return A const reference to R5.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_LoRegP &R5( void ) const;

    /*!
      @brief Access to low physical register R6.
      @return A const reference to R6.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_LoRegP &R6( void ) const;

    /*!
      @brief Access to low physical register R7.
      @return A const reference to R7.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_LoRegP &R7( void ) const;

    /*!
      @brief Access to high physical register R8.
      @return A const reference to R8.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_HiRegP &R8( void ) const;

    /*!
      @brief Access to high physical register R9.
      @return A const reference to R9.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_HiRegP &R9( void ) const;

    /*!
      @brief Access to high physical register R10.
      @return A const reference to R10.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_HiRegP &R10( void ) const;

    /*!
      @brief Access to high physical register R11.
      @return A const reference to R11.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_HiRegP &R11( void ) const;

    /*!
      @brief Access to high physical register R12.
      @return A const reference to R12.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_HiRegP &R12( void ) const;

    /*!
      @brief Access to high physical register R13.
      @return A const reference to R13.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_HiRegP &R13( void ) const;

    /*!
      @brief Access to high physical register R14.
      @return A const reference to R14.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_HiRegP &R14( void ) const;

    /*!
      @brief Access to high physical register R15.
      @return A const reference to R15.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_HiRegP &R15( void ) const;

    /*!
      @brief Access to stack pointer (alias R13).
      @return A const reference to R13.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_HiRegP &SP( void ) const;

    /*!
      @brief Access to link register (alias R14).
      @return A const reference to R14.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_HiRegP &LR( void ) const;

    /*!
      @brief Access to program counter (alias R15).
      @return A const reference to R15.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const ARM_HiRegP &PC( void ) const;

};

}       // namespace WIR

#endif  // _ARMBASE_H
