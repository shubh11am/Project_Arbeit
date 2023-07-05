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
  @file mips.h
  @brief This file provides the specific interface of the MIPS/SPIM
         architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _MIPS_H
#define _MIPS_H


//
// Include section
//

// Include WIR headers
#include <wir/wirprocessor.h>

#include <arch/generic/mipsimmediate5shamt.h>
#include <arch/generic/mipsimmediate16signed.h>
#include <arch/generic/mipsimmediate16unsigned.h>
#include <arch/generic/mipsio.h>
#include <arch/generic/mipsregphysical.h>
#include <arch/generic/mipsregvirtual.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class MIPS models a generic MIPS/SPIM processor architecture.

  The intention behind this class is not to model some real processor
  architecture, but instead to provide the %WIR library with an example of a
  simple and generic processor model for illustration, documentation and testing
  purposes.

  In order to model your very own processor architecture for the %WIR library,
  you should follow these steps:

  -# Set up a new directory under arch and integrate it into the %WIR build
     infrastructure, so that the build of this new processor model can be
     controlled by a configure switch.
  -# Create a class that inherits from WIR_Processor and that will hold your
     processor model. Equip your class with the (more or less empty) default
     constructors, the destructor, and assignment operators.
  -# Add an (empty) static init method to your class and have it invoked by
     WIR_Init. Register your processor model by calling
     WIR_Registry::registerProcessor at the end of your init method.
  -# Add condition fields to your processor model by inheriting from class
     WIR_Processor::Condition (this mechanism is not shown here for the
     trivial MIPS, but it is exactly the same as for the opcodes as described
     below).
  -# Add addressing modes to your processor model by inheriting from class
     WIR_Processor::AddressingMode (this mechanism is not shown here for the
     trivial MIPS, but it is exactly the same as for the opcodes as described
     next).
  -# Add opcodes to your processor model by inheriting from class
     WIR_Processor::OpCode. Declare public static members to your derived class
     for convenience so that you can access the opcodes in a readable and
     type-safe fashion. Create a separate cc file where you instantiate the
     declared opcodes by invoking the standard constructor of class OpCode and
     passing a string to it that denotes an opcode's readable mnemonic.
  -# Add register types to your processor model by inheriting from class
     WIR_Processor::RegisterType in the same way as above for the opcodes. When
     instantiating the register types in their cc file, add their corresponding
     bit widths and their pre-/suffixes for an assembler-compatible naming
     scheme.
  -# For each register type declared above, create a class modeling virtual
     registers of that type by inheriting from WIR_VirtualRegister. Equip your
     classes with the standard constructors, a destructor, and assignment
     operators. Start with non-hierarchical registers first, then proceed with
     hierarchical registers featuring sub-registers, if any.
  -# For each register type declared above, create a class modeling physical
     registers of that type by inheriting from WIR_PhysicalRegister. Make sure
     that classes WIR_BaseProcessor and your processor class are friends of the
     physical registers' classes. Again, start with the non-hierarchical
     registers first and continue with hierarchical registers next.
  -# Add physical registers to your processor class. For this purpose, add
     appropriate calls of method WIR_Processor::addPhReg( const std::string & )
     to the constructor of your processor class. Do this bottom-up so that you
     create non-hierarchical registers first, followed by hierarchical ones. Add
     convenience methods to your processor class allowing to access the physical
     registers.
  -# Add classes for the different kinds of immediate operands used by the new
     processor's ISA. This is done simply by inheriting from class
     WIR_SignedImmediateParameter or WIR_UnsignedImmediateParameter and setting
     the correct bit-widths.
  -# Add operation formats to your processor model by inheriting from class
     WIR_Processor::OperationFormat in the same way as above for the opcodes.
  -# Register the actual shapes of the operation formats (i.e. which parameters
     of which type in which order an operation may have) by adding corresponding
     calls of WIR_Registry::registerOperationFormat to the init method of your
     processor class.
  -# Next, assign the opcodes of your machine to only those operation formats
     that your ISA supports. Do so by adding corresponding calls of
     WIR_Registry::registerOpCode to the init method of your processor class.
  -# If the default %WIR stream I/O methods from wirio.h are insufficient to
     produce valid assembly output for your particular processor, create your
     very own processor-specific %WIR-dumping functions for operations,
     parameters, instructions, basic blocks etc. All processor-specific I/O
     functions have to be registered once by adding corresponding calls of
     WIR_Registry::registerXYZDumper() to the init method of your processor
     class (see beginning of method MIPS::init( void ). Furthermore, add an I/O
     manipulator for your processor architecture so that you can easily tell an
     I/O stream to dump assembly code for your architecture, cf.
     std::ostream &mips( std::ostream &os ).
  -# Support all individual steps described here by adding meaningful positive
     and negative test cases to a processor-specific testbench within your new
     directory, cf. arch/generic/tests.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class MIPS : public WIR_Processor<MIPS>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for MIPS/SPIM processor architectures.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS( const MIPS & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS( MIPS && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~MIPS( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS & operator = ( const MIPS & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    MIPS & operator = ( MIPS && );


    //
    // MIPS/SPIM-specific global initializations.
    //

    /*!
      @brief init performs some global initialization tasks for MIPS processor
             architectures.

      This includes setting up the MIPS machine operation formats and the
      assignment of valid operation formats to MIPS opcodes.

      @note init shall be called globally by WIR_Init(). It shall only perform
            tasks that cannot be expressed as initializations of static class
            members (since the order of static initialization is unspecified in
            C++) and that thus require execution by active code.
    */
    static void init( void );


    //
    // Data structures used to model the generic MIPS/SPIM architecture.
    //

    /*!
      @brief The public members of class OpCode model the MIPS opcodes.

      The opcodes modeled here are all 'true' opcodes, pseudo instructions are
      not modeled. Floating point operations are also not modeled for the sake
      of brevity.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OpCode : public WIR_Processor::OpCode
    {

      public:

        //! Add
        static const OpCode ADD;

        //! Add Immediate
        static const OpCode ADDI;

        //! Add Immediate Unsigned
        static const OpCode ADDIU;

        //! Add Unsigned
        static const OpCode ADDU;

        //! And
        static const OpCode AND;

        //! And Immediate
        static const OpCode ANDI;

        //! Branch on Equal
        static const OpCode BEQ;

        //! Branch on Not Equal
        static const OpCode BNE;

        //! Divide
        static const OpCode DIV;

        //! Divide Unsigned
        static const OpCode DIVU;

        //! Jump
        static const OpCode J;

        //! Jump and Link
        static const OpCode JAL;

        //! Jump and Link Register
        static const OpCode JALR;

        //! Jump Register
        static const OpCode JR;

        //! Load Byte
        static const OpCode LB;

        //! Load Byte Unsigned
        static const OpCode LBU;

        //! Load Halfword
        static const OpCode LH;

        //! Load Halfword Unsigned
        static const OpCode LHU;

        //! Load Upper Immediate
        static const OpCode LUI;

        //! Load Word
        static const OpCode LW;

        //! Move From Control
        static const OpCode MFCO;

        //! Move From Hi
        static const OpCode MFHI;

        //! Move From Lo
        static const OpCode MFLO;

        //! Multiply
        static const OpCode MULT;

        //! Multiply Unsigned
        static const OpCode MULTU;

        //! Nor
        static const OpCode NOR;

        //! Or
        static const OpCode OR;

        //! Or Immediate
        static const OpCode ORI;

        //! Store Byte
        static const OpCode SB;

        //! Store Halfword
        static const OpCode SH;

        //! Shift Left Logical
        static const OpCode SLL;

        //! Shift Left Logical Variable
        static const OpCode SLLV;

        //! Set Less Than
        static const OpCode SLT;

        //! Set Less Than Immediate
        static const OpCode SLTI;

        //! Set Less Than Immediate Unsigned
        static const OpCode SLTIU;

        //! Set Less Than Unsigned
        static const OpCode SLTU;

        //! Shift Right Arithmetic
        static const OpCode SRA;

        //! Shift Right Arithmetic Variable
        static const OpCode SRAV;

        //! Shift Right Logical
        static const OpCode SRL;

        //! Shift Right Logical Variable
        static const OpCode SRLV;

        //! Subtract
        static const OpCode SUB;

        //! Subtract Unsigned
        static const OpCode SUBU;

        //! Store Word
        static const OpCode SW;

        //! Xor
        static const OpCode XOR;

        //! Xor Immediate
        static const OpCode XORI;


      protected:

        // Inherit the Constructors from WIR_Processor::OpCode.
        using WIR_Processor::OpCode::OpCode;

    };

    /*!
      @brief The public members of class RegisterType model the different types
             of MIPS registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class RegisterType : public WIR_Processor::RegisterType
    {

      public:

        //! Integer registers.
        static const RegisterType reg;


      protected:

        // Inherit the constructors from WIR_Processor::RegisterType.
        using WIR_Processor::RegisterType::RegisterType;

    };

    /*!
      @brief The public members of class OperationFormat model the different
             formats of MIPS machine operations.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OperationFormat : public WIR_Processor::OperationFormat
    {

      public:

        //! 32-bit J-Format label
        static const OperationFormat L;

        //! 32-bit R-Format reg (def)
        static const OperationFormat R_1;

        //! 32-bit R-Format reg (use)
        static const OperationFormat R_2;

        //! 32-bit I-Format reg (def), signed immediate, reg (use)
        static const OperationFormat RIR_1;

        //! 32-bit I-Format reg (use), signed immediate, reg (use)
        static const OperationFormat RIR_2;

        //! 32-bit I-Format reg (def), unsigned immediate
        static const OperationFormat RIU;

        //! 32-bit I-Format reg (def), unsigned immediate, reg (use)
        static const OperationFormat RIUR;

        //! 32-bit R-Format reg (def), reg (use)
        static const OperationFormat RR_1;

        //! 32-bit R-Format reg (use), reg (use)
        static const OperationFormat RR_2;

        //! 32-bit I-Format reg (def), reg (use), signed immediate
        static const OperationFormat RRI;

        //! 32-bit I-Format reg (def), reg (use), unsigned immediate
        static const OperationFormat RRIU;

        //! 32-bit I-Format reg (use), reg (use), label
        static const OperationFormat RRL;

        //! 32-bit R-Format reg (def), reg (use), reg (use)
        static const OperationFormat RRR;

        //! 32-bit R-Format reg (def), reg (use), shamt
        static const OperationFormat RRS;


      protected:

        // Inherit the constructors from WIR_Processor::OperationFormat.
        using WIR_Processor::OperationFormat::OperationFormat;

    };

    /*!
      @brief Access to physical integer register $0 (zero).
      @return A const reference to $0.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r0( void ) const;

    /*!
      @brief Access to physical integer register $1 (assembler temporary).
      @return A const reference to $1.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r1( void ) const;

    /*!
      @brief Access to physical integer register $2 (function result).
      @return A const reference to $2.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r2( void ) const;

    /*!
      @brief Access to physical integer register $3 (function result).
      @return A const reference to $3.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r3( void ) const;

    /*!
      @brief Access to physical integer register $4 (function argument).
      @return A const reference to $4.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r4( void ) const;

    /*!
      @brief Access to physical integer register $5 (function argument).
      @return A const reference to $5.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r5( void ) const;

    /*!
      @brief Access to physical integer register $6 (function argument).
      @return A const reference to $6.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r6( void ) const;

    /*!
      @brief Access to physical integer register $7 (function argument).
      @return A const reference to $7.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r7( void ) const;

    /*!
      @brief Access to physical integer register $8 (temporaries).
      @return A const reference to $8.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r8( void ) const;

    /*!
      @brief Access to physical integer register $9 (temporaries).
      @return A const reference to $9.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r9( void ) const;

    /*!
      @brief Access to physical integer register $10 (temporaries).
      @return A const reference to $10.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r10( void ) const;

    /*!
      @brief Access to physical integer register $11 (temporaries).
      @return A const reference to $11.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r11( void ) const;

    /*!
      @brief Access to physical integer register $12 (temporaries).
      @return A const reference to $12.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r12( void ) const;

    /*!
      @brief Access to physical integer register $13 (temporaries).
      @return A const reference to $13.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r13( void ) const;

    /*!
      @brief Access to physical integer register $14 (temporaries).
      @return A const reference to $14.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r14( void ) const;

    /*!
      @brief Access to physical integer register $15 (temporaries).
      @return A const reference to $15.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r15( void ) const;

    /*!
      @brief Access to physical integer register $16 (saved temporaries).
      @return A const reference to $16.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r16( void ) const;

    /*!
      @brief Access to physical integer register $17 (saved temporaries).
      @return A const reference to $17.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r17( void ) const;

    /*!
      @brief Access to physical integer register $18 (saved temporaries).
      @return A const reference to $18.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r18( void ) const;

    /*!
      @brief Access to physical integer register $19 (saved temporaries).
      @return A const reference to $19.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r19( void ) const;

    /*!
      @brief Access to physical integer register $20 (saved temporaries).
      @return A const reference to $20.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r20( void ) const;

    /*!
      @brief Access to physical integer register $21 (saved temporaries).
      @return A const reference to $21.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r21( void ) const;

    /*!
      @brief Access to physical integer register $22 (saved temporaries).
      @return A const reference to $22.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r22( void ) const;

    /*!
      @brief Access to physical integer register $23 (saved temporaries).
      @return A const reference to $23.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r23( void ) const;

    /*!
      @brief Access to physical integer register $24 (temporaries).
      @return A const reference to $24.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r24( void ) const;

    /*!
      @brief Access to physical integer register $25 (temporaries).
      @return A const reference to $25.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r25( void ) const;

    /*!
      @brief Access to physical integer register $26 (reserved OS kernel).
      @return A const reference to $26.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r26( void ) const;

    /*!
      @brief Access to physical integer register $27 (reserved OS kernel).
      @return A const reference to $27.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r27( void ) const;

    /*!
      @brief Access to physical integer register $28 (global pointer).
      @return A const reference to $28.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r28( void ) const;

    /*!
      @brief Access to physical integer register $29 (stack pointer).
      @return A const reference to $29.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r29( void ) const;

    /*!
      @brief Access to physical integer register $30 (frame pointer).
      @return A const reference to $30.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r30( void ) const;

    /*!
      @brief Access to physical integer register $31 (return address).
      @return A const reference to $31.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const MIPS_RegP &r31( void ) const;

};

}       // namespace WIR

#endif  // _TC179x_H
