/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file rv32i.h
  @brief This file provides the specific interface of the RISC-V RV32I Base
         Integer instruction set, version 2.0.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/


#ifndef _RV32I_H
#define _RV32I_H


//
// Include section
//

// Include WIR headers
#include <wir/wirprocessor.h>

#include <arch/riscv/rvconst5unsigned.h>
#include <arch/riscv/rvconst12signed.h>
#include <arch/riscv/rvconst20unsigned.h>
#include <arch/riscv/rvio.h>
#include <arch/riscv/rvregphysical.h>
#include <arch/riscv/rvregvirtual.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class RV32I models the RISC-V RV32I Base Integer instruction set
         architecture, version 2.0.

  Specific other RISC-V variants like, e.g., the RV32IC or RV32IM ISAs are
  derived from this class by inheritance.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/
class RV32I : public WIR_Processor<RV32I>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for RV32I processor architectures.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32I( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32I( const RV32I & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32I( RV32I && );

    /*!
      @brief Destructor.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    virtual ~RV32I( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32I & operator = ( const RV32I & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32I & operator = ( RV32I && );


    //
    // RV32I-specific global initializations.
    //

    /*!
      @brief init performs some global initialization tasks for RV32I processor
             architectures.

      This includes setting up the assignment of valid operation formats to
      RV32I opcodes.

      @note init shall be called globally by WIR_Init(). It shall only perform
            tasks that cannot be expressed as initializations of static class
            members (since the order of static initialization is unspecified in
            C++) and that thus require execution by active code.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    static void init( void );


    //
    // Data structures used to model the RISC-V RV32I V2.0 architecture.
    //

    /*!
      @brief The public members of class OpCode model the RV32I's opcodes.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    class OpCode : public WIR_Processor::OpCode
    {

      public:

        //! Addition
        static const OpCode ADD;

        //! Add Immediate
        static const OpCode ADDI;

        //! Logical AND
        static const OpCode AND;

        //! Logical AND Immediate
        static const OpCode ANDI;

        //! Add Upper Immediate to PC
        static const OpCode AUIPC;

        //! Branch if Equal
        static const OpCode BEQ;

        //! Branch if Greater Than or Equal
        static const OpCode BGE;

        //! Branch if Greater Than or Equal Unsigned
        static const OpCode BGEU;

        //! Branch if Less Than
        static const OpCode BLT;

        //! Branch if Less Than Unsigned
        static const OpCode BLTU;

        //! Branch if Not Equal
        static const OpCode BNE;

        //! Atomic Read and Clear Bits in CSR
        static const OpCode CSRRC;

        //! Atomic Read and Clear Bits in CSR Immediate
        static const OpCode CSRRCI;

        //! Atomic Read and Set Bits in CSR
        static const OpCode CSRRS;

        //! Atomic Read and Set Bits in CSR Immediate
        static const OpCode CSRRSI;

        //! Atomic Read/Write CSR
        static const OpCode CSRRW;

        //! Atomic Read/Write CSR Immediate
        static const OpCode CSRRWI;

        //! Environment Break
        static const OpCode EBREAK;

        //! Environment Call
        static const OpCode ECALL;

        // Synchronization between RISC-V harts
        // Since this instruction is not clearly defined in the RISC-V ISA
        // manual, it is not implemented here.
        // static const OpCode FENCE;

        // Synchronization between instruction and data streams
        // Since this instruction is not clearly defined in the RISC-V ISA
        // manual, it is not implemented here.
        // static const OpCode FENCEI;

        //! Jump And Link (unconditional jump)
        static const OpCode J;

        //! Jump And Link (function call)
        static const OpCode JAL;

        //! Jump And Link Register
        static const OpCode JALR;

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

        //! Move pseudo-operation, de facto realized by an ADDI.
        static const OpCode MOV;

        //! Logical OR
        static const OpCode OR;

        //! Logical OR Immediate
        static const OpCode ORI;

        //! Store Byte
        static const OpCode SB;

        //! Store Halfword
        static const OpCode SH;

        //! Shift Left Logical
        static const OpCode SLL;

        //! Shift Left Logical Immediate
        static const OpCode SLLI;

        //! Set Less Than
        static const OpCode SLT;

        //! Set Less Than Immediate
        static const OpCode SLTI;

        //! Set Less Than Immediate Unsigned
        static const OpCode SLTIU;

        //! Set Less Than Unsigned
        static const OpCode SLTU;

        //! Shift Right Arithmetical
        static const OpCode SRA;

        //! Shift Right Arithmetical Immediate
        static const OpCode SRAI;

        //! Shift Right Logical
        static const OpCode SRL;

        //! Shift Right Logical Immediate
        static const OpCode SRLI;

        //! Subtraction
        static const OpCode SUB;

        //! Store Word
        static const OpCode SW;

        //! Logical XOR
        static const OpCode XOR;

        //! Logical XOR Immediate
        static const OpCode XORI;


      protected:

        // Inherit the Constructors from WIR_Processor::OpCode.
        using WIR_Processor::OpCode::OpCode;

    };

    /*!
      @brief The public members of class RegisterType model the different types
             of RISC-V registers.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    class RegisterType : public WIR_Processor::RegisterType
    {

      public:

        //! All general-purpose registers x0 - x31 (x0 hardwired to constant 0).
        static const RegisterType reg;


      protected:

        // Inherit the constructors from WIR_Processor::RegisterType.
        using WIR_Processor::RegisterType::RegisterType;

    };

    /*!
      @brief The public members of class OperationFormat model the RV32I's
             different formats of machine operations.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    class OperationFormat : public WIR_Processor::OperationFormat
    {

      public:

        //! 32-bit label
        static const OperationFormat L_1;

        //! 32-bit without operands
        static const OperationFormat NULL_1;

        //! 32-bit reg (def), reg (use), signed const12
        static const OperationFormat RC12R_1;

        //! 32-bit reg (use), reg (use), signed const12
        static const OperationFormat RC12R_2;

        //! 32-bit reg (def), unsigned const20
        static const OperationFormat RC20_1;

        //! 32-bit reg (def), label
        static const OperationFormat RL_1;

        //! 32-bit reg (def), %hi(label)
        static const OperationFormat RL_2;

        //! 32-bit reg (def), %lo(label), reg (use)
        static const OperationFormat RLR_1;

        //! 32-bit reg (use), %lo(label), reg (use)
        static const OperationFormat RLR_2;

        //! 32-bit reg (def), reg (use), unsigned const5
        static const OperationFormat RRC5_1;

        //! 32-bit reg (def), reg (use), signed const12
        static const OperationFormat RRC12_1;

        //! 32-bit reg (use), reg(use), label
        static const OperationFormat RRL_1;

        //! 32-bit reg (def), reg (use), %lo(label)
        static const OperationFormat RRL_2;

        //! 32-bit reg (def), reg (use)
        static const OperationFormat RR_1;

        //! 32-bit reg (def), reg (use), reg (use)
        static const OperationFormat RRR_1;

        /*!
          @brief 32-bit reg (def), string, unsigned const5

          The string parameter is allowed to be one of "frm", "fcsr", "fflags",
          "instret", "instreth", "cycle", "cycleh", "time" or "timeh".
        */
        static const OperationFormat RSC5_1;

        /*!
          @brief 32-bit reg (def), string, reg (use)

          The string parameter is allowed to be one of "frm", "fcsr", "fflags",
          "instret", "instreth", "cycle", "cycleh", "time" or "timeh".
        */
        static const OperationFormat RSR_1;


      protected:

        // Inherit the constructors from WIR_Processor::OperationFormat.
        using WIR_Processor::OperationFormat::OperationFormat;

    };

    /*!
      @brief Access to physical general-purpose x0.
      @return A const reference to x0.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &x0( void ) const;

    /*!
      @brief Access to physical general-purpose register x1.
      @return A const reference to x1.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &x1( void ) const;

    /*!
      @brief Access to physical general-purpose register x2.
      @return A const reference to x2.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &x2( void ) const;

    /*!
      @brief Access to physical general-purpose register x3.
      @return A const reference to x3.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &x3( void ) const;

    /*!
      @brief Access to physical general-purpose register x4.
      @return A const reference to x4.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &x4( void ) const;

    /*!
      @brief Access to physical general-purpose register x5.
      @return A const reference to x5.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &x5( void ) const;

    /*!
      @brief Access to physical general-purpose register x6.
      @return A const reference to x6.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &x6( void ) const;

    /*!
      @brief Access to physical general-purpose register x7.
      @return A const reference to x7.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &x7( void ) const;

    /*!
      @brief Access to physical general-purpose register x8.
      @return A const reference to x8.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &x8( void ) const;

    /*!
      @brief Access to physical general-purpose register x9.
      @return A const reference to x9.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
   const RV_RegP &x9( void ) const;

    /*!
      @brief Access to physical general-purpose register x10.
      @return A const reference to x10.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &x10( void ) const;

    /*!
      @brief Access to physical general-purpose register x11.
      @return A const reference to x11.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &x11( void ) const;

    /*!
      @brief Access to physical general-purpose register x12.
      @return A const reference to x12.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &x12( void ) const;

    /*!
      @brief Access to physical general-purpose register x13.
      @return A const reference to x13.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &x13( void ) const;

    /*!
      @brief Access to physical general-purpose register x14.
      @return A const reference to x14.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x14( void ) const;

   /*!
     @brief Access to physical general-purpose register x15.
     @return A const reference to x15.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x15( void ) const;

   /*!
     @brief Access to physical general-purpose register x16.
     @return A const reference to x16.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x16( void ) const;

   /*!
     @brief Access to physical general-purpose register x17.
     @return A const reference to x17.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x17( void ) const;

   /*!
     @brief Access to physical general-purpose register x18.
     @return A const reference to x18.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x18( void ) const;

   /*!
     @brief Access to physical general-purpose register x19.
     @return A const reference to x19.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x19( void ) const;

   /*!
     @brief Access to physical general-purpose register x20.
     @return A const reference to x20.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x20( void ) const;

   /*!
     @brief Access to physical general-purpose register x21.
     @return A const reference to x21.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x21( void ) const;

   /*!
     @brief Access to physical general-purpose register x22.
     @return A const reference to x22.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x22( void ) const;

   /*!
     @brief Access to physical general-purpose register x23.
     @return A const reference to x23.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x23( void ) const;

   /*!
     @brief Access to physical general-purpose register x24.
     @return A const reference to x24.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x24( void ) const;

   /*!
     @brief Access to physical general-purpose register x25.
     @return A const reference to x25.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x25( void ) const;

   /*!
     @brief Access to physical general-purpose register x26.
     @return A const reference to x26.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x26( void ) const;

   /*!
     @brief Access to physical general-purpose register x27.
     @return A const reference to x27.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x27( void ) const;

   /*!
     @brief Access to physical general-purpose register x28.
     @return A const reference to x28.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x28( void ) const;

   /*!
     @brief Access to physical general-purpose register x29.
     @return A const reference to x29.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x29( void ) const;

   /*!
     @brief Access to physical general-purpose register x30.
     @return A const reference to x30.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x30( void ) const;

   /*!
     @brief Access to physical general-purpose register x31.
     @return A const reference to x31.
     @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
   */
   const RV_RegP &x31( void ) const;

    /*!
      @brief Access to return address pointer.
      @return A const reference to x1.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &RA( void ) const;

    /*!
      @brief Access to stack pointer.
      @return A const reference to x2.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    const RV_RegP &SP( void ) const;

    /*!
      @brief isX0 checks whether the specified register is the RISC-V's x0
             register which is always zero.

      @param[in] r A const reference to a register to be checked.
      @return true if the specified register is physical and is x0 or if it is
              virtual and precolored with x0, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isX0( const WIR_BaseRegister & );

    /*!
      @brief isSP checks whether the specified register is the RISC-V's stack
             pointer.

      @param[in] r A const reference to a register to be checked.
      @return true if the specified register is physical and is the stack
              pointer or if it is virtual and precolored with x2, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isSP( const WIR_BaseRegister & );

    /*!
      @brief adjustStack allocates additional space in the specified function's
             stack frame and adjusts all stack-related memory accesses
             accordingly.

      @param[in,out] f A reference to a %WIR function.
      @param[in] size An integer denoting the number of bytes by which the stack
                      frame is extended.
      @param[in] insertedSpillCode A const reference to a list containing all
                                   spill instructions inside f generated so far.
                                   The instructions in insertedSpillCode will
                                   not be stack-adjusted, since it is presumed
                                   that their stack offsets determined during
                                   register allocation are OK.

      According to the RISC-V ABI (section 2.1, Integer Calling Convention), the
      stack grows downwards (towards lower addresses) and the stack pointer
      shall be aligned to a 128-bit boundary upon procedure entry. The first
      argument passed on the stack is located at offset zero of the stack
      pointer on function entry; following arguments are stored at
      correspondingly higher addresses.

      In the standard ABI, the stack pointer must remain aligned throughout
      procedure execution. [...]

      Procedures must not rely upon the persistence of stack-allocated data
      whose addresses lie below the stack pointer.

      @verbatim
      (Stack
       growing
       direction)
           |
           |   +-------------------------+      (high address)
           |   | Local Variables Func 1  |
           |   +-------------------------+
           |   | Argument Area for func- |
           |   | tions called by Func 1  |      (first argument passed on stack)
           |   +-------------------------+
           |   | Local Variables Func 2  |
           |   +-------------------------+
           |   | Argument Area for func- |
           |   | tions called by Func 2  |
           |   +-------------------------+ <--- Stack Pointer (SP) at entry
           V   | Local Variables Func 3  |      (CALL) to Function 3
               +-------------------------+
               | Argument Area for func- |
               | tions called by Func 3  |
               +-------------------------+ <--- Stack Pointer (SP) after stack
               |                         |      allocation of Function 3
               |           ...           |
               +-------------------------+      (low address)
      @endverbatim

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    static void adjustStack( WIR_Function &, int,
                             const std::list<std::reference_wrapper<WIR_Instruction>> & );

    /*!
      @brief isStackPointerADDI checks wether the given operation is an ADDI
             operation that modifies the stack pointer.

      @param[in] o A const reference to a %WIR operation to be examined.
      @return True if o is an ADDI and modifies the stack pointer, false
              otherwise.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    static bool isStackPointerADDI( const WIR_Operation & );

    /*!
      @brief boolBytes contains the width of a _Bool in bytes for the RV32I
             architecture.

      See RISC-V ABIs Specification, section 4.1.
    */
    static const int boolBytes = ( 8 / 8 );

    /*!
      @brief charBytes contains the width of a _char in bytes for the RV32I
             architecture.

      See RISC-V ABIs Specification, section 4.1.
    */
    static const int charBytes = ( 8 / 8 );

    /*!
      @brief shortBytes contains the width of a short in bytes for the RV32I
             architecture.

      See RISC-V ABIs Specification, section 4.1.
    */
    static const int shortBytes = ( 16 / 8 );

    /*!
      @brief intBytes contains the width of an int in bytes for the RV32I
             architecture.

      See RISC-V ABIs Specification, section 4.1.
    */
    static const int intBytes = ( 32 / 8 );

    /*!
      @brief longBytes contains the width of a long in bytes for the RV32I
             architecture.

      See RISC-V ABIs Specification, section 4.1.
    */
    static const int longBytes = ( 32 / 8 );

    /*!
      @brief longLongBytes contains the width of a long long in bytes for the
             RV32I architecture.

      See RISC-V ABIs Specification, section 4.1.
    */
    static const int longLongBytes = ( 64 / 8 );

    /*!
      @brief pointerBytes contains the width of a pointer in bytes for the RV32I
             architecture.

      See RISC-V ABIs Specification, section 4.1.
    */
    static const int pointerBytes = ( 32 / 8 );

    /*!
      @brief floatBytes contains the width of a float in bytes for the RV32I
             architecture.

      See RISC-V ABIs Specification, section 4.1.
    */
    static const int floatBytes = ( 32 / 8 );

    /*!
      @brief doubleBytes contains the width of a double in bytes for the RV32I
             architecture.

      See RISC-V ABIs Specification, section 4.1.
    */
    static const int doubleBytes = ( 64 / 8 );

    /*!
      @brief longDoubleBytes contains the width of a long double in bytes for the
             RV32I architecture.

      See RISC-V ABIs Specification, section 4.1.
    */
    static const int longDoubleBytes = ( 128 / 8 );


  private:

    /*!
      @brief clone creates a copy of an RV32I processor.

      @return A pointer to the newly created RV32I copy.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    virtual WIR_BaseProcessor *clone( void ) const;

};

}       // namespace WIR

#endif  // _RV32I_H
