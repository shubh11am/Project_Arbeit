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
  @file tc13.h
  @brief This file provides the specific interface of the Infineon TriCore
         V1.3 architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC13_H
#define _TC13_H


//
// Include section
//

// Include standard headers
#include <list>
#include <utility>

// Include WIR headers
#include <wir/wirprocessor.h>

#include <arch/tricore/tcaregphysical.h>
#include <arch/tricore/tcaregvirtual.h>
#include <arch/tricore/tcconst1unsigned.h>
#include <arch/tricore/tcconst2unsigned.h>
#include <arch/tricore/tcconst3unsigned.h>
#include <arch/tricore/tcconst4signed.h>
#include <arch/tricore/tcconst4unsigned.h>
#include <arch/tricore/tcconst5unsigned.h>
#include <arch/tricore/tcconst8unsigned.h>
#include <arch/tricore/tcconst9signed.h>
#include <arch/tricore/tcconst9unsigned.h>
#include <arch/tricore/tcconst10signed.h>
#include <arch/tricore/tcconst10unsigned.h>
#include <arch/tricore/tcconst16signed.h>
#include <arch/tricore/tcconst16unsigned.h>
#include <arch/tricore/tcconst18unsigned.h>
#include <arch/tricore/tcdregphysical.h>
#include <arch/tricore/tcdregvirtual.h>
#include <arch/tricore/tceregphysical.h>
#include <arch/tricore/tceregvirtual.h>
#include <arch/tricore/tcio.h>
#include <arch/tricore/tcpregphysical.h>
#include <arch/tricore/tcpregvirtual.h>
#include <arch/tricore/tcpswbitphysical.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Function;
class WIR_Instruction;


/*!
  @brief Class TC13 models the Infineon TriCore V1.3 instruction set architecture.

  Specific other TriCore variants like, e.g., the V1.3.1 ISA are derived from
  this class by inheritance.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC13 : public WIR_Processor<TC13>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for TC13 processor architectures.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC13( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC13( const TC13 & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC13( TC13 && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC13( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC13 & operator = ( const TC13 & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC13 & operator = ( TC13 && );


    //
    // TC13-specific global initializations.
    //

    /*!
      @brief init performs some global initialization tasks for TC13 processor
             architectures.

      This includes setting up the TC13's machine operation formats and the
      assignment of valid operation formats to TC13 opcodes.

      @note init shall be called globally by WIR_Init(). It shall only perform
            tasks that cannot be expressed as initializations of static class
            members (since the order of static initialization is unspecified in
            C++) and that thus require execution by active code.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static void init( void );


    //
    // Data structures used to model the Infineon TriCore V1.3 architecture.
    //

    /*!
      @brief The public members of class AddressingMode model the TC13's
             addressing modes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class AddressingMode : public WIR_Processor::AddressingMode
    {

      public:

        //! Pre-increment addressing.
        static const AddressingMode pre;

        //! Post-increment addressing.
        static const AddressingMode post;

        /*!
          @brief getProcessorTypeName returns a string containing the C++-
                 mangled name of the TC13 class to which an addressing mode
                 belongs.

          @return A string holding the TC13 type name.

          This method is only used for comparing addressing modes of different
          processor architectures in WIR_Operation::checkParameters().

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual std::string getProcessorTypeName( void ) const;


      protected:

        // Inherit the Constructors from WIR_Processor::AddressingMode.
        using WIR_Processor::AddressingMode::AddressingMode;

    };

    /*!
      @brief The public members of class OpCode model the TC13's opcodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OpCode : public WIR_Processor::OpCode
    {

      public:

        //! Absolute Value
        static const OpCode ABS;

        //! Absolut Value Packed Byte
        static const OpCode ABS_B;

        //! Absolut Value Packed Halfword
        static const OpCode ABS_H;

        //! Absolut Value of Difference
        static const OpCode ABSDIF;

        //! Absolut Value of Difference Packed Byte
        static const OpCode ABSDIF_B;

        //! Absolut Value of Difference Halfword
        static const OpCode ABSDIF_H;

        //! Absolut Value of Difference with Saturation
        static const OpCode ABSDIFS;

        //! Absolut Value of Difference Packed Halfword with Saturation
        static const OpCode ABSDIFS_H;

        //! Absolute Value with Saturation
        static const OpCode ABSS;

        //! Absolute Value Packed Halfword with Saturation
        static const OpCode ABSS_H;

        //! Add
        static const OpCode ADD;

        //! Add Address
        static const OpCode ADD_A;

        //! Add Packed Byte
        static const OpCode ADD_B;

        //! Add Float
        static const OpCode ADD_F;

        //! Add Packed Halfword
        static const OpCode ADD_H;

        //! Add with Carry
        static const OpCode ADDC;

        //! Add Immediate
        static const OpCode ADDI;

        //! Add Immediate High
        static const OpCode ADDIH;

        //! Add Immediate High to Address
        static const OpCode ADDIH_A;

        //! Add Signed with Saturation
        static const OpCode ADDS;

        //! Add Signed Packed Halfword with Saturation
        static const OpCode ADDS_H;

        //! Add Unsigned Packed Halfword with Saturation
        static const OpCode ADDS_HU;

        //! Add Unsigned with Saturation
        static const OpCode ADDS_U;

        //! Add Scaled Index to Address
        static const OpCode ADDSC_A;

        //! Add Bit-Scaled Index to Address
        static const OpCode ADDSC_AT;

        //! Add Extended
        static const OpCode ADDX;

        //! Logical AND
        static const OpCode AND;

        //! Accumulating Logical AND-AND
        static const OpCode AND_AND_T;

        //! Accumulating Logical AND-AND-Not
        static const OpCode AND_ANDN_T;

        //! Equal Accumulating
        static const OpCode AND_EQ;

        //! Greater Than or Equal Accumulating
        static const OpCode AND_GE;

        //! Greater Than or Equal Accumulating Unsigned
        static const OpCode AND_GE_U;

        //! Less Than Accumulating
        static const OpCode AND_LT;

        //! Less Than Accumulating Unsigned
        static const OpCode AND_LT_U;

        //! Not Equal Accumulating
        static const OpCode AND_NE;

        //! Accumulating Logical AND-NOR
        static const OpCode AND_NOR_T;

        //! Accumulating Logical AND-OR
        static const OpCode AND_OR_T;

        //! Bit Logical AND
        static const OpCode AND_T;

        //! AND-Not
        static const OpCode ANDN;

        //! Bit Logical AND-Not
        static const OpCode ANDN_T;

        //! Begin ISR
        static const OpCode BISR;

        //! Bit Merge
        static const OpCode BMERGE;

        //! Bit Split
        static const OpCode BSPLIT;

        //! Cache Address, Invalidate
        static const OpCode CACHEA_I;

        //! Cache Address, Writeback
        static const OpCode CACHEA_W;

        //! Cache Address, Writeback and Invalidate
        static const OpCode CACHEA_WI;

        //! Conditional Add
        static const OpCode CADD;

        //! Conditional Add-Not
        static const OpCode CADDN;

        //! Call
        static const OpCode CALL;

        //! Call Absolute
        static const OpCode CALLA;

        //! Call Indirect
        static const OpCode CALLI;

        //! Count Leading Ones
        static const OpCode CLO;

        //! Count Leading Ones in Packed Halfwords
        static const OpCode CLO_H;

        //! Count Leading Signs
        static const OpCode CLS;

        //! Count Leading Signs in Packed Halfwords
        static const OpCode CLS_H;

        //! Count Leading Zeroes
        static const OpCode CLZ;

        //! Count Leading Zeroes in Packed Halfwords
        static const OpCode CLZ_H;

        //! Conditional Move
        static const OpCode CMOV;

        //! Conditional Move-Not
        static const OpCode CMOVN;

        //! Compare Float
        static const OpCode CMP_F;

        //! Conditional Subtract
        static const OpCode CSUB;

        //! Conditional Subtract-Not
        static const OpCode CSUBN;

        //! Debug
        static const OpCode DEBUG;

        //! Extract from Double Register
        static const OpCode DEXTR;

        //! Disable Interrupts
        static const OpCode DISABLE;

        //! Divide Float
        static const OpCode DIV_F;

        //! Synchronize Data
        static const OpCode DSYNC;

        //! Divide-Adjust
        static const OpCode DVADJ;

        //! Divide-Initialization Word
        static const OpCode DVINIT;

        //! Divide-Initialization Byte
        static const OpCode DVINIT_B;

        //! Divide-Initialization Byte Unsigned
        static const OpCode DVINIT_BU;

        //! Divide-Initialization Halfword
        static const OpCode DVINIT_H;

        //! Divide-Initialization Halfword Unsigned
        static const OpCode DVINIT_HU;

        //! Divide-Initialization Word Unsigned
        static const OpCode DVINIT_U;

        //! Divide-Step
        static const OpCode DVSTEP;

        //! Divide-Step Unsigned
        static const OpCode DVSTEP_U;

        //! Enable Interrupts
        static const OpCode ENABLE;

        //! Equal
        static const OpCode EQ;

        //! Equal to Address
        static const OpCode EQ_A;

        //! Equal Packed Byte
        static const OpCode EQ_B;

        //! Equal Packed Halfword
        static const OpCode EQ_H;

        //! Equal Packed Word
        static const OpCode EQ_W;

        //! Equal Any Byte
        static const OpCode EQANY_B;

        //! Equal Any Halfword
        static const OpCode EQANY_H;

        //! Equal Zero Address
        static const OpCode EQZ_A;

        //! Extract Bit Field
        static const OpCode EXTR;

        //! Extract Bit Field Unsigned
        static const OpCode EXTR_U;

        //! Float to Integer
        static const OpCode FTOI;

        //! Float to Fraction
        static const OpCode FTOQ31;

        //! Float to Unsigned
        static const OpCode FTOU;

        //! Greater Than or Equal
        static const OpCode GE;

        //! Greater Than or Equal Address
        static const OpCode GE_A;

        //! Greater Than or Equal Unsigned
        static const OpCode GE_U;

        //! Insert Mask
        static const OpCode IMASK;

        //! Insert Bit
        static const OpCode INS_T;

        //! Insert Bit Field
        static const OpCode INSERT;

        //! Insert Bit-Not
        static const OpCode INSN_T;

        //! Synchronize Instructions
        static const OpCode ISYNC;

        //! Integer to Float
        static const OpCode ITOF;

        //! Find Maximum Index
        static const OpCode IXMAX;

        //! Find Maximum Index Unsigned
        static const OpCode IXMAX_U;

        //! Find Minimum Index
        static const OpCode IXMIN;

        //! Find Minimum Index Unsigned
        static const OpCode IXMIN_U;

        //! Jump Unconditional
        static const OpCode J;

        //! Jump Unconditional Absolute
        static const OpCode JA;

        //! Jump if Equal
        static const OpCode JEQ;

        //! Jump if Equal Address
        static const OpCode JEQ_A;

        //! Jump if Greater Than or Equal
        static const OpCode JGE;

        //! Jump if Greater Than or Equal Unsigned
        static const OpCode JGE_U;

        //! Jump if Greater Than or Equal to Zero
        static const OpCode JGEZ;

        //! Jump if Greater Than Zero
        static const OpCode JGTZ;

        //! Jump Indirect
        static const OpCode JI;

        //! Jump and Link
        static const OpCode JL;

        //! Jump and Link Absolute
        static const OpCode JLA;

        //! Jump if Less Than or Equal to Zero
        static const OpCode JLEZ;

        //! Jump and Link Indirect
        static const OpCode JLI;

        //! Jump if Less Than
        static const OpCode JLT;

        //! Jump if Less Than Unsigned
        static const OpCode JLT_U;

        //! Jump if Less Than Zero
        static const OpCode JLTZ;

        //! Jump if Not Equal
        static const OpCode JNE;

        //! Jump if Not Equal Address
        static const OpCode JNE_A;

        //! Jump if Not Equal and Decrement
        static const OpCode JNED;

        //! Jump if Not Equal and Increment
        static const OpCode JNEI;

        //! Jump if Not Equal to Zero
        static const OpCode JNZ;

        //! Jump if Not Equal to Zero Address
        static const OpCode JNZ_A;

        //! Jump if Not Equal to Zero Bit
        static const OpCode JNZ_T;

        //! Jump if Zero
        static const OpCode JZ;

        //! Jump if Zero Address
        static const OpCode JZ_A;

        //! Jump if Zero Bit
        static const OpCode JZ_T;

        //! Load Word to Address Register
        static const OpCode LD_A;

        //! Load Byte
        static const OpCode LD_B;

        //! Load Byte Unsigned
        static const OpCode LD_BU;

        //! Load Doubleword
        static const OpCode LD_D;

        //! Load Doubleword to Address Register
        static const OpCode LD_DA;

        //! Load Halfword
        static const OpCode LD_H;

        //! Load Halfword Unsigned
        static const OpCode LD_HU;

        //! Load Halfword Signed Fraction
        static const OpCode LD_Q;

        //! Load Word
        static const OpCode LD_W;

        //! Load Lower Context
        static const OpCode LDLCX;

        //! Load-Modify-Store
        static const OpCode LDMST;

        //! Load Upper Context
        static const OpCode LDUCX;

        //! Load Effective Address
        static const OpCode LEA;

        //! Loop
        static const OpCode LOOP;

        //! Loop Unconditional
        static const OpCode LOOPU;

        //! Less Than
        static const OpCode LT;

        //! Less Than Address
        static const OpCode LT_A;

        //! Less Than Packed Byte
        static const OpCode LT_B;

        //! Less Than Packed Byte Unsigned
        static const OpCode LT_BU;

        //! Less Than Packed Halfword
        static const OpCode LT_H;

        //! Less Than Packed Halfword Unsigned
        static const OpCode LT_HU;

        //! Less Than Unsigned
        static const OpCode LT_U;

        //! Less Than Packed Word
        static const OpCode LT_W;

        //! Less Than Packed Word Unsigned
        static const OpCode LT_WU;

        //! Multiply-Add
        static const OpCode MADD;

        //! Multiply Add Float
        static const OpCode MADD_F;

        //! Packed Multiply-Add Q Format
        static const OpCode MADD_H;

        //! Multiply-Add Q Format
        static const OpCode MADD_Q;

        //! Multiply-Add Unsigned
        static const OpCode MADD_U;

        //! Packed Multiply-Add Q Format-Multiprecision
        static const OpCode MADDM_H;

        //! Packed Multiply-Add Q Format-Multiprecision with Saturation
        static const OpCode MADDMS_H;

        //! Packed Multiply-Add Q Format with Rounding
        static const OpCode MADDR_H;

        //! Multiply-Add Q Format with Rounding
        static const OpCode MADDR_Q;

        //! Packed Multiply-Add Q Format with Rounding and Saturation
        static const OpCode MADDRS_H;

        //! Multiply-Add Q Format with Rounding and Saturation
        static const OpCode MADDRS_Q;

        //! Multiply-Add with Saturation
        static const OpCode MADDS;

        //! Packed Multiply-Add Q Format with Saturation
        static const OpCode MADDS_H;

        //! Multiply-Add Q Format with Saturation
        static const OpCode MADDS_Q;

        //! Multiply-Add Unsigned with Saturation
        static const OpCode MADDS_U;

        //! Packed Multiply-Add/Subtract Q Format
        static const OpCode MADDSU_H;

        //! Packed Multiply-Add/Subtract Q Format-Multiprecision
        static const OpCode MADDSUM_H;

        //! Packed Multiply-Add/Subtract Q Format-Multiprecision with Saturation
        static const OpCode MADDSUMS_H;

        //! Packed Multiply-Add/Subtract Q Format with Rounding
        static const OpCode MADDSUR_H;

        //! Packed Multiply-Add/Subtract Q Format with Rounding and Saturation
        static const OpCode MADDSURS_H;

        //! Packed Multiply-Add/Subtract Q Format with Saturation
        static const OpCode MADDSUS_H;

        //! Maximum Value
        static const OpCode MAX;

        //! Maximum Value Packed Byte
        static const OpCode MAX_B;

        //! Maximum Value Packed Byte Unsigned
        static const OpCode MAX_BU;

        //! Maximum Value Packed Halfword
        static const OpCode MAX_H;

        //! Maximum Value Packed Halfword Unsigned
        static const OpCode MAX_HU;

        //! Maximum Value Unsigned
        static const OpCode MAX_U;

        //! Move From Core Register
        static const OpCode MFCR;

        //! Minimum Value
        static const OpCode MIN;

        //! Minimum Value Packed Byte
        static const OpCode MIN_B;

        //! Minimum Value Packed Byte Unsigned
        static const OpCode MIN_BU;

        //! Minimum Value Packed Halfword
        static const OpCode MIN_H;

        //! Minimum Value Packed Halfword Unsigned
        static const OpCode MIN_HU;

        //! Minimum Value Unsigned
        static const OpCode MIN_U;

        //! Move
        static const OpCode MOV;

        //! Register-Register Move (needed for register allocation)
        static const OpCode MOV_RR;

        //! Move Value to Address Register
        static const OpCode MOV_A;

        //! Move Address from Address Register
        static const OpCode MOV_AA;

        //! Move Address to Data Register
        static const OpCode MOV_D;

        //! Move Unsigned
        static const OpCode MOV_U;

        //! Move High
        static const OpCode MOVH;

        //! Move High to Address
        static const OpCode MOVH_A;

        //! Multiply-Sub
        static const OpCode MSUB;

        //! Multiply Subtract Float
        static const OpCode MSUB_F;

        //! Packed Multiply-Sub Q Format
        static const OpCode MSUB_H;

        //! Multiply-Sub Q Format
        static const OpCode MSUB_Q;

        //! Multiply-Sub Unsigned
        static const OpCode MSUB_U;

        //! Packed Multiply-Sub/Add Q Format
        static const OpCode MSUBAD_H;

        //! Packed Multiply-Sub/Add Q Format-Multiprecision
        static const OpCode MSUBADM_H;

        //! Packed Multiply-Sub/Add Q Format-Multiprecision with Saturation
        static const OpCode MSUBADMS_H;

        //! Packed Multiply-Sub/Add Q Format with Rounding
        static const OpCode MSUBADR_H;

        //! Packed Multiply-Sub/Add Q Format with Rounding and Saturation
        static const OpCode MSUBADRS_H;

        //! Packed Multiply-Sub/Add Q Format with Saturation
        static const OpCode MSUBADS_H;

        //! Packed Multiply-Sub/Add Q Format-Multiprecision
        static const OpCode MSUBM_H;

        //! Packed Multiply-Sub/Add Q Format-Multiprecision with Saturation
        static const OpCode MSUBMS_H;

        //! Packed Multiply-Sub Q Format with Rounding
        static const OpCode MSUBR_H;

        //! Multiply-Sub Q Format with Rounding
        static const OpCode MSUBR_Q;

        //! Packed Multiply-Sub Q Format with Rounding and Saturation
        static const OpCode MSUBRS_H;

        //! Multiply-Sub Q Format with Rounding and Saturation
        static const OpCode MSUBRS_Q;

        //! Multiply-Sub with Saturation
        static const OpCode MSUBS;

        //! Packed Multiply-Sub Q Format with Saturation
        static const OpCode MSUBS_H;

        //! Multiply-Sub Q Format with Saturation
        static const OpCode MSUBS_Q;

        //! Multiply-Sub Unsigned with Saturation
        static const OpCode MSUBS_U;

        //! Move To Core Register
        static const OpCode MTCR;

        //! Multiply
        static const OpCode MUL;

        //! Multiply Float
        static const OpCode MUL_F;

        //! Packed Multiply Q Format
        static const OpCode MUL_H;

        //! Multiply Q Format
        static const OpCode MUL_Q;

        //! Multiply Unsigned
        static const OpCode MUL_U;

        //! Packed Multiply Q Format-Multiprecision
        static const OpCode MULM_H;

        //! Packed Multiply Q Format with Rounding
        static const OpCode MULR_H;

        //! Multiply Q Format with Rounding
        static const OpCode MULR_Q;

        //! Multiply with Saturation
        static const OpCode MULS;

        //! Multiply Unsigned with Saturation
        static const OpCode MULS_U;

        //! Logical NAND
        static const OpCode NAND;

        //! Bit Logical NAND
        static const OpCode NAND_T;

        //! Not Equal
        static const OpCode NE;

        //! Not Equal Address
        static const OpCode NE_A;

        //! Not Equal Zero Address
        static const OpCode NEZ_A;

        //! No Operation
        static const OpCode NOP;

        //! Logical NOR
        static const OpCode NOR;

        //! Bit Logical NOR
        static const OpCode NOR_T;

        //! Bitwise Complement NOT
        static const OpCode NOT;

        //! Logical OR
        static const OpCode OR;

        //! Accumulating Logical OR-AND
        static const OpCode OR_AND_T;

        //! Accumulating Logical OR-AND-Not
        static const OpCode OR_ANDN_T;

        //! Equal Accumulating
        static const OpCode OR_EQ;

        //! Greater Than or Equal Accumulating
        static const OpCode OR_GE;

        //! Greater Than or Equal Accumulating Unsigned
        static const OpCode OR_GE_U;

        //! Less Than Accumulating
        static const OpCode OR_LT;

        //! Less Than Accumulating Unsigned
        static const OpCode OR_LT_U;

        //! Not Equal Accumulating
        static const OpCode OR_NE;

        //! Accumulating Logical OR-NOR
        static const OpCode OR_NOR_T;

        //! Accumulating Logical OR-OR
        static const OpCode OR_OR_T;

        //! Bit Logical OR
        static const OpCode OR_T;

        //! Logical OR-Not
        static const OpCode ORN;

        //! Bit Logical OR-Not
        static const OpCode ORN_T;

        //! Pack floating point
        static const OpCode PACK;

        //! Parity
        static const OpCode PARITY;

        //! Fraction to Floating-point
        static const OpCode Q31TOF;

        //! Inverse Square Root Seed
        static const OpCode QSEED_F;

        //! Return from Call
        static const OpCode RET;

        //! Return from Exception
        static const OpCode RFE;

        //! Return from Monitor
        static const OpCode RFM;

        //! Restore Lower Context
        static const OpCode RSLCX;

        //! Reset Overflow Bits
        static const OpCode RSTV;

        //! Reverse-Subtract
        static const OpCode RSUB;

        //! Reverse-Subtract with Saturation
        static const OpCode RSUBS;

        //! Reverse-Subtract Unsigned with Saturation
        static const OpCode RSUBS_U;

        //! Saturate Byte
        static const OpCode SAT_B;

        //! Saturate Byte Unsigned
        static const OpCode SAT_BU;

        //! Saturate Halfword
        static const OpCode SAT_H;

        //! Saturate Halfword Unsigned
        static const OpCode SAT_HU;

        //! Select
        static const OpCode SEL;

        //! Select-Not
        static const OpCode SELN;

        //! Shift
        static const OpCode SH;

        //! Accumulating Shift-AND
        static const OpCode SH_AND_T;

        //! Accumulating Shift-AND-Not
        static const OpCode SH_ANDN_T;

        //! Shift Equal
        static const OpCode SH_EQ;

        //! Shift Greater Than or Equal
        static const OpCode SH_GE;

        //! Shift Greater Than or Equal Unsigned
        static const OpCode SH_GE_U;

        //! Shift Packed Halfwords
        static const OpCode SH_H;

        //! Shift Less Than
        static const OpCode SH_LT;

        //! Shift Less Than Unsigned
        static const OpCode SH_LT_U;

        //! Accumulating Shift-NAND
        static const OpCode SH_NAND_T;

        //! Shift Not Equal
        static const OpCode SH_NE;

        //! Accumulating Shift-NOR
        static const OpCode SH_NOR_T;

        //! Accumulating Shift-OR
        static const OpCode SH_OR_T;

        //! Accumulating Shift-OR-Not
        static const OpCode SH_ORN_T;

        //! Accumulating Shift-XNOR
        static const OpCode SH_XNOR_T;

        //! Accumulating Shift-XOR
        static const OpCode SH_XOR_T;

        //! Arithmetic Shift
        static const OpCode SHA;

        //! Arithmetic Shift Packed Halfwords
        static const OpCode SHA_H;

        //! Arithmetic Shift with Saturation
        static const OpCode SHAS;

        //! Store Word from Address Register
        static const OpCode ST_A;

        //! Store Byte
        static const OpCode ST_B;

        //! Store Doubleword
        static const OpCode ST_D;

        //! Store Doubleword from Address Registers
        static const OpCode ST_DA;

        //! Store Halfword
        static const OpCode ST_H;

        //! Store Halfword Signed Fraction
        static const OpCode ST_Q;

        //! Store Bit
        static const OpCode ST_T;

        //! Store Word
        static const OpCode ST_W;

        //! Store Lower Context
        static const OpCode STLCX;

        //! Store Upper Context
        static const OpCode STUCX;

        //! Subtract
        static const OpCode SUB;

        //! Subtract Address
        static const OpCode SUB_A;

        //! Subtract Packed Byte
        static const OpCode SUB_B;

        //! Subtract Float
        static const OpCode SUB_F;

        //! Subtract Packed Halfword
        static const OpCode SUB_H;

        //! Subtract with Carry
        static const OpCode SUBC;

        //! Subtract Signed with Saturation
        static const OpCode SUBS;

        //! Subtract Packed Halfword with Saturation
        static const OpCode SUBS_H;

        //! Subtract Packed Halfword Unsigned with Saturation
        static const OpCode SUBS_HU;

        //! Subtract Unsigned with Saturation
        static const OpCode SUBS_U;

        //! Subtract Extended
        static const OpCode SUBX;

        //! Save Lower Context
        static const OpCode SVLCX;

        //! Swap with Data Register
        static const OpCode SWAP_W;

        //! System Call
        static const OpCode SYSCALL;

        //! Translation Lokaside Buffer Demap
        static const OpCode TLBDEMAP;

        //! TLB-A Flush
        static const OpCode TLBFLUSH_A;

        //! TLB-B Flush
        static const OpCode TLBFLUSH_B;

        //! TLB Map
        static const OpCode TLBMAP;

        //! TLB Probe Address
        static const OpCode TLBPROBE_A;

        //! TLB Probe Index
        static const OpCode TLBPROBE_I;

        //! Trap on Sticky Overflow
        static const OpCode TRAPSV;

        //! Trap on Overflow
        static const OpCode TRAPV;

        //! Unpack floating point
        static const OpCode UNPACK;

        //! Update Flags
        static const OpCode UPDFL;

        //! Unsigned to Floating-point
        static const OpCode UTOF;

        //! Logical XNOR
        static const OpCode XNOR;

        //! Bit Logical XNOR
        static const OpCode XNOR_T;

        //! Logical XOR
        static const OpCode XOR;

        //! Equal Accumulating
        static const OpCode XOR_EQ;

        //! Greater Than or Equal Accumulating
        static const OpCode XOR_GE;

        //! Greater Than or Equal Accumulating Unsigned
        static const OpCode XOR_GE_U;

        //! Less Than Accumulating
        static const OpCode XOR_LT;

        //! Less Than Accumulating Unsigned
        static const OpCode XOR_LT_U;

        //! Not Equal Accumulating
        static const OpCode XOR_NE;

        //! Bit Logical XOR
        static const OpCode XOR_T;


      protected:

        // Inherit the Constructors from WIR_Processor::OpCode.
        using WIR_Processor::OpCode::OpCode;

    };

    /*!
      @brief The public members of class RegisterType model the TC13's different
             types of registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class RegisterType : public WIR_Processor::RegisterType
    {

      public:

        //! Address registers.
        static const RegisterType aReg;

        //! Data registers.
        static const RegisterType dReg;

        //! Extended data registers.
        static const RegisterType eReg;

        //! Extended address registers.
        static const RegisterType pReg;

        //! Processor Status Word PSW bits.
        static const RegisterType pswBit;


      protected:

        // Inherit the constructors from WIR_Processor::RegisterType.
        using WIR_Processor::RegisterType::RegisterType;

    };

    /*!
      @brief The public members of class OperationFormat model the TC13's
             different formats of machine operations.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OperationFormat : public WIR_Processor::OperationFormat
    {

      public:

        //! 32-bit aReg (use)
        static const OperationFormat A;

        //! 32-bit aReg (def), aReg (use)
        static const OperationFormat AA;

        //! 32-bit aReg (def), aReg (use), aReg (use)
        static const OperationFormat AAA;

        //! 32-bit aReg (def), aReg (use), signed offset10
        static const OperationFormat AAC10BOA;

        //! 32-bit aReg (def), addrmode, aReg (defuse), signed offset10
        static const OperationFormat AAC10PIA;

        //! 32-bit aReg (def), aReg (use), unsigned const16
        static const OperationFormat AAC16;

        //! 32-bit aReg (def), aReg (use), signed offset16
        static const OperationFormat AAC16BOA;

        //! 32-bit aReg (def), aReg (use), dReg (use)
        static const OperationFormat AAD;

        //! 32-bit aReg (def), aReg (use), dReg (use), unsigned const2
        static const OperationFormat AADC2;

        //! 32-bit aReg (use), aReg (use), label/displacement15
        static const OperationFormat AAL;

        //! 32-bit aReg (def), aReg (use), LO:label+displacement16
        static const OperationFormat AALC16BOA;

        //! 32-bit aReg (use), signed offset10, aReg (use)
        static const OperationFormat AC10ABOA;

        //! 32-bit addrmode, aReg (defuse), signed offset10, aReg (use)
        static const OperationFormat AC10APIA;

        //! 32-bit aReg (use), signed offset10
        static const OperationFormat AC10BOA;

        //! 32-bit aReg (use), aReg (use), signed offset10, PSW.C (use)
        static const OperationFormat AC10BOAPSW;

        //! 32-bit aReg (use), signed offset10, dReg (use)
        static const OperationFormat AC10DBOA_1;

        //! 32-bit aReg (use), signed offset10, dReg (defuse)
        static const OperationFormat AC10DBOA_2;

        //! 32-bit addrmode, aReg (defuse), signed offset10, dReg (use)
        static const OperationFormat AC10DPIA_1;

        //! 32-bit addrmode, aReg (defuse), signed offset10, dReg (defuse)
        static const OperationFormat AC10DPIA_2;

        //! 32-bit aReg (use), signed offset10, eReg (use)
        static const OperationFormat AC10EBOA;

        //! 32-bit addrmode, aReg (defuse), signed offset10, eReg (use)
        static const OperationFormat AC10EPIA;

        //! 32-bit aReg (use), signed offset10, pReg (use)
        static const OperationFormat AC10PBOA;

        //! 32-bit addrmode, aReg (defuse), signed offset10
        static const OperationFormat AC10PIA;

        //! 32-bit addrmode, aReg (defuse), signed offset10, pReg (use)
        static const OperationFormat AC10PPIA;

        //! 32-bit aReg (def), unsigned const16
        static const OperationFormat AC16;

        //! 32-bit aReg (use), signed offset16, dReg (use)
        static const OperationFormat AC16DBOA;

        //! 32-bit aReg (def), unsigned offset18
        static const OperationFormat AC18ABSA;

        //! 32-bit aReg (def), dReg (use)
        static const OperationFormat AD;

        //! 32-bit aReg (def), HI:label/displacement15
        static const OperationFormat AL_1;

        //! 32-bit aReg (use), label/displacement15
        static const OperationFormat AL_2;

        //! 32-bit aReg (defuse), label/displacement15
        static const OperationFormat AL_3;

        //! 32-bit aReg (def), label
        static const OperationFormat ALABSA;

        //! 32-bit aReg (use), LO:label+displacement16, dReg (use)
        static const OperationFormat ALC16DBOA;

        //! 32-bit aReg (def), pReg (defuse)
        static const OperationFormat APBRA;

        //! 32-bit aReg (def), pReg (defuse), signed offset10
        static const OperationFormat APC10CA;

        //! 32-bit unsigned const16, dReg (use), PSW.C (def)
        static const OperationFormat C16DPSW;

        //! 32-bit unsigned offset18, aReg (use)
        static const OperationFormat C18AABSA;

        //! 32-bit unsigned offset18
        static const OperationFormat C18ABSA;

        //! 32-bit unsigned offset18, PSW.C (use)
        static const OperationFormat C18ABSAPSW;

        //! 32-bit unsigned offset18, unsigned const3, unsigned const1
        static const OperationFormat C18C3C1;

        //! 32-bit unsigned offset18, dReg (use)
        static const OperationFormat C18DABSA_1;

        //! 32-bit unsigned offset18, dReg (defuse)
        static const OperationFormat C18DABSA_2;

        //! 32-bit unsigned offset18, eReg (use)
        static const OperationFormat C18EABSA;

        //! 32-bit unsigned offset18, pReg (use)
        static const OperationFormat C18PABSA;

        //! 32-bit unsigned const9
        static const OperationFormat C9;

        //! 32-bit unsigned const9, PSW.C (use)
        static const OperationFormat C9PSW;

        //! 32-bit dReg (use)
        static const OperationFormat D;

        //! 32-bit dReg (def), aReg (use)
        static const OperationFormat DA;

        //! 32-bit dReg (def), aReg (use), aReg (use)
        static const OperationFormat DAA;

        //! 32-bit dReg (def), aReg (use), signed offset10
        static const OperationFormat DAC10BOA;

        //! 32-bit dReg (def), addrmode, aReg (defuse), signed offset10
        static const OperationFormat DAC10PIA;

        //! 32-bit dReg (def), aReg (use), signed offset16
        static const OperationFormat DAC16BOA;

        //! 32-bit dReg (def), aReg (use), LO:label+displacement16
        static const OperationFormat DALC16BOA;

        //! 32-bit dReg (def), signed const16
        static const OperationFormat DC16_1;

        //! 32-bit dReg (def), unsigned const16
        static const OperationFormat DC16_2;

        //! 32-bit dReg (def), unsigned const16, PSW.C (use)
        static const OperationFormat DC16PSW;

        //! 32-bit dReg (def), unsigned offset18
        static const OperationFormat DC18ABSA;

        //! 32-bit dReg (use), signed const4, label/displacement15
        static const OperationFormat DC4L_1;

        //! 32-bit dReg (use), unsigned const4, label/displacement15
        static const OperationFormat DC4L_2;

        //! 32-bit dReg (defuse), signed const4, label/displacement15
        static const OperationFormat DC4L_3;

        //! 32-bit dReg (use), unsigned const5, label/displacement15
        static const OperationFormat DC5L;

        //! 32-bit dReg (def), dReg (use)
        static const OperationFormat DD;

        //! 32-bit dReg (def), dReg (use), signed const16
        static const OperationFormat DDC16_1;

        //! 32-bit dReg (def), dReg (use), unsigned const16
        static const OperationFormat DDC16_2;

        //! 32-bit dReg (def), dReg (use), unsigned const4, unsigned const5, unsigned const5
        static const OperationFormat DDC4C5C5;

        //! 32-bit dReg (def), dReg (use), unsigned const4, dReg (use), unsigned const5
        static const OperationFormat DDC4DC5;

        //! 32-bit dReg (def), dReg (use), unsigned const4, eReg (use)
        static const OperationFormat DDC4E;

        //! 32-bit dReg (def), dReg (use), unsigned const5, unsigned const5
        static const OperationFormat DDC5C5;

        //! 32-bit dReg (def), dReg (use), unsigned const5, dReg (use), unsigned const5
        static const OperationFormat DDC5DC5_1;

        //! 32-bit dReg (defuse), dReg (use), unsigned const5, dReg (use), unsigned const5
        static const OperationFormat DDC5DC5_2;

        //! 32-bit dReg (def), dReg (use), signed const9
        static const OperationFormat DDC9_1;

        //! 32-bit dReg (def), dReg (use), unsigned const9
        static const OperationFormat DDC9_2;

        //! 32-bit dReg (defuse), dReg (use), signed const9
        static const OperationFormat DDC9_3;

        //! 32-bit dReg (defuse), dReg (use), unsigned const9
        static const OperationFormat DDC9_4;

        //! 32-bit dReg (def), dReg (use), unsigned const9, PSW.C (def)
        static const OperationFormat DDC9PSW_1;

        //! 32-bit dReg (def), dReg (use), unsigned const9, PSW.C (defuse)
        static const OperationFormat DDC9PSW_2;

        //! 32-bit dReg (def), dReg (use), dReg (use)
        static const OperationFormat DDD_1;

        //! 32-bit dReg (defuse), dReg (use), dReg (use)
        static const OperationFormat DDD_2;

        //! 32-bit dReg (def), dReg (use), dReg (use), unsigned const1
        static const OperationFormat DDDC1_1;

        //! 32-bit dReg (def), dReg (use), dReg (use) L, unsigned const1
        static const OperationFormat DDDC1_2;

        //! 32-bit dReg (def), dReg (use), dReg (use) LL, unsigned const1
        static const OperationFormat DDDC1_3;

        //! 32-bit dReg (def), dReg (use), dReg (use) LU, unsigned const1
        static const OperationFormat DDDC1_4;

        //! 32-bit dReg (def), dReg (use), dReg (use) U, unsigned const1
        static const OperationFormat DDDC1_5;

        //! 32-bit dReg (def), dReg (use), dReg (use) UL, unsigned const1
        static const OperationFormat DDDC1_6;

        //! 32-bit dReg (def), dReg (use), dReg (use) UU, unsigned const1
        static const OperationFormat DDDC1_7;

        //! 32-bit dReg (def), dReg (use) L, dReg (use) L, unsigned const1
        static const OperationFormat DDDC1_8;

        //! 32-bit dReg (def), dReg (use) U, dReg (use) U, unsigned const1
        static const OperationFormat DDDC1_9;

        //! 32-bit dReg (def), dReg (use), dReg (use), unsigned const5
        static const OperationFormat DDDC5;

        //! 32-bit dReg (def), dReg (use), dReg (use), unsigned const5, unsigned const5
        static const OperationFormat DDDC5C5;

        //! 32-bit dReg (def), dReg (use), dReg (use), signed const9
        static const OperationFormat DDDC9_1;

        //! 32-bit dReg (def), dReg (use), dReg (use), unsigned const9
        static const OperationFormat DDDC9_2;

        //! 32-bit dReg (def), dReg (use), dReg (use), dReg (use)
        static const OperationFormat DDDD;

        //! 32-bit dReg (def), dReg (use), dReg (use), dReg (use), unsigned const1
        static const OperationFormat DDDDC1_1;

        //! 32-bit dReg (def), dReg (use), dReg (use), dReg (use) L, unsigned const1
        static const OperationFormat DDDDC1_2;

        //! 32-bit dReg (def), dReg (use), dReg (use), dReg (use) LL, unsigned const1
        static const OperationFormat DDDDC1_3;

        //! 32-bit dReg (def), dReg (use), dReg (use), dReg (use) LU, unsigned const1
        static const OperationFormat DDDDC1_4;

        //! 32-bit dReg (def), dReg (use), dReg (use), dReg (use) U, unsigned const1
        static const OperationFormat DDDDC1_5;

        //! 32-bit dReg (def), dReg (use), dReg (use), dReg (use) UL, unsigned const1
        static const OperationFormat DDDDC1_6;

        //! 32-bit dReg (def), dReg (use), dReg (use), dReg (use) UU, unsigned const1
        static const OperationFormat DDDDC1_7;

        //! 32-bit dReg (def), dReg (use), dReg (use) L, dReg (use) L, unsigned const1
        static const OperationFormat DDDDC1_8;

        //! 32-bit dReg (def), dReg (use), dReg (use) U, dReg (use) U, unsigned const1
        static const OperationFormat DDDDC1_9;

        //! 32-bit dReg (def), dReg (use), dReg (use), dReg (use), unsigned const5
        static const OperationFormat DDDDC5;

        //! 32-bit dReg (def), dReg (use), dReg (use), eReg (use)
        static const OperationFormat DDDE;

        //! 32-bit dReg (def), dReg (use), dReg (use), PSW.C (def)
        static const OperationFormat DDDPSW_1;

        //! 32-bit dReg (def), dReg (use), dReg (use), PSW.C (defuse)
        static const OperationFormat DDDPSW_2;

        //! 32-bit dReg (def), dReg (use), eReg (use)
        static const OperationFormat DDE;

        //! 32-bit dReg (use), dReg (use), label/displacement15
        static const OperationFormat DDL_1;

        //! 32-bit dReg (defuse), dReg (use), label/displacement15
        static const OperationFormat DDL_2;

        //! 32-bit dReg (def), eReg (use), dReg (use), PSW.C (use)
        static const OperationFormat DEDPSW;

        //! 32-bit dReg (def), eReg (use), dReg (use), dReg (use) UL, unsigned const1
        static const OperationFormat DEDDC1;

        //! 32-bit dReg (def), label
        static const OperationFormat DLABSA;

        //! 32-bit dReg (def), pReg (defuse)
        static const OperationFormat DPBRA;

        //! 32-bit dReg (def), pReg (defuse), signed offset10
        static const OperationFormat DPC10CA;

        //! 32-bit eReg (use)
        static const OperationFormat E;

        //! 32-bit eReg (def), aReg (use), signed offset10
        static const OperationFormat EAC10BOA;

        //! 32-bit eReg (def), addrmode, aReg (defuse), signed offset10
        static const OperationFormat EAC10PIA;

        //! 32-bit eReg (def), unsigned offset18
        static const OperationFormat EC18ABSA;

        //! 32-bit eReg (def), unsigned const4, unsigned const5, unsigned const5
        static const OperationFormat EC4C5C5;

        //! 32-bit eReg (def), unsigned const4, dReg (use), unsigned const5
        static const OperationFormat EC4DC5;

        //! 32-bit eReg (def), dReg (use)
        static const OperationFormat ED;

        //! 32-bit eReg (def), dReg (use), unsigned const5, unsigned const5
        static const OperationFormat EDC5C5;

        //! 32-bit eReg (def), dReg (use), signed const9
        static const OperationFormat EDC9_1;

        //! 32-bit eReg (def), dReg (use), unsigned const9
        static const OperationFormat EDC9_2;

        //! 32-bit eReg (def), dReg (use), dReg (use)
        static const OperationFormat EDD;

        //! 32-bit eReg (def), dReg (use), dReg (use), unsigned const1
        static const OperationFormat EDDC1_1;

        //! 32-bit eReg (def), dReg (use), dReg (use) L, unsigned const1
        static const OperationFormat EDDC1_2;

        //! 32-bit eReg (def), dReg (use), dReg (use) LL, unsigned const1
        static const OperationFormat EDDC1_3;

        //! 32-bit eReg (def), dReg (use), dReg (use) LU, unsigned const1
        static const OperationFormat EDDC1_4;

        //! 32-bit eReg (def), dReg (use), dReg (use) U, unsigned const1
        static const OperationFormat EDDC1_5;

        //! 32-bit eReg (def), dReg (use), dReg (use) UL, unsigned const1
        static const OperationFormat EDDC1_6;

        //! 32-bit eReg (def), dReg (use), dReg (use) UU, unsigned const1
        static const OperationFormat EDDC1_7;

        //! 32-bit eReg (def), dReg (use), dReg (use), unsigned const5
        static const OperationFormat EDDC5;

        //! 32-bit eReg (def), dReg (use), dReg (use)
        static const OperationFormat EED;

        //! 32-bit eReg (def), eReg (use), dReg (use), signed const9
        static const OperationFormat EEDC9_1;

        //! 32-bit eReg (def), eReg (use), dReg (use), unsigned const9
        static const OperationFormat EEDC9_2;

        //! 32-bit eReg (def), eReg (use), dReg (use), dReg (use)
        static const OperationFormat EEDD;

        //! 32-bit eReg (def), eReg (use), dReg (use), dReg (use), unsigned const1
        static const OperationFormat EEDDC1_1;

        //! 32-bit eReg (def), eReg (use), dReg (use), dReg (use) L, unsigned const1
        static const OperationFormat EEDDC1_2;

        //! 32-bit eReg (def), eReg (use), dReg (use), dReg (use) LL, unsigned const1
        static const OperationFormat EEDDC1_3;

        //! 32-bit eReg (def), eReg (use), dReg (use), dReg (use) LU, unsigned const1
        static const OperationFormat EEDDC1_4;

        //! 32-bit eReg (def), eReg (use), dReg (use), dReg (use) U, unsigned const1
        static const OperationFormat EEDDC1_5;

        //! 32-bit eReg (def), eReg (use), dReg (use), dReg (use) UL, unsigned const1
        static const OperationFormat EEDDC1_6;

        //! 32-bit eReg (def), eReg (use), dReg (use), dReg (use) UU, unsigned const1
        static const OperationFormat EEDDC1_7;

        //! 32-bit eReg (def), eReg (use), dReg (use) L, dReg (use) L, unsigned const1
        static const OperationFormat EEDDC1_8;

        //! 32-bit eReg (def), eReg (use), dReg (use) U, dReg (use) U, unsigned const1
        static const OperationFormat EEDDC1_9;

        //! 32-bit eReg (def), label
        static const OperationFormat ELABSA;

        //! 32-bit eReg (def), pReg (defuse)
        static const OperationFormat EPBRA;

        //! 32-bit eReg (def), pReg (defuse), signed offset10
        static const OperationFormat EPC10CA;

        //! 32-bit label/displacement24/15
        static const OperationFormat L;

        //! 32-bit label, aReg (use)
        static const OperationFormat LAABSA;

        //! 32-bit label
        static const OperationFormat LABSA;

        //! 32-bit label, PSW.C (use)
        static const OperationFormat LABSAPSW;

        //! 32-bit label, dReg (use)
        static const OperationFormat LDABSA_1;

        //! 32-bit label, dReg (defuse)
        static const OperationFormat LDABSA_2;

        //! 32-bit label, eReg (use)
        static const OperationFormat LEABSA;

        //! 32-bit label, pReg (use)
        static const OperationFormat LPABSA;

        //! 32-bit pReg (defuse), aReg (use)
        static const OperationFormat PABRA;

        //! 32-bit pReg (def), aReg (use), signed offset10
        static const OperationFormat PAC10BOA;

        //! 32-bit pReg (def), addrmode, aReg (defuse), signed offset10
        static const OperationFormat PAC10PIA;

        //! 32-bit pReg (defuse)
        static const OperationFormat PBRA;

        //! 32-bit pReg (defuse), signed offset10, aReg (use)
        static const OperationFormat PC10ACA;

        //! 32-bit pReg (defuse), signed offset10
        static const OperationFormat PC10CA;

        //! 32-bit pReg (defuse), signed offset10, dReg (use)
        static const OperationFormat PC10DCA_1;

        //! 32-bit pReg (defuse), signed offset10, dReg (defuse)
        static const OperationFormat PC10DCA_2;

        //! 32-bit pReg (defuse), signed offset10, eReg (use)
        static const OperationFormat PC10ECA;

        //! 32-bit pReg (defuse), signed offset10, pReg (use)
        static const OperationFormat PC10PCA;

        //! 32-bit pReg (def), unsigned offset18
        static const OperationFormat PC18ABSA;

        //! 32-bit pReg (defuse), dReg (use)
        static const OperationFormat PDBRA_1;

        //! 32-bit pReg (defuse), dReg (defuse)
        static const OperationFormat PDBRA_2;

        //! 32-bit pReg (defuse), eReg (use)
        static const OperationFormat PEBRA;

        //! 32-bit pReg (def), label
        static const OperationFormat PLABSA;

        //! 32-bit pReg (def), pReg (defuse)
        static const OperationFormat PPBRA_1;

        //! 32-bit pReg (defuse), pReg (use)
        static const OperationFormat PPBRA_2;

        //! 32-bit pReg (def), pReg (defuse), signed offset10
        static const OperationFormat PPC10CA;

        //! 32-bit PSW.C (def)
        static const OperationFormat PSW;

        //! 16-bit without operands
        static const OperationFormat S;

        //! 16-bit aReg (use)
        static const OperationFormat SA;

        //! 16-bit aReg (def), aReg (use)
        static const OperationFormat SAA_1;

        //! 16-bit aReg (def), aReg (use/register indirect addressing)
        static const OperationFormat SAA_2;

        //! 16-bit aReg (def), aReg (defuse)
        static const OperationFormat SAA_3;

        //! 16-bit aReg (use), aReg (use)
        static const OperationFormat SAA_4;

        //! 16-bit aReg (defuse), aReg (use)
        static const OperationFormat SAA_5;

        //! 16-bit aReg (defuse/postincrement), aReg (use)
        static const OperationFormat SAA_6;

        //! 16-bit aReg (def), aReg (use), D15 (use), unsigned const2
        static const OperationFormat SAAIC2;

        //! 16-bit aReg (def), unsigned const4
        static const OperationFormat SAC4_1;

        //! 16-bit aReg (defuse), signed const4
        static const OperationFormat SAC4_2;

        //! 16-bit aReg (use), unsigned const4, A15 (use)
        static const OperationFormat SAC4I_1;

        //! 16-bit aReg (use), unsigned const4, D15 (use)
        static const OperationFormat SAC4I_2;

        //! 16-bit aReg (def), dReg (use)
        static const OperationFormat SAD_1;

        //! 16-bit aReg (use), dReg (use)
        static const OperationFormat SAD_2;

        //! 16-bit aReg (defuse), dReg (use)
        static const OperationFormat SAD_3;

        //! 16-bit aReg (def), A15 (use), unsigned const4
        static const OperationFormat SAIC4;

        //! 16-bit aReg (use), label/displacement4
        static const OperationFormat SAL_1;

        //! 16-bit aReg (defuse), label/displacement4
        static const OperationFormat SAL_2;

        //! 16-bit unsigned const8
        static const OperationFormat SC8;

        //! 16-bit dReg (defuse)
        static const OperationFormat SD;

        //! 16-bit dReg (def), aReg (use)
        static const OperationFormat SDA_1;

        //! 16-bit dReg (def), aReg (use/register indirect addressing)
        static const OperationFormat SDA_2;

        //! 16-bit dReg (def), aReg (defuse)
        static const OperationFormat SDA_3;

        //! 16-bit dReg (def), signed const4
        static const OperationFormat SDC4_1;

        //! 16-bit dReg (defuse), signed const4
        static const OperationFormat SDC4_2;

        //! 16-bit dReg (defuse), signed const4, PSW.C (def)
        static const OperationFormat SDC4PSW;

        //! 16-bit dReg (def), dReg (use)
        static const OperationFormat SDD_1;

        //! 16-bit dReg (defuse), dReg (use)
        static const OperationFormat SDD_2;

        //! 16-bit dReg (def), A15 (use), unsigned const4
        static const OperationFormat SDIC4_1;

        //! 16-bit dReg (def), D15 (use), signed const4
        static const OperationFormat SDIC4_2;

        //! 16-bit dReg (defuse), D15 (use), signed const4
        static const OperationFormat SDIC4_3;

        //! 16-bit dReg (def), D15 (use), dReg (use)
        static const OperationFormat SDID_1;

        //! 16-bit dReg (defuse), D15 (use), dReg (use)
        static const OperationFormat SDID_2;

        //! 16-bit dReg (use), label/displacement4
        static const OperationFormat SDL;

        //! 16-bit A15 (def), aReg (use), unsigned const4
        static const OperationFormat SIAC4_1;

        //! 16-bit D15 (def), aReg (use), unsigned const4
        static const OperationFormat SIAC4_2;

        //! 16-bit A15 (use), unsigned const4, aReg (use)
        static const OperationFormat SIC4A;

        //! 16-bit A15 (use), unsigned const4, dReg (use)
        static const OperationFormat SIC4D;

        //! 16-bit D15 (use), signed const4, label/displacement4
        static const OperationFormat SIC4L;

        //! 16-bit D15 (use), unsigned const5, label/displacement4
        static const OperationFormat SIC5L;

        //! 16-bit D15 (def), unsigned const8
        static const OperationFormat SIC8_1;

        //! 16-bit D15 (defuse), unsigned const8
        static const OperationFormat SIC8_2;

        //! 16-bit D15 (def), dReg (use), signed const4
        static const OperationFormat SIDC4;

        //! 16-bit D15 (def), dReg (use), dReg (use)
        static const OperationFormat SIDD;

        //! 16-bit D15 (use), dReg (use), label/displacement4
        static const OperationFormat SIDL;

        //! 16-bit D15 (use), label/displacement8
        static const OperationFormat SIL;

        //! 16-bit A15 (def), SP (use), unsigned const10 (multiple of 4)
        static const OperationFormat SISPC10_1;

        //! 16-bit D15 (def), SP (use), unsigned const10 (multiple of 4)
        static const OperationFormat SISPC10_2;

        //! 16-bit label/displacement8
        static const OperationFormat SL;

        //! 16-bit PSW.C (def)
        static const OperationFormat SPSW;

        //! 16-bit SP (defuse), unsigned const8
        static const OperationFormat SSPC8;

        //! 16-bit SP (use), unsigned const10 (multiple of 4), A15 (use)
        static const OperationFormat SSPC10I_1;

        //! 16-bit SP (use), unsigned const10 (multiple of 4), D15 (use)
        static const OperationFormat SSPC10I_2;

        //! 32-bit without operands
        static const OperationFormat SYS;


      protected:

        // Inherit the constructors from WIR_Processor::OperationFormat.
        using WIR_Processor::OperationFormat::OperationFormat;

    };

    /*!
      @brief Access to physical address register A0 (global address register).
      @return A const reference to A0.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A0( void ) const;

    /*!
      @brief Access to physical address register A1 (global address register).
      @return A const reference to A1.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A1( void ) const;

    /*!
      @brief Access to physical address register A2.
      @return A const reference to A2.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A2( void ) const;

    /*!
      @brief Access to physical address register A3.
      @return A const reference to A3.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A3( void ) const;

    /*!
      @brief Access to physical address register A4.
      @return A const reference to A4.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A4( void ) const;

    /*!
      @brief Access to physical address register A5.
      @return A const reference to A5.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A5( void ) const;

    /*!
      @brief Access to physical address register A6.
      @return A const reference to A6.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A6( void ) const;

    /*!
      @brief Access to physical address register A7.
      @return A const reference to A7.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A7( void ) const;

    /*!
      @brief Access to physical address register A8 (global address register).
      @return A const reference to A8.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A8( void ) const;

    /*!
      @brief Access to physical address register A9 (global address register).
      @return A const reference to A9.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A9( void ) const;

    /*!
      @brief Access to physical address register A10 (stack pointer).
      @return A const reference to A10.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A10( void ) const;

    /*!
      @brief Access to physical address register A11 (return address pointer).
      @return A const reference to A11.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A11( void ) const;

    /*!
      @brief Access to physical address register A12.
      @return A const reference to A12.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A12( void ) const;

    /*!
      @brief Access to physical address register A13.
      @return A const reference to A13.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A13( void ) const;

    /*!
      @brief Access to physical address register A14.
      @return A const reference to A14.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A14( void ) const;

    /*!
      @brief Access to physical address register A15 (implicit address
             register).
      @return A const reference to A15.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &A15( void ) const;

    /*!
      @brief Access to stack pointer.
      @return A const reference to A10.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &SP( void ) const;

    /*!
      @brief Access to return address pointer.
      @return A const reference to A11.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &RA( void ) const;

    /*!
      @brief Access to physical data register D0.
      @return A const reference to D0.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D0( void ) const;

    /*!
      @brief Access to physical data register D1.
      @return A const reference to D1.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D1( void ) const;

    /*!
      @brief Access to physical data register D2.
      @return A const reference to D2.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D2( void ) const;

    /*!
      @brief Access to physical data register D3.
      @return A const reference to D3.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D3( void ) const;

    /*!
      @brief Access to physical data register D4.
      @return A const reference to D4.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D4( void ) const;

    /*!
      @brief Access to physical data register D5.
      @return A const reference to D5.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D5( void ) const;

    /*!
      @brief Access to physical data register D6.
      @return A const reference to D6.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D6( void ) const;

    /*!
      @brief Access to physical data register D7.
      @return A const reference to D7.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D7( void ) const;

    /*!
      @brief Access to physical data register D8.
      @return A const reference to D8.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D8( void ) const;

    /*!
      @brief Access to physical data register D9.
      @return A const reference to D9.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D9( void ) const;

    /*!
      @brief Access to physical data register D10.
      @return A const reference to D10.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D10( void ) const;

    /*!
      @brief Access to physical data register D11.
      @return A const reference to D11.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D11( void ) const;

    /*!
      @brief Access to physical data register D12.
      @return A const reference to D12.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D12( void ) const;

    /*!
      @brief Access to physical data register D13.
      @return A const reference to D13.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D13( void ) const;

    /*!
      @brief Access to physical data register D14.
      @return A const reference to D14.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D14( void ) const;

    /*!
      @brief Access to physical data register D15 (implicit data register).
      @return A const reference to D15.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &D15( void ) const;

    /*!
      @brief Access to physical extended data register E0.
      @return A const reference to E0.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ERegP &E0( void ) const;

    /*!
      @brief Access to physical extended data register E2.
      @return A const reference to E2.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ERegP &E2( void ) const;

    /*!
      @brief Access to physical extended data register E4.
      @return A const reference to E4.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ERegP &E4( void ) const;

    /*!
      @brief Access to physical extended data register E6.
      @return A const reference to E6.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ERegP &E6( void ) const;

    /*!
      @brief Access to physical extended data register E8.
      @return A const reference to E8.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ERegP &E8( void ) const;

    /*!
      @brief Access to physical extended data register E10.
      @return A const reference to E10.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ERegP &E10( void ) const;

    /*!
      @brief Access to physical extended data register E12.
      @return A const reference to E12.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ERegP &E12( void ) const;

    /*!
      @brief Access to physical extended data register E14.
      @return A const reference to E14.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ERegP &E14( void ) const;

    /*!
      @brief Access to physical extended address register P0.
      @return A const reference to P0.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_PRegP &P0( void ) const;

    /*!
      @brief Access to physical extended address register P2.
      @return A const reference to P2.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_PRegP &P2( void ) const;

    /*!
      @brief Access to physical extended address register P4.
      @return A const reference to P4.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_PRegP &P4( void ) const;

    /*!
      @brief Access to physical extended address register P6.
      @return A const reference to P6.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_PRegP &P6( void ) const;

    /*!
      @brief Access to physical extended address register P8.
      @return A const reference to P8.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_PRegP &P8( void ) const;

    /*!
      @brief Access to physical extended address register P10.
      @return A const reference to P10.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_PRegP &P10( void ) const;

    /*!
      @brief Access to physical extended address register P12.
      @return A const reference to P12.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_PRegP &P12( void ) const;

    /*!
      @brief Access to physical extended address register P14.
      @return A const reference to P14.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_PRegP &P14( void ) const;

    /*!
      @brief Access to physical Carry bit of Processor Status Word PSW.
      @return A const reference to PSW.C.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_PSWBit &PSW_C( void ) const;

    /*!
      @brief isSP checks whether the specified register is the TriCore's stack
             pointer.

      @param[in] r A const reference to a register to be checked.
      @return true if the specified register is physical and is the stack
              pointer or if it is virtual and precolored with A10, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isSP( const WIR_BaseRegister & );

    /*!
      @brief isRA checks whether the specified register is the TriCore's
             return address register A11.

      @param[in] r A const reference to a register to be checked.
      @return true if the specified register is physical and is A11 or if it is
                   virtual and precolored with A11, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isRA( const WIR_BaseRegister & );

    /*!
      @brief isA15 checks whether the specified register is the TriCore's
             implicit address register A15.

      @param[in] r A const reference to a register to be checked.
      @return true if the specified register is physical and is A15 or if it is
                   virtual and precolored with A15, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isA15( const WIR_BaseRegister & );

    /*!
      @brief isD15 checks whether the specified register is the TriCore's
             implicit data register D15.

      @param[in] r A const reference to a register to be checked.
      @return true if the specified register is physical and is D15 or if it is
                   virtual and precolored with D15, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isD15( const WIR_BaseRegister & );

    /*!
      @brief isPSW_C checks whether the specified register is the TriCore's
             carry bit of the Processor Status Word (PSW).

      @param[in] r A const reference to a register to be checked.
      @return true if the specified register is physical and is PSW.C, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isPSW_C( const WIR_BaseRegister & );

    /*!
      @brief isLCReg checks whether the specified register is part of the
             TriCore's lower register context.

      @param[in] r A const reference to a register to be checked.
      @return true if the specified register is physical and is part of the
              lower context, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isLCReg( const WIR_BaseRegister & );

    /*!
      @brief isIP determines whether the given operation is a TriCore IP
             operation.

      @param[in] o A const reference to a %WIR operation to be checked.
      @return true if o is a TriCore IP operation, false otherwise.

      See: TriCore DSP Optimization Guide - Part 1: Instruction Set,
      Chapter 13.1.3.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isIP( const WIR_Operation & );

    /*!
      @brief isLP determines whether the given operation is a TriCore LP
             operation.

      @param[in] o A const reference to a %WIR operation to be checked.
      @return true if o is a TriCore LP operation, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isLP( const WIR_Operation & );

    /*!
      @brief isLS determines whether the given operation is a TriCore LS
             operation.

      @param[in] o A const reference to a %WIR operation to be checked.
      @return true if o is a TriCore LS operation, false otherwise.

      See: TriCore DSP Optimization Guide - Part 1: Instruction Set,
      Chapter 13.1.3.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isLS( const WIR_Operation & );

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

      According to the TriCore EABI (section 2.2.2.1), the stack pointer points
      to the bottom (low address) of the stack frame. The stack pointer
      alignment is 8 bytes. The argument overflow area for outgoing arguments
      must be located at the bottom (low address end) of the frame, with the
      first overflow argument at zero offset from the stack pointer:

      @verbatim
      (Stack
       growing
       direction)
           |
           |   +-------------------------+      (high address)
           |   | Local Variables Frame 1 |
           |   +-------------------------+
           |   | Argument Overflow Area, |
           |   | Function 2 Arguments    |      (first argument passed on stack)
           |   +-------------------------+
           |   | Local Variables Frame 2 |
           |   +-------------------------+
           |   | Argument Overflow Area, |
           |   | Function 3 Arguments    |
           |   +-------------------------+ <--- Stack Pointer (SP) at entry
           V   | Local Variables Frame 3 |      (CALL) to Function 3
               +-------------------------+
               | Argument Overflow Area  |
               +-------------------------+      (low address)
      @endverbatim

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static void adjustStack( WIR_Function &, int,
                             const std::list<std::reference_wrapper<WIR_Instruction>> & );

    /*!
      @brief isStackPointerADDIHA returns whether the given operation is an
             ADDIH_A that modifies the stack pointer.

      @param[in] o A const reference to a %WIR operation to be examined.
      @return True iff o is an ADDIH_A modifying the stack pointer, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isStackPointerADDIHA( const WIR_Operation & );

    /*!
      @brief isStackPointerLEA returns whether the given operation is a LEA
             that modifies the stack pointer.

      @param[in] o A const reference to a %WIR operation to be examined.
      @return True iff o is a LEA modifying the stack pointer, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isStackPointerLEA( const WIR_Operation & );

    /*!
      @brief isStackpointerSUB returns whether the given operation is a
             subtraction that modifies the stack pointer.

      @param[in] o A const reference to a %WIR operation to be examined.
      @return True iff o is a subtraction modifying the stack pointer, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isStackPointerSUB( const WIR_Operation & );

};

}       // namespace WIR

#endif  // _TC13_H
