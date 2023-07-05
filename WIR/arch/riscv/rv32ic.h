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
  @file rv32ic.h
  @brief This file provides the specific interface of the RISC-V RV32IC Base
         Integer instruction set plus the C Standard Extension for Compressed
         Instructions, version 2.0.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/


#ifndef _RV32IC_H
#define _RV32IC_H


//
// Include section
//

// Include WIR headers
#include <arch/riscv/rv32i.h>
#include <arch/riscv/rvconst6unsigned.h>
#include <arch/riscv/rvconst6signed.h>
#include <arch/riscv/rvconst8unsigned.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class RV32IC models the RISC-V RV32IC Base Integer instruction set plus
         the C Standard Extension for Compressed Instructions, version 2.0.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/
class RV32IC : virtual public RV32I
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for RV32IC processor architectures.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32IC( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32IC( const RV32IC & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32IC( RV32IC && );

    /*!
      @brief Destructor.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    virtual ~RV32IC( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32IC & operator = ( const RV32IC & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32IC & operator = ( RV32IC && );


    //
    // RV32IC-specific global initializations.
    //

    /*!
      @brief init performs some global initialization tasks for RV32IC processor
             architectures.

      This includes setting up the assignment of valid operation formats to
      RV32IC opcodes.

      @note init shall be called globally by WIR_Init(). It shall only perform
            tasks that cannot be expressed as initializations of static class
            members (since the order of static initialization is unspecified in
            C++) and that thus require execution by active code.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    static void init( void );


    //
    // Data structures used to model the RISC-V RV32IC V2.0 architecture.
    //

    /*!
      @brief The public members of class OpCode model the RV32IC's opcodes.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    class OpCode : public RV32I::OpCode
    {

      public:

        //! Compressed Addition
        static const OpCode CADD;

        //! Compressed Add Immediate
        static const OpCode CADDI;

        //! Compressed Stack Pointer Adjustment
        static const OpCode CADDI16SP;

        //! Compressed Pointer Generation
        static const OpCode CADDI4SPN;

        //! Compressed Logical AND
        static const OpCode CAND;

        //! Compressed Logical AND Immediate
        static const OpCode CANDI;

        //! Compressed Branch if Equal Zero
        static const OpCode CBEQZ;

        //! Compressed Branch if Not Equal Zero
        static const OpCode CBNEZ;

        //! Compressed Environment Break
        static const OpCode CEBREAK;

        //! Compressed Jump
        static const OpCode CJ;

        //! Compressed Jump And Link
        static const OpCode CJAL;

        //! Compressed Jump And Link Register
        static const OpCode CJALR;

        //! Compressed Jump Register
        static const OpCode CJR;

        //! Compressed Load Immediate
        static const OpCode CLI;

        //! Compressed Load Upper Immediate
        static const OpCode CLUI;

        //! Compressed Load Word
        static const OpCode CLW;

        //! Compressed Load Word from Stack
        static const OpCode CLWSP;

        //! Compressed Move
        static const OpCode CMV;

        //! No Operation
        static const OpCode CNOP;

        //! Compressed Logical OR
        static const OpCode COR;

        //! Compressed Shift Left Logical Immediate
        static const OpCode CSLLI;

        //! Compressed Shift Right Arithmetical Immediate
        static const OpCode CSRAI;

        //! Compressed Shift Right Logical Immediate
        static const OpCode CSRLI;

        //! Compressed Subtraction
        static const OpCode CSUB;

        //! Compressed Store Word
        static const OpCode CSW;

        //! Compressed Store Word to Stack
        static const OpCode CSWSP;

        //! Compressed Logical XOR
        static const OpCode CXOR;


      protected:

        // Inherit the Constructors from RV32I::OpCode.
        using RV32I::OpCode::OpCode;

    };

    /*!
      @brief The public members of class OperationFormant model the RV32IC's
             different formats of machine operations.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    class OperationFormat : public RV32I::OperationFormat
    {

      public:

        //! 16-bit label
        static const OperationFormat SL_1;

        //! 16-bit without operands
        static const OperationFormat SNULL_1;

        //! 16-bit reg (use)
        static const OperationFormat SR_1;

        //! 16-bit reg (defuse), unsigned const5
        static const OperationFormat SRC5_1;

        //! 16-bit reg (def), unsigned const5, reg (use)
        static const OperationFormat SRC5R_1;

        //! 16-bit reg (use), unsigned const5, reg (use)
        static const OperationFormat SRC5R_2;

        //! 16-bit reg (def), signed const6
        static const OperationFormat SRC6_1;

        //! 16-bit reg (def), unsigned const6
        static const OperationFormat SRC6_2;

        //! 16-bit reg (defuse), signed const6
        static const OperationFormat SRC6_3;

        //! 16-bit sp (defuse), signed const6
        static const OperationFormat SRC6_4;

        //! 16-bit reg (def), unsigned const6, reg (use)
        static const OperationFormat SRC6R_1;

        //! 16-bit reg (use), unsigned const6, reg (use)
        static const OperationFormat SRC6R_2;

        //! 16-bit reg (def), label
        static const OperationFormat SRL_1;

        //! 16-bit reg (def), reg (use)
        static const OperationFormat SRR_1;

        //! 16-bit reg (defuse), reg (use)
        static const OperationFormat SRR_2;

        //! 16-bit reg (def), reg (use), unsigned const8
        static const OperationFormat SRRC8_1;


      protected:

        // Inherit the Constructors from RV32I::OperationFormat.
        using RV32I::OperationFormat::OperationFormat;

    };


  private:

    /*!
      @brief clone creates a copy of an RV32IC processor.

      @return A pointer to the newly created RV32IC copy.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    virtual WIR_BaseProcessor *clone( void ) const;

};

}       // namespace WIR

#endif  // _RV32IC_H
