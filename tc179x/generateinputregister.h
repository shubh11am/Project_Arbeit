/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2011 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _GENERATE_INPUT_REGISTER_H
#define _GENERATE_INPUT_REGISTER_H


//
// Include section
//

#include <arch/IO/PARSER/getllirregister.h>


//
// Class forward declarations
//

class InstructionFactory;
class IR_AsmOperand;
class IR_Integer;
class IR_SymbolExp;
class LLIR_Register;


/*!
  @brief Is handed to the inline asm parser to generate llir_registers as
         needed.

  The asm parser can only determine if an input register needs to be an ereg,
  once it has read the actual inline assembly, since to determine this, it has
  to look at the register modifiers. This class allows it to create the
  appropriate register during parsing.

  As every call to one of the methods creates a new vreg and code filling that
  vreg with the appropriate value, only one method should be called exactly one
  time.
*/
class GenerateInputRegister : public Tricap::GetLLIRRegister
{

  public:

    /*!
      @brief Create a new class to generate either an ereg or a vreg.

      @param instructionFactory the instructionFactory which is used to insert
             the generated instructions.
      @param operand represents the modifiers and an expression provided
             in the asm template.
    */
    GenerateInputRegister( InstructionFactory *, const IR_AsmOperand & );

    /*!
      @brief Create a new EReg and insert code to initialize it.

      The register is initialized with the value associated with the expression
      given to the constructor.
    */
    virtual LLIR_Register *getEReg();

    /*!
      @brief Create a new DReg and insert code to initialize it.

      The register is initialized with the value associated with the expression
      given to the constructor.
    */
    virtual LLIR_Register *getDReg();


  private:

    InstructionFactory *mInstructionFactory;
    LLIR_Register *mReg;
    IR_SymbolExp *mSymExp;
    const IR_Integer *mValue;
    const IR_AsmOperand &mOperand;

};

#endif  // _GENERATE_INPUT_REGISTER_H
