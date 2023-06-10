/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2018 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file asmregisterinitializer.h
  @brief This file provides the interface of a class initializing %WIR TriCore
         registers from high-level specifiers in GNU inline assembler code.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _ASMREGISTERINITIALIZER_H
#define _ASMREGISTERINITIALIZER_H


//
// Include section
//

// Include WIR headers
#include <arch/tricore/asmparser/tcasmregisterinitializer.h>


//
// Class forward declarations
//

namespace WIR {
class WIR_BaseRegister;
class WIR_VirtualRegister;
class TC_DRegV;
class TC_ERegV;
}

class IR_AsmOperand;
class IR_Integer;
class IR_SymbolExp;

class LLIR_Register;

class InstructionFactory;


/*!
  @brief Class AsmRegisterInitializer serves as an interface between the ICD-C-
         unaware TriCore assembler parser and the template register arguments
         present in GNU inline assembly.

  Using input and output operands in GNU inline assembly, the assembly code can
  be connected with variables inside the sorrounding C source code. These input
  and output operands thus act as the interface between C and assembly. This
  class thus establishes the link between a high-level IR and %WIR and connects
  C source code variables with %WIR registers.
*/
class AsmRegisterInitializer : public WIR::TC_AsmRegisterInitializer
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an ASM register initializer for a
             given Assembly argument and a TriCore instruction factory to be
             used for code generation.

      @param[in] o A const reference to a GNU inline assembly operand.
      @param[in] f A const reference to the instruction factory to be used for
                   code generation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    AsmRegisterInitializer( const IR_AsmOperand &, const InstructionFactory & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~AsmRegisterInitializer( void );


    //
    // Register management.
    //

    /*!
      @brief getDReg returns the virtual data register associated with this
             object.

      @return A const reference to a TriCore virtual data register.

      Depending on the register's context, getDReg also generates assembly code
      to properly initialize the data register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual const WIR::TC_DRegV &getDReg( void );

    /*!
      @brief getEReg returns the virtual extended register associated with this
             object.

      @return A const reference to a virtual TriCore extended register.

      Depending on the register's context, getEReg also generates assembly code
      to properly initialize the extended register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual const WIR::TC_ERegV &getEReg( void );


  private:

    //! mOperand holds a reference to the current GNU inline assembly operand.
    const IR_AsmOperand &mOperand;

    /*!
      @brief mFactory holds a reference to the instruction factory to be used
             for code generation.
    */
    const InstructionFactory &mFactory;

    /*!
      @brief mReg points to the %WIR register associated with a GNU inline
             assembly operand.
    */
    WIR::WIR_VirtualRegister *mReg;

    /*!
      @brief mSymExp points to a symbol expression if mOperand contains such a
             kind of expression.
    */
    IR_SymbolExp *mSymExp;

    /*!
      @brief mValue points to an %IR integer if mOperand contains an intConst
             expression.
    */
    const IR_Integer *mValue;

};

#endif  // _ASMREGISTERINITIALIZER_H
