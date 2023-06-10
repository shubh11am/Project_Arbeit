/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

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
  @file rv32instructionfactory.h
  @brief This file provides the interface of the RISC-V RV32 instruction factory
         producing WIR machine instructions.
*/


#ifndef _RV32_INSTRUCTION_FACTORY_H
#define _RV32_INSTRUCTION_FACTORY_H


//
// Include section
//

// Include standard headers
#include <utility>


//
// Class forward declarations
//

namespace WIR {
class WIR_BasicBlock;
class WIR_Data;
class WIR_Instruction;
class RV_RegV;
}

namespace RV32 {
class RV32_CodeSelector;
}

class IR_Exp;
class IR_Stmt;

class Configuration;


//
// Header section
//

namespace RV32 {

/*!
  @brief Class RV32_InstructionFactory has the sole purpose to produce RISC-V
         RV32 machine instructions during code selection.

  As there are masses of different instructions for the RISC-V architecture,
  this class was created to keep the code selector's rules clear, and to
  separate the rules from the machinery of instruction creation.
*/
class RV32_InstructionFactory
{

  public:

    //
    // Local type definitions.
    //

    /*!
      @brief The enum StmtType is used to represent a statement's type for
             assembly-level debug information.
    */
    enum StmtType
    {
      //! A simple statement consisting only of an expression.
      EXP_STMT,

      //! An if statement.
      IF_STMT,

      //! An if-else statement.
      IFELSE_STMT,

      //! A for-loop statement.
      FOR_STMT,

      //! A while-loop statement.
      WHILE_STMT,

      //! A do-while statement.
      DOWHILE_STMT,

      //! An unconditional jump statement (break, goto, continue).
      JUMP_STMT,

      //! A switch statement.
      SWITCH_STMT,

      //! A void-return statment.
      VOID_STMT,

      //! A non-void return statement.
      RETURN_STMT,

      //! An inline assembly statement.
      ASM_STMT,

      //! Default, if the type is not defined.
      NOT_DEFINED
    };


    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @param[in,out] config A reference to a WCC compiler configuration object.
      @param[in,out] codesel A reference to a RISC-V RV32 code selector instance
                             for which the factory produces instructions.
    */
    RV32_InstructionFactory( Configuration &, RV32_CodeSelector & );

    /*!
      @brief Destructor.
    */
    ~RV32_InstructionFactory( void );


    //
    // RV32I instruction production.
    //

    /*!
      @brief Inserts an ADD instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: ADD x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertADD( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an ADDI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: ADDI x[a] (def), x[b] (use), const12 (RRC12_1)

      Handling of offsets beyond signed 12 bits is included. Exact formats:

      LUI x[a] (def), <upper 20 bits of const12> (RC20_1) \n
      ADDI x[a] (def), x[a] (use), <lower 12 bits of const12> (RRC12_1) \n
      ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1)
    */
    void insertADDI( const WIR::RV_RegV &, const WIR::RV_RegV &, int,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an ADDI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] b A WIR_BasicBlock.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: ADDI x[a] (def), x[b] (use), const12 (RRC12_1)

      Handling of offsets beyond signed 12 bits is included. Exact formats:

      LUI x[a] (def), <upper 20 bits of const12> (RC20_1) \n
      ADDI x[a] (def), x[a] (use), <lower 12 bits of const12> (RRC12_1) \n
      ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1)
    */
    void insertADDI( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const WIR::WIR_Data &, const IR_Exp *,
                     StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an AND instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: AND x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertAND( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an ANDI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: ANDI x[a] (def), x[b] (use), const12 (RRC12_1)
    */
    void insertANDI( const WIR::RV_RegV &, const WIR::RV_RegV &, int,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an AUIPC instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] uconst20 An unsigned 20-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: AUIPC x[a] (def), uconst20 (RC20_1)
    */
    void insertAUIPC( const WIR::RV_RegV &, unsigned int,
                      const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a BEQ instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] disp12 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: BEQ x[a] (use), x[b] (use), disp12 (RRL_1)
    */
    void insertBEQ( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::WIR_BasicBlock &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a BGE instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] disp12 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: BGE x[a] (use), x[b] (use), disp12 (RRL_1)
    */
    void insertBGE( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::WIR_BasicBlock &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a BGEU instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] disp12 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: BGEU x[a] (use), x[b] (use), disp12 (RRL_1)
    */
    void insertBGEU( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const WIR::WIR_BasicBlock &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a BLT instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] disp12 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: BLT x[a] (use), x[b] (use), disp12 (RRL_1)
    */
    void insertBLT( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::WIR_BasicBlock &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a BLTU instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] disp12 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: BLTU x[a] (use), x[b] (use), disp12 (RRL_1)
    */
    void insertBLTU( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const WIR::WIR_BasicBlock &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a BNE instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] disp12 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: BNE x[a] (use), x[b] (use), disp12 (RRL_1)
    */
    void insertBNE( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::WIR_BasicBlock &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a JAL instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] disp20 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: JAL x[a] (def), disp20 (RL_1)
    */
    void insertJAL( const WIR::RV_RegV &, const WIR::WIR_BasicBlock &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a JALR instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: JALR x[a] (def), const12(x[b] (use)) (RC12R_1)
    */
    void insertJALR( const WIR::RV_RegV &, int, const WIR::RV_RegV &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a JALR instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] disp12 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: JALR x[a] (def), %lo(disp12)(x[b] (use)) (RLR_1)
    */
    void insertJALR( const WIR::RV_RegV &xa, const WIR::WIR_BasicBlock &,
                     const WIR::RV_RegV &xb,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a JALR instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: JALR x[a] (def), x[b] (use), const12 (RRC12_1)
    */
    void insertJALR( const WIR::RV_RegV &, const WIR::RV_RegV &, int,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a JALR instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] disp12 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: JALR x[a] (def), x[b] (use), %lo(disp12) (RRL_2)
    */
    void insertJALR( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const WIR::WIR_BasicBlock &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LB instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: LB x[a] (def), const12(x[b] (use)) (RC12R_1)

      Handling of address offsets beyond signed 12 bits is included. Exact
      formats:

      LUI x[a] (def), <upper 20 bits of const12> (RC20_1)
      ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1)
      LB x[a] (def), <lower 12 bits of const12>(x[a] (use)) (RC12R_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLB( const WIR::RV_RegV &, int, const WIR::RV_RegV &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LB instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] d A const reference to a data object.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: LB x[a] (def), %lo(d)(x[b] (use)) (RLR_1)
    */
    void insertLB( const WIR::RV_RegV &xa, const WIR::WIR_Data &,
                   const WIR::RV_RegV &xb,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LBU instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: LBU x[a] (def), const12(x[b] (use)) (RC12R_1)

      Handling of address offsets beyond signed 12 bits is included. Exact
      formats:

      LUI x[a] (def), <upper 20 bits of const12> (RC20_1) \n
      ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1) \n
      LBU x[a] (def), <lower 12 bits of const12>(x[a] (use)) (RC12R_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLBU( const WIR::RV_RegV &, int, const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LBU instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] d A const reference to a data object.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: LBU x[a] (def), %lo(d)(x[b] (use)) (RLR_1)
    */
    void insertLBU( const WIR::RV_RegV &, const WIR::WIR_Data &,
                    const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LH instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: LH x[a] (def), const12(x[b] (use)) (RC12R_1)

      Handling of address offsets beyond signed 12 bits is included. Exact
      formats:

      LUI x[a] (def), <upper 20 bits of const12> (RC20_1) \n
      ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1) \n
      LH x[a] (def), <lower 12 bits of const12>(x[a] (use)) (RC12R_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLH( const WIR::RV_RegV &, int, const WIR::RV_RegV &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LH instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] d A const reference to a data object.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: LH x[a] (def), %lo(d)(x[b] (use)) (RLR_1)
    */
    void insertLH( const WIR::RV_RegV &, const WIR::WIR_Data &,
                   const WIR::RV_RegV &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LHU instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: LHU x[a] (def), const12(x[b] (use)) (RC12R_1)

      Handling of address offsets beyond signed 12 bits is included. Exact
      formats:

      LUI x[a] (def), <upper 20 bits of const12> (RC20_1) \n
      ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1) \n
      LHU x[a] (def), <lower 12 bits of const12>(x[a] (use)) (RC12R_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLHU( const WIR::RV_RegV &, int, const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LHU instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] d A const reference to a data object.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: LHU x[a] (def), %lo(d)(x[b] (use)) (RLR_1)
    */
    void insertLHU( const WIR::RV_RegV &, const WIR::WIR_Data &,
                    const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LUI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] const20 An unsigned 20-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: LUI x[a] (def), const20 (RC20_1)
    */
    void insertLUI( const WIR::RV_RegV &, unsigned int,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LUI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] d A const reference to a data object.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: LUI x[a] (def), %hi(d) (RL_2)
    */
    void insertLUI( const WIR::RV_RegV &, const WIR::WIR_Data &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LW instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: LW x[a] (def), const12(x[b] (use)) (RC12R_1)

      Handling of address offsets beyond signed 12 bits is included. Exact
      formats:

      LUI x[a] (def), <upper 20 bits of const12> (RC20_1) \n
      ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1) \n
      LW x[a] (def), <lower 12 bits of const12>(x[a] (use)) (RC12R_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLW( const WIR::RV_RegV &, int, const WIR::RV_RegV &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LW instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] d A const reference to a data object.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: LW x[a] (def), %lo(d)(x[b] (use)) (RLR_1)
    */
    void insertLW( const WIR::RV_RegV &, const WIR::WIR_Data &,
                   const WIR::RV_RegV &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an OR instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: OR x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertOR( const WIR::RV_RegV &, const WIR::RV_RegV &,
                   const WIR::RV_RegV &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an ORI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: ORI x[a] (def), x[b] (use), const12 (RRC12_1)
    */
    void insertORI( const WIR::RV_RegV &, const WIR::RV_RegV &, int,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a SB instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SB x[a] (use), const12(x[b] (use)) (RC12R_2)

      Handling of address offsets beyond signed 12 bits is included. Exact
      formats:

      LUI x[z] (def), <upper 20 bits of const12> (RC20_1) \n
      ADD x[z] (def), x[z] (use), x[b] (use) (RRR_1) \n
      SB x[a] (use), <lower 12 bits of const12>(x[z] (use)) (RC12R_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSB( const WIR::RV_RegV &, int, const WIR::RV_RegV &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a SB instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] d A const reference to a data object.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SB x[a] (use), %lo(d)(x[b] (use)) (RLR_2)
    */
    void insertSB( const WIR::RV_RegV &, const WIR::WIR_Data &,
                   const WIR::RV_RegV &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a SH instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SH x[a] (use), const12(x[b] (use)) (RC12R_2)

      Handling of address offsets beyond signed 12 bits is included. Exact
      formats:

      LUI x[z] (def), <upper 20 bits of const12> (RC20_1) \n
      ADD x[z] (def), x[z] (use), x[b] (use) (RRR_1) \n
      SH x[a] (use), <lower 12 bits of const12>(x[z] (use)) (RC12R_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSH( const WIR::RV_RegV &, int, const WIR::RV_RegV &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a SH instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] d A const reference to a data object.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SH x[a] (use), %lo(d)(x[b] (use)) (RLR_2)
    */
    void insertSH( const WIR::RV_RegV &, const WIR::WIR_Data &,
                   const WIR::RV_RegV &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an SLL instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SLL x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertSLL( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an SLLI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] uconst5 An unsigned 5-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SLLI x[a] (def), x[b] (use), uconst5 (RRC5_1)
    */
    void insertSLLI( const WIR::RV_RegV &, const WIR::RV_RegV &, unsigned int,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an SLT instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SLT x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertSLT( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an SLTI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SLTI x[a] (def), x[b] (use), const12 (RRC12_1)
    */
    void insertSLTI( const WIR::RV_RegV &, const WIR::RV_RegV &, int,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an SLTIU instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SLTIU x[a] (def), x[b] (use), const12 (RRC12_1)
    */
    void insertSLTIU( const WIR::RV_RegV &, const WIR::RV_RegV &, int,
                      const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an SLTU instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SLTU x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertSLTU( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const WIR::RV_RegV &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an SRA instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SRA x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertSRA( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an SRAI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] uconst5 An unsigned 5-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SRAI x[a] (def), x[b] (use), uconst5 (RRC5_1)
    */
    void insertSRAI( const WIR::RV_RegV &, const WIR::RV_RegV &, unsigned int,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an SRL instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SRL x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertSRL( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an SRLI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] uconst5 An unsigned 5-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SRLI x[a] (def), x[b] (use), uconst5 (RRC5_1)
    */
    void insertSRLI( const WIR::RV_RegV &, const WIR::RV_RegV &, unsigned int,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a SUB instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SUB x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertSUB( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a SW instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SW x[a] (use), const12(x[b] (use)) (RC12R_2)

      Handling of address offsets beyond signed 12 bits is included. Exact
      formats:

      LUI x[z] (def), <upper 20 bits of const12> (RC20_1) \n
      ADD x[z] (def), x[z] (use), x[b] (use) (RRR_1) \n
      SW x[a] (use), <lower 12 bits of const12>(x[z] (use)) (RC12R_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSW( const WIR::RV_RegV &, int, const WIR::RV_RegV &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a SW instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] d A const reference to a data object.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: SW x[a] (use), %lo(d)(x[b] (use)) (RLR_2)
    */
    void insertSW( const WIR::RV_RegV &, const WIR::WIR_Data &,
                   const WIR::RV_RegV &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an XOR instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: XOR x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertXOR( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts an XORI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] const12 A signed 12-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: XORI x[a] (def), x[b] (use), const12 (RRC12_1)
    */
    void insertXORI( const WIR::RV_RegV &, const WIR::RV_RegV &, int,
                     const IR_Exp *, StmtType = EXP_STMT ) const;


    //
    // RV32IM instruction production.
    //

    /*!
      @brief Inserts a DIV instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: DIV x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertDIV( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a DIVU instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: DIVU x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertDIVU( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const WIR::RV_RegV &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a MUL instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: MUL x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertMUL( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a MULH instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: MULH x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertMULH( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const WIR::RV_RegV &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a MULHSU instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: MULHSU x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertMULHSU( const WIR::RV_RegV &, const WIR::RV_RegV &,
                       const WIR::RV_RegV &,
                       const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a MULHU instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: MULHU x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertMULHU( const WIR::RV_RegV &, const WIR::RV_RegV &,
                      const WIR::RV_RegV &,
                      const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a REM instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: REM x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertREM( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a REMU instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] xc A const reference to the third register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: REMU x[a] (def), x[b] (use), x[c] (use) (RRR_1)
    */
    void insertREMU( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const WIR::RV_RegV &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;


    //
    // RV32IC instruction production.
    //

    /*!
      @brief Inserts a C.ADD instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.ADD x[a] (defuse), x[b] (use) (SRR_2)

      or

      ADD x[a] (def), x[a] (use), x[b] (use) (RRR_1)
    */
    void insertCADD( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.ADDI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] const6 A signed 6-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.ADDI x[a] (defuse), const6 (SRC6_3)

      or

      ADDI x[a] (def), x[a] (use), const6 (RRC12_1)
    */
    void insertCADDI( const WIR::RV_RegV &, int,
                      const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.ADDI16SP instruction.

      @param[in] const6 A signed 6-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.ADDI16SP sp (defuse), const6 (SRC6_4)

      or

      ADDI sp (def), sp (use), const6 (RRC12_1)
    */
    void insertCADDI16SP( int,
                          const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.ADDI4SPN instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] uconst8 An unsigned 8-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.ADDI4SPN x[a] (def), sp (use), uconst8 (SRRC8_1)

      or

      ADDI x[a] (def), sp (use), uconst8 (RRC12_1)
    */
    void insertCADDI4SPN( const WIR::RV_RegV &, unsigned int,
                          const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a CALL instruction.

      @param[in] disp A const reference to the IR symbol of the function to be
                       called.
      @param[in] args A const reference to a list of registers that will be
                      added to the CALL instruction as implicitly used
                      parameters.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JAL x[1] (use), disp (RL_1)
    */
    void insertCALL( const IR_FunctionSymbol &,
                     const std::list<std::reference_wrapper<WIR::WIR_BaseRegister>> &,
                     const IR_Exp * = nullptr, StmtType = JUMP_STMT ) const;

    /*!
      @brief Inserts a CALL instruction.

      @param[in] disp A const reference to the %WIR function to be called.
      @param[in] args A const reference to a list of registers that will be
                      added to the CALL instruction as implicitly used
                      parameters.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JAL x[1] (use), disp (RL_1)
    */
    void insertCALL( const WIR::WIR_Function &,
                     const std::list<std::reference_wrapper<WIR::WIR_BaseRegister>> &,
                     const IR_Exp * = nullptr, StmtType = JUMP_STMT ) const;

    /*!
      @brief Inserts a CALLI instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] args A const reference to a list of registers that will be
                      added to the CALLI instruction as implicitly used
                      parameters.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JALR x[1], x[a] (use), 0 (RRC12_1)
    */
    void insertCALLI( const WIR::RV_RegV &,
                      const std::list<std::reference_wrapper<WIR::WIR_BaseRegister>> &,
                      const IR_Exp * = nullptr, StmtType = JUMP_STMT ) const;

    /*!
      @brief Inserts a C.AND instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.AND x[a] (defuse), x[b] (use) (SRR_2)

      or

      AND x[a] (def), x[a] (use), x[b] (use) (RRR_1)
    */
    void insertCAND( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.ANDI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] const6 A signed 6-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.ANDI x[a] (defuse), const6 (SRC6_3)

      or

      ANDI x[a] (def), x[a] (use), const6 (RRC12_1)
    */
    void insertCANDI( const WIR::RV_RegV &, int,
                      const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.BEQZ instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] disp8 A const reference to the %WIR basic block to be jumped
                       to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.BEQZ x[a] (use), disp8 (SRL_1)

      or

      BEQ x[a] (use), x0 (use), disp8 (RRL_1)
    */
    void insertCBEQZ( const WIR::RV_RegV &, const WIR::WIR_BasicBlock &,
                      const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.BNEZ instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] disp8 A const reference to the %WIR basic block to be jumped
                       to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.BNEZ x[a] (use), disp8 (SRL_1)

      or

      BNE x[a] (use), x0 (use), disp8 (RRL_1)
    */
    void insertCBNEZ( const WIR::RV_RegV &, const WIR::WIR_BasicBlock &,
                      const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.J instruction.

      @param[in] disp11 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.J disp11 (SL_1)

      or

      JAL x0 (def), disp11 (RL_1)
    */
    void insertCJ( const WIR::WIR_BasicBlock &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.JAL instruction.

      @param[in] disp11 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format:

      C.JAL disp11 (SL_1)

      or

      JAL x1 (def), disp11 (RL_1)
    */
    void insertCJAL( const WIR::WIR_BasicBlock &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.JALR instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.JALR x[a] (use) (SR_1)

      or

      JALR x1 (def), x[a] (use), 0 (RRC12_1)
    */
    void insertCJALR( const WIR::RV_RegV &,
                      const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.JR instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.JR x[a] (use) (SR_1)

      or

      JALR x0 (def), x[a] (use), 0 (RRC12_1)
    */
    void insertCJR( const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.LI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] const6 A signed 6-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.LI x[a] (def), const6 (SRC6_1)

      or

      ADDI x[a] (def), x0 (use), const6 (RRC12_1)
    */
    void insertCLI( const WIR::RV_RegV &, int,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.LUI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] uconst6 An unsigned 6-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.LUI x[a] (def), uconst6 (SRC6_2)

      or

      LUI x[a] (def), sign_extend( uconst6 ) (RC20_1)
    */
    void insertCLUI( const WIR::RV_RegV &, unsigned int,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.LW instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] uconst5 An unsigned 5-bit immediate constant.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.LW x[a] (def), uconst5(x[b] (use)) (SRC5R_1)

      or

      LW x[a] (def), uconst5(x[b] (use)) (RC12R_1)
    */
    void insertCLW( const WIR::RV_RegV &, unsigned int, const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.LWSP instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] uconst6 An unsigned 6-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: C.LWSP x[a] (def), uconst6(sp (use)) (SRC6R_1)
    */
    void insertCLWSP( const WIR::RV_RegV &, unsigned int,
                      const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.MV instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.MV x[a] (def), x[b] (use) (SRR_1)

      or

      ADDI x[a] (def), x[b] (use), 0 (RR_1)
    */
    void insertCMV( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.NOP instruction.

      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.NOP (SNULL_1)

      or

      ADDI x0 (def), x0 (use), 0 (RRC12_1)
    */
    void insertCNOP( const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.OR instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.OR x[a] (defuse), x[b] (use) (SRR_2)

      or

      OR x[a] (def), x[a] (use), x[b] (use) (RRR_1)
    */
    void insertCOR( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.SLLI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] uconst5 An unsigned 5-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.SLLI x[a] (defuse), uconst5 (SRC5_1)

      or

      SLLI x[a] (def), x[a] (use), uconst5 (RRC5_1)
    */
    void insertCSLLI( const WIR::RV_RegV &, unsigned int,
                      const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.SRAI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] uconst5 An unsigned 5-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.SRAI x[a] (defuse), uconst5 (SRC5_1)

      or

      SRAI x[a] (def), x[a] (use), uconst5 (RRC5_1)
    */
    void insertCSRAI( const WIR::RV_RegV &, unsigned int,
                      const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.SRLI instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] uconst5 An unsigned 5-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.SRLI x[a] (defuse), uconst5 (SRC5_1)

      or

      SRLI x[a] (def), x[a] (use), uconst5 (RRC5_1)
    */
    void insertCSRLI( const WIR::RV_RegV &, unsigned int,
                      const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.SUB instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.SUB x[a] (defuse), x[b] (use) (SRR_2)

      or

      SUB x[a] (def), x[a] (use), x[d] (use) (RRR_1)
    */
    void insertCSUB( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.SW instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] uconst5 An unsigned 5-bit immediate constant.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.SW x[a] (use), uconst5(x[b] (use)) (SRC5R_2)

      or

      SW x[a] (use), uconst5(x[b] (use)) (RC12R_2)
    */
    void insertCSW( const WIR::RV_RegV &, unsigned int, const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.SWSP instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] uconst6 An unsigned 6-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      C.SWSP x[a] (use), uconst6(sp (use)) (SRC6R_2)

      or

      SW x[a] (use), uconst6(sp (use)) (RC12R_2)
    */
    void insertCSWSP( const WIR::RV_RegV &, unsigned int,
                      const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a C.XOR instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format:

      C.XOR x[a] (defuse), x[b] (use) (SRR_2)

      or

      XOR x[a] (def), x[a] (use), x[b] (use) (RRR_1)
    */
    void insertCXOR( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;


    //
    // Pseudo-instruction production.
    //

    /*!
      @brief Inserts a BGT pseudo-instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] disp12 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: BLT x[b] (use), x[a] (use), disp12 (RRL_1)
    */
    void insertBGT( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::WIR_BasicBlock &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a BGTU pseudo-instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] disp12 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: BLTU x[b] (use), x[a] (use), disp12 (RRL_1)
    */
    void insertBGTU( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const WIR::WIR_BasicBlock &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a BLE pseudo-instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] disp12 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: BGE x[b] (use), x[a] (use), disp12 (RRL_1)
    */
    void insertBLE( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const WIR::WIR_BasicBlock &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a BLEU pseudo-instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] disp12 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: BGEU x[b] (use), x[a] (use), disp12 (RRL_1)
    */
    void insertBLEU( const WIR::RV_RegV &, const WIR::RV_RegV &,
                     const WIR::WIR_BasicBlock &,
                     const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a J pseudo-instruction.

      @param[in] disp20 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: JAL x0 (def), disp20 (RL_1)
    */
    void insertJ( const WIR::WIR_BasicBlock &,
                  const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a JR pseudo-instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: JALR x[0], x[a] (use), 0 (RRC12_1)
    */
    void insertJR( const WIR::RV_RegV &,
                   const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a MOV pseudo-instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: ADDI x[a] (def), x[b] (use), 0 (RR_1)
    */
    void insertMOV( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a NOP pseudo-instruction.

      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: ADDI x[0] (def), x[0] (use), 0 (RRC12_1)
    */
    void insertNOP( const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a NOT pseudo-instruction.

      @param[in] xa A const reference to the first register operand.
      @param[in] xb A const reference to the second register operand.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: XORI x[a] (def), x[b] (use), -1 (RRC12_1)
    */
    void insertNOT( const WIR::RV_RegV &, const WIR::RV_RegV &,
                    const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a RET pseudo-instruction.

      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact format: JALR x[0], x[1], 0 (RRC12_1)
    */
    void insertRETURN( const IR_Exp *, StmtType = VOID_STMT ) const;

    /*!
      @brief Inserts a RET pseudo-instruction.

      @param[in] arg A const reference to a register that will be added to the
                     RET instruction as implicitly used parameter.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JALR x[0], x[1], 0 (RRC12_1)
    */
    void insertRET( const WIR::RV_RegV &,
                    const IR_Exp * = nullptr, StmtType = RETURN_STMT ) const;

    //
    // Convenience functions.
    //

    /*!
      @brief Inserts instructions to move a constant into a register.

      @param[in] xa A const reference to the first register operand.
      @param[in] const32 A signed 32-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      The exact formats depend on the constant's value.

      For getMinSignedValue( 12 ) <= const32 <= getMaxSignedValue( 12 ):

      ADDI x[a] (def), x0 (use), const32 (RRC12_1)

      else

      LUI x[a] (def), <upper 20 bits of const32> (RC20_1) \n
      ADDI x[a] (def), x[a] (use), <lower 12 bits of const32> (RRC12_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOVConstant( const WIR::RV_RegV &, int,
                            const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief getMOVConstantCost returns the costs of instructions to move a
             constant into a register.

      @param[in] const32 A signed 32-bit immediate constant.

      The exact costs depend on the constant's value.

      For getMinSignedValue( 12 ) <= const32 <= getMaxSignedValue( 12 ):

      ADDI x[a] (def), x0 (use), const32 (RRC12_1)

      else

      LUI x[a] (def), <upper 20 bits of const32> (RC20_1) \n
      ADDI x[a] (def), x[a] (use), <lower 12 bits of const32> (RRC12_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    COST getMOVConstantCost( int ) const;

    /*!
      @brief Inserts a series of LUI and ADDI instructions.

      @param[in] xa A const reference to the first register operand.
      @param[in] const32 A signed 32-bit immediate constant.
      @param[in] exp A pointer to an IR expression to be used for the generation
                     of debug information for the newly inserted assembly
                     instruction.
      @param[in] type A specifier defaulting to EXP_STMT denoting the type of IR
                      statement to be used for the generation of debug
                      information.

      Exact formats:

      LUI x[a] (def), <upper 20 bits of const32> (RC20_1) \n
      ADDI x[a] (def), x[a] (use), <lower 12 bits of const32> (RRC12_1)
    */
    void insertLUI_ADDI( const WIR::RV_RegV &, int,
                         const IR_Exp *, StmtType = EXP_STMT ) const;

    /*!
      @brief getLUI_ADDICost returns the costs of instructions to move a
             constant into a register.

      @param[in] const32 A signed 32-bit immediate constant.

      Exact formats:

      LUI x[a] (def), <upper 20 bits of const32> (RC20_1) \n
      ADDI x[a] (def), x[a] (use), <lower 12 bits of const32> (RRC12_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    COST getLUI_ADDICost( int ) const;

    /*!
      @brief getCMVCost returns the costs of instructions for register-register
             moves.

      @return 2 bytes if the generation of 16-bit wide operations is activated,
              4 bytes otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    COST getCMVCost( void ) const;

    /*!
      @brief createReg creates a new virtual RISC-V register.

      @return A reference to the newly created register.
    */
    WIR::RV_RegV &createReg( void ) const;

    /*!
      @brief addDebugInfo generates debug information in the form of a %WIR
             comment and attaches it to the specified %WIR instruction.

      @param[in,out] i A reference to the %WIR instruction to which debug
                       information shall be attached.
      @param[in] s A const pointer to the IR statement for which assembly
                   instruction i was generated.
      @param[in] t An enum value specifying the type of IR statement s.
    */
    void addDebugInfo( WIR::WIR_Instruction &, const IR_Stmt *,
                       enum StmtType ) const;

    /*!
      @brief splitOffset splits the given address offset into its most- and
             least-significant 20- and 12-bit parts, resp.

      @param[in] o A constant signed address offset.
      @return A pair of values whose first element contains the most-significant
              20 bits of the offset and whose second element contains the least-
              significant 12 bits.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static std::pair<int, int> splitOffset( const int );


  private:

    //! mConfig holds the current compiler configuration.
    Configuration &mConfig;

    //! mCodesel refers to the current code selector instance.
    RV32_CodeSelector &mCodesel;

    /*!
      @brief m16BitOperations stores whether the code selector shall generate 16
             bits wide operations or not.
    */
    bool m16BitOperations;

};

}

#endif  // _RV32_INSTRUCTION_FACTORY_H
