/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2005 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _INSTRUCTION_FACTORY_TC179X_H
#define _INSTRUCTION_FACTORY_TC179X_H


//
// Include section
//

// Include standard headers
#include <deque>
#include <functional>
#include <list>
#include <memory>
#include <string>
#include <utility>
#include <vector>

// Include WIR headers
// We have to include the header here, since C++ does not allow the forward
// declaration of nested classes like, e.g., class TC13::AddressingMode...
#include <arch/tricore/tc13.h>
#include <arch/tricore/asmparser/tcasmargument.h>


//
// Class forward declarations
//

namespace WIR {
class WIR_BaseRegister;
class WIR_BasicBlock;
class WIR_Data;
class WIR_Function;
class WIR_Instruction;
class WIR_VirtualRegister;
class TC_ARegV;
class TC_DRegV;
class TC_ERegV;
class TC_PRegV;
}

class IR_AsmStmt;
class IR_Exp;
class IR_Function;
class IR_FunctionSymbol;
class IR_Integer;
class IR_Stmt;

class LLIR_Instruction;
class LLIR_Register;

class Configuration;
class TC179x_CodeSelector;
namespace Tricap {
class Argument;
}


/*!
  @brief Class InstructionFactory has the sole purpose of producing instruction
         objects during code selection.

  As there are masses of different instructions for the TriCore, this class was
  created to keep the code selector's interface clear, and separated from the
  instruction creation methods.
*/
class InstructionFactory
{

  public:

    /*!
      @brief This enum identifies library FPU-instruction-replacements.

      It only represents library routines and not FPU instructions.
      The mapping is performed in getSoftFloatSymbol().
    */
    enum class SoftFloatSymbol : char
    {
      ADDF,
      DIVF,
      MULF,
      SUBF,
      FEQ,
      FNEQ,
      FLT,
      FLE,
      FGT,
      FGE,
      DTOF,
      DTOI,
      DTOU,
      FTOD,
      FTOI,
      FTOU,
      ITOD,
      ITOF,
      UTOD,
      UTOF,
      LLTOF,
      LLTOD,
      ULLTOF,
      ULLTOD,
      FTOLL,
      DTOLL,
      FTOULL,
      DTOULL,
      DADD,
      DSUB,
      DMUL,
      DDIV,
      DEQ,
      DNEQ,
      DLT,
      DLE,
      DGT,
      DGE
    };

    /*!
      @brief This enum identifies library longlong-instruction-replacements.

      It only only represents library routines.
      The mapping is performed in getSoftLongLongSymbol().
    */
    enum class SoftLongLongSymbol : char
    {
      DIV,
      UDIV,
      MOD,
      UMOD
    };

    //! The enum StmtType declares the statement type for the debug information.
    enum StmtType
    {
      EXP_STMT,     // a simple statement consisting only of an expression
      IF_STMT,      // an if statament
      IFELSE_STMT,  // an if-else statement
      FOR_STMT,     // a for-loop statement
      WHILE_STMT,   // a while-loop statement
      DOWHILE_STMT, // a do-while statement
      JUMP_STMT,    // an unconditional jump statement (break,goto,continue)
      SWITCH_STMT,  // a switch statement
      VOID_STMT,    // a return-void statment
      RETURN_STMT,  // a non-void return statement
      ASM_STMT,     // an inline assembly statement
      NOT_DEFINED   // default, if the type is not defined
    };

    //! Constructor with parameter Configuration and BackAnnotation.
    InstructionFactory( Configuration &, TC179x_CodeSelector & );

    //! Destructor.
    ~InstructionFactory( void );

    /*!
      @brief setCurrentInstr sets the reference to the instruction lastly
             generated during code selection.
    */
    void setCurrentInstruction( LLIR_Instruction * );

    /*!
      @brief getCurrentInstr retrieves the reference to the instruction lastly
             generated during code selection.
    */
    LLIR_Instruction *getCurrentInstruction( void ) const;

    /*!
      @brief addDebugInfo generates debug information in the form of a LLIR
             comment and attaches it to the specified LLIR instruction.
    */
    void addDebugInfo( const IR_Stmt *, LLIR_Instruction *, enum StmtType );

    /*!
      @brief addDebugInfo generates debug information in the form of a %WIR
             comment and attaches it to the specified %WIR instruction.

      @param[in,out] i A reference to the %WIR instruction to which debug
                       information shall be attached.
      @param[in] s A const pointer to the IR statement for which assembly
                   instruction i was generated.
      @param[in] t An enum value specifying the type of IR statement s.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addDebugInfo( WIR::WIR_Instruction &i, const IR_Stmt *s,
                       enum StmtType t ) const;


    //
    // Native TriCore ISA instructions.
    //

    //! Inserts an ABS instruction.
    /*!
        Exact format: ABS D[c], D[b] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertABS( LLIR_Register *, LLIR_Register *,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ABS instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ABS D[c] (def), D[b] (use) (DD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertABS( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ADD instruction.
    /*!
        Exact format: ADD D[c], D[a], const9 (RC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertADD( LLIR_Register *, LLIR_Register *, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ADD instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ADD D[c] (def), D[a] (use), const9 (DDC9_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertADD( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ADD instruction.
    /*!
        Exact format: ADD D[c], D[a], D[b] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertADD( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ADD instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ADD D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertADD( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const WIR::TC_DRegV &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ADD instruction.
    /*!
        Exact format: ADD D[a], const4 (SRC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertADD( LLIR_Register *, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ADD instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] const4 A signed 4-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ADD D[a] (defuse), const4 (SDC4_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertADD( const WIR::TC_DRegV &, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ADD.A instruction.
    /*!
        Exact format: ADD.A A[a], const4 (SRC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertADD_A( LLIR_Register *, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ADD.A instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] const4 A signed 4-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ADD.A A[a] (defuse), const4 (SAC4_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertADD_A( const WIR::TC_ARegV &, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ADDC instruction.
    /*!
        Exact format: ADDC D[c], D[a], const9 (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertADDC( LLIR_Register *, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ADDC instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format:
      ADDC D[c] (def), D[a] (use), const9, PSW.C (defuse) (DDC9PSW_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertADDC( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ADDC instruction.
    /*!
        Exact format: ADDC D[c], D[a], D[b] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertADDC( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ADDC instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format:
      ADDC D[c] (def), D[a] (use), D[b] (use), PSW.C (defuse) (DDDPSW_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertADDC( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ADDI instruction.
    /*!
        Exact format: ADDI D[c], D[a], const16 (RLC) \n

        The last two parameters serve for generation of assembler debug information.
    */
    void insertADDI( LLIR_Register *, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ADDI instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const16 A signed 16-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ADDI D[c] (def), D[a] (use), const16 (DDC16_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertADDI( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ADDIH.A instruction.
    /*!
        Exact format: ADDIH.A A[c], A[a], const16 (RLC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertADDIH_A( LLIR_Register *, LLIR_Register *, int,
                        const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ADDIH.A instruction.

      @param[in] Ac A const reference to the first address register operand.
      @param[in] Aa A const reference to the second address register operand.
      @param[in] const16 An unsigned 16-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ADDIH.A A[c] (def), A[a] (use), const16 (AAC16)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertADDIH_A( const WIR::TC_ARegV &, const WIR::TC_ARegV &,
                        unsigned int,
                        const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ADDSC.A instruction.
    /*!
        Exact format: ADDSC.A A[c], A[b], D[a], n (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertADDSC_A( LLIR_Register *, LLIR_Register *, LLIR_Register *, int,
                        const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ADDSC.A instruction.

      @param[in] Ac A const reference to the first address register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] Da A const reference to the third data register operand.
      @param[in] const2 An unsigned 2-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ADDSC.A A[c] (def), A[b] (use), D[a] (use), const2 (AADC2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertADDSC_A( const WIR::TC_ARegV &, const WIR::TC_ARegV &,
                        const WIR::TC_DRegV &, unsigned int,
                        const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ADDX instruction.
    /*!
        Exact format: ADDX D[c], D[a], const9 (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertADDX( LLIR_Register *, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ADDX instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ADDX D[c] (def), D[a] (use), const9, PSW.C (def) (DDC9PSW_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertADDX( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ADDX instruction.
    /*!
        Exact format: ADDX D[c], D[a], D[b] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertADDX( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ADDX instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format:
      ADDX D[c] (def), D[a] (use), D[b] (use), PSW.C (def) (DDDPSW_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertADDX( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an AND instruction.
    /*!
        Exact format: AND D[c], D[a], const9 (RC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertAND( LLIR_Register *, LLIR_Register *, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an AND instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 An unsigned 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: AND D[c] (def), D[a] (use), const9 (DDC9_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertAND( const WIR::TC_DRegV &, const WIR::TC_DRegV &, unsigned int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an AND instruction.
    /*!
        Exact format: AND D[c], D[a], D[b] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertAND( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an AND instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: AND D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertAND( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const WIR::TC_DRegV &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an AND.EQ instruction.
    /*!
        Exact format: AND.EQ D[c], D[a], const9 (RC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertAND_EQ( LLIR_Register *, LLIR_Register *, int c0,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an AND.EQ instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: AND.EQ D[c] (defuse), D[a] (use), const9 (DDC9_3)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertAND_EQ( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an AND.EQ instruction.
    /*!
        Exact format: AND.EQ D[c], D[a], D[b] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertAND_EQ( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an AND.EQ instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: AND.EQ D[c] (defuse), D[a] (use), D[b] (use) (DDD_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertAND_EQ( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                       const WIR::TC_DRegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an AND.GE.U instruction.
    /*!
        Exact format: AND.GE.U D[c], D[a], D[b] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertAND_GE_U( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                         const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an AND.GE.U instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: AND.GE.U D[c] (defuse), D[a] (use), D[b] (use) (DDD_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertAND_GE_U( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                         const WIR::TC_DRegV &,
                         const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an AND.LT.U instruction.
    /*!
        Exact format: AND.LT.U D[c], D[a], D[b] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertAND_LT_U( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                         const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an AND.LT.U instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: AND.LT.U D[c] (defuse), D[a] (use), D[b] (use) (DDD_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertAND_LT_U( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                         const WIR::TC_DRegV &,
                         const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ANDN instruction.
    /*!
        Exact format: ANDN D[c], D[a], const9 (RC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertANDN( LLIR_Register *, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ANDN instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 An unsigned 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ANDN D[c] (def), D[a] (use), const9 (DDC9_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertANDN( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     unsigned int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a CALL instruction.
    /*!
        Exact format: CALL disp24 (B) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertCALL( const std::string &,
                     std::deque<LLIR_Register *> * = nullptr,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a CALL instruction.

      @param[in] disp8 A const reference to the IR symbol of the function to be
                       called.
      @param[in] args A const reference to a list of registers that will be
                      added to the CALL instruction as implicitly used
                      parameters.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: CALL disp8 (SL)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertCALL( const IR_FunctionSymbol &,
                     const std::list<std::reference_wrapper<WIR::WIR_BaseRegister>> &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a CALL instruction.

      @param[in] disp8 A const reference to the %WIR function to be called.
      @param[in] args A const reference to a list of registers that will be
                      added to the CALL instruction as implicitly used
                      parameters.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: CALL disp8 (SL)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertCALL( const WIR::WIR_Function &,
                     const std::list<std::reference_wrapper<WIR::WIR_BaseRegister>> &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a CALLI instruction.
    /*!
        Exact format: CALLI A[a] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertCALLI( LLIR_Register *r0,
                      std::deque<LLIR_Register *> * = nullptr,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

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

      Exact format: CALLI A[a] (A)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertCALLI( const WIR::TC_ARegV &,
                      const std::list<std::reference_wrapper<WIR::WIR_BaseRegister>> &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a DEXTR instruction.
    /*!
        Exact format: DEXTR D[c], D[a], D[b], pos (RRPW) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertDEXTR( LLIR_Register *, LLIR_Register *, LLIR_Register *, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a DEXTR instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] pos An unsigned 5-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: DEXTR D[c] (def), D[a] (use), D[b] (use), pos (DDDC5)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDEXTR( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &, unsigned int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a DEXTR instruction.
    /*!
        Exact format: DEXTR D[c], D[a], D[b], D[d] (RRRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertDEXTR( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a DEXTR instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] Dd A const reference to the fourth data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: DEXTR D[c] (def), D[a] (use), D[b] (use), D[d] (use) (DDDD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDEXTR( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a series of instructions for signed/unsigned byte division.
    /*!
       Exact formats:

       DVINIT.B E[c], D[a], D[b] (RR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVADJ E[c], E[c], D[b] (RRR) \n
       MOV D[d], Upper register of E[c] (SRR)

       resp.

       DVINIT.BU E[c], D[a], D[b] (RR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       MOV D[d], Upper register of E[c] (SRR)

       The last two parameters serve for generation of assembler debug
       information.
    */
    void insertDIV_B( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      bool = false,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a series of instructions for signed/unsigned byte division.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] u A Boolean denoting whether a signed or unsigned division is
                   generated.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      DVINIT.B E[x], D[a], D[b] (EDD) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVADJ E[x], E[x], D[b] (EED) \n
      MOV D[c], Even register of E[x] (MOV_RR SDD_1)

      resp.

      DVINIT.BU E[x], D[a], D[b] (EDD) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      MOV D[c], Even register of E[x] (MOV_RR SDD_1)
    */
    void insertDIV_B( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &, bool = false,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a series of instructions for signed/unsigned halfword division.
    /*!
       Exact formats:

       DVINIT.H E[c], D[a], D[b] (RR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVADJ E[c], E[c], D[b] (RRR) \n
       MOV D[d], Upper register of E[c] (SRR)

       resp.

       DVINIT.HU E[c], D[a], D[b] (RR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       MOV D[d], Upper register of E[c] (SRR)

       The last two parameters serve for generation of assembler debug
       information.
    */
    void insertDIV_H( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      bool = false,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a series of instructions for signed/unsigned halfword
             division.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] u A Boolean denoting whether a signed or unsigned division is
                   generated.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      DVINIT.H E[x], D[a], D[b] (EDD) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVADJ E[x], E[x], D[b] (EED) \n
      MOV D[c], Even register of E[x] (MOV_RR SDD_1)

      resp.

      DVINIT.HU E[x], D[a], D[b] (EDD) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      MOV D[c], Even register of E[x] (MOV_RR SDD_1)
    */
    void insertDIV_H( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &, bool = false,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a series of instructions for signed/unsigned integer division.
    /*!
       Exact formats:

       DVINIT E[c], D[a], D[b] (RR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVADJ E[c], E[c], D[b] (RRR) \n
       MOV D[d], Upper register of E[c] (SRR)

       resp.

       DVINIT.U E[c], D[a], D[b] (RR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       MOV D[d], Upper register of E[c] (SRR)

       The last two parameters serve for generation of assembler debug
       information.
    */
    void insertDIV_W( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      bool = false,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a series of instructions for signed/unsigned integer
             division.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] u A Boolean denoting whether a signed or unsigned division is
                   generated.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      DVINIT E[x], D[a], D[b] (EDD) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVADJ E[x], E[x], D[b] (EED) \n
      MOV D[c], Even register of E[x] (MOV_RR SDD_1)

      resp.

      DVINIT.U E[x], D[a], D[b] (EDD) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      MOV D[c], Even register of E[x] (MOV_RR SDD_1)
    */
    void insertDIV_W( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &, bool = false,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an EQ instruction.
    /*!
        Exact format: EQ D[c], D[a], const9 (RC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertEQ( LLIR_Register *, LLIR_Register *, int,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an EQ instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: EQ D[c] (def), D[a] (use), const9 (DDC9_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertEQ( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an EQ instruction.
    /*!
        Exact format: EQ D[c], D[a], D[b] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertEQ( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an EQ instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: EQ D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertEQ( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                   const WIR::TC_DRegV &,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an EQ.A instruction.
    /*!
        Exact format: EQ.A D[c], A[a], A[b] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertEQ_A( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an EQ.A instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Aa A const reference to the second data register operand.
      @param[in] Ab A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: EQ.A D[c] (def), A[a] (use), A[b] (use) (DAA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertEQ_A( const WIR::TC_DRegV &, const WIR::TC_ARegV &,
                     const WIR::TC_ARegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an EQZ.A instruction.
    /*!
        Exact format: EQZ.A D[c], A[a] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertEQZ_A( LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an EQZ.A instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Aa A const reference to the second address register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: EQZ.A D[c] (def), A[a] (use) (DA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertEQZ_A( const WIR::TC_DRegV &, const WIR::TC_ARegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an EXTR instruction.
    /*!
        Exact format: EXTR D[c], D[a], pos, width (RRPW) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertEXTR( LLIR_Register *, LLIR_Register *, int, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an EXTR instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] pos An unsigned 5-bit immediate constant.
      @param[in] width An unsigned 5-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: EXTR D[c] (def), D[a] (use), pos, width (DDC5C5)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertEXTR( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     unsigned int, unsigned int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an EXTR.U instruction.
    /*!
        Exact format: EXTR.U D[c], D[a], pos, width (RRPW) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertEXTR_U( LLIR_Register *, LLIR_Register *, int, int,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an EXTR.U instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] pos An unsigned 5-bit immediate constant.
      @param[in] width An unsigned 5-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: EXTR.U D[c] (def), D[a] (use), pos, width (DDC5C5)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertEXTR_U( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                       unsigned int, unsigned int,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a GE instruction.
    /*!
        Exact format: GE D[c], D[a], const9 (RC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertGE( LLIR_Register *, LLIR_Register *, int,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a GE instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: GE D[c] (def), D[a] (use), const9 (DDC9_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertGE( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a GE instruction.
    /*!
        Exact format: GE D[c], D[a], D[b] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertGE( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a GE instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: GE D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertGE( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                   const WIR::TC_DRegV &,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a GE_A instruction.
    /*!
        Exact format: GE.A D[c], A[a], A[b] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertGE_A( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a GE.A instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Aa A const reference to the second data register operand.
      @param[in] Ab A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: GE.A D[c] (def), A[a] (use), A[b] (use) (DAA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertGE_A( const WIR::TC_DRegV &, const WIR::TC_ARegV &,
                     const WIR::TC_ARegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a GE.U instruction.
    /*!
        Exact format: GE.U D[c], D[a], const9 (RC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertGE_U( LLIR_Register *, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a GE.U instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 An unsigned 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: GE.U D[c] (def), D[a] (use), const9 (DDC9_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertGE_U( const WIR::TC_DRegV &, const WIR::TC_DRegV &, unsigned int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a GE.U instruction.
    /*!
        Exact format: GE.U D[c], D[a], D[b] (RR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertGE_U( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a GE.U instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: GE.U D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertGE_U( const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an IMASK instruction.
    /*!
        Exact format: IMASK E[c], const4, pos, width (RCPW) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertIMASK( LLIR_Register *, int, int, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an IMASK instruction.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] const4 An unsigned 4-bit immediate constant.
      @param[in] pos An unsigned 5-bit immediate constant.
      @param[in] width An unsigned 5-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: IMASK E[c], const4, pos, width (EC4C5C5)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertIMASK( const WIR::TC_ERegV &, unsigned int, unsigned int,
                      unsigned int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an IMASK instruction.
    /*!
        Exact format: IMASK E[c], const4, D[d], width (RCRW) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertIMASK( LLIR_Register *, int, LLIR_Register *, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an IMASK instruction.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] const4 An unsigned 4-bit immediate constant.
      @param[in] Dd A const reference to the second data register operand.
      @param[in] width An unsigned 5-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: IMASK E[c], const4, D[d] (use), width (EC4DC5)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertIMASK( const WIR::TC_ERegV &, unsigned int,
                      const WIR::TC_DRegV &, unsigned int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an IMASK instruction.
    /*!
        Exact format: IMASK E[c], D[b], pos, width (RRPW) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertIMASK( LLIR_Register *, LLIR_Register *, int, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an IMASK instruction.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] pos An unsigned 5-bit immediate constant.
      @param[in] width An unsigned 5-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: IMASK E[c], D[b] (use), pos, width (EDC5C5)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertIMASK( const WIR::TC_ERegV &, const WIR::TC_DRegV &,
                      unsigned int, unsigned int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an IMASK instruction.
    /*!
        Exact format: IMASK E[c], D[b], D[d], width (RRRW) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertIMASK( LLIR_Register *, LLIR_Register *, LLIR_Register *, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an IMASK instruction.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] Dd A const reference to the third data register operand.
      @param[in] width An unsigned 5-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: IMASK E[c], D[b] (use), D[d] (use), width (EDDC5)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertIMASK( const WIR::TC_ERegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &, unsigned int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an INSERT instruction.
    /*!
        Exact format: INSERT D[c], D[a], const4, pos, width (RCPW) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertINSERT( LLIR_Register *, LLIR_Register *, int, int, int,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an INSERT instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const4 An unsigned 4-bit immediate constant.
      @param[in] pos An unsigned 5-bit immediate constant.
      @param[in] width An unsigned 5-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: INSERT D[c] (def), D[a] (use), const4, pos, width (DDC4C5C5)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertINSERT( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                       unsigned int, unsigned int, unsigned int,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an INSERT instruction.
    /*!
        Exact format: INSERT D[c], D[a], D[b], pos, width (RRPW) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertINSERT( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                       int, int,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an INSERT instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] pos An unsigned 5-bit immediate constant.
      @param[in] width An unsigned 5-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: INSERT D[c] (def), D[a] (use), D[b] (use), pos, width
                    (DDDC5C5)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertINSERT( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                       const WIR::TC_DRegV &, unsigned int, unsigned int,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a J instruction.
    /*!
        Exact format: J disp24 (B) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJ( const std::string &,
                  const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a J instruction.

      @param[in] disp8 A const reference to the %WIR basic block to be jumped
                       to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: J disp8 (SL)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJ( const WIR::WIR_BasicBlock &,
                  const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JEQ instruction.
    /*!
        Exact format: JEQ D[a], const4, disp15 (BRC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJEQ( LLIR_Register *, int, const std::string &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JEQ instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] const4 A signed 4-bit immediate constant.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JEQ D[a] (use), const4, disp15 (DC4L_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJEQ( const WIR::TC_DRegV &, int, const WIR::WIR_BasicBlock &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JEQ instruction.
    /*!
        Exact format: JEQ D[a], D[b], disp15 (BRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJEQ( LLIR_Register *, LLIR_Register *, const std::string &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JEQ instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JEQ D[a] (use), D[b] (use), disp15 (DDL_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJEQ( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const WIR::WIR_BasicBlock &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JEQ.A instruction.
    /*!
        Exact format: JEQ.A A[a], A[b], disp15 (BRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJEQ_A( LLIR_Register *, LLIR_Register *, const std::string &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JEQ.A instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JEQ.A A[a] (use), A[b] (use), disp15 (AAL)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJEQ_A( const WIR::TC_ARegV &, const WIR::TC_ARegV &,
                      const WIR::WIR_BasicBlock &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JGE instruction.
    /*!
        Exact format: JGE D[a], const4, disp15 (BRC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJGE( LLIR_Register *, int, const std::string &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JGE instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] const4 A signed 4-bit immediate constant.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JGE D[a] (use), const4, disp15 (DC4L_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJGE( const WIR::TC_DRegV &, int, const WIR::WIR_BasicBlock &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JGE instruction.
    /*!
        Exact format: JGE D[a], D[b], disp15 (BRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJGE( LLIR_Register *, LLIR_Register *, const std::string &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JGE instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JGE D[a] (use), D[b] (use), disp15 (DDL_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJGE( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const WIR::WIR_BasicBlock &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JGE.U instruction.
    /*!
        Exact format: JGE.U D[a], const4, disp15 (BRC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJGE_U( LLIR_Register *, int, const std::string &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JGE.U instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] const4 An unsigned 4-bit immediate constant.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JGE.U D[a] (use), const4, disp15 (DC4L_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJGE_U( const WIR::TC_DRegV &, unsigned int,
                      const WIR::WIR_BasicBlock &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JGE.U instruction.
    /*!
        Exact format: JGE.U D[a], D[b], disp15 (BRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJGE_U( LLIR_Register *, LLIR_Register *, const std::string &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JGE.U instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JGE.U D[a] (use), D[b] (use), disp15 (DDL_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJGE_U( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::WIR_BasicBlock &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JGEZ instruction.
    /*!
        Exact format: JGEZ D[b], disp4 (SBR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJGEZ( LLIR_Register *, const std::string &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JGEZ instruction.

      @param[in] Db A const reference to the first data register operand.
      @param[in] disp4 A const reference to the %WIR basic block to be jumped
                       to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JGEZ D[b] (use), disp4 (SDL)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJGEZ( const WIR::TC_DRegV &, const WIR::WIR_BasicBlock &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JGTZ instruction.
    /*!
        Exact format: JGTZ D[b], disp4 (SBR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJGTZ( LLIR_Register *, const std::string &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JGTZ instruction.

      @param[in] Db A const reference to the first data register operand.
      @param[in] disp4 A const reference to the %WIR basic block to be jumped
                       to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JGTZ D[b] (use), disp4 (SDL)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJGTZ( const WIR::TC_DRegV &, const WIR::WIR_BasicBlock &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JLEZ instruction.
    /*!
        Exact format: JLEZ D[b], disp4 (SBR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJLEZ( LLIR_Register *, const std::string &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JLEZ instruction.

      @param[in] Db A const reference to the first data register operand.
      @param[in] disp4 A const reference to the %WIR basic block to be jumped
                       to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JLEZ D[b] (use), disp4 (SDL)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJLEZ( const WIR::TC_DRegV &, const WIR::WIR_BasicBlock &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JLT instruction.
    /*!
        Exact format: JLT D[a], const4, disp15 (BRC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJLT( LLIR_Register *, int, const std::string &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JLT instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] const4 A signed 4-bit immediate constant.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JLT D[a] (use), const4, disp15 (DC4L_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJLT( const WIR::TC_DRegV &, int, const WIR::WIR_BasicBlock &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JLT instruction.
    /*!
        Exact format: JLT D[a], D[b], disp15 (BRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJLT( LLIR_Register *, LLIR_Register *, const std::string &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JLT instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JLT D[a] (use), D[b] (use), disp15 (DDL_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJLT( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const WIR::WIR_BasicBlock &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JLT.U instruction.
    /*!
        Exact format: JLT.U D[a], const4, disp15 (BRC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJLT_U( LLIR_Register *, int, const std::string &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JLT.U instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] const4 An unsigned 4-bit immediate constant.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JLT.U D[a] (use), const4, disp15 (DC4L_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJLT_U( const WIR::TC_DRegV &, unsigned int,
                      const WIR::WIR_BasicBlock &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JLT.U instruction.
    /*!
        Exact format: JLT.U D[a], D[b], disp15 (BRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJLT_U( LLIR_Register *, LLIR_Register *, const std::string &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JLT.U instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JLT.U D[a] (use), D[b] (use), disp15 (DDL_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJLT_U( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::WIR_BasicBlock &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JLTZ instruction.
    /*!
        Exact format: JLTZ D[b], disp4 (SBR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJLTZ( LLIR_Register *, const std::string &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JLTZ instruction.

      @param[in] Db A const reference to the first data register operand.
      @param[in] disp4 A const reference to the %WIR basic block to be jumped
                       to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JLTZ D[b] (use), disp4 (SDL)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJLTZ( const WIR::TC_DRegV &, const WIR::WIR_BasicBlock &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JNE instruction.
    /*!
        Exact format: JNE D[a], const4, disp15 (BRC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJNE( LLIR_Register *, int, const std::string &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JNE instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] const4 A signed 4-bit immediate constant.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JNE D[a] (use), const4, disp15 (DC4L_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJNE( const WIR::TC_DRegV &, int, const WIR::WIR_BasicBlock &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JNE instruction.
    /*!
        Exact format: JNE D[a], D[b], disp15 (BRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJNE( LLIR_Register *, LLIR_Register *, const std::string &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JNE instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JNE D[a] (use), D[b] (use), disp15 (DDL_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJNE( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const WIR::WIR_BasicBlock &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JNE.A instruction.
    /*!
        Exact format: JNE.A A[a], A[b], disp15 (BRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJNE_A( LLIR_Register *, LLIR_Register *, const std::string &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JNE.A instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JNE.A A[a] (use), A[b] (use), disp15 (AAL)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJNE_A( const WIR::TC_ARegV &, const WIR::TC_ARegV &,
                      const WIR::WIR_BasicBlock &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JNED instruction.
    /*!
        Exact format: JNED D[a], const4, disp15 (BRC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJNED( LLIR_Register *, int, const std::string &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JNED instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] const4 A signed 4-bit immediate constant.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JNED D[a] (defuse), const4, disp15 (DC4L_3)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJNED( const WIR::TC_DRegV &, int, const WIR::WIR_BasicBlock &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JNED instruction.
    /*!
        Exact format: JNED D[a], D[b], disp15 (BRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJNED( LLIR_Register *, LLIR_Register *, const std::string &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JNED instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JNED D[a] (defuse), D[b] (use), disp15 (DDL_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJNED( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::WIR_BasicBlock &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JNEI instruction.
    /*!
        Exact format: JNEI D[a], const4, disp15 (BRC) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJNEI( LLIR_Register *, int, const std::string &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JNEI instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] const4 A signed 4-bit immediate constant.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JNEI D[a] (defuse), const4, disp15 (DC4L_3)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJNEI( const WIR::TC_DRegV &, int, const WIR::WIR_BasicBlock &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JNEI instruction.
    /*!
        Exact format: JNEI D[a], D[b], disp15 (BRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJNEI( LLIR_Register *, LLIR_Register *, const std::string &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JNEI instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JNEI D[a] (defuse), D[b] (use), disp15 (DDL_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJNEI( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::WIR_BasicBlock &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JNZ instruction.
    /*!
        Exact format: JNZ D[b], disp4 (SBR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJNZ( LLIR_Register *, const std::string &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JNZ instruction.

      @param[in] Db A const reference to the first data register operand.
      @param[in] disp4 A const reference to the %WIR basic block to be jumped
                       to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JNZ D[b] (use), disp4 (SDL)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJNZ( const WIR::TC_DRegV &, const WIR::WIR_BasicBlock &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JNZ.A instruction.
    /*!
        Exact format: JNZ.A A[a], disp15 (BRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJNZ_A( LLIR_Register *, const std::string &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JNZ.A instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] disp4 A const reference to the %WIR basic block to be jumped
                       to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JNZ.A A[a] (use), disp4 (SAL_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJNZ_A( const WIR::TC_ARegV &, const WIR::WIR_BasicBlock &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JNZ.T instruction.
    /*!
        Exact format: JNZ.T D[a], n, disp15 (BRN) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJNZ_T( LLIR_Register *, int, const std::string &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JNZ.T instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] n An unsigned 5-bit immediate constant.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JNZ.T D[a] (use), n, disp15 (DC5L)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJNZ_T( const WIR::TC_DRegV &, unsigned int,
                      const WIR::WIR_BasicBlock &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JZ instruction.
    /*!
        Exact format: JZ D[b], disp4 (SBR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJZ( LLIR_Register *, const std::string &,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JZ instruction.

      @param[in] Db A const reference to the first data register operand.
      @param[in] disp4 A const reference to the %WIR basic block to be jumped
                       to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JZ D[b] (use), disp4 (SDL)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJZ( const WIR::TC_DRegV &, const WIR::WIR_BasicBlock &,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JZ.A instruction.
    /*!
        Exact format: JZ.A A[a], disp15 (BRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJZ_A( LLIR_Register *, const std::string &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JZ.A instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] disp4 A const reference to the %WIR basic block to be jumped
                       to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JZ.A A[a] (use), disp4 (SAL_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJZ_A( const WIR::TC_ARegV &, const WIR::WIR_BasicBlock &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a JZ.T instruction.
    /*!
        Exact format: JZ.T D[a], n, disp15 (BRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertJZ_T( LLIR_Register *, int, const std::string &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a JZ.T instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] n An unsigned 5-bit immediate constant.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: JZ.T D[a] (use), n, disp15 (DC5L)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertJZ_T( const WIR::TC_DRegV &, unsigned int,
                     const WIR::WIR_BasicBlock &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LD.A instruction.
    /*!
        Exact formats:

        LD.A A[a], [+Ab]off10 (BO + Pre-increment Addressing) \n
        LD.A A[a], [Ab+]off10 (BO + Post-increment Addressing) \n
        LD.A A[a], [Ab]off16 (BOL + Base+Long Offset Addressing) \n
        LD.A A[a], [Ab]off10 (BO + Base+Short Offset Addressing)

        Handling of large address offsets is included.

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLD_A( LLIR_Register *, const std::string &, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LD.A instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LD.A A[a] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is included. Exact
      formats:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.A A[a] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_A( const WIR::TC_ARegV &, const WIR::TC_ARegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LD.A instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      LD.A A[a] (def), [+A[b] (defuse)]off (AAC10PIA) \n
      LD.A A[a] (def), [A[b] (defuse)+]off (AAC10PIA)

      Handling of address offsets larger than signed 10 bits but within signed
      16 bits is included. Exact formats for pre-increment:

      LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
      LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)

      Exact formats for post-increment:

      LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA) \n
      LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is also included. Exact
      formats for pre-increment either:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.A A[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (AAC10PIA)

      or:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)

      Exact formats for post-increment either:

      LD.A A[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (AAC10PIA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

      or:

      LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_A( const WIR::TC_ARegV &, const WIR::TC13::AddressingMode &,
                     const WIR::TC_ARegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LD.A instruction.
    /*!
        Exact formats:

        LD.A A[a], [+Ab] HI:label (BO + Pre-increment Addressing) \n
        LD.A A[a], [Ab+] HI:label (BO + Post-increment Addressing) \n
        LD.A A[a], [Ab] HI:label (BOL + Base+Long Offset Addressing) \n
        LD.A A[a], [Ab] HI:label (BO + Base+Short Offset Addressing) \n
        LD.A A[a], [+Ab] LO:label (BO + Pre-increment Addressing) \n
        LD.A A[a], [Ab+] LO:label (BO + Post-increment Addressing) \n
        LD.A A[a], [Ab] LO:label (BOL + Base+Long Offset Addressing) \n
        LD.A A[a], [Ab] LO:label (BO + Base+Short Offset Addressing) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLD_A( LLIR_Register *, const std::string &, LLIR_Register *,
                     const std::string &, const std::string &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LD.A instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] d A const reference to a data object.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LD.A A[a] (def), [A[b] (use)] LO:label (AALBOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_A( const WIR::TC_ARegV &, const WIR::TC_ARegV &,
                     const WIR::WIR_Data &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LD.B instruction.
    /*!
        Exact formats:

        LD.B D[a], [+Ab]off10 (BO + Pre-increment Addressing) \n
        LD.B D[a], [Ab+]off10 (BO + Post-increment Addressing) \n
        LD.B D[a], [Ab]off10 (BO + Base+Short Offset Addressing)

        Handling of large address offsets is included.

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLD_B( LLIR_Register *, const std::string &, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LD.B instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LD.B D[a] (def), [A[b] (use)]off (DAC10BOA)

      Handling of address offsets beyond signed 10 bits is included. Exact
      formats either:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.B D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

      or:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LD.B D[a] (def), [A[x] (use)]0 (DAC10BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_B( const WIR::TC_DRegV &, const WIR::TC_ARegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LD.B instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      LD.B D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
      LD.B D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

      Handling of address offsets larger than signed 10 bits but within signed
      16 bits is included. Exact formats for pre-increment:

      LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
      LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA)

      Exact formats for post-increment:

      LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
      LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is also included. Exact
      formats for pre-increment either:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.B D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

      or:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA)

      Exact formats for post-increment either:

      LD.B D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

      or:

      LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_B( const WIR::TC_DRegV &, const WIR::TC13::AddressingMode &,
                     const WIR::TC_ARegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LD.BU instruction.
    /*!
        Exact formats:

        LD.BU D[a], [+Ab]off10 (BO + Pre-increment Addressing) \n
        LD.BU D[a], [Ab+]off10 (BO + Post-increment Addressing) \n
        LD.BU D[a], [Ab]off10 (BO + Base+Short Offset Addressing)

        Handling of large address offsets is included.

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLD_BU( LLIR_Register *, const std::string &, LLIR_Register *,
                      int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LD.BU instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LD.BU D[a] (def), [A[b] (use)]off (DAC10BOA)

      Handling of address offsets beyond signed 10 bits is included. Exact
      formats either:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.BU D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

      or:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LD.BU D[a] (def), [A[x] (use)]0 (DAC10BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_BU( const WIR::TC_DRegV &, const WIR::TC_ARegV &, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LD.BU instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      LD.BU D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
      LD.BU D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

      Handling of address offsets larger than signed 10 bits but within signed
      16 bits is included. Exact formats for pre-increment:

      LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
      LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA)

      Exact formats for post-increment:

      LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
      LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is also included. Exact
      formats for pre-increment either:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.BU D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

      or:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA)

      Exact formats for post-increment either:

      LD.BU D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

      or:

      LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_BU( const WIR::TC_DRegV &, const WIR::TC13::AddressingMode &,
                      const WIR::TC_ARegV &, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LD.D instruction.
    /*!
        Exact formats:

        LD.D E[a], [+Ab]off10 (BO + Pre-increment Addressing) \n
        LD.D E[a], [Ab+]off10 (BO + Post-increment Addressing) \n
        LD.D E[a], [Ab]off10 (BO + Base+Short Offset Addressing)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLD_D( LLIR_Register *, const std::string &, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LD.D instruction.

      @param[in] Ea A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LD.D D[a] (def), [A[b] (use)]off (EAC10BOA)

      Handling of address offsets beyond signed 10 bits is included. Exact
      formats either:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.D E[a] (def), [A[x] (use)]<lower 10 bits of off> (EAC10BOA)

      or:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LD.D E[a] (def), [A[x] (use)]0 (EAC10BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_D( const WIR::TC_ERegV &, const WIR::TC_ARegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LD.D instruction.

      @param[in] Ea A const reference to the first extended data register
                    operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      LD.D E[a] (def), [+A[b] (defuse)]off (EAC10PIA) \n
      LD.D E[a] (def), [A[b] (defuse)+]off (EAC10PIA)

      Handling of address offsets larger than signed 10 bits but within signed
      16 bits is included. Exact formats for pre-increment:

      LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
      LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA)

      Exact formats for post-increment:

      LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA) \n
      LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is also included. Exact
      formats for pre-increment either:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.D E[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (EAC10PIA)

      or:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA)

      Exact formats for post-increment either:

      LD.D E[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (EAC10PIA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

      or:

      LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_D( const WIR::TC_ERegV &, const WIR::TC13::AddressingMode &,
                     const WIR::TC_ARegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LD.H instruction.
    /*!
        Exact formats:

        LD.H D[a], [+Ab]off10 (BO + Pre-increment Addressing) \n
        LD.H D[a], [Ab+]off10 (BO + Post-increment Addressing) \n
        LD.H D[a], [Ab]off10 (BO + Base+Short Offset Addressing)

        Handling of large address offsets is included.

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLD_H( LLIR_Register *, const std::string &, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LD.H instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LD.H D[a] (def), [A[b] (use)]off (DAC10BOA)

      Handling of address offsets beyond signed 10 bits is included. Exact
      formats either:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.H D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

      or:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LD.H D[a] (def), [A[x] (use)]0 (DAC10BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_H( const WIR::TC_DRegV &, const WIR::TC_ARegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LD.H instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      LD.H D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
      LD.H D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

      Handling of address offsets larger than signed 10 bits but within signed
      16 bits is included. Exact formats for pre-increment:

      LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
      LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA)

      Exact formats for post-increment:

      LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
      LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is also included. Exact
      formats for pre-increment either:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.H D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

      or:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA)

      Exact formats for post-increment either:

      LD.H D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

      or:

      LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_H( const WIR::TC_DRegV &, const WIR::TC13::AddressingMode &,
                     const WIR::TC_ARegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LD.HU instruction.
    /*!
        Exact formats:

        LD.HU D[a], [+Ab]off10 (BO + Pre-increment Addressing) \n
        LD.HU D[a], [Ab+]off10 (BO + Post-increment Addressing) \n
        LD.HU D[a], [Ab]off10 (BO + Base+Short Offset Addressing)

        Handling of large address offsets is included.

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLD_HU( LLIR_Register *, const std::string &, LLIR_Register *,
                      int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LD.HU instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LD.HU D[a] (def), [A[b] (use)]off (DAC10BOA)

      Handling of address offsets beyond signed 10 bits is included. Exact
      formats either:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.HU D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

      or:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LD.HU D[a] (def), [A[x] (use)]0 (DAC10BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_HU( const WIR::TC_DRegV &, const WIR::TC_ARegV &, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LD.HU instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      LD.HU D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
      LD.HU D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

      Handling of address offsets larger than signed 10 bits but within signed
      16 bits is included. Exact formats for pre-increment:

      LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
      LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA)

      Exact formats for post-increment:

      LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
      LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is also included. Exact
      formats for pre-increment either:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.HU D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

      or:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA)

      Exact formats for post-increment either:

      LD.HU D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

      or:

      LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_HU( const WIR::TC_DRegV &, const WIR::TC13::AddressingMode &,
                      const WIR::TC_ARegV &, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a  LD.W instruction.
    /*!
        Exact formats:

        LD.W D[a], [+Ab]off10 (BO + Pre-increment Addressing) \n
        LD.W D[a], [Ab+]off10 (BO + Post-increment Addressing) \n
        LD.W D[a], [Ab]off16 (BOL + Base+Long Offset Addressing) \n
        LD.W D[a], [Ab]off10 (BO + Base+Short Offset Addressing)

        Handling of large address offsets is included.

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLD_W( LLIR_Register *, const std::string &, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LD.W instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LD.W D[a] (def), [A[b] (use)]off (DAC16BOA)

      Handling of address offsets beyond signed 16 bits is included. Exact
      formats:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.W D[a] (def), [A[x] (use)]<lower 16 bits of off> (DAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_W( const WIR::TC_DRegV &, const WIR::TC_ARegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LD.W instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      LD.W D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
      LD.W D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

      Handling of address offsets larger than signed 10 bits but within signed
      16 bits is included. Exact formats for pre-increment:

      LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
      LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA)

      Exact formats for post-increment:

      LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA) \n
      LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is also included. Exact
      formats for pre-increment either:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.W D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

      or:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA)

      Exact formats for post-increment either:

      LD.W D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

      or:

      LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_W( const WIR::TC_DRegV &, const WIR::TC13::AddressingMode &,
                     const WIR::TC_ARegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LD.W instruction.
    /*!
        Exact formats:

        LD.W D[a], [+Ab] HI:label (BO + Pre-increment Addressing) \n
        LD.W D[a], [Ab+] HI:label (BO + Post-increment Addressing) \n
        LD.W D[a], [Ab] HI:label (BOL + Base+Long Offset Addressing) \n
        LD.W D[a], [Ab] HI:label (BO + Base+Short Offset Addressing) \n
        LD.W D[a], [+Ab] LO:label (BO + Pre-increment Addressing) \n
        LD.W D[a], [Ab+] LO:label (BO + Post-increment Addressing) \n
        LD.W D[a], [Ab] LO:label (BOL + Base+Long Offset Addressing) \n
        LD.W D[a], [Ab] LO:label (BO + Base+Short Offset Addressing)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLD_W( LLIR_Register *, const std::string &, LLIR_Register *,
                     const std::string &, const std::string &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LD.W instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] d A const reference to a data object.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LD.W D[a] (def), [A[b] (use)] LO:label (DALBOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_W( const WIR::TC_DRegV &, const WIR::TC_ARegV &,
                     const WIR::WIR_Data &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LDMST instruction.
    /*!
        Exact formats:

        LDMST [+Ab]off10 E[a] (BO + Pre-increment Addressing) \n
        LDMST [Ab+]off10 E[a] (BO + Post-increment Addressing) \n
        LDMST [Ab]off10 E[a] (BO + Base+Short Offset Addressing)

        Handling of large address offsets is included.

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLDMST( const std::string &, LLIR_Register *, int,
                      LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LDMST instruction.

      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] Ea A const reference to the first extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LDMST [A[b] (use)]off, Ea (use) (AC10EBOA)

      Handling of address offsets beyond signed 10 bits is included. Exact
      formats either:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LDMST [A[x] (use)]<lower 10 bits of off>, Ea (use) (AC10EBOA)

      or:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LDMST [A[x] (use)]0, Ea (use) (AC10EBOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLDMST( const WIR::TC_ARegV &, int, const WIR::TC_ERegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LDMST instruction.

      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the first address register operand.
      @param[in] off A signed immediate offset.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      LDMST [+A[b] (defuse)]off, E[a] (use) (AC10EPIA) \n
      LDMST [A[b] (defuse)+]off, E[a] (use) (AC10EPIA)

      Handling of address offsets larger than signed 10 bits but within signed
      16 bits is included. Exact formats for pre-increment:

      LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
      LDMST [A[b] (use)]0, E[a] (use) (AC10EBOA)

      Exact formats for post-increment:

      LDMST [A[b] (use)]0, E[a] (use) (AC10EBOA) \n
      LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is also included. Exact
      formats for pre-increment either:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LDMST [+A[b] (defuse)]<lower 10 bits of off>, E[a] (use) (AC10EPIA)

      or:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
      LDMST [A[b] (use)]0, E[a] (use) (AC10EBOA)

      Exact formats for post-increment either:

      LDMST [A[b] (defuse)+]<lower 10 bits of off>, E[a] (use) (AC10EPIA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

      or:

      LDMST [A[b] (use)]0, E[a] (use) (AC10EBOA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLDMST( const WIR::TC13::AddressingMode &, const WIR::TC_ARegV &,
                      int, const WIR::TC_ERegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Depending on the constant, inserts a LEA instruction or a a sequence of instructions
    /*!
        Exact formats:

        for 0 <= c0 <= offset18: \n
        LEA A[a], off18 (ABS)

        for c0 != offset18: \n
        MOVH.A A[c], upper 16 bits of c0 (RLC) \n
        LEA A[c], [Ac] lower 16 bits of c0 (BOL)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLEA( LLIR_Register *, long long,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LEA instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LEA A[a] (def), off (AC18ABSA)

      Handling of address offsets incompatible with TriCore's offset18 format is
      included. Exact formats:

      MOVH.A A[a] (def), <upper 16 bits of off> (AC16) \n
      LEA A[a] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLEA( const WIR::TC_ARegV &, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Depending on the constant, inserts a LEA instruction or a a sequence of instructions
    /*!
        Exact formats:
        for -16bit <= c0 <= 16bit:

        LEA A[a], [Ab] off16 (BOL) \n

        for 16bit < c0: \n
        MOV.AA A[a], A[b] (SRR) \n
        ADDIH.A A[a], A[a], upper 16 bits of c0 (RLC) \n
        LEA A[a], [Aa] lower 16 bits of c0 (BOL) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLEA( LLIR_Register *, const std::string &, LLIR_Register *, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LEA instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LEA A[a] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is included. Exact
      formats:

      MOV.AA A[a] (def), A[b] (use) (SAA_1) \n
      ADDIH.A A[a] (def), A[a] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[a] (def), [A[a] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLEA( const WIR::TC_ARegV &, const WIR::TC_ARegV &, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LEA instruction.
    /*!
        Exact formats:

        LEA A[a], [Ab] HI:label (BOL) \n
        LEA A[a], [Ab] LO:label (BOL)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLEA( LLIR_Register *, const std::string &, LLIR_Register *,
                    const std::string &, const std::string &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LEA instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] b A const reference to a basic block.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LEA A[a] (def), [A[b] (use)] LO:label (AALBOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLEA( const WIR::TC_ARegV &, const WIR::TC_ARegV &,
                    const WIR::WIR_BasicBlock &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LEA instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] d A const reference to a data object.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LEA A[a] (def), [A[b] (use)] LO:label (AALBOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLEA( const WIR::TC_ARegV &, const WIR::TC_ARegV &,
                    const WIR::WIR_Data &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a LEA instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] f A const reference to a function.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LEA A[a] (def), [A[b] (use)] LO:label (AALBOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLEA( const WIR::TC_ARegV &, const WIR::TC_ARegV &,
                    const WIR::WIR_Function &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LOOP instruction.
    /*!
        Exact format: LOOP A[b], disp15 (BRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLOOP( LLIR_Register *, const std::string &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LOOP instruction.

      @param[in] Ab A const reference to the first address register operand.
      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LOOP A[b] (defuse), disp15 (AL_3)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLOOP( const WIR::TC_ARegV &, const WIR::WIR_BasicBlock &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LOOPU instruction.
    /*!
        Exact format: LOOPU disp15 (BRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLOOPU( const std::string &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LOOPU instruction.

      @param[in] disp15 A const reference to the %WIR basic block to be jumped
                        to.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LOOPU disp15 (L)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLOOPU( const WIR::WIR_BasicBlock &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LT instruction.
    /*!
        Exact format: LT D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLT( LLIR_Register *, LLIR_Register *, int,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LT instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LT D[c] (def), D[a] (use), const9 (DDC9_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLT( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LT instruction.
    /*!
        Exact format: LT D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLT( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LT instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LT D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLT( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                   const WIR::TC_DRegV &,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LT.A instruction.
    /*!
        Exact format: LT.A D[c], A[a], A[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLT_A( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LT.A instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Aa A const reference to the second address register operand.
      @param[in] Ab A const reference to the third address register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LT.A D[c] (def), A[a] (use), A[b] (use) (DAA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLT_A( const WIR::TC_DRegV &, const WIR::TC_ARegV &,
                     const WIR::TC_ARegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LT.U instruction.
    /*!
        Exact format: LT.U D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLT_U( LLIR_Register *, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LT.U instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 An unsigned 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LT.U D[c] (def), D[a] (use), const9 (DDC9_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLT_U( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     unsigned int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a LT.U instruction.
    /*!
        Exact format: LT.U D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertLT_U( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a LT.U instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: LT.U D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLT_U( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MADD instruction.
    /*!
        Exact format: MADD D[c], D[d], D[a], const9 (RCR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMADD( LLIR_Register *, LLIR_Register *, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MADD instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Dd A const reference to the second data register operand.
      @param[in] Da A const reference to the third data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MADD D[c] (def), D[d] (use), D[a] (use), const9 (DDDC9_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMADD( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MADD instruction.
    /*!
        Exact format: MADD D[c], D[d], D[a], D[b] (RRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMADD( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MADD instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Dd A const reference to the second data register operand.
      @param[in] Da A const reference to the third data register operand.
      @param[in] Db A const reference to the fourth data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MADD D[c] (def), D[d] (use), D[a] (use), D[b] (use) (DDDD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMADD( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MADD.U instruction.
    /*!
        Exact format: MADD.U E[c], E[d], D[a], const9 (RCR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMADD_U( LLIR_Register *, LLIR_Register *, LLIR_Register *, int,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MADD.U instruction.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ed A const reference to the second extended data register
                    operand.
      @param[in] Da A const reference to the third data register operand.
      @param[in] const9 An unsigned 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MADD.U E[c] (def), E[d] (use), D[a] (use), const9 (EEDC9_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMADD_U( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                       const WIR::TC_DRegV &, unsigned int,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MADD.U instruction.
    /*!
        Exact format: MADD.U E[c], E[d], D[a], D[b] (RRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMADD_U( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                       LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType type = EXP_STMT );

    /*!
      @brief Inserts a MADD.U instruction.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ed A const reference to the second extended data register
                    operand.
      @param[in] Da A const reference to the third data register operand.
      @param[in] Db A const reference to the fourth data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MADD.U E[c] (def), E[d] (use), D[a] (use), D[b] (use) (EEDD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMADD_U( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                       const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MAX instruction.
    /*!
        Exact format: MAX D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMAX( LLIR_Register *, LLIR_Register *, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MAX instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MAX D[c] (def), D[a] (use), const9 (DDC9_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMAX( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MAX instruction.
    /*!
        Exact format: MAX D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMAX( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MAX instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MAX D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMAX( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const WIR::TC_DRegV &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MAX.U instruction.
    /*!
        Exact format: MAX.U D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMAX_U( LLIR_Register *, LLIR_Register *, int,
                      const IR_Exp * = nullptr, StmtType type = EXP_STMT );

    /*!
      @brief Inserts a MAX.U instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 An unsigned 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MAX.U D[c] (def), D[a] (use), const9 (DDC9_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMAX_U( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      unsigned int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MAX.U instruction.
    /*!
        Exact format: MAX.U D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMAX_U( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MAX.U instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MAX.U D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMAX_U( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MIN instruction.
    /*!
        Exact format: MIN D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMIN( LLIR_Register *, LLIR_Register *, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MIN instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MIN D[c] (def), D[a] (use), const9 (DDC9_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMIN( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MIN instruction.
    /*!
        Exact format: MIN D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMIN( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MIN instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MIN D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMIN( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const WIR::TC_DRegV &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MIN.U instruction.
    /*!
        Exact format: MIN.U D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMIN_U( LLIR_Register *, LLIR_Register *, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MIN.U instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 An unsigned 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MIN.U D[c] (def), D[a] (use), const9 (DDC9_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMIN_U( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      unsigned int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MIN.U instruction.
    /*!
        Exact format: MIN.U D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMIN_U( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MIN.U instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MIN.U D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMIN_U( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a series of instructions for signed/unsigned byte modulo.
    /*!
       Exact formats:

       DVINIT.B E[c], D[a], D[b] (RR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVADJ E[c], E[c], D[b] (RRR) \n
       MOV D[d], Upper register of E[c] (SRR)

       resp.

       DVINIT.BU E[c], D[a], D[b] (RR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       MOV D[d], Upper register of E[c] (SRR)

       The last two parameters serve for generation of assembler debug
       information.
    */
    void insertMOD_B( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      bool = false,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a series of instructions for signed/unsigned byte modulo.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] u A Boolean denoting whether a signed or unsigned modulo is
                   generated.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      DVINIT.B E[x], D[a], D[b] (EDD) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVADJ E[x], E[x], D[b] (EED) \n
      MOV D[c], Odd register of E[x] (MOV_RR SDD_1)

      resp.

      DVINIT.BU E[x], D[a], D[b] (EDD) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      MOV D[c], Odd register of E[x] (MOV_RR SDD_1)
    */
    void insertMOD_B( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &, bool = false,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a series of instructions for signed/unsigned halfword modulo.
    /*!
       Exact formats:

       DVINIT.H E[c], D[a], D[b] (RR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVADJ E[c], E[c], D[b] (RRR) \n
       MOV D[d], Upper register of E[c] (SRR)

       resp.

       DVINIT.HU E[c], D[a], D[b] (RR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       MOV D[d], Lower register of E[c] (SRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMOD_H( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      bool = false,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a series of instructions for signed/unsigned halfword
             modulo.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] u A Boolean denoting whether a signed or unsigned modulo is
                   generated.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      DVINIT.H E[x], D[a], D[b] (EDD) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVADJ E[x], E[x], D[b] (EED) \n
      MOV D[c], Odd register of E[x] (MOV_RR SDD_1)

      resp.

      DVINIT.HU E[x], D[a], D[b] (EDD) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      MOV D[c], Odd register of E[x] (MOV_RR SDD_1)
    */
    void insertMOD_H( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &, bool = false,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a series of instructions for signed/unsigned integer modulo.
    /*!
       Exact formats:

       DVINIT E[c], D[a], D[b] (RR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVSTEP E[c], E[c], D[b] (RRR) \n
       DVADJ E[c], E[c], D[b] (RRR) \n
       MOV D[d], Lower register of E[c] (SRR)

       resp.

       DVINIT.U E[c], D[a], D[b] (RR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       DVSTEP.U E[c], E[c], D[b] (RRR) \n
       MOV D[d], Lower register of E[c] (SRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMOD_W( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      bool = false,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a series of instructions for signed/unsigned integer
             modulo.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] u A Boolean denoting whether a signed or unsigned modulo is
                   generated.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      DVINIT E[x], D[a], D[b] (EDD) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVSTEP E[x], E[x], D[b] (EED) \n
      DVADJ E[x], E[x], D[b] (EED) \n
      MOV D[c], Odd register of E[x] (MOV_RR SDD_1)

      resp.

      DVINIT.U E[x], D[a], D[b] (EDD) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      DVSTEP.U E[x], E[x], D[b] (EED) \n
      MOV D[c], Odd register of E[x] (MOV_RR SDD_1)
    */
    void insertMOD_W( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &, bool = false,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MOV instruction.
    /*!
        Exact formats:

        MOV D[c], const16 (RLC) \n
        MOV D[c], const4 (SRC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMOV( LLIR_Register *, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MOV instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] c A signed immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats either:

      MOV D[c] (def), c16 (DC16_1)

      or:

      MOV D[c] (def), c4 (SDC4_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOV( const WIR::TC_DRegV &, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MOV instruction.
    /*!
        Exact format: MOV D[a], D[b] (SRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMOV( LLIR_Register *, LLIR_Register *,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MOV instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MOV D[c] (def), D[b] (use) (MOV_RR SDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOV( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a MOV instruction.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Eb A const reference to the second extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      MOV <even register of E[c] (def)>, <even register of E[b] (use)> (MOV_RR SDD_1)
      MOV <odd register of E[c] (def)>, <odd register of E[b] (use)> (MOV_RR SDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOV( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MOV.A instruction.
    /*!
        Exact format: MOV.A A[a], D[b] (SRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMOV_A( LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MOV.A instruction.

      @param[in] Ac A const reference to the first address register operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MOV.A A[c] (def), D[b] (use) (SAD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOV_A( const WIR::TC_ARegV &, const WIR::TC_DRegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MOV.AA instruction.
    /*!
        Exact format: MOV.AA A[a], A[b] (SRR) \n

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMOV_AA( LLIR_Register *, LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MOV.AA instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MOV.AA A[a] (def), A[b] (use) (SAA_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOV_AA( const WIR::TC_ARegV &, const WIR::TC_ARegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MOV.D instruction.
    /*!
        Exact format: MOV.D D[a], A[b] (SRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMOV_D( LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MOV.D instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MOV.D D[a] (def), A[b] (use) (SDA_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOV_D( const WIR::TC_DRegV &, const WIR::TC_ARegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MOV.U instruction.
    /*!
        Exact format: MOV.U D[c], const16 (RLC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMOV_U( LLIR_Register *, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MOV.U instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] const16 An unsigned 16-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MOV.U D[c] (def), const16 (DC16_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOV_U( const WIR::TC_DRegV &, unsigned int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MOVH instruction.
    /*!
        Exact format: MOVH D[c], const16 (RLC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMOVH( LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MOVH instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] const16 An unsigned 16-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MOVH D[c] (def), const16 (DC16_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOVH( const WIR::TC_DRegV &, unsigned int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MOVH.A instruction.
    /*!
        Exact format: MOVH.A A[c], const16 (RLC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMOVH_A( LLIR_Register *, int,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MOVH.A instruction.

      @param[in] Ac A const reference to the first address register operand.
      @param[in] const16 An unsigned 16-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MOVH.A A[c] (def), const16 (AC16)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOVH_A( const WIR::TC_ARegV &, unsigned int,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MOVH.A instruction.
    /*!
        Exact formats:

        MOVH.A A[c], HI:label (RLC) \n
        MOVH.A A[c], LO:label (RLC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMOVH_A( LLIR_Register *, const std::string &,
                       const std::string &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MOVH.A instruction.

      @param[in] Ac A const reference to the first address register operand.
      @param[in] b A const reference to a basic block.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MOVH.A A[c] (def), HI:label (AL_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOVH_A( const WIR::TC_ARegV &, const WIR::WIR_BasicBlock &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a MOVH.A instruction.

      @param[in] Ac A const reference to the first address register operand.
      @param[in] d A const reference to a data object.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MOVH.A A[c] (def), HI:label (AL_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOVH_A( const WIR::TC_ARegV &, const WIR::WIR_Data &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a MOVH.A instruction.

      @param[in] Ac A const reference to the first address register operand.
      @param[in] f A const reference to a function.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MOVH.A A[c] (def), HI:label (AL_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOVH_A( const WIR::TC_ARegV &, const WIR::WIR_Function &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MSUB instruction.
    /*!
        Exact format: MSUB D[c], D[d], D[a], const9 (RCR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMSUB( LLIR_Register *, LLIR_Register *, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MSUB instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Dd A const reference to the second data register operand.
      @param[in] Da A const reference to the third data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MSUB D[c] (def), D[d] (use), D[a] (use), const9 (DDDC9_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMSUB( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MSUB instruction.
    /*!
        Exact format: MSUB D[c], D[d], D[a], D[b] (RRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMSUB( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MSUB instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Dd A const reference to the second data register operand.
      @param[in] Da A const reference to the third data register operand.
      @param[in] Db A const reference to the fourth data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MSUB D[c] (def), D[d] (use), D[a] (use), D[b] (use) (DDDD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMSUB( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MSUB.U instruction.
    /*!
        Exact format: MSUB.U E[c], E[d], D[a], const9 (RCR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMSUB_U( LLIR_Register *, LLIR_Register *, LLIR_Register *, int,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MSUB.U instruction.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ed A const reference to the second extended data register
                    operand.
      @param[in] Da A const reference to the third data register operand.
      @param[in] const9 An unsigned 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MSUB.U E[c] (def), E[d] (use), D[a] (use), const9 (EEDC9_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMSUB_U( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                       const WIR::TC_DRegV &, unsigned int,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MSUB.U instruction.
    /*!
        Exact format: MSUB.U E[c], E[d], D[a], D[b] (RRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMSUB_U( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                       LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MSUB.U instruction.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ed A const reference to the second extended data register
                    operand.
      @param[in] Da A const reference to the third data register operand.
      @param[in] Db A const reference to the fourth data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MSUB.U E[c] (def), E[d] (use), D[a] (use), D[b] (use) (EEDD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMSUB_U( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                       const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MUL instruction.
    /*!
        Exact format: MUL D[c], D[a], const9 (32signed RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMUL( LLIR_Register *, LLIR_Register *, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MUL instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MUL D[c] (def), D[a] (use), const9 (DDC9_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMUL( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a MUL instruction.

      Exact format: MUL D[c], D[a], D[b] (32signed RR2)

      The last two parameters serve for generation of assembly debug
      information.
    */
    void insertMUL( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MUL instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MUL D[c] (def), D[a] (use), D[b] (use) (DDD_1) \n

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMUL( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const WIR::TC_DRegV &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MUL instruction.
    /*!
        Exact format: MUL D[a], D[b] (SRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMUL( LLIR_Register *, LLIR_Register *,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MUL instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Db A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MUL D[a] (defuse), D[b] (use) (SDD_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMUL( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MUL.U instruction.
    /*!
        Exact format: MUL.U E[c], D[a], D[b] (RR2)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMUL_U( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MUL.U instruction.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MUL.U E[c] (def), D[a] (use), D[b] (use) (EDD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMUL_U( const WIR::TC_ERegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a NE instruction.
    /*!
        Exact format: NE D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertNE( LLIR_Register *, LLIR_Register *, int,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a NE instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: NE D[c] (def), D[a] (use), const9 (DDC9_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertNE( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a NE instruction.
    /*!
        Exact format: NE D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertNE( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a NE instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: NE D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertNE( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                   const WIR::TC_DRegV &,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a NE.A instruction.
    /*!
        Exact format: NE.A D[c], A[a], A[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertNE_A( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a NE.A instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Aa A const reference to the second address register operand.
      @param[in] Ab A const reference to the third address register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: NE.A D[c] (def), A[a] (use), A[b] (use) (DAA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertNE_A( const WIR::TC_DRegV &, const WIR::TC_ARegV &,
                     const WIR::TC_ARegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a NEZ.A instruction.
    /*!
        Exact format: NEZ.A D[c], A[a] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertNEZ_A( LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a NEZ.A instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Aa A const reference to the second address register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: NEZ.A D[c] (def), A[a] (use) (DA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertNEZ_A( const WIR::TC_DRegV &, const WIR::TC_ARegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a NOR instruction.
    /*!
        Exact format: NOR D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertNOR( LLIR_Register *, LLIR_Register *, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a NOR instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 An unsigned 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: NOR D[c] (def), D[a] (use), const9 (DDC9_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertNOR( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    unsigned int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an OR instruction.
    /*!
        Exact format: OR D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertOR( LLIR_Register *, LLIR_Register *, int,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an OR instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 An unsigned 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: OR D[c] (def), D[a] (use), const9 (DDC9_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertOR( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                   unsigned int,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an OR instruction.
    /*!
        Exact format: OR D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertOR( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an OR instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: OR D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertOR( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                   const WIR::TC_DRegV &,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an OR.EQ instruction.
    /*!
        Exact format: OR.EQ D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertOR_EQ( LLIR_Register *, LLIR_Register *, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an OR.EQ instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: OR.EQ D[c] (defuse), D[a] (use), const9 (DDC9_3)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertOR_EQ( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an OR.LT instruction.
    /*!
        Exact format: OR.LT D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertOR_LT( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an OR.LT instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: OR.LT D[c] (defuse), D[a] (use), D[b] (use) (DDD_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertOR_LT( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an OR.LT.U instruction.
    /*!
        Exact format: OR.LT.U D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertOR_LT_U( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                        const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an OR.LT.U instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: OR.LT.U D[c] (defuse), D[a] (use), D[b] (use) (DDD_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertOR_LT_U( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                        const WIR::TC_DRegV &,
                        const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an OR.NE instruction.
    /*!
        Exact format: OR.NE D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertOR_NE( LLIR_Register *, LLIR_Register *, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an OR.NE instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: OR.NE D[c] (defuse), D[a] (use), const9 (DDC9_3)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertOR_NE( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an OR.NE instruction.
    /*!
        Exact format: OR.NE D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertOR_NE( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an OR_NE instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: OR_NE D[c] (defuse), D[a] (use), D[b] (use) (DDD_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertOR_NE( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an OR.T instruction.
    /*!
        Exact format: OR.T D[c], D[a], pos1, D[b], pos2 (BIT)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertOR_T( LLIR_Register *, LLIR_Register *, int, LLIR_Register *,
                     int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an OR.T instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] p1 An unsigned 5-bit immediate constant.
      @param[in] Db A const reference to the third data register operand.
      @param[in] p2 An unsigned 5-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: OR.T D[c] (def), D[a] (use), p1, D[b] (use), p2 (DDC5DC5_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertOR_T( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     unsigned int, const WIR::TC_DRegV &, unsigned int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ORN instruction.
    /*!
        Exact format: ORN D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertORN( LLIR_Register *, LLIR_Register *, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ORN instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 An unsigned 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ORN D[c] (def), D[a] (use), const9 (DDC9_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertORN( const WIR::TC_DRegV &, const WIR::TC_DRegV &, unsigned int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a RET instruction.
    /*!
        Exact format: RET (SR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertRET( const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a RET instruction.

      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: RET PSW.C (def) (SPSW)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertRETURN( const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a RET instruction with implicit parameter of the given LLIR register.
    /*!
        Exact format: RET (SR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertRET( LLIR_Register *,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a RET instruction.

      @param[in] arg A const reference to a register that will be added to the
                     RET instruction as implicitly used parameter.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: RET PSW.C (def) (SPSW)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertRET( const WIR::WIR_BaseRegister &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a RSUB instruction.
    /*!
        Exact format: RSUB D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertRSUB( LLIR_Register *, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a RSUB instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: RSUB D[c] (def), D[a] (use), const9 (DDC9_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertRSUB( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a RSUB instruction.
    /*!
        Exact format: RSUB D[a] (SR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertRSUB( LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a RSUB instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: RSUB D[a] (defuse) (SD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertRSUB( const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a SH instruction.
    /*!
        Exact format: SH D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertSH( LLIR_Register *, LLIR_Register *, int,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a SH instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: SH D[c] (def), D[a] (use), const9 (DDC9_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSH( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a SH instruction.
    /*!
        Exact format: SH D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertSH( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a SH instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: SH D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSH( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                   const WIR::TC_DRegV &,
                   const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a SHA instruction.
    /*!
        Exact format: SHA D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertSHA( LLIR_Register *, LLIR_Register *, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a SHA instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 A signed 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: SHA D[c] (def), D[a] (use), const9, PSW.C (def) (DDC9PSW_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSHA( const WIR::TC_DRegV &, const WIR::TC_DRegV &, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a SHA instruction.
    /*!
        Exact format: SHA D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertSHA( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a SHA instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format:
      SHA D[c] (def), D[a] (use), D[b] (use), PSW.C (def) (DDDPSW_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSHA( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const WIR::TC_DRegV &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a ST.A instruction.
    /*!
        Exact formats:

        ST.A [+Ab]off10, A[a] (BO + Pre-increment Addressing) \n
        ST.A [Ab+]off10, A[a] (BO + Post-increment Addressing) \n
        ST.A [Ab]off10, A[a] (BO + Base+Short Offset Addressing)

        Handling of large address offsets is included.

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertST_A( const std::string &, LLIR_Register *, int, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a ST.A instruction.

      @param[in] Ab A const reference to the first address register operand.
      @param[in] off A signed immediate offset.
      @param[in] Aa A const reference to the second address register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ST.A [A[b] (use)]off, A[a] (use) (AC10ABOA)

      Handling of address offsets beyond signed 10 bits is included. Exact
      formats either:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      ST.A [A[x] (use)]<lower 10 bits of off>, A[a] (use) (AC10ABOA)

      or:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
      ST.A [A[x] (use)]0, A[a] (use) (AC10ABOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertST_A( const WIR::TC_ARegV &, int, const WIR::TC_ARegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a ST.A instruction.

      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the first address register operand.
      @param[in] off A signed immediate offset.
      @param[in] Aa A const reference to the second address register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      ST.A [+A[b] (defuse)]off, A[a] (use) (AC10APIA) \n
      ST.A [A[b] (defuse)+]off, A[a] (use) (AC10APIA)

      Handling of address offsets larger than signed 10 bits but within signed
      16 bits is included. Exact formats for pre-increment:

      LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
      ST.A [A[b] (use)]0, A[a] (use) (AC10ABOA)

      Exact formats for post-increment:

      ST.A [A[b] (use)]0, A[a] (use) (AC10ABOA) \n
      LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is also included. Exact
      formats for pre-increment either:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      ST.A [+A[b] (defuse)]<lower 10 bits of off>, A[a] (use) (AC10APIA)

      or:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
      ST.A [A[b] (use)]0, A[a] (use) (AC10ABOA)

      Exact formats for post-increment either:

      ST.A [A[b] (defuse)+]<lower 10 bits of off>, A[a] (use) (AC10APIA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

      or:

      ST.A [A[b] (use)]0, A[a] (use) (AC10ABOA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertST_A( const WIR::TC13::AddressingMode &, const WIR::TC_ARegV &,
                     int, const WIR::TC_ARegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a ST.B instruction.
    /*!
        Exact formats:

        ST.B [+Ab]off10, A[a] (BO + Pre-increment Addressing) \n
        ST.B [Ab+]off10, A[a] (BO + Post-increment Addressing) \n
        ST.B [Ab]off10, A[a] (BO + Base+Short Offset Addressing)

        Handling of large address offsets is included.

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertST_B( const std::string &, LLIR_Register *, int, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a ST.B instruction.

      @param[in] Ab A const reference to the first address register operand.
      @param[in] off A signed immediate offset.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ST.B [A[b] (use)]off, D[a] (use) (AC10DBOA_1)

      Handling of address offsets beyond signed 10 bits is included. Exact
      formats either:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      ST.B [A[x] (use)]<lower 10 bits of off>, D[a] (use) (AC10DBOA_1)

      or:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
      ST.B [A[x] (use)]0, D[a] (use) (AC10DBOA_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertST_B( const WIR::TC_ARegV &, int, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a ST.B instruction.

      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the first address register operand.
      @param[in] off A signed immediate offset.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      ST.B [+A[b] (defuse)]off, D[a] (use) (AC10DPIA_1) \n
      ST.B [A[b] (defuse)+]off, D[a] (use) (AC10DPIA_1)

      Handling of address offsets larger than signed 10 bits but within signed
      16 bits is included. Exact formats for pre-increment:

      LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
      ST.B [A[b] (use)]0, D[a] (use) (AC10DBOA_1)

      Exact formats for post-increment:

      ST.B [A[b] (use)]0, D[a] (use) (AC10DBOA_1) \n
      LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is also included. Exact
      formats for pre-increment either:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      ST.B [+A[b] (defuse)]<lower 10 bits of off>, D[a] (use) (AC10DPIA_1)

      or:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
      ST.B [A[b] (use)]0, D[a] (use) (AC10DBOA_1)

      Exact formats for post-increment either:

      ST.B [A[b] (defuse)+]<lower 10 bits of off>, D[a] (use) (AC10DPIA_1) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

      or:

      ST.B [A[b] (use)]0, D[a] (use) (AC10DBOA_1) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertST_B( const WIR::TC13::AddressingMode &, const WIR::TC_ARegV &,
                     int, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a ST.D instruction.
    /*!
        Exact formats:

        ST.D [+Ab]off10, A[a] (BO + Pre-increment Addressing) \n
        ST.D [Ab+]off10, A[a] (BO + Post-increment Addressing) \n
        ST.D [Ab]off10, A[a] (BO + Base+Short Offset Addressing)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertST_D( const std::string &, LLIR_Register *, int, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a ST.D instruction.

      @param[in] Ab A const reference to the first address register operand.
      @param[in] off A signed immediate offset.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ST.D [A[b] (use)]off, E[a] (use) (AC10EBOA)

      Handling of address offsets beyond signed 10 bits is included. Exact
      formats either:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      ST.D [A[x] (use)]<lower 10 bits of off>, E[a] (use) (AC10EBOA)

      or:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
      ST.D [A[x] (use)]0, E[a] (use) (AC10EBOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertST_D( const WIR::TC_ARegV &, int, const WIR::TC_ERegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a ST.D instruction.

      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the first address register operand.
      @param[in] off A signed immediate offset.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      ST.D [+A[b] (defuse)]off, E[a] (use) (AC10EPIA) \n
      ST.D [A[b] (defuse)+]off, E[a] (use) (AC10EPIA)

      Handling of address offsets larger than signed 10 bits but within signed
      16 bits is included. Exact formats for pre-increment:

      LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
      ST.D [A[b] (use)]0, E[a] (use) (AC10EBOA)

      Exact formats for post-increment:

      ST.D [A[b] (use)]0, E[a] (use) (AC10EBOA) \n
      LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is also included. Exact
      formats for pre-increment either:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      ST.D [+A[b] (defuse)]<lower 10 bits of off>, E[a] (use) (AC10EPIA)

      or:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
      ST.D [A[b] (use)]0, E[a] (use) (AC10EBOA)

      Exact formats for post-increment either:

      ST.D [A[b] (defuse)+]<lower 10 bits of off>, E[a] (use) (AC10EPIA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

      or:

      ST.D [A[b] (use)]0, E[a] (use) (AC10EBOA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertST_D( const WIR::TC13::AddressingMode &, const WIR::TC_ARegV &,
                     int, const WIR::TC_ERegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a ST.H instruction.
    /*!
        Exact formats:

        ST.H [+Ab]off10, A[a] (BO + Pre-increment Addressing) \n
        ST.H [Ab+]off10, A[a] (BO + Post-increment Addressing) \n
        ST.H [Ab]off10, A[a] (BO + Base+Short Offset Addressing)

        Handling of large address offsets is included.

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertST_H( const std::string &, LLIR_Register *, int, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a ST.H instruction.

      @param[in] Ab A const reference to the first address register operand.
      @param[in] off A signed immediate offset.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ST.H [A[b] (use)]off, D[a] (use) (AC10DBOA_1)

      Handling of address offsets beyond signed 10 bits is included. Exact
      formats either:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      ST.H [A[x] (use)]<lower 10 bits of off>, D[a] (use) (AC10DBOA_1)

      or:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
      ST.H [A[x] (use)]0, D[a] (use) (AC10DBOA_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertST_H( const WIR::TC_ARegV &, int, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a ST.H instruction.

      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the first address register operand.
      @param[in] off A signed immediate offset.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      ST.H [+A[b] (defuse)]off, D[a] (use) (AC10DPIA_1) \n
      ST.H [A[b] (defuse)+]off, D[a] (use) (AC10DPIA_1)

      Handling of address offsets larger than signed 10 bits but within signed
      16 bits is included. Exact formats for pre-increment:

      LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
      ST.H [A[b] (use)]0, D[a] (use) (AC10DBOA_1)

      Exact formats for post-increment:

      ST.H [A[b] (use)]0, D[a] (use) (AC10DBOA_1) \n
      LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is also included. Exact
      formats for pre-increment either:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      ST.H [+A[b] (defuse)]<lower 10 bits of off>, D[a] (use) (AC10DPIA_1)

      or:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
      ST.H [A[b] (use)]0, D[a] (use) (AC10DBOA_1)

      Exact formats for post-increment either:

      ST.H [A[b] (defuse)+]<lower 10 bits of off>, D[a] (use) (AC10DPIA_1) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

      or:

      ST.H [A[b] (use)]0, D[a] (use) (AC10DBOA_1) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertST_H( const WIR::TC13::AddressingMode &, const WIR::TC_ARegV &,
                     int, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a ST.W instruction.
    /*!
        Exact formats:

        ST.W [+Ab]off10, A[a] (BO + Pre-increment Addressing) \n
        ST.W [Ab+]off10, A[a] (BO + Post-increment Addressing) \n
        ST.W [Ab]off16, A[a] (BOL + Base+Long Offset Addressing) \n
        ST.W [Ab]off10, A[a] (BO + Base+Short Offset Addressing)

        Handling of large address offsets is included.

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertST_W( const std::string &, LLIR_Register *, int, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a ST.W instruction.

      @param[in] Ab A const reference to the first address register operand.
      @param[in] off A signed immediate offset.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ST.W [A[b] (use)]off, D[a] (use) (AC16DBOA)

      Handling of address offsets beyond signed 16 bits is included. Exact
      formats:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      ST.W [A[x] (use)]<lower 16 bits of off>, D[a] (use) (AC16DBOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertST_W( const WIR::TC_ARegV &, int, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief Inserts a ST.W instruction.

      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the first address register operand.
      @param[in] off A signed immediate offset.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      ST.W [+A[b] (defuse)]off, D[a] (use) (AC10DPIA_1) \n
      ST.W [A[b] (defuse)+]off, D[a] (use) (AC10DPIA_1)

      Handling of address offsets larger than signed 10 bits but within signed
      16 bits is included. Exact formats for pre-increment:

      LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
      ST.W [A[b] (use)]0, D[a] (use) (AC10DBOA_1)

      Exact formats for post-increment:

      ST.W [A[b] (use)]0, D[a] (use) (AC10DBOA_1) \n
      LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is also included. Exact
      formats for pre-increment either:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      ST.W [+A[b] (defuse)]<lower 10 bits of off>, D[a] (use) (AC10DPIA_1)

      or:

      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
      ST.W [A[b] (use)]0, D[a] (use) (AC16DBOA)

      Exact formats for post-increment either:

      ST.W [A[b] (defuse)+]<lower 10 bits of off>, D[a] (use) (AC10DPIA_1) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

      or:

      ST.W [A[b] (use)]0, D[a] (use) (AC16DBOA) \n
      ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertST_W( const WIR::TC13::AddressingMode &, const WIR::TC_ARegV &,
                     int, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a ST.W instruction.
    /*!
        Exact formats:

        ST.W [+Ab] HI:label, A[a] (BO + Pre-increment Addressing) \n
        ST.W [Ab+] HI:label, A[a] (BO + Post-increment Addressing) \n
        ST.W [Ab] HI:label, A[a] (BOL + Base+Long Offset Addressing) \n
        ST.W [Ab] HI:label, A[a] (BO + Base+Short Offset Addressing) \n
        ST.W [+Ab] LO:label, A[a] (BO + Pre-increment Addressing) \n
        ST.W [Ab+] LO:label, A[a] (BO + Post-increment Addressing) \n
        ST.W [Ab] LO:label, A[a] (BOL + Base+Long Offset Addressing) \n
        ST.W [Ab] LO:label, A[a] (BO + Base+Short Offset Addressing)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertST_W( const std::string &, LLIR_Register *, const std::string &,
                     const std::string &, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a ST.W instruction.

      @param[in] Ab A const reference to the first address register operand.
      @param[in] d A const reference to a data object.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ST.W [A[b] (use)] LO:label, D[a] (use) (ALDBOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertST_W( const WIR::TC_ARegV &, const WIR::WIR_Data &,
                     const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a SUB instruction.
    /*!
        Exact format: SUB D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertSUB( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a SUB instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: SUB D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSUB( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const WIR::TC_DRegV &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a SUB.A instruction.
    /*!
        Exact format: SUB.A A[c], A[a], A[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertSUB_A( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a SUB.A instruction.

      @param[in] Ac A const reference to the first address register operand.
      @param[in] Aa A const reference to the second address register operand.
      @param[in] Ab A const reference to the third address register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: SUB.A A[c] (def), A[a] (use), A[b] (use) (AAA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSUB_A( const WIR::TC_ARegV &, const WIR::TC_ARegV &,
                      const WIR::TC_ARegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a SUBC instruction.
    /*!
        Exact format: SUBC D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertSUBC( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a SUBC instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format:
      SUBC D[c] (def), D[a] (use), D[b] (use), PSW.C (defuse) (DDDPSW_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSUBC( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a SUBX instruction.
    /*!
        Exact format: SUBX D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertSUBX( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a SUBX instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format:
      SUBX D[c] (def), D[a] (use), D[b] (use), PSW.C (def) (DDDPSW_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSUBX( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an XNOR instruction.
    /*!
        Exact format: XNOR D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertXNOR( LLIR_Register *, LLIR_Register *, int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an XNOR instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 An unsigned 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: XNOR D[c] (def), D[a] (use), const9 (DDC9_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertXNOR( const WIR::TC_DRegV &, const WIR::TC_DRegV &, unsigned int,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an XOR instruction.
    /*!
        Exact format: XOR D[c], D[a], const9 (RC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertXOR( LLIR_Register *, LLIR_Register *, int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an XOR instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] const9 An unsigned 9-bit immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: XOR D[c] (def), D[a] (use), const9 (DDC9_2)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertXOR( const WIR::TC_DRegV &, const WIR::TC_DRegV &, unsigned int,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an XOR instruction.
    /*!
        Exact format: XOR D[c], D[a], D[b] (RR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertXOR( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an XOR instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: XOR D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertXOR( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                    const WIR::TC_DRegV &,
                    const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;


    //
    // Native TriCore floating-point instructions or library calls.
    //

    //! Inserts an ADD.F instruction or a call to a software routine.
    /*!
        Exact format: ADD.F D[c], D[d], D[a]
    */
    void insertADD_F( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ADD.F instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Dd A const reference to the second data register operand.
      @param[in] Da A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ADD.F D[c] (def), D[d] (use), D[a] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertADD_F( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a CMP.F instruction or a call to a software routine.
    /*!
        Exact format: CMP.F D[c], D[a], D[b]

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertCMP_F( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a CMP.F instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: CMP.F D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertCMP_F( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a DIV.F instruction or a call to a software routine.
    /*!
        Exact format: DIV.F D[c], D[a], D[b]

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertDIV_F( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a DIV.F instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: DIV.F D[c] (def), D[a] (use), D[b] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDIV_F( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a FTOI instruction or a call to a software routine.
    /*!
        Exact format: FTOI D[c], D[a]

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertFTOI( LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a FTOI instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: FTOI D[c] (def), D[a] (use) (DD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertFTOI( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a FTOQ31 instruction or a call to a software routine.
    /*!
        Exact format: FTOQ31 D[c], D[a], D[b]

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertFTOQ31( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a FTOQ31 instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: FTOQ31 D[c] (def), D[a] (use), D[b] (use)  (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertFTOQ31( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                       const WIR::TC_DRegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a FTOU instruction or a call to a software routine.
    /*!
        Exact format: FTOU D[c], D[a]

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertFTOU( LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a FTOU instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: FTOU D[c] (def), D[a] (use) (DD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertFTOU( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an ITOF instruction or a call to a software routine.
    /*!
        Exact format: ITOF D[c], D[a]

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertITOF( LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an ITOF instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: ITOF D[c] (def), D[a] (use) (DD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertITOF( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MADD.F instruction or a call to a software routine.
    /*!
        Exact format: MADD.F D[c], D[d], D[a], D[b] (RRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMADD_F( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                       LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MADD_F instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Dd A const reference to the second data register operand.
      @param[in] Da A const reference to the third data register operand.
      @param[in] Db A const reference to the fourth data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MADD.F D[c] (def), D[d] (use), D[a] (use), D[b] (use)  (DDDD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMADD_F( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                       const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MSUB.F instruction or a call to a software routine.
    /*!
        Exact format: MSUB.F D[c], D[d], D[a], D[b] (RRR)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMSUB_F( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                       LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MSUB_F instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Dd A const reference to the second data register operand.
      @param[in] Da A const reference to the third data register operand.
      @param[in] Db A const reference to the fourth data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MSUB.F D[c] (def), D[d] (use), D[a] (use), D[b] (use)  (DDDD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMSUB_F( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                       const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a MUL.F instruction or a call to a software routine.
    /*!
        Exact format: MUL.F D[c], D[a], D[b]

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMUL_F( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a MUL.F instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: MUL.F D[c] (def), D[a] (use), D[b] (use)  (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMUL_F( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a Q31TOF instruction or a call to a software routine.
    /*!
        Exact format: Q31TOF D[c], D[a], D[b]

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertQ31TOF( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a Q31TOF instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: Q31TOF D[c] (def), D[a] (use), D[b] (use)  (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertQ31TOF( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                       const WIR::TC_DRegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a SUB.F instruction or a call to a software routine.
    /*!
        Exact format: SUB.F D[c], D[d], D[a]

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertSUB_F( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a SUB.F instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Dd A const reference to the second data register operand.
      @param[in] Da A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: SUB.F D[c] (def), D[d] (use), D[a] (use) (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSUB_F( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                      const WIR::TC_DRegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an UTOF instruction or a call to a software routine.
    /*!
        Exact format: UTOF D[c], D[a]

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertUTOF( LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts an UTOF instruction.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact format: UTOF D[c] (def), D[a] (use) (DD)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertUTOF( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;


    //
    // Floating-point related comparisons that have no equivalent in the
    // TriCore's native FPU instruction set and that are thus realized using
    // library calls.
    //

    //! Inserts a call to a software routine.
    void insertEQ_F( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-float equality comparison.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: EQ.F D[c] (def), D[a] (use), D[b] (use)  (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertEQ_F( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine.
    void insertGE_F( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-float greater-equal comparison.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: GE.F D[c] (def), D[a] (use), D[b] (use)  (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertGE_F( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine.
    void insertGT_F( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-float greater-than comparison.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: GT.F D[c] (def), D[a] (use), D[b] (use)  (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertGT_F( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine.
    void insertLE_F( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-float less-equal comparison.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: LE.F D[c] (def), D[a] (use), D[b] (use)  (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLE_F( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine.
    void insertLT_F( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-float less-than comparison.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: LT.F D[c] (def), D[a] (use), D[b] (use)  (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLT_F( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine.
    void insertNE_F( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-float inequality comparison.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] Db A const reference to the third data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: NE.F D[c] (def), D[a] (use), D[b] (use)  (DDD_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertNE_F( const WIR::TC_DRegV &, const WIR::TC_DRegV &,
                     const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;


    //
    // Soft-double operations realized using library calls.
    //

    //! Inserts a call to a software routine that adds two doubles.
    void insertADD_D( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-double addition.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: ADD.D E[c] (def), E[a] (use), E[b] (use)  (EEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertADD_D( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                      const WIR::TC_ERegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that divides two doubles.
    void insertDIV_D( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-double division.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: DIV.D E[c] (def), E[a] (use), E[b] (use)  (EEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDIV_D( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                      const WIR::TC_ERegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that checks if two doubles are equal.
    void insertEQ_D( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-double equality comparison.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: EQ.D D[c] (def), E[a] (use), E[b] (use)  (DEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertEQ_D( const WIR::TC_DRegV &, const WIR::TC_ERegV &,
                     const WIR::TC_ERegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that checks if one double is greater or equal than the other.
    void insertGE_D( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-double greater-equal comparison.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: GE.D D[c] (def), E[a] (use), E[b] (use)  (DEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertGE_D( const WIR::TC_DRegV &, const WIR::TC_ERegV &,
                     const WIR::TC_ERegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that checks if one double is greater than the other.
    void insertGT_D( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-double greater-than comparison.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: GT.D D[c] (def), E[a] (use), E[b] (use)  (DEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertGT_D( const WIR::TC_DRegV &, const WIR::TC_ERegV &,
                     const WIR::TC_ERegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that checks if one double is less or equal than the other.
    void insertLE_D( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-double less-equal comparison.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: LE.D D[c] (def), E[a] (use), E[b] (use)  (DEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLE_D( const WIR::TC_DRegV &, const WIR::TC_ERegV &,
                     const WIR::TC_ERegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that checks if one double is less than the other.
    void insertLT_D( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-double less-than comparison.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: LT.D D[c] (def), E[a] (use), E[b] (use)  (DEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLT_D( const WIR::TC_DRegV &, const WIR::TC_ERegV &,
                     const WIR::TC_ERegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that multiplies two doubles.
    void insertMUL_D( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-double multiplication.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: MUL.D E[c] (def), E[a] (use), E[b] (use)  (EEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMUL_D( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                      const WIR::TC_ERegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that checks if two doubles are not equal.
    void insertNE_D( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-double inequality comparison.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: NE.D D[c] (def), E[a] (use), E[b] (use)  (DEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertNE_D( const WIR::TC_DRegV &, const WIR::TC_ERegV &,
                     const WIR::TC_ERegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that subtracts two doubles.
    void insertSUB_D( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-double subtraction.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: SUB.D E[c] (def), E[a] (use), E[b] (use)  (EEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSUB_D( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                      const WIR::TC_ERegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;


    //
    // Soft-long long operations (incl. unsigned) realized using library calls.
    //

    //! Inserts a call to a software routine that divides two long longs.
    void insertDIV_LL( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-long long division.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: DIV.LL E[c] (def), E[a] (use), E[b] (use)  (EEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDIV_LL( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                       const WIR::TC_ERegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that divides two long longs.
    void insertDIV_ULL( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                        const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-unsigned long long division.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: DIV.ULL E[c] (def), E[a] (use), E[b] (use)  (EEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDIV_ULL( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                        const WIR::TC_ERegV &,
                        const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that computes two long longs modulus.
    void insertMOD_LL( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-long long modulo.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: MOD.LL E[c] (def), E[a] (use), E[b] (use)  (EEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOD_LL( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                       const WIR::TC_ERegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that computes two unsigned long longs modulus.
    void insertMOD_ULL( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                        const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for soft-unsigned long long modulo.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] Eb A const reference to the third extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: MOD.ULL E[c] (def), E[a] (use), E[b] (use)  (EEE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOD_ULL( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                        const WIR::TC_ERegV &,
                        const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;


    //
    // Data type conversions realized using library calls.
    //

    //! Inserts a call to a software routine that converts double to float.
    void insertDTOF( LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for double to float conversion.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: DTOF D[c] (def), E[a] (use) (DE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDTOF( const WIR::TC_DRegV &, const WIR::TC_ERegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that converts double to integer.
    void insertDTOI( LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for double to integer conversion.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: DTOI D[c] (def), E[a] (use) (DE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDTOI( const WIR::TC_DRegV &, const WIR::TC_ERegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that converts double to long long.
    void insertDTOLL( LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for double to long long conversion.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: DTOLL E[c] (def), E[a] (use) (EE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDTOLL( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that converts double to unsigned integer.
    void insertDTOU( LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for double to unsigned integer conversion.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: DTOU D[c] (def), E[a] (use) (DE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDTOU( const WIR::TC_DRegV &, const WIR::TC_ERegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that converts double to unsigned long long.
    void insertDTOULL( LLIR_Register *, LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for double to unsigned long long conversion.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: DTOULL E[c] (def), E[a] (use) (EE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDTOULL( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that converts float to double.
    void insertFTOD( LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for float to double conversion.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: FTOD E[c] (def), D[a] (use) (ED)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertFTOD( const WIR::TC_ERegV &, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that converts float to long long.
    void insertFTOLL( LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for float to long long conversion.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: FTOLL E[c] (def), D[a] (use) (ED)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertFTOLL( const WIR::TC_ERegV &, const WIR::TC_DRegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that converts float to unsigned long long.
    void insertFTOULL( LLIR_Register *, LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for float to unsigned long long conversion.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: FTOULL E[c] (def), D[a] (use) (ED)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertFTOULL( const WIR::TC_ERegV &, const WIR::TC_DRegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that converts integer to double.
    void insertITOD( LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for integer to double conversion.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: ITOD E[c] (def), D[a] (use) (ED)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertITOD( const WIR::TC_ERegV &, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that converts long long to double.
    void insertLLTOD( LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType type = EXP_STMT );

    /*!
      @brief Inserts a library call for long long to double conversion.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: LLTOD E[c] (def), E[a] (use) (EE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLLTOD( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that converts long long to float.
    void insertLLTOF( LLIR_Register *, LLIR_Register *,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for long long to float conversion.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: LLTOF D[c] (def), E[a] (use) (DE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLLTOF( const WIR::TC_DRegV &, const WIR::TC_ERegV &,
                      const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that converts unsigned long long to double.
    void insertULLTOD( LLIR_Register *, LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for unsigned long long to double conversion.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: ULLTOD E[c] (def), E[a] (use) (EE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertULLTOD( const WIR::TC_ERegV &, const WIR::TC_ERegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that converts unsigned long long to float.
    void insertULLTOF( LLIR_Register *, LLIR_Register *,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for unsigned long long to float conversion.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] Ea A const reference to the second extended data register
                    operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: ULLTOF D[c] (def), E[a] (use) (DE)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertULLTOF( const WIR::TC_DRegV &, const WIR::TC_ERegV &,
                       const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts a call to a software routine that converts unsigned integer to double.
    void insertUTOD( LLIR_Register *, LLIR_Register *,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a library call for unsigned integer to double conversion.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] Da A const reference to the second data register operand.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Assumed format: UTOD E[c] (def), D[a] (use) (ED)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertUTOD( const WIR::TC_ERegV &, const WIR::TC_DRegV &,
                     const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;


    //
    // Convenience functions.
    //

    //! Inserts instructions to move a constant into a register
    /*
      The exact instructions inserted depend on the bitwidth of the constant:
      bitwidth < 17:
        MOV D[a], c0
      bitwidth == 17:
        MOV.U D[a], c0
      17 < bitwidth < 33:
        MOVH D[a], upper 16 bits of c0 (RLC) \n
        ADDI D[a], D[a], lower 16 bits of c0 (RLC) \n

      The last two parameters serve for generation of assembler debug
      information.
    */
    void insertMOVConstant( LLIR_Register *, int,
                            const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts instructions to move a constant into a register.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] c A signed immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      The exact formats depend on the constant's value.

      For getMinSignedValue( 16 ) <= c <= getMaxSignedValue( 16 ):

      MOV D[c] (def), c (DC16_1)

      else for 0 <= c <= getMaxUnsignedValue( 16 ):

      MOV.U D[c] (def), c (DC16_2)

      else

      MOVH D[c] (def), <upper 16 bits of c> (DC16_2) \n
      ADDI D[c] (def), D[c] (use), <lower 16 bits of c> (DDC16_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOVConstant( const WIR::TC_DRegV &, int,
                            const IR_Exp * = nullptr,
                            StmtType = EXP_STMT ) const;

    //! Inserts a series of instructions to move a 64-bit constant into an ereg.
    /*!
        Calls insertMOVConstant for the lower and the upper word of the
        constant.

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMOVConstantLL( LLIR_Register *, const IR_Integer *,
                              const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a series of instructions to move a 64-bit constant into an
             extended data register.

      @param[in] Ec A const reference to the first extended data register
                    operand.
      @param[in] c A const reference to the 64-bit IR constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      insertMOVConstant calls insertMOVConstant above for the lower and the
      upper words of the 64-bit constant.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOVConstant( const WIR::TC_ERegV &, const IR_Integer &,
                            const IR_Exp * = nullptr,
                            StmtType = EXP_STMT ) const;

    //! Inserts a series of MOVH and ADDI instructions.
    /*!
        Exact formats:

        MOVH D[c], upper 16 bits of c0 (RLC) \n
        ADDI D[c], D[c], lower 16 bits of c0 (RLC)

        The last two parameters serve for generation of assembler debug
        information.
    */
    void insertMOVH_ADDI( LLIR_Register *, int,
                          const IR_Exp * = nullptr, StmtType = EXP_STMT );

    /*!
      @brief Inserts a series of MOVH and ADDI instructions.

      @param[in] Dc A const reference to the first data register operand.
      @param[in] c A signed immediate constant.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      MOVH D[c] (def), <upper 16 bits of c> (DC16_2) \n
      ADDI D[c] (def), D[c] (use), <lower 16 bits of c> (DDC16_1)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertMOVH_ADDI( const WIR::TC_DRegV &, int,
                          const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    //! Inserts an inline assembly instruction.
    /*!
      The inserted instructions are taken from the assembly statement's asm
      std::string.
    */
    void insertInlineAsm( const IR_AsmStmt &,
                          const std::vector<Tricap::Argument *> & );

    /*!
      @brief Inserts assembly instructions for a given GNU inline assembly
             statement.

      @param[in] s A const reference to the inline assembly statement.
      @param[in] args A const reference to a vector storing the output and
                      input arguments of the inline assembly directive.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertInlineAsm( const IR_AsmStmt &,
                          const std::vector<std::unique_ptr<WIR::TC_AsmArgument>> & ) const;

    /*!
      @brief getSoftFloatSymbol returns a string containing the library function
             name to be called for the specified soft-float symbol.

      @param[in] sfs An enum value specifying the soft-float symbol to be looked
                     up.
      @return A const reference to a string containing the corresponding
              function name of the soft-float library.

      getSoftFloatSymbol distinguishes between WCC's internal soft-float
      library and the one from GCC.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::string &getSoftFloatSymbol( SoftFloatSymbol sfs ) const;

    /*!
      @brief getSoftFloatFunction returns the %WIR library function that
             corresponds to the specified soft-float symbol.

      @param[in] sfs An enum value specifying the soft-float symbol to be looked
                     up.
      @return A reference to the corresponding %WIR function.

      If the %WIR system for which code is currently generated does not contain
      the requested function, a novel empty, external %WIR function is created
      and returned.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR::WIR_Function &getSoftFloatFunction( SoftFloatSymbol sfs ) const;

    /*!
      @brief getSoftLongLongSymbol returns a string containing the library
             function name to be called for the specified soft-long long symbol.

      @param[in] slls An enum value specifying the soft-long long symbol to be
                      looked up.
      @return A const reference to a string containing the corresponding
              function name of the soft-long long library.

      getSoftLongLongSymbol distinguishes between WCC's internal soft-long long
      library and the one from GCC.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::string &getSoftLongLongSymbol( SoftLongLongSymbol slls ) const;

    /*!
      @brief getSoftLongLongFunction returns the %WIR library function that
             corresponds to the specified soft-long long symbol.

      @param[in] slls An enum value specifying the soft-long long symbol to be
                      looked up.
      @return A reference to the corresponding %WIR function.

      If the %WIR system for which code is currently generated does not contain
      the requested function, a novel empty, external %WIR function is created
      and returned.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR::WIR_Function &getSoftLongLongFunction( SoftLongLongSymbol ) const;

    //! CreateRegister creates a new LLIR register.
    LLIR_Register *CreateRegister( std::string, bool = false );

    //! CreateERegister creates a new Extended LLIR register with two hierarchical child registers.
    LLIR_Register *CreateERegister( std::string, LLIR_Register * = nullptr,
                                    LLIR_Register * = nullptr );

    /*!
      @brief createAReg creates a new virtual TriCore address register.

      @return A reference to the newly created address register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR::TC_ARegV &createAReg( void ) const;

    /*!
      @brief createDReg creates a new virtual TriCore data register.

      @return A reference to the newly created data register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR::TC_DRegV &createDReg( void ) const;

    /*!
      @brief createEReg creates a new virtual TriCore extended data register.

      @return A reference to the newly created extended data register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR::TC_ERegV &createEReg( void ) const;

    /*!
      @brief createPReg creates a new virtual TriCore extended address register.

      @return A reference to the newly created extended address register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR::TC_PRegV &createPReg( void ) const;

    /*!
      @brief splitOffset splits the given address offset into its most- and
             least-significant 16-bit parts.

      @param[in] o A constant signed address offset.
      @return A pair of values whose first element contains the most-significant
              16 bits of the offset and whose second element contains the least-
              significant 16 bits.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static std::pair<int, int> splitOffset( const int );


  private:

    //! handleLargeOffset inserts instructions for large address offsets > 16 bit.
    /*!
      Exact formats:

      ADDIH.A A[c], A[a], upper 16 bits of c0 (RLC) \n
      LEA A[c], [Ac] lower 16 bits of c0 (BOL)
    */
    void handleLargeOffset( LLIR_Register *, const std::string &, int,
                            LLIR_Register * = nullptr );

    /*!
      @brief handleLargeOffset inserts instructions in order to handle address
             offsets beyond signed 16 bits.

      @param[in] Ac A const reference to the first address register operand.
      @param[in] Aa A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer defaulting to nullptr that points to an IR
                     expression to be used for the generation of debug
                     information for the newly inserted assembly instruction.
      @param[in] type An enum value specifying the type of IR statement to be
                      used during the generation of debug information.

      Exact formats:

      ADDIH.A A[c] (def), A[a] (use), <upper 16 bits of off> (AAC16) \n
      LEA A[c] (def), [A[c] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void handleLargeOffset( const WIR::TC_ARegV &, const WIR::TC_ARegV &, int,
                            const IR_Exp * = nullptr, StmtType = EXP_STMT ) const;

    /*!
      @brief handleDataAccess adds data access meta-information to assembly
             instructions.

      @param[in,out] ins A pointer to a LLIR instruction to be annotated.
      @param[in] exp A pointer to an IR expression that is used to identify the
                     data object accessed by the assembly instruction.
      @param[in] deref A Boolean defaulting to false denoting whether a pointer
                       dereference shall be considered.

      handleDataAccess adds a LLIR pragma to the given LLIR instruction. This
      pragma carries the string content "DATA ACCESS: _objectname".
    */
    void handleDataAccess( LLIR_Instruction *, const IR_Exp *, bool = false );

    /*!
      @brief handleDataAccess adds data access meta-information to assembly
             instructions.

      @param[in,out] i A reference to a %WIR instruction to be annotated.
      @param[in] exp A pointer to an IR expression that is used to identify the
                     data object accessed by the assembly instruction.
      @param[in] deref A Boolean defaulting to false denoting whether a pointer
                       dereference shall be considered.

      handleDataAccess adds a DataAccess container to the %WIR operation inside
      the given WIR instruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void handleDataAccess( WIR::WIR_Instruction &, const IR_Exp *,
                           bool = false ) const;

    //! mConfig holds the current compiler configuration.
    Configuration &mConfig;

    //! mCodesel refers to the current code selector instance.
    TC179x_CodeSelector &mCodesel;

    /*!
      @brief mCurrentInstr points to the LLIR instruction lastly generated
             during code selection.
    */
    LLIR_Instruction *mCurrentInstr;

    /*!
      @brief m16BitOperations stores whether the code selector shall generate 16
             bits wide operations or not.
    */
    bool m16BitOperations;

};

#endif  // _INSTRUCTION_FACTORY_TC179X_H
