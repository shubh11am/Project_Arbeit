/*
This header file belongs to the

                Hamburg University of Technology (TUHH)
                       WCC Compiler framework

and is property of its respective copyright holder. It must neither be used
nor published even in parts without explicit written permission.

Copyright 2007 - 2022

Hamburg University of Technology (TUHH)
Institute of Embedded Systems
21071 Hamburg
Germany

http://www.tuhh.de/es/esd/research/wcc
*/


#ifndef _INSTRUCTION_FACTORY_ARM7_H
#define _INSTRUCTION_FACTORY_ARM7_H

// Include standard headers
#include <deque>
#include <string>

// Include ICD headers
#include <icd-c.h>
#include <llir3/llir3.h>


namespace WIR {
class WIR_BaseRegister;
class WIR_Instruction;
class WIR_Function;
class WIR_VirtualRegister;
}


class Configuration;
class CodeSelector;
class ARM7_CodeSelector;

//! This class has the sole purpose of producing instruction objects during code selection
/*!
  As there are masses of different instructions for the ARM7, this class was
  created to keep the code selector's interface clear and separated from the
  instruction creation methods.
 */
class InstructionFactory
{
  public:
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
      DGE,
      SDIVSI,
      UDIVSI
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
      JUMP_STMT,    // an unconditional jump statement (break,goto,continue,return)
      SWITCH_STMT,  // a switch statement
      VOID_STMT,    // a return-void statment
      RETURN_STMT,  // a non-void return statement
      ASM_STMT,     // an inline assembly statement
      NOT_DEFINED   // default, if the type is not defined
    };

    /*!
      @brief This enum identifies library longlong-instruction-replacements.

      It only only represents library routines.
      The mapping is performed in getSoftLongLongSymbol().
    */
    enum class SoftLongLongSymbol : char
    {
      LLDIV,
      ULLDIV,
      LLMOD,
      ULLMOD,
      LLSHL,
      LLSHR,
      ULLSHR,
    };

    /*!
      @brief getSoftLongLongSymbol returns a string containing the library
             function name to be called for the specified soft-long long symbol.

      @param[in] slls An enum value specifying the soft-long long symbol to be
                      looked up.
      @return A const reference to a string containing the corresponding
              function name of the soft-long long library.

      getSoftLongLongSymbol distinguishes between WCC's internal soft-long long
      library and the one from GCC.

    */
    const std::string &getSoftLongLongSymbol( SoftLongLongSymbol ) const;

    //! Constructor with parameter Configuration and BackAnnotation.
    InstructionFactory( Configuration&, ARM7_CodeSelector& );

    //! Default Destructor.
    ~InstructionFactory();

    //! setCurrentInstr sets the reference to the instruction lastly generated
    //! during code selection.
    void setCurrentInstruction( LLIR_Instruction * );

    //! getCurrentInstr retrieves the reference to the instruction lastly
    //! generated during code selection.
    LLIR_Instruction *getCurrentInstruction() const;

    //! addDebugInfo assigns assembler debug information to the given LLIR instruction.
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
    void addDebugInfo( WIR::WIR_Instruction &, const IR_Stmt *,
                       enum StmtType ) const;

    void insertADD( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertADD( LLIR_Register *, LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertADD( const std::string &, const std::string &, LLIR_Register *,
                    LLIR_Register *, LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertADD( const std::string &, const std::string &, LLIR_Register *,
                    LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertADD( const std::string &, LLIR_Register *, LLIR_Register *, int,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT);

    void insertADD( const std::string &, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT);

    void insertADD( const std::string &, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, std::string, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertADD( const std::string &, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, std::string, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertADC( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertADC( const std::string &, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertADC( LLIR_Register *, LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertAND( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertAND( LLIR_Register *, LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    //B{cond} target_address
    void insertB( const std::string &, int, IR_Exp * = nullptr,
                  StmtType type = EXP_STMT );

    // B{cond} target_label
    void insertB( const std::string &, const std::string &, IR_Exp * = nullptr,
      StmtType type = EXP_STMT );

    // B target_address
    void insertB( int, IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertBIC( const std::string&, const std::string&, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, IR_Exp * = nullptr, StmtType = EXP_STMT );

    //BL{cond} target_address
    void insertBL( const std::string &, int, std::deque<LLIR_Register *> * = nullptr,
                   IR_Exp * = nullptr, StmtType type = EXP_STMT );

    //BL{cond} target_label
    void insertBL( const std::string &, const std::string &, std::deque<LLIR_Register *> * = nullptr,
                   IR_Exp * = nullptr, StmtType type = EXP_STMT );

    //BL target_address
    void insertBL( int, std::deque<LLIR_Register *> * = nullptr, IR_Exp * = nullptr,
                   StmtType type = EXP_STMT );

    //BX target_address
    void insertBX( LLIR_Register*, std::deque<LLIR_Register *> * = nullptr,
                   IR_Exp * = nullptr,
                   StmtType type = EXP_STMT );

    //BX{cond} target_address
    void insertBX( const std::string &,LLIR_Register*,
                   std::deque<LLIR_Register *> * = nullptr, IR_Exp * = nullptr,
                   StmtType type = EXP_STMT );

    // CMP{cond} Rn, shifter_operand
    void insertCMP( const std::string &, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    // CMP{cond} Rn, shifter_operand
    void insertCMP( const std::string &, LLIR_Register *, int , IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertEOR( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    // EOR Rd, Rm
    void insertEOR( LLIR_Register *, LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    // Exact format: LDM{cond}addressing_mode Rn{!}, registers
    void insertLDM( const std::string &, const std::string &, LLIR_Register *,
                    const std::string &, std::deque<LLIR_Register *> *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    //LDR{cond} Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDR( const std::string &, LLIR_Register *, LLIR_Register *,
                    int, IR_Exp * = nullptr, StmtType type = EXP_STMT );

    // LDR{cond} Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDR( const std::string &, LLIR_Register *,
                    LLIR_Register *, LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    // LDR{cond} Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDR( const std::string& l0, LLIR_Register *r0, LLIR_Register* r1,
                    const std::string& l1, LLIR_Register* r2, IR_Exp* exp = nullptr,
                    StmtType type = EXP_STMT );

    //LDR{cond} Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDR( const std::string &, LLIR_Register *, LLIR_Register *,
                    LLIR_Register * , std::string, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    //LDR{cond} Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDR( const std::string &, LLIR_Register *, LLIR_Register *,
                    const std::string &, LLIR_Register * , std::string, int,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    //LDR{cond} Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDR( const std::string &, LLIR_Register *, bool, const std::string,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    //LDR{cond}B Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDRB( const std::string &, LLIR_Register *,
                     LLIR_Register *, int, IR_Exp * = nullptr,
                      StmtType type = EXP_STMT );

    //LDR{cond}B Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDRB( const std::string &, LLIR_Register *, LLIR_Register *,
                     LLIR_Register *, IR_Exp * = nullptr, StmtType type = EXP_STMT );

    // LDR{cond}B Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDRB( const std::string& l0, LLIR_Register *r0, LLIR_Register* r1,
                     const std::string& l1, LLIR_Register* r2, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //LDR{cond}B Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDRB( const std::string &, LLIR_Register *, LLIR_Register *,
                     LLIR_Register * , std::string, int, IR_Exp * = nullptr,
                     StmtType type = EXP_STMT );

    //LDR{cond}B Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDRB( const std::string &, LLIR_Register *, LLIR_Register *,
                    const std::string &, LLIR_Register * , std::string, int,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    //LDR{cond}H Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDRH( const std::string &, LLIR_Register *,
                     LLIR_Register *, int, IR_Exp * = nullptr,
                     StmtType type = EXP_STMT );

    //LDR{cond}H Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDRH( const std::string &, LLIR_Register *, LLIR_Register *,
                     LLIR_Register *, IR_Exp * = nullptr,
                     StmtType type = EXP_STMT );

    // LDR{cond}H Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDRH( const std::string& l0, LLIR_Register *r0, LLIR_Register* r1,
                     const std::string& l1, LLIR_Register* r2, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //LDR{cond}SH Rd, addressing_mode
    void insertLDRSH( const std::string &, LLIR_Register *,
                      LLIR_Register *, int, IR_Exp * = nullptr,
                      StmtType type = EXP_STMT );

    //LDR{cond}SH Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDRSH( const std::string &, LLIR_Register *, LLIR_Register *,
                     LLIR_Register *, IR_Exp * = nullptr,
                     StmtType type = EXP_STMT );

    // LDR{cond}SH Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDRSH( const std::string& l0, LLIR_Register *r0, LLIR_Register* r1,
                     const std::string& l1, LLIR_Register* r2, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //LDR{cond}SB Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDRSB( const std::string &, LLIR_Register *,
                      LLIR_Register *, int, IR_Exp * = nullptr,
                      StmtType type = EXP_STMT );

    //LDR{cond}SB Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDRSB( const std::string &, LLIR_Register *, LLIR_Register *,
                     LLIR_Register *, IR_Exp * = nullptr,
                     StmtType type = EXP_STMT );

    // LDR{cond}SB Rd, addressing_mode, cond = OPER_AL not passed
    void insertLDRSB( const std::string& l0, LLIR_Register *r0, LLIR_Register* r1,
                     const std::string& l1, LLIR_Register* r2, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    void insertMUL( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertMUL( const std::string &, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertMLA( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertMLA( const std::string &, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertUMULL( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      LLIR_Register *, IR_Exp * = nullptr,
                      StmtType type = EXP_STMT );

    void insertUMULL( const std::string &, LLIR_Register *, LLIR_Register *,
                      LLIR_Register *, LLIR_Register *, IR_Exp * = nullptr,
                      StmtType type = EXP_STMT );

    void insertUMLAL( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      LLIR_Register *, IR_Exp * = nullptr,
                      StmtType type = EXP_STMT );

    void insertUMLAL( const std::string &, LLIR_Register *, LLIR_Register *,
                      LLIR_Register *, LLIR_Register *, IR_Exp * = nullptr,
                      StmtType type = EXP_STMT );

    void insertSMULL( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      LLIR_Register *, IR_Exp * = nullptr,
                      StmtType type = EXP_STMT );

    void insertSMULL( const std::string &, LLIR_Register *, LLIR_Register *,
                      LLIR_Register *, LLIR_Register *, IR_Exp * = nullptr,
                      StmtType type = EXP_STMT );

    void insertSMLAL( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                      LLIR_Register *, IR_Exp * = nullptr,
                      StmtType type = EXP_STMT );

    void insertSMLAL( const std::string &, LLIR_Register *, LLIR_Register *,
                      LLIR_Register *, LLIR_Register *, IR_Exp * = nullptr,
                      StmtType type = EXP_STMT );

    void insertUMODSI( const std::string&, LLIR_Register*, LLIR_Register*,
                       LLIR_Register*, IR_Exp* = nullptr,
                       StmtType type = EXP_STMT );

    void insertSMODSI( const std::string&, LLIR_Register*, LLIR_Register*,
                       LLIR_Register*, IR_Exp* = nullptr,
                       StmtType type = EXP_STMT );

    void insertMOV( LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertMOV( LLIR_Register *, LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    //Exact format: MOV{cond}{S} Rd, shifter_operand
    void insertMOV( LLIR_Register *, LLIR_Register *, std::string, int,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    //Exact format: MOV{cond}{S} Rd, shifter_operand
    void insertMOV( const std::string &, LLIR_Register *, LLIR_Register *,
                    const std::string &, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertMOV( const std::string &, LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    // MOV{con}{s} RD, RM, <shift_mode>, RM
    void insertMOV( const std::string&, const std::string&, LLIR_Register *,
                    LLIR_Register*, const std::string&, LLIR_Register*,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT);

    void insertMOV( const std::string&, const std::string&, LLIR_Register *,
                    LLIR_Register*, const std::string&, int,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT);

    void insertMVN( const std::string &, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertMVN( const std::string &, LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT);

    void insertORR( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertORR( LLIR_Register *, LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertORR( const std::string &, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertORR( const std::string &, LLIR_Register *, LLIR_Register *, int,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertRSB( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertRSB( LLIR_Register *, LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertRSB( const std::string &, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertRSB( const std::string &, LLIR_Register *, LLIR_Register *, int,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertRSC( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertRSC( LLIR_Register *, LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertSBC( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertSBC( const std::string &, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertSBC( LLIR_Register *, LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    //STM{cond}addressing_mode Rn{!}, registers
    void insertSTM( const std::string &, const std::string &, LLIR_Register *,
                    const std::string &, std::deque<LLIR_Register *> *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    //STR{cond} Rd, addressing_mode
    void insertSTR( const std::string &, const std::string &, LLIR_Register *,
                    LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    //STR{cond} Rd, addressing_mode
    void insertSTR( const std::string &, const std::string &, LLIR_Register *,
                    LLIR_Register *, LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    //STR{cond} Rd, addressing_mode
    void insertSTR( const std::string &, const std::string&, LLIR_Register *, LLIR_Register*,
                    const std::string&, LLIR_Register*, IR_Exp* exp = nullptr,
                    StmtType type = EXP_STMT );

    //STR{cond} Rd, addressing_mode
    void insertSTR( const std::string &, const std::string &,
                    LLIR_Register *, LLIR_Register *,
                    LLIR_Register * , const std::string &, int,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    //STR{cond} Rd, addressing_mode
    void insertSTR( const std::string &, const std::string &,
                    LLIR_Register *, LLIR_Register *,
                    const std::string &, LLIR_Register * , const std::string &, int,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    //STR{cond}B Rd, addressing_mode
    void insertSTRB( const std::string &, const std::string &, LLIR_Register *,
                     LLIR_Register *, int, IR_Exp * = nullptr,
                     StmtType type = EXP_STMT );

    //STR{cond}B Rd, addressing_mode
    void insertSTRB( const std::string &, const std::string &, LLIR_Register *,
                    LLIR_Register *, LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    //STR{cond}B Rd, addressing_mode
    void insertSTRB( const std::string &, const std::string&, LLIR_Register *, LLIR_Register*,
                    const std::string&, LLIR_Register*, IR_Exp* exp = nullptr,
                    StmtType type = EXP_STMT );

    //STR{cond}B Rd, addressing_mode
    void insertSTRB( const std::string &, const std::string &,
                    LLIR_Register *, LLIR_Register *,
                    LLIR_Register * , const std::string &, int,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    //STR{cond}B Rd, addressing_mode
    void insertSTRB( const std::string &, const std::string &,
                    LLIR_Register *, LLIR_Register *,
                    const std::string &, LLIR_Register * , const std::string &, int,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    //STR{cond}H Rd, addressing_mode
    void insertSTRH( const std::string &, const std::string &, LLIR_Register *,
                     LLIR_Register *, int, IR_Exp * = nullptr,
                     StmtType type = EXP_STMT );

    //STR{cond}H Rd, addressing_mode
    void insertSTRH( const std::string &, const std::string &, LLIR_Register *,
                    LLIR_Register *, LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    //STR{cond}H Rd, addressing_mode
    void insertSTRH( const std::string &, const std::string&, LLIR_Register *,
                     LLIR_Register*, const std::string&, LLIR_Register*,
                     IR_Exp* exp = nullptr, StmtType type = EXP_STMT );

    void insertSUB( LLIR_Register *,  LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertSUB( LLIR_Register *, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertSUB( const std::string &, const std::string &, LLIR_Register *,
                    LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT);

    void insertSUB( const std::string &, const std::string &, LLIR_Register *,
                    LLIR_Register *, LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT);

    void insertSUB( const std::string &, LLIR_Register *, LLIR_Register *,
                    int, IR_Exp * = nullptr, StmtType type = EXP_STMT);

    void insertSUB( const std::string &, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT);

    void insertSUB( const std::string &, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, std::string, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertSUB( const std::string &, LLIR_Register *, LLIR_Register *,
                    LLIR_Register *, std::string, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    void insertTEQ( LLIR_Register *, LLIR_Register *, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertTEQ( LLIR_Register *, int, IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertTEQ( const std::string &, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    // TST{cond} Rn, shifter_operand
    void insertTST( const std::string &, LLIR_Register *, LLIR_Register *,
                    IR_Exp * = nullptr, StmtType type = EXP_STMT );

    // TST{cond} Rn, shifter_operand
    void insertTST( const std::string &, LLIR_Register *, int , IR_Exp * = nullptr,
                    StmtType type = EXP_STMT );

    void insertASMWordDir( const std::string &,  LLIR_BB*, IR_Exp * = nullptr,
                           StmtType type = EXP_STMT );

    //! Inserts a combination of a MOV and ORRs to generate a 32 bit constant.
    /*!
      In order to move a 32 bit immediate into a register, we use a combination
      of up to 4 subsequent instructions (first a mov, then logical or) to place
      the constant into the given register.
      @param reg The target register.
      @param constant The given constant.
      @param irExp The corresponding IR Expression.
      @param stmtType The corresponding IR statement type.
    */
    void insertMOV_ORR( LLIR_Register* reg, const int constant,
                        IR_Exp* irExp = nullptr, StmtType stmtType = EXP_STMT );

    //! Returns the combined cost of the MOV and ORRs of insertMOV_ORR.
    /*!
      Always returns CT( INS_MOV_32 ) plus the correct number of ORRs inserted
      when using insertMOV_ORR to load constants into a register.
      @param constant The constant to load.
    */
    COST insertMOV_ORR_Cost( const int constant );

    // Operations for FLOATs

    void insertFloatOperation( LLIR_Register *reg, const SoftFloatSymbol &,
                               IR_Exp * = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to calculate a float add.
    /*!
     * Inserts the required instructions to perform a floating point addition.
     * @param cond Condition code of the addition.
     * @param result Result register.
     * @param reg0 First floating point parameter.
     * @param reg1 Second floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertADD_F( const std::string& cond, LLIR_Register* result, LLIR_Register*
                      reg0, LLIR_Register* reg1, IR_Exp* exp = nullptr,
                      StmtType type = EXP_STMT );

    //! Inserts instructions to calculate a double add.
    /*!
     * Inserts the required instructions to perform a double precision floating
     * point addition.
     * @param cond Condition code of the addition.
     * @param eresult Result register.
     * @param ereg0 First double precision floating point parameter.
     * @param ereg1 Second double precision floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertADD_D( const std::string &cond, LLIR_Register *eresult,
                      LLIR_Register *ereg0, LLIR_Register *ereg1,
                      IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to calculate a long long int add.
    /*!
     * Inserts the required instructions to perform a long long integer
     * addition.
     * @param cond Condition code of the addition.
     * @param eresult Result register.
     * @param ereg0 First long long integer parameter.
     * @param ereg1 Second long long integer parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertADD_LL( const std::string &cond, LLIR_Register *eresult,
                       LLIR_Register *ereg0, LLIR_Register *ereg1,
                       IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to calculate a float sub.
    /*!
     * Inserts the required instructions to perform a floating point
     * subtraction.
     * @param cond Condition code of the subtraction.
     * @param result Result register.
     * @param reg0 First floating point parameter.
     * @param reg1 Second floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertSUB_F( const std::string& cond, LLIR_Register* result, LLIR_Register*
                      reg0, LLIR_Register* reg1, IR_Exp* exp = nullptr,
                      StmtType type = EXP_STMT );

    //! Inserts instructions to calculate a double sub.
    /*!
     * Inserts the required instructions to perform a double precision floating
     * point subtraction.
     * @param cond Condition code of the subtraction.
     * @param eresult Result register.
     * @param ereg0 First double precision floating point parameter.
     * @param ereg1 Second double precision floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertSUB_D( const std::string &cond, LLIR_Register *eresult,
                      LLIR_Register *ereg0, LLIR_Register *ereg1,
                      IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to calculate a long long int sub.
    /*!
     * Inserts the required instructions to perform a long long integer
     * subtraction.
     * @param cond Condition code of the subtraction.
     * @param eresult Result register.
     * @param ereg0 First long long integer parameter.
     * @param ereg1 Second long long integer parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertSUB_LL( const std::string &cond, LLIR_Register *eresult,
                       LLIR_Register *ereg0, LLIR_Register *ereg1,
                       IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to calculate a float mul.
    /*!
     * Inserts the required instructions to perform a floating point
     * multiplication.
     * @param cond Condition code of the multiplication.
     * @param result Result register.
     * @param reg0 First floating point parameter.
     * @param reg1 Second floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertMUL_F( const std::string& cond, LLIR_Register* result, LLIR_Register*
                      reg0, LLIR_Register* reg1, IR_Exp* exp = nullptr,
                      StmtType type = EXP_STMT );

    //! Inserts instructions to calculate a double mul.
    /*!
     * Inserts the required instructions to perform a double precision floating
     * point multiplication.
     * @param cond Condition code of the double multiplication.
     * @param eresult Result register.
     * @param ereg0 First double precision floating point parameter.
     * @param ereg1 Second double precision floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertMUL_D( const std::string &cond, LLIR_Register *eresult,
                      LLIR_Register *ereg0, LLIR_Register *ereg1,
                      IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to calculate a long long int mul.
    /*!
     * Inserts the required instructions to perform a long long integer
     * multiplication.
     * @param cond Condition code of the multiplication.
     * @param eresult Result register.
     * @param ereg0 First long long integer parameter.
     * @param ereg1 Second long long integer parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertMUL_LL( const std::string &cond, LLIR_Register *eresult,
                       LLIR_Register *ereg0, LLIR_Register *ereg1,
                       IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to calculate a float div.
    /*!
     * Inserts the required instructions to perform a floating point division.
     * @param cond Condition code of the division.
     * @param result Result register.
     * @param reg0 First floating point parameter.
     * @param reg1 Second floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertDIV_F( const std::string& cond, LLIR_Register* result, LLIR_Register*
                      reg0, LLIR_Register* reg1, IR_Exp* exp = nullptr,
                      StmtType type = EXP_STMT );

    //! Inserts instructions to calculate a double div.
    /*!
     * Inserts the required instructions to perform a double precision floating
     * point division.
     * @param cond Condition code of the division.
     * @param eresult Result register.
     * @param ereg0 First double precision floating point parameter.
     * @param ereg1 Second double precision floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertDIV_D( const std::string &cond, LLIR_Register *eresult,
                      LLIR_Register *ereg0, LLIR_Register *ereg1,
                      IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to calculate a long long int div.
    /*!
     * Inserts the required instructions to perform a long long integer
     * division.
     * @param cond Condition code of the division.
     * @param eresult Result register.
     * @param ereg0 First long long integer parameter.
     * @param ereg1 Second long long integer parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertDIV_LL( const std::string &cond, LLIR_Register *eresult,
                       LLIR_Register *ereg0, LLIR_Register *ereg1,
                       IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to calculate an unsigned long long int div.
    /*!
     * Inserts the required instructions to perform an unsigned long long
     * integer division.
     * @param cond Condition code of the division.
     * @param eresult Result register.
     * @param ereg0 First unsigned long long integer parameter.
     * @param ereg1 Second unsigned long long integer parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertDIV_ULL( const std::string &cond, LLIR_Register *eresult,
                        LLIR_Register *ereg0, LLIR_Register *ereg1,
                        IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to perform a ll/ull logical bit shift left.
    /*!
     * Inserts the required instructions to perform a logical bit shift left
     * with two signed or unsigned long long integer operands.
     * @param cond Condition code of the division.
     * @param eresult Result register.
     * @param ereg0 First long long integer parameter.
     * @param ereg1 Second long long integer parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertSHL_LL( const std::string &cond, LLIR_Register *eresult,
                       LLIR_Register *ereg0, LLIR_Register *ereg1,
                       IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to perform a ll bit shift right.
    /*!
     * Inserts the required instructions to perform a bit shift right of an
     * signed long long integer.
     * @param cond Condition code of the bit shift.
     * @param eresult Result register.
     * @param ereg0 First long long integer parameter.
     * @param ereg1 Second integer parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertSHR_LL( const std::string &cond, LLIR_Register *eresult,
                       LLIR_Register *ereg0, LLIR_Register *ereg1,
                       IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to perform a ull logical bit shift right.
    /*!
     * Inserts the required instructions to perform a bit shift right of an
     * unsigned long long integer.
     * @param cond Condition code of the bit shift.
     * @param eresult Result register.
     * @param ereg0 First unsigned long long integer parameter.
     * @param ereg1 Second integer parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertSHR_ULL( const std::string &cond, LLIR_Register *eresult,
                        LLIR_Register *ereg0, LLIR_Register *ereg1,
                        IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to calculate an unsigned long long int mod.
    /*!
     * Inserts the required instructions to perform an unsigned long long
     * integer modulo operation.
     * @param cond Condition code of the modulo operation.
     * @param eresult Result register.
     * @param ereg0 First unsigned long long integer parameter.
     * @param ereg1 Second unsigned long long integer parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertMOD_LL( const std::string &cond, LLIR_Register *eresult,
                       LLIR_Register *ereg0, LLIR_Register *ereg1,
                       IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to calculate an unsigned long long int mod.
    /*!
     * Inserts the required instructions to perform an unsigned long long
     * integer modulo operation.
     * @param cond Condition code of the modulo operation.
     * @param eresult Result register.
     * @param ereg0 First unsigned long long integer parameter.
     * @param ereg1 Second unsigned long long integer parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertMOD_ULL( const std::string &cond, LLIR_Register *eresult,
                        LLIR_Register *ereg0, LLIR_Register *ereg1,
                        IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to calculate a signed integer div.
    /*!
     * Inserts the required instructions to perform a signed integer division.
     * @param cond Condition code of the division.
     * @param result Result register.
     * @param reg0 First parameter (dividend).
     * @param reg1 Second floating point parameter (divisor).
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertSDIVSI( const std::string& cond, LLIR_Register* result,
                       LLIR_Register *reg0, LLIR_Register *reg1,
                       IR_Exp* exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to calculate an unsigned integer div.
    /*!
     * Inserts the required instructions to perform an unsigned integer division.
     * @param cond Condition code of the division.
     * @param result Result register.
     * @param reg0 First parameter (dividend).
     * @param reg1 Second floating point parameter (divisor).
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertUDIVSI( const std::string& cond, LLIR_Register* result,
                       LLIR_Register *reg0, LLIR_Register *reg1,
                       IR_Exp* exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts the call to handle a double to float conversion.
    /*!
     * @brief insertDTOF inserts the call to a double to float conversion
     * routine and places the result into the given register.
     * @param cond Conditonal code.
     * @param result Result register.
     * @param ereg Source register (pseudo-extended).
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertDTOF( const std::string& cond, LLIR_Register* result,
                     LLIR_Register* ereg, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

     //! Inserts the call to handle a float to double conversion.
    /*!
     * @brief insertFTOD inserts the call to a float to double conversion
     * routine and places the result into the given extended register.
     * @param cond Conditonal code.
     * @param result Extended result register.
     * @param reg Source register.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertFTOD( const std::string& cond, LLIR_Register* eresult,
                     LLIR_Register* reg, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts the call to handle a double to integer conversion.
    /*!
     * @brief insertDTOI inserts the call to a double to integer conversion
     * routine and places the result into the given register.
     * @param cond Conditonal code.
     * @param result Result register.
     * @param ereg0 Source register (pseudo-extended).
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertDTOI( const std::string &cond, LLIR_Register *result,
                     LLIR_Register *ereg0, IR_Exp *exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts the call to handle a double to an unsigned integer conversion.
    /*!
     * @brief insertDTOU inserts the call to a double to an unsigned integer
     * conversion routine and places the result into the given register.
     * @param cond Conditonal code.
     * @param result Result register.
     * @param ereg0 Source register (pseudo-extended).
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertDTOU( const std::string &cond, LLIR_Register *result,
                     LLIR_Register *ereg0, IR_Exp *exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts the call to handle a double to long long integer conversion.
    /*!
     * @brief insertDTOLL inserts the call to a double to long long integer
     * conversion routine and places the result into the given register.
     * @param cond Conditonal code.
     * @param eresult Result register.
     * @param ereg Source register (pseudo-extended).
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertDTOLL( const std::string& cond, LLIR_Register* eresult,
                     LLIR_Register* ereg, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts the call to handle a long long integer to double conversion.
    /*!
     * @brief insertLLTOD inserts the call to a long long integer to double
     * conversion routine and places the result into the given register.
     * @param cond Conditonal code.
     * @param eresult Result register.
     * @param ereg Source register (pseudo-extended).
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertLLTOD( const std::string& cond, LLIR_Register* eresult,
                     LLIR_Register* ereg, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts the call to handle a long long integer to float conversion.
    /*!
     * @brief insertLLTOF inserts the call to a long long integer to float
     * conversion routine and places the result into the given register.
     * @param cond Conditonal code.
     * @param res Result register.
     * @param ereg Source register (pseudo-extended).
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertLLTOF( const std::string& cond, LLIR_Register* res,
                     LLIR_Register* ereg, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts the call to handle a unsigned long long integer to float conversion.
    /*!
     * @brief insertULLTOF inserts the call to a uns. long long integer to float
     * conversion routine and places the result into the given register.
     * @param cond Conditonal code.
     * @param res Result register.
     * @param ereg Source register (pseudo-extended).
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertULLTOF( const std::string& cond, LLIR_Register* res,
                     LLIR_Register* ereg, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts the call to handle a double to unsigned long long integer
    //! conversion.
    /*!
     * @brief insertDTOULL inserts the call to a double to unsigned long long
     * integer conversion routine and places the result into the given register.
     * @param cond Conditonal code.
     * @param eresult Result register.
     * @param ereg Source register (pseudo-extended).
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertDTOULL( const std::string& cond, LLIR_Register* eresult,
                     LLIR_Register* ereg, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts the call to handle a unsigned long long integer to double
    //! conversion.
    /*!
     * @brief insertULLTOD inserts the call to a unsigned long long integer to
     * double conversion routine and places the result into the given register.
     * @param cond Conditonal code.
     * @param eresult Result register.
     * @param ereg Source register (pseudo-extended).
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertULLTOD( const std::string& cond, LLIR_Register* eresult,
                     LLIR_Register* ereg, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts instructions to check a greater than relation of two double
    //! values.
    /*!
     * Inserts instructions to check the greater than relation between two
     * double precision floating point parameters and places the result into the
     * given register.
     * @brief insertGT_D
     * @param cond Conditonal code.
     * @param result Result register where an integer ( 0 or 1 ) is stored.
     * @param ereg0 First double precision floating point parameter.
     * @param ereg1 Second double precision floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertGT_D( const std::string &cond, LLIR_Register *result,
                     LLIR_Register *ereg0, LLIR_Register *ereg1,
                     IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to check a lower than relation of two double
    //! values.
    /*!
     * Inserts instructions to check the lower than relation between two
     * double precision floating point parameters and places the result into the
     * given register.
     * @brief insertLT_D
     * @param cond Conditonal code.
     * @param result Result register where an integer ( 0 or 1 ) is stored.
     * @param ereg0 First double precision floating point parameter.
     * @param ereg1 Second double precision floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertLT_D( const std::string &cond, LLIR_Register *result,
                     LLIR_Register *ereg0, LLIR_Register *ereg1,
                     IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to check a greater than or equal to relation of two
    //! double values.
    /*!
     * Inserts instructions to check the greater than or equal to relation
     * between two double precision floating point parameters and places the
     * result into the given register.
     * @brief insertGEQ_D
     * @param cond Conditonal code.
     * @param result Result register where an integer ( 0 or 1 ) is stored.
     * @param ereg0 First double precision floating point parameter.
     * @param ereg1 Second double precision floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertGEQ_D( const std::string &cond, LLIR_Register *result,
                     LLIR_Register *ereg0, LLIR_Register *ereg1,
                     IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to check a lower than or equal to relation of two
    //! double values.
    /*!
     * Inserts instructions to check the lower than or equal to relation between
     * two double precision floating point parameters and places the result into
     * the given register.
     * @brief insertLEQ_D
     * @param cond Conditonal code.
     * @param result Result register where an integer ( 0 or 1 ) is stored.
     * @param ereg0 First double precision floating point parameter.
     * @param ereg1 Second double precision floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertLEQ_D( const std::string &cond, LLIR_Register *result,
                     LLIR_Register *ereg0, LLIR_Register *ereg1,
                     IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to test the equality two double values.
    /*!
     * Inserts instructions to perform an equality comparison of two double
     * precision floating point parameters and places the result into the given
     * register. The result is one, if they are equal.
     * @brief insertEQ_D
     * @param cond Conditonal code.
     * @param result Result register where an integer ( 0 or 1 ) is stored.
     * @param ereg0 First double precision floating point parameter.
     * @param ereg1 Second double precision floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertEQ_D( const std::string &cond, LLIR_Register *result,
                     LLIR_Register *ereg0, LLIR_Register *ereg1,
                     IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to test the inequality of two double values.
    /*!
     * Inserts instructions to perform an inequality comparison of two double
     * precision floating point parameters and places the result into the given
     * register. The result is one, if they are not equal.
     * @brief insertNEQ_D
     * @param cond Conditonal code.
     * @param result Result register where an integer ( 0 or 1 ) is stored.
     * @param ereg0 First double precision floating point parameter.
     * @param ereg1 Second double precision floating point parameter.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertNEQ_D( const std::string &cond, LLIR_Register *result,
                     LLIR_Register *ereg0, LLIR_Register *ereg1,
                     IR_Exp *exp = nullptr, StmtType type = EXP_STMT );

    //! Inserts instructions to check lower-than for two floats.
    /*!
     * @brief insertLT_F inserts instructions to check lower-than for two
     * floats.
     * @param cond Condition code.
     * @param result Result register of the compare.
     * @param reg0 First register to compare.
     * @param reg1 Second register to compare.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertLT_F( const std::string& cond, LLIR_Register* result, LLIR_Register*
                     reg0, LLIR_Register* reg1, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts instructions to check lower-equal for two floats.
    /*!
     * @brief insertLT_F inserts instructions to check lower-equal for two
     * floats.
     * @param cond Condition code.
     * @param result Result register of the compare.
     * @param reg0 First register to compare.
     * @param reg1 Second register to compare.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertLE_F( const std::string& cond, LLIR_Register* result, LLIR_Register*
                     reg0, LLIR_Register* reg1, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts instructions to check greater-than for two floats.
    /*!
     * @brief insertLT_F inserts instructions to check greater-than for two
     * floats.
     * @param cond Condition code.
     * @param result Result register of the compare.
     * @param reg0 First register to compare.
     * @param reg1 Second register to compare.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertGT_F( const std::string& cond, LLIR_Register* result, LLIR_Register*
                     reg0, LLIR_Register* reg1, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts instructions to check greater-equal for two floats.
    /*!
     * @brief insertLT_F inserts instructions to check greater-equal for two
     * floats.
     * @param cond Condition code.
     * @param result Result register of the compare.
     * @param reg0 First register to compare.
     * @param reg1 Second register to compare.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertGE_F( const std::string& cond, LLIR_Register* result, LLIR_Register*
                     reg0, LLIR_Register* reg1, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts instructions to check equality for two floats.
    /*!
     * @brief insertEQ_F inserts instructions to check equality for two
     * floats.
     * @param cond Condition code.
     * @param result Result register of the compare.
     * @param reg0 First register to compare.
     * @param reg1 Second register to compare.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertEQ_F( const std::string& cond, LLIR_Register* result, LLIR_Register*
                     reg0, LLIR_Register* reg1, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );

    //! Inserts instructions to check non-equality for two floats.
    /*!
     * @brief insertNE_F inserts instructions to check non-equality for two
     * floats.
     * @param cond Condition code.
     * @param result Result register of the compare.
     * @param reg0 First register to compare.
     * @param reg1 Second register to compare.
     * @param exp Optional IR expression.
     * @param type Optional IR Stmt Type.
     */
    void insertNE_F( const std::string& cond, LLIR_Register* result, LLIR_Register*
                     reg0, LLIR_Register* reg1, IR_Exp* exp = nullptr,
                     StmtType type = EXP_STMT );


    //! CreateRegister creates a new LLIR register.
    LLIR_Register *CreateRegister( std::string target, bool isAddrReg = false );

    //! CreateERegister creates a new pseudo extended 64 bit register.
    LLIR_Register *CreateERegister( std::string, LLIR_Register * = nullptr,
                                    LLIR_Register * = nullptr );

    ARM7_CodeSelector *getCodeSelector();

    const std::string &getSoftFloatSymbol( SoftFloatSymbol ) const;

    WIR::WIR_Function &getSoftFloatFunction( SoftFloatSymbol ) const;


  private:

    //! handleDataAccess creates data access pragmas for instructions.
    /*!
     The information which data objects are accessed is calculated from the
     param exp and added to the param ins as a pragma with the content
     "DATA ACCESS: _objectname"
     */
    void handleDataAccess(LLIR_Instruction*ins, IR_Exp* exp, bool deref = false);

    //! The current configuration.
    Configuration &mConfig;

    //! The current code selector.
    ARM7_CodeSelector &mCodesel;

    //! mCurrentInstr points to the instruction lastly generated during code selection.
    LLIR_Instruction *mCurrentInstr;

};

#endif  // _INSTRUCTION_FACTORY_ARM7_H
