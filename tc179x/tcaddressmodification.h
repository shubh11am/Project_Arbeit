/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2020 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _TC_ADDRESSMODIFICATION_H
#define _TC_ADDRESSMODIFICATION_H


//
// Include section
//

// Include standard headers
#include <string>
#include <utility>

// Include WIR headers
// We have to include the header here, since C++ does not allow the forward
// declaration of nested classes like, e.g., class TC13::AddressingMode...
#include <arch/tricore/tc13.h>

// Include local headers
#include <codesel/addressmodification.h>
#include <tc179x/cs_tc179x.h>


//
// Class forward declarations
//

class IR_Exp;
class IR_Type;
class LLIR_Register;

namespace WIR {
class WIR_BaseRegister;
class WIR_Data;
class WIR_Function;
class TC_ARegV;
}

class TC_LValue;


//
// Header section
//

/*!
  @brief Class TC_AddressModification describes an address register increment or
         decrement.

  This is used for the generation of TriCore LD/ST operations with
  pre-/postincrement mode.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_AddressModification : public AddressModification
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Constructor initializing an empty address modification.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AddressModification( void );

    /*!
      @brief Constructor for register+offset-based address modifications.

      @param[in] reg A pointer to the involved LLIR address register.
      @param[in] r A const reference to the involved %WIR address register.
      @param[in] o A long integer denoting the offset of an address
                   modification.
      @param[in] t A pointer to the IR data type that is associated with the
                   accessed memory location.
      @param[in] bo A Boolean that specifies whether the offset is a byte offset
                    or not.
      @param[in] mt A specifier of the address modification time.
      @param[in] mo A specifier of the address modification operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AddressModification( LLIR_Register *, const WIR::WIR_BaseRegister &,
                            long, IR_Type *, bool, ModTime = ModTime::NONE,
                            ModOper = ModOper::ADD );

    /*!
      @brief Constructor for data label-based address modifications.

      @param[in] l A const reference to a string denoting the LLIR label to be
                   used for the memory access.
      @param[in] d A const reference to a %WIR data object to be used as label
                   for the memory access.
      @param[in] t A pointer to the IR data type that is associated with the
                   accessed memory location.
      @param[in] bo A Boolean that specifies whether the offset is a byte offset
                    or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AddressModification( const std::string &, const WIR::WIR_Data &,
                            IR_Type *, bool );

    /*!
      @brief Constructor for function label-based address modifications.

      @param[in] l A const reference to a string denoting the LLIR label to be
                   used for the memory access.
      @param[in] f A const reference to a %WIR function to be used as label for
                   the memory access.
      @param[in] t A pointer to the IR data type that is associated with the
                   accessed memory location.
      @param[in] bo A Boolean that specifies whether the offset is a byte offset
                    or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AddressModification( const std::string &, const WIR::WIR_Function &,
                            IR_Type *, bool );

    /*!
      @brief Constructor for global/stack-based base register and integer
             offset.

      @param[in] l A pointer to an lvalue specifying the base register.
      @param[in] o A long integer denoting the offset of an address
                   modification.
      @param[in] t A pointer to the IR data type that is associated with the
                   accessed memory location.
      @param[in] bo A Boolean that specifies whether the offset is a byte offset
                    or not.
      @param[in] mt A specifier of the address modification time.
      @param[in] mo A specifier of the address modification operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AddressModification( TC_LValue *, long, IR_Type *, bool,
                            ModTime = ModTime::NONE, ModOper = ModOper::ADD );

    /*!
      @brief Copy constructor.

      @param[in] am A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AddressModification( const TC_AddressModification & );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_AddressModification( void );

    /*!
      @brief Assignment operator.

      @param[in] am A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AddressModification & operator = ( const TC_AddressModification & );

    /*!
      @brief applyModification applies the pointer arithmetics encoded by this
             object.

      @param[in] exp A pointer to an IR expression.
      @return A pair of LLIR/WIR registers containing the readily computed
              address.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::pair<LLIR_Register *, WIR::TC_ARegV *> applyModification( IR_Exp * );

    /*!
      @brief applyModificationCost computes the costs for applying the pointer
             arithmetics encoded by this object.

      @return The cost value associated with the involved pointer arithmetics.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static COST applyModificationCost( void );

    /*!
      @brief createLoad generates code that loads the value from the effective
             address encoded by this object into the given register.

      @param[in] dest A pointer to the LLIR register to be loaded.
      @param[in] dst A pointer to the %WIR register to be loaded.
      @param[in] exp A pointer to an IR expression.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void createLoad( LLIR_Register *, WIR::WIR_BaseRegister *, IR_Exp * );

    /*!
      @brief createStore generates code that stores the given register to the
             effective address encoded by this object.

      @param[in] source A pointer to the LLIR register to be stored.
      @param[in] src A pointer to the %WIR register to be stored.
      @param[in] exp A pointer to an IR expression.
      @param[in] bfAccess A Boolean denoting whether a bit-field is written.
      @param[in] bfOffset An unsigned value denoting the bit-field offset.
      @param[in] bfLength An unsigned value denoting the bit-field length.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void createStore( LLIR_Register *, WIR::WIR_BaseRegister *, IR_Exp *,
                      bool, unsigned char, unsigned char );

    /*!
      @brief createStoreCost computes the costs for a store of a register to the
             effective address encoded by this object.

      @param[in] exp A pointer to an IR expression.
      @return The cost value associated with the involved store operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static COST createStoreCost( const IR_Exp * );

    /*!
      @brief getMemoryAccessMode returns the LLIR string specifying the access
             mode that should be used during the access that performs the
             described modification.

      @return A string denoting the TriCore-LLIR addressing mode to be used.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getMemoryAccessMode( void ) const;

    /*!
      @brief getByteOffset returns the byte offset that will be applied by the
             modification, depending on the actual offset, the size of the base
             IR type, whether a subtraction or addition is performed, or whether
             the modification has already been done or not.

      @return A signed long containing the byte offset.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    long getByteOffset( void ) const;


  protected:

    /*!
      @brief Inserts a LD.A instruction.

      @param[in] Aa A pointer to the first address register operand.
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

      Exact format: LD.A A[a] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is included. Exact
      formats:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.A A[a] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_A( LLIR_Register *, LLIR_Register *, int, const IR_Exp * );

    /*!
      @brief Inserts a LD.A instruction.

      @param[in] Aa A pointer to the first address register operand.
      @param[in] m A const reference to a string denoting a TriCore addressing
                   mode (either pre- or post-increment).
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
    void insertLD_A( LLIR_Register *, const std::string &, LLIR_Register *, int,
                     const IR_Exp * );

    /*!
      @brief Inserts a LD.A instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

      Exact format: LD.A A[a] (def), [A[b] (use)]off (AAC16BOA)

      Handling of address offsets beyond signed 16 bits is included. Exact
      formats:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.A A[a] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_A( const WIR::TC_ARegV &, const WIR::TC_ARegV &, int,
                     const IR_Exp * );

    /*!
      @brief Inserts a LD.A instruction.

      @param[in] Aa A const reference to the first address register operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
                     const WIR::TC_ARegV &, int, const IR_Exp * );

    /*!
      @brief Inserts a LD.B instruction.

      @param[in] Da A pointer to the first data register operand.
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
    void insertLD_B( LLIR_Register *, LLIR_Register *, int, const IR_Exp * );

    /*!
      @brief Inserts a LD.B instruction.

      @param[in] Da A pointer to the first data register operand.
      @param[in] m A const reference to a string denoting a TriCore addressing
                   mode (either pre- or post-increment).
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
    void insertLD_B( LLIR_Register *, const std::string &, LLIR_Register *, int,
                     const IR_Exp * );

    /*!
      @brief Inserts a LD.B instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
                     const IR_Exp * );

    /*!
      @brief Inserts a LD.B instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
                     const WIR::TC_ARegV &, int, const IR_Exp * );

    /*!
      @brief Inserts a LD.BU instruction.

      @param[in] Da A pointer to the first data register operand.
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
    void insertLD_BU( LLIR_Register *, LLIR_Register *, int, const IR_Exp * );

    /*!
      @brief Inserts a LD.BU instruction.

      @param[in] Da A pointer to the first data register operand.
      @param[in] m A const reference to a string denoting a TriCore addressing
                   mode (either pre- or post-increment).
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
    void insertLD_BU( LLIR_Register *, const std::string &, LLIR_Register *,
                      int, const IR_Exp * );

    /*!
      @brief Inserts a LD.BU instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
                      const IR_Exp * );

    /*!
      @brief Inserts a LD.BU instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
                      const WIR::TC_ARegV &, int, const IR_Exp * );

    /*!
      @brief Inserts a LD.D instruction.

      @param[in] Ea A pointer to the first data register operand.
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
    void insertLD_D( LLIR_Register *, LLIR_Register *, int, const IR_Exp * );

    /*!
      @brief Inserts a LD.D instruction.

      @param[in] Ea A pointer to the first extended data register operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
    void insertLD_D( LLIR_Register *, const std::string &, LLIR_Register *, int,
                     const IR_Exp * );

    /*!
      @brief Inserts a LD.D instruction.

      @param[in] Ea A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
                     const IR_Exp * );

    /*!
      @brief Inserts a LD.D instruction.

      @param[in] Ea A const reference to the first extended data register
                    operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
                     const WIR::TC_ARegV &, int, const IR_Exp * );

    /*!
      @brief Inserts a LD.H instruction.

      @param[in] Da A pointer to the first data register operand.
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
    void insertLD_H( LLIR_Register *, LLIR_Register *, int, const IR_Exp * );

    /*!
      @brief Inserts a LD.H instruction.

      @param[in] Da A pointer to the first data register operand.
      @param[in] m A const reference to a string denoting a TriCore addressing
                   mode (either pre- or post-increment).
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
    void insertLD_H( LLIR_Register *, const std::string &, LLIR_Register *, int,
                     const IR_Exp * );

    /*!
      @brief Inserts a LD.H instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
                     const IR_Exp * );

    /*!
      @brief Inserts a LD.H instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
                     const WIR::TC_ARegV &, int, const IR_Exp * );

    /*!
      @brief Inserts a LD.HU instruction.

      @param[in] Da A pointer to the first data register operand.
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
    void insertLD_HU( LLIR_Register *, LLIR_Register *, int, const IR_Exp * );

    /*!
      @brief Inserts a LD.HU instruction.

      @param[in] Da A pointer to the first data register operand.
      @param[in] m A const reference to a string denoting a TriCore addressing
                   mode (either pre- or post-increment).
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
    void insertLD_HU( LLIR_Register *, const std::string &, LLIR_Register *,
                      int, const IR_Exp * );

    /*!
      @brief Inserts a LD.HU instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
                      const IR_Exp * );

    /*!
      @brief Inserts a LD.HU instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
                      const WIR::TC_ARegV &, int, const IR_Exp * );

    /*!
      @brief Inserts a LD.W instruction.

      @param[in] Da A pointer to the first data register operand.
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

      Exact format: LD.W D[a] (def), [A[b] (use)]off (DAC16BOA)

      Handling of address offsets beyond signed 16 bits is included. Exact
      formats:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.W D[a] (def), [A[x] (use)]<lower 16 bits of off> (DAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_W( LLIR_Register *, LLIR_Register *, int, const IR_Exp * );

    /*!
      @brief Inserts a LD.W instruction.

      @param[in] Da A pointer to the first data register operand.
      @param[in] m A const reference to a string denoting a TriCore addressing
                   mode (either pre- or post-increment).
      @param[in] Ab A pointer to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

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
    void insertLD_W( LLIR_Register *, const std::string &, LLIR_Register *, int,
                     const IR_Exp * );

    /*!
      @brief Inserts a LD.W instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

      Exact format: LD.W D[a] (def), [A[b] (use)]off (DAC16BOA)

      Handling of address offsets beyond signed 16 bits is included. Exact
      formats:

      ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
      LD.W D[a] (def), [A[x] (use)]<lower 16 bits of off> (DAC16BOA)

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertLD_W( const WIR::TC_DRegV &, const WIR::TC_ARegV &, int,
                     const IR_Exp * );

    /*!
      @brief Inserts a LD.W instruction.

      @param[in] Da A const reference to the first data register operand.
      @param[in] m A const reference to a TriCore addressing mode (either pre-
                   or post-increment).
      @param[in] Ab A const reference to the second address register operand.
      @param[in] off A signed immediate offset.
      @param[in] exp A pointer that points to an IR expression to be used for
                     the generation of debug information for the newly inserted
                     assembly instruction.

      Exact formats:

      LD.W D[a] (def), [+A[b] (defuse)]off (AAC10PIA) \n
      LD.W D[a] (def), [A[b] (defuse)+]off (AAC10PIA)

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
                     const WIR::TC_ARegV &, int, const IR_Exp * );

    /*!
      @brief clone creates a copy of an address modification object.

      @return A pointer to the newly created copy of this object.

      clone just calls the corresponding copy constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual AddressModification *clone( void ) const;

    /*!
      @brief getOffsetFactor computes the factor by which the offset needs to be
             scaled according to a TC address modification's base type.

      @return An integer holding the offset's scaling factor.
    */
    virtual int getOffsetFactor( void ) const;

};

#endif  // _TC_ADDRESSMODIFICATION_H
