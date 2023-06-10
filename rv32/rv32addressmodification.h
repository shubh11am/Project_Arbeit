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
  @file rv32addressmodification.h
  @brief This file provides the interface of the RISC-V RV32 class describing
         address register modifications.
*/


#ifndef _RV32_ADDRESSMODIFICATION_H
#define _RV32_ADDRESSMODIFICATION_H


//
// Include section
//

// Include local headers
#include <codesel/addressmodification.h>
#include <rv32/rv32codesel.h>


//
// Class forward declarations
//

namespace WIR {
class WIR_BaseRegister;
class WIR_Data;
class RV_RegV;
}

class IR_Exp;
class IR_Type;


//
// Header section
//

namespace RV32 {

class RV32_LValue;

/*!
  @brief Class RV32_AddressModification describes an address register
         modification.

  The instruction sets of many architectures allow for very sophisticated LD/ST
  instructions applying immediate, register or shifted register offsets, in
  immediate, post-indexed or pre-indexed modes. For that reason, this class is
  designed to store all the details of such a modification so that it can be
  applied as part of a LD/ST instruction instead of additional instructions just
  exclusively performing address arithmetics.

  The RISC-V RV32IMC ISA, however, does not contain such sophisticated features,
  except classical base+offset addressing, so that class
  RV32_AddressModification is basically empty on purpose.
*/
class RV32_AddressModification : public AddressModification
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Constructor initializing an empty address modification.
    */
    RV32_AddressModification( void );

    /*!
      @brief Constructor for case with local base register and register offset.

      @param[in] reg A pointer to the LLIR base register.
      @param[in] oReg A pointer to the LLIR offset register.
      @param[in] t A pointer to the IR data type that is associated with the
                   accessed memory location.
      @param[in] mt A specifier of the address modification time.
      @param[in] mo A specifier of the address modification operation.
      @param[in] bo A Boolean defaulting to false that specifies whether the
                    offset is a byte offset or not.
      @param[in] dr A Boolean defaulting to false that is used to perform
                    dry-runs.
    */
    RV32_AddressModification( WIR::WIR_BaseRegister &, WIR::WIR_BaseRegister &, IR_Type *,
                             ModTime, ModOper, bool = false, bool = false );

    /*!
      @brief Constructor for register+offset-based address modifications.

      @param[in] r A const reference to the involved %WIR address register.
      @param[in] o A long integer denoting the offset of an address
                   modification.
      @param[in] t A pointer to the IR data type that is associated with the
                   accessed memory location.
      @param[in] bo A Boolean that specifies whether the offset is a byte offset
                    or not.
      @param[in] mo A specifier of the address modification operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    RV32_AddressModification( const WIR::WIR_BaseRegister &, long, IR_Type *,
                              bool, ModOper = ModOper::ADD );

    /*!
      @brief Constructor for data label-based address modifications.

      @param[in] d A const reference to a %WIR data object to be used as label
                   for the memory access.
      @param[in] t A pointer to the IR data type that is associated with the
                   accessed memory location.
      @param[in] bo A Boolean that specifies whether the offset is a byte offset
                    or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    RV32_AddressModification( const std::string &, const WIR::WIR_Data &, IR_Type *, bool );

    /*!
      @brief Constructor for function label-based address modifications.

      @param[in] f A const reference to a %WIR function to be used as label for
                   the memory access.
      @param[in] t A pointer to the IR data type that is associated with the
                   accessed memory location.
      @param[in] bo A Boolean that specifies whether the offset is a byte offset
                    or not.
    */
    RV32_AddressModification( const WIR::WIR_Function &, IR_Type *, bool );

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
    */
    RV32_AddressModification( RV32_LValue *, long, IR_Type *, bool,
                            ModTime = ModTime::NONE, ModOper = ModOper::ADD );

    /*!
      @brief Copy constructor.

      @param[in] am A const reference to another object to be copied.
    */
    RV32_AddressModification( const RV32_AddressModification & );

    /*!
      @brief Destructor.
    */
    virtual ~RV32_AddressModification( void );

    /*!
      @brief Assignment operator.

      @param[in] am A const reference to another object to be copied.
    */
    RV32_AddressModification & operator = ( const RV32_AddressModification & );

    /*!
      @brief applyModification applies the pointer arithmetics encoded by this
             object and returns the calculated register.

      @param[in] exp A pointer to an IR expression.
      @return A pointer to a WIR register containing the readily computed
              address.
    */
    WIR::RV_RegV *applyModification( IR_Exp * );

    /*!
      @brief applyModificationCost computes the costs for applying the pointer
             arithmetics encoded by this object.

      @return The cost value associated with the involved pointer arithmetics.
    */
    static COST applyModificationCost( void );

    /*!
      @brief createLoad generates code that loads the value from the effective
             address encoded by this object into the given register.

      @param[in] dst A pointer to the %WIR register to be loaded.
      @param[in] exp A pointer to an IR expression.
    */
    void createLoad( WIR::WIR_BaseRegister *, IR_Exp * );

    /*!
      @brief createStore generates code that stores the given register to the
             effective address encoded by this object.

      @param[in] src A pointer to the %WIR register to be stored.
      @param[in] exp A pointer to an IR expression.
      @param[in] bfAccess A Boolean denoting whether a bit-field is written.
      @param[in] bfOffset An unsigned value denoting the bit-field offset.
      @param[in] bfLength An unsigned value denoting the bit-field length.
    */
    void createStore( WIR::WIR_BaseRegister *, IR_Exp *, bool, unsigned char,
                      unsigned char );

    /*!
      @brief createStoreCost computes the costs for a store of a register to the
             effective address encoded by this object.

      @param[in] exp A pointer to an IR expression.
      @return The cost value associated with the involved store operation.
    */
    static COST createStoreCost( const IR_Exp * );

    /*!
      @brief getByteOffset returns the byte offset that will be applied by the
             modification, depending on the actual offset, the size of the base
             IR type, whether a subtraction or addition is performed, or whether
             the modification has already been done or not.

      @return A signed long containing the byte offset.
    */
    long getByteOffset( void ) const;


  protected:

    /*!
      @brief clone creates a copy of an address modification object.

      @return A pointer to the newly created copy of this object.

      clone just calls the corresponding copy constructor.
    */
    virtual AddressModification *clone( void ) const;

    /*!
      @brief getOffsetFactor computes the factor by which the offset needs to be
             scaled according to a RISC-V address modification's base type.

      @return An integer holding the offset's scaling factor.
    */
    virtual int getOffsetFactor( void ) const;

};

}

#endif  // _RV32_ADDRESSMODIFICATION_H
