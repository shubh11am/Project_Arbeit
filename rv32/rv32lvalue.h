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
  @file rv32lvalue.h
  @brief This file provides the interface of the RISC-V RV32 class describing
         lvalues stored in memory.
*/


#ifndef _RV32_LVALUE_H
#define _RV32_LVALUE_H


//
// Include section
//

// Include local headers
#include <codesel/lvalue.h>
#include <rv32/rv32addresswithoffset.h>

//
// Class forward declarations
//

class IR_Exp;

namespace WIR {
class WIR_VirtualRegister;
class WIR_BaseRegister;
class WIR_Data;
class RV_RegV;
}

class RV32_AddressModification;
class RV32_AddressWithOffset;

//
// Header section
//

namespace RV32 {

/*!
  @brief RV32_LValue is returned by all RISC-V rules that generate code for an
         lvalue whose value is stored in memory.

  In these cases, the rules that perform the memory access not only return the
  result register, but they also provide all the information that is needed to
  write the result of a computation back into the lvalue.
*/
class RV32_LValue : public LValue
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Constructor initializing an empty lvalue.
    */
    RV32_LValue( void );

    /*!
      @brief Constructor initializing a base/offset-based lvalue with base
              address pre/post incr/decr.

      @param[in] r A pointer to the %WIR register that shall hold the result of
                    the memory access, or nullptr.
      @param[in] am A const reference to an address modifier to be applied for a
                    memory access.
    */
    RV32_LValue( WIR::WIR_VirtualRegister *, const RV32_AddressModification & );

    /*!
      @brief Constructor initializing a data object label-based lvalue.

      @param[in] r A pointer to the %WIR register that shall hold the result of
                   the memory access, or nullptr.
      @param[in] d A const reference to a %WIR data object to be used for the
                   memory access.
      @param[in] t A pointer to the IR type that is stored at the accessed
                   memory location.
    */
    RV32_LValue( const std::string &, WIR::WIR_VirtualRegister *, const WIR::WIR_Data &, IR_Type * );

    /*!
      @brief Constructor initializing a function label-based lvalue.

      @param[in] r A pointer to the %WIR register that shall hold the result of
                   the memory access, or nullptr.
      @param[in] f A pointer to a %WIR function to be used for the memory
                   access.
      @param[in] t A pointer to the IR type that is stored at the accessed
                   memory location.
    */
    RV32_LValue( WIR::WIR_VirtualRegister *, const WIR::WIR_Function &,
                 IR_Type * );

    /*!
      @brief Copy constructor.

      @param[in] am A const reference to another object to be copied.
    */
    RV32_LValue( const RV32_LValue & );

    /*!
      @brief Destructor.
    */
    virtual ~RV32_LValue( void );

    /*!
      @brief Assignment operator.

      @param[in] am A const reference to another object to be copied.
    */
    RV32_LValue & operator = ( const RV32_LValue & );


    //
    // Lvalue handling.
    //

    /*!
      @brief getAddress returns the register that holds the base of the memory
             access plus any modification that should be applied to it.

      @return A reference to field mAddress;
    */
    RV32_AddressModification &getAddress( void ) const;

    /*!
      @brief storeBack stores a register value back into memory.

      @param[in] exp A pointer defaulting to nullptr to an IR expression.
      @param[in] src A pointer defaulting to nullptr to a %WIR register to be
                     stored back. If nullptr is passed, the store will use this
                     lvalue's result register.
    */
    void storeBack( IR_Exp * = nullptr, WIR::WIR_VirtualRegister * = nullptr );

    /*!
      @brief getBaseOffsetForm transforms a memory access into base + offset
             form.

      @param[in] exp A const pointer that points to the IR expression where the
                     memory access is performed.
      @return A base + offset specification.

      If the data location is given in label form, getBaseOffsetForm issues some
      instructions realizing the transformation to base + offset form.
    */
    RV32_AddressWithOffset getBaseOffsetForm( const IR_Exp * );

    /*!
      @brief convertToBaseOffsetForm uses getBaseOffsetForm to convert a
             label-based lvalue into base + offset form.

      @param[in] exp A const pointer that points to the IR expression where the
                     memory access is performed.
    */
    void convertToBaseOffsetForm( const IR_Exp * );

    /*!
      @brief For the given bitfield offset and length, getBitsInNextWord returns
             the number of bits which span into the next word.

      @param[in] bo The bitfield offset.
      @param[in] bl The bitfield length.
      @return The number of bits overlapping into the next word.
    */
    static int getBitsInNextWord( int, int );

    /*!
      @brief calculateAddress applies the pointer arithmetics denoted by the
             embedded RV32_AddressModification object and returns the calculated
             register.

      @param[in] exp A pointer to an IR expression.
      @return A pointer to an WIR register containing the readily computed
              address.

      calculateAddress requires that createLoad/createStore wasn't applied to
      the RV32_AddressModification object yet.
    */
    WIR::RV_RegV* calculateAddress( IR_Exp *exp );

    /*!
      @brief calculateAddressCost returns the costs for the pointer arithmetics
             encoded by this object.

      @return The cost value associated with the involved pointer arithmetics.
    */
    COST calculateAddressCost( void ) const;

  protected:

    /*!
      @brief clone creates a copy of an lvalue.

      @return A pointer to the newly created copy of this object.

      clone just calls the corresponding copy constructor.
    */
    virtual LValue *clone( void ) const;

};

}

#endif  // _RV32_LVALUE_H
