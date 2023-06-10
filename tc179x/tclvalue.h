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


#ifndef _TC_LVALUE_H
#define _TC_LVALUE_H


//
// Include section
//

// Include standard headers
#include <string>

// Include local headers
#include <codesel/lvalue.h>
#include <tc179x/tcaddresswithoffset.h>


//
// Class forward declarations
//

class IR_Exp;
class IR_Type;

class LLIR_Register;

namespace WIR {
class WIR_Data;
class WIR_Function;
class WIR_VirtualRegister;
}

class TC_AddressModification;


//
// Header section
//

/*!
  @brief TC_LValue is returned by all TriCore rules that generate code for an
         lvalue whose value is stored in memory.

  In these cases, the rules that perform the memory access not only return the
  result register, but they also provide all the information that is needed to
  write the result of a computation back into the lvalue.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_LValue : public LValue
{

  public:

    /*!
      @brief Constructor initializing an empty lvalue.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_LValue( void );

    /*!
      @brief Constructor initializing a data object label-based lvalue.

      @param[in] reg A pointer to the LLIR register that shall hold the result
                     of the memory access, or nullptr.
      @param[in] l A const reference to a string denoting the LLIR label to be
                   used for the memory access.
      @param[in] r A pointer to the %WIR register that shall hold the result of
                   the memory access, or nullptr.
      @param[in] d A const reference to a %WIR data object to be used for the
                   memory access.
      @param[in] t A pointer to the IR type that is stored at the accessed
                   memory location.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_LValue( LLIR_Register *, const std::string &, WIR::WIR_VirtualRegister *,
               const WIR::WIR_Data &, IR_Type * );

    /*!
      @brief Constructor initializing a function label-based lvalue.

      @param[in] reg A pointer to the LLIR register that shall hold the result
                     of the memory access, or nullptr.
      @param[in] l A const reference to a string denoting the LLIR label to be
                   used for the memory access.
      @param[in] r A pointer to the %WIR register that shall hold the result of
                   the memory access, or nullptr.
      @param[in] f A pointer to a %WIR function to be used for the memory
                   access.
      @param[in] t A pointer to the IR type that is stored at the accessed
                   memory location.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_LValue( LLIR_Register *, const std::string &, WIR::WIR_VirtualRegister *,
               const WIR::WIR_Function &, IR_Type * );

    /*!
      @brief Constructor initializing a base/offset-based lvalue with base
             address pre/post incr/decr.

      @param[in] reg A pointer to the LLIR register that shall hold the result
                     of the memory access, or nullptr.
      @param[in] r A pointer to the %WIR register that shall hold the result of
                   the memory access, or nullptr.
      @param[in] am A const reference to an address modifier to be applied for a
                    memory access.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_LValue( LLIR_Register *, WIR::WIR_VirtualRegister *,
               const TC_AddressModification & );

    /*!
      @brief Constructor initializing a bitfield-based lvalue.

      @param[in] reg A pointer to the LLIR register that shall hold the result
                     of the memory access, or nullptr.
      @param[in] r A pointer to the %WIR register that shall hold the result of
                   the memory access, or nullptr.
      @param[in] am A const reference to an address modifier to be applied for a
                    memory access.
      @param[in] bo An unsigned char denoting the offset of the bitfield in
                    bits.
      @param[in] bl An unsigned char denoting the length of the bitfield in
                    bits.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_LValue( LLIR_Register *, WIR::WIR_VirtualRegister *,
               const TC_AddressModification &, unsigned char, unsigned char );

    /*!
      @brief Copy constructor.

      @param[in] am A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_LValue( const TC_LValue & );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_LValue( void );

    /*!
      @brief Assignment operator.

      @param[in] am A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_LValue & operator = ( const TC_LValue & );

    /*!
      @brief getAddress returns the register that holds the base of the memory
             access plus any modification that should be applied to it.

      @return A reference to field mAddress;

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AddressModification &getAddress( void ) const;

    /*!
      @brief storeBack stores a register value back into memory.

      @param[in] exp A pointer defaulting to nullptr to an IR expression.
      @param[in] source A pointer defaulting to nullptr to an LLIR register to
                        be stored back. If nullptr is passed, the store will use
                        this lvalue's result register.
      @param[in] src A pointer defaulting to nullptr to a %WIR register to be
                     stored back. If nullptr is passed, the store will use this
                     lvalue's result register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void storeBack( IR_Exp * = nullptr, LLIR_Register * = nullptr,
                    WIR::WIR_VirtualRegister * = nullptr );

    /*!
      @brief getBaseOffsetForm transforms a memory access into base + offset
             form.

      @param[in] exp A const pointer that points to the IR expression where the
                     memory access is performed.
      @return A base + offset specification.

      If the data location is given in label form, getBaseOffsetForm issues some
      instructions realizing the transformation to base + offset form.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AddressWithOffset getBaseOffsetForm( const IR_Exp * ) const;

    /*!
      @brief convertToBaseOffsetForm uses getBaseOffsetForm to convert a
             label-based lvalue into base + offset form.

      @param[in] exp A const pointer that points to the IR expression where the
                     memory access is performed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void convertToBaseOffsetForm( const IR_Exp * );

    /*!
      @brief For the given bitfield offset and length, getBitsInNextWord returns
             the number of bits which span into the next word.

      @param[in] bo The bitfield offset.
      @param[in] bl The bitfield length.
      @return The number of bits overlapping into the next word.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static int getBitsInNextWord( int, int );


  protected:

    /*!
      @brief clone creates a copy of an lvalue.

      @return A pointer to the newly created copy of this object.

      clone just calls the corresponding copy constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual LValue *clone( void ) const;

};

#endif  // _TC_LVALUE_H
