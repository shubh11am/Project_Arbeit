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


#ifndef _ARM_LVALUE_H
#define _ARM_LVALUE_H


//
// Include section
//

// Include standard headers
#include <string>

// Include local headers
#include <codesel/lvalue.h>
#include <arm7/cs_arm7.h>


//
// Class forward declarations
//

class IR_Exp;

class LLIR_Register;

class ARM_AddressModification;


//
// Header section
//

/*!
  @brief ARM_LValue is returned by all ARM rules that generate code for an
         lvalue whose value is stored in memory.

  In these cases, the rules that perform the memory access not only return the
  result register, but they also provide all the information that is needed to
  write the result of a computation back into the lvalue.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class ARM_LValue : public LValue
{

  public:

    // TODO Take care of the label-based addressing.

    /*!
      @brief Constructor initializing an empty lvalue.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_LValue( void );

    /*!
      @brief Constructor initializing a base/offset-based lvalue with base
             address pre/post incr/decr.

      @param[in] reg A pointer to the LLIR register that shall hold the result
                     of the memory access, or nullptr.
      @param[in] am A const reference to an address modifier to be applied for a
                    memory access.
      @param[in] dr A Boolean defaulting to false that is used to perform
                    dry-runs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_LValue( LLIR_Register *, const ARM_AddressModification &,
                bool = false );

    /*!
      @brief Constructor initializing a bitfield-based lvalue.

      @param[in] reg A pointer to the LLIR register that shall hold the result
                     of the memory access, or nullptr.
      @param[in] am A const reference to an address modifier to be applied for a
                    memory access.
      @param[in] bo An unsigned char denoting the offset of the bitfield in
                    bits.
      @param[in] bl An unsigned char denoting the length of the bitfield in
                    bits.
      @param[in] dr A Boolean defaulting to false that is used to perform
                    dry-runs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_LValue( LLIR_Register *, const ARM_AddressModification &,
                unsigned char, unsigned char, bool = false );

    /*!
      @brief Copy constructor.

      @param[in] am A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_LValue( const ARM_LValue & );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~ARM_LValue( void );

    /*!
      @brief Assignment operator.

      @param[in] am A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_LValue & operator = ( const ARM_LValue & );

    /*!
      @brief getAddress returns the register that holds the base of the memory
             access plus any modification that should be applied to it.

      @return A reference to field mAddress;

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_AddressModification &getAddress( void ) const;

    /*!
      @brief storeBack stores a register value back into memory.

      @param[in] source A pointer to an LLIR register to be stored back.
      @param[in] exp A pointer to an IR expression.
    */
    void storeBack( LLIR_Register *, IR_Exp * );

    /*!
      @brief storeBackCost returns the costs for storing back into memory.

      @return The cost value associated with a memory store.
    */
    COST storeBackCost( void );

    /*!
      @brief storeBackWorstCost computes the worst-case costs for storing back
             into memory.

      @return The worst-case cost value associated with a memory store.

      Note that this function assumes worst-case calculations are used along the
      entire tree *and* that the tree pattern matcher assumes that a value is
      always loaded from memory first and optionally stored back later (i.e.,
      loadResult = true).
    */
    static COST storeBackWorstCost( void );

    /*!
      @brief calculateAddress applies the pointer arithmetics denoted by the
             embedded ARM_AddressModification object and returns the calculated
             register.

      @param[in] exp A pointer to an IR expression.
      @return A pointer to an LLIR register containing the readily computed
              address.

      calculateAddress requires that createLoad/createStore wasn't applied to
      the ARM_AddressModification object yet.
    */
    LLIR_Register *calculateAddress( IR_Exp * );

    /*!
      @brief calculateAddressCost returns the costs for the pointer arithmetics
             encoded by this object.

      @return The cost value associated with the involved pointer arithmetics.
    */
    COST calculateAddressCost( void ) const;

    /*!
      @brief calculateAddressWorstCost computes the worst-case costs for the
             pointer arithmetics encoded by this object.

      @return The worst-case cost value associated with the involved pointer
              arithmetics.

      Note that this function assumes worst-case calculations are used along the
      entire tree *and* that the tree pattern matcher assumes that a value is
      always loaded from memory first and optionally stored back later (i.e.,
      loadResult = true).
    */
    static COST calculateAddressWorstCost( void );

    /*!
      @brief getBitsInNextWord is a convenience method that returns
             "getBitsInNextWord( offset, length )" for the current object's
             values.

      @return The number of bits overlapping into the next word.

      If the current object does not denote a bitfield access, then this method
      returns -1.
    */
    int getBitsInNextWord( void ) const;

    /*!
      @brief For the given bitfield offset and length, getBitsInNextWord returns
             the number of bits which span into the next word.

      @param[in] bo The bitfield offset.
      @param[in] bl The bitfield length.
      @return The number of bits overlapping into the next word.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static int getBitsInNextWord( int, int );

    /*!
      @brief signExtendBitfieldWorstCostcomputes the worst-case costs for the
             sign-extension of a bitfield value.

      @return The worst-case cost value associated with 32bit sign-extension of
              a bitfield value.
    */
    static COST signExtendBitfieldWorstCost( void );

    /*!
      @brief For a bitfield access, signExtendBitfield sign-extends the given
             value to 32 bits.

      @param[in] value A pointer to an LLIR register holding the value to be
                       sign-extended.
      @return A pointer to a new LLIR register holding the sign-extended value.
    */
    LLIR_Register *signExtendBitfield( LLIR_Register * ) const;


  protected:

    /*!
      @brief clone creates a copy of an lvalue.

      @return A pointer to the newly created copy of this object.

      clone just calls the corresponding copy constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual LValue *clone( void ) const;


    //
    // Attributes.
    //

    /*!
      @brief mDryRun denotes whether this instance was created as part of a dry
             run.

      In that case, no instructions may be created by this ARM_LValue. It shall
      serve only as a cost-calculator.
    */
    bool mDryRun;

};

#endif  // _ARM_LVALUE_H
