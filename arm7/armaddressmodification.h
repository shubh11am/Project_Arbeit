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


#ifndef _ARM7_ADDRESSMODIFICATION_H
#define _ARM7_ADDRESSMODIFICATION_H


//
// Include section
//

// Include standard headers
#include <string>

// Include local headers
#include <codesel/addressmodification.h>
#include <arm7/cs_arm7.h>


//
// Class forward declarations
//

class IR_Exp;
class IR_Type;

class ARM_LValue;


//
// Header section
//

/*!
  @brief Class ARM_AddressModification describes an address register
         modification.

  Since the ARMv4 architecture allows for very sophisticated LD/ST instructions
  applying immediate, register or shifted register offsets, in immediate,
  post-indexed or pre-indexed modes, this class stores all the details of such a
  modification so that it can be applied as part of a LD/ST instruction instead
  of additional instructions just exclusively performing address arithmetics.
*/
class ARM_AddressModification : public AddressModification
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Constructor initializing an empty address modification.
    */
    ARM_AddressModification( void );

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
    ARM_AddressModification( LLIR_Register *, LLIR_Register *, IR_Type *,
                             ModTime, ModOper, bool = false, bool = false );

    /*!
      @brief Constructor for case with local base register and integer offset.

      @param[in] reg A pointer to the LLIR base register.
      @param[in] o A long integer denoting the offset of an address
                   modification.
      @param[in] t A pointer to the IR data type that is associated with the
                   accessed memory location.
      @param[in] mt A specifier of the address modification time.
      @param[in] mo A specifier of the address modification operation.
      @param[in] bo A Boolean defaulting to false that specifies whether the
                    offset is a byte offset or not.
      @param[in] dr A Boolean defaulting to false that is used to perform
                    dry-runs.
    */
    ARM_AddressModification( LLIR_Register *, long, IR_Type *, ModTime, ModOper,
                             bool = false, bool = false );

    /*!
      @brief Constructor for case with local base register and no offset.

      @param[in] reg A pointer to the LLIR base register.
      @param[in] t A pointer to the IR data type that is associated with the
                   accessed memory location.
      @param[in] dr A Boolean defaulting to false that is used to perform
                    dry-runs.
    */
    ARM_AddressModification( LLIR_Register *, IR_Type *, bool = false );

    /*!
      @brief Constructor for case with global/stack-based base register and
             register offset.

      @param[in] l A pointer to an lvalue specifying the base register.
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
    ARM_AddressModification( ARM_LValue *, LLIR_Register *, IR_Type *, ModTime,
                             ModOper, bool = false, bool = false );

    /*!
      @brief Constructor for case with global/stack-based base register and
             integer offset.

      @param[in] l A pointer to an lvalue specifying the base register.
      @param[in] o A long integer denoting the offset of an address
                   modification.
      @param[in] t A pointer to the IR data type that is associated with the
                   accessed memory location.
      @param[in] mt A specifier of the address modification time.
      @param[in] mo A specifier of the address modification operation.
      @param[in] bo A Boolean defaulting to false that specifies whether the
                    offset is a byte offset or not.
      @param[in] dr A Boolean defaulting to false that is used to perform
                    dry-runs.
    */
    ARM_AddressModification( ARM_LValue *, long, IR_Type *, ModTime, ModOper,
                             bool = false, bool = false );

    /*!
      @brief Copy constructor.

      @param[in] am A const reference to another object to be copied.
    */
    ARM_AddressModification( const ARM_AddressModification & );

    /*!
      @brief Destructor.
    */
    ~ARM_AddressModification( void );

    /*!
      @brief applyModification applies the pointer arithmetics encoded by this
             object and returns the calculated register.

      @param[in] exp A pointer to an IR expression.
      @return A pointer to an LLIR register containing the readily computed
              address.
    */
    LLIR_Register *applyModification( IR_Exp * );

    /*!
      @brief applyModificationCost computes the costs for the pointer
             arithmetics encoded by this object.

      @return The cost value associated with the involved pointer arithmetics.
    */
    COST applyModificationCost( void ) const;

    /*!
      @brief applyModificationWorstCost computes the worst-case costs for the
             pointer arithmetics encoded by this object.

      @return The worst-case cost value associated with the involved pointer
              arithmetics.

      Note that this function assumes worst-case calculations are used along the
      entire tree *and* that the tree pattern matcher assumes that a value is
      always loaded from memory first and optionally stored back later (i.e.,
      loadResult = true).
    */
    static COST applyModificationWorstCost( void );

    /*!
      @brief createLoad generates code that loads the value from the effective
             address encoded by this object into the given register.

      @param[in] dest A pointer to the LLIR register to be loaded.
      @param[in] exp A pointer to an IR expression.
    */
    void createLoad( LLIR_Register *, IR_Exp * );

    /*!
      @brief createLoadCost computes the costs for a load of the value from the
             effective address encoded by this object into a register.

      @return The cost value associated with the involved load operation.
    */
    COST createLoadCost( void );

    /*!
      @brief createLoadWorstCost computes the worst-case costs for a load of the
             value from the effective address encoded by this object into a
             register.

      @return The worst-case cost value associated with the involved load
              operation.
    */
    static COST createLoadWorstCost( void );

    /*!
      @brief createStore generates code that stores the given register to the
             effective address encoded by this object.

      @param[in] source A pointer to the LLIR register to be stored.
      @param[in] exp A pointer to an IR expression.
    */
    void createStore( LLIR_Register *, IR_Exp * );

    /*!
      @brief createStoreCost computes the costs for a store of a register to the
             effective address encoded by this object.

      @return The cost value associated with the involved store operation.
    */
    COST createStoreCost( void );

    /*!
      @brief createStoreWorstCost computes the worst-case costs for a store of a
             register to the effective address encoded by this object.

      @return The worst-case cost value associated with the involved store
              operation.

      Note that this cost is very small, because it is assumed that worst-case
      estimates are used along the entire tree. In that case, the bulk of the
      cost lies within the load, that is assumed to always take place, even if
      loadResult turns out to be false.
    */
    static COST createStoreWorstCost( void );


  protected:

    /*!
      @brief createLoadStore generates code for either a load or a store of the
             given register from/to the effective address encoded by this
             object.

      @param[in] createStore A Boolean flag denoting whether a store or a load
                             shall be created.
      @param[in] reg A pointer to the LLIR register to be loaded/stored.
      @param[in] exp A pointer to an IR expression.
    */
    void createLoadStore( bool, LLIR_Register *, IR_Exp * );

    /*!
      @brief createLoadStoreCost computes the costs for either a load or a store
             of the given register from/to the effective address encoded by this
             object.

      @param[in] createStore A Boolean flag denoting whether a store or a load
                             shall be created.
      @return The cost value associated with the involved load/store operation.
    */
    COST createLoadStoreCost( bool );

    /*!
      @brief insertLdSt inserts a single LD/ST instruction.

      @param[in] createStore A Boolean flag denoting whether a store or a load
                             shall be created.
      @param[in] mode2 A const reference to a string denoting the use of
                       immediate / post-indexed / pre-indexed addressing with an
                       immediate / register / scaled register offset (e.g.,
                       OPER_IMMOFF).
      @param[in] dest A pointer to the LLIR register to be loaded/stored.
      @param[in] regOffset A pointer to the LLIR offset register, if any.
      @param[in] intOffset An integer storing the offset.
      @param[in] shift_mode A const reference to a string denoting the shift to
                            apply when using the scaled register offset (e.g.,
                            OPER_LSL). For the empty string, no scaling will be
                            performed.
      @param[in] shift An integer denoting the immediate 5-bit shift amount.
      @param[in] modOper A specifier of the modification operation.
      @param[in] exp A pointer to an IR expression.
    */
    void insertLdSt( bool, const std::string &, LLIR_Register *,
                     LLIR_Register *, int, const std::string &, int, ModOper,
                     IR_Exp * );

    /*!
      @brief insertLdStCost computes the costs for a single LD/ST instruction.

      @param[in] createStore A Boolean flag denoting whether a store or a load
                             shall be created.
      @param[in] mode2 A const reference to a string denoting the use of
                       immediate / post-indexed / pre-indexed addressing with an
                       immediate / register / scaled register offset (e.g.,
                       OPER_IMMOFF).
    */
    COST insertLdStCost( bool, const std::string & ) const;

    /*!
      @brief insertMOV_ORR_Cost computes the costs for using insertMOV_ORR as
             helper.

      @param[in] value An integer denoting the value to be bit-masked using
                       MOV_ORR.
      @return The cost value associated with the involved MOV_ORR.
    */
    static COST insertMOV_ORR_Cost( int );

    /*!
      @brief isMode3 determines whether addressing modes 2 or 3 are applicable.

      @return true if addressing modes 2 or 3 are applicable, false otherwise.
    */
    bool isMode3( void ) const;

    /*!
      @brief clone creates a copy of an address modification object.

      @return A pointer to the newly created copy of this object.

      clone just calls the corresponding copy constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual AddressModification *clone( void ) const;

    /*!
      @brief getOffsetFactor computes the factor by which the offset needs to be
             scaled according to an ARM address modification's base type.

      @return An integer holding the offset's scaling factor.
    */
    virtual int getOffsetFactor( void ) const;


    //
    // Attributes.
    //

    /*!
      @brief A Boolean flag denoting whether this AddressModification was
             created during a dry run.

      If this flag is set, attempting to generate instructions with the instance
      will result in a failing assertion. An instance with this flag set shall
      only serve as a cost calculator.
    */
    bool mDryRun;

};

#endif  // _ARM7_ADDRESSMODIFICATION_H
