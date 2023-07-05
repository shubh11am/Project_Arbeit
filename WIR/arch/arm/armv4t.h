/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2015 - 2022, Heiko Falk.

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file armv4t.h
  @brief This file provides the specific interface of the ARMv4T instruction set
         architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _ARMV4T_H
#define _ARMV4T_H


//
// Include section
//

// Include WIR headers
#include <arch/arm/armbase.h>
#include <arch/arm/armconst6unsigned.h>
#include <arch/arm/armconst7unsigned.h>
#include <arch/arm/armconst9unsigned.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class ARMv4T models the %ARMv4T instruction set architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class ARMv4T : public ARM_Base
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for ARMv4T-based processor architectures.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv4T( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv4T( const ARMv4T & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv4T( ARMv4T && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~ARMv4T( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv4T & operator = ( const ARMv4T & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARMv4T & operator = ( ARMv4T && );


    //
    // ARMv4T-specific global initializations.
    //

    /*!
      @brief init performs some global initialization tasks for ARMv4T-based
             processor architectures.

      This includes setting up the ARMv4T machine operation formats and the
      assignment of valid operation formats to ARMv4T opcodes.

      @note init shall be called globally by WIR_Init(). It shall only perform
            tasks that cannot be expressed as initializations of static class
            members (since the order of static initialization is unspecified in
            C++) and that thus require execution by active code.
    */
    static void init( void );


    //
    // Data structures used to model the ARMv4T architecture.
    //

    /*!
      @brief The public members of class OpCode model the ARMv4T opcodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OpCode : public ARM_Base::OpCode
    {

      public:

        //! Arithmetic Shift Right
        static const OpCode ASR;

        //! Branch and Exchange
        static const OpCode BX;

        //! Load Multiple Increment After
        static const OpCode LDMIA;

        //! Logical Shift Left
        static const OpCode LSL;

        //! Logical Shift Right
        static const OpCode LSR;

        //! Negate
        static const OpCode NEG;

        //! Pop Multiple Registers
        static const OpCode POP;

        //! Push Multiple Registers
        static const OpCode PUSH;

        //! Rotate Right Register
        static const OpCode ROR;

        //! Store Multiple Increment After
        static const OpCode STMIA;


      protected:

        // Inherit the Constructors from ARM_Base::OpCode.
        using ARM_Base::OpCode::OpCode;

    };

    /*!
      @brief The public members of class OperationFormat model the different
             formats of ARMv4T machine operations.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OperationFormat : public ARM_Base::OperationFormat
    {

      public:

        //
        // 32-bit ARM operation formats.
        //

        //! 32-bit cond, reg (use)
        static const OperationFormat CR_3;


        //
        // 16-bit THUMB operation formats.
        //

        //! 16-bit unsigned const8
        static const OperationFormat TC8_1;

        //! 16-bit unsigned const9
        static const OperationFormat TC9_1;

        //! 16-bit cond, label
        static const OperationFormat TCL;

        //! 16-bit label
        static const OperationFormat TL_1;

        //! 32-bit label
        static const OperationFormat TL_2;

        //! 16-bit reg (use)
        static const OperationFormat TR_1;

        //! 16-bit { loreg (def) }
        static const OperationFormat TR1_1;

        //! 16-bit { loreg (def), PC (def) }
        static const OperationFormat TR1PC;

        //! 16-bit { loreg (use) }
        static const OperationFormat TR1_2;

        //! 16-bit { loreg (use), LR (use) }
        static const OperationFormat TR1LR;

        //! 16-bit { loreg (def), loreg (def) }
        static const OperationFormat TR2_1;

        //! 16-bit { loreg (def), loreg (def), PC (def) }
        static const OperationFormat TR2PC;

        //! 16-bit { loreg (use), loreg (use) }
        static const OperationFormat TR2_2;

        //! 16-bit { loreg (use), loreg (use), LR (use) }
        static const OperationFormat TR2LR;

        //! 16-bit { loreg (def), loreg (def), loreg (def) }
        static const OperationFormat TR3_1;

        //! 16-bit { loreg (def), loreg (def), loreg (def), PC (def) }
        static const OperationFormat TR3PC;

        //! 16-bit { loreg (use), loreg (use), loreg (use) }
        static const OperationFormat TR3_2;

        //! 16-bit { loreg (use), loreg (use), loreg (use), LR (use) }
        static const OperationFormat TR3LR;

        //! 16-bit { loreg (def), loreg (def), loreg (def), loreg (def) }
        static const OperationFormat TR4_1;

        //! 16-bit { loreg (def), loreg (def), loreg (def), loreg (def), PC (def) }
        static const OperationFormat TR4PC;

        //! 16-bit { loreg (use), loreg (use), loreg (use), loreg (use) }
        static const OperationFormat TR4_2;

        //! 16-bit { loreg (use), loreg (use), loreg (use), loreg (use), LR (use) }
        static const OperationFormat TR4LR;

        //! 16-bit { loreg (def), loreg (def), loreg (def), loreg (def), loreg (def) }
        static const OperationFormat TR5_1;

        //! 16-bit { loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), PC (def) }
        static const OperationFormat TR5PC;

        //! 16-bit { loreg (use), loreg (use), loreg (use), loreg (use), loreg (use) }
        static const OperationFormat TR5_2;

        //! 16-bit { loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), LR (use) }
        static const OperationFormat TR5LR;

        //! 16-bit { loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def) }
        static const OperationFormat TR6_1;

        //! 16-bit { loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), PC (def) }
        static const OperationFormat TR6PC;

        //! 16-bit { loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use) }
        static const OperationFormat TR6_2;

        //! 16-bit { loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), LR (use) }
        static const OperationFormat TR6LR;

        //! 16-bit { loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def) }
        static const OperationFormat TR7_1;

        //! 16-bit { loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), PC (def) }
        static const OperationFormat TR7PC;

        //! 16-bit { loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use) }
        static const OperationFormat TR7_2;

        //! 16-bit { loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), LR (use) }
        static const OperationFormat TR7LR;

        //! 16-bit { loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def) }
        static const OperationFormat TR8_1;

        //! 16-bit { loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), PC (def) }
        static const OperationFormat TR8PC;

        //! 16-bit { loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use) }
        static const OperationFormat TR8_2;

        //! 16-bit { loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), LR (use) }
        static const OperationFormat TR8LR;

        //! 16-bit loreg (def), usigned const8
        static const OperationFormat TRC8_1;

        //! 16-bit loreg (use), usigned const8
        static const OperationFormat TRC8_2;

        //! 16-bit loreg (defuse), usigned const8
        static const OperationFormat TRC8_3;

        //! 16-bit loreg (def), PC (use), unsigned const10
        static const OperationFormat TRPCC10_1;

        //! 16-bit loreg (def), [PC (use), unsigned const10]
        static const OperationFormat TRPCC10_2;

        //! 16-bit loreg (def), loreg (use)
        static const OperationFormat TRR_1;

        //! 16-bit reg (def), reg (use)
        static const OperationFormat TRR_2;

        //! 16-bit loreg (use), loreg (use)
        static const OperationFormat TRR_3;

        //! 16-bit reg (use), reg (use)
        static const OperationFormat TRR_4;

        //! 16-bit loreg (defuse), loreg (use)
        static const OperationFormat TRR_5;

        //! 16-bit reg (defuse), reg (use)
        static const OperationFormat TRR_6;

        //! 16-bit loreg (defuse)!, { loreg (def) }
        static const OperationFormat TRR1_1;

        //! 16-bit loreg (defuse)!, { loreg (use) }
        static const OperationFormat TRR1_2;

        //! 16-bit loreg (defuse)!, { loreg (def), loreg (def) }
        static const OperationFormat TRR2_1;

        //! 16-bit loreg (defuse)!, { loreg (use), loreg (use) }
        static const OperationFormat TRR2_2;

        //! 16-bit loreg (defuse)!, { loreg (def), loreg (def), loreg (def) }
        static const OperationFormat TRR3_1;

        //! 16-bit loreg (defuse)!, { loreg (use), loreg (use), loreg (use) }
        static const OperationFormat TRR3_2;

        //! 16-bit loreg (defuse)!, { loreg (def), loreg (def), loreg (def), loreg (def) }
        static const OperationFormat TRR4_1;

        //! 16-bit loreg (defuse)!, { loreg (use), loreg (use), loreg (use), loreg (use) }
        static const OperationFormat TRR4_2;

        //! 16-bit loreg (defuse)!, { loreg (def), loreg (def), loreg (def), loreg (def), loreg (def) }
        static const OperationFormat TRR5_1;

        //! 16-bit loreg (defuse)!, { loreg (use), loreg (use), loreg (use), loreg (use), loreg (use) }
        static const OperationFormat TRR5_2;

        //! 16-bit loreg (defuse)!, { loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def) }
        static const OperationFormat TRR6_1;

        //! 16-bit loreg (defuse)!, { loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use) }
        static const OperationFormat TRR6_2;

        //! 16-bit loreg (defuse)!, { loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def) }
        static const OperationFormat TRR7_1;

        //! 16-bit loreg (defuse)!, { loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use) }
        static const OperationFormat TRR7_2;

        //! 16-bit loreg (defuse)!, { loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def), loreg (def) }
        static const OperationFormat TRR8_1;

        //! 16-bit loreg (defuse)!, { loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use), loreg (use) }
        static const OperationFormat TRR8_2;

        //! 16-bit loreg (def), loreg (use), unsigned const3
        static const OperationFormat TRRC3_1;

        //! 16-bit loreg (def), loreg (use), unsigned const5
        static const OperationFormat TRRC5_1;

        //! 16-bit loreg (def), [loreg (use), unsigned const5]
        static const OperationFormat TRRC5_2;

        //! 16-bit loreg (use), [loreg (use), unsigned const5]
        static const OperationFormat TRRC5_3;

        //! 16-bit loreg (def), loreg (use), unsigned const60
        static const OperationFormat TRRC6_1;

        //! 16-bit loreg (def), [loreg (use), unsigned const6]
        static const OperationFormat TRRC6_2;

        //! 16-bit loreg (use), [loreg (use), unsigned const6]
        static const OperationFormat TRRC6_3;

        //! 16-bit loreg (def), [loreg (use), unsigned const7]
        static const OperationFormat TRRC7_1;

        //! 16-bit loreg (use), [loreg (use), unsigned const7]
        static const OperationFormat TRRC7_2;

        //! 16-bit loreg (def), loreg (use), loreg (use)
        static const OperationFormat TRRR_1;

        //! 16-bit loreg (def), [loreg (use), loreg (use)]
        static const OperationFormat TRRR_2;

        //! 16-bit loreg (use), [loreg (use), loreg (use)]
        static const OperationFormat TRRR_3;

        //! 16-bit loreg (def), SP (use), unsigned const10
        static const OperationFormat TRSPC10_1;

        //! 16-bit loreg (def), [SP (use), unsigned const10]
        static const OperationFormat TRSPC10_2;

        //! 16-bit loreg (use), [SP (use), unsigned const10]
        static const OperationFormat TRSPC10_3;


      protected:

        // Inherit the constructors from ARM_Base::OperationFormat.
        using ARM_Base::OperationFormat::OperationFormat;

    };


  private:

    /*!
      @brief clone creates a copy of an ARMv4T processor.

      @return A pointer to the newly created ARMv4T copy.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseProcessor *clone( void ) const;

};

}       // namespace WIR

#endif  // _ARMV4T_H
