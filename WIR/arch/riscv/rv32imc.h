/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

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
  @file rv32imc.h
  @brief This file provides the specific interface of the RISC-V RV32IMC Base
         Integer instruction set plus the M Standard Extension for Integer
         Multiplication and Division and the C Standard Extension for
         Compressed Instructions, version 2.0.

  @author Simon Kopischke <Simon.Kopischke@tuhh.de>
*/


#ifndef _RV32IMC_H
#define _RV32IMC_H


//
// Include section
//

// Include WIR headers
#include <arch/riscv/rv32ic.h>
#include <arch/riscv/rv32im.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class RV32IMC models the RISC-V RC32IMC Base Integer instruction set
         plus the M Standard Extension for Integer Multiplication and Division
         and the C Standard Extension for Compressed Extension for Compressed
         Instructions, version 2.0.

  @author Simon Kopischke <Simon.Kopischke@tuhh.de>
*/
class RV32IMC : public RV32IC,
                public RV32IM
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for RV32IMC processor architectures.
      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    RV32IMC( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    RV32IMC( const RV32IMC & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    RV32IMC( RV32IMC && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~RV32IMC( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    RV32IMC & operator = ( const RV32IMC & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    RV32IMC & operator = ( RV32IMC && );


    //
    // RV32IMC-specific global initializations.
    //

    /*!
      @brief init performs some global initialization tasks for RV32IMC
             processor architectures.

      This includes setting up the assignment of valid operation formats to
      RV32IMC opcodes.

      @note init shall be called globally by WIR_Init(). It shall only perform
            tasks that cannot be expressed as initializations of static class
            members (since the order of static initialization is unspecified in
            C++) and that thus require execution by active code.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
      */
    static void init( void );


    //
    // Data structures used to model the RISC-V RV32IMC V2.0 architecture,
    // inherited from the base classes RV32IM and RV32IC.
    //

    /*!
      @brief The public members of class OpCode model the RV32IMC's opcodes.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    class OpCode : public RV32IC::OpCode,
                   public RV32IM::OpCode
    {

      protected:

        // Inherit the Constructors from RV32IC::OpCode and RV32IM::OpCode.
        using RV32IC::OpCode::OpCode;
        using RV32IM::OpCode::OpCode;

    };

    /*!
      @brief The public members of class OperationFormant model the RV32IMC's
             different formats of machine operations.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    class OperationFormat : public RV32IC::OperationFormat
    {
      protected:

        // Inherit the Constructors from RV32IC::OperationFormat.
        using RV32IC::OperationFormat::OperationFormat;

    };


  private:

    /*!
      @brief clone creates a copy of an RV32IMC processor.

      @return A pointer to the newly created RV32IMC copy.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    virtual WIR_BaseProcessor *clone( void ) const;

};

}       // namespace WIR

#endif  // _RV32IMC_H
