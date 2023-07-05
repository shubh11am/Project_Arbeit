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
  @file rv32im.h
  @brief This file provides the specific interface of the RISC-V RV32IM Base
         Integer instruction set plus the M Standard Extension for Integer
         Multiplication and Division, version 2.0.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/


#ifndef _RV32IM_H
#define _RV32IM_H


//
// Include section
//

// Include WIR headers
#include <arch/riscv/rv32i.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class RV32IM models the RISC-V RV32IM Base Integer instruction set plus
         the M Standard Extension for Integer Multiplication and Division,
         version 2.0.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/
class RV32IM : virtual public RV32I
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for RV32IM processor architectures.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32IM( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32IM( const RV32IM & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32IM( RV32IM && );

    /*!
      @brief Destructor.
      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    virtual ~RV32IM( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32IM & operator = ( const RV32IM & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    RV32IM & operator = ( RV32IM && );


    //
    // RV32IM-specific global initializations.
    //

    /*!
      @brief init performs some global initialization tasks for RV32IM processor
             architectures.

      This includes setting up the assignment of valid operation formats to
      RV32IM opcodes.

      @note init shall be called globally by WIR_Init(). It shall only perform
            tasks that cannot be expressed as initializations of static class
            members (since the order of static initialization is unspecified in
            C++) and that thus require execution by active code.
    */
    static void init( void );


    //
    // Data structures used to model the RISC-V RV32IM V2.0 architecture.
    //

    /*!
      @brief The public members of class OpCode model the RV32IM's opcodes.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    class OpCode : public RV32I::OpCode
    {

      public:

        //! Signed Division
        static const OpCode DIV;

        //! Unsigned Division
        static const OpCode DIVU;

        //! Multiplication (lower bits)
        static const OpCode MUL;

        //! Multiplication (upper bits, signed x signed)
        static const OpCode MULH;

        //! Multiplication (upper bits, signed x unsigned)
        static const OpCode MULHSU;

        //! Multiplication (upper bits, unsigned x unsigned)
        static const OpCode MULHU;

        //! Signed Remainder
        static const OpCode REM;

        //! Unsigned Remainder
        static const OpCode REMU;


      protected:

        // Inherit the Constructors from RV32I::OpCode.
        using RV32I::OpCode::OpCode;

    };


  private:

    /*!
      @brief clone creates a copy of an RV32IM processor.

      @return A pointer to the newly created RV32IM copy.

      @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
    */
    virtual WIR_BaseProcessor *clone( void ) const;

};

}       // namespace WIR

#endif  // _RV32IM_H
