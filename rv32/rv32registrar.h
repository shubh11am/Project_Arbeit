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
  @file rv32registrar.h
  @brief This file provides the interface of the RISC-V RV32 code selector
         registration class.
*/


#ifndef _RV32_REGISTRAR_H
#define _RV32_REGISTRAR_H


//
// Class forward declarations
//

class CodeSelector;

namespace RV32 {
class RV32_CodeSelector;
}


//
// Preprocessor macros
//

//! Preprocessor macro RVCODESEL can be used to access the RISC-V code selector.
#define RVCODESEL dynamic_cast<RV32::RV32_CodeSelector &>( \
 RV32::RV32_Registrar::getCodeSelector() )

/*!
  @brief Preprocessor macro RVINSTRUCTIONS can be used to access the RISC-V
         instruction factory.
*/
#define RVINSTRUCTIONS RVCODESEL.getInstructionFactory()

/*!
  @brief Preprocessor macro RVIR_CONFIGURATION can be used to access the RISC-V
         code selector's IR configuration.
*/
#define RVIR_CONFIGURATION ( &RVCODESEL.getTask().getIR()->getConfig() )


//
// Header section
//

namespace RV32 {

/*!
  @brief Class RV32_Registrar serves for the central registration of RISC-V code
         selector objects for WCC's FSM.

  This class is purely static so that constructors and destructors are deleted.
*/
class RV32_Registrar
{

  public:

    /*!
      @brief setCodeSelector registers a code selector instance for current use.

      @param[in] cs A reference to a code selector to be used for code
                    generation.
    */
    static void setCodeSelector( CodeSelector & );

    /*!
      @brief isCodeSelectorRegistered returns whether a code selector instance
             currently is registered or not.

      @return true iff a code selector instance is registered, false otherwise.
    */
    static bool isCodeSelectorRegistered( void );

    /*!
      @brief getCodeSelector returns the currently registered code selector
             instance.

      @return A reference to the currently registered code selector.
    */
    static CodeSelector &getCodeSelector( void );


  private:

    /*!
      @brief Class RV32_Registrar is purely static so that its default
             constructor is deleted.
    */
    RV32_Registrar( void ) = delete;

    /*!
      @brief Class RV32_Registrar is purely static so that its destructor is
             deleted.
    */
    ~RV32_Registrar( void ) = delete;

    //! mCodeSelector holds the currently registered code selector instance.
    static CodeSelector *mCodeSelector;

};

}

#endif  // _RV32_REGISTRAR_H
