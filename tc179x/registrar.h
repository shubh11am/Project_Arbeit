/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2007 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _REGISTRAR_TC179X_H
#define _REGISTRAR_TC179X_H


//
// Include section
//

// Include local headers
#include <codesel/codesel.h>
#include <tc179x/cs_tc179x.h>


//
// Preprocessor macros
//

//! Convenience define to shorten references to Code Selector in the code
#define TCCODESEL dynamic_cast<TC179x_CodeSelector *>( Registrar::getCodeSelector() )

//! Convenience define to shorten references to Code Selector in the code
#define TCINSTRUCTIONS TCCODESEL->getInstructionFactory()

//! Convenience define to shorten references to the IR configuration in the code
#define TCIR_CONFIGURATION ( &TCCODESEL->getTask().getIR()->getConfig() )


//
// Header section
//

//! The class Registrar serves for central registration of code selector objects.
class Registrar
{

  public:

    //! This class is purely static. Do not use the constructor or destructor!
    Registrar() = delete;
    ~Registrar() = delete;

    //! Register a given code selector instance.
    static void setCodeSelector( CodeSelector * );

    //! Retrieve currently registered code selector instance.
    static CodeSelector *getCodeSelector();


  private:

    //! mCodeSelector holds the currently registered code selector instance.
    static CodeSelector *mCodeSelector;

};

#endif  // _REGISTRAR_TC179X_H
