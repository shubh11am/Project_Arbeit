/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2005 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _REGISTRAR_ARM7_H
#define _REGISTRAR_ARM7_H


// Include local headers
#include <codesel/codesel.h>
#include "arm7/cs_arm7.h"


//! Convenience define to shorten references to Code Selector in the code
#define ARMCODESEL dynamic_cast<ARM7_CodeSelector*> (Registrar::getCodeSelector())

//! Convenience define to shorten references to Code Selector in the code
#define ARMINSTRUCTIONS ARMCODESEL->getInstructionFactory()

//! Convenience define to shorten references to the IR configuration in the code
#define ARMIR_CONFIGURATION ( &ARMCODESEL->getTask().getIR()->getConfig() )

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

#endif  // _REGISTRAR_ARM7_H
