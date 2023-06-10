/*

   This source file belongs to the

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



//
// Include section
//

// Include private headers
#include "registrar.h"
#include <cassert>

//
// Class section
//

CodeSelector *Registrar::mCodeSelector = nullptr;


/*!
   Register a given code selector instance.
*/
void Registrar::setCodeSelector( CodeSelector *cs )
{
  Registrar::mCodeSelector = cs;
};


/*!
   Retrieve currently registered code selector instance.
*/
CodeSelector *Registrar::getCodeSelector()
{
  return mCodeSelector;
};
