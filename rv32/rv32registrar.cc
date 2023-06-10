/*

   This source file belongs to the

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
  @file rv32registrar.cc
  @brief This file implements the RISC-V RV32 code selector registration class.
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include local headers
#include <rv32/rv32registrar.h>


//
// Code section
//

using namespace std;


namespace RV32 {

//
// Global variable declaration section
//

CodeSelector *RV32_Registrar::mCodeSelector = nullptr;


//
// Public class methods
//

/*
  setCodeSelector registers a code selector instance for current use.
*/
void RV32_Registrar::setCodeSelector( CodeSelector &cs )
{
  DSTART( "static void RV32_Registrar::setCodeSelector(CodeSelector&)" );

  RV32_Registrar::mCodeSelector = &cs;
};


/*
  isCodeSelectorRegistered returns whether a code selector instance currently is
  registered or not.
*/
bool RV32_Registrar::isCodeSelectorRegistered( void )
{
  DSTART( "static bool RV32_Registrar::isCodeSelectorRegistered()" );

  return( RV32_Registrar::mCodeSelector != nullptr );
};


/*
  getCodeSelector returns the currently registered code selector instance.
*/
CodeSelector &RV32_Registrar::getCodeSelector( void )
{
  DSTART( "static CodeSelector& RV32_Registrar::getCodeSelector()" );

  ufAssert( mCodeSelector != nullptr );

  return( *mCodeSelector );
};

}
