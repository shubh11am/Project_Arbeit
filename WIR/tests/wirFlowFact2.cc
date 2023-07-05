/*

   This source file belongs to the

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


//
// Include section
//

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


/*
  This file tests class WIR_Loopbound, it should fail an assertion.
*/

int main( void )
{
  WIR_Init();

  // An unintialized loop bound is not bound to a loop. Thus, getLoop would
  // dereference a nullptr and will fail an assertion.
  WIR_LoopBound unbound;

  unbound.getLoop();
};
