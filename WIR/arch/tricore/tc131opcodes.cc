/*

   This source file belongs to the

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
  @file tc131opcodes.cc
  @brief This file declares the Infineon TriCore V1.3.1's opcodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>


//
// Preprocessor macros
//

#define MA true                   // Memory Access?
#define noMA false
#define ST true                   // Memory Store?
#define noST false
#define LD true                   // Memory Load?
#define noLD false
#define MV true                   // Register-Register Move?
#define noMV false
#define FC true                   // Function Call?
#define noFC false
#define IFC true                  // Indirect Function Call?
#define noIFC false
#define RT true                   // Function Return?
#define noRT false
#define CJ true                   // Conditional Jump?
#define noCJ false
#define UJ true                   // Unconditional Jump?
#define noUJ false
#define IJ true                   // Indirect Jump?
#define noIJ false
#define AD true                   // Assembly Data Directive?
#define noAD false
#define SE true                   // Has Side Effects?
#define noSE false


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

const TC131::OpCode TC131::OpCode::CACHEI_W   { string { "cachei.w" },
                                                MA, noST, noLD, noMV, noFC,
                                                noIFC, noRT, noCJ, noUJ, noIJ,
                                                noAD, SE };
const TC131::OpCode TC131::OpCode::CACHEI_WI  { string { "cachei.wi" },
                                                MA, noST, noLD, noMV, noFC,
                                                noIFC, noRT, noCJ, noUJ, noIJ,
                                                noAD, SE };
const TC131::OpCode TC131::OpCode::FTOIZ      { string { "ftoiz" } };
const TC131::OpCode TC131::OpCode::FTOQ31Z    { string { "ftoq31z" } };
const TC131::OpCode TC131::OpCode::FTOUZ      { string { "ftouz" } };

}       // namespace WIR
