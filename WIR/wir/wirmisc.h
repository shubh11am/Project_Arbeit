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
  @file wirmisc.h
  @brief This file declares miscellaneous global functions that are used
         everywhere within the %WIR library.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_MISC_H
#define _WIR_MISC_H


//
// Include section
//

// Include standard headers
#include <string>


//
// Header section
//

/*!
  @brief The namespace WIR groups all global variables, functions and objects of
         WCC's Intermediate Representation.
  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
namespace WIR {

//
// Class forward declarations
//

class WIR_Parameter;


//
// WIR library initialization.
//

/*!
  @brief WIR_Init performs some global initialization tasks for the entire WIR
         library.

  This includes initialization of the debug macros, the I/O system and ISA-
  specific initializations.

  @note It is mandatory to call WIR_Init at the very beginning in application
        code, before any actions are done using the %WIR library, since static
        global variables must be properly initialized at first.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void WIR_Init( void );


//
// WIR version information.
//

/*!
  @brief WIR_Module returns the name of this WIR class library.

  @return A string containing the library's name.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::string WIR_Module( void );

/*!
  @brief WIR_Version returns the version of this WIR class library.

  @return A string containing the library's version.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::string WIR_Version( void );


//
// WIR I/O stream management.
//

/*!
  @brief WIR_ProcessorIO adds a new index for I/O streams in order to establish
         %WIR-specific I/O manipulators.

  @return A new I/O index value for %WIR processors.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
int WIR_ProcessorIO( void );


//
// Basic math and bit-manipulation functions.
//

/*!
  @brief count1Bits computes how many bits in the binary representation of the
         passed value are set to 1.

  @param[in] i The unsigned integer value whose number of 1-bits shall be
               counted.
  @return The number of 1-bits.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
unsigned int count1Bits( unsigned int i );

/*!
  @brief isPowerOfTwo returns whether the passed value is a power of 2.

  @param[in] i The unsigned integer value that shall be checked for a power of
               2.
  @return true iff the specified value is a power of 2, false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
bool isPowerOfTwo( unsigned int i );

}       // namespace WIR

#endif  // _WIR_MISC_H
