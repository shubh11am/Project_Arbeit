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
  @file mipsio.cc
  @brief This file implements MIPS/SPIM-specific stream I/O routines for the
         %WIR library.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <iostream>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/generic/mips.h>


//
// Code section
//

namespace WIR {


using namespace std;


/*
  mips is an I/O manipulator that provides a MIPS assembler dump of WIR.
*/
std::ostream &mips( std::ostream &os )
{
  DSTART( "ostream& mips(ostream&)" );

  os.iword( WIR_ProcessorIO() ) = MIPS::getProcessorTypeID();
  return( os );
};


/*
  dumpMIPSOperation dumps a %WIR operation to an output stream in a
  MIPS/SPIM-specific fashion.
*/
void dumpMIPSOperation( std::ostream &os, const WIR_Operation &o )
{
  DSTART( "void dumpMIPSOperation(ostream&, const WIR_Operation&)" );

  // Indent output.
  os << string( os.iword( WIR_Indentation() ), ' ' );

  // Output the operation's opcode.
  os << o.getOpCode().getName();

  // Output parameters.
  auto params = o.getParameters();
  bool firstParam = true;
  unsigned int expParamCount = 0;
  WIR_BaseProcessor::OperationFormat f = o.getOperationFormat();

  for ( auto it = params.begin(); it != params.end(); ++it ) {
    auto &p = (*it).get();
    bool emitParam = p.isExplicit();

    if ( emitParam ) {
      ++expParamCount;

      // Output separator between parameters.
      if ( firstParam ) {
        os << string( 8 - o.getOpCode().getName().size(), ' ' );
        firstParam = false;
      } else
        os << ", ";

      // Catch special operation formats here.
      if ( ( expParamCount == 2 ) &&
           ( ( f == MIPS::OperationFormat::RIR_1 ) ||
             ( f == MIPS::OperationFormat::RIR_2 ) ||
             ( f == MIPS::OperationFormat::RIUR ) ) ) {
        // Base + Offset addressing.
        os << dynamic_cast<WIR_BaseImmediateParameter &>( (*it).get() );
        do {
          ++it;
        } while ( (*it).get().isImplicit() );
        os << "(" << dynamic_cast<WIR_RegisterParameter &>( (*it).get() )
           << ")";
      } else {

        // Output current parameter itself.
        switch ( p.getType() ) {

          default: {
            // For immediate, register and label parameters, no special handling
            // is required.
            os << p;

            break;
          }
        }
      }
    }
  }
};

}       // namespace WIR
