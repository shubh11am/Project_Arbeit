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
  @file mipsoperationformats.cc
  @brief This file declares the MIPS operation formats.

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
#include <arch/generic/mips.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

// Operation formats
const MIPS::OperationFormat MIPS::OperationFormat::L { 32, "L" };
const MIPS::OperationFormat MIPS::OperationFormat::R_1 { 32, "R_1" };
const MIPS::OperationFormat MIPS::OperationFormat::R_2 { 32, "R_2" };
const MIPS::OperationFormat MIPS::OperationFormat::RIR_1 { 32, "RIR_1" };
const MIPS::OperationFormat MIPS::OperationFormat::RIR_2 { 32, "RIR_2" };
const MIPS::OperationFormat MIPS::OperationFormat::RIU { 32, "RIU" };
const MIPS::OperationFormat MIPS::OperationFormat::RIUR { 32, "RIUR" };
const MIPS::OperationFormat MIPS::OperationFormat::RR_1 { 32, "RR_1" };
const MIPS::OperationFormat MIPS::OperationFormat::RR_2 { 32, "RR_2" };
const MIPS::OperationFormat MIPS::OperationFormat::RRI { 32, "RRI" };
const MIPS::OperationFormat MIPS::OperationFormat::RRIU { 32, "RRIU" };
const MIPS::OperationFormat MIPS::OperationFormat::RRL { 32, "RRL" };
const MIPS::OperationFormat MIPS::OperationFormat::RRR { 32, "RRR" };
const MIPS::OperationFormat MIPS::OperationFormat::RRS { 32, "RRS" };

}       // namespace WIR
