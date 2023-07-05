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
  @file armv5teoperationformats.cc
  @brief This file declares the ARMv5TE-specific operation formats.

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
#include <arch/arm/armv5te.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

// 32-bit ARM operation formats.
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::CAORRS_1 { 32, "CAORRS_1" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::CAORRS_2 { 32, "CAORRS_2" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::CPARAC8_1 { 32, "CPARAC8_1" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::CPARAC8_2 { 32, "CPARAC8_2" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::CPARAR_1 { 32, "CPARAR_1" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::CPARAR_2 { 32, "CPARAR_2" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::CPRAC8_1 { 32, "CPRAC8_1" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::CPRAC8_2 { 32, "CPRAC8_2" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::CPRAR_1 { 32, "CPRAR_1" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::CPRAR_2 { 32, "CPRAR_2" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::RAC12_1 { 32, "RAC12_1" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::RAR_1 { 32, "RAR_1" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::RAR_2 { 32, "RAR_2" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::RARAC60_1 { 32, "RARAC60_1" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::RARC5_1 { 32, "RARC5_1" };
const ARMv5TE::OperationFormat ARMv5TE::OperationFormat::RARC50_1 { 32, "RARC50_1" };

}       // namespace WIR
