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
  @file armv5toperationformats.cc
  @brief This file declares the ARMv5T-specific operation formats.

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
#include <arch/arm/armv5t.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

// 32-bit ARM operation formats.
const ARMv5T::OperationFormat ARMv5T::OperationFormat::AORSSO_2 { 32, "AORSSO_2" };
const ARMv5T::OperationFormat ARMv5T::OperationFormat::AOSSSO { 32, "AOSSSO" };
const ARMv5T::OperationFormat ARMv5T::OperationFormat::ASARAC8_1 { 32, "ASARAC8_1" };
const ARMv5T::OperationFormat ARMv5T::OperationFormat::ASARAC8_2 { 32, "ASARAC8_2" };
const ARMv5T::OperationFormat ARMv5T::OperationFormat::ASRAC8_1 { 32, "ASRAC8_1" };
const ARMv5T::OperationFormat ARMv5T::OperationFormat::ASRAC8_2 { 32, "ASRAC8_2" };
const ARMv5T::OperationFormat ARMv5T::OperationFormat::ASRC8_1 { 32, "ASRC8_1" };
const ARMv5T::OperationFormat ARMv5T::OperationFormat::ASRC8_2 { 32, "ASRC8_2" };
const ARMv5T::OperationFormat ARMv5T::OperationFormat::C16_1 { 32, "C16_1" };
const ARMv5T::OperationFormat ARMv5T::OperationFormat::L { 32, "L" };

}       // namespace WIR
