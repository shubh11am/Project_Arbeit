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
  @file armv4toperationformats.cc
  @brief This file declares the ARMv4T-specific operation formats.

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
#include <arch/arm/armv4t.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

// 32-bit ARM operation formats.
const ARMv4T::OperationFormat ARMv4T::OperationFormat::CR_3 { 32, "CR_3" };

// 16-bit THUMB operation formats.
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TC8_1 { 16, "TC8_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TC9_1 { 16, "TC9_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TCL { 16, "TCL" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TL_1 { 16, "TL_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TL_2 { 32, "TL_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR_1 { 16, "TR_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR1_1 { 16, "TR1_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR1PC { 16, "TR1PC" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR1_2 { 16, "TR1_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR1LR { 16, "TR1LR" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR2_1 { 16, "TR2_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR2PC { 16, "TR2PC" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR2_2 { 16, "TR2_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR2LR { 16, "TR2LR" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR3_1 { 16, "TR3_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR3PC { 16, "TR3PC" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR3_2 { 16, "TR3_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR3LR { 16, "TR3LR" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR4_1 { 16, "TR4_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR4PC { 16, "TR4PC" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR4_2 { 16, "TR4_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR4LR { 16, "TR4LR" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR5_1 { 16, "TR5_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR5PC { 16, "TR5PC" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR5_2 { 16, "TR5_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR5LR { 16, "TR5LR" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR6_1 { 16, "TR6_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR6PC { 16, "TR6PC" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR6_2 { 16, "TR6_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR6LR { 16, "TR6LR" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR7_1 { 16, "TR7_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR7PC { 16, "TR7PC" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR7_2 { 16, "TR7_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR7LR { 16, "TR7LR" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR8_1 { 16, "TR8_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR8PC { 16, "TR8PC" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR8_2 { 16, "TR8_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TR8LR { 16, "TR8LR" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRC8_1 { 16, "TRC8_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRC8_2 { 16, "TRC8_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRC8_3 { 16, "TRC8_3" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRPCC10_1 { 16, "TRPCC10_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRPCC10_2 { 16, "TRPCC10_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR_1 { 16, "TRR_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR_2 { 16, "TRR_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR_3 { 16, "TRR_3" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR_4 { 16, "TRR_4" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR_5 { 16, "TRR_5" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR_6 { 16, "TRR_6" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR1_1 { 16, "TRR1_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR1_2 { 16, "TRR1_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR2_1 { 16, "TRR2_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR2_2 { 16, "TRR2_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR3_1 { 16, "TRR3_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR3_2 { 16, "TRR3_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR4_1 { 16, "TRR4_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR4_2 { 16, "TRR4_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR5_1 { 16, "TRR5_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR5_2 { 16, "TRR5_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR6_1 { 16, "TRR6_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR6_2 { 16, "TRR6_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR7_1 { 16, "TRR7_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR7_2 { 16, "TRR7_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR8_1 { 16, "TRR8_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRR8_2 { 16, "TRR8_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRRC3_1 { 16, "TRRC3_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRRC5_1 { 16, "TRRC5_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRRC5_2 { 16, "TRRC5_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRRC5_3 { 16, "TRRC5_3" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRRC6_1 { 16, "TRRC6_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRRC6_2 { 16, "TRRC6_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRRC6_3 { 16, "TRRC6_3" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRRC7_1 { 16, "TRRC7_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRRC7_2 { 16, "TRRC7_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRRR_1 { 16, "TRRR_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRRR_2 { 16, "TRRR_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRRR_3 { 16, "TRRR_3" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRSPC10_1 { 16, "TRSPC10_1" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRSPC10_2 { 16, "TRSPC10_2" };
const ARMv4T::OperationFormat ARMv4T::OperationFormat::TRSPC10_3 { 16, "TRSPC10_3" };

}       // namespace WIR
