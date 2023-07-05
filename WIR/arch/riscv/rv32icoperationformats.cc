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

/*!
  @file rv32icoperationformats.cc
  @brief This file declares the RISC-V RV32IC's operation formats.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32ic.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

const RV32IC::OperationFormat RV32IC::OperationFormat::SL_1 { 16, "SL_1" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SNULL_1 { 16, "SNULL_1" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SR_1 { 16, "SR_1" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SRC5_1 { 16, "SRC5_1" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SRC5R_1 { 16, "SRC5R_1" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SRC5R_2 { 16, "SRC5R_2" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SRC6_1 { 16, "SRC6_1" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SRC6_2 { 16, "SRC6_2" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SRC6_3 { 16, "SRC6_3" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SRC6_4 { 16, "SRC6_4" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SRC6R_1 { 16, "SRC6R_1" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SRC6R_2 { 16, "SRC6R_2" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SRL_1 { 16, "SRL_1" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SRR_1 { 16, "SRR_1" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SRR_2 { 16, "SRR_2" };
const RV32IC::OperationFormat RV32IC::OperationFormat::SRRC8_1 { 16, "SRRC8_1" };

}       // namespace WIR
