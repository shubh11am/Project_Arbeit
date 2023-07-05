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
  @file rv32ioperationformats.cc
  @brief This file declares the RISC-V RV32I's operation formats.

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
#include <arch/riscv/rv32i.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

const RV32I::OperationFormat RV32I::OperationFormat::L_1 { 32, "L_1" };
const RV32I::OperationFormat RV32I::OperationFormat::NULL_1 { 32, "NULL_1" };
const RV32I::OperationFormat RV32I::OperationFormat::RC12R_1 { 32, "RC12R_1" };
const RV32I::OperationFormat RV32I::OperationFormat::RC12R_2 { 32, "RC12R_2" };
const RV32I::OperationFormat RV32I::OperationFormat::RC20_1 { 32, "RC20_1" };
const RV32I::OperationFormat RV32I::OperationFormat::RL_1 { 32, "RL_1" };
const RV32I::OperationFormat RV32I::OperationFormat::RL_2 { 32, "RL_2" };
const RV32I::OperationFormat RV32I::OperationFormat::RLR_1 { 32, "RLR_1" };
const RV32I::OperationFormat RV32I::OperationFormat::RLR_2 { 32, "RLR_2" };
const RV32I::OperationFormat RV32I::OperationFormat::RRC5_1 { 32, "RRC5_1" };
const RV32I::OperationFormat RV32I::OperationFormat::RRC12_1 { 32, "RRC12_1" };
const RV32I::OperationFormat RV32I::OperationFormat::RRL_1 { 32, "RRL_1" };
const RV32I::OperationFormat RV32I::OperationFormat::RRL_2 { 32, "RRL_2" };
const RV32I::OperationFormat RV32I::OperationFormat::RR_1 { 32, "RR_1" };
const RV32I::OperationFormat RV32I::OperationFormat::RRR_1 { 32, "RRR_1" };
const RV32I::OperationFormat RV32I::OperationFormat::RSC5_1 { 32, "RSC5_1" };
const RV32I::OperationFormat RV32I::OperationFormat::RSR_1 { 32, "RSR_1" };

}       // namespace WIR
