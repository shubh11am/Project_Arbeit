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
  @file tcschedulinginfo.cc
  @brief This file implements TriCore-specific helper functions for instruction
         scheduling.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <sstream>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>

// Include local headers
#include "tcschedulinginfo.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  getType determines the TriCore-specific type of a %WIR operation.

  See also
  - TriCore Compiler Writer's Guide, section 2.1.1.1, page 31.
  - TriCore User Guide V1.6.4, DSP Optimization Guide Part 1, 2003.
  - TriCore User's Manual V1.3.8, Volume 2 Instruction Set, 2008, chapter 4,
    pages 507ff.
  - Thomas Pucyk, Local and global instruction scheduling approaches for the
    TriCore processor, diploma thesis, TU Dortmund, 2009, section 2.1.3.
*/
TC_SchedulingInfo::OperationType TC_SchedulingInfo::getType( const WIR_Operation &o )
{
  DSTART(
    "static TC_SchedulingInfo::OperationType TC_SchedulingInfo::getType(const WIR_Operation&)" );

  if ( TC13::isLP( o ) )
    return( OperationType::lp );

  if ( TC13::isLS( o ) )
    return( OperationType::ls );

  if ( !mOperationTypesInitialized )
    initOperationTypes();

  return( mOperationTypes.at( o.getOpCode() ) );
};


/*
  getLatency determines the number of clock cycles it takes from issuing the
  first operation until availability of its results, depending on the second
  operation which is issued immediately after the first one.
*/
long long TC_SchedulingInfo::getLatency( const WIR_Operation &o1,
                                         const WIR_Operation &o2 )
{
  DSTART(
    "static long long int TC_SchedulingInfo::getLatency(const WIR_Operation&, const WIR_Operation&)" );

  auto t1 = getType( o1 );

  if ( !mFloatLatencyInitialized )
    initFloatLatency();

  if ( t1 == TC_SchedulingInfo::OperationType::fp ) {
    DOUT(
      "Latency of '" << o1.getOpCode().getName() << "' is " <<
      mFloatLatency.at( o1.getOpCode() ) << "." << endl );
    return( mFloatLatency.at( o1.getOpCode() ) );
  }

  auto res = mTypeLatency.at( t1 ).at( getType( o2 ) );
  DOUT(
    "Latency of '" << o1.getOpCode().getName() << "' depending on '" <<
    o2.getOpCode().getName() << "' is " << res << "." << endl );
  return( res );
};


/*
  getLatency determines the number of clock cycles it takes from issuing the
  given operation until availability of its results, assuming that the operation
  is the last one within a basic block.
*/
long long TC_SchedulingInfo::getLatency( const WIR_Operation &o )
{
  DSTART(
    "static long long int TC_SchedulingInfo::getLatency(const WIR_Operation&)" );

  auto t = getType( o );

  if ( t == TC_SchedulingInfo::OperationType::fp ) {
    DOUT(
      "Latency of '" << o.getOpCode().getName() << "' is " <<
      mFloatLatency.at( o.getOpCode() ) << "." << endl );
    return( mFloatLatency.at( o.getOpCode() ) );
  }

  auto res = mTypeLatency.at( t ).at( TC_SchedulingInfo::OperationType::ip );
  DOUT(
    "Latency of '" << o.getOpCode().getName() << "' is " << res << "." <<
    endl );
  return( res );
};


/*
  getPriority determines an operation's scheduling priority depending on another
  predecessor operation.
*/
long long TC_SchedulingInfo::getPriority( const WIR_Operation &o,
                                          const WIR_Operation &pred )
{
  DSTART(
    "static long long int TC_SchedulingInfo::getPriority(const WIR_Operation&, const WIR_Operation&)" );

  return( mPairPriority.at( getType( pred ) ).at( getType( o ) ) );
};


/*
  getPriority determines the scheduling priority of a stand-alone operation.

  An operation is considered stand-alone if it has no predecessor in the current
  schedule, i.e., it if is an entry of a scheduling region.
*/
long long TC_SchedulingInfo::getPriority( const WIR_Operation &o )
{
  DSTART(
    "static long long int TC_SchedulingInfo::getPriority(const WIR_Operation&)" );

  return( mSinglePriority.at( getType( o ) ) );
};


//
// Private class methods
//

/*
  initOperationTypes initializes map mOperation Types.

  See the comment for attribute mOperationTypesInitialized for more information.
*/
void TC_SchedulingInfo::initOperationTypes( void )
{
  DSTART( "static void TC_SchedulingInfo::initOperationTypes()" );

  mOperationTypes.clear();

  mOperationTypes = {
    { TC131::OpCode::ABS, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ABS_B, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ABS_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ABSDIF, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ABSDIF_B, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ABSDIF_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ABSDIFS, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ABSDIFS_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ABSS, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ABSS_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ADD, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ADD_B, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ADD_F, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::ADD_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ADDC, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ADDI, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ADDIH, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ADDIH_A, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::ADDS, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ADDS_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ADDS_HU, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ADDS_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ADDSC_A, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::ADDSC_AT, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::ADDX, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::AND, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::AND_AND_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::AND_ANDN_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::AND_GE, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::AND_GE_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::AND_LT, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::AND_LT_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::AND_NE, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::AND_NOR_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::AND_OR_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::AND_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ANDN, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ANDN_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::AND_EQ, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::BISR, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::BMERGE, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::BSPLIT, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::CACHEA_I, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::CACHEA_W, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::CACHEA_WI, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::CACHEI_W, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::CACHEI_WI, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::CADD, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::CADDN, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::CALL, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::CALLA, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::CALLI, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::CLO, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::CLO_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::CLS, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::CLS_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::CLZ, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::CLZ_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::CMOV, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::CMOVN, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::CMP_F, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::CSUB, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::CSUBN, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::DEBUG, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::DEXTR, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::DISABLE, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::DIV_F, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::DSYNC, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::DVADJ, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::DVINIT, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::DVINIT_B, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::DVINIT_BU, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::DVINIT_H, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::DVINIT_HU, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::DVINIT_U, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::DVSTEP, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::DVSTEP_U, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::ENABLE, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::EQ, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::EQ_B, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::EQ_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::EQ_W, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::EQANY_B, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::EQANY_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::EXTR, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::EXTR_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::FTOI, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::FTOIZ, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::FTOQ31, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::FTOQ31Z, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::FTOU, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::FTOUZ, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::GE, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::GE_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::IMASK, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::INS_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::INSERT, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::INSN_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ISYNC, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::ITOF, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::IXMAX, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::IXMAX_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::IXMIN, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::IXMIN_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::JA, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JEQ, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JEQ_A, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JGE, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JGE_U, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JGEZ, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JGTZ, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JI, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JL, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JLA, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JLEZ, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JLI, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JLT, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JLT_U, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JLTZ, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JNE, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JNE_A, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::JNED, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JNEI, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JNZ, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JNZ_A, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JNZ_T, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JZ, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JZ_A, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::JZ_T, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::LT, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::LT_B, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::LT_BU, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::LT_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::LT_HU, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::LT_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::LT_W, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::LT_WU, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MADD, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADD_F, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::MADD_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADD_Q, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADD_U, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDM_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDMS_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDR_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDR_Q, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDRS_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDRS_Q, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDS, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDS_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDS_Q, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDS_U, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDSU_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDSUM_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDSUMS_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDSUR_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDSURS_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MADDSUS_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MAX, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MAX_B, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MAX_BU, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MAX_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MAX_HU, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MAX_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MFCR, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::MIN, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MIN_B, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MIN_BU, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MIN_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MIN_HU, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MIN_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MOV, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MOV_RR, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MOV_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MOVH, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MSUB, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUB_F, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::MSUB_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUB_Q, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUB_U, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBAD_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBADM_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBADMS_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBADR_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBADRS_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBADS_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBM_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBMS_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBR_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBR_Q, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBRS_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBRS_Q, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBS, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBS_H, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBS_Q, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MSUBS_U, TC_SchedulingInfo::OperationType::mac },
    { TC131::OpCode::MTCR, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::MUL, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MUL_F, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::MUL_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MUL_Q, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MUL_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MULM_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MULR_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MULR_Q, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MULS, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::MULS_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::NAND, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::NAND_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::NE, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::NOR, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::NOR_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::NOT, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::OR, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::OR_AND_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::OR_ANDN_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::OR_EQ, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::OR_GE, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::OR_GE_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::OR_LT, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::OR_LT_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::OR_NE, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::OR_NOR_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::OR_OR_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::OR_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ORN, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::ORN_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::PACK, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::PARITY, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::Q31TOF, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::QSEED_F, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::RET, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::RFE, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::RFM, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::RSLCX, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::RSTV, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::RSUB, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::RSUBS, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::RSUBS_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SAT_B, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SAT_BU, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SAT_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SAT_HU, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SEL, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SELN, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_AND_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_ANDN_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_EQ, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_GE, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_GE_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_LT, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_LT_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_NAND_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_NE, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_NOR_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_OR_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_ORN_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_XNOR_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SH_XOR_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SHA, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SHA_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SHAS, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SUB, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SUB_B, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SUB_F, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::SUB_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SUBC, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SUBS, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SUBS_H, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SUBS_HU, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SUBS_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SUBX, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::SVLCX, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::SWAP_W, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::SYSCALL, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::TLBDEMAP, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::TLBFLUSH_A, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::TLBFLUSH_B, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::TLBMAP, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::TLBPROBE_A, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::TLBPROBE_I, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::TRAPSV, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::TRAPV, TC_SchedulingInfo::OperationType::dp },
    { TC131::OpCode::UNPACK, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::UPDFL, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::UTOF, TC_SchedulingInfo::OperationType::fp },
    { TC131::OpCode::XNOR, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::XNOR_T, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::XOR, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::XOR_EQ, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::XOR_GE, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::XOR_GE_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::XOR_LT, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::XOR_LT_U, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::XOR_NE, TC_SchedulingInfo::OperationType::ip },
    { TC131::OpCode::XOR_T, TC_SchedulingInfo::OperationType::ip } };

  mOperationTypesInitialized = true;
};


/*
  initFloatLatency initializes map mFloatLatency.

  See the comment for attribute mFloatLatencyInitialized for more information.
*/
void TC_SchedulingInfo::initFloatLatency( void )
{
  DSTART( "static void TC_SchedulingInfo::initFloatLatency()" );

  mFloatLatency.clear();

  mFloatLatency = {
    { TC131::OpCode::ADD_F, 2 },
    { TC131::OpCode::CMP_F, 2 },
    { TC131::OpCode::DIV_F, 16 },
    { TC131::OpCode::FTOI, 5 },
    { TC131::OpCode::FTOIZ, 5 },
    { TC131::OpCode::FTOQ31, 4 },
    { TC131::OpCode::FTOQ31Z, 4 },
    { TC131::OpCode::FTOU, 4 },
    { TC131::OpCode::FTOUZ, 4 },
    { TC131::OpCode::ITOF, 4 },
    { TC131::OpCode::MADD_F, 4 },
    { TC131::OpCode::MSUB_F, 4 },
    { TC131::OpCode::MUL_F, 4 },
    { TC131::OpCode::Q31TOF, 4 },
    { TC131::OpCode::QSEED_F, 4 },
    { TC131::OpCode::SUB_F, 4 },
    { TC131::OpCode::UPDFL, 4 },
    { TC131::OpCode::UTOF, 4 } };

  mFloatLatencyInitialized = true;
};


//
// Private data structures
//

// mOperationTypes maps TriCore opcodes to their operation types.
map<WIR_BaseProcessor::OpCode,
    TC_SchedulingInfo::OperationType> TC_SchedulingInfo::mOperationTypes;


// mOperationTypesInitialized stores whether map mOperationTypes above has
// already been initialized or not.
bool TC_SchedulingInfo::mOperationTypesInitialized = false;


// mFloatLatency maps TriCore floating-point opcodes to their latencies in clock
// cycles.
map<WIR_BaseProcessor::OpCode, long long> TC_SchedulingInfo::mFloatLatency;


// mFloatLatencyInitialized stores whether map mFloatLatency above has already
// been initialized or not.
bool TC_SchedulingInfo::mFloatLatencyInitialized = false;


/*
  mTypeLatency maps the types of two succeeding operations to the number of
  clock cycles it takes from issuing the first operation until availability of
  its results, depending on the type of the operation issued immediately after
  the first one.

  Multi-cycle operations are considered as single-cycle here, since they block
  issuing any other following operation.

  Latencies of floating-point operations are managed in table mFloatLatency.

  Beware: Do not set latencies in this table to 0. The two cases below are an
          exception, since they are handeled separately.
*/
const map<TC_SchedulingInfo::OperationType,
          map<TC_SchedulingInfo::OperationType, long long>> TC_SchedulingInfo::mTypeLatency {
    { TC_SchedulingInfo::OperationType::ip,
      { { TC_SchedulingInfo::OperationType::ip, 1 },
        { TC_SchedulingInfo::OperationType::ls, 0 },
        { TC_SchedulingInfo::OperationType::dp, 1 },
        { TC_SchedulingInfo::OperationType::mac, 1 },
        { TC_SchedulingInfo::OperationType::lp, 1 },
        { TC_SchedulingInfo::OperationType::fp, 1 } } },
    { TC_SchedulingInfo::OperationType::ls,
      { { TC_SchedulingInfo::OperationType::ip, 1 },
        { TC_SchedulingInfo::OperationType::ls, 1 },
        { TC_SchedulingInfo::OperationType::dp, 1 },
        { TC_SchedulingInfo::OperationType::mac, 1 },
        { TC_SchedulingInfo::OperationType::lp, 1 },
        { TC_SchedulingInfo::OperationType::fp, 1 } } },
    { TC_SchedulingInfo::OperationType::dp,
      { { TC_SchedulingInfo::OperationType::ip, 1 },
        { TC_SchedulingInfo::OperationType::ls, 1 },
        { TC_SchedulingInfo::OperationType::dp, 1 },
        { TC_SchedulingInfo::OperationType::mac, 1 },
        { TC_SchedulingInfo::OperationType::lp, 1 },
        { TC_SchedulingInfo::OperationType::fp, 1 } } },
    { TC_SchedulingInfo::OperationType::mac,
      { { TC_SchedulingInfo::OperationType::ip, 2 },
        { TC_SchedulingInfo::OperationType::ls, 0 },
        { TC_SchedulingInfo::OperationType::dp, 2 },
        { TC_SchedulingInfo::OperationType::mac, 2 },
        { TC_SchedulingInfo::OperationType::lp, 2 },
        { TC_SchedulingInfo::OperationType::fp, 2 } } },
    { TC_SchedulingInfo::OperationType::lp,
      { { TC_SchedulingInfo::OperationType::ip, 1 },
        { TC_SchedulingInfo::OperationType::ls, 1 },
        { TC_SchedulingInfo::OperationType::dp, 1 },
        { TC_SchedulingInfo::OperationType::mac, 1 },
        { TC_SchedulingInfo::OperationType::lp, 1 },
        { TC_SchedulingInfo::OperationType::fp, 1 } } },
    { TC_SchedulingInfo::OperationType::fp,
      { { TC_SchedulingInfo::OperationType::ip, 1 },
        { TC_SchedulingInfo::OperationType::ls, 1 },
        { TC_SchedulingInfo::OperationType::dp, 1 },
        { TC_SchedulingInfo::OperationType::mac, 1 },
        { TC_SchedulingInfo::OperationType::lp, 1 },
        { TC_SchedulingInfo::OperationType::fp, 1 } } } };


// mPairPriority maps the types of two succeeding operations to their respective
// scheduling priority. Rows denote preceding operation types, columns the
// currently scheduled operation.
const map<TC_SchedulingInfo::OperationType,
          map<TC_SchedulingInfo::OperationType, long long>> TC_SchedulingInfo::mPairPriority {
    { TC_SchedulingInfo::OperationType::ip,
      { { TC_SchedulingInfo::OperationType::ip, 1 },
        { TC_SchedulingInfo::OperationType::ls, 3 },
        { TC_SchedulingInfo::OperationType::dp, 2 },
        { TC_SchedulingInfo::OperationType::mac,21 },
        { TC_SchedulingInfo::OperationType::lp, 1 },
        { TC_SchedulingInfo::OperationType::fp, 1 } } },
    { TC_SchedulingInfo::OperationType::ls,
      { { TC_SchedulingInfo::OperationType::ip, 2 },
        { TC_SchedulingInfo::OperationType::ls, 1 },
        { TC_SchedulingInfo::OperationType::dp, 3 },
        { TC_SchedulingInfo::OperationType::mac, 3 },
        { TC_SchedulingInfo::OperationType::lp, 1 },
        { TC_SchedulingInfo::OperationType::fp, 3 } } },
    { TC_SchedulingInfo::OperationType::dp,
      { { TC_SchedulingInfo::OperationType::ip, 2 },
        { TC_SchedulingInfo::OperationType::ls, 1 },
        { TC_SchedulingInfo::OperationType::dp, 3 },
        { TC_SchedulingInfo::OperationType::mac, 3 },
        { TC_SchedulingInfo::OperationType::lp, 1 },
        { TC_SchedulingInfo::OperationType::fp, 3 } } },
    { TC_SchedulingInfo::OperationType::mac,
      { { TC_SchedulingInfo::OperationType::ip, 1 },
        { TC_SchedulingInfo::OperationType::ls, 3 },
        { TC_SchedulingInfo::OperationType::dp, 2 },
        { TC_SchedulingInfo::OperationType::mac, 2 },
        { TC_SchedulingInfo::OperationType::lp, 1 },
        { TC_SchedulingInfo::OperationType::fp, 1 } } },
    { TC_SchedulingInfo::OperationType::lp,
      { { TC_SchedulingInfo::OperationType::ip, 1 },
        { TC_SchedulingInfo::OperationType::ls, 1 },
        { TC_SchedulingInfo::OperationType::dp, 1 },
        { TC_SchedulingInfo::OperationType::mac, 1 },
        { TC_SchedulingInfo::OperationType::lp, 1 },
        { TC_SchedulingInfo::OperationType::fp, 1 } } },
    { TC_SchedulingInfo::OperationType::fp,
      { { TC_SchedulingInfo::OperationType::ip, 2 },
        { TC_SchedulingInfo::OperationType::ls, 1 },
        { TC_SchedulingInfo::OperationType::dp, 3 },
        { TC_SchedulingInfo::OperationType::mac, 3 },
        { TC_SchedulingInfo::OperationType::lp, 1 },
        { TC_SchedulingInfo::OperationType::fp, 4 } } } };


// mSinglePriority maps the types of operations having no predecessor to their
// respective scheduling priority. Operations without previously scheduled
// predecessors are those that are entries of a scheduling region.
const map<TC_SchedulingInfo::OperationType, long long> TC_SchedulingInfo::mSinglePriority {
  { TC_SchedulingInfo::OperationType::ip, 2 },
  { TC_SchedulingInfo::OperationType::ls, 1 },
  { TC_SchedulingInfo::OperationType::dp, 3 },
  { TC_SchedulingInfo::OperationType::mac, 3 },
  { TC_SchedulingInfo::OperationType::lp, 1 },
  { TC_SchedulingInfo::OperationType::fp, 3 } };

}       // namespace WIR
