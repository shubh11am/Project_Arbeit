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
  @file tc13opcodes.cc
  @brief This file declares the Infineon TriCore V1.3's opcodes.

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
#include <arch/tricore/tc13.h>


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

const TC13::OpCode TC13::OpCode::ABS        { string { "abs" } };
const TC13::OpCode TC13::OpCode::ABS_B      { string { "abs.b" } };
const TC13::OpCode TC13::OpCode::ABS_H      { string { "abs.h" } };
const TC13::OpCode TC13::OpCode::ABSDIF     { string { "absdif" } };
const TC13::OpCode TC13::OpCode::ABSDIF_B   { string { "absdif.b" } };
const TC13::OpCode TC13::OpCode::ABSDIF_H   { string { "absdif.h" } };
const TC13::OpCode TC13::OpCode::ABSDIFS    { string { "absdifs" } };
const TC13::OpCode TC13::OpCode::ABSDIFS_H  { string { "absdifs.h" } };
const TC13::OpCode TC13::OpCode::ABSS       { string { "abss" } };
const TC13::OpCode TC13::OpCode::ABSS_H     { string { "abss.h" } };
const TC13::OpCode TC13::OpCode::ADD        { string { "add" } };
const TC13::OpCode TC13::OpCode::ADD_A      { string { "add.a" } };
const TC13::OpCode TC13::OpCode::ADD_B      { string { "add.b" } };
const TC13::OpCode TC13::OpCode::ADD_F      { string { "add.f" } };
const TC13::OpCode TC13::OpCode::ADD_H      { string { "add.h" } };
const TC13::OpCode TC13::OpCode::ADDC       { string { "addc" } };
const TC13::OpCode TC13::OpCode::ADDI       { string { "addi" } };
const TC13::OpCode TC13::OpCode::ADDIH      { string { "addih" } };
const TC13::OpCode TC13::OpCode::ADDIH_A    { string { "addih.a" } };
const TC13::OpCode TC13::OpCode::ADDS       { string { "adds" } };
const TC13::OpCode TC13::OpCode::ADDS_H     { string { "adds.h" } };
const TC13::OpCode TC13::OpCode::ADDS_HU    { string { "adds.hu" } };
const TC13::OpCode TC13::OpCode::ADDS_U     { string { "adds.u" } };
const TC13::OpCode TC13::OpCode::ADDSC_A    { string { "addsc.a" } };
const TC13::OpCode TC13::OpCode::ADDSC_AT   { string { "addsc.at" } };
const TC13::OpCode TC13::OpCode::ADDX       { string { "addx" } };
const TC13::OpCode TC13::OpCode::AND        { string { "and" } };
const TC13::OpCode TC13::OpCode::AND_AND_T  { string { "and.and.t" } };
const TC13::OpCode TC13::OpCode::AND_ANDN_T { string { "and.andn.t" } };
const TC13::OpCode TC13::OpCode::AND_EQ     { string { "and.eq" } };
const TC13::OpCode TC13::OpCode::AND_GE     { string { "and.ge" } };
const TC13::OpCode TC13::OpCode::AND_GE_U   { string { "and.ge.u" } };
const TC13::OpCode TC13::OpCode::AND_LT     { string { "and.lt" } };
const TC13::OpCode TC13::OpCode::AND_LT_U   { string { "and.lt.u" } };
const TC13::OpCode TC13::OpCode::AND_NE     { string { "and.ne" } };
const TC13::OpCode TC13::OpCode::AND_NOR_T  { string { "and.nor.t" } };
const TC13::OpCode TC13::OpCode::AND_OR_T   { string { "and.or.t" } };
const TC13::OpCode TC13::OpCode::AND_T      { string { "and.t" } };
const TC13::OpCode TC13::OpCode::ANDN       { string { "andn" } };
const TC13::OpCode TC13::OpCode::ANDN_T     { string { "andn.t" } };
const TC13::OpCode TC13::OpCode::BISR       { string { "bisr" },
                                              MA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::BMERGE     { string { "bmerge" } };
const TC13::OpCode TC13::OpCode::BSPLIT     { string { "bsplit" } };
const TC13::OpCode TC13::OpCode::CACHEA_I   { string { "cachea.i" },
                                              MA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::CACHEA_W   { string { "cachea.w" },
                                              MA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::CACHEA_WI  { string { "cachea.wi" },
                                              MA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::CADD       { string { "cadd" } };
const TC13::OpCode TC13::OpCode::CADDN      { string { "caddn" } };
const TC13::OpCode TC13::OpCode::CALL       { string { "call" },
                                              MA, noST, noLD, noMV, FC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::CALLA      { string { "calla" },
                                              MA, noST, noLD, noMV, FC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::CALLI      { string { "calli" },
                                              MA, noST, noLD, noMV, noFC,
                                              IFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::CLO        { string { "clo" } };
const TC13::OpCode TC13::OpCode::CLO_H      { string { "clo.h" } };
const TC13::OpCode TC13::OpCode::CLS        { string { "cls" } };
const TC13::OpCode TC13::OpCode::CLS_H      { string { "cls.h" } };
const TC13::OpCode TC13::OpCode::CLZ        { string { "clz" } };
const TC13::OpCode TC13::OpCode::CLZ_H      { string { "clz.h" } };
const TC13::OpCode TC13::OpCode::CMOV       { string { "cmov" } };
const TC13::OpCode TC13::OpCode::CMOVN      { string { "cmovn" } };
const TC13::OpCode TC13::OpCode::CMP_F      { string { "cmp.f" } };
const TC13::OpCode TC13::OpCode::CSUB       { string { "csub" } };
const TC13::OpCode TC13::OpCode::CSUBN      { string { "csubn" } };
const TC13::OpCode TC13::OpCode::DEBUG      { string { "debug" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::DEXTR      { string { "dextr" } };
const TC13::OpCode TC13::OpCode::DISABLE    { string { "disable" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::DIV_F      { string { "div.f" } };
const TC13::OpCode TC13::OpCode::DSYNC      { string { "dsync" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::DVADJ      { string { "dvadj" } };
const TC13::OpCode TC13::OpCode::DVINIT     { string { "dvinit" } };
const TC13::OpCode TC13::OpCode::DVINIT_B   { string { "dvinit.b" } };
const TC13::OpCode TC13::OpCode::DVINIT_BU  { string { "dvinit.bu" } };
const TC13::OpCode TC13::OpCode::DVINIT_H   { string { "dvinit.h" } };
const TC13::OpCode TC13::OpCode::DVINIT_HU  { string { "dvinit.hu" } };
const TC13::OpCode TC13::OpCode::DVINIT_U   { string { "dvinit.u" } };
const TC13::OpCode TC13::OpCode::DVSTEP     { string { "dvstep" } };
const TC13::OpCode TC13::OpCode::DVSTEP_U   { string { "dvstep.u" } };
const TC13::OpCode TC13::OpCode::ENABLE     { string { "enable" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::EQ         { string { "eq" } };
const TC13::OpCode TC13::OpCode::EQ_A       { string { "eq.a" } };
const TC13::OpCode TC13::OpCode::EQ_B       { string { "eq.b" } };
const TC13::OpCode TC13::OpCode::EQ_H       { string { "eq.h" } };
const TC13::OpCode TC13::OpCode::EQ_W       { string { "eq.w" } };
const TC13::OpCode TC13::OpCode::EQANY_B    { string { "eqany.b" } };
const TC13::OpCode TC13::OpCode::EQANY_H    { string { "eqany.h" } };
const TC13::OpCode TC13::OpCode::EQZ_A      { string { "eqz.a" } };
const TC13::OpCode TC13::OpCode::EXTR       { string { "extr" } };
const TC13::OpCode TC13::OpCode::EXTR_U     { string { "extr.u" } };
const TC13::OpCode TC13::OpCode::FTOI       { string { "ftoi" } };
const TC13::OpCode TC13::OpCode::FTOQ31     { string { "ftoq31" } };
const TC13::OpCode TC13::OpCode::FTOU       { string { "ftou" } };
const TC13::OpCode TC13::OpCode::GE         { string { "ge" } };
const TC13::OpCode TC13::OpCode::GE_A       { string { "ge.a" } };
const TC13::OpCode TC13::OpCode::GE_U       { string { "ge.u" } };
const TC13::OpCode TC13::OpCode::IMASK      { string { "imask" } };
const TC13::OpCode TC13::OpCode::INS_T      { string { "ins.t" } };
const TC13::OpCode TC13::OpCode::INSERT     { string { "insert" } };
const TC13::OpCode TC13::OpCode::INSN_T     { string { "insn.t" } };
const TC13::OpCode TC13::OpCode::ISYNC      { string { "isync" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::ITOF       { string { "itof" } };
const TC13::OpCode TC13::OpCode::IXMAX      { string { "ixmax" } };
const TC13::OpCode TC13::OpCode::IXMAX_U    { string { "ixmax.u" } };
const TC13::OpCode TC13::OpCode::IXMIN      { string { "ixmin" } };
const TC13::OpCode TC13::OpCode::IXMIN_U    { string { "ixmin.u" } };
const TC13::OpCode TC13::OpCode::J          { string { "j" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, UJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JA         { string { "ja" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, UJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JEQ        { string { "jeq" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JEQ_A      { string { "jeq.a" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JGE        { string { "jge" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JGE_U      { string { "jge.u" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JGEZ       { string { "jgez" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JGTZ       { string { "jgtz" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JI         { string { "ji" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, UJ, IJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JL         { string { "jl" },
                                              noMA, noST, noLD, noMV, FC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JLA        { string { "jla" },
                                              noMA, noST, noLD, noMV, FC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JLEZ       { string { "jlez" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JLI        { string { "jli" },
                                              noMA, noST, noLD, noMV, noFC,
                                              IFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JLT        { string { "jlt" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JLT_U      { string { "jlt.u" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JLTZ       { string { "jltz" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JNE        { string { "jne" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JNE_A      { string { "jne.a" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JNED       { string { "jned" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JNEI       { string { "jnei" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JNZ        { string { "jnz" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JNZ_A      { string { "jnz.a" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JNZ_T      { string { "jnz.t" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JZ         { string { "jz" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JZ_A       { string { "jz.a" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::JZ_T       { string { "jz.t" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::LD_A       { string { "ld.a" },
                                              noMA, noST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::LD_B       { string { "ld.b" },
                                              noMA, noST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::LD_BU      { string { "ld.bu" },
                                              noMA, noST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::LD_D       { string { "ld.d" },
                                              noMA, noST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::LD_DA      { string { "ld.da" },
                                              noMA, noST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::LD_H       { string { "ld.h" },
                                              noMA, noST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::LD_HU      { string { "ld.hu" },
                                              noMA, noST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::LD_Q       { string { "ld.q" },
                                              noMA, noST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::LD_W       { string { "ld.w" },
                                              noMA, noST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::LDLCX      { string { "ldlcx" },
                                              noMA, noST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::LDMST      { string { "ldmst" },
                                              noMA, ST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::LDUCX      { string { "lducx" },
                                              noMA, noST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::LEA        { string { "lea" } };
const TC13::OpCode TC13::OpCode::LOOP       { string { "loop" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, CJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::LOOPU      { string { "loopu" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, UJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::LT         { string { "lt" } };
const TC13::OpCode TC13::OpCode::LT_A       { string { "lt.a" } };
const TC13::OpCode TC13::OpCode::LT_B       { string { "lt.b" } };
const TC13::OpCode TC13::OpCode::LT_BU      { string { "lt.bu" } };
const TC13::OpCode TC13::OpCode::LT_H       { string { "lt.h" } };
const TC13::OpCode TC13::OpCode::LT_HU      { string { "lt.hu" } };
const TC13::OpCode TC13::OpCode::LT_U       { string { "lt.u" } };
const TC13::OpCode TC13::OpCode::LT_W       { string { "lt.w" } };
const TC13::OpCode TC13::OpCode::LT_WU      { string { "lt.wu" } };
const TC13::OpCode TC13::OpCode::MADD       { string { "madd" } };
const TC13::OpCode TC13::OpCode::MADD_F     { string { "madd.f" } };
const TC13::OpCode TC13::OpCode::MADD_H     { string { "madd.h" } };
const TC13::OpCode TC13::OpCode::MADD_Q     { string { "madd.q" } };
const TC13::OpCode TC13::OpCode::MADD_U     { string { "madd.u" } };
const TC13::OpCode TC13::OpCode::MADDM_H    { string { "maddm.h" } };
const TC13::OpCode TC13::OpCode::MADDMS_H   { string { "maddms.h" } };
const TC13::OpCode TC13::OpCode::MADDR_H    { string { "maddr.h" } };
const TC13::OpCode TC13::OpCode::MADDR_Q    { string { "maddr.q" } };
const TC13::OpCode TC13::OpCode::MADDRS_H   { string { "maddrs.h" } };
const TC13::OpCode TC13::OpCode::MADDRS_Q   { string { "maddrs.q" } };
const TC13::OpCode TC13::OpCode::MADDS      { string { "madds" } };
const TC13::OpCode TC13::OpCode::MADDS_H    { string { "madds.h" } };
const TC13::OpCode TC13::OpCode::MADDS_Q    { string { "madds.q" } };
const TC13::OpCode TC13::OpCode::MADDS_U    { string { "madds.u" } };
const TC13::OpCode TC13::OpCode::MADDSU_H   { string { "maddsu.h" } };
const TC13::OpCode TC13::OpCode::MADDSUM_H  { string { "maddsum.h" } };
const TC13::OpCode TC13::OpCode::MADDSUMS_H { string { "maddsums.h" } };
const TC13::OpCode TC13::OpCode::MADDSUR_H  { string { "maddsur.h" } };
const TC13::OpCode TC13::OpCode::MADDSURS_H { string { "maddsurs.h" } };
const TC13::OpCode TC13::OpCode::MADDSUS_H  { string { "maddsus.h" } };
const TC13::OpCode TC13::OpCode::MAX        { string { "max" } };
const TC13::OpCode TC13::OpCode::MAX_B      { string { "max.b" } };
const TC13::OpCode TC13::OpCode::MAX_BU     { string { "max.bu" } };
const TC13::OpCode TC13::OpCode::MAX_H      { string { "max.h" } };
const TC13::OpCode TC13::OpCode::MAX_HU     { string { "max.hu" } };
const TC13::OpCode TC13::OpCode::MAX_U      { string { "max.u" } };
const TC13::OpCode TC13::OpCode::MFCR       { string { "mfcr" } };
const TC13::OpCode TC13::OpCode::MIN        { string { "min" } };
const TC13::OpCode TC13::OpCode::MIN_B      { string { "min.b" } };
const TC13::OpCode TC13::OpCode::MIN_BU     { string { "min.bu" } };
const TC13::OpCode TC13::OpCode::MIN_H      { string { "min.h" } };
const TC13::OpCode TC13::OpCode::MIN_HU     { string { "min.hu" } };
const TC13::OpCode TC13::OpCode::MIN_U      { string { "min.u" } };
const TC13::OpCode TC13::OpCode::MOV        { string { "mov" } };
const TC13::OpCode TC13::OpCode::MOV_RR     { string { "mov" },
                                              noMA, noST, noLD, MV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::MOV_A      { string { "mov.a" } };
const TC13::OpCode TC13::OpCode::MOV_AA     { string { "mov.aa" },
                                              noMA, noST, noLD, MV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::MOV_D      { string { "mov.d" } };
const TC13::OpCode TC13::OpCode::MOV_U      { string { "mov.u" } };
const TC13::OpCode TC13::OpCode::MOVH       { string { "movh" } };
const TC13::OpCode TC13::OpCode::MOVH_A     { string { "movh.a" } };
const TC13::OpCode TC13::OpCode::MSUB       { string { "msub" } };
const TC13::OpCode TC13::OpCode::MSUB_F     { string { "msub.f" } };
const TC13::OpCode TC13::OpCode::MSUB_H     { string { "msub.h" } };
const TC13::OpCode TC13::OpCode::MSUB_Q     { string { "msub.q" } };
const TC13::OpCode TC13::OpCode::MSUB_U     { string { "msub.u" } };
const TC13::OpCode TC13::OpCode::MSUBAD_H   { string { "msubad.h" } };
const TC13::OpCode TC13::OpCode::MSUBADM_H  { string { "msubadm.h" } };
const TC13::OpCode TC13::OpCode::MSUBADMS_H { string { "msubadms.h" } };
const TC13::OpCode TC13::OpCode::MSUBADR_H  { string { "msubadr.h" } };
const TC13::OpCode TC13::OpCode::MSUBADRS_H { string { "msubadrs.h" } };
const TC13::OpCode TC13::OpCode::MSUBADS_H  { string { "msubads.h" } };
const TC13::OpCode TC13::OpCode::MSUBM_H    { string { "msubm.h" } };
const TC13::OpCode TC13::OpCode::MSUBMS_H   { string { "msubms.h" } };
const TC13::OpCode TC13::OpCode::MSUBR_H    { string { "msubr.h" } };
const TC13::OpCode TC13::OpCode::MSUBR_Q    { string { "msubr.q" } };
const TC13::OpCode TC13::OpCode::MSUBRS_H   { string { "msubrs.h" } };
const TC13::OpCode TC13::OpCode::MSUBRS_Q   { string { "msubrs.q" } };
const TC13::OpCode TC13::OpCode::MSUBS      { string { "msubs" } };
const TC13::OpCode TC13::OpCode::MSUBS_H    { string { "msubs.h" } };
const TC13::OpCode TC13::OpCode::MSUBS_Q    { string { "msubs.q" } };
const TC13::OpCode TC13::OpCode::MSUBS_U    { string { "msubs.u" } };
const TC13::OpCode TC13::OpCode::MTCR       { string { "mtcr" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::MUL        { string { "mul" } };
const TC13::OpCode TC13::OpCode::MUL_F      { string { "mul.f" } };
const TC13::OpCode TC13::OpCode::MUL_H      { string { "mul.h" } };
const TC13::OpCode TC13::OpCode::MUL_Q      { string { "mul.q" } };
const TC13::OpCode TC13::OpCode::MUL_U      { string { "mul.u" } };
const TC13::OpCode TC13::OpCode::MULM_H     { string { "mulm.h" } };
const TC13::OpCode TC13::OpCode::MULR_H     { string { "mulr.h" } };
const TC13::OpCode TC13::OpCode::MULR_Q     { string { "mulr.q" } };
const TC13::OpCode TC13::OpCode::MULS       { string { "muls" } };
const TC13::OpCode TC13::OpCode::MULS_U     { string { "muls.u" } };
const TC13::OpCode TC13::OpCode::NAND       { string { "nand" } };
const TC13::OpCode TC13::OpCode::NAND_T     { string { "nand.t" } };
const TC13::OpCode TC13::OpCode::NE         { string { "ne" } };
const TC13::OpCode TC13::OpCode::NE_A       { string { "ne.a" } };
const TC13::OpCode TC13::OpCode::NEZ_A      { string { "nez.a" } };
const TC13::OpCode TC13::OpCode::NOP        { string { "nop" } };
const TC13::OpCode TC13::OpCode::NOR        { string { "nor" } };
const TC13::OpCode TC13::OpCode::NOR_T      { string { "nor.t" } };
const TC13::OpCode TC13::OpCode::NOT        { string { "not" } };
const TC13::OpCode TC13::OpCode::OR         { string { "or" } };
const TC13::OpCode TC13::OpCode::OR_AND_T   { string { "or.and.t" } };
const TC13::OpCode TC13::OpCode::OR_ANDN_T  { string { "or.andn.t" } };
const TC13::OpCode TC13::OpCode::OR_EQ      { string { "or.eq" } };
const TC13::OpCode TC13::OpCode::OR_GE      { string { "or.ge" } };
const TC13::OpCode TC13::OpCode::OR_GE_U    { string { "or.ge.u" } };
const TC13::OpCode TC13::OpCode::OR_LT      { string { "or.lt" } };
const TC13::OpCode TC13::OpCode::OR_LT_U    { string { "or.lt.u" } };
const TC13::OpCode TC13::OpCode::OR_NE      { string { "or.ne" } };
const TC13::OpCode TC13::OpCode::OR_NOR_T   { string { "or.nor.t" } };
const TC13::OpCode TC13::OpCode::OR_OR_T    { string { "or.or.t" } };
const TC13::OpCode TC13::OpCode::OR_T       { string { "or.t" } };
const TC13::OpCode TC13::OpCode::ORN        { string { "orn" } };
const TC13::OpCode TC13::OpCode::ORN_T      { string { "orn.t" } };
const TC13::OpCode TC13::OpCode::PACK       { string { "pack" } };
const TC13::OpCode TC13::OpCode::PARITY     { string { "parity" } };
const TC13::OpCode TC13::OpCode::Q31TOF     { string { "q31tof" } };
const TC13::OpCode TC13::OpCode::QSEED_F    { string { "qseed.f" } };
const TC13::OpCode TC13::OpCode::RET        { string { "ret" },
                                              MA, noST, noLD, noMV, noFC,
                                              noIFC, RT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::RFE        { string { "rfe" },
                                              MA, noST, noLD, noMV, noFC,
                                              noIFC, RT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::RFM        { string { "rfm" },
                                              MA, noST, noLD, noMV, noFC,
                                              noIFC, RT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::RSLCX      { string { "rslcx" },
                                              noMA, noST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::RSTV       { string { "rstv" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::RSUB       { string { "rsub" } };
const TC13::OpCode TC13::OpCode::RSUBS      { string { "rsubs" } };
const TC13::OpCode TC13::OpCode::RSUBS_U    { string { "rsubs.u" } };
const TC13::OpCode TC13::OpCode::SAT_B      { string { "sat.b" } };
const TC13::OpCode TC13::OpCode::SAT_BU     { string { "sat.bu" } };
const TC13::OpCode TC13::OpCode::SAT_H      { string { "sat.h" } };
const TC13::OpCode TC13::OpCode::SAT_HU     { string { "sat.hu" } };
const TC13::OpCode TC13::OpCode::SEL        { string { "sel" } };
const TC13::OpCode TC13::OpCode::SELN       { string { "seln" } };
const TC13::OpCode TC13::OpCode::SH         { string { "sh" } };
const TC13::OpCode TC13::OpCode::SH_AND_T   { string { "sh.and.t" } };
const TC13::OpCode TC13::OpCode::SH_ANDN_T  { string { "sh.andn.t" } };
const TC13::OpCode TC13::OpCode::SH_EQ      { string { "sh.eq" } };
const TC13::OpCode TC13::OpCode::SH_GE      { string { "sh.ge" } };
const TC13::OpCode TC13::OpCode::SH_GE_U    { string { "sh.ge.u" } };
const TC13::OpCode TC13::OpCode::SH_H       { string { "sh.h" } };
const TC13::OpCode TC13::OpCode::SH_LT      { string { "sh.lt" } };
const TC13::OpCode TC13::OpCode::SH_LT_U    { string { "sh.lt.u" } };
const TC13::OpCode TC13::OpCode::SH_NAND_T  { string { "sh.nand.t" } };
const TC13::OpCode TC13::OpCode::SH_NE      { string { "sh.ne" } };
const TC13::OpCode TC13::OpCode::SH_NOR_T   { string { "sh.nor.t" } };
const TC13::OpCode TC13::OpCode::SH_OR_T    { string { "sh.or.t" } };
const TC13::OpCode TC13::OpCode::SH_ORN_T   { string { "sh.orn.t" } };
const TC13::OpCode TC13::OpCode::SH_XNOR_T  { string { "sh.xnor.t" } };
const TC13::OpCode TC13::OpCode::SH_XOR_T   { string { "sh.xor.t" } };
const TC13::OpCode TC13::OpCode::SHA        { string { "sha" } };
const TC13::OpCode TC13::OpCode::SHA_H      { string { "sha.h" } };
const TC13::OpCode TC13::OpCode::SHAS       { string { "shas" } };
const TC13::OpCode TC13::OpCode::ST_A       { string { "st.a" },
                                              noMA, ST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::ST_B       { string { "st.b" },
                                              noMA, ST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::ST_D       { string { "st.d" },
                                              noMA, ST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::ST_DA      { string { "st.da" },
                                              noMA, ST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::ST_H       { string { "st.h" },
                                              noMA, ST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::ST_Q       { string { "st.q" },
                                              noMA, ST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::ST_T       { string { "st.t" },
                                              noMA, ST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::ST_W       { string { "st.w" },
                                              noMA, ST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::STLCX      { string { "stlcx" },
                                              noMA, ST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::STUCX      { string { "stucx" },
                                              noMA, ST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::SUB        { string { "sub" } };
const TC13::OpCode TC13::OpCode::SUB_A      { string { "sub.a" } };
const TC13::OpCode TC13::OpCode::SUB_B      { string { "sub.b" } };
const TC13::OpCode TC13::OpCode::SUB_F      { string { "sub.f" } };
const TC13::OpCode TC13::OpCode::SUB_H      { string { "sub.h" } };
const TC13::OpCode TC13::OpCode::SUBC       { string { "subc" } };
const TC13::OpCode TC13::OpCode::SUBS       { string { "subs" } };
const TC13::OpCode TC13::OpCode::SUBS_H     { string { "subs.h" } };
const TC13::OpCode TC13::OpCode::SUBS_HU    { string { "subs.hu" } };
const TC13::OpCode TC13::OpCode::SUBS_U     { string { "subs.u" } };
const TC13::OpCode TC13::OpCode::SUBX       { string { "subx" } };
const TC13::OpCode TC13::OpCode::SVLCX      { string { "svlcx" },
                                              noMA, ST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::SWAP_W     { string { "swap.w" },
                                              noMA, ST, LD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, noSE };
const TC13::OpCode TC13::OpCode::SYSCALL    { string { "syscall" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::TLBDEMAP   { string { "tlbdemap" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::TLBFLUSH_A { string { "tlbflush.a" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::TLBFLUSH_B { string { "tlbflush.b" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::TLBMAP     { string { "tlbmap" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::TLBPROBE_A { string { "tlbprobe.a" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::TLBPROBE_I { string { "tlbprobe.i" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::TRAPSV     { string { "trapsv" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::TRAPV      { string { "trapv" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::UNPACK     { string { "unpack" } };
const TC13::OpCode TC13::OpCode::UPDFL      { string { "updfl" },
                                              noMA, noST, noLD, noMV, noFC,
                                              noIFC, noRT, noCJ, noUJ, noIJ,
                                              noAD, SE };
const TC13::OpCode TC13::OpCode::UTOF       { string { "utof" } };
const TC13::OpCode TC13::OpCode::XNOR       { string { "xnor" } };
const TC13::OpCode TC13::OpCode::XNOR_T     { string { "xnor.t" } };
const TC13::OpCode TC13::OpCode::XOR        { string { "xor" } };
const TC13::OpCode TC13::OpCode::XOR_EQ     { string { "xor.eq" } };
const TC13::OpCode TC13::OpCode::XOR_GE     { string { "xor.ge" } };
const TC13::OpCode TC13::OpCode::XOR_GE_U   { string { "xor.ge.u" } };
const TC13::OpCode TC13::OpCode::XOR_LT     { string { "xor.lt" } };
const TC13::OpCode TC13::OpCode::XOR_LT_U   { string { "xor.lt.u" } };
const TC13::OpCode TC13::OpCode::XOR_NE     { string { "xor.ne" } };
const TC13::OpCode TC13::OpCode::XOR_T      { string { "xor.t" } };

}       // namespace WIR
