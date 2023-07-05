/*
  Verbatim C code section
*/

%{
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


#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <cstdlib>
#include <cstring>
#include <sstream>
#include <string>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>

// Include local headers
#include <arch/tricore/asmparser/tcasmcontext.h>
#include <arch/tricore/asmparser/tcasmlex.h>
#include "tcasmyacc.hh"


using namespace std;


//
// Global data type declarations
//

// Import the parser's token type into local types.
using token = WIR::TC_AsmYacc::token;
using token_type = WIR::TC_AsmYacc::token_type;


//
// Preprocessor macros
//

/*
  By default, yylex returns int but we use token_type. Unfortunately,
  yyterminate by default returns 0, which is not of token_type.
*/
#define yyterminate() return( token::END )

/*
  The following macro disables inclusion of unistd.h which is not available in
  Visual C++ on Win32. The C++ scanner uses STL streams instead.
*/
#define YY_NO_UNISTD_H

%}


/*
  Flex options section
*/

/* Enable C++ scanner class generation. */
%option c++

/* Change the name of the scanner class to 'TC_AsmFlexLexer'. */
%option prefix="TC_Asm"

/* Enable scanner to generate debug output. */
%option debug

/* No support for include files is planned. */
%option yywrap nounput

/* Enables the use of start condition stacks. */
%option stack

/*
  The following macro suffices to track locations accurately. Each time yylex is
  invoked, the begin position is moved on to the end position.
*/
%{
#define YY_USER_ACTION yylloc->columns( yyleng );
%}

/*
  The following macro is a little helper to keep the rules producing mnemonic
  tokens simple.
*/
%{
#define TOK_MNEMONIC( opcode )                                                 \
  yylval->mnemonic = &TC131::OpCode::opcode; return( token::MNEMONIC );
%}


%%


 /*
  Rules section
 */

 /* Code to put at the beginning of yylex() */
%{
  // Reset location.
  yylloc->step();
%}

"\n" {
  return( token::NEWLINE );
};

 /* Gobble up whitespaces. */
[ \t\r]+ {
  yylloc->step();
};

 /* TriCore 1.3 Mnemonics */
"abs" { TOK_MNEMONIC( ABS ) };
"abs.b" { TOK_MNEMONIC( ABS_B ) };
"abs.h" { TOK_MNEMONIC( ABS_H ) };
"absdif" { TOK_MNEMONIC( ABSDIF ) };
"absdif.b" { TOK_MNEMONIC( ABSDIF_B ) };
"absdif.h" { TOK_MNEMONIC( ABSDIF_H ) };
"absdifs" { TOK_MNEMONIC( ABSDIFS ) };
"absdifs.h" { TOK_MNEMONIC( ABSDIFS_H ) };
"abss" { TOK_MNEMONIC( ABSS ) };
"abss.h" { TOK_MNEMONIC( ABSS_H ) };
"add" { TOK_MNEMONIC( ADD ) };
"add.a" { TOK_MNEMONIC( ADD_A ) };
"add.b" { TOK_MNEMONIC( ADD_B ) };
"add.f" { TOK_MNEMONIC( ADD_F ) };
"add.h" { TOK_MNEMONIC( ADD_H ) };
"addc" { TOK_MNEMONIC( ADDC ) };
"addi" { TOK_MNEMONIC( ADDI ) };
"addih" { TOK_MNEMONIC( ADDIH ) };
"addih.a" { TOK_MNEMONIC( ADDIH_A ) };
"adds" { TOK_MNEMONIC( ADDS ) };
"adds.h" { TOK_MNEMONIC( ADDS_H ) };
"adds.hu" { TOK_MNEMONIC( ADDS_HU ) };
"adds.u" { TOK_MNEMONIC( ADDS_U ) };
"addsc.a" { TOK_MNEMONIC( ADDSC_A ) };
"addsc.at" { TOK_MNEMONIC( ADDSC_AT ) };
"addx" { TOK_MNEMONIC( ADDX ) };
"and" { TOK_MNEMONIC( AND ) };
"and.and.t" { TOK_MNEMONIC( AND_AND_T ) };
"and.andn.t" { TOK_MNEMONIC( AND_ANDN_T ) };
"and.eq" { TOK_MNEMONIC( AND_EQ ) };
"and.ge" { TOK_MNEMONIC( AND_GE ) };
"and.ge.u" { TOK_MNEMONIC( AND_GE_U ) };
"and.lt" { TOK_MNEMONIC( AND_LT ) };
"and.lt.u" { TOK_MNEMONIC( AND_LT_U ) };
"and.ne" { TOK_MNEMONIC( AND_NE ) };
"and.nor.t" { TOK_MNEMONIC( AND_NOR_T ) };
"and.or.t" { TOK_MNEMONIC( AND_OR_T ) };
"and.t" { TOK_MNEMONIC( AND_T ) };
"andn" { TOK_MNEMONIC( ANDN ) };
"andn.t" { TOK_MNEMONIC( ANDN_T ) };
"bisr" { TOK_MNEMONIC( BISR ) };
"bmerge" { TOK_MNEMONIC( BMERGE ) };
"bsplit" { TOK_MNEMONIC( BSPLIT ) };
"cachea.i" { TOK_MNEMONIC( CACHEA_I ) };
"cachea.w" { TOK_MNEMONIC( CACHEA_W ) };
"cachea.wi" { TOK_MNEMONIC( CACHEA_WI ) };
"cadd" { TOK_MNEMONIC( CADD ) };
"caddn" { TOK_MNEMONIC( CADDN ) };
"call" { TOK_MNEMONIC( CALL ) };
"calla" { TOK_MNEMONIC( CALLA ) };
"calli" { TOK_MNEMONIC( CALLI ) };
"clo" { TOK_MNEMONIC( CLO ) };
"clo.h" { TOK_MNEMONIC( CLO_H ) };
"cls" { TOK_MNEMONIC( CLS ) };
"cls.h" { TOK_MNEMONIC( CLS_H ) };
"clz" { TOK_MNEMONIC( CLZ ) };
"clz.h" { TOK_MNEMONIC( CLZ_H ) };
"cmov" { TOK_MNEMONIC( CMOV ) };
"cmovn" { TOK_MNEMONIC( CMOVN ) };
"cmp.f" { TOK_MNEMONIC( CMP_F ) };
"csub" { TOK_MNEMONIC( CSUB ) };
"csubn" { TOK_MNEMONIC( CSUBN ) };
"debug" { TOK_MNEMONIC( DEBUG ) };
"dextr" { TOK_MNEMONIC( DEXTR ) };
"disable" { TOK_MNEMONIC( DISABLE ) };
"div.f" { TOK_MNEMONIC( DIV_F ) };
"dsync" { TOK_MNEMONIC( DSYNC ) };
"dvadj" { TOK_MNEMONIC( DVADJ ) };
"dvinit" { TOK_MNEMONIC( DVINIT ) };
"dvinit.b" { TOK_MNEMONIC( DVINIT_B ) };
"dvinit.bu" { TOK_MNEMONIC( DVINIT_BU ) };
"dvinit.h" { TOK_MNEMONIC( DVINIT_H ) };
"dvinit.hu" { TOK_MNEMONIC( DVINIT_HU ) };
"dvinit.u" { TOK_MNEMONIC( DVINIT_U ) };
"dvstep" { TOK_MNEMONIC( DVSTEP ) };
"dvstep.u" { TOK_MNEMONIC( DVSTEP_U ) };
"enable" { TOK_MNEMONIC( ENABLE ) };
"eq" { TOK_MNEMONIC( EQ ) };
"eq.a" { TOK_MNEMONIC( EQ_A ) };
"eq.b" { TOK_MNEMONIC( EQ_B ) };
"eq.h" { TOK_MNEMONIC( EQ_H ) };
"eq.w" { TOK_MNEMONIC( EQ_W ) };
"eqany.b" { TOK_MNEMONIC( EQANY_B ) };
"eqany.h" { TOK_MNEMONIC( EQANY_H ) };
"eqz.a" { TOK_MNEMONIC( EQZ_A ) };
"extr" { TOK_MNEMONIC( EXTR ) };
"extr.u" { TOK_MNEMONIC( EXTR_U ) };
"ftoi" { TOK_MNEMONIC( FTOI ) };
"ftoq31" { TOK_MNEMONIC( FTOQ31 ) };
"ftou" { TOK_MNEMONIC( FTOU ) };
"ge" { TOK_MNEMONIC( GE ) };
"ge.a" { TOK_MNEMONIC( GE_A ) };
"ge.u" { TOK_MNEMONIC( GE_U ) };
"imask" { TOK_MNEMONIC( IMASK ) };
"ins.t" { TOK_MNEMONIC( INS_T ) };
"insert" { TOK_MNEMONIC( INSERT ) };
"insn.t" { TOK_MNEMONIC( INSN_T ) };
"isync" { TOK_MNEMONIC( ISYNC ) };
"itof" { TOK_MNEMONIC( ITOF ) };
"ixmax" { TOK_MNEMONIC( IXMAX ) };
"ixmax.u" { TOK_MNEMONIC( IXMAX_U ) };
"ixmin" { TOK_MNEMONIC( IXMIN ) };
"ixmin.u" { TOK_MNEMONIC( IXMIN_U ) };
"j" { TOK_MNEMONIC( J ) };
"ja" { TOK_MNEMONIC( JA ) };
"jeq" { TOK_MNEMONIC( JEQ ) };
"jeq.a" { TOK_MNEMONIC( JEQ_A ) };
"jge" { TOK_MNEMONIC( JGE ) };
"jge.u" { TOK_MNEMONIC( JGE_U ) };
"jgez" { TOK_MNEMONIC( JGEZ ) };
"jgtz" { TOK_MNEMONIC( JGTZ ) };
"ji" { TOK_MNEMONIC( JI ) };
"jl" { TOK_MNEMONIC( JL ) };
"jla" { TOK_MNEMONIC( JLA ) };
"jlez" { TOK_MNEMONIC( JLEZ ) };
"jli" { TOK_MNEMONIC( JLI ) };
"jlt" { TOK_MNEMONIC( JLT ) };
"jlt.u" { TOK_MNEMONIC( JLT_U ) };
"jltz" { TOK_MNEMONIC( JLTZ ) };
"jne" { TOK_MNEMONIC( JNE ) };
"jne.a" { TOK_MNEMONIC( JNE_A ) };
"jned" { TOK_MNEMONIC( JNED ) };
"jnei" { TOK_MNEMONIC( JNEI ) };
"jnz" { TOK_MNEMONIC( JNZ ) };
"jnz.a" { TOK_MNEMONIC( JNZ_A ) };
"jnz.t" { TOK_MNEMONIC( JNZ_T ) };
"jz" { TOK_MNEMONIC( JZ ) };
"jz.a" { TOK_MNEMONIC( JZ_A ) };
"jz.t" { TOK_MNEMONIC( JZ_T ) };
"ld.a" { TOK_MNEMONIC( LD_A ) };
"ld.b" { TOK_MNEMONIC( LD_B ) };
"ld.bu" { TOK_MNEMONIC( LD_BU ) };
"ld.d" { TOK_MNEMONIC( LD_D ) };
"ld.da" { TOK_MNEMONIC( LD_DA ) };
"ld.h" { TOK_MNEMONIC( LD_H ) };
"ld.hu" { TOK_MNEMONIC( LD_HU ) };
"ld.q" { TOK_MNEMONIC( LD_Q ) };
"ld.w" { TOK_MNEMONIC( LD_W ) };
"ldlcx" { TOK_MNEMONIC( LDLCX ) };
"ldmst" { TOK_MNEMONIC( LDMST ) };
"lducx" { TOK_MNEMONIC( LDUCX ) };
"lea" { TOK_MNEMONIC( LEA ) };
"loop" { TOK_MNEMONIC( LOOP ) };
"loopu" { TOK_MNEMONIC( LOOPU ) };
"lt" { TOK_MNEMONIC( LT ) };
"lt.a" { TOK_MNEMONIC( LT_A ) };
"lt.b" { TOK_MNEMONIC( LT_B ) };
"lt.bu" { TOK_MNEMONIC( LT_BU ) };
"lt.h" { TOK_MNEMONIC( LT_H ) };
"lt.hu" { TOK_MNEMONIC( LT_HU ) };
"lt.u" { TOK_MNEMONIC( LT_U ) };
"lt.w" { TOK_MNEMONIC( LT_W ) };
"lt.wu" { TOK_MNEMONIC( LT_WU ) };
"madd" { TOK_MNEMONIC( MADD ) };
"madd.f" { TOK_MNEMONIC( MADD_F ) };
"madd.h" { TOK_MNEMONIC( MADD_H ) };
"madd.q" { TOK_MNEMONIC( MADD_Q ) };
"madd.u" { TOK_MNEMONIC( MADD_U ) };
"maddm.h" { TOK_MNEMONIC( MADDM_H ) };
"maddms.h" { TOK_MNEMONIC( MADDMS_H ) };
"maddr.h" { TOK_MNEMONIC( MADDR_H ) };
"maddr.q" { TOK_MNEMONIC( MADDR_Q ) };
"maddrs.h" { TOK_MNEMONIC( MADDRS_H ) };
"maddrs.q" { TOK_MNEMONIC( MADDRS_Q ) };
"madds" { TOK_MNEMONIC( MADDS ) };
"madds.h" { TOK_MNEMONIC( MADDS_H ) };
"madds.q" { TOK_MNEMONIC( MADDS_Q ) };
"madds.u" { TOK_MNEMONIC( MADDS_U ) };
"maddsu.h" { TOK_MNEMONIC( MADDSU_H ) };
"maddsum.h" { TOK_MNEMONIC( MADDSUM_H ) };
"maddsums.h" { TOK_MNEMONIC( MADDSUMS_H ) };
"maddsur.h" { TOK_MNEMONIC( MADDSUR_H ) };
"maddsurs.h" { TOK_MNEMONIC( MADDSURS_H ) };
"maddsus.h" { TOK_MNEMONIC( MADDSUS_H ) };
"max" { TOK_MNEMONIC( MAX ) };
"max.b" { TOK_MNEMONIC( MAX_B ) };
"max.bu" { TOK_MNEMONIC( MAX_BU ) };
"max.h" { TOK_MNEMONIC( MAX_H ) };
"max.hu" { TOK_MNEMONIC( MAX_HU ) };
"max.u" { TOK_MNEMONIC( MAX_U ) };
"mfcr" { TOK_MNEMONIC( MFCR ) };
"min" { TOK_MNEMONIC( MIN ) };
"min.b" { TOK_MNEMONIC( MIN_B ) };
"min.bu" { TOK_MNEMONIC( MIN_BU ) };
"min.h" { TOK_MNEMONIC( MIN_H ) };
"min.hu" { TOK_MNEMONIC( MIN_HU ) };
"min.u" { TOK_MNEMONIC( MIN_U ) };
"mov" { TOK_MNEMONIC( MOV ) };
"mov.a" { TOK_MNEMONIC( MOV_A ) };
"mov.aa" { TOK_MNEMONIC( MOV_AA ) };
"mov.d" { TOK_MNEMONIC( MOV_D ) };
"mov.u" { TOK_MNEMONIC( MOV_U ) };
"movh" { TOK_MNEMONIC( MOVH ) };
"movh.a" { TOK_MNEMONIC( MOVH_A ) };
"msub" { TOK_MNEMONIC( MSUB ) };
"msub.f" { TOK_MNEMONIC( MSUB_F ) };
"msub.h" { TOK_MNEMONIC( MSUB_H ) };
"msub.q" { TOK_MNEMONIC( MSUB_Q ) };
"msub.u" { TOK_MNEMONIC( MSUB_U ) };
"msubad.h" { TOK_MNEMONIC( MSUBAD_H ) };
"msubadm.h" { TOK_MNEMONIC( MSUBADM_H ) };
"msubadms.h" { TOK_MNEMONIC( MSUBADMS_H ) };
"msubadr.h" { TOK_MNEMONIC( MSUBADR_H ) };
"msubadrs.h" { TOK_MNEMONIC( MSUBADRS_H ) };
"msubads.h" { TOK_MNEMONIC( MSUBADS_H ) };
"msubm.h" { TOK_MNEMONIC( MSUBM_H ) };
"msubms.h" { TOK_MNEMONIC( MSUBMS_H ) };
"msubr.h" { TOK_MNEMONIC( MSUBR_H ) };
"msubr.q" { TOK_MNEMONIC( MSUBR_Q ) };
"msubrs.h" { TOK_MNEMONIC( MSUBRS_H ) };
"msubrs.q" { TOK_MNEMONIC( MSUBRS_Q ) };
"msubs" { TOK_MNEMONIC( MSUBS ) };
"msubs.h" { TOK_MNEMONIC( MSUBS_H ) };
"msubs.q" { TOK_MNEMONIC( MSUBS_Q ) };
"msubs.u" { TOK_MNEMONIC( MSUBS_U ) };
"mtcr" { TOK_MNEMONIC( MTCR ) };
"mul" { TOK_MNEMONIC( MUL ) };
"mul.f" { TOK_MNEMONIC( MUL_F ) };
"mul.h" { TOK_MNEMONIC( MUL_H ) };
"mul.q" { TOK_MNEMONIC( MUL_Q ) };
"mul.u" { TOK_MNEMONIC( MUL_U ) };
"mulm.h" { TOK_MNEMONIC( MULM_H ) };
"mulr.h" { TOK_MNEMONIC( MULR_H ) };
"mulr.q" { TOK_MNEMONIC( MULR_Q ) };
"muls" { TOK_MNEMONIC( MULS ) };
"muls.u" { TOK_MNEMONIC( MULS_U ) };
"nand" { TOK_MNEMONIC( NAND ) };
"nand.t" { TOK_MNEMONIC( NAND_T ) };
"ne" { TOK_MNEMONIC( NE ) };
"ne.a" { TOK_MNEMONIC( NE_A ) };
"nez.a" { TOK_MNEMONIC( NEZ_A ) };
"nop" { TOK_MNEMONIC( NOP ) };
"nor" { TOK_MNEMONIC( NOR ) };
"nor.t" { TOK_MNEMONIC( NOR_T ) };
"not" { TOK_MNEMONIC( NOT ) };
"or" { TOK_MNEMONIC( OR ) };
"or.and.t" { TOK_MNEMONIC( OR_AND_T ) };
"or.andn.t" { TOK_MNEMONIC( OR_ANDN_T ) };
"or.eq" { TOK_MNEMONIC( OR_EQ ) };
"or.ge" { TOK_MNEMONIC( OR_GE ) };
"or.ge.u" { TOK_MNEMONIC( OR_GE_U ) };
"or.lt" { TOK_MNEMONIC( OR_LT ) };
"or.lt.u" { TOK_MNEMONIC( OR_LT_U ) };
"or.ne" { TOK_MNEMONIC( OR_NE ) };
"or.nor.t" { TOK_MNEMONIC( OR_NOR_T ) };
"or.or.t" { TOK_MNEMONIC( OR_OR_T ) };
"or.t" { TOK_MNEMONIC( OR_T ) };
"orn" { TOK_MNEMONIC( ORN ) };
"orn.t" { TOK_MNEMONIC( ORN_T ) };
"pack" { TOK_MNEMONIC( PACK ) };
"parity" { TOK_MNEMONIC( PARITY ) };
"q31tof" { TOK_MNEMONIC( Q31TOF ) };
"qseed.f" { TOK_MNEMONIC( QSEED_F ) };
"ret" { TOK_MNEMONIC( RET ) };
"rfe" { TOK_MNEMONIC( RFE ) };
"rfm" { TOK_MNEMONIC( RFM ) };
"rslcx" { TOK_MNEMONIC( RSLCX ) };
"rstv" { TOK_MNEMONIC( RSTV ) };
"rsub" { TOK_MNEMONIC( RSUB ) };
"rsubs" { TOK_MNEMONIC( RSUBS ) };
"rsubs.u" { TOK_MNEMONIC( RSUBS_U ) };
"sat.b" { TOK_MNEMONIC( SAT_B ) };
"sat.bu" { TOK_MNEMONIC( SAT_BU ) };
"sat.h" { TOK_MNEMONIC( SAT_H ) };
"sat.hu" { TOK_MNEMONIC( SAT_HU ) };
"sel" { TOK_MNEMONIC( SEL ) };
"seln" { TOK_MNEMONIC( SELN ) };
"sh" { TOK_MNEMONIC( SH ) };
"sh.and.t" { TOK_MNEMONIC( SH_AND_T ) };
"sh.andn.t" { TOK_MNEMONIC( SH_ANDN_T ) };
"sh.eq" { TOK_MNEMONIC( SH_EQ ) };
"sh.ge" { TOK_MNEMONIC( SH_GE ) };
"sh.ge.u" { TOK_MNEMONIC( SH_GE_U ) };
"sh.h" { TOK_MNEMONIC( SH_H ) };
"sh.lt" { TOK_MNEMONIC( SH_LT ) };
"sh.lt.u" { TOK_MNEMONIC( SH_LT_U ) };
"sh.nand.t" { TOK_MNEMONIC( SH_NAND_T ) };
"sh.ne" { TOK_MNEMONIC( SH_NE ) };
"sh.nor.t" { TOK_MNEMONIC( SH_NOR_T ) };
"sh.or.t" { TOK_MNEMONIC( SH_OR_T ) };
"sh.orn.t" { TOK_MNEMONIC( SH_ORN_T ) };
"sh.xnor.t" { TOK_MNEMONIC( SH_XNOR_T ) };
"sh.xor.t" { TOK_MNEMONIC( SH_XOR_T ) };
"sha" { TOK_MNEMONIC( SHA ) };
"sha.h" { TOK_MNEMONIC( SHA_H ) };
"shas" { TOK_MNEMONIC( SHAS ) };
"st.a" { TOK_MNEMONIC( ST_A ) };
"st.b" { TOK_MNEMONIC( ST_B ) };
"st.d" { TOK_MNEMONIC( ST_D ) };
"st.da" { TOK_MNEMONIC( ST_DA ) };
"st.h" { TOK_MNEMONIC( ST_H ) };
"st.q" { TOK_MNEMONIC( ST_Q ) };
"st.t" { TOK_MNEMONIC( ST_T ) };
"st.w" { TOK_MNEMONIC( ST_W ) };
"stlcx" { TOK_MNEMONIC( STLCX ) };
"stucx" { TOK_MNEMONIC( STUCX ) };
"sub" { TOK_MNEMONIC( SUB ) };
"sub.a" { TOK_MNEMONIC( SUB_A ) };
"sub.b" { TOK_MNEMONIC( SUB_B ) };
"sub.f" { TOK_MNEMONIC( SUB_F ) };
"sub.h" { TOK_MNEMONIC( SUB_H ) };
"subc" { TOK_MNEMONIC( SUBC ) };
"subs" { TOK_MNEMONIC( SUBS ) };
"subs.h" { TOK_MNEMONIC( SUBS_H ) };
"subs.hu" { TOK_MNEMONIC( SUBS_HU ) };
"subs.u" { TOK_MNEMONIC( SUBS_U ) };
"subx" { TOK_MNEMONIC( SUBX ) };
"svlcx" { TOK_MNEMONIC( SVLCX ) };
"swap.w" { TOK_MNEMONIC( SWAP_W ) };
"syscall" { TOK_MNEMONIC( SYSCALL ) };
"tlbdemap" { TOK_MNEMONIC( TLBDEMAP ) };
"tlbflush.a" { TOK_MNEMONIC( TLBFLUSH_A ) };
"tlbflush.b" { TOK_MNEMONIC( TLBFLUSH_B ) };
"tlbmap" { TOK_MNEMONIC( TLBMAP ) };
"tlbprobe.a" { TOK_MNEMONIC( TLBPROBE_A ) };
"tlbprobe.i" { TOK_MNEMONIC( TLBPROBE_I ) };
"trapsv" { TOK_MNEMONIC( TRAPSV ) };
"trapv" { TOK_MNEMONIC( TRAPV ) };
"unpack" { TOK_MNEMONIC( UNPACK ) };
"updfl" { TOK_MNEMONIC( UPDFL ) };
"utof" { TOK_MNEMONIC( UTOF ) };
"xnor" { TOK_MNEMONIC( XNOR ) };
"xnor.t" { TOK_MNEMONIC( XNOR_T ) };
"xor" { TOK_MNEMONIC( XOR ) };
"xor.eq" { TOK_MNEMONIC( XOR_EQ ) };
"xor.ge" { TOK_MNEMONIC( XOR_GE ) };
"xor.ge.u" { TOK_MNEMONIC( XOR_GE_U ) };
"xor.lt" { TOK_MNEMONIC( XOR_LT ) };
"xor.lt.u" { TOK_MNEMONIC( XOR_LT_U ) };
"xor.ne" { TOK_MNEMONIC( XOR_NE ) };
"xor.t" { TOK_MNEMONIC( XOR_T ) };

 /* TriCore 1.3.1 Mnemonics */
"cachei.w" { TOK_MNEMONIC( CACHEI_W ) };
"cachei.wi" { TOK_MNEMONIC( CACHEI_WI ) };
"ftoiz" { TOK_MNEMONIC( FTOIZ ) };
"ftoq31z" { TOK_MNEMONIC( FTOQ31Z ) };
"ftouz" { TOK_MNEMONIC( FTOUZ ) };

 /* Mnemonic prefixes */
".code16" {
  return( token::CODE16 );
};

".code32" {
  return( token::CODE32 );
};

 /* Registers */
"%a"(([0-9])|(1[1-4])) {
  int n = atoi( yytext + 2 );
  yylval->aReg = &(mContext.getARegP( n ));

  return( token::AREG );
};

"%a10" {
  yylval->aReg = &(mContext.getARegP( 10 ));

  return( token::AREG10 );
};

"%sp" {
  yylval->aReg = &(mContext.getARegP( 10 ));

  return( token::AREG10 );
};

"%a15" {
  yylval->aReg = &(mContext.getARegP( 15 ));

  return( token::AREG15 );
};

"%d"(([0-9])|(1[0-4])) {
  int n = atoi( yytext + 2 );
  yylval->dReg = &(mContext.getDRegP( n ));

  return( token::DREG );
};

"%d15" {
  yylval->dReg = &(mContext.getDRegP( 15 ));

  return( token::DREG15 );
};

"%d"(([0-9])|(1[0-5]))[lL] {
  int n = atoi( yytext + 2 );
  yylval->dReg = &(mContext.getDRegP( n ));

  return( token::DREGL );
};

"%d"(([0-9])|(1[0-5]))[lL]{2} {
  int n = atoi( yytext + 2 );
  yylval->dReg = &(mContext.getDRegP( n ));

  return( token::DREGLL );
};

"%d"(([0-9])|(1[0-5]))[lL][uU] {
  int n = atoi( yytext + 2 );
  yylval->dReg = &(mContext.getDRegP( n ));

  return( token::DREGLU );
};

"%d"(([0-9])|(1[0-5]))[uU] {
  int n = atoi( yytext + 2 );
  yylval->dReg = &(mContext.getDRegP( n ));

  return( token::DREGU );
};

"%d"(([0-9])|(1[0-5]))[uU][lL] {
  int n = atoi( yytext + 2 );
  yylval->dReg = &(mContext.getDRegP( n ));

  return( token::DREGUL );
};

"%d"(([0-9])|(1[0-5]))[uU]{2} {
  int n = atoi( yytext + 2 );
  yylval->dReg = &(mContext.getDRegP( n ));

  return( token::DREGUU );
};

"%e"([02468]|(10)|(12)|(14)) {
  int n = atoi( yytext + 2 );
  yylval->eReg = &(mContext.getERegP( n ));

  return( token::EREG );
};

 /*
   Core Special Function Register addresses (cf. TriCore Architecture Manual
   Vol 1, sections 3.2ff).
 */
"$pcxi" { yylval->uIntVal = 0xfe00; return( token::IMMEDIATE_U ); };
"$pcx" { yylval->uIntVal = 0xfe00; return( token::IMMEDIATE_U ); };
"$psw" { yylval->uIntVal = 0xfe04; return( token::IMMEDIATE_U ); };
"$pc" { yylval->uIntVal = 0xfe08; return( token::IMMEDIATE_U ); };
"$syscon" { yylval->uIntVal = 0xfe14; return( token::IMMEDIATE_U ); };
"$cpu_id" { yylval->uIntVal = 0xfe18; return( token::IMMEDIATE_U ); };
"$icr" { yylval->uIntVal = 0xfe2c; return( token::IMMEDIATE_U ); };
"$fcx" { yylval->uIntVal = 0xfe38; return( token::IMMEDIATE_U ); };
"$lcx" { yylval->uIntVal = 0xfe3c; return( token::IMMEDIATE_U ); };

 /* Addressing modes */
"+c" {
  return( token::CIRC );
};

"+r" {
  return( token::BREV );
};

"+" {
  return( token::PLUS );
};

"[" {
  return( token::LBRACKET );
};

"]" {
  return( token::RBRACKET );
};

"HI:" {
  return( token::HI );
};

"LO:" {
  return( token::LO );
};

 /* Template arguments */
"%%" {
  /*
    Actual registers' names should be prefixed with %%. In that case, we replace
    %% by % and continue.
  */
  if ( mHasTemplates )
    yyless( 1 );
};

"%"1?[0-9] {
  yylval->uIntVal = atoi( yytext + 1 );
  return( token::TPLARG );
};

"%"A1?[0-9] {
  yylval->uIntVal = atoi( yytext + 2 );
  return( token::TPLARGMODA );
};

"%"H1?[0-9] {
  yylval->uIntVal = atoi( yytext + 2 );
  return( token::TPLARGMODH );
};

"%"L1?[0-9] {
  yylval->uIntVal = atoi( yytext + 2 );
  return( token::TPLARGMODL );
};

"%"1?[0-9][lL] {
  yylval->uIntVal = atoi( yytext + 1 );
  return( token::TPLARGL );
};

"%"1?[0-9][lL][lL] {
  yylval->uIntVal = atoi( yytext + 1 );
  return( token::TPLARGLL );
};

"%"1?[0-9][lL][uU] {
  yylval->uIntVal = atoi( yytext + 1 );
  return( token::TPLARGLU );
};

"%"1?[0-9][uU] {
  yylval->uIntVal = atoi( yytext + 1 );
  return( token::TPLARGU );
};

"%"1?[0-9][uU][lL] {
  yylval->uIntVal = atoi( yytext + 1 );
  return( token::TPLARGUL );
};

"%"1?[0-9][uU][uU] {
  yylval->uIntVal = atoi( yytext + 1 );
  return( token::TPLARGUU );
};

 /* Immediate values (dec | hex | oct | bin) */
"-"?[1-9][0-9]* {
  stringstream stream;
  stream << dec << yytext;
  long long i;
  stream >> i;
  if ( i < 0 ) {
    yylval->intVal = i;
    return( token::IMMEDIATE_S );
  }

  yylval->uIntVal = i;
  return( token::IMMEDIATE_U );
};

"-"?0[xX][0-9A-Fa-f]{1,8} {
  stringstream stream;
  stream << hex << yytext;
  long long i;
  stream >> i;
  if ( i < 0 ) {
    yylval->intVal = i;
    return( token::IMMEDIATE_S );
  }
  yylval->uIntVal = i;
  return( token::IMMEDIATE_U );
};

"-"?0[1-7]* {
  stringstream stream;
  stream << oct << yytext;
  long long i;
  stream >> i;
  if ( i < 0 ) {
    yylval->intVal = i;
    return( token::IMMEDIATE_S );
  }
  yylval->uIntVal = i;
  return( token::IMMEDIATE_U );
};

0[Bb][1][01]* {
  yylval->intVal = 0;
  string bin( yytext );

  for ( auto it = bin.begin(); it != bin.end(); ++it ) {
    yylval->uIntVal <<= 1;
    if ( *it == '1' )
      yylval->intVal |= 1;
  }

  return( token::IMMEDIATE_U );
};

 /* String literals */
"."?[a-zA-Z][a-zA-Z0-9_]* {
  yylval->stringVal = strdup( yytext );
  return( token::STRING );
};

 /* Miscellaneous character tokens */
, {
  return( token::COMMA );
};

: {
  return( token::COLON );
};

; {
  return( token::SEMICOLON );
};

%%


/*
  C++ code section
*/

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor creating a TriCore assembly code scanner.
*/
TC_AsmLex::TC_AsmLex( TC_AsmContext &ctx, istream *arg_yyin, bool tpl,
                      ostream *arg_yyout ) :
  TC_AsmFlexLexer { arg_yyin, arg_yyout },
  mContext { ctx },
  mHasTemplates { tpl }
{
  DSTART( "TC_AsmLex::TC_AsmLex(TC_AsmContext&, istream*, bool, ostream*)" );
};


/*
  Destructor.
*/
TC_AsmLex::~TC_AsmLex( void )
{
  DSTART( "virtual TC_AsmLex::~TC_AsmLex()" );
};


/*
  setDebug (de-) activates debug output of the scanner if this is compiled into
  the scanner.
*/
void TC_AsmLex::setDebug( bool d )
{
  DSTART( "void TC_AsmLex::setDebug(bool)" );

  yy_flex_debug = d;
};

}       // namespace WIR


/*
  The following code serves to fill the vtable of TC_AsmFlexLexer::yylex() for
  class TC_AsmFlexLexer. We define the scanner's main yylex function via YY_DECL
  such that it resides in class TC_AsmLex instead.
*/

#ifdef yylex
#undef yylex
#endif

int TC_AsmFlexLexer::yylex()
{
  std::cerr << "In TC_AsmFlexLexer::yylex()!" << std::endl;
  return( 0 );
};


/*
  When the scanner receives an end-of-file indication from YY_INPUT, it then
  checks the yywrap() function. If yywrap() returns false (zero), then it is
  assumed that the function has gone ahead and set up `yyin' to point to another
  another input file, and scanning continues. If it returns true (non-zero),
  then the scanner terminates, returning 0 to its caller.
*/
int TC_AsmFlexLexer::yywrap()
{
  return( 1 );
};
