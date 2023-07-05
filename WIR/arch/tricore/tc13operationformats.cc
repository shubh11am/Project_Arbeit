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
  @file tc13operationformats.cc
  @brief This file declares the Infineon TriCore TC13's operation formats.

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
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

// Operation formats
const TC13::OperationFormat TC13::OperationFormat::A { 32, "A" };
const TC13::OperationFormat TC13::OperationFormat::AA { 32, "AA" };
const TC13::OperationFormat TC13::OperationFormat::AAA { 32, "AAA" };
const TC13::OperationFormat TC13::OperationFormat::AAC10BOA { 32, "AAC10BOA" };
const TC13::OperationFormat TC13::OperationFormat::AAC10PIA { 32, "AAC10PIA" };
const TC13::OperationFormat TC13::OperationFormat::AAC16 { 32, "AAC16" };
const TC13::OperationFormat TC13::OperationFormat::AAC16BOA { 32, "AAC16BOA" };
const TC13::OperationFormat TC13::OperationFormat::AAD { 32, "AAD" };
const TC13::OperationFormat TC13::OperationFormat::AADC2 { 32, "AADC2" };
const TC13::OperationFormat TC13::OperationFormat::AAL { 32, "AAL" };
const TC13::OperationFormat TC13::OperationFormat::AALC16BOA { 32, "AALC16BOA" };
const TC13::OperationFormat TC13::OperationFormat::AC10ABOA { 32, "AC10ABOA" };
const TC13::OperationFormat TC13::OperationFormat::AC10APIA { 32, "AC10APIA" };
const TC13::OperationFormat TC13::OperationFormat::AC10BOA { 32, "AC10BOA" };
const TC13::OperationFormat TC13::OperationFormat::AC10BOAPSW { 32, "AC10BOAPSW" };
const TC13::OperationFormat TC13::OperationFormat::AC10DBOA_1 { 32, "AC10DBOA_1" };
const TC13::OperationFormat TC13::OperationFormat::AC10DBOA_2 { 32, "AC10DBOA_2" };
const TC13::OperationFormat TC13::OperationFormat::AC10DPIA_1 { 32, "AC10DPIA_1" };
const TC13::OperationFormat TC13::OperationFormat::AC10DPIA_2 { 32, "AC10DPIA_2" };
const TC13::OperationFormat TC13::OperationFormat::AC10EBOA { 32, "AC10EBOA" };
const TC13::OperationFormat TC13::OperationFormat::AC10EPIA { 32, "AC10EPIA" };
const TC13::OperationFormat TC13::OperationFormat::AC10PBOA { 32, "AC10PBOA" };
const TC13::OperationFormat TC13::OperationFormat::AC10PIA { 32, "AC10PIA" };
const TC13::OperationFormat TC13::OperationFormat::AC10PPIA { 32, "AC10PPIA" };
const TC13::OperationFormat TC13::OperationFormat::AC16 { 32, "AC16" };
const TC13::OperationFormat TC13::OperationFormat::AC16DBOA { 32, "AC16DBOA" };
const TC13::OperationFormat TC13::OperationFormat::AC18ABSA { 32, "AC18ABSA" };
const TC13::OperationFormat TC13::OperationFormat::ALABSA { 32, "ALABSA" };
const TC13::OperationFormat TC13::OperationFormat::AD { 32, "AD" };
const TC13::OperationFormat TC13::OperationFormat::AL_1 { 32, "AL_1" };
const TC13::OperationFormat TC13::OperationFormat::AL_2 { 32, "AL_2" };
const TC13::OperationFormat TC13::OperationFormat::AL_3 { 32, "AL_3" };
const TC13::OperationFormat TC13::OperationFormat::ALC16DBOA { 32, "ALC16DBOA" };
const TC13::OperationFormat TC13::OperationFormat::APBRA { 32, "APBRA" };
const TC13::OperationFormat TC13::OperationFormat::APC10CA { 32, "APC10CA" };
const TC13::OperationFormat TC13::OperationFormat::C16DPSW { 32, "C16DPSW" };
const TC13::OperationFormat TC13::OperationFormat::C18AABSA { 32, "C18AABSA" };
const TC13::OperationFormat TC13::OperationFormat::C18ABSA { 32, "C18ABSA" };
const TC13::OperationFormat TC13::OperationFormat::C18ABSAPSW { 32, "C18ABSAPSW" };
const TC13::OperationFormat TC13::OperationFormat::C18C3C1 { 32, "C18C3C1" };
const TC13::OperationFormat TC13::OperationFormat::C18DABSA_1 { 32, "C18DABSA_1" };
const TC13::OperationFormat TC13::OperationFormat::C18DABSA_2 { 32, "C18DABSA_2" };
const TC13::OperationFormat TC13::OperationFormat::C18EABSA { 32, "C18EABSA" };
const TC13::OperationFormat TC13::OperationFormat::C18PABSA { 32, "C18PABSA" };
const TC13::OperationFormat TC13::OperationFormat::C9 { 32, "C9" };
const TC13::OperationFormat TC13::OperationFormat::C9PSW { 32, "C9PSW" };
const TC13::OperationFormat TC13::OperationFormat::D { 32, "D" };
const TC13::OperationFormat TC13::OperationFormat::DA { 32, "DA" };
const TC13::OperationFormat TC13::OperationFormat::DAA { 32, "DAA" };
const TC13::OperationFormat TC13::OperationFormat::DAC10BOA { 32, "DAC10BOA" };
const TC13::OperationFormat TC13::OperationFormat::DAC10PIA { 32, "DAC10PIA" };
const TC13::OperationFormat TC13::OperationFormat::DAC16BOA { 32, "DAC16BOA" };
const TC13::OperationFormat TC13::OperationFormat::DALC16BOA { 32, "DALC16BOA" };
const TC13::OperationFormat TC13::OperationFormat::DC16_1 { 32, "DC16_1" };
const TC13::OperationFormat TC13::OperationFormat::DC16_2 { 32, "DC16_2" };
const TC13::OperationFormat TC13::OperationFormat::DC16PSW { 32, "DC16PSW" };
const TC13::OperationFormat TC13::OperationFormat::DC18ABSA { 32, "DC18ABSA" };
const TC13::OperationFormat TC13::OperationFormat::DC4L_1 { 32, "DC4L_1" };
const TC13::OperationFormat TC13::OperationFormat::DC4L_2 { 32, "DC4L_2" };
const TC13::OperationFormat TC13::OperationFormat::DC4L_3 { 32, "DC4L_3" };
const TC13::OperationFormat TC13::OperationFormat::DC5L { 32, "DC5L" };
const TC13::OperationFormat TC13::OperationFormat::DD { 32, "DD" };
const TC13::OperationFormat TC13::OperationFormat::DDC16_1 { 32, "DDC16_1" };
const TC13::OperationFormat TC13::OperationFormat::DDC16_2 { 32, "DDC16_2" };
const TC13::OperationFormat TC13::OperationFormat::DDC4C5C5 { 32, "DDC4C5C5" };
const TC13::OperationFormat TC13::OperationFormat::DDC4DC5 { 32, "DDC4DC5" };
const TC13::OperationFormat TC13::OperationFormat::DDC4E { 32, "DDC4E" };
const TC13::OperationFormat TC13::OperationFormat::DDC5C5 { 32, "DDC5C5" };
const TC13::OperationFormat TC13::OperationFormat::DDC5DC5_1 { 32, "DDC5DC5_1" };
const TC13::OperationFormat TC13::OperationFormat::DDC5DC5_2 { 32, "DDC5DC5_2" };
const TC13::OperationFormat TC13::OperationFormat::DDC9_1 { 32, "DDC9_1" };
const TC13::OperationFormat TC13::OperationFormat::DDC9_2 { 32, "DDC9_2" };
const TC13::OperationFormat TC13::OperationFormat::DDC9_3 { 32, "DDC9_3" };
const TC13::OperationFormat TC13::OperationFormat::DDC9_4 { 32, "DDC9_4" };
const TC13::OperationFormat TC13::OperationFormat::DDC9PSW_1 { 32, "DDC9PSW_1" };
const TC13::OperationFormat TC13::OperationFormat::DDC9PSW_2 { 32, "DDC9PSW_2" };
const TC13::OperationFormat TC13::OperationFormat::DDD_1 { 32, "DDD_1" };
const TC13::OperationFormat TC13::OperationFormat::DDD_2 { 32, "DDD_2" };
const TC13::OperationFormat TC13::OperationFormat::DDDC1_1 { 32, "DDDC1_1" };
const TC13::OperationFormat TC13::OperationFormat::DDDC1_2 { 32, "DDDC1_2" };
const TC13::OperationFormat TC13::OperationFormat::DDDC1_3 { 32, "DDDC1_3" };
const TC13::OperationFormat TC13::OperationFormat::DDDC1_4 { 32, "DDDC1_4" };
const TC13::OperationFormat TC13::OperationFormat::DDDC1_5 { 32, "DDDC1_5" };
const TC13::OperationFormat TC13::OperationFormat::DDDC1_6 { 32, "DDDC1_6" };
const TC13::OperationFormat TC13::OperationFormat::DDDC1_7 { 32, "DDDC1_7" };
const TC13::OperationFormat TC13::OperationFormat::DDDC1_8 { 32, "DDDC1_8" };
const TC13::OperationFormat TC13::OperationFormat::DDDC1_9 { 32, "DDDC1_9" };
const TC13::OperationFormat TC13::OperationFormat::DDDC5 { 32, "DDDC5" };
const TC13::OperationFormat TC13::OperationFormat::DDDC5C5 { 32, "DDDC5C5" };
const TC13::OperationFormat TC13::OperationFormat::DDDC9_1 { 32, "DDDC9_1" };
const TC13::OperationFormat TC13::OperationFormat::DDDC9_2 { 32, "DDDC9_2" };
const TC13::OperationFormat TC13::OperationFormat::DDDD { 32, "DDDD" };
const TC13::OperationFormat TC13::OperationFormat::DDDDC1_1 { 32, "DDDDC1_1" };
const TC13::OperationFormat TC13::OperationFormat::DDDDC1_2 { 32, "DDDDC1_2" };
const TC13::OperationFormat TC13::OperationFormat::DDDDC1_3 { 32, "DDDDC1_3" };
const TC13::OperationFormat TC13::OperationFormat::DDDDC1_4 { 32, "DDDDC1_4" };
const TC13::OperationFormat TC13::OperationFormat::DDDDC1_5 { 32, "DDDDC1_5" };
const TC13::OperationFormat TC13::OperationFormat::DDDDC1_6 { 32, "DDDDC1_6" };
const TC13::OperationFormat TC13::OperationFormat::DDDDC1_7 { 32, "DDDDC1_7" };
const TC13::OperationFormat TC13::OperationFormat::DDDDC1_8 { 32, "DDDDC1_8" };
const TC13::OperationFormat TC13::OperationFormat::DDDDC1_9 { 32, "DDDDC1_9" };
const TC13::OperationFormat TC13::OperationFormat::DDDDC5 { 32, "DDDDC5" };
const TC13::OperationFormat TC13::OperationFormat::DDDE { 32, "DDDE" };
const TC13::OperationFormat TC13::OperationFormat::DDDPSW_1 { 32, "DDDPSW_1" };
const TC13::OperationFormat TC13::OperationFormat::DDDPSW_2 { 32, "DDDPSW_2" };
const TC13::OperationFormat TC13::OperationFormat::DDE { 32, "DDE" };
const TC13::OperationFormat TC13::OperationFormat::DDL_1 { 32, "DDL_1" };
const TC13::OperationFormat TC13::OperationFormat::DDL_2 { 32, "DDL_2" };
const TC13::OperationFormat TC13::OperationFormat::DEDPSW { 32, "DEDPSW" };
const TC13::OperationFormat TC13::OperationFormat::DEDDC1 { 32, "DEDDC1" };
const TC13::OperationFormat TC13::OperationFormat::DLABSA { 32, "DLABSA" };
const TC13::OperationFormat TC13::OperationFormat::DPBRA { 32, "DPBRA" };
const TC13::OperationFormat TC13::OperationFormat::DPC10CA { 32, "DPC10CA" };
const TC13::OperationFormat TC13::OperationFormat::E { 32, "E" };
const TC13::OperationFormat TC13::OperationFormat::EAC10BOA { 32, "EAC10BOA" };
const TC13::OperationFormat TC13::OperationFormat::EAC10PIA { 32, "EAC10PIA" };
const TC13::OperationFormat TC13::OperationFormat::EC18ABSA { 32, "EC18ABSA" };
const TC13::OperationFormat TC13::OperationFormat::EC4C5C5 { 32, "EC4C5C5" };
const TC13::OperationFormat TC13::OperationFormat::EC4DC5 { 32, "EC4DC5" };
const TC13::OperationFormat TC13::OperationFormat::ED { 32, "ED" };
const TC13::OperationFormat TC13::OperationFormat::EDC5C5 { 32, "EDC5C5" };
const TC13::OperationFormat TC13::OperationFormat::EDC9_1 { 32, "EDC9_1" };
const TC13::OperationFormat TC13::OperationFormat::EDC9_2 { 32, "EDC9_2" };
const TC13::OperationFormat TC13::OperationFormat::EDD { 32, "EDD" };
const TC13::OperationFormat TC13::OperationFormat::EDDC1_1 { 32, "EDDC1_1" };
const TC13::OperationFormat TC13::OperationFormat::EDDC1_2 { 32, "EDDC1_2" };
const TC13::OperationFormat TC13::OperationFormat::EDDC1_3 { 32, "EDDC1_3" };
const TC13::OperationFormat TC13::OperationFormat::EDDC1_4 { 32, "EDDC1_4" };
const TC13::OperationFormat TC13::OperationFormat::EDDC1_5 { 32, "EDDC1_5" };
const TC13::OperationFormat TC13::OperationFormat::EDDC1_6 { 32, "EDDC1_6" };
const TC13::OperationFormat TC13::OperationFormat::EDDC1_7 { 32, "EDDC1_7" };
const TC13::OperationFormat TC13::OperationFormat::EDDC5 { 32, "EDDC5" };
const TC13::OperationFormat TC13::OperationFormat::EED { 32, "EED" };
const TC13::OperationFormat TC13::OperationFormat::EEDC9_1 { 32, "EEDC9_1" };
const TC13::OperationFormat TC13::OperationFormat::EEDC9_2 { 32, "EEDC9_2" };
const TC13::OperationFormat TC13::OperationFormat::EEDD { 32, "EEDD" };
const TC13::OperationFormat TC13::OperationFormat::EEDDC1_1 { 32, "EEDDC1_1" };
const TC13::OperationFormat TC13::OperationFormat::EEDDC1_2 { 32, "EEDDC1_2" };
const TC13::OperationFormat TC13::OperationFormat::EEDDC1_3 { 32, "EEDDC1_3" };
const TC13::OperationFormat TC13::OperationFormat::EEDDC1_4 { 32, "EEDDC1_4" };
const TC13::OperationFormat TC13::OperationFormat::EEDDC1_5 { 32, "EEDDC1_5" };
const TC13::OperationFormat TC13::OperationFormat::EEDDC1_6 { 32, "EEDDC1_6" };
const TC13::OperationFormat TC13::OperationFormat::EEDDC1_7 { 32, "EEDDC1_7" };
const TC13::OperationFormat TC13::OperationFormat::EEDDC1_8 { 32, "EEDDC1_8" };
const TC13::OperationFormat TC13::OperationFormat::EEDDC1_9 { 32, "EEDDC1_9" };
const TC13::OperationFormat TC13::OperationFormat::ELABSA { 32, "ELABSA" };
const TC13::OperationFormat TC13::OperationFormat::EPBRA { 32, "EPBRA" };
const TC13::OperationFormat TC13::OperationFormat::EPC10CA { 32, "EPC10CA" };
const TC13::OperationFormat TC13::OperationFormat::L { 32, "L" };
const TC13::OperationFormat TC13::OperationFormat::LAABSA { 32, "LAABSA" };
const TC13::OperationFormat TC13::OperationFormat::LABSA { 32, "LABSA" };
const TC13::OperationFormat TC13::OperationFormat::LABSAPSW { 32, "LABSAPSW" };
const TC13::OperationFormat TC13::OperationFormat::LDABSA_1 { 32, "LDABSA_1" };
const TC13::OperationFormat TC13::OperationFormat::LDABSA_2 { 32, "LDABSA_2" };
const TC13::OperationFormat TC13::OperationFormat::LEABSA { 32, "LEABSA" };
const TC13::OperationFormat TC13::OperationFormat::LPABSA { 32, "LPABSA" };
const TC13::OperationFormat TC13::OperationFormat::PABRA { 32, "PABRA" };
const TC13::OperationFormat TC13::OperationFormat::PAC10BOA { 32, "PAC10BOA" };
const TC13::OperationFormat TC13::OperationFormat::PAC10PIA { 32, "PAC10PIA" };
const TC13::OperationFormat TC13::OperationFormat::PBRA { 32, "PBRA" };
const TC13::OperationFormat TC13::OperationFormat::PC10ACA { 32, "PC10ACA" };
const TC13::OperationFormat TC13::OperationFormat::PC10CA { 32, "PC10CA" };
const TC13::OperationFormat TC13::OperationFormat::PC10DCA_1 { 32, "PC10DCA_1" };
const TC13::OperationFormat TC13::OperationFormat::PC10DCA_2 { 32, "PC10DCA_2" };
const TC13::OperationFormat TC13::OperationFormat::PC10ECA { 32, "PC10ECA" };
const TC13::OperationFormat TC13::OperationFormat::PC10PCA { 32, "PC10PCA" };
const TC13::OperationFormat TC13::OperationFormat::PC18ABSA { 32, "PC18ABSA" };
const TC13::OperationFormat TC13::OperationFormat::PDBRA_1 { 32, "PDBRA_1" };
const TC13::OperationFormat TC13::OperationFormat::PDBRA_2 { 32, "PDBRA_2" };
const TC13::OperationFormat TC13::OperationFormat::PEBRA { 32, "PEBRA" };
const TC13::OperationFormat TC13::OperationFormat::PLABSA { 32, "PLABSA" };
const TC13::OperationFormat TC13::OperationFormat::PPBRA_1 { 32, "PPBRA_1" };
const TC13::OperationFormat TC13::OperationFormat::PPBRA_2 { 32, "PPBRA_2" };
const TC13::OperationFormat TC13::OperationFormat::PPC10CA { 32, "PPC10CA" };
const TC13::OperationFormat TC13::OperationFormat::PSW { 32, "PSW" };
const TC13::OperationFormat TC13::OperationFormat::S { 16, "S" };
const TC13::OperationFormat TC13::OperationFormat::SA { 16, "SA" };
const TC13::OperationFormat TC13::OperationFormat::SAA_1 { 16, "SAA_1" };
const TC13::OperationFormat TC13::OperationFormat::SAA_2 { 16, "SAA_2" };
const TC13::OperationFormat TC13::OperationFormat::SAA_3 { 16, "SAA_3" };
const TC13::OperationFormat TC13::OperationFormat::SAA_4 { 16, "SAA_4" };
const TC13::OperationFormat TC13::OperationFormat::SAA_5 { 16, "SAA_5" };
const TC13::OperationFormat TC13::OperationFormat::SAA_6 { 16, "SAA_6" };
const TC13::OperationFormat TC13::OperationFormat::SAAIC2 { 16, "SAAIC2" };
const TC13::OperationFormat TC13::OperationFormat::SAC4_1 { 16, "SAC4_1" };
const TC13::OperationFormat TC13::OperationFormat::SAC4_2 { 16, "SAC4_2" };
const TC13::OperationFormat TC13::OperationFormat::SAC4I_1 { 16, "SAC4I_1" };
const TC13::OperationFormat TC13::OperationFormat::SAC4I_2 { 16, "SAC4I_2" };
const TC13::OperationFormat TC13::OperationFormat::SAD_1 { 16, "SAD_1" };
const TC13::OperationFormat TC13::OperationFormat::SAD_2 { 16, "SAD_2" };
const TC13::OperationFormat TC13::OperationFormat::SAD_3 { 16, "SAD_3" };
const TC13::OperationFormat TC13::OperationFormat::SAIC4 { 16, "SAIC4" };
const TC13::OperationFormat TC13::OperationFormat::SAL_1 { 16, "SAL_1" };
const TC13::OperationFormat TC13::OperationFormat::SAL_2 { 16, "SAL_2" };
const TC13::OperationFormat TC13::OperationFormat::SC8 { 16, "SC8" };
const TC13::OperationFormat TC13::OperationFormat::SD { 16, "SD" };
const TC13::OperationFormat TC13::OperationFormat::SDA_1 { 16, "SDA_1" };
const TC13::OperationFormat TC13::OperationFormat::SDA_2 { 16, "SDA_2" };
const TC13::OperationFormat TC13::OperationFormat::SDA_3 { 16, "SDA_3" };
const TC13::OperationFormat TC13::OperationFormat::SDC4_1 { 16, "SDC4_1" };
const TC13::OperationFormat TC13::OperationFormat::SDC4_2 { 16, "SDC4_2" };
const TC13::OperationFormat TC13::OperationFormat::SDC4PSW { 16, "SDC4PSW" };
const TC13::OperationFormat TC13::OperationFormat::SDD_1 { 16, "SDD_1" };
const TC13::OperationFormat TC13::OperationFormat::SDD_2 { 16, "SDD_2" };
const TC13::OperationFormat TC13::OperationFormat::SDIC4_1 { 16, "SDIC4_1" };
const TC13::OperationFormat TC13::OperationFormat::SDIC4_2 { 16, "SDIC4_2" };
const TC13::OperationFormat TC13::OperationFormat::SDIC4_3 { 16, "SDIC4_3" };
const TC13::OperationFormat TC13::OperationFormat::SDID_1 { 16, "SDID_1" };
const TC13::OperationFormat TC13::OperationFormat::SDID_2 { 16, "SDID_2" };
const TC13::OperationFormat TC13::OperationFormat::SDL { 16, "SDL" };
const TC13::OperationFormat TC13::OperationFormat::SIAC4_1 { 16, "SIAC4_1" };
const TC13::OperationFormat TC13::OperationFormat::SIAC4_2 { 16, "SIAC4_2" };
const TC13::OperationFormat TC13::OperationFormat::SIC4A { 16, "SIC4A" };
const TC13::OperationFormat TC13::OperationFormat::SIC4D { 16, "SIC4D" };
const TC13::OperationFormat TC13::OperationFormat::SIC4L { 16, "SIC4L" };
const TC13::OperationFormat TC13::OperationFormat::SIC5L { 16, "SIC5L" };
const TC13::OperationFormat TC13::OperationFormat::SIC8_1 { 16, "SIC8_1" };
const TC13::OperationFormat TC13::OperationFormat::SIC8_2 { 16, "SIC8_2" };
const TC13::OperationFormat TC13::OperationFormat::SIDC4 { 16, "SIDC4" };
const TC13::OperationFormat TC13::OperationFormat::SIDD { 16, "SIDD" };
const TC13::OperationFormat TC13::OperationFormat::SIDL { 16, "SIDL" };
const TC13::OperationFormat TC13::OperationFormat::SIL { 16, "SIL" };
const TC13::OperationFormat TC13::OperationFormat::SISPC10_1 { 16, "SISPC10_1" };
const TC13::OperationFormat TC13::OperationFormat::SISPC10_2 { 16, "SISPC10_2" };
const TC13::OperationFormat TC13::OperationFormat::SL { 16, "SL" };
const TC13::OperationFormat TC13::OperationFormat::SPSW { 16, "SPSW" };
const TC13::OperationFormat TC13::OperationFormat::SSPC8 { 16, "SSPC8" };
const TC13::OperationFormat TC13::OperationFormat::SSPC10I_1 { 16, "SSPC10I_1" };
const TC13::OperationFormat TC13::OperationFormat::SSPC10I_2 { 16, "SSPC10I_2" };
const TC13::OperationFormat TC13::OperationFormat::SYS { 32, "SYS" };

}       // namespace WIR
