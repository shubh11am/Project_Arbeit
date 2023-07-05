/*

   This header file belongs to the

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
  @file tcconstprop.h
  @brief This file provides the interface of a TriCore-specific constant
         propagation optimization.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_CONSTPROP_H
#define _TC_CONSTPROP_H


//
// Include section
//

// Include standard headers
#include <list>
#include <map>

// Include WIR headers
#include <wir/wirtypes.h>
#include <optimizations/constprop/wirconstprop.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_CompilationUnit;
class WIR_Function;
class WIR_Instruction;
class WIR_Operation;
class WIR_RegisterParameter;
class WIR_Symbol;
class WIR_System;
class WIR_UpDownValue;


/*!
  @brief Class TC_ConstProp is a TriCore-specific optimization that propagates
         constants in %WIR functions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_ConstProp final : public WIR_ConstProp
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for system-level optimization.

      @param[in] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_ConstProp( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_ConstProp( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_ConstProp( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_ConstProp( void );


  protected:

    /*!
      @brief runOptimization propagates constants in the given function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );

    /*!
      @brief doConstProp does the actual TriCore-specific propagation of
             constants for a given operation.

      @param[in] o A const reference to a %WIR operation.
      @param[in] inValue A const reference to a map mapping all defined or
                         def-used register parameters to their outgoing bit
                         value.
      @return A Boolean denoting whether new instructions were produced for o or
              not.

      doConstProp does not actually modify the currently examined %WIR operation
      o. Instead, new instructions realizing the constant folding of o are added
      to map mNewInstructions.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool doConstProp( const WIR_Operation &,
                              const std::map<WIR_id_t, WIR_UpDownValue> & );


  private:

    /*!
      @brief prop_DDD_DDC9 propagates constants for a TriCore operation of
             format DDD_1 or DDD_2.

      @param[in] o A const reference to an operation of format DDD_1 or DDD_2
                   whose last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_1 or DDC9_3, resp.

      If the given up value does not fit into a 9 bits signed constant,
      prop_DDD_DDC9 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDD_DDC9( const WIR_Operation &,
                                              const WIR_UpDownValue & );

    /*!
      @brief prop_DDD_DDC9_2 propagates constants for a commutative TriCore
             operation of format DDD_1 or DDD_2.

      @param[in] o A const reference to an operation of format DDD_1 or DDD_2
                   whose second explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's second
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_1 or DDC9_3, resp.

      If the given up value does not fit into a 9 bits signed constant,
      prop_DDD_DDC9_2 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDD_DDC9_2( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_DDD_DDC9_3 propagates constants for the commutative TriCore
             operations *GE and *LT of format DDD_1 or DDD_2.

      @param[in] o A const reference to a *GE or *LT operation of format DDD_1
                   or DDD_2 whose second explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's second
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_1 or DDC9_3, resp.

      For *GE Dc, "const Da", Db, prop_DDD_DDC9_3 returns
      *LT Dc, Db, "const Da+1".
      For *LT Dc, "const Da", Db, prop_DDD_DDC9_3 returns
      *GE Dc, Db, "const Da+1".
      If the given up value plus one does not fit into a 9 bits signed constant,
      prop_DDD_DDC9_3 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDD_DDC9_3( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_DDD_DDC9_4 propagates constants for a TriCore SUB or SUBS
             operation of format DDD_1.

      @param[in] o A const reference to a SUB/SUBS operation of format DDD_1
                   whose last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_1.

      For SUB* Dc, Da, "const Db", prop_DDD_DDC9_4 returns
      ADD* Dc, Da, -1*"const Db".
      If the inverse of the given up value does not fit into a 9 bits signed
      constant, prop_DDD_DDC9_4 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDD_DDC9_4( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_DDD_DDC9_5 propagates constants for the commutative TriCore
             operations SUB or SUBS of format DDD_1.

      @param[in] o A const reference to a SUB/SUBS operation of format DDD_1
                   whose second explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's second
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_1.

      For SUB* Dc, "const Da", Db, prop_DDD_DDC9_5 returns
      RSUB* Dc, Db, "const Da".
      If the given up value does not fit into a 9 bits signed constant,
      prop_DDD_DDC9_5 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDD_DDC9_5( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_DDD_DDC9_6 propagates constants for a TriCore operation of
             format DDDPSW_1 or DDDPSW_2.

      @param[in] o A const reference to an operation of format DDDPSW_1 or
                   DDDPSW_2 whose last data register parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   data register parameter.
      @return A list of TriCore instructions with format DDC9PSW_1 or DDC9PSW_2,
              resp.

      If the given up value does not fit into a 9 bits signed constant,
      prop_DDD_DDC9_6 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDD_DDC9_6( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_DDD_DDC9_7 propagates constants for a commutative TriCore
             operation of format DDDPSW_1 or DDDPSW_2.

      @param[in] o A const reference to an operation of format DDDPSW_1 or
                   DDDPSW_2 whose second explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's second
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9PSW_1 or DDC9PSW_2,
              resp.

      If the given up value does not fit into a 9 bits signed constant,
      prop_DDD_DDC9_7 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDD_DDC9_7( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_DDD_DDC16 propagates constants for a TriCore ADD operation of
             format DDD_1.

      @param[in] o A const reference to an ADD operation of format DDD_1 whose
                   last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore ADDI instructions with format DDC16_1.

      If the given up value does not fit into a 16 bits signed constant,
      prop_DDD_DDC16 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDD_DDC16( const WIR_Operation &,
                                               const WIR_UpDownValue & );

    /*!
      @brief prop_DDD_DDC16_1 propagates constants for the commutative TriCore
             ADD operation of format DDD_1.

      @param[in] o A const reference to an ADD operation of format DDD_1 whose
                   second explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's second
                   explicit parameter.
      @return A list of TriCore ADDI instructions with format DDC16_1.

      If the given up value does not fit into a 16 bits signed constant,
      prop_DDD_DDC16 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDD_DDC16_1( const WIR_Operation &,
                                                 const WIR_UpDownValue & );

    /*!
      @brief prop_xxx_xxC16 propagates constants for a TriCore ADD/ADD.A
             operation of formats DDD_1/AAA.

      @param[in] o A const reference to an ADD/ADD.A operation of format DDD_1
                   or AAA whose last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore ADDIH/ADDIH.A instructions with formats DDC16_2
              or AAC16, resp.

      If the given up value does not fit into the shape expected by
      ADDIH/ADDIH.A, prop_xxx_xxC16 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_xxx_xxC16( const WIR_Operation &,
                                               const WIR_UpDownValue & );

    /*!
      @brief prop_xxx_xxC16_1 propagates constants for a commutative TriCore
             ADD/ADD.A operation of formats DDD_1/AAA.

      @param[in] o A const reference to an ADD/ADD.A operation of format DDD_1
                   or AAA whose second explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's second
                   explicit parameter.
      @return A list of TriCore ADDIH/ADDIH.A instructions with formats DDC16_2
              or AAC16, resp.

      If the given up value does not fit into the shape expected by
      ADDIH/ADDIH.A, prop_xxx_xxC16_1 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_xxx_xxC16_1( const WIR_Operation &,
                                                 const WIR_UpDownValue & );

    /*!
      @brief prop_xDD_DDC9_u propagates constants for an unsigned TriCore
             operation of format DDD_1 or EDD.

      @param[in] o A const reference to an operation of format DDD_1 or EDD
                   whose last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_2 or EDC9_2.

      If the given up value does not fit into a 9 bits unsigned constant,
      prop_xDD_DDC9_u returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_xDD_DDC9_u( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_xDD_DDC9_2u propagates constants for a commutative unsigned
             TriCore operation of format DDD_1 or EDD.

      @param[in] o A const reference to an operation of format DDD_1 or EDD
                   whose second explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's second
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_2 or EDC9_2.

      If the given up value does not fit into a 9 bits unsigned constant,
      prop_xDD_DDC9_2u returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_xDD_DDC9_2u( const WIR_Operation &,
                                                 const WIR_UpDownValue & );

    /*!
      @brief prop_DDD_DDC9_3u propagates constants for the commutative unsigned
             TriCore operations *GE.U and *LT_U of format DDD_1 or DDD_2.

      @param[in] o A const reference to a *GE.U or *LT.U operation of format
                   DDD_1 or DDD_2 whose second explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's second
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_2 or DDC9_4, resp.

      For GE.U Dc, "const Da", Db, prop_DDD_DDC9_3u returns
      LT.U Dc, Db, "const Da+1".
      For LT.U Dc, "const Da", Db, prop_DDD_DDC9_3u returns
      GE.U Dc, Db, "const Da+1".
      If the given up value plus one does not fit into a 9 bits unsigned
      constant, prop_DDD_DDC9_3u returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDD_DDC9_3u( const WIR_Operation &,
                                                 const WIR_UpDownValue & );

    /*!
      @brief prop_xxDD_xxDC9 propagates constants for a TriCore operation of
             format DDDD or EEDD.

      @param[in] o A const reference to an operation of format DDDD or EEDD
                   whose last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DDDC9_1 or EEDC9_1,
              resp.

      If the given up value does not fit into a 9 bits signed constant,
      prop_xxDD_xxDC9 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_xxDD_xxDC9( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_xxDD_xxDC9_2 propagates constants for a comutative TriCore
             operation of format DDDD or EEDD.

      @param[in] o A const reference to an operation of format DDDD or EEDD
                   whose third explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's third
                   explicit parameter.
      @return A list of TriCore instructions with format DDDC9_1 or EEDC9_1,
              resp.

      If the given up value does not fit into a 9 bits signed constant,
      prop_xxDD_xxDC9 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_xxDD_xxDC9_2( const WIR_Operation &,
                                                  const WIR_UpDownValue & );

    /*!
      @brief prop_CSUB propagates constants for TriCore CSUB/CSUBN operation of
             format DDDD.

      @param[in] o A const reference to an operation of format DDDD whose last
                   explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore CADD* instructions with format DDDC9_1.

      For CSUB* Dc, Dd, Da, "const Db", prop_CSUB returns
      CADD* Dc, Dd, Da, -1*"const Db".
      If the inverse of the given up value does not fit into a 9 bits signed
      constant, prop_CSUB returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_CSUB( const WIR_Operation &,
                                          const WIR_UpDownValue & );

    /*!
      @brief prop_xxDD_xxDC9_u propagates constants for an unsigned TriCore
             operation of format DDDD or EEDD.

      @param[in] o A const reference to an operation of format DDDD or EEDD
                   whose last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DDDC9_2 or EEDC9_2,
              resp.

      If the given up value does not fit into a 9 bits unsigned constant,
      prop_xxDD_xxDC9_u returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_xxDD_xxDC9_u( const WIR_Operation &,
                                                  const WIR_UpDownValue & );

    /*!
      @brief prop_xxDD_xxDC9_2u propagates constants for a comutative unsigned
             TriCore operation of format DDDD or EEDD.

      @param[in] o A const reference to an operation of format DDDD or EEDD
                   whose third explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's third
                   explicit parameter.
      @return A list of TriCore instructions with format DDDC9_2 or EEDC9_2,
              resp.

      If the given up value does not fit into a 9 bits signed constant,
      prop_xxDD_xxDC9_2u returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_xxDD_xxDC9_2u( const WIR_Operation &,
                                                   const WIR_UpDownValue & );

    /*!
      @brief prop_Sxx_SxC4 propagates constants for a TriCore operation of
             format SAA_5 or SDD_2.

      @param[in] o A const reference to an operation of format SAA_5 or SDD_2
                   whose last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format SAC4_2 or SDC4_2, resp.

      If the given up value does not fit into a 4 bits signed constant,
      prop_Sxx_SxC4 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_Sxx_SxC4( const WIR_Operation &,
                                              const WIR_UpDownValue & );

    /*!
      @brief prop_SDD_DDC9 propagates constants for a TriCore operation of
             format SDD_2.

      @param[in] o A const reference to an operation of format SDD_2 whose last
                   explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_1.

      If the given up value does not fit into a 9 bits signed constant,
      prop_SDD_DDC9 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_SDD_DDC9( const WIR_Operation &,
                                              const WIR_UpDownValue & );

    /*!
      @brief prop_SDD_DDC9_2 propagates constants for a commutative TriCore
             operation of format SDD_2.

      @param[in] o A const reference to an operation of format SDD_2 whose first
                   explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's first
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_1.

      If the given up value does not fit into a 9 bits signed constant,
      prop_SDD_DDC9_2 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_SDD_DDC9_2( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_SDD_DDC9_3 propagates constants for a TriCore SUB/SUBS
             operation of format SDD_2.

      @param[in] o A const reference to a SUB/SUBS operation of format SDD_2
                   whose last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_1.

      For SUB* Da, "const Db", prop_SDD_DDC9_3 returns
      ADD* Da, Da, -1*"const Db".
      If the inverse of the given up value does not fit into a 9 bits signed
      constant, prop_SDD_DDC9_3 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_SDD_DDC9_3( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_SDD_DDC9_4 propagates constants for commutative TriCore
             SUB/SUBS operation of format SDD_2.

      @param[in] o A const reference to a SUB/SUBS operation of format SDD_2
                   whose first explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's first
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_1.

      For SUB* "const Da", Db, prop_SDD_DDC9_4 returns RSUB* Da, Db, "const Da".
      If the given up value does not fit into a 9 bits signed constant,
      prop_SDD_DDC9_4 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_SDD_DDC9_4( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_SDD_SIC8_2 propagates constants for TriCore operations AND or
             OR into format SIC8_2.

      @param[in] o A const reference to an AND/OR operation of format SDD_2
                   whose last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format SIC8_2.

      If the given up value does not fit into an 8 bits unsigned constant,
      prop_SDD_SIC8_2 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_SDD_SIC8_2( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_SIxD_SIxC4 propagates constants for a TriCore operation of
             formats SDID or SIDD into formats SDIC4 or SIDC4, resp.

      @param[in] o A const reference to an operation of format SDID or SIDD,
                   resp., whose last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format SDIC4 or SIDC4, resp.

      If the given up value does not fit into a 4 bits signed constant,
      prop_SIxD_SIxC4 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_SIxD_SIxC4( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_SIxD_SIxC4_2 propagates constants for a commutative TriCore
             operation of format SIDD into format SIDC4.

      @param[in] o A const reference to an operation of format SIDD whose second
                   explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's second
                   explicit parameter.
      @return A list of TriCore instructions with format SIDC4.

      If the given up value does not fit into a 4 bits signed constant,
      prop_SIxD_SIxC4_2 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_SIxD_SIxC4_2( const WIR_Operation &,
                                                  const WIR_UpDownValue & );

    /*!
      @brief prop_SIxD_SIDC4 propagates constants for a TriCore SUB operation of
             formats SIDD or SDID_1.

      @param[in] o A const reference to a SUB operation of formats SIDD or
                   SDID_1 whose last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format SIDC4.

      For SUB D15, Da, "const Db", prop_SIxD_SIDC4 returns
      ADD D15, Da, -1*"const Db".
      If the inverse of the given up value does not fit into a 4 bits signed
      constant, prop_SIxD_SIDC4 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_SIxD_SIDC4( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_SDD_DDC9_u propagates constants for an unsigned TriCore
             operation of format SDD_2.

      @param[in] o A const reference to an operation of format SDD_2 whose last
                   explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_2.

      If the given up value does not fit into a 9 bits unsigned constant,
      prop_SDD_DDC9_u returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_SDD_DDC9_u( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_SDD_DDC9_2u propagates constants for a commutative unsigned
             TriCore operation of format SDD_2.

      @param[in] o A const reference to an operation of format SDD_2 whose first
                   explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's first
                   explicit parameter.
      @return A list of TriCore instructions with format DDC9_2.

      If the given up value does not fit into a 9 bits unsigned constant,
      prop_SDD_DDC9_2u returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_SDD_DDC9_2u( const WIR_Operation &,
                                                 const WIR_UpDownValue & );

    /*!
      @brief prop_DDL_DC4L propagates constants for a TriCore jump operation of
             format DDL_1 or DDL_2.

      @param[in] o A const reference to an operation of format DDL_1 or DDL_2
                   whose second explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's second
                   explicit parameter.
      @return A list of TriCore instructions with format DC4L_1 or DC4L_3, resp.

      If the given up value does not fit into a 4 bits signed constant,
      prop_DDL_DC4L returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDL_DC4L( const WIR_Operation &,
                                              const WIR_UpDownValue & );

    /*!
      @brief prop_DDL_DC4L_2 propagates constants for a commutative TriCore jump
             operation of format DDL_1.

      @param[in] o A const reference to an operation of format DDL_1 whose first
                   explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's first
                   explicit parameter.
      @return A list of TriCore instructions with format DC4L_1.

      If the given up value does not fit into a 4 bits signed constant,
      prop_DDL_DC4L_2 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDL_DC4L_2( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_DDL_DC4L_3 propagates constants for the commutative TriCore
             operations JGE and JLT of format DDL_1.

      @param[in] o A const reference to a JGE or JLT operation of format DDD_1
                   whose first explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's first
                   explicit parameter.
      @return A list of TriCore instructions with format DC4L_1.

      For JGE "const Da", Db, disp15 prop_DDL_DC4L_3 returns
      JLT Db, "const Da+1", disp15.
      For JLT "const Da", Db, disp15 prop_DDL_DC4L_3 returns
      JGE Db, "const Da+1", disp15.
      If the given up value plus one does not fit into a 4 bits signed constant,
      prop_DDL_DC4L_3 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDL_DC4L_3( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_DDL_DC4L_u propagates constants for an unsigned TriCore jump
             operation of format DDL_1.

      @param[in] o A const reference to an unsigned operation of format DDL_1
                   whose second explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's second
                   explicit parameter.
      @return A list of TriCore instructions with format DC4L_2.

      If the given up value does not fit into a 4 bits unsigned constant,
      prop_DDL_DC4L_u returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDL_DC4L_u( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_DDL_DC4L_2u propagates constants for the commutative unsigned
             TriCore operations JGE.U and JLT.U of format DDL_1.

      @param[in] o A const reference to a JGE.U or JLT.U operation of format
                   DDD_1 whose first explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's first
                   explicit parameter.
      @return A list of TriCore instructions with format DC4L_2.

      For JGE.U "const Da", Db, disp15 prop_DDL_DC4L_2u returns
      JLT.U Db, "const Da+1", disp15.
      For JLT.U "const Da", Db, disp15 prop_DDL_DC4L_2u returns
      JGE.U Db, "const Da+1", disp15.
      If the given up value plus one does not fit into a 4 bits unsigned
      constant, prop_DDL_DC4L_2u returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDL_DC4L_2u( const WIR_Operation &,
                                                 const WIR_UpDownValue & );

    /*!
      @brief prop_SIDL_SIC4L propagates constants for a TriCore jump operation
             of format SIDL.

      @param[in] o A const reference to an operation of format SIDL whose second
                   explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's second
                   explicit parameter.
      @return A list of TriCore instructions with format SIC4L.

      If the given up value does not fit into a 4 bits signed constant,
      prop_SIDL_SIC4L returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_SIDL_SIC4L( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_AD_SAC4 propagates constants for a TriCore MOV.A operation
             of formats AD or SAD_1.

      @param[in] o A const reference to an operation of format AD or SAD_1 whose
                   last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format SAC4_1.

      If the given up value does not fit into a 4 bits unsigned constant,
      prop_AD_SAC4 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_AD_SAC4( const WIR_Operation &,
                                             const WIR_UpDownValue & );

    /*!
      @brief prop_DD_SDC4 propagates constants for a TriCore MOV operation of
             formats DD or SDD_1.

      @param[in] o A const reference to an operation of format DD or SDD_1 whose
                   last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format SDC4_1.

      If the given up value does not fit into a 4 bits signed constant,
      prop_DD_SDC4 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DD_SDC4( const WIR_Operation &,
                                             const WIR_UpDownValue & );

    /*!
      @brief prop_DD_SIC8 propagates constants for a TriCore MOV operation of
             formats DD or SDD_1 whose first register parameter is D15.

      @param[in] o A const reference to an operation of format DD or SDD_1 whose
                   last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format SIC8_1.

      If the given up value does not fit into an 8 bits unsigned constant,
      prop_DD_SIC8 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DD_SIC8( const WIR_Operation &,
                                             const WIR_UpDownValue & );

    /*!
      @brief prop_DD_DC16 propagates constants for a TriCore MOV operation of
             formats DD or SDD_1.

      @param[in] o A const reference to an operation of format DD or SDD_1 whose
                   last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DC16_1.

      If the given up value does not fit into a 16 bits signed constant,
      prop_DD_DC16 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DD_DC16( const WIR_Operation &,
                                             const WIR_UpDownValue & );

    /*!
      @brief prop_DD_DC16_1 propagates constants for a TriCore MOV operation of
             formats DD or SDD_1.

      @param[in] o A const reference to an operation of format DD or SDD_1 whose
                   last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore MOV.U instructions with format DC16_2.

      If the given up value does not fit into a 16 bits unsigned constant,
      prop_DD_DC16_1 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DD_DC16_1( const WIR_Operation &,
                                               const WIR_UpDownValue & );

    /*!
      @brief prop_xx_xC16 propagates constants for a TriCore MOV/MOV.AA
             operation of formats DD/SDD_1/AA/SAA_1.

      @param[in] o A const reference to a MOV/MOV.AA operation of formats DD,
                   SDD_1, AA or SAA_1 whose last explicit parameter is a
                   constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore MOVH/MOVH.A instructions with formats DC16_2
              or AC16, resp.

      If the given up value does not fit into the shape expected by
      MOVH/MOVH.A, prop_xx_xC16 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_xx_xC16( const WIR_Operation &,
                                             const WIR_UpDownValue & );

    /*!
      @brief prop_DDDC5_DDC5C5 propagates constants for TriCore EXTR* operations
             of format DDDC5.

      @param[in] o A const reference to an operation of format DDDC5 whose
                   third explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's third
                   explicit parameter.
      @return A list of TriCore instructions with format DDC5C5.

      If the given up value does not fit into a 5 bits unsigned constant,
      prop_DDDC5_DDC5C5 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDDC5_DDC5C5( const WIR_Operation &,
                                                  const WIR_UpDownValue & );

    /*!
      @brief prop_DDE_DDC5C5 propagates constants for TriCore EXTR* operations
             of format DDE.

      @param[in] o A const reference to an operation of format DDE whose last
                   explicit parameter with bits [36:32] and [4:0] is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DDC5C5.

      If bits [36:32] or [4:0] of the given up value do not fit into a 5 bits
      unsigned constant, prop_DDE_DDC5C5 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDE_DDC5C5( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_DDE_DDC5C5_2 propagates constants for TriCore EXTR* operations
             of format DDE.

      @param[in] o A const reference to an operation of format DDE whose last
                   explicit parameter with bits [36:32] is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DDDC5.

      If bits [36:32] of the given up value do not fit into a 5 bits unsigned
      constant, prop_DDE_DDC5C5_2 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDE_DDC5C5_2( const WIR_Operation &,
                                                  const WIR_UpDownValue & );

    /*!
      @brief prop_DDDD_DDDC5 propagates constants for TriCore DEXTR operations
             of format DDDD.

      @param[in] o A const reference to a DEXTR operation of format DDDD whose
                   last explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DDDC5.

      If the given up value does not fit into a 5 bits unsigned constant,
      prop_DDDD_DDDC5 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDDD_DDDC5( const WIR_Operation &,
                                                const WIR_UpDownValue & );

    /*!
      @brief prop_DDxDC5_DDxC5C5 propagates constants for TriCore INSERT
             operations of formats DDC4DC5 or DDDDC5, resp.

      @param[in] o A const reference to an operation of format DDC4DC5 or DDDDC5
                   whose fourth explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's fourth
                   explicit parameter.
      @return A list of TriCore instructions with format DDC4C5C5 or DDDC5C5,
              resp.

      If the given up value does not fit into a 5 bits unsigned constant,
      prop_DDxDC5_DDxC5C5 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDxDC5_DDxC5C5( const WIR_Operation &,
                                                    const WIR_UpDownValue & );

    /*!
      @brief prop_DDxE_DDxC5C5 propagates constants for TriCore INSERT
             operations of formats DDC4E or DDDE, resp.

      @param[in] o A const reference to an operation of format DDC4E or DDDE
                   whose last explicit parameter with bits [36:32] and [4:0] is
                   a constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DDC4C5C5 or DDDC5C5,
              resp.

      If bits [36:32] or [4:0] of the given up value do not fit into a 5 bits
      unsigned constant, prop_DDxE_DDxC5C5 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDxE_DDxC5C5( const WIR_Operation &,
                                                  const WIR_UpDownValue & );

    /*!
      @brief prop_DDxE_DDxC5C5_2 propagates constants for TriCore INSERT
             operations of formats DDC4E or DDDE, resp.

      @param[in] o A const reference to an operation of format DDC4E or DDDE
                   whose last explicit parameter with bits [36:32] is a
                   constant.
      @param[in] v A const reference to the up value of the operation's last
                   explicit parameter.
      @return A list of TriCore instructions with format DDC4DC5 or DDDDC5,
              resp.

      If bits [36:32] of the given up value do not fit into a 5 bits unsigned
      constant, prop_DDxE_DDxC5C5_2 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_DDxE_DDxC5C5_2( const WIR_Operation &,
                                                    const WIR_UpDownValue & );

    /*!
      @brief prop_EDDC5_EC4C5C5 propagates constants for TriCore IMASK
             operations of format EDDC5.

      @param[in] o A const reference to an operation of format EDDC5 whose
                   second and third explicit parameter are constant.
      @param[in] v1 A const reference to the up value of the operation's second
                    explicit parameter.
      @param[in] v2 A const reference to the up value of the operation's third
                    explicit parameter.
      @return A list of TriCore instructions with format EC4C5C5.

      If the given up values do not fit into a 4 or 5 bits unsigned constant,
      resp., prop_EDDC5_EC4C5C5 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_EDDC5_EC4C5C5( const WIR_Operation &,
                                                   const WIR_UpDownValue &,
                                                   const WIR_UpDownValue & );

    /*!
      @brief prop_ExDC5_ExC5C5 propagates constants for TriCore IMASK operations
             of formats EC4DC5 or EDDC5.

      @param[in] o A const reference to an operation of format EC4DC5 or EDDC5
                   whose third explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's third
                   explicit parameter.
      @return A list of TriCore instructions with formats EC4C5C5 or EDC5C5,
              resp.

      If bits [4:0] of the given up value do not fit into a 5 bits unsigned
      constant, prop_ExDC5_ExC5C5 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_ExDC5_ExC5C5( const WIR_Operation &,
                                                  const WIR_UpDownValue & );

    /*!
      @brief prop_EDxC5_EC4xC5 propagates constants for TriCore IMASK operations
             of formats EDC5C5 or EDDC5.

      @param[in] o A const reference to an operation of format EDC5C5 or EDDC5
                   whose second explicit parameter is a constant.
      @param[in] v A const reference to the up value of the operation's second
                   explicit parameter.
      @return A list of TriCore instructions with formats EC4C5C5 or EC4DC5,
              resp.

      If the given up value do not fit into a 4 bits unsigned constant,
      prop_EDxC5_EC4xC5 returns an empty instruction list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_EDxC5_EC4xC5( const WIR_Operation &,
                                                  const WIR_UpDownValue & );

    /*!
      @brief prop_xACBOA_xALBOA propagates locations refering to symbol
             addresses into TriCore LD.A and LD.W operations of formats
             AAC10BOA, AAC16BOA DAC10BOA or DAC16BOA.

      @param[in] o A const reference to a LD.A or LD.W operation of formats
                   AAC10BOA, AAC16BOA DAC10BOA or DAC16BOA.
      @param[in] s A const reference to the %WIR symbol used by the LD.A or LD.W
                   operation.
      @return A list of TriCore instructions with formats AALC16BOA or
              DALC16BOA, resp.

      This method produces an instruction sequence of format

        MOVH.A areg_0, HI:s
        LD.W   areg_1, [areg_0] LO:s

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_xACBOA_xALBOA( const WIR_Operation &,
                                                   const WIR_Symbol & );

    /*!
      @brief prop_ACDBOA_ALDBOA propagates locations refering to symbol
             addresses into TriCore ST.W operations of formats AC10DBOA or
             AC16DBOA.

      @param[in] o A const reference to a ST.W operation of formats AC10DBOA or
                   AC16DBOA.
      @param[in] s A const reference to the %WIR symbol used by the ST.W
                   operation.
      @return A list of TriCore instructions with format ALC16DBOA.

      This method produces an instruction sequence of format

        MOVH.A areg_0, HI:s
        ST.W   [areg_0] LO:s, dreg

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> prop_ACDBOA_ALDBOA( const WIR_Operation &,
                                                   const WIR_Symbol & );

    /*!
      @brief patchDefinedParameter updates the bit-values associated with a
             defined parameter of an operation subject to constant propagation.

      @param[in] p A const reference to the defined register parameter of the
                   operation created by constant propagation.
      @param[in] pOrig A const reference to the defined register parameter of
                       the original operation before constant propagation.

      For the targets of all out-edges of p, patchDefinedParameter removes pOrig
      from the set of incoming bit-values and adds p instead.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void patchDefinedParameter( const WIR_RegisterParameter &,
                                const WIR_Parameter & );

    /*!
      @brief patchUsedParameter updates the bit-values associated with a used
             parameter of an operation subject to constant propagation.

      @param[in] p A const reference to the used register parameter of the
                   operation created by constant propagation.
      @param[in] pOrig A const reference to the used register parameter of the
                       original operation before constant propagation.

      For the sources of all in-edges of p, patchUsedParameter adds p to the set
      of outgoing bit-values.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void patchUsedParameter( const WIR_Parameter &,
                             const WIR_Parameter & ) const;

    /*!
      @brief patchUsedERegParameter updates the bit-values associated with a
             used extended register parameter of an operation subject to
             constant propagation.

      @param[in] p A reference to the used data register parameter of the
                   operation created by constant propagation.
      @param[in] pOrig A const reference to the used extended register parameter
                       of the original operation before constant propagation.

      For the sources of all in-edges of pOrig, patchUsedERegParameter checks
      whether the current edge relates to the extended register's first child.
      If so, p is added to the set of outgoing bit-values.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void patchUsedERegParameter( WIR_Parameter &,
                                 const WIR_Parameter & ) const;

    /*!
      @brief patchImmediateParameter creates a bit-value container for an
             immediate parameter newly created during constant propagation.

      @param[in,out] p A reference to the immediate parameter to be updated.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void patchImmediateParameter( WIR_BaseImmediateParameter & ) const;

    /*!
      @brief checkSelfEdges checkes whether an original operation contains
             DFG edges starting and ending both at some of the operation's
             parameters. Such self-edges are then taken over to a new
             constant-folded operation.

      @param[in,out] o A reference to a new constant-folded operation.
      @param[in] oOrig A const reference to an original operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkSelfEdges( WIR_Operation &, const WIR_Operation & ) const;

    /*!
      @brief isSymbol checks whether an up/down value entirely refers to one
             %WIR symbol.

      @param[in] v A const reference to the up/down value to be checked.
      @return true iff the given up/down value refers completely to one symbol,
              false otherwise.

      In order to refer completely to a symbol, all bits of the up/down value
      must be L, all locations must refer to one and the same symbol, and bit
      i in the given up/down value must refer to bit i of the symbol's address.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSymbol( const WIR_UpDownValue & ) const;

};

}       // namespace WIR

#endif  // _TC_CONSTPROP_H
