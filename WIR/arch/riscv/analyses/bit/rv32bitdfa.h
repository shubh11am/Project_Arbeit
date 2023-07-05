/*

   This header file belongs to the

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
  @file rv32bitdfa.h
  @brief This file provides the basic interface of a RISC-V-specific bit-true
         data flow analysis.

  @author Maurice Hoffmann <Maurice.Hoffmann@tuhh.de>
*/


#ifndef _RV32_BITDFA_H
#define _RV32_BITDFA_H


//
// Include section
//

// Include standard headers
#include <map>

// Include WIR headers
#include <wir/wirtypes.h>
#include <analyses/bit/wirbitdfa.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_DFGEdgeProperty;
class WIR_Function;
class WIR_RegisterParameter;


/*!
  @brief Class RV32_BitDFA performs bit-true data flow analysis for the RISC-V
         architecture based on the %WIR data flow graph.

  @author Maurice Hoffmann <Maurice.Hoffmann@tuhh.de>
*/
class RV32_BitDFA : public WIR_BitDFA
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for function-level analysis.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Maurice Hoffmann <Maurice.Hoffmann@tuhh.de>
    */
    explicit RV32_BitDFA( WIR_Function & );

    /*!
      @brief Destructor.
      @author Maurice Hoffmann <Maurice.Hoffmann@tuhh.de>
    */
    virtual ~RV32_BitDFA( void );


  protected:

    /*!
      @brief simulateTopDown performs the RISC-V-specific top-down simulation
             of the given %WIR operation.

      @param[in] o A const reference to the operation to be simulated top-down.
      @param[in] operands A reference to a map containing the down values of o's
                          operands, depending on the IDs of their associated
                          %WIR parameters.
      @param[in,out] results A reference to a map containing the down values of
                             o's results, depending on the IDs of their
                             associated %WIR parameters.

      For a documentation of the semantics of RISC-V operations, please refer
      to the RISC-V Instruction Set Manual Volume I: User-Level ISA.

      @author Maurice Hoffmann <Maurice.Hoffmann@tuhh.de>
    */
    virtual void simulateTopDown( const WIR_Operation &,
                                  std::map<WIR_id_t, WIR_UpDownValue> &,
                                  std::map<WIR_id_t, WIR_UpDownValue> & );

    /*!
      @brief simulateBottomUp performs the RISC-V-specific bottom-up simulation
             of the given %WIR operation.

      @param[in] o A const reference to the operation to be simulated bottom-up.
      @param[in] in A reference to a map containing the up values of o's
                    incoming edges, depending on the IDs of their associated
                    %WIR parameters.
      @param[in] out A reference to a map containing the up values of o's
                     outgoing edges, depending on the IDs of their associated
                     %WIR parameters.
      @param[in,out] results A reference to a map containing the up values of
                             o's results, depending on the IDs of their
                             associated %WIR parameters.

      For a documentation of the semantics of RISC-V operations, please refer
      to the RISC-V Instruction Set Manual Volume I: User-Level ISA.

      @author Maurice Hoffmann <Maurice.Hoffmann@tuhh.de>
    */
    virtual void simulateBottomUp( const WIR_Operation &,
                                   std::map<WIR_id_t, WIR_UpDownValue> &,
                                   std::map<WIR_id_t, WIR_UpDownValue> &,
                                   std::map<WIR_id_t, WIR_UpDownValue> & );

    /*!
      @brief postProcessEdge can be used to set properties of newly created DFG
             edges to particular, processor-dependent values.

      @param[in,out] ep A reference to a DFG edge's properties to be
                        post-processed.

      postProcessEdge checks whether the edge refers to register x0 which always
      contains the value 0. Reading this register should result in up/down
      values only containing 0 bits, which is realized here.
    */
    virtual void postProcessEdge( WIR_DFGEdgeProperty & );

    /*!
      @brief addEdge returns whether an edge from a defined register parameter
             to a used register parameter shall be added to the DFG.

      @param[in] def A const reference to a defined register parameter.
      @param[in] use A const reference to a used register parameter.
      @return true if the edge shall be added, false otherwise.

      addEdge checks whether the defined register is x0 which always contains
      the value 0, irrespective of what is written into it. For this register, a
      proper def-use relationship in the DFG does not make sense so that no edge
      will be added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool addEdge( const WIR_RegisterParameter &,
                          const WIR_RegisterParameter & );

};

}       // namespace WIR

#endif  // _RV32_BITDFA_H
