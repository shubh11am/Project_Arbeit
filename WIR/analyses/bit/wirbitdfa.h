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
  @file wirbitdfa.h
  @brief This file provides the interface of generic bit-true data flow
         analyses.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_BITDFA_H
#define _WIR_BITDFA_H


//
// Include section
//

// Include standard headers
#include <map>

// Include WIR headers
#include <wir/wirtypes.h>
#include <analyses/generic/wiranalysis.h>
#include <analyses/bit/wirdfg.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Function;
class WIR_Operation;
class WIR_Parameter;
class WIR_RegisterParameter;


/*!
  @brief Class WIR_BitDFA is a generic base class for bit-true data flow
         analysis based on the %WIR data flow graph.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_BitDFA : public WIR_Analysis,
                   public WIR_DFG
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for function-level analysis.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_BitDFA( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_BitDFA( void );


    //
    // Hierarchical register handling.
    //

    /*!
      @brief getHierarchicalRegisterOffset computes the offset in bits by that
             a first register occurs in the hierarchy of a second register, or
             vice versa.

      @param[in] p1 A const reference to the first register parameter whose
                    potentially hierarchical register shall be examined.
      @param[in] p2 A const reference to the second register parameter whose
                    potentially hierarchical register shall be examined.
      @return An unsigned integer denotig the bit offset between the first and
              second register hierarchies.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static unsigned int getHierarchicalRegisterOffset( const WIR_RegisterParameter &,
                                                       const WIR_RegisterParameter & );

    /*!
      @brief getHierarchicalUpDownValue returns an edge's up/down value under
             consideration of the edge's source and target parameter bit widths.

      @param[in] src A const reference to an edge's source parameter.
      @param[in] tgt A const reference to an edge's target parameter.
      @param[in] v A const reference to the down value to be set.
      @param[in] t A Boolean flag defaulting to true that indicates whether the
                   specifities of top-down (true) or bottom-up (false) analysis
                   shall be considered.

      While determining the up/down value for the current edge, the bit widths
      of the edge's source and target register parameters are considered and
      appropriate insert or extract operations are applied if required.

      If top-down analysis is considered, getHierarchicalUpDownValue behaves as
      follows:
      - If the edge's source register is wider than the target register, the
        location of target within the hierarchical source register is determined
        and exactly those bits are extracted from v and returned.
      - If otherwise the target register is wider than the source register, the
        location of source within the hierarchical target register is determined
        and v is inserted into an up/down value of target width into exactly
        that location.

      If bottom-up analysis is considered, getHierarchicalUpDownValue behaves as
      follows:
      - If the edge's source register is wider than the target register, the
        location of target within the hierarchical source register is determined
        and v is inserted into an up/down value of source width into exactly
        that location.
      - If otherwise the target register is wider than the source register, the
        location of source within the hierarchical target register is determined
        and exactly those bits are extracted from v and returned.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static WIR_UpDownValue getHierarchicalUpDownValue( const WIR_RegisterParameter &,
                                                       const WIR_RegisterParameter &,
                                                       const WIR_UpDownValue &,
                                                       bool = true );

    /*!
      @brief combineInEdge combines the up/down value of a node's incoming edge
             with previous up/down values, if multiple edges with potentially
             different up/down values refer to the very same parameter of a %WIR
             operation.

      @param[in] src A const reference to an incoming edge's source parameter.
      @param[in] tgt A const reference to an incoming edge's target parameter.
      @param[in] v A const reference to the edge's up/down value to be combined.
      @param[in,out] operands A reference to a map storing the combined up/down
                              values per %WIR parameter.
      @param[in,out] initializedOperandBits A reference to a map required for
                                            book-keeping of hierarchical
                                            registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static void combineInEdge( const WIR_Parameter &, const WIR_Parameter &,
                               const WIR_UpDownValue &,
                               std::map<WIR_id_t, WIR_UpDownValue> &,
                               std::map<WIR_id_t, WIR_UpDownValue> & );

    /*!
      @brief combineOutEdge combines the up value of a node's outgoing edge with
             previous up values, if multiple edges with potentially different
             up values refer to the very same parameter of a %WIR operation.

      @param[in] src A const reference to an incoming edge's source parameter.
      @param[in] tgt A const reference to an incoming edge's target parameter.
      @param[in] v A const reference to the edge's up/down value to be combined.
      @param[in,out] operands A reference to a map storing the combined up
                              values per %WIR parameter.
      @param[in,out] initializedOperandBits A reference to a map required for
                                            book-keeping of hierarchical
                                            registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static void combineOutEdge( const WIR_RegisterParameter &,
                                const WIR_RegisterParameter &,
                                const WIR_UpDownValue &,
                                std::map<WIR_id_t, WIR_UpDownValue> &,
                                std::map<WIR_id_t, WIR_UpDownValue> & );


  protected:

    /*!
      @brief runAnalysis performs bit-true data flow flow analysis of the given
             function.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runAnalysis( WIR_Function & );

    /*!
      @brief simulateTopDown performs the actual top-down simulation of the
             given %WIR operation.

      @param[in] o A const reference to the operation to be simulated top-down.
      @param[in] operands A reference to a map containing the down values of o's
                          operands, depending on the IDs of their associated
                          %WIR parameters.
      @param[in,out] results A reference to a map containing the down values of
                             o's results, depending on the IDs of their
                             associated %WIR parameters.

      Since the top-down simulation of an actual %WIR operation is
      processor-specific, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void simulateTopDown( const WIR_Operation &o,
                                  std::map<WIR_id_t, WIR_UpDownValue> &operands,
                                  std::map<WIR_id_t, WIR_UpDownValue> &results ) = 0;

    /*!
      @brief simulateBottomUp performs the actual bottom-up simulation of the
             given %WIR operation.

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

      Since the bottom-up simulation of an actual %WIR operation is
      processor-specific, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void simulateBottomUp( const WIR_Operation &o,
                                   std::map<WIR_id_t, WIR_UpDownValue> &in,
                                   std::map<WIR_id_t, WIR_UpDownValue> &out,
                                   std::map<WIR_id_t, WIR_UpDownValue> &results ) = 0;


    //
    // Hierarchical register handling.
    //

    /*!
      @brief getHierarchicalRegisterOffset computes the offset in bits by that
             the edge's source register occurs in the hierarchy of the edge's
             target register, or vice versa.

      @param[in] e A graph edge whose potentially hierarchical source and target
                   registers are examined.
      @return An unsigned integer denotig the bit offset between the source and
              target register hierarchies.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getHierarchicalRegisterOffset( DGraphEdge ) const;

    /*!
      @brief getHierarchicalUpDownValue returns an edge's up/down value under
             consideration of the edge's source and target parameter bit widths.

      @param[in] e A graph edge whose potentially hierarchical source and target
                   registers are examined.
      @param[in] v A const reference to the down value to be set.
      @param[in] t A Boolean flag defaulting to true that indicates whether the
                   specifities of top-down (true) or bottom-up (false) analysis
                   shall be considered.

      While determining the up/down value for the current edge, the bit widths
      of the edge's source and target register parameters are considered and
      appropriate insert or extract operations are applied if required.

      If top-down analysis is considered, getHierarchicalUpDownValue behaves as
      follows:
      - If the edge's source register is wider than the target register, the
        location of target within the hierarchical source register is determined
        and exactly those bits are extracted from v and returned.
      - If otherwise the target register is wider than the source register, the
        location of source within the hierarchical target register is determined
        and v is inserted into an up/down value of target width into exactly
        that location.

      If bottom-up analysis is considered, getHierarchicalUpDownValue behaves as
      follows:
      - If the edge's source register is wider than the target register, the
        location of target within the hierarchical source register is determined
        and v is inserted into an up/down value of source width into exactly
        that location.
      - If otherwise the target register is wider than the source register, the
        location of source within the hierarchical target register is determined
        and exactly those bits are extracted from v and returned.

      getHierarchicalUpDownValue asserts if it is invoked for an edge of type
      inequal 'op'.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue getHierarchicalUpDownValue( DGraphEdge,
                                                const WIR_UpDownValue &,
                                                bool = true ) const;


  private:

    /*!
      @brief init initializes data structures by attaching fresh containers to
             the register parameters of the specified %WIR function.

      @param[in] f A reference to a WIR_Function to be initialized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void init( WIR_Function & );

    /*!
      @brief topDownAnalysis performs top-down data flow analysis of the
             current function.

      The work flow of top-down analysis is described in section 4.4.8 of J.
      Wagner. "Retargierbare Ausnutzung von Spezialoperationen für Eingebettete
      Systeme mit Hilfe bitgenauer Wertflussanalyse". Ph.D. thesis, Dortmund
      University, page 173ff., 2006.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void topDownAnalysis( void );

    /*!
      @brief topDownAnalysis performs top-down data flow analysis for the given
             DFG node and its associated %WIR operation.

      @param[in] v The current DFG node to be analyzed.
      @param[in] o A const reference to the %WIR operation associated with DFG
                   node v.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void topDownAnalysis( DGraphVertex, const WIR_Operation & );

    /*!
      @brief bottomUpAnalysis performs bottom-up data flow analysis of the
             current function.

      The work flow of bottom-up analysis is described in section 4.4.8 of J.
      Wagner. "Retargierbare Ausnutzung von Spezialoperationen für Eingebettete
      Systeme mit Hilfe bitgenauer Wertflussanalyse". Ph.D. thesis, Dortmund
      University, page 173ff., 2006.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void bottomUpAnalysis( void );

    /*!
      @brief bottomUpAnalysis performs bottom-up data flow analysis for the
             given DFG node and its associated %WIR operation.

      @param[in] v The current DFG node to be analyzed.
      @param[in] o A const reference to the %WIR operation associated with DFG
                   node v.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void bottomUpAnalysis( DGraphVertex, const WIR_Operation & );

    /*!
      @brief combineInEdge combines the up/down value of a node's incoming edge
             with previous up/down values, if multiple edges with potentially
             different up/down values refer to the very same parameter of a %WIR
             operation.

      @param[in] e The current incoming edge to be considered.
      @param[in,out] operands A reference to a map storing the combined up/down
                              values per %WIR parameter.
      @param[in,out] initializedOperandBits A reference to a map required for
                                            book-keeping of hierarchical
                                            registers.
      @param[in] d A Boolean flag indicating whether the edge's down- (true) or
                   up-values (false) shall be considered.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void combineInEdge( const DGraphEdge, std::map<WIR_id_t, WIR_UpDownValue> &,
                        std::map<WIR_id_t, WIR_UpDownValue> &, bool ) const;

    /*!
      @brief combineOutEdge combines the up value of a node's outgoing edge with
             previous up values, if multiple edges with potentially different
             up values refer to the very same parameter of a %WIR operation.

      @param[in] e The current outgoing edge to be considered.
      @param[in,out] operands A reference to a map storing the combined up
                              values per %WIR parameter.
      @param[in,out] initializedOperandBits A reference to a map required for
                                            book-keeping of hierarchical
                                            registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void combineOutEdge( const DGraphEdge, std::map<WIR_id_t, WIR_UpDownValue> &,
                         std::map<WIR_id_t, WIR_UpDownValue> & ) const;

    /*!
      @brief combine returns the "smallest common" combination of two up/down
             values.

      @param[in] v1 A const reference to the first up/down value to be combined.
      @param[in] v2 A const reference to the second up/down value to be
                    combined.
      @param[in] o An unsigned integer denoting a bit offset to be used when
                   combining a smaller up/down value with a larger one.
      @return The up/down value resulting from the combination.

      The com operator is defined in Jens Wagner, Retargierbare Ausnutzung von
      Spezialoperationen für Eingebettete Systeme mit Hilfe bitgenauer
      Wertflussanalyse, page 177, Figure 4.19.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static WIR_UpDownValue combine( const WIR_UpDownValue &,
                                    const WIR_UpDownValue &, unsigned int );

    /*!
      @brief createContainers takes the up/down values from the data flow graph
             and attaches them persistently to the current %WIR function using
             bit-value containers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void createContainers( void );

};

}       // namespace WIR

#endif  // _WIR_BITDFA_H
