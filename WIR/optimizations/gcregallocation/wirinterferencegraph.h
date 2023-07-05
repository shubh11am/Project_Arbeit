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
  @file wirinterferencegraph.h
  @brief This file provides the basic interface of interference graphs.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_INTERFERENCEGRAPH_H
#define _WIR_INTERFERENCEGRAPH_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <fstream>
#include <list>
#include <map>
#include <set>
#include <string>

// Include WIR headers
#include <wir/wirtypes.h>

// Include local headers
#include "wirigraphedgeproperty.h"
#include "wirigraphnodeproperty.h"


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseProcessor;
class WIR_BaseRegister;
class WIR_Function;
class WIR_PhysicalRegister;
class WIR_VirtualRegister;


/*!
  @brief Class WIR_InterferenceGraph implements a generic, machine-independent,
         %WIR interference graph for graph-coloring based register allocators.

  This interference graph holds nodes for both a processor's physical registers,
  and the virtual registers inside a %WIR system which shall be allocated. When
  passing a %WIR system to a constructor of class WIR_InterferenceGraph, the
  constructor automatically extracts all physical and virtual registers inside
  the %WIR and already generates interference graph nodes for them. For this
  reason, the API of WIR_InterferenceGraph provides no public methods to add
  nodes. During the automatic creation of nodes, the interference graph also
  determines the number of colors available for a future graph coloring.

  All properties of WIR_InterferenceGraph can be accessed using references to
  %WIR registers as "keys". %WIR registers are thus used to identify graph
  nodes. The internal graph machinery (based on boost) is completely hidden,
  only STL containers and %WIR registers are required to use the
  WIR_InterferenceGraph.

  For simple processors with no hierarchical registers, the invariant holds that
  the interference graph contains one dedicated node per virtual register. Such
  a graph node can be colored with exactly one color so that a unique mapping of
  the modeled virtual register to a physical register is given.

  However, things are a bit more complicated for processors featuring
  hierarchical registers (e.g., Infineon Tricore and its extended registers).
  Suppose, a %WIR system includes a virtual extended register e_2 with child
  registers d_0 and d_1. Then, only one node for all 3 registers in this
  hierarchy is added to the interference graph. This node models the root node
  of the register hierarchy, i.e., e_2 here. However, you can also use childs
  d_0 or d_1 to query the interference graph, and all such queries are
  redirected to the graph node of e_2. The interference graph node of e_2 saves
  the information that d_0 and d_1 are children, or more precise, leafs in its
  register hierarchy.

  Register allocation shall now assign colors to the leafs of such a register
  hierarchy, i.e., to d_0 and d_1. Hence, it is possible to assign a color to
  such a leaf register using setColor. Invoking getColors on such a leaf returns
  this unique, previously assigned color. However, invoking getColors on the
  root of a register hierarchy, i.e., on e_2, yields all colors that are
  assigned to all leaf nodes. For this reason, getColors returns a set of
  colors, and not only one single color. In turn, it is not allowed to assign a
  color to a root of a register hierarchy using setColor, since this has no
  clear meaning. This modeling of hierarchical registers is taken from:
  P. Briggs. "Register Allocation via Graph Coloring". Ph.D. thesis,
  Rice University, 1992.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_InterferenceGraph final
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for a given %WIR function.

      @param[in] phregs A const reference to a list of physical %WIR registers.
                   be used to construct the interference graph.
      @param[in] f A reference to a %WIR function whose virtual registers will
                   be used to construct the interference graph.
      @param[in] vregs A const reference to a subset of f's VREGs that shall be
                       represented by interference graph nodes. If this set is
                       empty, all VREGs of the %WIR function f are included.
      @param[in] verbosity A Boolean denoting whether verbose messages shall be
                           dumped.
      @param[in] keepTmpFiles A Boolean denoting whether temporary files shall
                              be kept.

      This constructor creates a dedicated node for each physical register of
      phregs in the interference graph, and each such node gets its own unique
      color right from the beginning. Furthermore, the number of created
      physical registers determines the number of available colors for the
      entire interference graph, i.e., the value returned by
      getAvailableColors().

      For all virtual registers contained in f and optionally constrained by set
      vregs, this constructor also adds dedicated uncolored nodes to the
      interference graph.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_InterferenceGraph( const std::list<std::reference_wrapper<const WIR_PhysicalRegister>> &,
                           WIR_Function &,
                           const WIR_VirtualRegisterSet &,
                           bool = false, bool = false );

    /*!
      @brief Constructor for a completely empty interference graph.

      This constructor produces a completely empty interference graph with no
      nodes for both physical and virtual registers, and with no associated %WIR
      elements.

      The use of this constructor is strongly discouraged, you are requested to
      use one of the other constructors above. The only scenario where to use
      this discouraged constructor is to create an empty graph that shall
      later be filled using the '=' operator below.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_InterferenceGraph( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_InterferenceGraph( const WIR_InterferenceGraph & );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_InterferenceGraph( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_InterferenceGraph & operator = ( const WIR_InterferenceGraph & );


    //
    // General graph properties.
    //

    /*!
      @brief containsNode checks whether a node for the specified %WIR register
             already exists in the interference graph.

      @param[in] r A const reference to the register whose existence shall be
                   checked.
      @return True if a graph node exists, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsNode( const WIR_BaseRegister & ) const;

    /*!
      @brief areSameNodes checks whether the interference graph nodes for the
             two specified registers are the same.

      @param[in] r1 A const reference to the first register whose graph node
                    shall be checked.
      @param[in] r2 A const reference to the second register whose graph node
                    shall be checked.
      @return True if the graph nodes of both registers are the same, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool areSameNodes( const WIR_BaseRegister &,
                       const WIR_BaseRegister & ) const;

    /*!
      @brief getDegree returns the degree of the interference graph node
             specified by the given %WIR register.

      @param[in] r A const reference to the register whose degree shall be
                   returned.
      @return The computed degree of the specified graph node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getDegree( const WIR_BaseRegister & ) const;

    /*!
      @brief getNeighbors returns a set of all neighboring registers.

      @param[in] r A const reference to the register whose neighbors shall be
                   returned.
      @return The set of neighbor registers.

      The set returned by this method includes both virtual and physical
      neighboring registers. Furthermore, only such nodes which have not been
      pushed onto the stack are included. getNeighbors only returns the
      component mRegister of all neighboring graph nodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterSet getNeighbors( const WIR_BaseRegister & ) const;

    /*!
      @brief getNeighborVREGs returns a set of all neighboring virtual
             registers.

      @param[in] r A const reference to the register whose neighbors shall be
                   returned.
      @return The set of virtual neighbor registers.

      The set returned by this method includes only virtual neighboring
      registers. Furthermore, only such nodes which have not been pushed onto
      the stack are included. getNeighborVREGs only returns the component
      mRegister of all neighboring graph nodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_VirtualRegisterSet getNeighborVREGs( const WIR_BaseRegister & ) const;

    /*!
      @brief setLoopNestingDepth sets the maximal loop depth in which the
             specified register is defined or used.

      @param[in] r A const reference to the register whose loop nesting depth
                   shall be set.
      @param[in] depth The loop nesting depth to be assigned to the specified
                       register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setLoopNestingDepth( const WIR_BaseRegister &, unsigned int );

    /*!
      @brief getLoopNestingDepth returns the maximal loop depth in which the
             specified register is defined or used.

      @param[in] r A const reference to the register whose loop nesting depth
                   shall be returned.
      @return The loop nesting depth of the specified register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getLoopNestingDepth( const WIR_BaseRegister & ) const;

    /*!
      @brief visualize dumps the current interference graph into a DOT file and
             invokes xdot on it.

      @param[in] allNodes A Boolean defaulting to 'false' and specifying whether
                          really all nodes (unpushed and pushed ones) or only
                          nodes not pushed onto the stack shall be displayed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualize( bool = false ) const;


    //
    // Interference handling.
    //

    /*!
      @brief addInterference adds interference edges between the nodes specified
             by the given %WIR registers.

      @param[in] r1 A const reference to the first node of the new edge.
      @param[in] r2 A const reference to the second node of the new edge.
      @param[in] e The number of parallel edges to be inserted between the two
                   sepcified graph nodes.

      Using the parameter e, it can be specified how many parallel edges between
      two nodes will be inserted. This might be necessary for graph nodes
      representing hierarchical registers. In such a case, the interference
      graph becomes a multi-graph. When using the default value 0 for e,
      addInterference automatically determines how many childs a hierarchical
      node contains and inserts exactly this number of parallel edges. If this
      automatic behavior is undesired, appropriate numbers of parallel edges can
      be specified explicitly.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addInterference( const WIR_BaseRegister &, const WIR_BaseRegister &,
                          unsigned int = 0 );

    /*!
      @brief interfere returns whether the nodes represented by the specified
             %WIR registers interfere.

      @param[in] r1 A const reference to the first register whose interference
                    shall be checked.
      @param[in] r2 A const reference to the second register whose interference
                    shall be checked.
      @return true if the two nodes interfere, false otherwise.

      Interference is indicated if there is an edge between the two nodes in the
      interference graph, and this edge is not pushed onto the stack.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool interfere( const WIR_BaseRegister &, const WIR_BaseRegister & ) const;


    //
    // Color handling.
    //

    /*!
      @brief getAvailableColors returns the number of colors available to color
             the interference graph.

      @return An unsigned integer holding the number of available colors.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getAvailableColors() const;

    /*!
      @brief getColorOfPhreg returns the color number internally assigned to the
             specified physical register.

      @param[in] r A const reference to a physical register.
      @return A color value from the interval [1, getAvailableColors()].

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getColorOfPhreg( const WIR_PhysicalRegister & ) const;

    /*!
      @brief getPhregOfColor returns the physical register assigned to the
             specified color.

      @param[in] c A color number from the interval [1, getAvailableColors()].
      @return A reference to the corresponding physical register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_PhysicalRegister &getPhregOfColor( unsigned int ) const;

    /*!
      @brief setColor colors the node of the specified %WIR register with the
             given color.

      @param[in] r A const reference to the register that shall be colored.
      @param[in] c The number of the color to be used.

      The parameter c must be in the interval [0, getAvailableColors()].
      Assigning a color from [1, getAvailableColors()] implies that the
      corresponding node is unmarked as potential and actual spill. Assigning a
      color 0 to a register means to uncolor a node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setColor( const WIR_BaseRegister &, unsigned int );

    /*!
      @brief getColors returns the set of colors assigned to the node of the
             specified %WIR register.

      @param[in] r A const reference to the register whose colors shall be
                   returned.
      @return A set of values between [0, getAvailableColors()]. An empty set
              denotes an an uncolored node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::set<unsigned int> getColors( const WIR_BaseRegister & ) const;

    /*!
      @brief getColorName returns the name of the physical register with which
             the node of the specified %WIR register is colored.

      @param[in] r A const reference to the register whose color name shall be
                   returned.
      @return A string containing the desired PHREG name. The empty string is
              returned for an uncolored node.

      This method only returns a valid name if the specified %WIR register is
      either a simple, non-hierarchical register, or if it is a leaf of a
      register hierarchy. For a non-leaf of a register hierarchy, it does not
      make sense to return one single color name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getColorName( const WIR_BaseRegister & ) const;

    /*!
      @brief isColored returns whether the node of the specified %WIR register
             is colored.

      @param[in] r A const reference to the register whose coloring shall be
                   checked.
      @return true if the node is colored, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isColored( const WIR_BaseRegister & ) const;

    /*!
      @brief containsUncoloredNodes returns whether the interference graph
             contains any uncolored node.

      @return true if the graph contains an uncolored node, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsUncoloredNodes( void ) const;

    /*!
      @brief getPossibleColors returns a set of colors that can be used to color
             the specified node.

      @param[in] r A const reference to the register for whose node in the
                   interference graph possible colors shall be determined.
      @return The set of possible colors, such that it does not include any
              color already used for an adjacent node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::set<unsigned int> getPossibleColors( const WIR_BaseRegister & ) const;


    //
    // Coalescing.
    //

    /*!
      @brief coalesceNodes coalesces the two interference graph nodes specified
             by the given %WIR registers to a single node.

      @param[in] r1 A const reference to the first register that shall be
                    coalesced.
      @param[in] r2 A const reference to the second register that shall be
                    coalesced.
      @return A reference to that register remaining as node in the interference
              graph after coalescing.

      coalesceNodes is only applicable to pairs (r1, r2) of registers for which
      holds:

      - r1 and r2 are both neither pushed on the stack, nor physical registers.
      - r1 and r2 do not interfere, i.e., there is no edge {r1, r2} in the
        interference graph, and r1 and r2 are not represented by the same
        interference graph node.

      If these preconditions hold, coalesceNodes merges both registers to a
      single node and connects the edges of the two original nodes to the new
      coalesced node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseRegister &coalesceNodes( const WIR_BaseRegister &,
                                     const WIR_BaseRegister & );

    /*!
      @brief getCoalescedAliases returns the set of coalescing aliases of the
             specified %WIR register, if any.

      @param[in] r A const reference to the register whose coalescing aliases
                   shall be determined.
      @return A set containing the belonging coalescing aliases.

      If the specified register is not coalesced with any other register, an
      empty set is returned.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterSet getCoalescedAliases( const WIR_BaseRegister & ) const;

    /*!
      @brief For some arbitrary leaf or root register in a register hierarchy,
             getUnaliasedReg returns this register's un-aliased partner as
             stored natively in some interference graph node.

      @param[in] r A const reference to the register whose native un-aliased
                   partner shall be determined.
      @return A reference to the un-aliased register partner.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseRegister &getUnaliasedReg( const WIR_BaseRegister & ) const;

    /*!
      @brief getRemainingReg returns that one of the two given registers that
             will remain in the interference graph when coalescing the two graph
             nodes.

      @param[in] r1 A const reference to the first register that shall be
                    coalesced.
      @param[in] r2 A const reference to the second register that shall be
                    coalesced.
      @return The register remaining as node in the interference graph after
              coalescing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseRegister &getRemainingReg( const WIR_BaseRegister &,
                                       const WIR_BaseRegister & ) const;

    /*!
      @brief getVanishingReg returns that one of the two given registers that
             will vanish when coalescing the two graph nodes.

      @param[in] r1 A const reference to the first register that shall be
                    coalesced.
      @param[in] r2 A const reference to the second register that shall be
                    coalesced.
      @return The register vanishing from the interference graph after
              coalescing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseRegister &getVanishingReg( const WIR_BaseRegister &,
                                       const WIR_BaseRegister & ) const;


    //
    // Spilling.
    //

    /*!
      @brief setPotentialSpill marks or unmarks the node of the specified
             virtual register as potential spill.

      @param[in] r A const reference to the virtual register that shall be
                   marked or unmarked as potential spill.
      @param[in] m The potential spill mark to be assigned.

      If this method is applied to a node not representing an uncolored virtual
      register, no changes are made. Marking a node as potential spill has the
      side-effect that it is no longer marked as actual spill.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setPotentialSpill( const WIR_VirtualRegister &, bool = true );

    /*!
      @brief isPotentialSpill returns whether the node of the specified virtual
             register is marked as potential spill.

      @param[in] r A const reference to the virtual register whose potential
                   spill mark shall be returned.
      @return true if the node is marked as potential spill, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isPotentialSpill( const WIR_VirtualRegister & ) const;

    /*!
      @brief setActualSpill marks or unmarks the node of the specified virtual
             register as actual spill.

      @param[in] r A const reference to the virtual register that shall be
                   marked or unmarked as actual spill.
      @param[in] m The actual spill mark to be assigned.

      If this method is applied to a node not representing an uncolored virtual
      register, no changes are made. Marking a node as actual spill has the
      side-effect that it is no longer marked as potential spill.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setActualSpill( const WIR_VirtualRegister &, bool = true );

    /*!
      @brief isActualSpill returns whether the node of the specified virtual
             register is marked as actual spill.

      @param[in] r A const reference to the virtual register whose actual spill
                   mark shall be returned.
      @return true if the node is marked as actual spill, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isActualSpill( const WIR_VirtualRegister & ) const;

    /*!
      @brief setSpillCosts sets the spill costs of the specified virtual
             register.

      @param[in] r A const reference to the virtual register whose spill costs
                   shall be set.
      @param[in] c The costs to be assigned.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setSpillCosts( const WIR_VirtualRegister &, unsigned int );

    /*!
      @brief getSpillCosts returns the spill costs of the specified virtual
             register.

      @param[in] r A const reference to the virtual register whose spill costs
                   shall be returned.
      @return The spill costs assigned to this register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getSpillCosts( const WIR_VirtualRegister & ) const;


    //
    // Stack handling.
    //

    /*!
      @brief pushNode pushes the specified interference graph node to the
             internal stack and virtually removes all incident edges.

      @param[in] r A const reference to the virtual register whose node and
                   edges shall be pushed onto the stack.

      This method only pushes nodes representing virtual registers onto the
      stack. If it is applied to a node that is already pushed onto the stack,
      no changes are made.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void pushNode( const WIR_VirtualRegister & );

    /*!
      @brief pushPriority Node pushes the specified interference graph node to
             the internal priority stack and virtually removes all incident
             edges.

      @param[in] r A pointer to the virtual register whose node and edges shall
                   be pushed onto the stack.

      This method only pushes nodes representing virtual registers onto the
      priority stack. If it is applied to a node that is already pushed onto the
      stack, no changes are made.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void pushPriorityNode( const WIR_VirtualRegister & );

    /*!
      @brief popNode pops the top node from the internal stack(s) and re-inserts
             its node and incident edges back into the interference graph.

      @return A reference to that virtual register just popped from the stack.

      First, all high-priority nodes from mPriorityNodeStack are popped, if any,
      followed by the nodes from mNodeStack.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_VirtualRegister &popNode( void );

    /*!
      @brief isStackEmpty returns whether the interference graph's internal
             stacks of pushed nodes are empty or not.

      @return true if the stacks are empty, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isStackEmpty( void ) const;


  private:

    //
    // Local type definitions.
    //

    /*!
      @brief IGraph represents the internal boost structure of the interference
             graph.
    */
    using IGraph = WIR_UDirMGraph<IGraphNodeProperty, IGraphEdgeProperty>;

    //! IGraphVertex represents an interference graph vertex.
    using IGraphVertex =
      WIR_UDirMGraphVertex<IGraphNodeProperty, IGraphEdgeProperty>;

    //! IGraphEdge represents an interference graph edge.
    using IGraphEdge =
      WIR_UDirMGraphEdge<IGraphNodeProperty, IGraphEdgeProperty>;


    //
    // General graph properties.
    //

    /*!
      @brief addNodesForPHREGs adds interference graph nodes for all specified
             physical registers.

      Interference graph nodes will be created for all leaf registers, but not
      for parents in register hierarchies. For each added node, an individual
      color is already assigned.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addNodesForPHREGs( void );

    /*!
      @brief addNodesForVREGs adds interference graph nodes for all virtual
             registers in the specified %WIR function.

      @param[in] vregs A const reference to a subset of f's VREGs that shall be
                       represented by interference graph nodes. If this set is
                       empty, all VREGs of a %WIR function are included.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addNodesForVREGs( const WIR_VirtualRegisterSet & );

    /*!
      @brief addNode adds a node for the specified register to the
             interference graph.

      @param[in] r A const reference to the register to be added.

      If a node for the given register already exists in the interference graph,
      no duplicate node is added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addNode( const WIR_BaseRegister & );


    //
    // Coalescing.
    //

    /*!
      @brief In the course of coalescing, mergeAliases updates the internal
             alias data structure and merges the aliases related to the
             specified two registers to be coalesced.

      @param[in] r1 A const reference to the first %WIR register whose aliases
                    shall be merged with those of r2.
      @param[in] r2 A const reference to the second %WIR register whose aliases
                    shall be merged with those of r1.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void mergeAliases( const WIR_BaseRegister &, const WIR_BaseRegister & );

    /*!
      @brief In the course of coalescing, mergeColors merges the colors related
             to the specified two registers to be coalesced.

      @param[in] r1 A const reference to the first %WIR register whose colors
                    shall be merged with those of r2.
      @param[in] r2 A const reference to the second %WIR register whose colors
                    shall be merged with those of r1.
      @param[in] v1 A const reference to the interference graph node
                    representing r1.
      @param[in] v2 A const reference to the interference graph node
                    representing r2.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void mergeColors( const WIR_BaseRegister &, const WIR_BaseRegister &,
                      const IGraphVertex &, const IGraphVertex & );


    //
    // Graph visualization.
    //

    /*!
      @brief visualizeNodes dumps the interference graph nodes into a given DOT
             file.

      @param[in] dotFile A reference to a DOT file opened for writing.
      @param[out] nodeToDotInt A reference to a map mapping a %WIR register ID
                               denoting an interference graph node to an integer
                               used to identify the node in the DOT file.
      @param[in] allNodes A Boolean defaulting to 'false' and specifying whether
                          really all nodes (unpushed and pushed ones) or only
                          nodes not pushed onto the stack shall be displayed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualizeNodes( std::fstream &,
                         std::map<WIR_id_t, unsigned int> &,
                         bool = false ) const;

    /*!
      @brief visualizeEdges dumps the interference graph edges into a given DOT
             file.

      @param[in] dotFile A reference to a DOT file opened for writing.
      @param[in] nodeToDotInt A const reference to a map mapping a %WIR register
                              ID denoting an interference graph node to an
                              integer used to identify the node in the DOT file.
      @param[in] allNodes A Boolean defaulting to 'false' and specifying whether
                          really all edges (unpushed and pushed ones) or only
                          edges not pushed onto the stack shall be displayed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualizeEdges( std::fstream &,
                         const std::map<WIR_id_t, unsigned int> &,
                         bool = false ) const;

    /*!
      @brief buildNodeName returns a string denoting the specified node's name.

      @param[in] v A const reference to the graph node whose name shall be
                   built.
      @return A string containing the node's name as it will be displayed in
              dot.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string buildNodeName( const IGraphVertex & ) const;

    /*!
      @brief buildRegName returns a string denoting the specified register's
             name.

      @param[in] r A const reference to the register whose name shall be built.
      @return A string containing the register's name as it will be displayed in
              dot.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string buildRegName( const WIR_BaseRegister & ) const;

    /*!
      @brief buildNodeColor returns a string denoting the DOT name of the
             specified color number.

      @param[in] c The color number whose DOT name shall be built.
      @param[in] r A const reference to the register whose color string shall be
                   constructed.
      @return A string containing the node's color as it will be displayed in
              dot.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string buildNodeColor( unsigned int, const WIR_BaseRegister & ) const;

    /*!
      @brief buildNodeDepth returns a string denoting the specified node's loop
             nesting depth.

      @param[in] v A const reference to the graph node whose loop nesting depth
                   shall be built.
      @return A string containing the node's loop nesting depth as it will be
              displayed in dot.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string buildNodeDepth( const IGraphVertex & ) const;

    /*!
      @brief getFillColor returns a string denoting a graphviz color to be used
             for color value c.

      @param[in] c The number of a color.
      @return A string containing the graphviz name of the specified color.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getFillColor( unsigned int ) const;


    //
    // Attributes.
    //

    //! mPhregs stores the list of physical %WIR registers.
    std::list<std::reference_wrapper<const WIR_PhysicalRegister>> mPhregs;

    /*!
      @brief mFunction points to the %WIR function whose virtual registers will
             be represented in the interference graph.
    */
    WIR_Function *mFunction;

    //! mVerbosity stores whether verbose messages should be dumped.
    bool mVerbosity;

    //! mKeepTmpFiles stores whether temporary files should be kept.
    bool mKeepTmpFiles;

    /*!
      @brief mIGraph represents the internal boost graph data structure for the
             interference graph.
    */
    IGraph mIGraph;

    /*!
      @brief mColorOfPhreg maps the ID of a physical register to its pre-defined
             color.
    */
    std::map<WIR_id_t, unsigned int> mColorOfPhreg;

    /*!
      @brief mPhregOfColor maps each color to its corresponding physical
             register.

      mPhregOfColor thus is the reverse map of mColorOfPhreg.
    */
    std::map<unsigned int, std::reference_wrapper<WIR_PhysicalRegister>> mPhregOfColor;

    /*!
      @brief mNodeByRegister maps a %WIR register's ID to its representing
             interference graph node.
    */
    std::map<WIR_id_t, IGraphVertex> mNodeByRegister;

    /*!
      @brief mAvailableColors holds the number of colors available to color the
             interference graph.
    */
    unsigned int mAvailableColors;

    /*!
      @brief mAliases maps the ID of a root or leaf of a register hierarchy to
             the set of its aliasing registers as produced by coalescing.
    */
    std::map<WIR_id_t, WIR_RegisterSet> mAliases;

    /*!
      @brief mNodeStack holds the stack data structure used for removing nodes
             of virtual registers from the interference graph during graph
             coloring.
    */
    std::list<std::reference_wrapper<WIR_VirtualRegister>> mNodeStack;

    /*!
      @brief mPriorityNodeStack holds the stack data structure used for removing
             high-priority nodes of virtual registers from the interference
             graph during graph coloring.
    */
    std::list<std::reference_wrapper<WIR_VirtualRegister>> mPriorityNodeStack;

};

}       // namespace WIR

#endif  // _WIR_INTERFERENCEGRAPH_H
