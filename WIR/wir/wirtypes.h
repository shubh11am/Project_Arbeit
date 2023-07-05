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
  @file wirtypes.h
  @brief This file provides several simple basic data types that are used here
         and there within the %WIR library.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIRTYPES_H
#define _WIRTYPES_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <set>
#include <list>
#include <stdint.h>
#include <string>
#include <utility>

// Include boost headers
#include <boost/graph/adjacency_list.hpp>

// Include WIR headers
#include <analyses/generic/wirinterval.h>
#include <analyses/generic/wirintervalset.h>
#include <analyses/generic/wirmodulointervalset.h>
#include <analyses/generic/wirsaturatingint.h>
#include <wir/API/wiridapi.h>
#include <wir/API/wirinheritableenum.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseRegister;
class WIR_BasicBlock;
class WIR_BaseContainer;
class WIR_ControlTreeNode;
class WIR_Data;
class WIR_FlowFact;
class WIR_Instruction;
class WIR_MemoryRegion;
class WIR_Operation;
class WIR_PhysicalRegister;
class WIR_RegisterParameter;
class WIR_Section;
class WIR_Symbol;
class WIR_SystemComponent;
class WIR_VirtualRegister;


//
// Global data type declarations
//

//! This type represents pairs of basic blocks.
using WIR_BasicBlockPair =
  std::pair<std::reference_wrapper<WIR_BasicBlock>,
            std::reference_wrapper<WIR_BasicBlock>>;


//! This type represents an address in memory.
using WIR_MemoryAddress = WIR_SaturatingInt<uint32_t>;


//! This type represents a memory address range.
using WIR_AddressRange = WIR_Interval<WIR_MemoryAddress>;


//! This type represents sets of memory address ranges.
using WIR_AddressRangeSet = typename WIR_AddressRange::intervalSet;


//! This type represents displacements between memory locations.
using WIR_disp_t = signed long long;


//! This type represents offsets within a TDMA bus schedule.
using WIR_TDMAOffset = uint16_t;


//! This type represents the base type for TDMA offset analysis.
using WIR_OffsetIntervalSet =
  WIR_ModuloIntervalSet<WIR_TDMAOffset, WIR_IntervalCompression::permanent>;


/*!
  @brief This enum represents different arbitration strategies of %WIR buses.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_BusArbitrationType : char
{
  //! Fixed-Priority arbitration.
  fp,

  //! Priority-Division arbitration.
  pd,

  //! Round-Robin arbitration.
  rr,

  //! Time-Division Multiple Access (TDMA) arbitration.
  tdma
};


/*!
  @brief This enum represents different kinds of %WIR initialization data, e.g.,
         bytes, shorts, halfwords etc.

  These types of initialization data actually correspond to everything that can
  be expressed as GNU-as compatible assembly code directives for initialization.

  Initializers of kind 'space' serve to introduce some bytes of space (e.g., for
  alignment) in the assembled output.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_DataInitType : char
{
  //! A string literal initializer.
  iascii,

  //! A zero-terminated string literal initializer.
  iasciz,

  //! A byte literal initializer.
  ibyte,

  //! A double-float literal initializer.
  idouble,

  //! A double-word literal initializer.
  idword,

  //! A float literal initializer.
  ifloat,

  //! A half-word literal initializer.
  ihword,

  //! A short-word literal initializer.
  ishort,

  //! No literal, only some skipped space in the assembly output.
  ispace,

  //! A symbolic pointer literal initializer.
  isymbol,

  //! A word literal initializer.
  iword
};


/*!
  @brief This enum represents different types of %WIR parameters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_ParameterType : char
{
  //! An addressing mode parameter.
  addr,

  //! A condition field parameter.
  cond,

  //! An immediate parameter.
  imm,

  //! A label parameter.
  label,

  //! A register parameter.
  reg,

  //! A string parameter.
  str
};


/*!
  @brief This enum represents different types of scheduling constraints between
         %WIR instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_SchedulingConstraintType : char
{
  //! Instructions that always have to be direct neighbors in a fixed order.
  sequential,

  /*!
    @brief Instructions that have to occur in a fixed order, but maybe
           interspersed with other instructions.
  */
  ordered
};


/*!
  @brief This enum represents the usage of %WIR parameters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_Usage : char
{
  //! A parameter that is defined.
  def,

  //! A parameter that is both defined and used at the same time.
  defuse,

  //! A parameter that is used.
  use
};


/*!
  @brief This enum represents different attributes of %WIR memory regions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_MemoryRegionAttributes : unsigned char
{
  //! To be used for memory regions without any specific attributes.
  none = 0,

  //! To be used for memory regions that can be read.
  read = 1 << 0,

  //! To be used for memory regions that can be written.
  write = 1 << 1,

  //! To be used for memory regions that can be executed.
  execute = 1 << 2,

  //! To be used for memory regions that are allocated.
  allocated = 1 << 3,

  //! To be used for memory regions that are initialized.
  initialized = 1 << 4,

  //! To be used for memory regions that are cached.
  cached = 1 << 5,

  //! Covers all existing attributes.
  all = ( cached << 1 ) - 1
};


/*!
  @brief This enum represents different kinds of symbols.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_SymbolType : char
{
  //! Basic block symbols.
  block,

  //! Data symbols.
  data,

  //! Function symbols.
  function
};


/*!
  @brief This enum represents different kinds of system components.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_SystemComponentType : char
{
  //! A bus.
  bus,

  //! A cache.
  cache,

  //! A core.
  core,

  //! A memory region.
  memory,

  //! An ELF executable file section.
  section,

  //! A %WIR system itself.
  system
};


/*!
  @brief This enum represents different kinds of flow facts.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
enum class WIR_FlowFactType : char
{
  //! An entry point.
  entrypoint,

  //! A flow restriction.
  flowrestriction,

  //! A loop bound.
  loopbound
};


//
// Comparator classes and comparison operators
//

/*!
  @brief This operator performs a less-than comparison of two %WIR objects
         featuring unique IDs.

  @param[in] lhs A const reference to the left-hand operand.
  @param[in] rhs A const reference to the right-hand operand.
  @return true iff the left operand is less than the right one, false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
inline bool operator < ( const WIR_ID_API &lhs, const WIR_ID_API &rhs )
{
  return( lhs.getID() < rhs.getID() );
};


/*!
  @brief WIR_Compare is a comparator class that is used to sort, e.g., sets of
         %WIR classes uniquely.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
template<typename T>
struct WIR_Compare
{
  inline bool operator()( const std::reference_wrapper<T> &lhs,
                          const std::reference_wrapper<T> &rhs ) const
  {
    return( lhs.get().getID() < rhs.get().getID() );
  };
};


/*!
  @brief WIR_Compare_BasicBlockPair is a comparator class that is used to sort,
         e.g., sets of basic block pairs uniquely.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
struct WIR_Compare_BasicBlockPair
{
  bool operator()( const WIR_BasicBlockPair &lhs,
                   const WIR_BasicBlockPair &rhs ) const;
};


/*!
  @brief This operator performs a less-than comparison of two WIR_MemoryRegions.

  @param[in] lhs A const reference to the left-hand operand.
  @param[in] rhs A const reference to the right-hand operand.
  @return true iff the left operand is less than the right one, false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
bool operator < ( const WIR_MemoryRegion &lhs, const WIR_MemoryRegion &rhs );


/*!
  @brief WIR_Compare_MemoryRegions is a comparator class that is used to sort,
         e.g., sets of %WIR memory regions uniquely by their base addresses.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
struct WIR_Compare_MemoryRegions
{
  bool operator()( const std::reference_wrapper<WIR_MemoryRegion> &lhs,
                   const std::reference_wrapper<WIR_MemoryRegion> &rhs ) const;
};


//
// Sets of WIR classes
//

/*!
  @brief WIR_BasicBlockPairSet represents sets of basic block pairs.
*/
using WIR_BasicBlockPairSet =
  std::set<WIR_BasicBlockPair, WIR_Compare_BasicBlockPair>;


/*!
  @brief WIR_BasicBlockSet represents sets of references to %WIR basic blocks.
*/
using WIR_BasicBlockSet =
  std::set<std::reference_wrapper<WIR_BasicBlock>, WIR_Compare<WIR_BasicBlock>>;


/*!
  @brief WIR_ContainerSet represents sets of references to %WIR containers.
*/
using WIR_ContainerSet =
  std::set<std::reference_wrapper<WIR_BaseContainer>,
           WIR_Compare<WIR_BaseContainer>>;


/*!
  @brief WIR_ControlTreeNodeSet represents sets of control tree nodes.
*/
using WIR_ControlTreeNodeSet =
  std::set<std::reference_wrapper<WIR_ControlTreeNode>,
           WIR_Compare<WIR_ControlTreeNode>>;


/*!
  @brief WIR_DataSet represents sets of references to %WIR data objects.
*/
using WIR_DataSet =
  std::set<std::reference_wrapper<WIR_Data>, WIR_Compare<WIR_Data>>;


/*!
  @brief WIR_InstructionSet represents sets of references to %WIR instructions.
*/
using WIR_InstructionSet =
  std::set<std::reference_wrapper<WIR_Instruction>,
           WIR_Compare<WIR_Instruction>>;


/*!
  @brief WIR_MemoryRegionSet represents sets of references to %WIR memory
         regions.
*/
using WIR_MemoryRegionSet =
  std::set<std::reference_wrapper<WIR_MemoryRegion>, WIR_Compare_MemoryRegions>;


/*!
  @brief WIR_SectionSet represents sets of references to %WIR sections.
*/
using WIR_SectionSet =
  std::set<std::reference_wrapper<WIR_Section>, WIR_Compare<WIR_Section>>;


/*!
  @brief WIR_SymbolSet represents sets of references to %WIR symbols.
*/
using WIR_SymbolSet =
  std::set<std::reference_wrapper<WIR_Symbol>, WIR_Compare<WIR_Symbol>>;


/*!
  @brief WIR_SystemComponentSet represents sets of references to %WIR system
         components.
*/
using WIR_SystemComponentSet =
  std::set<std::reference_wrapper<WIR_SystemComponent>,
           WIR_Compare<WIR_SystemComponent>>;


/*!
  @brief WIR_OperationSet represents sets of references to %WIR operations.
*/
using WIR_OperationSet =
  std::set<std::reference_wrapper<WIR_Operation>, WIR_Compare<WIR_Operation>>;


/*!
  @brief WIR_PhysicalRegisterSet represents sets of references to physical %WIR
         registers.
*/
using WIR_PhysicalRegisterSet =
  std::set<std::reference_wrapper<WIR_PhysicalRegister>,
           WIR_Compare<WIR_PhysicalRegister>>;


/*!
  @brief WIR_RegisterParameterSet represents sets of references to %WIR register
         parameters.
*/
using WIR_RegisterParameterSet =
  std::set<std::reference_wrapper<WIR_RegisterParameter>,
           WIR_Compare<WIR_RegisterParameter>>;


/*!
  @brief WIR_RegisterSet represents sets of references to %WIR registers.
*/
using WIR_RegisterSet =
  std::set<std::reference_wrapper<WIR_BaseRegister>,
           WIR_Compare<WIR_BaseRegister>>;


/*!
  @brief WIR_VirtualRegisterSet represents sets of references to virtual %WIR
         registers.
*/
using WIR_VirtualRegisterSet =
  std::set<std::reference_wrapper<WIR_VirtualRegister>,
           WIR_Compare<WIR_VirtualRegister>>;


//
// Comparison operators for WIR sets
//

/*!
  @brief This operator compares two WIR_RegisterSets for equality.

  @param[in] lhs A const reference to the left-hand operand.
  @param[in] rhs A const reference to the right-hand operand.
  @return true iff both operands are equal, false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
bool operator == ( const WIR_RegisterSet &lhs, const WIR_RegisterSet &rhs );


/*!
  @brief This operator compares two WIR_RegisterSets for inequality.

  @param[in] lhs A const reference to the left-hand operand.
  @param[in] rhs A const reference to the right-hand operand.
  @return true iff both operands are inequal, false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
bool operator != ( const WIR_RegisterSet &lhs, const WIR_RegisterSet &rhs );


//
// Graphs
//

/*!
  @brief WIR_DirMGraph represents arbitrary directed multi-graphs with custom
         vertex and edge properties and both in- and out-edges.

  @tparam VT This template argument is used to specify the graph's vertex
             property type.
  @tparam ET This template argument is used to specify the graph's edge property
             type.

  The Boost graph is directed with support for parallel edges between two nodes,
  i.e., it is a multi-graph. Furthermore, there is support for efficient access
  to both out- and in-edges.
*/
template<class VT, class ET>
using WIR_DirMGraph =
  boost::adjacency_list<boost::vecS, boost::vecS, boost::bidirectionalS,
                        VT, ET>;


/*!
  @brief WIR_DirMGraphVertex represents a directed multi-graph's vertex.

  @tparam VT This template argument is used to specify the graph's vertex
             property type.
  @tparam ET This template argument is used to specify the graph's edge property
             type.
*/
template<class VT, class ET>
using WIR_DirMGraphVertex =
  typename boost::graph_traits<WIR_DirMGraph<VT, ET>>::vertex_descriptor;


/*!
  @brief WIR_DirMGraphEdge represents a directed multi-graph's edge.

  @tparam VT This template argument is used to specify the graph's vertex
             property type.
  @tparam ET This template argument is used to specify the graph's edge property
             type.
*/
template<class VT, class ET>
using WIR_DirMGraphEdge =
  typename boost::graph_traits<WIR_DirMGraph<VT, ET>>::edge_descriptor;


/*!
  @brief WIR_DirGraph represents arbitrary directed graphs with custom vertex
         and edge properties and both in- and out-edges.

  @tparam VT This template argument is used to specify the graph's vertex
             property type.
  @tparam ET This template argument is used to specify the graph's edge property
             type.

  The Boost graph is directed without support for parallel edges between two
  nodes. Furthermore, there is support for efficient access to both out- and
  in-edges.
*/
template<class VT, class ET>
using WIR_DirGraph =
  boost::adjacency_list<boost::setS, boost::vecS, boost::bidirectionalS,
                        VT, ET>;


/*!
  @brief WIR_DirGraphVertex represents a directed graph's vertex.

  @tparam VT This template argument is used to specify the graph's vertex
             property type.
  @tparam ET This template argument is used to specify the graph's edge property
             type.
*/
template<class VT, class ET>
using WIR_DirGraphVertex =
  typename boost::graph_traits<WIR_DirGraph<VT, ET>>::vertex_descriptor;


/*!
  @brief WIR_DirGraphEdge represents a directed graph's edge.

  @tparam VT This template argument is used to specify the graph's vertex
             property type.
  @tparam ET This template argument is used to specify the graph's edge property
             type.
*/
template<class VT, class ET>
using WIR_DirGraphEdge =
  typename boost::graph_traits<WIR_DirGraph<VT, ET>>::edge_descriptor;


/*!
  @brief WIR_UDirMGraph represents arbitrary undirected multi-graphs with custom
         vertex and edge properties.

  @tparam VT This template argument is used to specify the graph's vertex
             property type.
  @tparam ET This template argument is used to specify the graph's edge property
             type.

  The Boost graph is undirected with support for parallel edges between two
  nodes, i.e., it is a multi-graph.
*/
template<class VT, class ET>
using WIR_UDirMGraph =
  boost::adjacency_list<boost::vecS, boost::vecS, boost::undirectedS,
                        VT, ET>;


/*!
  @brief WIR_UDirMGraphVertex represents an undirected multi-graph's vertex.

  @tparam VT This template argument is used to specify the graph's vertex
             property type.
  @tparam ET This template argument is used to specify the graph's edge property
             type.
*/
template<class VT, class ET>
using WIR_UDirMGraphVertex =
  typename boost::graph_traits<WIR_UDirMGraph<VT, ET>>::vertex_descriptor;


/*!
  @brief WIR_UDirMGraphEdge represents an undirected multi-graph's edge.

  @tparam VT This template argument is used to specify the graph's vertex
             property type.
  @tparam ET This template argument is used to specify the graph's edge property
             type.
*/
template<class VT, class ET>
using WIR_UDirMGraphEdge =
  typename boost::graph_traits<WIR_UDirMGraph<VT, ET>>::edge_descriptor;

}       // namespace WIR

#endif  // _WIRTYPES_H
