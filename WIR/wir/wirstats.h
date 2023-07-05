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
  @file wirstats.h
  @brief This file provides the interface of %WIR code statistics.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_STATS_H
#define _WIR_STATS_H


//
// Include section
//

// Include standard headers
#include <set>

// Include WIR headers
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_CompilationUnit;
class WIR_Function;
class WIR_Instruction;
class WIR_Operation;
class WIR_Parameter;
class WIR_System;


/*!
  @brief Class WIR_Stats represents counters of %WIR objects that can be used
         to compute how many objects have been added or removed during during
         two reference points in time.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Stats final
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty statistics.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Stats( void );

    /*!
      @brief Default constructor for system-level statistics.

      @param[in] o A const reference to a WIR_System to be analyzed.

      The %WIR objects counted in o during construction serve as first reference
      point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Stats( const WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level analysis.

      @param[in] o A const reference to a WIR_CompilationUnit to be analyzed.

      The %WIR objects counted in o during construction serve as first reference
      point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Stats( const WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level analysis.

      @param[in] o A const reference to a WIR_Function to be analyzed.

      The %WIR objects counted in o during construction serve as first reference
      point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Stats( const WIR_Function & );

    /*!
      @brief Default constructor for basic block-level analysis.

      @param[in] o A const reference to a WIR_BasicBlock to be analyzed.

      The %WIR objects counted in o during construction serve as first reference
      point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Stats( const WIR_BasicBlock & );

    /*!
      @brief Default constructor for instruction-level analysis.

      @param[in] o A const reference to a WIR_Instruction to be analyzed.

      The %WIR objects counted in o during construction serve as first reference
      point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Stats( const WIR_Instruction & );

    /*!
      @brief Default constructor for operation-level analysis.

      @param[in] o A const reference to a WIR_Operation to be analyzed.

      The %WIR objects counted in o during construction serve as first reference
      point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Stats( const WIR_Operation & );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Stats( const WIR_Stats & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Stats( WIR_Stats && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_Stats( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Stats & operator = ( const WIR_Stats & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Stats & operator = ( WIR_Stats && );

    /*!
      @brief The == operator checks for equality of statistics.

      @param[in] __o A const reference to another object to be compared.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool operator == ( const WIR_Stats & ) const;

    /*!
      @brief The != operator checks for inequality of statistics.

      @param[in] __o A const reference to another object to be compared.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool operator != ( const WIR_Stats & ) const;


    //
    // Statistics handling.
    //

    /*!
      @brief count counts the numbers of %WIR objects for system-level
             statistics.

      @param[in] o A const reference to a WIR_System to be analyzed.

      The %WIR objects counted in o serve as second reference point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void count( const WIR_System & );

    /*!
      @brief count counts the numbers of %WIR objects for compilation unit-level
             statistics.

      @param[in] o A const reference to a WIR_CompilationUnit to be analyzed.

      The %WIR objects counted in o serve as second reference point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void count( const WIR_CompilationUnit & );

    /*!
      @brief count counts the numbers of %WIR objects for function-level
             statistics.

      @param[in] o A const reference to a WIR_Function to be analyzed.

      The %WIR objects counted in o serve as second reference point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void count( const WIR_Function & );

    /*!
      @brief count counts the numbers of %WIR objects for basic block-level
             statistics.

      @param[in] o A const reference to a WIR_BasicBlock to be analyzed.

      The %WIR objects counted in o serve as second reference point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void count( const WIR_BasicBlock & );

    /*!
      @brief count counts the numbers of %WIR objects for instruction-level
             statistics.

      @param[in] o A const reference to a WIR_Instruction to be analyzed.

      The %WIR objects counted in o serve as second reference point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void count( const WIR_Instruction & );

    /*!
      @brief count counts the numbers of %WIR objects for operation-level
             statistics.

      @param[in] o A const reference to a WIR_Operation to be analyzed.

      The %WIR objects counted in o serve as second reference point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void count( const WIR_Operation & );

    /*!
      @brief rotate associates the data collected for the second reference point
             with the first reference point.

      This way, WIR_Stats supports incremental counting of %WIR objects over
      multiple code transformation steps.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void rotate( void );

    /*!
      @brief clear removes all counted elements from the statistics.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clear( void );

    /*!
      @brief empty returns whether a statistics is empty.

      @return true if no %WIR objects were counted at all, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool empty( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief setIndentation sets the number of white spaces to be displayed at
             the very beginning of each new line when dumping a %WIR statistics
             with the << operator.

      @param[in] i The number of white spaces for indentation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setIndentation( unsigned int );

    /*!
      @brief The << operator dumps a %WIR statistics to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] s A const reference to the %WIR stats to be dumped.
      @return A reference to the same output stream.

      If the statistics to be dumped contains only one reference point (i.e.,
      the statistics objects has just been created using the above
      constructors), the absolute number of %WIR objects counted by the
      constructors is dumped. If a second reference point has been counted by
      using some of the count methods above, the difference between the two
      reference points in terms of added/deleted %WIR objects is dumped.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &, const WIR_Stats & );


  private:

    /*!
      @brief count collects the IDs of %WIR objects for system-level statistics.

      @param[in] s A const reference to a WIR_System to be analyzed.
      @param[in,out] sid A reference to a set of WIR_System IDs.
      @param[in,out] cid A reference to a set of WIR_CompilationUnit IDs.
      @param[in,out] did A reference to a set of WIR_Data IDs.
      @param[in,out] fid A reference to a set of WIR_Function IDs.
      @param[in,out] bid A reference to a set of WIR_BasicBlock IDs.
      @param[in,out] iid A reference to a set of WIR_Instruction IDs.
      @param[in,out] oid A reference to a set of WIR_Operation IDs.
      @param[in,out] pid A reference to a set of WIR_Parameter IDs.
      @param[in,out] rid A reference to a set of WIR_BaseRegister IDs.

      The IDs of %WIR objects are collected in the corresponding sets passed as
      arguments.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void count( const WIR_System &, std::set<WIR_id_t> &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> & );

    /*!
      @brief count collects the IDs of %WIR objects for compilation unit-level
             statistics.

      @param[in] c A const reference to a WIR_CompilationUnit to be analyzed.
      @param[in,out] cid A reference to a set of WIR_CompilationUnit IDs.
      @param[in,out] did A reference to a set of WIR_Data IDs.
      @param[in,out] fid A reference to a set of WIR_Function IDs.
      @param[in,out] bid A reference to a set of WIR_BasicBlock IDs.
      @param[in,out] iid A reference to a set of WIR_Instruction IDs.
      @param[in,out] oid A reference to a set of WIR_Operation IDs.
      @param[in,out] pid A reference to a set of WIR_Parameter IDs.
      @param[in,out] rid A reference to a set of WIR_BaseRegister IDs.

      The IDs of %WIR objects are collected in the corresponding sets passed as
      arguments.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void count( const WIR_CompilationUnit &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> & );

    /*!
      @brief count collects the IDs of %WIR objects for function-level
             statistics.

      @param[in] f A const reference to a WIR_Function to be analyzed.
      @param[in,out] fid A reference to a set of WIR_Function IDs.
      @param[in,out] bid A reference to a set of WIR_BasicBlock IDs.
      @param[in,out] iid A reference to a set of WIR_Instruction IDs.
      @param[in,out] oid A reference to a set of WIR_Operation IDs.
      @param[in,out] pid A reference to a set of WIR_Parameter IDs.
      @param[in,out] rid A reference to a set of WIR_BaseRegister IDs.

      The IDs of %WIR objects are collected in the corresponding sets passed as
      arguments.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void count( const WIR_Function &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> & );

    /*!
      @brief count collects the IDs of %WIR objects for basic block-level
             statistics.

      @param[in] b A const reference to a WIR_BasicBlock to be analyzed.
      @param[in,out] bid A reference to a set of WIR_BasicBlock IDs.
      @param[in,out] iid A reference to a set of WIR_Instruction IDs.
      @param[in,out] oid A reference to a set of WIR_Operation IDs.
      @param[in,out] pid A reference to a set of WIR_Parameter IDs.
      @param[in,out] rid A reference to a set of WIR_BaseRegister IDs.

      The IDs of %WIR objects are collected in the corresponding sets passed as
      arguments.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void count( const WIR_BasicBlock &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> &, std::set<WIR_id_t> & );

    /*!
      @brief count collects the IDs of %WIR objects for instruction-level
             statistics.

      @param[in] i A const reference to a WIR_Instruction to be analyzed.
      @param[in,out] iid A reference to a set of WIR_Instruction IDs.
      @param[in,out] oid A reference to a set of WIR_Operation IDs.
      @param[in,out] pid A reference to a set of WIR_Parameter IDs.
      @param[in,out] rid A reference to a set of WIR_BaseRegister IDs.

      The IDs of %WIR objects are collected in the corresponding sets passed as
      arguments.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void count( const WIR_Instruction &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> & );

    /*!
      @brief count collects the IDs of %WIR objects for operation-level
             statistics.

      @param[in] o A const reference to a WIR_Operation to be analyzed.
      @param[in,out] oid A reference to a set of WIR_Operation IDs.
      @param[in,out] pid A reference to a set of WIR_Parameter IDs.
      @param[in,out] rid A reference to a set of WIR_BaseRegister IDs.

      The IDs of %WIR objects are collected in the corresponding sets passed as
      arguments.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void count( const WIR_Operation &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> &, std::set<WIR_id_t> & );

    /*!
      @brief count collects the IDs of %WIR objects for parameter-level
             statistics.

      @param[in] p A const reference to a WIR_Parameter to be analyzed.
      @param[in,out] pid A reference to a set of WIR_Parameter IDs.
      @param[in,out] rid A reference to a set of WIR_BaseRegister IDs.

      The IDs of %WIR objects are collected in the corresponding sets passed as
      arguments.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void count( const WIR_Parameter &, std::set<WIR_id_t> &,
                std::set<WIR_id_t> & );

    /*!
      @brief clearSets1 erases all data associated with a statistic's first
             reference point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearSets1( void );

    /*!
      @brief clearSets2 erases all data associated with a statistic's second
             reference point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearSets2( void );

    /*!
      @brief diff computes how many IDs appear in the first set but not in the
             second one.

      @param[in] s1 A first set of IDs of %WIR objects.
      @param[in] s2 A second set of IDs of %WIR objects.
      @return The number of IDs from set s1 that are not in set s2.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int diff( const std::set<WIR_id_t> &,
                       const std::set<WIR_id_t> & ) const;

    //! mSystems1 is the first set of IDs of %WIR systems.
    std::set<WIR_id_t> mSystems1;

    //! mCompilationUnits1 is the first set of IDs of compilation units.
    std::set<WIR_id_t> mCompilationUnits1;

    //! mFunctions1 is the first set of IDs of functions.
    std::set<WIR_id_t> mFunctions1;

    //! mBasicBlocks1 is the first set of IDs of basic blocks.
    std::set<WIR_id_t> mBasicBlocks1;

    //! mInstructions1 is the first set of IDs of instructions.
    std::set<WIR_id_t> mInstructions1;

    //! mOperations1 is the first set of IDs of operations.
    std::set<WIR_id_t> mOperations1;

    //! mParameters1 is the first set of IDs of parameters.
    std::set<WIR_id_t> mParameters1;

    //! mRegisters1 is the first set of IDs of registers.
    std::set<WIR_id_t> mRegisters1;

    //! mDataObjects1 is the first set of IDs of data objects.
    std::set<WIR_id_t> mDataObjects1;

    //! mSystems2 is the second set of IDs of %WIR systems.
    std::set<WIR_id_t> mSystems2;

    //! mCompilationUnits2 is the second set of IDs of compilation units.
    std::set<WIR_id_t> mCompilationUnits2;

    //! mFunctions2 is the second set of IDs of functions.
    std::set<WIR_id_t> mFunctions2;

    //! mBasicBlocks2 is the second set of IDs of basic blocks.
    std::set<WIR_id_t> mBasicBlocks2;

    //! mInstructions2 is the second set of IDs of instructions.
    std::set<WIR_id_t> mInstructions2;

    //! mOperations2 is the second set of IDs of operations.
    std::set<WIR_id_t> mOperations2;

    //! mParameters2 is the second set of IDs of parameters.
    std::set<WIR_id_t> mParameters2;

    //! mRegisters2 is the second set of IDs of registers.
    std::set<WIR_id_t> mRegisters2;

    //! mDataObjects2 is the second set of IDs of data objects.
    std::set<WIR_id_t> mDataObjects2;

    //! mIndent holds the number of white spaces for indentation.
    unsigned int mIndent;

};

}       // namespace WIR

#endif  // _WIR_STATS_H
