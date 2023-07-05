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
  @file wirreachabilityanalysis.h
  @brief This file provides the interface of a reachability control flow
         analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_REACHABILITYANALYSIS_H
#define _WIR_REACHABILITYANALYSIS_H


//
// Include section
//

// Include boost headers
#include <boost/graph/depth_first_search.hpp>

// Include WIR headers
#include <analyses/controlflow/wircfganalysis.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_Function;


/*!
  @brief Class WIR_ReachabilityAnalysis is the %WIR reachability control flow
         analysis.

  Analysis results are stored in WIR_Reachability containers that are attached
  to %WIR basic blocks.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_ReachabilityAnalysis : public WIR_ControlFlowAnalysis
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
    explicit WIR_ReachabilityAnalysis( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_ReachabilityAnalysis( void );


  protected:

    /*!
      @brief runAnalysis performs reachability control flow analysis of the
             given function.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runAnalysis( WIR_Function & );


  private:

    /*!
      @brief init initializes data structures by attaching fresh containers to
             basic blocks.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void init( WIR_Function & );


    //
    // Reachability visitor.
    //

    class ReachabilityVisitor : public boost::default_dfs_visitor
    {

      public:

        /*!
          @brief Default constructor for reachability visitors.

          @param[in,out] c A reference to a %WIR container storing reachability
                           information for basic blocks.
          @param[in] be A Boolean flag denoting whether reachability is analyzed
                        using back edges (true) or not (false).
          @param[in] s A reference to the current CFG start node.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        ReachabilityVisitor( WIR_Reachability &c, bool, CGraphVertex & );

        /*!
          @brief discover_vertex visits a new reachable CFG node.

          @param[in] v The currently visited CFG node.
          @param[in] g A const reference to the CFG.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void discover_vertex( CGraphVertex, const CGraph & ) const;

        /*!
          @brief back_edge examines an identified back edge.

          @param[in] e The currently examined CFG edge.
          @param[in] g A const reference to the CFG.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void back_edge( CGraphEdge, const CGraph & ) const;


      private:

        /*!
          @brief mContainer refers to a reachability container to be filled
                 during the current CFG traversal.
        */
        WIR_Reachability &mContainer;

        /*!
          @brief mWithBackEdges stores whether reachability is computed with or
                 without consideration of back edges in the CFG.
        */
        bool mWithBackEdges;

        //! mStart refers to the start node of the current CFG traversal.
        CGraphVertex &mStart;

    };

};

}       // namespace WIR

#endif  // _WIR_REACHABILITYANALYSIS_H
