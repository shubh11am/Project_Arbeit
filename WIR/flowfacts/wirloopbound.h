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
  @file wirloopbound.h
  @brief This file provides the interface of %WIR loop bounds.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/


#ifndef _WIR_LOOPBOUND_H
#define _WIR_LOOPBOUND_H


//
// Include section
//

// Include standard headers
#include <map>
#include <ostream>

// Include WIR headers
#include <flowfacts/wirflowfact.h>


//
// Header Section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_System;


/*!
  @brief Class WIR_LoopBound provides a description of minimum and maximum loop
         iteration bounds.

  A loop bound specifies the minimum and the maximum number of iterations (more
  precisely: tests of a loop's exit condition as in aiT).

  Iff min (and max) == -1: no loop bound is available.
  This is implemented to detected loop bounds even if no actual numbers for loop
  bounds were specified yet. This is interesting for the translation to
  CRL2 (-> aiT) which needs a single annotation for every loop.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
class WIR_LoopBound final : public WIR_FlowFact
{

  public:

    //
    // Local type definitions.
    //

    /*!
      @brief This enum specifies the type of a loop.
    */
    enum class LoopControlType : char
    {
      //! Specifier for head-controlled loops (for and while-do loops).
      headcontrolled,

      //! Specifier for tail-controlled loop (do-while loops).
      tailcontrolled,

      //! Unknown loop control.
      unknown
    };


    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty loop bound.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_LoopBound( void );

    /*!
      @brief Constructor creating a non-empty loop bound.

      @param[in] _min Minimum number of iterations.
      @param[in] _max Maximum number of iterations.
      @param[in] _loop The loop, the iterations are specified for.
      @param[in] _ctrl A specifier defaulting to unknown whether this loop is
                       head- or tail-controlled.

      Ideally, the basic block specified by the third parameter is the one
      containing the condition of the loop. The user is responsible for not
      annotating one loop by more than one loop bound!

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_LoopBound( int, int, const WIR_BasicBlock &,
                   LoopControlType = LoopControlType::unknown );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      The copy will not be inserted in any %WIR system or referenced by any
      WIR_FlowFactRefs.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_LoopBound( const WIR_LoopBound & );

    /*!
      @brief Destructor.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual ~WIR_LoopBound( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      The copy will not be inserted in any %WIR system or referenced by any
      WIR_FlowFactRefs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LoopBound & operator = ( const WIR_LoopBound & );


    //
    // Generic type handling.
    //

    /*!
      @brief getType returns the type of a %WIR flow fact, i.e., whether it is
             an entry point, flow restriction or loop bound.

      @return WIR_FlowFactType::loopbound

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual WIR_FlowFactType getType( void ) const override;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR loop bound to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] l A const reference to the %WIR loop bound to be dumped.
      @return A reference to the same output stream.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &, const WIR_LoopBound & );


    //
    // Manipulation of loop bounds.
    //

    /*!
      @brief setMin sets the minimum number of iterations.

      @param[in] min A signed integer denoting the new minimum number of loop
                     iterations.

      The new minimum has to be lower than the current maximum. If the minimum
      is set to -1 (no loop bound available), the maximum is also updated.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void setMin( int );

    /*!
      @brief setMax sets the maximum number of iterations.

      @param[in] max A signed integer denoting the new maximum number of loop
                     iterations.

      The new maximum has to be larger than the current minimum. If the minimum
      is -1, it is set to zero, because a loop bound is now specified.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void setMax( int );

    /*!
      @brief setLoop sets the loop this loop bound is referring to.

      @param[in] l A const reference to a %WIR basic block denoting the new
                   loop's entry for this loop bound.
      @param[in] ctrl A specifier defaulting to unknown whether this loop is
                      head- or tail-controlled.

      @note When using this method, the user is responsible not to annotate one
            loop with more than one loop bound (a loop can consist of more than
            one basic block!).

      This method invokes the update of FlowFactRefs.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void setLoop( const WIR_BasicBlock &,
                  LoopControlType = LoopControlType::unknown );


    //
    // Access to loop bounds.
    //

    /*!
      @brief getMin returns the minimum number of iterations specified in this
             loop bound.

      @return A signed integer denoting the minimum number of iterations, or -1
              if no loop bound is available.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    int getMin( void ) const;

    /*!
      @brief getMax returns the maximum number of iterations specified in this
             loop bound.

      @return A signed integer denoting the maximum number of iterations, or -1
              if no loop bound is available.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    int getMax( void ) const;

    /*!
      @brief getLoop returns one basic block of the loop this loop bound
             annotates.

      @return A const reference to the annotated loop.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    const WIR_BasicBlock &getLoop( void ) const;

    /*!
      @brief getLoopControlType returns the control type of the loop (e.g.,
             head-controlled).

      @return The control type of the annotated loop.

      For some WCET analyzers, it may be required to add one additional
      iteration for head-controlled loops for the last back-edge invocation
      before the loop terminates (e.g., aiT needs this).

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    LoopControlType getLoopControlType( void ) const;


    //
    // Evaluation.
    //

    /*!
      @brief isBoundSpecified returns whether actual loop bounds are specified.

      @return true if any loop bounds are specified, false otherwise.

      This method does not consider whether this loop bound refers to an actual
      loop or not.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    bool isBoundSpecified( void ) const;

    /*!
      @brief isSignificant returns whether a loop bound is significant for WCET
             calculation or not.

      @return true if bounds are specified, false otherwise.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual bool isSignificant( void ) const override;


    //
    // Administration.
    //

    /*!
      @brief reorganize adjusts all references to %WIR basic blocks stored by a
             flow fact after a deep copy of flow facts.

      @param[in] blockIDMap A const reference to a map translating the IDs of
                            the original basic blocks to the new basic block
                            "replacing" it.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual void reorganize( const std::map<WIR_id_t, WIR_BasicBlock *> & ) override;


  protected:

    /*!
      @brief onInsert is called whenever this loop bound is added to a
             WIR_System.

      @param[in] s A pointer to the WIR_System to which this object is added. If
                   no pointer is specified, this loop bound is marked as not to
                   be assigend to any WIR_System at all.

      This method inserts a reference of itself into the FlowFactRef of the
      basic block this loop bound is specified for.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual void onInsert( WIR_System * = nullptr ) override;

    /*!
      @brief clone creates a copy of a %WIR loop bound.

      @return A pointer to the newly created copy of this loop bound.

      This method only calls the copy constructor and allocates a new IR loop
      bound on the heap.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual WIR_FlowFact *clone( void ) const override;


  private:

    /*!
      @brief doStructuralAnalysis applies %WIR's structural analysis to the
             given basic block to determine if it in fact refers to a loop.

      @param[in] b A const reference to a basic block containing the loop exit
                   condition.
      @return true if the given basic block does contain the exit condition of a
              loop, false otherwise.

      @note Currently, this method will not return false, but rather fails with
            an assertion to make debugging easier.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    bool doStructuralAnalysis( const WIR_BasicBlock & ) const;

    /*!
      @brief mMin holds the minimum number of iterations for the loop specified
             by @ref mLoop.
    */
    int mMin;

    /*!
      @brief mMax holds the maximum number of iterations for the loop specified
             by @ref mLoop.
    */
    int mMax;

    /*!
      @brief mLoop points to the loop this loop bound is specified for.

      Refers only to one basic block within the loop.

      This basic block is not(!) unique for the identification of the loop.
    */
    const WIR_BasicBlock *mLoop;

    /*!
      @brief mLoopControlType holds the control type of the loop referred to by
             @ref mLoop.
    */
    LoopControlType mLoopControlType;

};

}       // namespace WIR

#endif  // _WIR_LOOPBOUND_H
