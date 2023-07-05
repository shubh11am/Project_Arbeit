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
  @file wirflowfactref.h
  @brief This file provides the interface of a %WIR container representing
         references of flow facts to basic blocks.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/


#ifndef _WIR_FLOWFACTREF_H
#define _WIR_FLOWFACTREF_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>
#include <ostream>

// Include WIR headers
#include <wir/wircontainer.h>
#include <flowfacts/wirflowfact.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_FlowFact;


/*!
  @brief Class WIR_FlowFactRef holds all flow facts a basic block is referenced
         by.

  This class provides comfortable access methods to those flow facts concerning
  the basic block the FlowFactRef is built for.

  The contained references to flow facts are managed by the flow facts and a
  %WIR System automatically.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
class WIR_FlowFactRef final : public WIR_Container<WIR_FlowFactRef>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_FlowFactRef( const WIR_FlowFactRef & );

    /*!
      @brief Destructor.
      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual ~WIR_FlowFactRef( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_FlowFactRef & operator = ( const WIR_FlowFactRef & );

    /*!
      @brief isUnique returns whether flow fact references are unique, i.e.,
             whether at most one instance of this container type can be attached
             to a %WIR class.

      @returns Always true, flow fact references are unique.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual bool isUnique( void ) const override;


    //
    // Static get method.
    //

    /*!
      @brief get returns the FlowFactRef attached to a given basic block.

      @param[in,out] b A reference to the basic block to get the FlowFactRef
                       for.
      @return A reference to a FlowFactRef associated with the given basic
              block.

      get either returns the WIR_FlowFactRef already attached to the given basic
      block or, in case that it currently does not contain a FlowFactRef,
      creates a new one and attaches it automatically.

      @note This is the recommended method of getting access to FlowFactRefs.
            Constructors are for internal use only.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    static WIR_FlowFactRef &get( WIR_BasicBlock & );


    //
    // Access to referenced flow facts.
    //

    /*!
      @brief getFlowfacts returns a list of all flow facts belonging to this
             basic block.

      @return A const reference to a list of (wrapped) flow fact references.

      Only flow facts which are part of a %WIR System are returned.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_FlowFact>> &getFlowFacts( void ) const;

    /*!
      @brief This variant of getFlowfacts returns a correctly casted set of flow
             facts of type DerivedFlowfactClass.

      @tparam DerivedFlowFactClass The name of a derived flow fact class.
      @return A list containing (wrapped) references to all attached flow facts
              of template type DerivedFlowfactClass. The returned references are
              already correctly casted.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    template<typename DerivedFlowFactClass>
    const std::list<std::reference_wrapper<DerivedFlowFactClass>> getFlowFacts( void ) const
    {
      DSTART(
        "const list<reference_wrapper<DerivedFlowFactClass> > "
        "WIR_FlowFactRef::getFlowFacts() const" );

      std::list<std::reference_wrapper<DerivedFlowFactClass>> res;
      WIR_FlowFactType ffType = DerivedFlowFactClass().getType();

      for ( WIR_FlowFact &f : mFlowFacts )
        if ( ffType == f.getType() )
          res.push_back( dynamic_cast<DerivedFlowFactClass &>( f ) );

      return( res );
    };


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps all flow facts stored by this container to an
             output stream.

      @param[in] os A reference to an output stream.
      @param[in] c A const reference to the %WIR flow fact reference container
                   to be dumped.
      @return A reference to the same output stream.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_FlowFactRef & );


  private:

    friend class WIR_FlowFact;
    friend class WIR_FlowRestriction;
    friend class WIR_LoopBound;

    //
    // Handling of flow facts.
    //

    /*!
      @brief Default constructor.

      @param[in,out] b A reference to the basic block for which this FlowFactRef
                       is created.

      This constructor creates an empty FlowFactRef for the given basic block
      and attaches it as a container to the given basic block.

      @note Only use this constructor when dynamically allocating a new
            FlowFactRef.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    explicit WIR_FlowFactRef( WIR_BasicBlock & );

    /*!
      @brief pushBackFlowFact adds a reference to a flow fact.

      @param[in] f A const reference to a WIR_FlowFact.

      pushBackFlowFact is used by the flow facts to tell the FlowFactRef that
      they concern it and are part of a %WIR System.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void pushBackFlowFact( const WIR_FlowFact & );

    /*!
      @brief eraseFlowFact erases a reference to a flow fact.

      @param[in] f A reference to a WIR_FlowFact to be erased.

      eraseFlowFact is used by the flow facts to tell the FlowFactRef that they
      no longer concern it or are removed from a %WIR System.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void eraseFlowFact( WIR_FlowFact & );

    //! mFlowFacts holds all flow facts referencing the current basic block.
    std::list<std::reference_wrapper<WIR_FlowFact>> mFlowFacts;

};

}       // namespace WIR

#endif  // _WIR_FLOWFACTREF_H
