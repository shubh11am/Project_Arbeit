/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file wirentrypoint.h
  @brief This file provides the interface of %WIR entry points, a type of flow
         fact marking functions as entry points for execution.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/


#ifndef _WIR_ENTRYPOINT_H
#define _WIR_ENTRYPOINT_H


//
// Include section
//

// Include standard headers
#include <map>
#include <ostream>
#include <set>
#include <string>

// Include WIR headers
#include <flowfacts/wirflowfact.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_Function;
class WIR_System;


/*!
  @brief Class WIR_EntryPoint provides a description of entry points where
         control flow can start, e.g., in multi-task applications.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
class WIR_EntryPoint : public WIR_FlowFact
{
  public:

    //
    // Constructor and destructors
    //

    /*!
      @brief Default constructor creating an empty entry point.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_EntryPoint( void );

    /*!
      @brief Constructor creating an entry point referring to a %WIR function.

      @param[in] __f A const reference to the %WIR function this entry point
                     describes.
      @param[in] __a Specifies general purpose attributes of an entry point.

      TODO: Some more documentation about this map and an entry point's possible
            attributes! Some more intelligent way to handle attributes than
            plain strings!?

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_EntryPoint( const WIR_Function &,
                    const std::map<std::string, std::string> & );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      The copy will not be inserted in any %WIR system or referenced by any
      WIR_FlowFactRefs.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_EntryPoint( const WIR_EntryPoint & );

    /*!
      @brief Destructor.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual ~WIR_EntryPoint( void );

    /*!
      @brief Copy-assignement operator.

      @param[in] __o A const reference to another object to be copied.

      The copy will not be inserted in any %WIR system or referenced by any
      WIR_FlowFactRefs.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_EntryPoint & operator = ( const WIR_EntryPoint & );


    //
    // Generic type handling.
    //

    /*!
      @brief getType returns the type of a %WIR flow fact, i.e., whether it is
             an entry point, flow restriction or loop bound.

      @return WIR_FlowFactType::entrypoint

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual WIR_FlowFactType getType( void ) const override;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR entry point to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] ep A const reference to the %WIR entry point to be dumped.
      @return A reference to the same output stream.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_EntryPoint & );


    //
    // Manipulation of entry points.
    //

    /*!
      @brief setFunction sets the %WIR function this entry point describes.

      @param[in] f A const reference to a %WIR function to refer to.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void setFunction( const WIR_Function & );


    //
    // Access to entry points.
    //

    /*!
      @brief getFunction returns the function marked as entry point.

      @return A reference to the %WIR function marked as entry point.

      getFunction fails with an assertion if the entry point's function is not
      set.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_Function &getFunction( void ) const;

    /*!
      @brief getEntryPointNames determines the names of all functions within a
             given %WIR system that are marked as entry point.

      @param[in] sys A const reference to a %WIR system to extract entry points
                     from.
      @return A set of strings with the names of all functions being entry
              points.

      getEntryPointNames adds all entry points found in a given %WIR system to
      the returned set. If no entry points are present in a %WIR system, the
      "main" function is returned as the default entry point.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    static std::set<std::string> getEntryPointNames( const WIR_System & );

    /*!
      @brief getAttributes returns an entry point's map of general purpose
             attributes.

      @return A const reference to the map containing general purpose
              attributes.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    const std::map<std::string, std::string> &getAttributes( void ) const;


    //
    // Administration.
    //

    /*!
      @brief reorganize adjusts all references to %WIR functions stored by an
             entry point after a deep copy of flow facts.

      @param[in] blockIDMap A const reference to a map translating the IDs of
                            the original basic blocks to the new basic block
                            "replacing" it.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual void reorganize( const std::map<WIR_id_t, WIR_BasicBlock *> & ) override;


  protected:

    /*!
      @brief onInsert is called whenever this entry point is added to a
             WIR_System.

      @param[in] s A pointer to the WIR_System to which this object is added. If
                   no pointer is specified, this entry point is marked as
                   not to be assigend to any WIR_System at all.

      This method adds references of itself to that basic block's FlowfactRef
      that is the first basic block of the function marked as an entry point.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void onInsert( WIR_System * = nullptr ) override;

    /*!
      @brief clone creates a copy of a %WIR entry point.

      @return A pointer to the newly created copy of this entry point.

      This method only calls the copy constructor and allocates a new %WIR
      entry point on the heap.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual WIR_FlowFact *clone( void ) const override;


  private:

    /*!
      @brief mFunctionPointer points to the %WIR function being marked as an
             entry point.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_Function *mFunctionPointer;

    /*!
      @brief mAttributes holds general purpose attributes attached to an
             entry point.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    std::map<std::string, std::string> mAttributes;

};

}       // namespace WIR

#endif  // _WIR_ENTRYPOINT_H
