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
  @file wirdata.h
  @brief This file provides the interface of %WIR data objects.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_DATA_H
#define _WIR_DATA_H


//
// Include section
//

// Include standard headers
#include <cstddef>
#include <functional>
#include <list>
#include <ostream>
#include <string>

// Include WIR headers
#include <wir/API/wircontainerapi.h>
#include <wir/API/wiridapi.h>
#include <wir/API/wirinsertionapi.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_CompilationUnit;
class WIR_DataInit;


/*!
  @brief Class WIR_Data is the generic representation of data objects.

  Here, data objects refer to every piece of data that is visible globally or
  within a compilation unit. Furthermore, function-internal data that is
  declared as static is also modeled by WIR_Data. Basically, WIR_Data models all
  kinds of data that is not allocated in the stack.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Data final : public WIR_ID_API,
                       public WIR_Container_API
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty data object.

      @param[in] s A const reference to a string to be copied that holds the
                   data object's name.

      This constructor asserts if it is passed an empty string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Data( const std::string & );

    /*!
      @brief Default constructor creating an empty data object.

      @param[in] s An R-value reference to a string to be moved that holds the
                   data object's name.

      This constructor asserts if it is passed an empty string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Data( std::string && );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      When copying a data object that is inserted in some %WIR compilation unit,
      the resulting copy will not be inserted in a compilation unit. Copying a
      data object implies that the newly created data is set as
      getDontOptimize() == false.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Data( const WIR_Data & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a data object that is inserted in some %WIR compilation
      unit results in an assertion, since you are not allowed to move data
      objects whose ownership is managed by a %WIR compilation unit.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Data( WIR_Data && );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_Data( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When copying a data object that is inserted in some %WIR compilation unit,
      the resulting copy will not be inserted in a compilation unit. Copying a
      data object implies that the newly created data is set as
      getDontOptimize() == false.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Data & operator = ( const WIR_Data & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a data object that is inserted in some %WIR compilation
      unit results in an assertion, since you are not allowed to move data
      objects whose ownership is managed by a %WIR compilation unit.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Data & operator = ( WIR_Data && );


    //
    // Compilation unit handling.
    //

    // Realize the API to manage a data object's parent compilation unit.
    WIR_INSERTION_DECL( WIR_CompilationUnit, CompilationUnit );


    //
    // Name handling.
    //

    /*!
      @brief setName sets a data object's specific name.

      @param[in] s A const reference to a string to be copied that holds the
                   data object's name.

      setName asserts if it is passed an empty string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setName( const std::string & );

    /*!
      @brief setName sets a data object's specific name.

      @param[in] s An R-value reference to a string to be moved that holds the
                   data object's name.

      setName asserts if it is passed an empty string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setName( std::string && );

    /*!
      @brief getName returns a data object's specific name.

      @return A string that holds the data object's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getName( void ) const;


    //
    // Size computation.
    //

    /*!
      @brief setSize sets a data object's size in bytes.

      @param[in] s The data object's byte size.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setSize( std::size_t );

    /*!
      @brief getSize returns a data object's size in bytes.

      @return The data object's byte size.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::size_t getSize( void ) const;


    //
    // Initialization data handling.
    //

    /*!
      @brief isInitialized returns whether a data object is initialized or not.

      @return true iff the data object is initialized, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isInitialized( void ) const;

    /*!
      @brief pushBackInitData adds a new WIR_DataInit at the end of list
             mInitData, after its current last element.

      @param[in] o A const reference to the WIR_DataInit to be copy-added.
      @return A reference to the newly inserted element.

      The content of o is copied to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DataInit &pushBackInitData( const WIR_DataInit & );

    /*!
      @brief pushBackInitData adds a new WIR_DataInit at the end of list
             mInitData, after its current last element.

      @param[in] o An R-value reference to the WIR_DataInit to be move-added.
      @return A reference to the newly inserted element.

      The content of o is moved to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DataInit &pushBackInitData( WIR_DataInit && );

    /*!
      @brief clearInitData removes all elements from list mInitData.

      This destroys all removed elements.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearInitData( void );

    /*!
      @brief getInitData returns the list mInitData.

      @return A const reference to the list mInitData.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_DataInit>> &getInitData( void ) const;

    /*!
      @brief begin returns an iterator to the first initializer of a data
             object.

      @return A const iterator pointing to the first initializer.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_DataInit>>::const_iterator begin( void ) const;

    /*!
      @brief end returns an iterator to the end of a data objects's initializer
             list.

      @return A const iterator pointing to the position after the last
              initializer of a data object.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_DataInit>>::const_iterator end( void ) const;

    /*!
      @brief foldSpaces folds subsequent space entries in the initialization
             list into one combined space entry.

      This destroys all removed elements.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void foldSpaces( void );


    //
    // Modification prevention.
    //

    /*!
      @brief setDontOptimize sets whether a data object can be modified or must
             not be changed by some optimization or transformation.

      @param[in] f A Boolean flag denoting whether the data object must not be
                   modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setDontOptimize( bool = true );

    /*!
      @brief getDontOptimize returns whether a data object can be modified or
             must not be changed by some optimization or transformation.

      @return true if the data object must not be modified, false otherwise.

      A data object must not be modified if the data by itself has been marked
      as such using setDontOptimize, or if it is inserted into a %WIR
      compilation unit that in turn must not be modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getDontOptimize( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR data object to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] d A const reference to the %WIR data object to be dumped.
      @return A reference to the same output stream.

      By applying processor-specific I/O manipulators to the output stream
      beforehand, this << operator can flexibly emit valid assembly output for
      arbitrary processor architectures.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &, const WIR_Data & );


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_Data( std::string ) instead.
    */
    WIR_Data( void ) = delete;

    /*!
      @brief checkDontOptimize checks whether a data object must not be
             modified.

      If this data object must not be modified, checkDontOptimize asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkDontOptimize( void ) const;

    /*!
      @brief mDontOptimize stores whether a data object can be modified or must
             not be changed by some optimization or transformation.
    */
    bool mDontOptimize;

    //! mName holds a data object's name.
    std::string mName;

    //! mSize holds a data object's size in bytes.
    std::size_t mSize;

    //! mInitData holds all stored initialization data.
    std::list<WIR_DataInit> mInitData;

    /*!
      @brief mInitDataReferences holds (wrapped) references to all stored
             initialization data.
    */
    std::list<std::reference_wrapper<WIR_DataInit>> mInitDataReferences;

};

}       // namespace WIR

#endif  // _WIR_DATA_H
