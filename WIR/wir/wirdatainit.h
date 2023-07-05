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
  @file wirdatainit.h
  @brief This file provides the interface of initialization data for %WIR data
         objects.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_DATAINIT_H
#define _WIR_DATAINIT_H


//
// Include section
//

// Include standard headers
#include <cstddef>
#include <list>
#include <string>

// Include WIR headers
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_DataInit represents initialization data for data objects.

  Initialization data can be of different kind, e.g., bytes, shorts, halfwords
  etc. These types of initialization data actually correspond to everything that
  can be expressed as GNU-as compatible assembly code directives for
  initialization. The actual value used for initialization is represented as
  string here for the sake of simplicity.

  Initializers of kind 'space' comes with no actual init value, since spaces
  only serve to introduce some bytes of space (e.g., for alignment) in the
  assembled output.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_DataInit final
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an initializer object with the given
             string literals.

      @tparam Args References to initializer strings.
      @param[in] t A specification of the initializer's actual type.
      @param[in] s A const reference to an initializer string.
      @param[in] __args A variadic number of const references to more
                        initializer strings.

      This constructor uses variadic templates so that argument lists of
      arbitrary lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    WIR_DataInit( WIR_DataInitType t, const std::string &s,
                  const Args&... __args ) :
      mType { t },
      mSpaces { 0 }
    {
      DSTART(
        "WIR_DataInit::WIR_DataInit(WIR_DataInitType, const string&, const Args& ...)" );

      mValues.push_back( s );
      addInitializers( __args... );
    };

    /*!
      @brief Default constructor for initialization spaces.

      @param[in] s The number of bytes for spaces in initialization data.

      This constructor sets the type of the WIR_DataInit object to
      WIR_DataInitType::ispace.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_DataInit( std::size_t );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DataInit( const WIR_DataInit & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DataInit( WIR_DataInit && );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_DataInit( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DataInit & operator = ( const WIR_DataInit & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DataInit & operator = ( WIR_DataInit && );


    //
    // Initializer handling.
    //

    /*!
      @brief getType returns the type of a %WIR initialization literal.

      @return The initializer's type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DataInitType getType( void ) const;

    /*!
      @brief getValues returns an initializer's init literals.

      @return The initializer's value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::string> &getValues( void ) const;

    /*!
      @brief For initializers of type WIR_DataInitType::ispace, getSpace returns
             the number of skipped space bytes.

      @return The number of skipped bytes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::size_t getSpace( void ) const;


    //
    // Helper functions.
    //

    /*!
      @brief escapeString converts special characters (e.g., line feeds or tabs)
             into an escaped version (e.g., \\n or \\t).

      @param[in] s A const reference to a string containing special characters.
      @return A string where special characters are safely escaped.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static std::string escapeString( const std::string & );

    /*!
      @brief unescapeString converts escape characters (e.g., \\n or \\t) into
             an unescaped version with special characters (e.g., \\n or \\t).

      @param[in] s A const reference to a string containing escaped characters.
      @return A string where with special characters.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static std::string unescapeString( const std::string & );


  private:

    /*!
      @brief No standard construction allowed.
    */
    WIR_DataInit( void ) = delete;

    /*!
      @brief addInitializers adds the given list of initializer literals to this
             initialization data object.

      @tparam Args References to initializer strings.
      @param[in] s A const reference to an initializer string.
      @param[in] args A variadic number of const references to more initializer
                      strings.

      addInitializers uses variadic templates so that argument lists of
      arbitrary lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    void addInitializers( const std::string &s, const Args&... args )
    {
      DSTART(
        "void WIR_DataInit::addInitializers(const string&, const Args& ...)" );

      mValues.push_back( s );
      addInitializers( args... );
    };

    /*!
      @brief Dummy function for adding initializer literals which does nothing.

      It only serves to terminate the recursion of the variadic method
      addInitializers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addInitializers( void ) const;

    //! mType stores the type of an initializer.
    WIR_DataInitType mType;

    //! mValues stores an initializer's values.
    std::list<std::string> mValues;

    //! mSpaces stores the number of skipped space bytes.
    std::size_t mSpaces;

};

}       // namespace WIR

#endif  // _WIR_DATAINIT_H
