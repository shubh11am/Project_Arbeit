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
  @file wircomment.h
  @brief This file provides the interface of a %WIR container representing
         comments.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_COMMENT_H
#define _WIR_COMMENT_H


//
// Include section
//

// Include standard headers
#include <string>

// Include WIR headers
#include <wir/wircontainer.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_Comment models single-line comments.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Comment : public WIR_Container<WIR_Comment>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @param[in] s A const reference to a comment string to be copied.

      Any newline characters in the comment text are removed, since this class
      models single-line comments only.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Comment( const std::string & );

    /*!
      @brief Default constructor.

      @param[in] s An R-value reference to a comment string to be moved.

      Any newline characters in the comment text are removed, since this class
      models single-line comments only.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Comment( std::string && );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Comment( const WIR_Comment & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Comment( WIR_Comment && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Comment( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Comment & operator = ( const WIR_Comment & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Comment & operator = ( WIR_Comment && );

    /*!
      @brief isUnique returns whether comments are unique, i.e., whether at most
             one instance of this container type can be attached to a %WIR
             class.

      @return Always false, comments are not unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;


    //
    // Comment text handling.
    //

    /*!
      @brief setText sets a comment's text string.

      @param[in] s A const reference to a string to be copied that holds the
                   comment's text.

      Any newline characters in the comment text are removed, since this class
      models single-line comments only.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setText( const std::string & );

    /*!
      @brief setText sets a comment's text string.

      @param[in] s An R-value reference to a string to be moved that holds the
                   comment's text.

      Any newline characters in the comment text are removed, since this class
      models single-line comments only.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setText( std::string && );

    /*!
      @brief getText returns a comment's text.

      @return A string that holds the comment's text.

      Any newline characters in the comment text are removed by getText, since
      this class models single-line comments only.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getText( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR comment to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] o A const reference to the %WIR comment to be dumped.
      @return A reference to the same output stream.

      By applying processor-specific I/O manipulators to the output stream
      beforehand, this << operator can flexibly emit valid assembly output for
      arbitrary processor architectures.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &, const WIR_Comment & );


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_Comment( const std::string & ) instead.
    */
    WIR_Comment( void ) = delete;

    //! mCommentText holds a comment's text.
    std::string mCommentText;

};

}       // namespace WIR

#endif  // _WIR_COMMENT_H
