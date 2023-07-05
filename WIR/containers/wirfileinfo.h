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
  @file wirfileinfo.h
  @brief This file provides the interface of a %WIR container representing
         file locations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_FILEINFO_H
#define _WIR_FILEINFO_H


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
  @brief Class WIR_FileInfo models file locations, i.e., positions denoted by
         file names and line numbers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_FileInfo : public WIR_Container<WIR_FileInfo>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @param[in] s A const reference to a file name to be copied.
      @param[in] l A line number.

      Any newline characters in the file name are removed, since this class
      models single-line names only.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_FileInfo( const std::string &, unsigned long long );

    /*!
      @brief Default constructor.

      @param[in] s An R-value reference to a file name to be moved.
      @param[in] l A line number.

      Any newline characters in the file name are removed, since this class
      models single-line names only.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_FileInfo( std::string &&, unsigned long long );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_FileInfo( const WIR_FileInfo & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_FileInfo( WIR_FileInfo && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_FileInfo( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_FileInfo & operator = ( const WIR_FileInfo & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_FileInfo & operator = ( WIR_FileInfo && );

    /*!
      @brief isUnique returns whether file information is unique, i.e., whether
             at most one instance of this container type can be attached to a
             %WIR class.

      @return Always true, file information is unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;


    //
    // File information handling.
    //

    /*!
      @brief setFileName sets a file name.

      @param[in] s A const reference to a string to be copied that holds the
                   file name.

      Any newline characters in the file name are removed, since this class
      models single-line names only.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setFileName( const std::string & );

    /*!
      @brief setFileName sets a file name.

      @param[in] s An R-value reference to a string to be moved that holds the
                   file name.

      Any newline characters in the file name are removed, since this class
      models single-line names only.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setFileName( std::string && );

    /*!
      @brief getFileName returns the file name.

      @return A string that holds the file name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getFileName( void ) const;

    /*!
      @brief setLineNumber sets a line number.

      @param[in] l A new line number.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setLineNumber( unsigned long long );

    /*!
      @brief getLineNumber returns the line number.

      @return The line number.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long long getLineNumber( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps %WIR file information to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] o A const reference to the %WIR fileinfo to be dumped.
      @return A reference to the same output stream.

      By applying processor-specific I/O manipulators to the output stream
      beforehand, this << operator can flexibly emit valid assembly output for
      arbitrary processor architectures.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &, const WIR_FileInfo & );


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_FileInfo( const std::string &, unsigned long long ) instead.
    */
    WIR_FileInfo( void ) = delete;

    //! mFileName holds the file name.
    std::string mFileName;

    //! mLineNumber holds the line number.
    unsigned long long mLineNumber;

};

}       // namespace WIR

#endif  // _WIR_FILEINFO_H
