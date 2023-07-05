/*

   This source file belongs to the

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


//
// Include section
//
// Include standard headers
#include <string>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  string s1( "Comment 3" ), s2( "bar.c" );
  WIR_Function f( "bar" );
  WIR_Comment c1( "Comment 1" );
  WIR_FileInfo f1( "main.c", 8 );
  WIR_Comment c2( "Comment 2" );
  WIR_FileInfo f2( "foo.c", 16 );
  WIR_Comment c3( s1 );
  WIR_FileInfo f3( s2, 24 );

  f.insertContainer( c1 );
  f.insertContainer( f1 );
  f.insertContainer( c2 );
  f.insertContainer( f2 );
  f.insertContainer( c3 );
  f.insertContainer( WIR_FileInfo( "test.c", 42 ) );
  f.insertContainer( WIR_Comment( "I like to move it move it." ) );
  f.insertContainer( f3 );

  auto allContainers = f.getContainers();
  auto allComments = f.getContainers<WIR_Comment>();
  auto allFileInfos = f.getContainers<WIR_FileInfo>();

  // Check the number of attached containers first. Since FileInfo containers
  // are unique, the total number of attached containers must not be 8.
  ufAssert( allContainers.size() == 5 );
  ufAssert( allComments.size() == 4 );
  ufAssert( allFileInfos.size() == 1 );

  // Check the contents of all these containers.
  auto it1 = allComments.begin();
  WIR_Comment &cref1 = *it1;
  ++it1;
  WIR_Comment &cref2 = *it1;
  ++it1;
  WIR_Comment &cref3 = *it1;
  ++it1;
  WIR_Comment &cref4 = *it1;
  ++it1;

  WIR_FileInfo &fref1 = *(allFileInfos.begin());

  ufAssert( cref1.getText() == c1.getText() );
  ufAssert( cref2.getText() == c2.getText() );
  ufAssert( cref3.getText() == c3.getText() );
  ufAssert( cref4.getText() == "I like to move it move it." );

  ufAssert( fref1.getFileName() == "bar.c" );
  ufAssert( fref1.getLineNumber() == 24 );

  ufAssert( cref1 != c1 );
  ufAssert( cref2 != c2 );
  ufAssert( cref3 != c3 );

  ufAssert( fref1 != f3 );

  auto it2 = allContainers.begin();
  ufAssert( (*it2++).get().getID() == cref1.getID() );
  ufAssert( (*it2++).get().getID() == cref2.getID() );
  ufAssert( (*it2++).get().getID() == cref3.getID() );
  ufAssert( (*it2++).get().getID() == cref4.getID() );
  ufAssert( (*it2++).get().getID() == fref1.getID() );

  cref2.setText( "This is a comment" );
  it1 = allComments.begin();
  ++it1;
  ufAssert( (*it1).get().getText() == "This is a comment" );
  cref2.setText( s1 );
  ufAssert( (*it1).get().getText() == "Comment 3" );

  cref2 = c2;
  ufAssert( (*it1).get().getText() == c2.getText() );
  cref2 = move( c3 );
  ufAssert( (*it1).get().getText() == "Comment 3" );
  ufAssert( c3.getText() == "" );
  WIR_Comment c4( move( c1 ) );
  ufAssert( c1.getText() == "" );
  ufAssert( c4.getText() == "Comment 1" );

  // Check the contains methods of the container API.
  ufAssert( f.containsContainers() );
  ufAssert( f.containsContainers( WIR_Comment::getContainerTypeID() ) );
  ufAssert( f.containsContainers( fref1.getContainerType() ) );
  ufAssert( f.containsContainer( cref1 ) );
  ufAssert( f.containsContainer( cref2 ) );
  ufAssert( f.containsContainer( cref3 ) );
  ufAssert( f.containsContainer( cref4 ) );
  ufAssert( f.containsContainer( fref1 ) );
  ufAssert( !f.containsContainer( c1 ) );

  // Check attached container types.
  auto ctypes = f.getContainerTypes();
  ufAssert( ctypes.size() == 2 );
  ufAssert(
    ctypes.find( WIR_Comment::getContainerTypeID() ) != ctypes.end() );
  ufAssert(
    ctypes.find( WIR_FileInfo::getContainerTypeID() ) != ctypes.end() );

  // Check erase methods of the container API.
  f.eraseContainer( fref1 );
  ufAssert( f.containsContainers() );
  ctypes = f.getContainerTypes();
  ufAssert( ctypes.size() == 1 );
  ufAssert(
    ctypes.find( WIR_Comment::getContainerTypeID() ) != ctypes.end() );

  f.eraseContainers( WIR_Comment::getContainerTypeID() );
  ufAssert( !f.containsContainers() );
  ufAssert( f.getContainerTypes().empty() );

  f.insertContainer( c1 );
  f.insertContainer( f1 );
  f.insertContainer( c2 );
  f.insertContainer( f2 );
  f.insertContainer( c3 );
  f.insertContainer( WIR_FileInfo( "test.c", 42 ) );
  f.insertContainer( WIR_Comment( "I like to move it move it." ) );
  f.insertContainer( f3 );

  // Check clear method.
  ufAssert( f.containsContainers() );
  ctypes = f.getContainerTypes();
  ufAssert( ctypes.size() == 2 );
  ufAssert(
    ctypes.find( WIR_Comment::getContainerTypeID() ) != ctypes.end() );
  ufAssert(
    ctypes.find( WIR_FileInfo::getContainerTypeID() ) != ctypes.end() );
  ufAssert( f.getContainers().size() == 5 );

  f.clearContainers();
  ufAssert( !f.containsContainers() );
  ufAssert( f.getContainerTypes().empty() );
  ufAssert( f.getContainers().empty() );

  // Test copy constructor.
  f.insertContainer( c1 );
  f.insertContainer( f1 );
  f.insertContainer( c2 );
  f.insertContainer( f2 );
  f.insertContainer( c3 );
  f.insertContainer( WIR_FileInfo( "test.c", 42 ) );
  f.insertContainer( WIR_Comment( "I like to move it move it." ) );
  f.insertContainer( f3 );

  WIR_Function fun2( f );
  ufAssert( fun2.containsContainers() );
  ctypes = fun2.getContainerTypes();
  ufAssert( ctypes.size() == 2 );
  ufAssert(
    ctypes.find( WIR_Comment::getContainerTypeID() ) != ctypes.end() );
  ufAssert(
    ctypes.find( WIR_FileInfo::getContainerTypeID() ) != ctypes.end() );
  ufAssert( fun2.getContainers().size() == 5 );

  allComments = fun2.getContainers<WIR_Comment>();
  it1 = allComments.begin();
  ufAssert( (*it1++).get().getText() == c1.getText() );
  ufAssert( (*it1++).get().getText() == c2.getText() );
  ufAssert( (*it1++).get().getText() == c3.getText() );
  ufAssert( (*it1++).get().getText() == "I like to move it move it." );
  ufAssert(
    fun2.getContainers<WIR_FileInfo>().begin()->get().getFileName() ==
      "bar.c" );

  // Test move constructor.
  WIR_Function fun3( move( fun2 ) );
  ufAssert( !fun2.containsContainers() );
  ufAssert( fun3.containsContainers() );
  ctypes = fun3.getContainerTypes();
  ufAssert( ctypes.size() == 2 );
  ufAssert(
    ctypes.find( WIR_Comment::getContainerTypeID() ) != ctypes.end() );
  ufAssert(
    ctypes.find( WIR_FileInfo::getContainerTypeID() ) != ctypes.end() );
  ufAssert( fun3.getContainers().size() == 5 );

  allComments = fun3.getContainers<WIR_Comment>();
  it1 = allComments.begin();
  ufAssert( (*it1++).get().getText() == c1.getText() );
  ufAssert( (*it1++).get().getText() == c2.getText() );
  ufAssert( (*it1++).get().getText() == c3.getText() );
  ufAssert( (*it1++).get().getText() == "I like to move it move it." );
  ufAssert(
    fun3.getContainers<WIR_FileInfo>().begin()->get().getFileName() ==
      "bar.c" );

  // Test copy-assignment.
  WIR_Function fun4( "abc" );
  fun4 = fun3;
  ufAssert( fun4.containsContainers() );
  ctypes = fun4.getContainerTypes();
  ufAssert( ctypes.size() == 2 );
  ufAssert(
    ctypes.find( WIR_Comment::getContainerTypeID() ) != ctypes.end() );
  ufAssert(
    ctypes.find( WIR_FileInfo::getContainerTypeID() ) != ctypes.end() );
  ufAssert( fun4.getContainers().size() == 5 );

  allComments = fun4.getContainers<WIR_Comment>();
  it1 = allComments.begin();
  ufAssert( (*it1++).get().getText() == c1.getText() );
  ufAssert( (*it1++).get().getText() == c2.getText() );
  ufAssert( (*it1++).get().getText() == c3.getText() );
  ufAssert( (*it1++).get().getText() == "I like to move it move it." );
  ufAssert(
    fun4.getContainers<WIR_FileInfo>().begin()->get().getFileName() ==
      "bar.c" );

  // Test move-assignment.
  WIR_Function fun5( "xyz" );
  fun5 = move( fun4 );
  ufAssert( !fun4.containsContainers() );
  ufAssert( fun5.containsContainers() );
  ctypes = fun5.getContainerTypes();
  ufAssert( ctypes.size() == 2 );
  ufAssert(
    ctypes.find( WIR_Comment::getContainerTypeID() ) != ctypes.end() );
  ufAssert(
    ctypes.find( WIR_FileInfo::getContainerTypeID() ) != ctypes.end() );
  ufAssert( fun5.getContainers().size() == 5 );

  allComments = fun5.getContainers<WIR_Comment>();
  it1 = allComments.begin();
  ufAssert( (*it1++).get().getText() == c1.getText() );
  ufAssert( (*it1++).get().getText() == c2.getText() );
  ufAssert( (*it1++).get().getText() == c3.getText() );
  ufAssert( (*it1++).get().getText() == "I like to move it move it." );
  ufAssert(
    fun5.getContainers<WIR_FileInfo>().begin()->get().getFileName() ==
      "bar.c" );

  return( 0 );
}
