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
#include <sstream>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <analyses/bit/wirupdownvalue.h>
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  MIPS p;
  WIR_Function f( "foo" );
  MIPS_RegV &x =
    static_cast<MIPS_RegV &>( f.pushBackVirtualRegister( MIPS_RegV() ) );
  WIR_Operation o1
    { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
      WIR_RegisterParameter( x, WIR_Usage::def ),
      WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
      MIPS_Immediate16_Signed( 2 ) };
  WIR_Operation o2
    { MIPS::OpCode::J, MIPS::OperationFormat::L, WIR_LabelParameter( f ) };

  // Test default constructors.
  WIR_UpDownValue v1 { 32 };
  WIR_UpDownValue v2 { WIR_L4::bX, 23, true };
  WIR_UpDownValue v3 { MIPS_Immediate16_Signed( 26361 ) };

  // Test << operator.
  stringstream str;

  str << v1;
  ufAssert( str.str() == "U^32" );

  str.str( std::string() );
  str << v2;
  ufAssert( str.str() == "X^23" );

  str.str( std::string() );
  str << v3;
  ufAssert( str.str() == "26361" );

  v3.setBit( 15, WIR_L4::b1 );
  str.str( std::string() );
  str << v3;
  ufAssert( str.str() == "-6407" );

  v3.setBit( 11, WIR_L4::bX );
  v3.setBit( 5, WIR_L4::bU );
  str.str( std::string() );
  str << v3;
  ufAssert( str.str() == "1110 X110 11U1 1001" );

  return( 0 );
}
