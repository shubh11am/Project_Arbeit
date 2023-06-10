/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2009 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

// Include local headers
#include "composed_struct_bitfield_incl.h"
#include "codesel/addresswithoffset.h"

//! Representation of uninitialized memory to be used with init lists.
/*! Initialization lists may not init a composed object fully.
    The uninit areas can be scattered around in the objects memory when
    designators are used. This class keeps track of all uninit areas
    so that they can be zero initialized.
*/
class UninitializedMemoryArea {

  public:

  //! Construct a new instance with a 'size' byte memory area.
  explicit UninitializedMemoryArea( unsigned int size );

  //! Recursively parse an InitListExp to determine the uninitialized parts.
  void splitByInitExpressions( IR_InitListExp *initList, int offset = 0);
  //! Explicitly mark an area [start, end) as initialized.
  void splitByArea( unsigned int start, unsigned int end );
  //! Apply initWithZero() to all uninit memory chunks.
  void zeroMemory( AddressWithOffset addr, IR_Exp *exp = nullptr );
  //! Calculate the cost for zeroing the memory
  COST zeroMemoryCost( unsigned int alignment );

  private:

  //! A chunk in this context is a region in memory described by a half-open interval [start, end).
  struct Chunk {
    unsigned int start;
    unsigned int end;
  };

  //! A list containing all uninitialized chunks.
  std::list<struct Chunk> mChunks;
};

