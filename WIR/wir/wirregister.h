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
  @file wirregister.h
  @brief This file provides the template-based interface of both physical and
         virtual %WIR registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_REGISTER_H
#define _WIR_REGISTER_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <iostream>
#include <list>
#include <string>
#include <memory>
#include <vector>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wirbaseregister.h>
#include <wir/wirfunction.h>
#include <wir/wirmisc.h>
#include <wir/wirregistry.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseProcessor;
class WIR_PhysicalRegister;


/*!
  @brief Class WIR_GenericRegister models generic registers, be it virtual or
         physical processor registers.

  @tparam DerivedRegisterClass This template argument is used to specify the
                               actual type of register to be instantiated.
  @tparam Virtual This Boolean template argument is used to distinguish between
                  virtual and physical registers in template instantiations. Set
                  it to true if you instantiate a virtual register class,
                  otherwise use false.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
template <typename DerivedRegisterClass, bool Virtual>
class WIR_GenericRegister : public WIR_BaseRegister
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for registers.

      @param[in] __r A const reference to a register type to be used.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_GenericRegister( const WIR_BaseProcessor::RegisterType &__r ) :
      WIR_BaseRegister { __r }
    {
      DSTART(
        "WIR_GenericRegister<DerivedRegisterClass, Virtual>::WIR_GenericRegister(const WIR_BaseProcessor::RegisterType&)" );
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_GenericRegister( const WIR_GenericRegister &__o ) :
      WIR_BaseRegister { __o }
    {
      DSTART(
        "WIR_GenericRegister<DerivedRegisterClass, Virtual>::WIR_GenericRegister(const WIR_GenericRegister<DerivedRegisterClass, Virtual>&)" );
    };

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_GenericRegister( WIR_GenericRegister &&__o ) :
      WIR_BaseRegister { std::move( __o ) },
      mChildReferences { std::move( __o.mChildReferences ) }
    {
      DSTART(
        "WIR_GenericRegister<DerivedRegisterClass, Virtual>::WIR_GenericRegister(WIR_GenericRegister<DerivedRegisterClass, Virtual>&&)" );

      __o.mChildReferences.clear();
    };

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_GenericRegister( void )
    {
      DSTART(
        "WIR_GenericRegister<DerivedRegisterClass, Virtual>::~WIR_GenericRegister()" );
    };

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_GenericRegister & operator = ( const WIR_GenericRegister &__o )
    {
      DSTART(
        "WIR_GenericRegister<DerivedRegisterClass, Virtual>& WIR_GenericRegister<DerivedRegisterClass, Virtual>::operator=(const WIR_GenericRegister<DerivedRegisterClass, Virtual>&)" );

      WIR_BaseRegister::operator = ( __o );

      return( *this );
    };

    /*!
      @brief Move-assignment operator.

      @param[in] __o A const reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_GenericRegister & operator = ( WIR_GenericRegister &&__o )
    {
      DSTART(
        "WIR_GenericRegister<DerivedRegisterClass, Virtual>& WIR_GenericRegister<DerivedRegisterClass, Virtual>::operator=(WIR_GenericRegister<DerivedRegisterClass, Virtual>&&)" );

      WIR_BaseRegister::operator = ( std::move( __o ) );

      mChildReferences = std::move( __o.mChildReferences );
      __o.mChildReferences.clear();

      return( *this );
    };


    //
    // Virtual register handling.
    //

    /*!
      @brief isVirtual returns whether a register is virtual.

      @return true if the register is virtual, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isVirtual( void ) const
    {
      DSTART(
        "bool WIR_GenericRegister<DerivedRegisterClass, Virtual>::isVirtual() const" );

      return( Virtual );
    };


    //
    // Register hierarchy handling.
    //

    /*!
      @brief getChilds returns the list mChildReferences.

      @return A const reference to the list mChildReferences.

      Child registers are returned in little-endian order. I.e., the
      least-significant child register is the first element of the returned
      list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<DerivedRegisterClass>> &getChilds( void ) const
    {
      DSTART(
        "const list<reference_wrapper<DerivedRegisterClass> >& WIR_GenericRegister<DerivedRegisterClass, Virtual>::getChilds() const" );

      return( mChildReferences );
    };

    /*!
      @brief begin returns an iterator to the first child register.

      @return A const iterator pointing to the first child.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_iterator begin( void ) const
    {
      DSTART(
        "typename list<reference_wrapper<DerivedRegisterClass> >::const_iterator WIR_GenericRegister<DerivedRegisterClass, Virtual>::begin() const" );

      return( mChildReferences.begin() );
    };

    /*!
      @brief end returns an iterator to the end of the child register list.

      @return A const iterator pointing to the position after the last child
              register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_iterator end( void ) const
    {
      DSTART(
        "typename list<reference_wrapper<DerivedRegisterClass> >::const_iterator WIR_GenericRegister<DerivedRegisterClass, Virtual>::end() const" );

      return( mChildReferences.end() );
    };

    /*!
      @brief rbegin returns an iterator to the reverse-first child register.

      @return A const iterator pointing to the last child.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_reverse_iterator rbegin( void ) const
    {
      DSTART(
        "typename list<reference_wrapper<DerivedRegisterClass> >::const_reverse_iterator WIR_GenericRegister<DerivedRegisterClass, Virtual>::rbegin() const" );

      return( mChildReferences.rbegin() );
    };

    /*!
      @brief rend returns an iterator to the reverse-end of the child register
             list.

      @return A const iterator pointing to the position before the first child
              register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_reverse_iterator rend( void ) const
    {
      DSTART(
        "typename list<reference_wrapper<DerivedRegisterClass> >::const_reverse_iterator WIR_GenericRegister<DerivedRegisterClass, Virtual>::rend() const" );

      return( mChildReferences.rend() );
    };

    /*!
      @brief containsChild returns whether list mChildReferences contains a %WIR
             register with the specified ID.

      @param[in] id An object's ID to be found.
      @return true if mChildReferences contains an object with the given ID,
              false otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            list's length. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool containsChild( WIR_id_t id ) const
    {
      DSTART(
        "bool WIR_GenericRegister<DerivedRegisterClass, Virtual>::containsChild(WIR_id_t) const" );

      for ( DerivedRegisterClass &item : mChildReferences )
        if ( item.getID() == id )
          return( true );

      return( false );
    };

    /*!
      @brief containsChild returns whether list mChildReferences contains the
             specified %WIR register.

      @param[in] o A const reference to the %WIR register to be found.
      @return true if mChildReferences contains the specified object, false
              otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            list's length. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsChild( const DerivedRegisterClass &o ) const
    {
      DSTART(
        "bool WIR_GenericRegister<DerivedRegisterClass, Virtual>::containsChild(const DerivedRegisterClass&) const" );

      for ( DerivedRegisterClass &item : mChildReferences )
        if ( item == o )
          return( true );

      return( false );
    };

    /*!
      @brief findChild finds a %WIR register with the specified ID in list
             mChildReferences.

      @param[in] id An object's ID to be found.
      @return An iterator pointing to the found element with the specified ID,
              or the end() iterator otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            list's length. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_iterator findChild( WIR_id_t id ) const
    {
      DSTART(
        "typename list<reference_wrapper<DerivedRegisterClass> >::const_iterator WIR_GenericRegister<DerivedRegisterClass, Virtual>::findChild(WIR_id_t) const" );

      for ( auto it = mChildReferences.begin(); it != mChildReferences.end();
            ++it )
        if ( (*it).get().getID() == id )
          return( it );

      return( mChildReferences.end() );
    };

    /*!
      @brief findChild finds the specified %WIR register in list
             mChildReferences.

      @param[in] o A const reference to the %WIR register to be found.
      @return An iterator pointing to the found element, or the end() iterator
              otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            list's length. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_iterator findChild( const DerivedRegisterClass &o ) const
    {
      DSTART(
        "typename list<reference_wrapper<DerivedRegisterClass> >::const_iterator WIR_GenericRegister<DerivedRegisterClass, Virtual>::findChild(const DerivedRegisterClass&) const" );

      for ( auto it = mChildReferences.begin(); it != mChildReferences.end();
            ++it )
        if ( (*it).get() == o )
          return( it );

      return( mChildReferences.end() );
    };

    /*!
      @brief getParent returns a register's parent register.

      @return A reference to the parent register, or a reference to itself if it
              has no parent.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedRegisterClass &getParent( void ) const
    {
      DSTART(
        "DerivedRegisterClass& WIR_GenericRegister<DerivedRegisterClass, Virtual>::getParent() const" );

      if ( mParent == nullptr ) {
        WIR_GenericRegister<DerivedRegisterClass, Virtual> &r =
          const_cast<WIR_GenericRegister<DerivedRegisterClass, Virtual> &>(
            *this );
        return( static_cast<DerivedRegisterClass &>( r ) );
      } else
        return( static_cast<DerivedRegisterClass &>( *mParent ) );
    };

    /*!
      @brief getRoot returns the root of a complex register hierarchy that this
             register is part of.

      @return A reference to the root register, or a reference to itself if it
              the root itself.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedRegisterClass &getRoot( void ) const
    {
      DSTART(
        "DerivedRegisterClass& WIR_GenericRegister<DerivedRegisterClass, Virtual>::getRoot() const" );

      if ( mParent == nullptr ) {
        WIR_GenericRegister<DerivedRegisterClass, Virtual> &r =
          const_cast<WIR_GenericRegister<DerivedRegisterClass, Virtual> &>(
            *this );
        return( static_cast<DerivedRegisterClass &>( r ) );
      } else
        return( getParent().getRoot() );
    };

    /*!
      @brief hasChilds returns whether a register has child registers.

      @return true if the register has childs, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool hasChilds( void ) const
    {
      DSTART(
        "bool WIR_GenericRegister<DerivedRegisterClass, Virtual>::hasChilds() const" );

      return( mChildReferences.size() != 0 );
    };

    /*!
      @brief getLeafs returns all leafs of the register hierarchy rooted by this
             object, i.e., all registers that have no childs.

      @return A vector containing all leaf registers in little-endian order. If
              the current register has no childs, the returned vector contains
              only the current register.

      Leaf registers are returned in little-endian order. I.e., the
      least-significant leaf register is the first element of the returned
      vector.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::vector<std::reference_wrapper<DerivedRegisterClass>> getLeafs( void ) const
    {
      DSTART(
        "const vector<reference_wrapper<DerivedRegisterClass> > WIR_GenericRegister<DerivedRegisterClass, Virtual>::getLeafs() const" );

      std::vector<std::reference_wrapper<DerivedRegisterClass>> res;
      std::list<DerivedRegisterClass *> q;
      WIR_GenericRegister<DerivedRegisterClass, Virtual> *p =
        const_cast<WIR_GenericRegister<DerivedRegisterClass, Virtual> *>(
          this );
      q.push_back( static_cast<DerivedRegisterClass *>( p ) );

      while ( !q.empty() ) {
        DerivedRegisterClass *r = q.front();
        q.pop_front();

        if ( !( r->hasChilds() ) )
          // We found a leaf register, so we add it to res.
          res.push_back( *r );
        else
          // The current register is not a leaf so that we insert its child into
          // the ready queue.
          for ( DerivedRegisterClass &c : r->getChilds() )
            q.push_back( &c );
      }

      return( res );
    };


  protected:

    /*!
      @brief mChildReferences holds (wrapped) references to all stored %WIR
             child registers.
    */
    std::list<std::reference_wrapper<DerivedRegisterClass>> mChildReferences;


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_GenericRegister( const WIR_BaseProcessor::RegisterType & )
             instead.
    */
    WIR_GenericRegister( void ) = delete;

};


/*!
  @brief Class WIR_Register models generic registers, be it virtual or
         physical processor registers.

  @tparam DerivedRegisterClass This template argument is used to specify the
                               actual type of register to be instantiated.
  @tparam Virtual This boolean template argument is used to distinguish between
                  virtual and physical registers in template instantiations. Set
                  it to true if you instantiate a virtual register class,
                  otherwise use false.

  This variant of WIR_Register just serves as primary template from which
  specialized template instantiations along the Boolean template argument
  Virtual are derived. Thus, WIR_Register here can be seen just as a piece of
  dummy code that is not really used elsewhere. The key implementation issues
  for physical or virtual registers can be found in the two template
  specializations below.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
template <typename DerivedRegisterClass, bool Virtual>
class WIR_Register : public WIR_GenericRegister<DerivedRegisterClass, Virtual>
{
};


/*!
  @brief The specialized template class WIR_Register<DerivedRegisterClass, true>
         models generic virtual registers.

  @tparam DerivedRegisterClass This template argument is used to specify the
                               actual type of virtual register to be
                               instantiated.
  @tparam Virtual This boolean template argument is used to distinguish between
                  virtual and physical registers in template instantiations.
                  Here, it is specialized to the fixed value true for virtual
                  registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
template <typename DerivedRegisterClass>
class WIR_Register<DerivedRegisterClass, true> : public WIR_GenericRegister<DerivedRegisterClass, true>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for virtual registers.

      @param[in] __r A const reference to a register type to be used.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    // cppcheck-suppress uninitMemberVar
    explicit WIR_Register( const WIR_BaseProcessor::RegisterType &__r ) :
      WIR_GenericRegister<DerivedRegisterClass, true> { __r },
      mFunctionPointer { nullptr }
    {
      DSTART(
        "WIR_Register<DerivedRegisterClass, true>::WIR_Register(const WIR_BaseProcessor::RegisterType&)" );
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      When copying a register that is inserted in some %WIR function, the
      resulting copy will not be inserted in a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Register( const WIR_Register &__o ) :
      WIR_GenericRegister<DerivedRegisterClass, true> { __o },
      mFunctionPointer { nullptr }
    {
      DSTART(
        "WIR_Register<DerivedRegisterClass, true>::WIR_Register(const WIR_Register<DerivedRegisterClass, true>&)" );

      // Copy child registers.
      clearChilds();
      for ( DerivedRegisterClass &r : __o.mChildReferences )
        pushBackChild( r );
    };

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a register that is inserted in some %WIR function results
      in an assertion, since you are not allowed to move a register whose
      ownership is managed by a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Register( WIR_Register &&__o ) :
      WIR_GenericRegister<DerivedRegisterClass, true> { std::move( __o ) },
      mChildPointers { std::move( __o.mChildPointers ) },
      mFunctionPointer { nullptr }
    {
      DSTART(
        "WIR_Register<DerivedRegisterClass, true>::WIR_Register(WIR_Register<DerivedRegisterClass, true>&&)" );

      ufAssertT(
        __o.mFunctionPointer == nullptr,
        "Invalid attempt to move virtual register '" << __o.getName() << "' " <<
        "out of its owning function '" << __o.getFunction().getName() << "'." );
      ufAssertT(
        __o.mParent == nullptr,
        "Invalid attempt to move a child register out of its parent register" <<
        " '" << __o.getParent().getName() << "'." );

      __o.mChildPointers.clear();

      // Adjust the parent IDs of the register's childs.
      for ( DerivedRegisterClass &r : this->mChildReferences )
        r.mParent = this;
    };

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Register( void )
    {
      DSTART( "WIR_Register<DerivedRegisterClass, true>::~WIR_Register()" );

      // Remove precolor information, if any.
      if ( isPrecolored() )
        getFunction().erasePrecolor(
          static_cast<DerivedRegisterClass &>( *this ) );

      clearChilds();
    };

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When copying a register that is inserted in some %WIR function, the
      resulting copy will not be inserted in a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedRegisterClass & operator = ( const DerivedRegisterClass &__o )
    {
      DSTART(
        "DerivedRegisterClass& WIR_Register<DerivedRegisterClass, true>::operator=(const DerivedRegisterClass&)" );

      WIR_GenericRegister<DerivedRegisterClass, true>::operator = ( __o );

      // Copy childs
      clearChilds();
      for ( DerivedRegisterClass &r : __o.mChildReferences )
        pushBackChild( r );

      mFunctionPointer = nullptr;

      return( static_cast<DerivedRegisterClass &>( *this ) );
    };

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a register that is inserted in some %WIR function results
      in an assertion, since you are not allowed to move a register whose
      ownership is managed by a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedRegisterClass & operator = ( DerivedRegisterClass &&__o )
    {
      DSTART(
        "DerivedRegisterClass& WIR_Register<DerivedRegisterClass, true>::operator=(DerivedRegisterClass&&)" );

      ufAssertT(
        __o.mFunctionPointer == nullptr,
        "Invalid attempt to move virtual register '" << __o.getName() << "' " <<
        "out of its owning function '" << __o.getFunction().getName() << "'." );
      ufAssertT(
        __o.mParent == nullptr,
        "Invalid attempt to move a child register out of its parent register" <<
        " '" << __o.getParent().getName() << "'." );

      WIR_GenericRegister<DerivedRegisterClass, true>::operator = (
        std::move( __o ) );

      mChildPointers = move( __o.mChildPointers );
      __o.mChildPointers.clear();

      mFunctionPointer = nullptr;

      // Adjust the parent IDs of the register's childs.
      for ( DerivedRegisterClass &r : this->mChildReferences )
        r.mParent = this;

      return( static_cast<DerivedRegisterClass &>( *this ) );
    };


    //
    // Function handling.
    //

    /*!
      @brief isInserted returns whether this object is inserted into some
             WIR_Function.

      @return true if the object is inserted, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isInserted( void ) const
    {
      DSTART(
        "bool WIR_Register<DerivedRegisterClass, true>::isInserted() const" );

      DACTION(
        DOUT( "myID = " << this->getID() << "; mFunction = " );
        if ( this->mFunctionPointer == nullptr )
          DOUT( "<nullptr>" << std::endl );
        else
          DOUT( this->mFunctionPointer->getID() << std::endl ); );

      return( mFunctionPointer != nullptr );
    };

    /*!
      @brief getFunction returns the WIR_Function to which this object is
             assigned.

      @return A reference to the assigned WIR_Function.

      If this object has not been assigned to a WIR_Function before, getFunction
      will fail with an assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Function &getFunction( void ) const
    {
      DSTART(
        "WIR_Function& WIR_Register<DerivedRegisterClass, true>::getFunction() const" );

      DACTION(
        DOUT( "myID = " << this->getID() << "; mFunction = " );
        if ( this->mFunctionPointer == nullptr )
          DOUT( "<nullptr>" << std::endl );
        else
          DOUT( this->mFunctionPointer->getID() << std::endl ); );

      return( *mFunctionPointer );
    };


    //
    // Precolor handling.
    //

    /*!
      @brief isPrecolored returns whether a virtual register is precolored or
             not.

      @return true iff the register is inserted into a %WIR function and is
              precolored therein, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isPrecolored( void ) const
    {
      DSTART(
        "bool WIR_Register<DerivedRegisterClass, true>::isPrecolored() const" );

      if ( this->isInserted() )
        return(
          getFunction().containsPrecolor(
            static_cast<const DerivedRegisterClass &>( *this ) ) );

      return( false );
    };

    /*!
      @brief getPrecolor returns the physical register associated with this
             virtual register.

      @return A reference to the physical register with which this register is
              precolored.

      getPrecolor asserts if this virtual register does not belong to a %WIR
      function or if it is not precolored.
    */
    WIR_PhysicalRegister &getPrecolor( void ) const
    {
      DSTART(
        "WIR_PhysicalRegister& WIR_Register<DerivedRegisterClass, true>::getPrecolor() const" );

      #ifdef FAILSAFEMODE
      ufAssert( this->isInserted() );
      #endif

      return(
        getFunction().findPrecolor(
          static_cast<const DerivedRegisterClass &>( *this ) ) );
    };


  protected:

    //
    // Register hierarchy handling.
    //

    /*!
      @brief pushBackChild adds a new virtual register at the end of lists
             mChildPointers and mChildReferences, after its current last
             element.

      @param[in] o A const reference to the virtual register to be copy-added.
      @return A reference to the newly inserted child register.

      @details The content of o is copied to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedRegisterClass & pushBackChild( const DerivedRegisterClass &o )
    {
      DSTART(
        "DerivedRegisterClass& WIR_Register<DerivedRegisterClass, true>::pushBackChild(const DerivedRegisterClass&)" );

      DerivedRegisterClass *p =
        static_cast<DerivedRegisterClass *>( o.clone() );
      p->mParent = this;
      mChildPointers.push_back( std::unique_ptr<DerivedRegisterClass>( p ) );
      this->mChildReferences.push_back( *p );

      return( this->mChildReferences.back().get() );
    };

    /*!
      @brief pushBackChild adds a new virtual register at the end of lists
             mChildPointers and mChildReferences, after its current last
             element.

      @param[in] o An R-value reference to the virtual register to be copy-
                 added.
      @return A reference to the newly inserted child register.

      @details The content of o is copied to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedRegisterClass & pushBackChild( DerivedRegisterClass &&o )
    {
      DSTART(
        "DerivedRegisterClass& WIR_Register<DerivedRegisterClass, true>::pushBackChild(DerivedRegisterClass&&)" );

      DerivedRegisterClass *p =
        static_cast<DerivedRegisterClass *>( o.clone() );
      p->mParent = this;
      mChildPointers.push_back( std::unique_ptr<DerivedRegisterClass>( p ) );
      this->mChildReferences.push_back( *p );

      return( this->mChildReferences.back().get() );
    };

    /*!
      @brief pushFrontChild adds a new virtual register at the beginning of
             lists mChildPointers and mChildReferences, right before its current
             first element.

      @param[in] o A const reference to the virtual register to be copy-added.
      @return A reference to the newly inserted child register.

      @details The content of o is copied to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedRegisterClass & pushFrontChild( const DerivedRegisterClass &o )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      DerivedRegisterClass *p =
        static_cast<DerivedRegisterClass *>( o.clone() );
      p->mParent = this;
      mChildPointers.push_front( std::unique_ptr<DerivedRegisterClass>( p ) );
      this->mChildReferences.push_front( *p );

      return( this->mChildReferences.front().get() );
    };

    /*!
      @brief pushFrontChild adds a new virtual register at the beginning of
             lists mChildPointers and mChildReferences, right before its current
             first element.

      @param[in] o An R-value reference to the virtual register to be copy-
                 added.
      @return A reference to the newly inserted child register.

      @details The content of o is copied to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedRegisterClass & pushFrontChild( DerivedRegisterClass &&o )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      DerivedRegisterClass *p =
        static_cast<DerivedRegisterClass *>( o.clone() );
      p->mParent = this;
      mChildPointers.push_front( std::unique_ptr<DerivedRegisterClass>( p ) );
      this->mChildReferences.push_front( *p );

      return( this->mChildReferences.front().get() );
    };

    /*!
      @brief insertChild inserts a new virtual register before the element at
             the specified position.

      @param[in] pos An iterator denoting the position where the new element is
                     inserted.
      @param[in] o A const reference to the virtual register to be copy- added.
      @return An iterator pointing to the newly inserted element.

      @details The content of o is copied to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::iterator insertChild( typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_iterator pos,
                                                                                            const DerivedRegisterClass &o )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      auto itp = mChildPointers.begin();
      for ( auto itr = this->mChildReferences.begin(); itr != pos;
            ++itr, ++itp ) ;
      DerivedRegisterClass *p =
        static_cast<DerivedRegisterClass *>( o.clone() );
      p->mParent = this;
      mChildPointers.insert( itp, std::unique_ptr<DerivedRegisterClass>( p ) );
      return( this->mChildReferences.insert( pos, *p ) );
    };

    /*!
      @brief insertChild inserts a new virtual register before the element at
             the specified position.

      @param[in] pos An iterator denoting the position where the new element is
                     inserted.
      @param[in] o An R-value reference to the virtual register to be copy-
                   added.
      @return An iterator pointing to the newly inserted element.

      @details The content of o is copied to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::iterator insertChild( typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_iterator pos,
                                                                                            DerivedRegisterClass &&o )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      auto itp = mChildPointers.begin();
      for ( auto itr = this->mChildReferences.begin(); itr != pos;
            ++itr, ++itp ) ;
      DerivedRegisterClass *p =
        static_cast<DerivedRegisterClass *>( o.clone() );
      p->mParent = this;
      mChildPointers.insert( itp, std::unique_ptr<DerivedRegisterClass>( p ) );
      return( this->mChildReferences.insert( pos, *p ) );
    };

    /*!
      @brief replaceChild replaces the list element at the specified position by
             a new virtual register.

      @param[in] pos An iterator pointing to the position to be replaced.
      @param[in] o A const reference to the virtual register to be copy-
                   inserted.
      @return An iterator pointing to the newly inserted element.

      @details The content of o is copied to the new list element. The replaced
               element is immediately destroyed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::iterator replaceChild( typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_iterator pos,
                                                                                             const DerivedRegisterClass &o )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      auto itp = mChildPointers.begin();
      for ( auto itr = this->mChildReferences.begin(); itr != pos;
            ++itr, ++itp ) ;
      DerivedRegisterClass *p =
        static_cast<DerivedRegisterClass *>( o.clone() );
      p->mParent = this;
      mChildPointers.insert( itp, std::unique_ptr<DerivedRegisterClass>( p ) );
      auto it = this->mChildReferences.insert( pos, *p );
      this->mChildReferences.erase( pos );
      mChildPointers.erase( itp );
      return( it );
    };

    /*!
      @brief replaceChild replaces the list element at the specified position by
             a new virtual register.

      @param[in] pos An iterator pointing to the position to be replaced.
      @param[in] o An R-value reference to the virtual register to be copy-
                   inserted.
      @return An iterator pointing to the newly inserted element.

      @details The content of o is copied to the new list element. The replaced
               element is immediately destroyed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::iterator replaceChild( typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_iterator pos,
                                                                                             DerivedRegisterClass &&o )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      auto itp = mChildPointers.begin();
      for ( auto itr = this->mChildReferences.begin(); itr != pos;
            ++itr, ++itp ) ;
      DerivedRegisterClass *p =
        static_cast<DerivedRegisterClass *>( o.clone() );
      p->mParent = this;
      mChildPointers.insert( itp, std::unique_ptr<DerivedRegisterClass>( p ) );
      auto it = this->mChildReferences.insert( pos, *p );
      this->mChildReferences.erase( pos );
      mChildPointers.erase( itp );
      return( it );
    };

    /*!
      @brief popBackChild removes the last virtual register from lists
             mChildPointers and mChildReferences.

      @details This destroys the removed element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void popBackChild( void )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      this->mChildReferences.pop_back();
      mChildPointers.pop_back();
    };

    /*!
      @brief popFrontChild removes the first virtual register from lists
             mChildPointers and mChildReferences.

      @details This destroys the removed element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void popFrontChild( void )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      this->mChildReferences.pop_front();
      mChildPointers.pop_front();
    };

    /*!
      @brief eraseChild removes a single virtual register from the specified
             position.

      @param[in] pos An iterator denoting the position where the element is
                     removed.
      @return An iterator pointing to the element following the erased element.

      @details This destroys the removed element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::iterator eraseChild( typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_iterator pos )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      auto itp = mChildPointers.begin();
      for ( auto itr = this->mChildReferences.begin(); itr != pos;
            ++itr, ++itp ) ;
      auto itr = this->mChildReferences.erase( pos );
      mChildPointers.erase( itp );
      return( itr );
    };

    /*!
      @brief clearChilds removes all list elements from lists mChildPointers and
             mChildReferences.

      @details This destroys all removed elements.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearChilds( void )
    {
      DSTART( "void WIR_Register<DerivedRegisterClass, true>::clearChilds()" );

      this->mChildReferences.clear();
      mChildPointers.clear();
    };


  protected:

    /*!
      @brief onInsert is called whenever a virtual register is added to a
             WIR_Function.

      @param[in] p A pointer to the WIR_Function to which this object is added.
                   If no pointer is specified, this object is marked as not to
                   be assigned to any WIR_Function at all.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void onInsert( WIR_Function *p = nullptr )
    {
      DSTART(
        "void WIR_Register<DerivedRegisterClass, true>::onInsert(WIR_Function*)" );

      if ( p != nullptr )
        ufAssertT(
          !WIR_BaseRegister::isChild(),
          "Illegal attempt to insert only a part of a hierarchical register" <<
          " into a function." );

      // Besides the current register itself, we also need to mark all childs as
      // inserted.
      std::list<DerivedRegisterClass *> q;
      q.push_back( static_cast<DerivedRegisterClass *>( this ) );

      while ( !q.empty() ) {
        auto *r = q.front();
        q.pop_front();

        r->mFunctionPointer = p;

        DACTION(
          DOUT( "myID = " << r->getID() << "; setting mFunction = " );
          if ( r->mFunctionPointer == nullptr )
            DOUT( "<nullptr>" << std::endl );
          else
            DOUT( r->mFunctionPointer->getID() << std::endl ); );

        for ( DerivedRegisterClass &c : r->getChilds() )
          q.push_back( &c );
      }
    };


  private:

    friend class WIR_Function;

    /*!
      @brief No standard construction allowed, users must use
             WIR_Register( const WIR_BaseProcessor::RegisterType & ) instead.
    */
    WIR_Register( void ) = delete;

    /*!
      @brief clone creates a copy of a virtual %WIR register.

      @return A pointer to the newly created copy of this register.

      Using the template argument, clone just calls the correct copy constructor
      of the actual register class.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseRegister *clone( void ) const
    {
      DSTART(
        "WIR_BaseRegister* WIR_Register<DerivedRegisterClass, true>::clone() const" );

      return(
        new DerivedRegisterClass(
          *( static_cast<const DerivedRegisterClass *>( this ) ) ) );
    };

    /*!
      @brief mChildPointers holds managed pointers to all stored virtual %WIR
             child registers.
    */
    std::list<std::unique_ptr<DerivedRegisterClass>> mChildPointers;

    /*!
      mFunctionPointer points to that WIR_Function object to which this virtual
      register belongs.
    */
    WIR_Function *mFunctionPointer;

};


/*!
  @brief The specialized template class
         WIR_Register<DerivedRegisterClass, false> models generic physical
         registers.

  @tparam DerivedRegisterClass This template argument is used to specify the
                               actual type of physical register to be
                               instantiated.
  @tparam Virtual This boolean template argument is used to distinguish between
                  virtual and physical registers in template instantiations.
                  Here, it is specialized to the fixed value false for physical
                  registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
template <typename DerivedRegisterClass>
class WIR_Register<DerivedRegisterClass, false> : public WIR_GenericRegister<DerivedRegisterClass, false>
{

  public:

    //
    // Destructor.
    //

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Register( void )
    {
      DSTART( "WIR_Register<DerivedRegisterClass, false>::~WIR_Register()" );

      clearChilds();
    };


    //
    // Processor handling.
    //

    /*!
      @brief isInserted returns whether this object is inserted into some
             WIR_BaseProcessor.

      @return true if the object is inserted, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isInserted( void ) const
    {
      DSTART(
        "bool WIR_Register<DerivedRegisterClass, false>::isInserted() const" );

      DACTION(
        DOUT( "myID = " << this->getID() << "; mProcessor = " );
        if ( this->mProcessorPointer == nullptr )
          DOUT( "<nullptr>" << std::endl );
        else
          DOUT( this->mProcessorPointer->getID() << std::endl ); );

      return( mProcessorPointer != nullptr );
    };

    /*!
      @brief getProcessor returns the WIR_BaseProcessor to which this object is
             assigned.

      @return A reference to the assigned WIR_BaseProcessor.

      If this object has not been assigned to a WIR_BaseProcessor before,
      getProcessor will fail with an assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseProcessor &getProcessor( void ) const
    {
      DSTART(
        "WIR_BaseProcessor& WIR_Register<DerivedRegisterClass, false>::getProcessor() const" );

      DACTION(
        DOUT( "myID = " << this->getID() << "; mProcessor = " );
        if ( this->mProcessorPointer == nullptr )
          DOUT( "<nullptr>" << std::endl );
        else
          DOUT( this->mProcessorPointer->getID() << std::endl ); );

      return( *mProcessorPointer );
    };

    /*!
      @brief isStackPointer returns whether a physical register is some
             processor's stack pointer.

      @return true if the register is the stack pointer, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isStackPointer( void ) const
    {
      DSTART(
        "bool WIR_Register<DerivedRegisterClass, false>::isStackPointer() const" );

      return( mIsStackPointer );
    };


  protected:

    //
    // Constructors.
    //

    /*!
      @brief Default constructor for physical registers.

      @param[in] __r A const reference to a register type to be used.
      @param[in] __s A const reference to a string that holds the physical
                     register's name (without its specific pre-/suffix as
                     determined by the register's type).
      @param[in] __sp A Boolean denoting whether the physical register is the
                      stack pointer.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Register( const WIR_BaseProcessor::RegisterType &__r,
                  const std::string &__s, const bool __sp = false ) :
      WIR_GenericRegister<DerivedRegisterClass, false> { __r },
      mIsStackPointer { __sp },
      mProcessorPointer { nullptr }
    {
      DSTART(
        "WIR_Register<DerivedRegisterClass, false>::WIR_Register(const WIR_BaseProcessor::RegisterType&, const string&, bool)" );

      this->mName = __s;
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      When copying a register that is inserted in some %WIR processor, the
      resulting copy will not be inserted in a processor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Register( const WIR_Register &__o ) :
      WIR_GenericRegister<DerivedRegisterClass, false> { __o },
      mIsStackPointer { __o.mIsStackPointer },
      mProcessorPointer { nullptr }
    {
      DSTART(
        "WIR_Register<DerivedRegisterClass, false>::WIR_Register(const WIR_Register<DerivedRegisterClass, false>&)" );

      // Copy register name.
      this->mName = __o.mName;

      // Clear child registers. Proper handling of childs is done in
      // WIR_BaseProcessor::copyProcessor().
      clearChilds();
    };

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When copying a register that is inserted in some %WIR processor, the
      resulting copy will not be inserted in a processor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedRegisterClass & operator = ( const DerivedRegisterClass &__o )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      WIR_GenericRegister<DerivedRegisterClass, false>::operator = ( __o );

      this->mName = __o.mName;
      mIsStackPointer = __o.mIsStackPointer;
      mProcessorPointer = nullptr;

      // Clear child registers. Proper handling of childs is done in
      // WIR_BaseProcessor::copyProcessor().
      clearChilds();

      return( static_cast<DerivedRegisterClass &>( *this ) );
    };


    //
    // Register hierarchy handling.
    //

    /*!
      @brief pushBackChild adds a new physical register at the end of list
             mChildReferences, after its current last element.

      @param[in] o A reference to the physical register to be added.
      @return A reference to the newly inserted child register.

      @details The reference o itself is stored as new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedRegisterClass & pushBackChild( DerivedRegisterClass &o )
    {
      DSTART(
        "DerivedRegisterClass& WIR_Register<DerivedRegisterClass, false>::pushBackChild(DerivedRegisterClass&)" );

      o.mParent = this;
      this->mChildReferences.push_back( o );

      return( this->mChildReferences.back().get() );
    };

    /*!
      @brief pushFrontChild adds a new physical register at the beginning of
             list mChildReferences, right before its current first element.

      @param[in] o A reference to the physical register to be added.
      @return A reference to the newly inserted child register.

      @details The reference o itself is stored as new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedRegisterClass & pushFrontChild( DerivedRegisterClass &o )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      o.mParent = this;
      this->mChildReferences.push_front( o );

      return( this->mChildReferences.front().get() );
    };

    /*!
      @brief insertChild inserts a new physical register before the element at
             the specified position.

      @param[in] pos An iterator denoting the position where the new element is
                     inserted.
      @param[in] o A reference to the physical register to be added.
      @return An iterator pointing to the newly inserted element.

      @details The reference o itself is stored as new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::iterator insertChild( typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_iterator pos,
                                                                                            DerivedRegisterClass &o )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      o.mParent = this;
      return( this->mChildReferences.insert( pos, o ) );
    };

    /*!
      @brief replaceChild replaces the list element at the specified position by
             a new physical register.

      @param[in] pos An iterator pointing to the position to be replaced.
      @param[in] o A reference to the physical register to be added.
      @return An iterator pointing to the newly inserted element.

      @details The reference o itself is stored as new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::iterator replaceChild( typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_iterator pos,
                                                                                             DerivedRegisterClass &o )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      o.mParent = this;
      auto it = this->mChildReferences.insert( pos, o );
      this->mChildReferences.erase( pos );
      return( it );
    };

    /*!
      @brief popBackChild removes the last physical register from list
             mChildReferences.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void popBackChild( void )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      this->mChildReferences.back().get().mParent = nullptr;
      this->mChildReferences.pop_back();
    };

    /*!
      @brief popFrontChild removes the first physical register from list
             mChildReferences.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void popFrontChild( void )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      this->mChildReferences.front().get().mParent = nullptr;
      this->mChildReferences.pop_front();
    };

    /*!
      @brief eraseChild removes a single physical register from the specified
             position.

      @param[in] pos An iterator denoting the position where the element is
                     removed.
      @return An iterator pointing to the element following the erased element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    typename std::list<std::reference_wrapper<DerivedRegisterClass>>::iterator eraseChild( typename std::list<std::reference_wrapper<DerivedRegisterClass>>::const_iterator pos )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      (*pos).get().mParent = nullptr;
      return( this->mChildReferences.erase( pos ) );
    };

    /*!
      @brief clearChilds removes all list elements from list mChildReferences.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearChilds( void )
    {
      DSTART(
        "void WIR_Register<DerivedRegisterClass, false>::clearChilds()" );

      this->mChildReferences.clear();
    };

    /*!
      @brief onInsert is called whenever a physical register is added to a
             WIR_BaseProcessor.

      @param[in] p A pointer to the WIR_BaseProcessor to which this object is
                   added. If no pointer is specified, this object is marked as
                   not to be assigned to any WIR_BaseProcessor at all.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void onInsert( WIR_BaseProcessor *p = nullptr )
    {
      DSTART(
        "void WIR_Register<DerivedRegisterClass, false>::onInsert(WIR_BaseProcessor*)" );

      if ( p != nullptr )
        ufAssertT(
          !WIR_BaseRegister::isChild(),
          "Illegal attempt to insert only a part of a hierarchical register" <<
          " into a processor." );

      // Besides the current register itself, we also need to mark all childs as
      // inserted.
      std::list<DerivedRegisterClass *> q;
      q.push_back( static_cast<DerivedRegisterClass *>( this ) );

      while ( !q.empty() ) {
        auto *r = q.front();
        q.pop_front();

        r->mProcessorPointer = p;

        DACTION(
          DOUT( "myID = " << r->getID() << "; setting mProcessor = " );
          if ( r->mProcessorPointer == nullptr )
            DOUT( "<nullptr>" << std::endl );
          else
            DOUT( r->mProcessorPointer->getID() << std::endl ); );

        for ( DerivedRegisterClass &c : r->getChilds() )
          q.push_back( &c );
      }
    };


  private:

    friend class WIR_BaseProcessor;

    /*!
      @brief No standard construction allowed, users must use
             WIR_Register( const WIR_BaseProcessor::RegisterType &,
                           const std::string &, const bool )
             instead.
    */
    WIR_Register( void ) = delete;

    /*!
      @brief No move-construction of physical registers allowed, since moving a
             physical register out of its processor is impossible.
    */
    WIR_Register( WIR_Register && ) = delete;

    /*!
      @brief No move-assignment of physical registers allowed, since moving a
             physical register out of its processor is impossible.
    */
    WIR_Register & operator = ( WIR_Register && ) = delete;

    /*!
      @brief clone creates a copy of a physical %WIR register.

      @return A pointer to the newly created copy of this register.

      Using the template argument, clone just calls the correct copy constructor
      of the actual register class.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseRegister *clone( void ) const
    {
      DSTART(
        "WIR_BaseRegister* WIR_Register<DerivedRegisterClass, false>::clone() const" );

      return(
        new DerivedRegisterClass(
          *( static_cast<const DerivedRegisterClass *>( this ) ) ) );
    };

    /*!
      @brief mIsStackPointer stores whether a physical register is a processor's
             stack pointer.
    */
    bool mIsStackPointer;

    /*!
      @brief mProcessorPointer points to that WIR_BaseProcessor object to which
             this physical register belongs.
    */
    WIR_BaseProcessor *mProcessorPointer;

};

}       // namespace WIR

#endif  // _WIR_REGISTER_H
