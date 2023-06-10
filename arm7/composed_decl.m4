#
#
#  This source file belongs to the
#
#           Hamburg University of Technology (TUHH)
#             WCC Compiler Framework
#
#  and is property of its respective copyright holder. It must neither be used
#  nor published even in parts without explicit written permission.
#
#  Copyright 2009 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


# Represents a composed type object through its base poiner and offset from there
# On a dry run only the offset is evaluated, the address will always be a
# nullptr.
%declare<AddressWithOffset> composed_type_object<bool dryRun>;

# Represents a whole init list
# If 'useBaseOffsetAddressing' is true, then 'reg + address_increment' is the
# base address of the object to initialize, else this is 'reg' only.
%declare<void> init_list<LLIR_Register *reg, long address_increment,
                         bool useBaseOffsetAddressing>;

# Represents a single element of an init list. If 'useBaseOffsetAddressing'
# is true, then the address to which its value should be written is given by
# 'reg + address_increment', else it is given by 'reg' only. If
# 'useBaseOffsetAddressing' is false then 'address_increment' specifies the
# number by which 'reg' should be incremented after the access.
%declare<void> init_list_element<LLIR_Register *reg, long address_increment,
                                 bool useBaseOffsetAddressing>;

# Represents an initalizable object (which is a composed object or an array)
# Composed type and array nonterminals must first be converted to this
# nonterminal to get initialized.
%declare<AddressWithOffset> initializable_object<void>;

# Represents an initalized object (which is a composed object or an array)
# This nonterminal can then be converted back into a composed type or array
# type nonterminal.
%declare<AddressWithOffset> initialized_object<void>;

%declare<long><0> component_offset<void>;

include(composed_struct_bitfield_decl.m4)
