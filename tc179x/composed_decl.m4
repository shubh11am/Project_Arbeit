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


# Represents a composed type object through its base poiner and offset from
# there.
%declare<TC_AddressWithOffset> composed_type_object<void>;

# Composed type return values may be given in a dreg, ereg or via the stack,
# depending on the composed type size. This helper non-terminal represents such
# a return value. At the moment, this can just be used to convert it into a
# 'composed_type_object' which stores the return value on the stack if it was
# passed through a register.
# TODO: In the future, it would be possible to implement some basic operations
# directly on the 'composed_call_result' like for example the component access
# expressions that yield a dreg / areg (these should work with appropriate EXTR
# operations). It is not neccessary to provide rules that can write back
# modifications to the 'composed_call_result', because this is undefined
# behaviour according to ANSI C 6.5.2.2. point 5.
%declare<TC_AddressWithOffset> composed_call_result<void>;

include(composed_initlist_decl.m4)
include(composed_struct_decl.m4)
