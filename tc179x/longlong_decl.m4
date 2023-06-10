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
#  Copyright 2010 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


# An extended register that contains a long long value.
%declare<ERegPair><{ nullptr, ref( dummyERegV ) }> llereg<void>;

# An extended register with a long long together with its storage location. This
# should only be used by LHSs of assignments to write back the value after the
# assignment. Parameter loadResult indicates whether the result should really be
# loaded into memory (true) or whether just the address should be computed.
%declare<TC_LValue> deref_llereg<bool loadResult>;

# A long long integer constant.
%declare<IR_Integer> llconst;
