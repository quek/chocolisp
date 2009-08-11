=head1 7 COMPILATION
byte code
=cut

.HLL 'choco'

.namespace [ "CHIMACHO" ]

.macro package
        .local pmc package
        package = get_global "package"
.endm

.macro retlambda(name)
        .const 'Sub' k = .name
        $P99 = newclosure k
        .return($P99)
.endm

.sub '' :anon :load
        say "initializing..."
        $P0 = newclass "T"

        $P0 = subclass "T", "ENVIRONMENT"
        addattribute $P0, 'next'

        $P0 = subclass "ENVIRONMENT", "ACTIVATION-FRAME"
        addattribute $P0, 'argument'

        $P0 = subclass "T", "CONS"
        addattribute $P0, 'car'
        addattribute $P0, 'cdr'

        $P0 = subclass "T", "ATOM"

        $P0 = subclass "ATOM", "CLOSURE"
        addattribute $P0, 'code'
        addattribute $P0, 'closed-environment'

        $P0 = subclass "ATOM", "PACKAGE"
        addattribute $P0, 'name'
        addattribute $P0, 'use'
        addattribute $P0, 'external-symbols'
        addattribute $P0, 'internal-symbols'

        $P0 = subclass "ATOM", "SYMBOL"
        addattribute $P0, 'name'
        addattribute $P0, 'value'
        addattribute $P0, 'function'
        addattribute $P0, 'plist'
        addattribute $P0, 'package'

        $P0 = subclass "SYMBOL", "NULL"

        .local pmc package
        package = new "PACKAGE"
        package = "CHIMACHO"

        ## instraction set
        .local pmc instraction_set
        instraction_set = new 'ResizablePMCArray'
        #instraction_set = 255

        .const 'Sub' SHALLOW_ARGUMENT_REF0 = 'iSHALLOW-ARGUMENT-REF0'
        instraction_set[1] = SHALLOW_ARGUMENT_REF0

        .const 'Sub' SHALLOW_ARGUMENT_REF1 = 'iSHALLOW-ARGUMENT-REF1'
        instraction_set[2] = SHALLOW_ARGUMENT_REF1

        .const 'Sub' SHALLOW_ARGUMENT_REF2 = 'iSHALLOW-ARGUMENT-REF2'
        instraction_set[3] = SHALLOW_ARGUMENT_REF2

        .const 'Sub' SHALLOW_ARGUMENT_REF3 = 'iSHALLOW-ARGUMENT-REF3'
        instraction_set[4] = SHALLOW_ARGUMENT_REF3

        .const 'Sub' SHALLOW_ARGUMENT_REF = 'iSHALLOW-ARGUMENT-REF'
        instraction_set[5] = SHALLOW_ARGUMENT_REF

        .const 'Sub' DEEP_ARGUMENT_REF = 'iDEEP-ARGUMENT-REF'
        instraction_set[6] = DEEP_ARGUMENT_REF

        .const 'Sub' GLOBAL_REF = 'iGLOBAL-REF'
        instraction_set[7] = GLOBAL_REF

        .const 'Sub' CHECKED_GLOBAL_REF = 'iCHECKED-GLOBAL-REF'
        instraction_set[8] = CHECKED_GLOBAL_REF

        .const 'Sub' PREDEFINED0 = 'iPREDEFINED0'
        instraction_set[10] = PREDEFINED0

        .const 'Sub' PREDEFINED1 = 'iPREDEFINED1'
        instraction_set[11] = PREDEFINED1

        .const 'Sub' PREDEFINED2 = 'iPREDEFINED2'
        instraction_set[12] = PREDEFINED2

        .const 'Sub' PREDEFINED3 = 'iPREDEFINED3'
        instraction_set[13] = PREDEFINED3

        .const 'Sub' PREDEFINED4 = 'iPREDEFINED4'
        instraction_set[14] = PREDEFINED4

        .const 'Sub' PREDEFINED5 = 'iPREDEFINED5'
        instraction_set[15] = PREDEFINED5

        .const 'Sub' PREDEFINED6 = 'iPREDEFINED6'
        instraction_set[16] = PREDEFINED6

        .const 'Sub' SET_SHALLOW_ARGUMENT0 = 'iSET-SHALLOW-ARGUMENT0'
        instraction_set[21] = SET_SHALLOW_ARGUMENT0

        .const 'Sub' SET_SHALLOW_ARGUMENT1 = 'iSET-SHALLOW-ARGUMENT1'
        instraction_set[22] = SET_SHALLOW_ARGUMENT1

        .const 'Sub' SET_SHALLOW_ARGUMENT2 = 'iSET-SHALLOW-ARGUMENT2'
        instraction_set[23] = SET_SHALLOW_ARGUMENT2

        .const 'Sub' SET_SHALLOW_ARGUMENT3 = 'iSET-SHALLOW-ARGUMENT3'
        instraction_set[24] = SET_SHALLOW_ARGUMENT3

        .const 'Sub' SET_SHALLOW_ARGUMENT = 'iSET-SHALLOW-ARGUMENT'
        instraction_set[25] = SET_SHALLOW_ARGUMENT

        .const 'Sub' SET_DEEP_ARGUMENT = 'iSET-DEEP-ARGUMENT'
        instraction_set[26] = SET_DEEP_ARGUMENT

        .const 'Sub' SET_GLOBAL = 'iSET-GLOBAL'
        instraction_set[27] = SET_GLOBAL
.end

.sub 'run'
        .local int instruction
        instruction = 'fetch-byte'()
        .tailcall 'run'()
.end

.sub 'fetch-byte'
        .local pmc code, pc
        .local int byte
        code = get_global "*code*"
        pc = get_global "*pc*"
        byte = code[pc]
        pc += 1
        set_global "*pc*", pc
        .return(byte)
.end

.sub 'list'
        .param pmc args :slurpy
        .return(args)
.end

.sub 'cons'
        .param pmc x
        .param pmc y
        .local pmc cons
        cons = new "CONS"
        setattribute cons, 'car', x
        setattribute cons, 'cdr', y
        .return(cons)
.end

.sub 'car'
        .param pmc x
        .local pmc val
        val = getattribute x, 'car'
        .return(val)
.end

.sub 'cdr'
        .param pmc x
        .local pmc val
        val = getattribute x, 'cdr'
        .return(val)
.end

.sub 'deep-fetch'
        .param pmc env
        .param int i
        .param int j
        eq_num i, 0, return
        .local pmc next
        next = getattribute env, 'next'
        i -= 1
        .tailcall 'deep-fetch'(next, i, j)
return:
        .local pmc argument, val
        argument = getattribute env, 'argument'
        val = argument[j]
        .return(val)
.end

.sub 'deep-update!'
        .param pmc env
        .param int i
        .param int j
        .param pmc val
        eq_num i, 0, this
        .local pmc next
        next = getattribute env, 'next'
        i -= 1
        .tailcall 'deep-update!'(next, i, j, val)
this:
        .local pmc argument
        argument = getattribute env, 'argument'
        argument[j] = val
.end

#### Meanings
.sub 'mSHALLOW-ARGUMENT-REF'
        .param int j
        unless j <= 3 goto n
        j  += 1
        .tailcall 'list'(j)
n:
        .tailcall 'list'(5, j)
.end

.sub 'mSET-SHALLOW-ARGUMENT'
        .param int j
        unless j <= 3 goto n
        j  += 21
        .tailcall 'list'(j)
n:
        .tailcall 'list'(25, j)
.end

.sub 'mDEEP-ARGUMENT-REF'
        .param int i
        .param int j
        .tailcall 'list'(6, i, j)
.end

.sub 'mSET-DEEP-ARGUMENT'
        .param int i
        .param int j
        .tailcall 'list'(26, i, j)
.end

.sub 'mGLOBAL-REF'
        .param int i
        .tailcall 'list'(7, i)
.end

.sub 'mCHECKED-GLOBAL-REF'
        .param int i
        .tailcall 'list'(8, i)
.end

.sub 'mSET-GLOBAL'
        .param int i
        .tailcall 'list'(27, i)
.end

.sub 'mPREDEFINED'
        .param int i
        if i > 8 goto n
        ## 0 t, 1 nil, 2 cons, 3 car, 4 cdr, 5 atom, 6 eq
        i += 10
        .tailcall 'list'(i)
n:
        .tailcall 'list'(19, i)
.end

#### Instractions
.sub 'iSHALLOW-ARGUMENT-REF0'
        .local pmc env, val, argument
        env = get_global "*env*"
        argument = getattribute env, 'argument'
        val = argument[0]
        set_global "*val*", val
.end

.sub 'iSHALLOW-ARGUMENT-REF1'
        .local pmc env, val, argument
        env = get_global "*env*"
        argument = getattribute env, 'argument'
        val = argument[1]
        set_global "*val*", val
.end

.sub 'iSHALLOW-ARGUMENT-REF2'
        .local pmc env, val, argument
        env = get_global "*env*"
        argument = getattribute env, 'argument'
        val = argument[2]
        set_global "*val*", val
.end

.sub 'iSHALLOW-ARGUMENT-REF3'
        .local pmc env, val, argument
        env = get_global "*env*"
        argument = getattribute env, 'argument'
        val = argument[3]
        set_global "*val*", val
.end

.sub 'iSHALLOW-ARGUMENT-REF'
        .local int j
        .local pmc env, val, argument
        j = 'fetch-byte'()
        env = get_global "*env*"
        argument = getattribute env, 'argument'
        val = argument[j]
        set_global "*val*", val
.end

.sub 'iSET-SHALLOW-ARGUMENT0'
        .local pmc env, val, argument
        env = get_global "*env*"
        argument = getattribute env, 'argument'
        val = get_global "*val*"
        argument[0] = val
.end

.sub 'iSET-SHALLOW-ARGUMENT1'
        .local pmc env, val, argument
        env = get_global "*env*"
        argument = getattribute env, 'argument'
        val = get_global "*val*"
        argument[1] = val
.end

.sub 'iSET-SHALLOW-ARGUMENT2'
        .local pmc env, val, argument
        env = get_global "*env*"
        argument = getattribute env, 'argument'
        val = get_global "*val*"
        argument[2] = val
.end

.sub 'iSET-SHALLOW-ARGUMENT3'
        .local pmc env, val, argument
        env = get_global "*env*"
        argument = getattribute env, 'argument'
        val = get_global "*val*"
        argument[3] = val
.end

.sub 'iSET-SHALLOW-ARGUMENT'
        .local int j
        .local pmc env, val, argument
        j = 'fetch-byte'()
        env = get_global "*env*"
        argument = getattribute env, 'argument'
        val = get_global "*val*"
        argument[j] = val
.end

.sub 'iDEEP-ARGUMENT-REF'
        .local int i, j
        .local pmc env, val
        i = 'fetch-byte'()
        j = 'fetch-byte'()
        env = get_global "*env*"
        val = 'deep-fetch'(env, i, j)
        set_global "*val*", val
.end

.sub 'iSET-DEEP-ARGUMENT'
        .local int i, j
        .local pmc env, val
        i = 'fetch-byte'()
        j = 'fetch-byte'()
        env = get_global "*env*"
        val = get_global "*val*"
        'deep-update!'(env, i, j, val)
.end

.sub 'iGLOBAL-REF'
        .local int i
        .local pmc val
        i = 'fetch-byte'()
        val = 'global-fetch'(i)
        set_global "*val*", val
.end

.sub 'iCHECKED-GLOBAL-REF'
        .local int i
        .local pmc val
        i = 'fetch-byte'()
        val = 'global-fetch'(i)
        $I0 = isnull val
        unless $I0 goto next
        die "Uninitialized global variable"
next:
        set_global "*val*", val
.end

.sub 'iSET-GLOBAL'
        .local int i
        .local pmc val
        i = 'fetch-byte'()
        val = get_global "*val*"
        'global-update!'(i, val)
.end

.sub 'iPREDEFINED0'
        .package
        .local pmc val
        val = package.'intern'("T")
        set_global "*val*", val
.end

.sub 'iPREDEFINED1'
        .package
        .local pmc val
        val = package.'intern'("NIL")
        set_global "*val*", val
.end

.sub 'iPREDEFINED2'
        .const 'Sub' val = 'cons'
        set_global "*val*", val
.end

.sub 'iPREDEFINED3'
        .const 'Sub' val = 'car'
        set_global "*val*", val
.end

.sub 'iPREDEFINED4'
        .const 'Sub' val = 'cdr'
        set_global "*val*", val
.end

.sub 'iPREDEFINED5'
        .const 'Sub' val = 'atom'
        set_global "*val*", val
.end

.sub 'iPREDEFINED6'
        .const 'Sub' val = 'eq'
        set_global "*val*", val
.end


.sub 'iPREDEFINED'
        .local pmc i
        .local pmc val
        i = 'fetch-byte'()
        val = 'predefined-fetch'(i)
        set_global "*val*", val
.end


.namespace [ "PACKAGE" ]

.sub init :vtable
        $P0 = new 'ResizablePMCArray'
        setattribute self, 'external-symbols', $P0
        $P0 = new 'ResizablePMCArray'
        setattribute self, 'internal-symbols', $P0
.end

.sub set_string_native :vtable
        .param pmc str
        setattribute self, 'name', str
.end
