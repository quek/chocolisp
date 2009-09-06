.sub 'CONSTANT'
        .param pmc v
        .lex 'v', v
        .const 'Sub' k = '%CONSTANT'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%CONSTANT' :outer('CONSTANT')
        .local pmc v
        v = find_lex 'v'
        .return(v)
.end

.sub 'SHALLOW-ARGUMENT-REF'
        .local int j
        .lex 'j', j
        .const 'Sub' k = '%SHALLOW-ARGUMENT-REF'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%SHALLOW-ARGUMENT-REF' :outer('SHALLOW-ARGUMENT-REF')
        .local pmc j
        .env
        j = find_lex 'j'
        .local pmc argument
        argument = getattribute env, 'argument'
        $P0 = argument[j]
        .return($P0)
.end

.sub 'PREDEFINED'
        .param pmc i
        .lex 'i', i
        .const 'Sub' k = '%PREDEFINED'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%PREDEFINED' :outer('PREDEFINED')
        .local pmc i
        i = find_lex 'i'
        .tailcall 'predefined-fetch'(i)
.end

.sub 'DEEP-ARGUMENT-REF'
        .param pmc i
        .param pmc j
        .lex 'i', i
        .lex 'j', j
        .const 'Sub' k = '%DEEP-ARGUMENT-REF'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%DEEP-ARGUMENT-REF' :outer('DEEP-ARGUMENT-REF')
        .local pmc i, j
        i = find_lex 'i'
        j = find_lex 'j'
        .tailcall 'deep-fetch'(i, j)
.end

.sub 'GLOBAL-REF'
        .param pmc i
        .lex 'i', i
        .const 'Sub' k = '%GLOBAL-REF'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%GLOBAL-REF' :outer('GLOBAL-REF')
        .local pmc i
        i = find_lex 'i'
        .tailcall 'global-fetch'(i)
.end

.sub 'CHECKED-GLOBAL-REF'
        .param pmc i
        .lex 'i', i
        .const 'Sub' k = '%CHECKED-GLOBAL-REF'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%CHECKED-GLOBAL-REF' :outer('CHECKED-GLOBAL-REF')
        .local pmc i, v
        i = find_lex 'i'
        v = 'global-fetch'(i)
        $I0 = isnull v
        if $I0 goto error
        .return(v)
error:
        $P0 = new "Exception"
        $P0 = "Uninitialized variable"
        throw $P0
.end

.sub 'ALTERNATIVE'
        .param pmc m1
        .param pmc m2
        .param pmc m3
        .lex 'm1', m1
        .lex 'm2', m2
        .lex 'm3', m3
        .const 'Sub' k = '%ALTERNATIVE'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%ALTERNATIVE' :outer('ALTERNATIVE')
        .local pmc m1, m2, m3
        m1 = find_lex 'm1'
        $P0 = m1()
        $I0 = 'null'($P0)
        if $I0 goto else
        m2 = find_lex 'm2'
        .tailcall m2()
else:
        m3 = find_lex 'm3'
        .tailcall m3()
.end

.sub 'SEQUENECE'
        .param pmc m
        .param pmc ms
        .lex 'm', m
        .lex 'ms', ms
        .const 'Sub' k = '%SEQUENCE'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%SEQUENCE' :outer('SEQUENECE')
        .local pmc m, ms
        m = find_lex 'm'
        ms = find_lex 'ms'
        m()
        .tailcall ms()
.end

.sub 'FIX-CLOSURE'
        .param pmc ms
        .param pmc arity
        .local pmc arity_plus1
        arity_plus1 = arity + 1
        .lex 'ms', ms
        .lex 'arity_plus1', arity_plus1
        .const 'Sub' k = '%FIX-CLOSURE'
        $P0 = newclosure k
        .return(k)
.end

.sub '%FIX-CLOSURE' :outer('FIX-CLOSURE')
        .local pmc closure, code
        .const 'Sub' k = 'fix-the-function'
        code = newclosure k
        .env
        closure = new "CLOSURE"
        setattribute closure, 'codo', code
        setattribute closure, 'closed-environment', env
        .return(closure)
.end

.sub 'fix-the-function' :outer('%FIX-CLOSURE')
        .param pmc vs
        .param pmc sr
        .local pmc ms, argument, arity_plus1, env
        .local int argument_len, i_arity_plus1
        ms = find_lex 'ms'
        arity_plus1 = find_lex 'arity_plus1'
        i_arity_plus1 = arity_plus1
        argument = getattribute vs, 'argument'
        argument_len = argument
        if argument_len != i_arity_plus1 goto error
        env = 'sr-extend*'(sr, vs)
        set_global "*env*", env
        .tailcall ms()
error:
        $P0 = new 'Exception'
        $P0 = "Incorrect arity"
        throw $P0
.end

.sub 'NARY-CLOSURE'
        .param pmc ms
        .param pmc arity
        .local pmc arity_plus1
        arity_plus1 = arity + 1
        .lex 'ms', ms
        .lex 'arity', arity
        .lex 'arity_plus1', arity_plus1
        .const 'Sub' k = '%NARY-CLOSURE'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%NARY-CLOSURE' :outer('NARY-CLOSURE')
        .local pmc closure, code
        .const 'Sub' k = 'nary-the-function'
        code = newclosure k
        .env
        closure = new "CLOSURE"
        setattribute closure, 'code', code
        setattribute closure, 'closed-environment', env
        .return(closure)
.end

.sub 'nary-the-function' :outer('%NARY-CLOSURE')
        .param pmc vs
        .param pmc sr
        .local pmc argument, arity, arity_plus1, ms
        .local int argument_len, i_arity_plus1
        argument = getattribute vs, 'argument'
        argument_len = argument
        arity_plus1 = find_lex 'arity_plus1'
        i_arity_plus1 = arity_plus1
        if argument_len < i_arity_plus1 goto error
        arity = find_lex 'arity'
        'listify!'(vs, arity)
        .local pmc env
        env = 'sr-extend*'(sr, vs)
        set_global "*env*", env
        ms = find_lex 'ms'
        .tailcall ms()
error:
        $P0 = new 'Exception'
        $P0 = "Incorrect arity"
        throw $P0
.end

.sub 'REGULAR-CALL'
        .param pmc m
        .param pmc ms
        .lex 'm', m
        .lex 'ms', ms
        .const 'Sub' k = '%REGULAR-CALL'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%REGULAR-CALL' :outer('REGULAR-CALL')
        .local pmc m, ms, f, vs, result
        m = find_lex 'm'
        ms = find_lex 'ms'
        f = m()
        vs = ms()
        .env
        result = f.'invoke'(vs)
        set_global "*env*", env
        .return(result)
.end

.sub 'STORE-ARGUMENT'
        .param pmc m
        .param pmc ms
        .param pmc rank
        .lex 'm', m
        .lex 'ms', ms
        .lex 'rank', rank
        .const 'Sub' k = '%STORE-ARGUMENT'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%STORE-ARGUMENT' :outer('STORE-ARGUMENT')
        .local pmc m, ms, rank, v, vs, argument
        m = find_lex 'm'
        ms = find_lex 'ms'
        v = m()
        vs = ms()
        argument = getattribute vs, 'argument'
        rank = find_lex 'rank'
        argument[rank] = v
        .return(vs)
.end

.sub 'ALLOCATE-FRAME'
        .param pmc size
        .local pmc size_plus1
        size_plus1 = size + 1
        .lex 'size_plus1', size_plus1
        .const 'Sub' k = '%ALLOCATE-FRAME'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%ALLOCATE-FRAME' :outer('ALLOCATE-FRAME')
        .local pmc size_plus1
        size_plus1 = find_lex 'size_plus1'
        .tailcall 'allocate-activation-frame'(size_plus1)
.end

.sub 'CALL'
        .param pmc address
        .param pmc args
        .lex 'address', address
        .lex 'args', args
        .const 'Sub' k = '%CALL'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%CALL' :outer('CALL')
        .local pmc address, args, vals, arg, val
        .local int size, i
        address = find_lex 'address'
        args = find_lex 'args'
        vals = new 'FixedPMCArray'
        size = args
        vals = size
        i = 0
loop:
        if i == size goto end
        arg = args[i]
        val = arg()
        vals[i] = val
        i += 1
        goto loop
end:
        .tailcall address(vals :flat)
.end
