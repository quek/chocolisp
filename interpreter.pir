=head1 6 Fast Interpretation

=cut

.HLL 'choco'

.namespace [ "CHIMACHO" ]

.macro nil
        .local pmc nil
        nil = get_hll_global ["CHIMACHO"], "NIL"
.endm

.macro t
        .local pmc t
        t = get_hll_global ["CHIMACHO"], "T"
.endm

.macro env
        .local pmc env
        env = get_hll_global ["CHIMACHO"], "*env*"
.endm

.macro package
        .local pmc package
        package = get_hll_global ["CHIMACHO"], "*PACKAGE*"
        package = package.'value'()
.endm

.include "classes.pir"
.include "cons.pir"
.include "closure.pir"
.include "package.pir"
.include "symbol.pir"
.include "combinator.pir"
.include "environment.pir"
.include "primitive.pir"
.include "read.pir"
.include "interpreter-test.pir"

.sub main :main
        say "Interpreter"
toplevel:
        '%run-test'()
.end

.sub init :load :init
        say "init :load :init"

        .package

        package.'%%intern'("QUOTE")
        package.'%%intern'("LAMBDA")
        package.'%%intern'("IF")
        package.'%%intern'("PROGN")
        package.'%%intern'("SETQ")
.end

.sub 'sr-extend*'
        .param pmc sr
        .param pmc vs
        setattribute vs, 'next', sr
        .return(vs)
.end

.sub 'deep-fetch'
        .param pmc sr
        .param int i
        .param int j
        eq_num i, 0, return
        .local pmc next
        next = getattribute sr, 'next'
        i -= 1
        .tailcall 'deep-fetch'(next, i, j)
return:
        .local pmc argument, ret
        argument= getattribute sr, 'argument'
        ret = argument[j]
        .return(ret)
.end

.sub 'r-extend*'
        .param pmc r
        .param pmc ns
        .tailcall 'cons'(ns, r)
.end

.sub 'cons'
        .param pmc car
        .param pmc cdr
        .local pmc cons
        cons = new "CONS"
        cons.'car!'(car)
        cons.'cdr!'(cdr)
        .return(cons)
.end

.sub 'consp' :multi(_)
        .param pmc x
        .return(0)
.end

.sub 'consp' :multi("CONS")
        .param pmc x
        .return(1)
.end

.sub 'atom'
        .param pmc x
        .local int ret
        ret = 'consp'(x)
        ret = not ret
        .return(ret)
.end

.sub 'symbolp' :multi(_)
        .param pmc x
        .return(0)
.end

.sub 'symbolp' :multi("SYMBOL")
        .param pmc x
        .return(1)
.end

.sub 'null'
        .param pmc x
        .t
        .nil
        eq_addr x, nil, true
        .return(0)
true:
        .return(1)
.end

.sub 'meaning'
        .param pmc e
        .param pmc r
        $I0 = 'consp'(e)
        if $I0 goto cons
atom:
        $I0 = 'symbolp'(e)
        if $I0 goto symbol
literal:
        .tailcall 'meaning-quote'(e, r)
symbol:
        .tailcall 'meaning-reference'(e, r)
cons:
        .local pmc car, cdr
        car = e.'car'()
        cdr = e.'cdr'()
        .tailcall 'meaning-application'(car, cdr, r)
.end

.sub 'meaning-reference'
        .param pmc n            # e
        .param pmc r
        .local pmc kind
        kind = 'compute-kind'(r, n)
        $I0 = isnull kind
        if $I0 goto error
        .local pmc type
        type = kind.'car'()
        if type ==  "local" goto local
        if type == "global" goto global
        if type == "predefined" goto predefined
        goto error
local:
        .local int i, j
        $P0 = kind.'cdr'()
        i = $P0.'car'()
        j = $P0.'cdr'()
        if i == 0 goto shallow
deep:
        .tailcall 'DEEP-ARGUMENT-REF'(i, j)
shallow:
        .tailcall 'SHALLOW-ARGUMENT-REF'(j)
global:
        .local int i
        i = kind.'cdr'()
        .tailcall 'CHECKED-GLOBAL-REF'(i)
predefined:
        .local int i
        i = kind.'cdr'()
        .tailcall 'PREDEFINED'(i)
error:
        $P0 = new 'Exception'
        $S0 = "No such variable "
        $S1 = n.'name'()
        $S0 .= $S1
        $P0 = $S0
        throw $P0
.end

.sub 'meaning-application' :multi("QUOTE", _, _)
        .param pmc quote
        .param pmc e
        .param pmc r
        .local pmc v
        v = e.'car'()
        .tailcall 'meaning-quote'(v, r)
.end

.sub 'meaning-application' :multi("IF", _, _)
        .param pmc _if
        .param pmc e
        .param pmc r
        .local pmc e1, e2, e3
        e1 = e.'car'()
        e2 = e.'cdr'()
        e3 = e2.'cdr'()
        e2 = e2.'car'()
        e3 = e3.'car'()
        .tailcall 'meaning-alternative'(e1, e2, e3, r)
.end

.sub 'meaning-application' :multi("SETQ", _, _)
        .param pmc setq
        .param pmc e
        .param pmc r
        .local pmc n, ee
        n = e.'car'()
        ee = e.'cdr'()
        ee = ee.'car'()
        .tailcall 'meaning-assignment'(n, ee, r)
.end

.sub 'meaning-application' :multi("PROGN", _, _)
        .param pmc progn
        .param pmc e
        .param pmc r
        .tailcall 'meaning-sequence'(e, r)
.end

.sub 'meaning-application' :multi("LAMBDA", _, _)
        .param pmc lambda
        .param pmc e
        .param pmc r
        .local pmc lambda_list, body
        lambda_list = e.'car'()
        body = e.'cdr'()
        .tailcall 'meaning-abstraction'(lambda_list, body, r)
.end

.sub 'meaning-application' :multi(_, _, _)
        .param pmc e
        .param pmc es
        .param pmc r
        $I0 = 'primitive?'(e, es, r)
        if $I0 goto primitive
        $I0 = isa e, "CONS"
        if $I0 goto cons
        goto regular
cons:
        $P0 = e.'car'()
        $I0 = isa $P0, "LAMBDA"
        if $I0 goto lambda
        goto regular
primitive:
        .tailcall 'meaning-primitive-application'(e, es, r)
lambda:
        .tailcall 'meaning-closed-application'(e, es, r)
regular:
        .tailcall 'meaning-regular-application'(e, es, r)
.end

.sub 'primitive?'
        .param pmc e
        .param pmc es
        .param pmc r
        $I0 = isa e, "SYMBOL"
        if $I0 == 0 goto false
        .local pmc kind, type
        kind = 'compute-kind'(r, e)
        $I0 = isnull kind
        if $I0 goto false
        type = kind.'car'()
        if type != "predefined" goto false
        .local pmc desc
        desc = 'get-description'(e)
        $I0 = isnull desc
        if $I0 goto false
        .local int es_len, arity
        es_len = 'length'(es)
        arity = desc.'cdr'()
        if es_len != arity goto error
        .return(1)
false:
        .return(0)
error:
        $P0 = new "STATIC-WRONG"
        $P0 = "Incorrect arity for primitive "
        $S0 = e
        $P0 .= $S0
        throw $P0
.end

.sub 'meaning-quote'
        .param pmc v
        .param pmc r
        .tailcall 'CONSTANT'(v)
.end

.sub 'meaning-alternative'
        .param pmc e1
        .param pmc e2
        .param pmc e3
        .param pmc r
        .local pmc m1, m2, m3
        m1 = 'meaning'(e1, r)
        m2 = 'meaning'(e2, r)
        m3 = 'meaning'(e3, r)
        .tailcall 'ALTERNATIVE'(m1, m2, m3)
.end

.sub 'meaning-assignment'
        .param pmc n
        .param pmc e
        .param pmc r
        .local pmc m, kind, type
        m = 'meaning'(e, r)
        kind = 'compute-kind'(r, n)
        $I0 = isnull kind
        if $I0 goto error
        type = kind.'car'()
        if type ==  "local" goto local
        if type == "global" goto global
        if type == "predefined" goto predefined
        goto error
local:
        .local int i, j
        $P0 = kind.'cdr'()
        i = $P0.'car'()
        j = $P0.'cdr'()
        if i == 0 goto shallow
deep:
        .tailcall 'DEEP-ARGUMENT-SET!'(i, j, m)
shallow:
        .tailcall 'SHALLOW-ARGUMENT-STE!'(j, m)
global:
        .local int i
        i = kind.'cdr'()
        .tailcall 'GLOBAL-SET!'(i, m)
predefined:
        $P0 = new "STATIC-WRONG"
        $P0 = "Immutable predefined variable "
        $S0 = n.'name'()
        $P0 .= $S0
        throw $P0
error:
        $P0 = new "STATIC-WRONG"
        $P0 = "No such variable "
        $S0 = n.'name'()
        $P0 .= $S0
        throw $P0
.end

.sub 'meaning-sequence'
        .param pmc e
        .param pmc r
        .nil
        $I0 = 'consp'(e)
        if $I0 goto cons
        .tailcall 'meaning'(nil, r)
cons:
        .local pmc car, cdr
        car = e.'car'()
        cdr = e.'cdr'()
        $I0 = 'consp'(cdr)
        if $I0 goto multi
single:
        .tailcall 'meaning-single-sequenece'(car, r)
multi:
        .tailcall 'meaning-multiple-sequenece'(car, cdr, r)
.end

.sub 'meaning-single-sequenec'
        .param pmc e
        .param pmc r
        .tailcall 'meaning'(e, r)
.end

.sub 'meaning-multiple-sequenece'
        .param pmc e
        .param pmc es
        .param pmc r
        .local pmc m, ms
        m = 'meaning'(e, r)
        ms = 'meaning-sequence'(es, r)
        .tailcall 'SEQUENEC'(m, ms)
.end

.sub 'meaning-abstraction'
        .param pmc lambda_list
        .param pmc body
        .param pmc r
        .local pmc rest, regular
        .nil
        rest = lambda_list
        regular = nil
parse:
        $I0 = 'consp'(rest)
        if $I0 goto cons
        $I0 = 'null'(rest)
        if $I0 goto fix
        goto dotted
cons:
        $P0 = rest.'car'()
        rest = rest.'cdr'()
        regular = 'cons'($P0, regular)
        goto parse
fix:
        .tailcall 'meaning-fix-abstraction'(lambda_list, body, r)
dotted:
        regular = 'reverse'(regular)
        .tailcall 'meaning-dotted-abstraction'(regular, rest, body, r)
.end

.sub 'meaning-fix-abstraction'
        .param pmc lambda_list
        .param pmc body
        .param pmc r
        .local int arity
        .local pmc r2, ms
        arity = 'length'(lambda_list)
        r2 = 'r-extend*'(r, lambda_list)
        ms = 'meaning-sequence'(body, r2)
        .tailcall 'FIX-CLOSURE'(ms, arity)
.end

.sub 'meaning-dotted-abstraction'
        .param pmc lambda_list
        .param pmc rest
        .param pmc body
        .param pmc r
        .local int arity
        .local pmc r2, ms, ns
        .nil
        arity = 'length'(lambda_list)
        ns = 'cons'(rest, nil)
        ns = 'append'(lambda_list, ns)
        r2 = 'r-extend*'(r, ns)
        ms = 'meaning-sequence'(body, r2)
        .tailcall 'NARY-CLOSURE'(ms, arity)
.end

.sub 'reverse'
        .param pmc x
        .param pmc acc
        .nil
        acc = nil
loop:
        $I0 = 'null'(x)
        if $I0 goto end
        .local pmc car
        car = x.'car'()
        x = x.'cdr'()
        acc = 'cons'(car, acc)
        goto loop
end:
        .return(acc)
.end

.sub 'length'
        .param pmc x
        .local int len
        len = 0
loop:
        $I0 = 'null'(x)
        if $I0 goto end
        len += 1
        x = x.'cdr'()
        goto loop
end:
        .return(len)
.end

.sub 'append'
        .param pmc x
        .param pmc y
        $I0 = 'null'(x)
        if $I0 goto end
        .local pmc a, b
        a = x.'car'()
        b = x.'cdr'()
        b = 'append'(b, y)
        y = 'cons'(a, b)
end:
        .return(y)
.end

.sub 'listify!'
        .param pmc vs
        .param int arity
        .local pmc argument, result
        .local int idx
        .nil
        result = nil
        argument = getattribute vs, 'argument'
        idx = argument
        idx -= 1
loop:
        if idx == arity goto end
        idx -= 1
        $P0 = argument[idx]
        result = 'cons'($P0, result)
        goto loop
end:
        argument[arity] = result
.end

.sub 'meaning-primitive-application'
        .param pmc e
        .param pmc es
        .param pmc r
        .local pmc desc, address, args, m, x
        .local int size, i
        desc = 'get-description'(e)
        address = desc.'car'()
        size = 'length'(es)
        args = new 'FixedPMCArray'
        args = size
        i = 0
loop:
        if i == size goto end
        x = es.'car'()
        m = 'meaning'(x, r)
        args[i] = m
        i += 1
        es = es.'cdr'()
        goto loop
end:
        .tailcall 'CALL'(address, args)
.end

.sub 'meaning-regular-application'
        .param pmc e
        .param pmc es
        .param pmc r
        .local pmc m, ms
        .local pmc es_len
        m = 'meaning'(e, r)
        es_len = 'length'(es)
        ms = 'meaning*'(es, r, es_len)
        .tailcall 'REGULAR-CALL'(m, ms)
.end

.sub 'meaning*'
        .param pmc es
        .param pmc r
        .param int size
        $I0 = isa es, "CONS"
        if $I0 goto cons
        .tailcall 'meaning-no-argument'(r, size)
cons:
        .local pmc car, cdr
        car = es.'car'()
        cdr = es.'cdr'()
        .tailcall 'meaning-some-argument'(car, cdr, r, size)
.end

.sub 'meaning-some-argument'
        .param pmc e
        .param pmc es
        .param pmc r
        .param int size
        .local pmc m, ms
        .local int rank, es_len
        m = 'meaning'(e, r)
        ms = 'meaning*'(es, r, size)
        es_len = 'length'(es)
        rank = size - es_len
        rank -= 1
        .tailcall 'STORE-ARGUMENT'(m, ms, rank)
.end

.sub 'meaning-no-argument'
        .param pmc r
        .param int size
        .tailcall 'ALLOCATE-FRAME'(size)
.end

.sub 'allocate-activation-frame'
        .param int size
        .local pmc af
        .local pmc argument
        argument = new 'FixedPMCArray'
        argument = size
        af = new "ACTIVATION-FRAME"
        setattribute af, 'argument', argument
        .return(af)
.end




.sub 'definitial'
        .param pmc symbol
        .param pmc value
        'g.init-initialize!'(symbol, value)
.end

.sub 'defprimitive'
        .param pmc symbol
        .param pmc value
        .param int arity
        .local pmc closure
        closure = 'behavior'(symbol, value, arity)
        .tailcall 'definitial'(symbol, closure)
.end

.sub 'behavior' :outer('defprimitive')
        .param pmc symbol
        .param pmc value
        .param pmc arity
        .lex 'symbol', symbol
        .lex 'value', value
        .local pmc arity_plus1
        arity_plus1 = arity + 1
        .lex 'arity_plus1', arity_plus1
        .local pmc behavior, closure, sr_init
        .const 'Sub' k = '%behavior'
        behavior = newclosure k
        closure = new "CLOSURE"
        setattribute closure, 'code', behavior
        sr_init = get_global "sr.init"
        setattribute closure, 'closed-environment', sr_init
        .local pmc description
        description = 'cons'(value, arity)
        'description-extend!'(symbol, description)
        .return(closure)
.end

.sub '%behavior' :outer('behavior')
        .param pmc vs
        .param pmc sr
        .local pmc arity_plus1, argument, value
        .local int i_arity_plus1, argument_len
        arity_plus1 = find_lex 'arity_plus1'
        i_arity_plus1 = arity_plus1
        argument = getattribute vs, 'argument'
        argument_len = argument
        if arity_plus1 != argument_len goto error
        value = find_lex 'value'
        .tailcall value(argument :flat)
error:
        $P0 = new 'Exception'
        $P0 = "Incorrect arity "
        .local pmc symbol
        symbol = find_lex 'symbol'
        $S0 = symbol
        $P0 .= $S0
        throw $P0
.end


.namespace [ "ACTIVATION-FRAME" ]

##.sub init :vtable
##        say "ACTIVATION-FRAME:init"
##        $P0 = new "ResizablePMCArray"
##        setattribute self, 'argument', $P0
##.end
