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

.macro package
        .local pmc package
        package = get_hll_global ["CHIMACHO"], "*PACKAGE*"
.endm

.include "classes.pir"
.include "cons.pir"
.include "package.pir"
.include "symbol.pir"

.sub main :main
        say "Fast Interpretation"
.end

.sub init :load :init
        say "init :load :init"

        .local pmc package
        package = new "PACKAGE"
        $P0 = new 'String'
        $P0 = "COMMON-LISP"
        setattribute package, 'name', $P0
        $P0 = new 'ResizablePMCArray'
        setattribute package, 'external-symbols', $P0
        $P0 = new 'ResizablePMCArray'
        setattribute package, 'internal-symbols', $P0
        set_global "*PACKAGE*", package

        .local pmc nil
        nil = package.'%intern'("NULL", "NIL")
        nil.'value!'(nil)
        set_global "NIL", nil

        .local pmc t
        t = package.'%intern'("NULL", "T")
        t.'value!'(t)
        set_global "T", t

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
        'deep-fetch'(next, i, j)
return:
        .local pmc argument, ret
        argument= getattribute sr, 'argument'
        ret = argument[j]
        .return(ret)
.end

.sub 'r-extend*'
        .param pmc r
        .param pmc ns
        r.'unshift'(ns)
        .return(r)
.end

.sub 'cons'
        .param pmc car
        .param pmc cdr
        .local pmc cons
        cons = new "CONS"
        cons.'car'(car)
        cons.'cdr'(cdr)
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

.sub 'local-variable?'
        .param pmc r
        .param int i
        .param pmc n
        $I0 = 'consp'(r)
        if $I0 goto l1
        .return(0)
l1:
        ## ちょっとわからなくなった。
.end

.sub 'meaning'
        .param pmc e
        .param pmc r
        $I0 = 'cons'(e)
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

.sub 'meaning-application' :multi("QUOTE", _, _)
        .param pmc quote
        .param pmc e
        .param pmc r
        'meanig-quote'(e, r)
.end

.sub 'meanig-quote'
        .param pmc e
        .param pmc r
        .local pmc v
        v = e.'car'()
        .lex 'v', v
        .const 'Sub' k = '%meaning-quote'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%meaning-quote' :outer('meanig-quote')
        .param pmc sr
        .param pmc k
        .local pmc v
        v = find_lex 'v'
        .tailcall k(v)
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

.sub 'meaning-alternative'
        .param pmc e1
        .param pmc e2
        .param pmc e3
        .param pmc r
        .local pmc m1, m2, m3
        m1 = 'meaning'(e1, r)
        m2 = 'meaning'(e2, r)
        m3 = 'meaning'(e3, r)
        .lex 'm1', m1
        .lex 'm2', m2
        .lex 'm3', m3
        .const 'Sub' k = '%meaning-alternative'
        $P0 = newclosure k
        .return($P0)
.end

.sub '%meaning-alternative' :outer('meaning-alternative')
        .param pmc sr
        .param pmc k
        .local pmc m1
        .lex 'sr', sr
        .lex 'k', k
        m1 = find_lex 'm1'
        .const 'Sub' kk = '%%meaning-alternative'
        $P0 = newclosure kk
        .tailcall m1(sr, $P0)
.end

.sub '%%meaning-alternative' :outer('%meaning-alternative')
        .param pmc v
        .local pmc m2, m3, sr, k
        m2 = find_lex 'm2'
        m3 = find_lex 'm3'
        sr = find_lex 'sr'
        k = find_lex 'k'
        $I0 = 'null'(k)
        if $I0 goto false
true:
        .tailcall m2(sr, k)
false:
        .tailcall m3(sr, k)
.end




.namespace [ "ACTIVATION-FRAME" ]

.sub init :vtable
        $P0 = new "ResizablePMCArray"
        setattribute self, 'argument', $P0
.end
