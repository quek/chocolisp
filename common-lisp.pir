.HLL "chocolisp"

.namespace [ "COMMON-LISP" ]

.include "parrot-macro.pir"

.sub '' :anon :load :init
.end

.sub 'ATOM'
        .param pmc x
        $I0 = isa x, ["CHOCO";"CONS"]
        if $I0 goto false
        .t
        .return(t)
false:
        .nil
        .return(nil)
.end

.sub 'CONS'
        .param pmc car
        .param pmc cdr
        .tailcall cons(car, cdr)
.end

.sub 'CAR'
        .param pmc cons
        $I0 = isa cons, ["CHOCO";"CONS"]
        if $I0 goto LCONS
        .nil
        eq_addr cons, nil, LNIL
        die "error"
LNIL:
        .return(nil)
LCONS:
        $P0 = getattribute cons, 'car'
        .return($P0)
.end

.sub 'CDR'
        .param pmc cons
        $I0 = isa cons, ["CHOCO";"CONS"]
        if $I0 goto LCONS
        .nil
        eq_addr cons, nil, LNIL
        die "error"
LNIL:
        .return(nil)
LCONS:
        $P0 = getattribute cons, 'cdr'
        .return($P0)
.end

.sub 'NULL'
        .param pmc x
        .nil
        eq_addr x, nil, true
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'FUNCALL'
        .param pmc f
        .param pmc args :slurpy
        .tailcall f(args :flat)
.end

.sub 'APPLY'
        .param pmc f
        .param pmc arg
        .param pmc rest :slurpy
        .nil
        $I0 = rest
        if $I0 goto rest_supplied
        $P0 = 'ATOM'(arg)
        eq_addr $P0, nil, error
        .tailcall f(arg :flat)
rest_supplied:
        .local pmc array
        array = new 'ResizablePMCArray'
        push array, arg
        .local pmc i
        i = iter rest
        $P0 = shift i
loop:
        unless i goto end
        push array, $P0
        $P0 = shift i
        goto loop
end:
        $P0 = list_to_array($P0)
        array.'append'($P0)
        .tailcall f(array :flat)
error:
        die "arg is atom."
.end

.sub 'EQ'
        .param pmc x
        .param pmc y
        eq_addr x, y, true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'LENGTH' :multi('String')
        .param string x
        $I0 = length x
        .return($I0)
.end

.sub 'LENGTH' :multi(["CHOCO";"CONS"])
        .param pmc x
        .nil
        $I0 = 0
loop:
        eq_addr x, nil, end
        $I0 += 1
        x = getattribute x, 'cdr'
        goto loop
end:
        .return($I0)
.end

.sub 'SUBSEQ' :multi('String')
        .param string s
        .param int start
        .param int end :optional
        .param int has_end :opt_flag
        if has_end goto L1
        $S0 = substr s, start
        .return($S0)
L1:
        $S0 = substr s, start, end
        .return($S0)
.end


.sub '='
        .param pmc x
        .param pmc y
        eq_num x, y, true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'FIND-PACKAGE'
        .param pmc x
        $P0 = find_package(x)
        .return(x)
.end

.sub 'STRING='
        .param pmc x
        .param pmc y
        eq_str x, y, true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'STRING<'
        .param pmc x
        .param pmc y
        lt_str x, y, true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'STRING<='
        .param pmc x
        .param pmc y
        le_str x, y, true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'TYPE-OF'
        .param pmc x
        .param pmc package
        package = find_package("COMMON-LISP")
        $I0 = isa x, ["CHOCO";"SYMBOL"]
        if $I0 goto SYMBOL
        $I0 = isa x, "String"
        if $I0 goto STRING
        $I0 = isa x, "Integer"
        if $I0 goto INTEGER
        .t
        .return(t)
SYMBOL:
        $P0 = package.'intern'("SYMBOL")
        .return($P0)
STRING:
        $P0 = package.'intern'("String")
        .return($P0)
INTEGER:
        $P0 = package.'intern'("Integer")
        .return($P0)
.end

.sub '+'
        .param pmc args :slurpy
        $P0 = box 0
loop:
        $I0 = args
        unless $I0 goto end
        $P1 = shift args
        $P0 += $P1
        goto loop
end:
        .return($P0)
.end

.sub '-'
        .param pmc first
        .param pmc rest :slurpy
        $I0 = rest
        if $I0 goto L1
        $P0 = neg first
        .return($P0)
L1:
        $P0 = first
        $P1 = iter rest
loop:
        unless $P1 goto end
        $P2 = shift $P1
        $P0 -= $P2
        goto loop
end:
        .return($P0)
.end


.sub 'PRINC'
        .param pmc x
        print x
        .return(x)
.end

.sub 'PRINT'
        .param pmc x
        say ""
        print x
        print " "
        .return(x)
.end

##################################################################
## dummy
.sub 'MAKE-BROADCAST-STREAM'
        .param pmc args :slurpy
        $P0 = args[0]
        .return($P0)
.end
