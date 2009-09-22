.HLL "chocolisp"

.namespace [ "COMMON-LISP" ]

.include "parrot-macro.pir"

.sub '' :anon :load :init
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
        .param pmc x
        $I0 = x.'length'()
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
        .param pmc s
        .param int start
        .param int end
        $S0 = s.'substr'(start, end)
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
