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


.sub 'PRINT'
        .param pmc x
        say ""
        print x
        print " "
        .return(x)
.end
