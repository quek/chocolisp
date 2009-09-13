.HLL "chocolisp"

.namespace [ "COMMON-LISP" ]

.include "parrot-macro.pir"

.sub '' :anon :load :init
.end

.sub 'CAR'
        .param pmc cons
        $I0 = isa cons, "CONS"
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
        $I0 = isa cons, "CONS"
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

.sub 'CONS'
        .param pmc car
        .param pmc cdr
        $P0 = new "CONS"
        setattribute $P0, 'car', car
        setattribute $P0, 'cdr', cdr
        .return($P0)
.end
