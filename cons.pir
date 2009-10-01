.namespace ["COMMON-LISP";"CONS"]

.sub get_string :vtable
        .local pmc car, cdr
        .nil
        cdr = self
        $S0 = "("
        goto l1
loop:
        $S0 .= " "
l1:
        car = getattribute cdr, 'car'
        $S1 = car
        $S0 .= $S1
        cdr = getattribute cdr, 'cdr'
        $I0 = isa cdr, ["COMMON-LISP";"CONS"]
        if $I0 goto loop
        eq_addr cdr, nil, end
        $S0 .= " . "
        $S1 = cdr
        $S0 .= $S1
end:
        $S0 .= ")"
        .return($S0)
.end
