.namespace [ "CONS" ]

.sub get_string :vtable :method
        .local pmc car, cdr
        car = self.'car'()
        cdr = self.'cdr'()
        $S0 = "("
        $S1 = car
        $S0 .= $S1
        $S0 .= " . "
        $S1 = cdr
        $S0 .= $S1
        $S0 .= ")"
        .return($S0)
.end

.sub 'car' :method
        $P0 = getattribute self, 'car'
        .return($P0)
.end

.sub 'car!' :method
        .param pmc v
        setattribute self, 'car', v
.end

.sub 'cdr' :method
        $P0 = getattribute self, 'cdr'
        .return($P0)
.end

.sub 'cdr!' :method
        .param pmc v
        setattribute self, 'cdr', v
.end
