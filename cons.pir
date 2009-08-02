.namespace [ "CONS" ]

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
