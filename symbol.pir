.namespace [ "CHOCO";"SYMBOL" ]

.sub init :vtable
        .nil
        setattribute self, 'plist', nil
.end

.sub get_string :vtable
        $P0 = getattribute self, 'name'
        $S0 = $P0
        .return($S0)
.end

.sub set_string_native :vtable
        .param pmc str
        setattribute self, 'name', str
.end

.sub specialize :method
        $P0 = getattribute self, 'dynamic-values'
        unless_null $P0, end
        $P1 = new 'ResizablePMCArray'
        setattribute self, 'dynamic-values', $P1
end:
.end

.sub push_dynamic_value :method
        .param pmc value
        $P0 = getattribute self, 'dynamic-values'
        push $P0, value
.end

.sub pop_dynamic_value :method
        $P0 = getattribute self, 'dynamic-values'
        $P1 = pop $P0
        .return($P1)
.end

.sub get_dynamic_value :method
        $P0 = getattribute self, 'dynamic-values'
        $I0 = $P0
        $I0 -= 1
        $P1 = $P0[$I0]
        .return($P1)
.end
