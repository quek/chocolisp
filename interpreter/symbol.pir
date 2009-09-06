.namespace [ "SYMBOL" ]

.sub get_string :vtable :method
        $P0 = getattribute self, 'name'
        .return($P0)
.end

.sub 'name' :method
        $P0 = getattribute self, 'name'
        .return($P0)
.end

.sub 'name!' :method
        .param pmc name
        setattribute self, 'name', name
.end

.sub 'value' :method
        $P0 = getattribute self, 'value'
        .return($P0)
.end

.sub 'value!' :method
        .param pmc value
        setattribute self, 'value', value
.end

.sub 'function' :method
        $P0 = getattribute self, 'function'
        .return($P0)
.end

.sub 'function!' :method
        .param pmc function
        setattribute self, 'function', function
.end

.sub 'package' :method
        $P0 = getattribute self, 'package'
        .return($P0)
.end

.sub 'package!' :method
        .param pmc package
        setattribute self, 'package', package
.end
