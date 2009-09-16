.namespace [ "CHOCO";"SYMBOL" ]

.sub init :vtable
        .nil
        setattribute self, 'plist', nil
        setattribute self, 'special-var-p', nil
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
        .t
        setattribute self, 'special-var-p', t
.end
