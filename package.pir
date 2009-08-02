.namespace [ "PACKAGE" ]

.sub 'find-symbol' :method
        .param string name
        .local pmc external
        external = getattribute self, 'external-symbols'
        $P0 = external[name]
        $I0 = isnull $P0
        if $I0 goto l1
        .return($P0)
l1:
        .local pmc internal
        internal = getattribute self, 'internal-symbols'
        $P0 = internal[name]
        .return($P0)
.end

.sub 'intern' :method
        .param string name
        .local pmc symbol
        symbol = self.'find-symbol'(name)
        $I0 = isnull symbol
        if $I0 goto intern
        .return(symbol)
intern:
        symbol = new "SYMBOL"
        symbol.'name!'(name)
        symbol.'package!'(self)
        $P0 = getattribute self, 'internal-symbols'
        $P0[name] = symbol
        .return(symbol)
.end

.sub '%intern' :method
        .param string class_name
        .param string symbol_name
        .local pmc symbol
        symbol = new class_name
        symbol.'name!'(symbol_name)
        symbol.'package!'(self)
        $P0 = getattribute self, 'external-symbols'
        $P0[symbol_name] = symbol
        .return(symbol)
.end

.sub '%%intern' :method
        .param string name
        .tailcall self.'%intern'(name, name)
.end
