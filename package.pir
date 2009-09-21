.namespace [ "CHOCO";"PACKAGE" ]

.sub init :vtable
        $P0 = new 'Hash'
        setattribute self, 'external-symbols', $P0
        $P0 = new 'Hash'
        setattribute self, 'internal-symbols', $P0
        $P0 = new 'ResizablePMCArray'
        setattribute self, 'use-list', $P0
        $P0 = new 'ResizablePMCArray'
        setattribute self, 'nick-names', $P0
.end

.sub set_string_native :vtable
        .param pmc str
        setattribute self, 'name', str
.end

.sub 'find-external-symbol' :method
        .param string name
        .local pmc external
        external = getattribute self, 'external-symbols'
        $P0 = external[name]
        .return($P0)
.end

.sub 'find-symbol' :method
        .param string name
        .local pmc external
        $P0 = self.'find-external-symbol'(name)
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
        symbol = new ["CHOCO";"SYMBOL"]
        symbol = name
        setattribute symbol, 'package', self
        .local pmc internal
        internal = getattribute self, 'internal-symbols'
        internal[name] = symbol
        .return(symbol)
.end

.sub 'export' :method
        .param pmc symbol
        .local pmc symbol_name
        symbol_name = getattribute symbol, 'name'
        .local pmc external
        external = getattribute self, 'external-symbols'
        external[symbol_name] = symbol
        .local pmc internal
        internal = getattribute self, 'internal-symbols'
        delete internal[symbol_name]
.end
