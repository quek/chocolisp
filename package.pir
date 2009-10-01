.namespace [ "COMMON-LISP";"PACKAGE" ]

.sub '' :anon :init :load
        $P0 = subclass ["COMMON-LISP";"ATOM"], ["COMMON-LISP";"PACKAGE"]
        addattribute $P0, 'name'
        addattribute $P0, 'nick-names'
        addattribute $P0, 'use-list'
        addattribute $P0, 'external-symbols'
        addattribute $P0, 'internal-symbols'
.end

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

.sub get_string :vtable
        $S0 = "#<PACKAGE \""
        $P1 = getattribute self, 'name'
        $S1 = $P1
        $S0 .= $S1
        $S0 .= "\">"
        .return($S0)
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
        if_null $P0, L1
        .return($P0)
L1:
        .local pmc uses, i, package
        uses = getattribute self, 'use-list'
        i = iter uses
loop:
        unless i goto L20
        package = shift i
        $P0 = package.'find-external-symbol'(name)
        if_null $P0, loop
        .return($P0)
L20:
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
        symbol = new ["COMMON-LISP";"SYMBOL"]
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

.sub  'intern-and-export' :method
        .param pmc name
        $P0 = self.'intern'(name)
        self.'export'($P0)
        .return($P0)
.end

.namespace [ "COMMON-LISP";"KEYWORD-PACKAGE" ]

.sub 'intern' :method
        .param string name
        .local pmc symbol
        symbol = self.'find-symbol'(name)
        $I0 = isnull symbol
        if $I0 goto intern
        .return(symbol)
intern:
        symbol = new ["COMMON-LISP";"SYMBOL"]
        symbol = name
        setattribute symbol, 'package', self
        setattribute symbol, 'value', self
        .local pmc external
        external = getattribute self, 'external-symbols'
        external[name] = symbol
        .return(symbol)
.end
