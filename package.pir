.namespace [ "PACKAGE" ]

.sub init :load :init
        .local pmc package
        package = new "PACKAGE"
        $P0 = new 'String'
        $P0 = "COMMON-LISP"
        setattribute package, 'name', $P0
        $P0 = new 'Hash'
        setattribute package, 'external-symbols', $P0
        $P0 = new 'Hash'
        setattribute package, 'internal-symbols', $P0
        set_hll_global ["CHIMACHO"], "*PACKAGE*", package
.end

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
        .local pmc external
        external = getattribute self, 'external-symbols'
        external[name] = symbol
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
