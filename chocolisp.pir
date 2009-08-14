.HLL "chocolisp"

.namespace [ "CHOCO" ]

.macro nil
        .local pmc nil
        nil = get_global "NIL"
.endm

.macro t
        .local pmc t
        t = get_global "T"
.endm

.sub main :main
        .t
        .nil
        say t
        say nil
.end

.sub '' :anon :load :init
        say "Chocolisp..."

        $P0 = new 'Hash'
        set_global "*all-packages*", $P0

        $P0 = newclass "T"

        $P0 = subclass "T", "CONS"
        addattribute $P0, 'car'
        addattribute $P0, 'cdr'

        $P0 = subclass "T", "ATOM"

        $P0 = subclass "ATOM", "PACKAGE"
        addattribute $P0, 'name'
        addattribute $P0, 'nick-names'
        addattribute $P0, 'use'
        addattribute $P0, 'external-symbols'
        addattribute $P0, 'internal-symbols'

        $P0 = subclass "ATOM", "SYMBOL"
        addattribute $P0, 'name'
        addattribute $P0, 'value'
        addattribute $P0, 'function'
        addattribute $P0, 'plist'
        addattribute $P0, 'package'


        .local pmc common_lisp_package
        common_lisp_package = new "PACKAGE"
        common_lisp_package = "COMMON-LISP"
        set_global "COMMON-LISP", common_lisp_package

        .local pmc nil
        nil = common_lisp_package.'intern'("NIL")
        setattribute nil, 'value', nil
        set_global "NIL", nil

        .local pmc t
        t = common_lisp_package.'intern'("T")
        setattribute t, 'value', t
        set_global "T", t
.end

.namespace [ "PACKAGE" ]

.sub init :vtable
        $P0 = new 'Hash'
        setattribute self, 'external-symbols', $P0
        $P0 = new 'Hash'
        setattribute self, 'internal-symbols', $P0
.end

.sub set_string_native :vtable
        .param pmc str
        setattribute self, 'name', str
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
        symbol = name
        setattribute symbol, 'package', self
        .local pmc external
        external = getattribute self, 'external-symbols'
        external[name] = symbol
        .return(symbol)
.end

.namespace [ "SYMBOL" ]

.sub get_string :vtable
        $P0 = getattribute self, 'name'
        $S0 = $P0
        .return($S0)
.end

.sub set_string_native :vtable
        .param pmc str
        setattribute self, 'name', str
.end
