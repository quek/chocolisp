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

        .local pmc compiler, code, fasl1, fasl2
        compiler = compreg "PIR"
XXX:
        code = 'CONSTANT'(777)
        fasl1 = compiler(code)
        code = 'CONSTANT'(888)
        fasl2 = compiler(code)
        $P0 = fasl1()
        say $P0
        $P0 = fasl2()
        say $P0
        ##goto XXX
.end

.sub '' :anon :load :init
        say "Chocolisp..."

        .local pmc all_packages
        all_packages = new 'Hash'
        set_global "*all-packages*", all_packages

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
        all_packages["COMMON-LISP"] = common_lisp_package
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


.sub 'meaning'
        .param pmc e
        .param pmc r
        .param pmc f
        .param pmc d
        .param pmc tail

        $I0 = isa e, "CONS"
        if $I0 goto cons
atom:
        $I0 = isa e, "SYMBOL"
        if $I0 goto symbol
literal:
        .tailcall 'meaning-quote'(e, r, f, d, tail)
symbol:
        .tailcall 'meaning-reference'(e, r, f, tail)
cons:
        .local pmc car, cdr
        car = getattribute e, "car"
        cdr = getattribute e, "cdr"
        .tailcall 'meaning-application'(car, cdr, r, f, d, tail)
.end

.sub 'meaning-quote'
        .param pmc v
        .param pmc r
        .param pmc f
        .param pmc d
        .param pmc tail
        'CONSTANT'(v)
.end


.sub 'CONSTANT'
        .param pmc v
        .local pmc code
        code = new 'CodeString'
        code.'emit'(<<"        HERE", 'v'=>v)
        .sub '__constant__'
                .return(%v)
        .end
        HERE
        .return(code)
.end

## Objectification
.sub 'initialize classes for Objectification' :anon :load :init
        $P0 = newclass "Object"

        $P0 = subclass "Object", "Reference"
        addattribute $P0, 'variable'

        $P0 = subclass "Reference", "LocalReference"

        $P0 = subclass "Reference", "GlobalReference"

        $P0 = subclass "Reference", "PredefinedReference"

        $P0 = subclass "Program", "GlobalAssignment"
        addattribute $P0, 'variable'
        addattribute $P0, 'form'

        $P0 = subclass "Program", "LocalAssignment"
        addattribute $P0, 'reference'
        addattribute $P0, 'form'

        $P0 = subclass "Program", "Function"
        addattribute $P0, 'variables'
        addattribute $P0, 'body'

        $P0 = subclass "Program", "Alternative"
        addattribute $P0, 'condition'
        addattribute $P0, 'consequent'
        addattribute $P0, 'alternate'

        $P0 = subclass "Program", "Sequence"
        addattribute $P0, 'first'
        addattribute $P0, 'last'

        $P0 = subclass "Program", "Constant"
        addattribute $P0, 'value'
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
