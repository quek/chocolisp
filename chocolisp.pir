.HLL "chocolisp"

.sub 'PRINT'
        .param pmc x
        say x
.end

.namespace [ "CHOCO" ]

.include "parrot-macro.pir"

.sub main :main
        .t
        .nil
        say t
        say nil

        .local pmc compiler
        compiler = compreg "PIR"

        .local pmc e, r, f, d, tail
        e = nil
        r = nil
        f = nil
        d = nil
        tail = nil
        $P0 = 'meaning'(e, r, f, d, tail)
        $P1 = inspect $P0
        say $P1

        $P0 = get_hll_global [ "COMMON-LISP" ], "CONS"
        $P1 = $P0("car value", "cdr value")
        $P0 = get_hll_global [ "COMMON-LISP" ], "CAR"
        $P1 = $P0($P1)
        say $P1

        say "==== start compiler/a.pbc ===="
        load_bytecode "compiler/a.pbc"
        say "==== end compiler/a.pbc ===="
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

        $P0 = common_lisp_package.'intern'("QUOTE")
        set_global "QUOTE", $P0
        $P0 = common_lisp_package.'intern'("LAMBDA")
        set_global "LAMBDA", $P0
        $P0 = common_lisp_package.'intern'("IF")
        set_global "IF", $P0
        $P0 = common_lisp_package.'intern'("PROGN")
        set_global "PROGN", $P0
        $P0 = common_lisp_package.'intern'("SETQ")
        set_global "SETQ", $P0
        $P0 = common_lisp_package.'intern'("DEFUN")
        set_global "DEFUN", $P0

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
        .tailcall 'meaning-reference'(e, r, f, d, tail)
cons:
        .local pmc car, cdr, _quote, _lambda, _if, _progn, _setq, _defun
        car = getattribute e, "car"
        cdr = getattribute e, "cdr"

        _quote = get_global "QUOTE"
        _lambda = get_global "LAMBDA"
        _if = get_global "IF"
        _progn = get_global "PROGN"
        _setq = get_global "SETQ"
        _defun = get_global "DEFUN"
        eq_addr car, _quote,  lquote
        eq_addr car, _lambda, llambda
        eq_addr car, _if,     lif
        eq_addr car, _progn,  lprogn
        eq_addr car, _setq,   lsetq
        eq_addr car, _defun,   ldefun
else:
        .tailcall 'meaning-application'(car, cdr, r, f, d, tail)
lquote:
        .local pmc cadr
        cadr = getattribute cdr, 'car'
        .tailcall 'meaning-quote'(cadr, r, f, d, tail)
llambda:
        .local pmc card, cddr
        cadr = getattribute cdr, 'car'
        cddr = getattribute cdr, 'cdr'
        .tailcall 'meaning-abstraction'(cadr, cddr, r, f, d, tail)
lif:
        .local pmc cadr, caddr, cadddr
        cadr = getattribute cdr, 'car'
        cadddr = getattribute cdr, 'cdr'
        caddr = getattribute cadddr, 'car'
        cadddr = getattribute cadddr, 'cdr'
        cadddr = getattribute cadddr, 'car'
        .tailcall 'meaning-alternative'(cadr, caddr, cadddr, r, f, d, tail)
lprogn:
        .tailcall 'meaning-sequence'(cdr, r, f, d, tail)
lsetq:
        .local pmc cadr, caddr
        cadr = getattribute cdr, 'car'
        caddr = getattribute cdr, 'cdr'
        caddr = getattribute caddr, 'car'
        .tailcall 'meaning-assignment'(cadr, caddr, r, f, d, tail)
ldefun:
        .local pmc cadr, caddr
        cadr = getattribute cdr, 'car'
        caddr = getattribute cdr, 'cdr'
        caddr = getattribute caddr, 'car'
        .tailcall 'meaning-definition'(cadr, caddr, r, f, d, tail)
.end

.sub 'meaning-quote'
        .param pmc v
        .param pmc r
        .param pmc f
        .param pmc d
        .param pmc tail
        .tailcall 'CONSTANT'(v)
.end

.sub 'meaning-alternative'
        .param pmc ec
        .param pmc et
        .param pmc ef
        .param pmc r
        .param pmc f
        .param pmc d
        .param pmc tail
        .local pmc mc, mt, mf
        mc = 'meaning'(ec, r, f, d, tail)
        mt = 'meaning'(ec, r, f, d, tail)
        mf = 'meaning'(ec, r, f, d, tail)
        .tailcall 'ALTERNATIVE'(mc, mt, mf)
.end

.sub 'meaning-reference'
        .param pmc symbol
        .param pmc r
        .param pmc f
        .param pmc d
        .param pmc tail
        $I0 = 'compute-kind'(symbol, r, d)
        if $I0 == 1 goto local
        if $I0 == 2 goto global
        if $I0 == 3 goto dynamic
local:
        $P0 = new "LocalReference"
        setattribute $P0, 'variable', symbol
        .return($P0)
global:
        $P0 = new "GlobalReference"
        setattribute $P0, 'variable', symbol
        .return($P0)
dynamic:
        $P0 = new "DynamicReference"
        setattribute $P0, 'variable', symbol
        .return($P0)
.end

.sub 'compute-kind'
        .param pmc symbol
        .param pmc r
        .param pmc d
        ## p.49 あたり
        .return (1)
.end

.sub 'meaning-assignment'
        .param pmc lhs
        .param pmc rhs
        .param pmc r
        .param pmc f
        .param pmc d
        .param pmc tail

        .local pmc reference, form
        reference = 'meaning'(lhs, r, f, d, tail)
        form = 'meaning'(rhs, r, f, d, tail)
        $I0 = isa reference, "LocalReference"
        if $I0 goto local
        $I0 = isa reference, "GlobalReference"
        if $I0 goto global
        $I0 = isa reference, "DynamicReference"
        if $I0 goto dynamic
        die "meaning-assignment"
local:
        $P0 = new "LocalAssignment"
        setattribute $P0, 'reference', reference
        setattribute $P0, 'form', form
        .return($P0)
global:
        $P0 = new "GlobalAssignment"
        .local pmc variable
        variable = getattribute reference, 'variable'
        setattribute $P0, 'variable', variable
        setattribute $P0, 'form', form
        .return($P0)
dynamic:
        $P0 = new "DynamicAssignment"
        setattribute $P0, 'reference', reference
        setattribute $P0, 'form', form
        .return($P0)
.end

.sub 'meaning-application'
        .param pmc function
        .param pmc arguments
        .param pmc r
        .param pmc f
        .param pmc d
        .param pmc tail

        .local pmc parrotArguments
        parrotArguments = 'arguments lisp -> parrot'(arguments, r, f, d, tail)
.end

.sub 'arguments lisp -> parrot'
        .param pmc arguments
        .param pmc r
        .param pmc f
        .param pmc d
        .param pmc tail
        .nil
        eq_addr arguments, nil, end
cons:
        .local pmc car, cdr, first, others
        car = getattribute arguments, 'car'
        first = 'meaning'(car, r, d, f, tail)
        $P0 = new "Arguments"
        setattribute $P0, 'first', first
        cdr = getattribute arguments, 'cdr'
        others = 'arguments lisp -> parrot'
        setattribute $P0, 'others', others
        .return($P0)
end:
        $P0 = new "NoArgument"
        .return($P0)
.end


#############################################################################
.sub 'CONSTANT'
        .param pmc v
        $P0 = new "Constant"
        setattribute $P0, 'value', v
        .return($P0)
.end

.sub 'ALTERNATIVE'
        .param pmc mc
        .param pmc mt
        .param pmc mf
        $P0 = new "Alternative"
        setattribute $P0, 'condition', mc
        setattribute $P0, 'consequent', mt
        setattribute $P0, 'alternate', mf
        .return($P0)
.end

## Objectification
.sub 'initialize classes for Objectification' :anon :load :init
        $P0 = newclass "Object"

        $P0 = subclass "Object", "Program"

        $P0 = subclass "Program", "Reference"
        addattribute $P0, 'variable'

        $P0 = subclass "Reference", "LocalReference"

        $P0 = subclass "Reference", "GlobalReference"

        $P0 = subclass "Reference", "PredefinedReference"

        $P0 = subclass "Reference", "DynamicReference"

        $P0 = subclass "Program", "LocalAssignment"
        addattribute $P0, 'reference'
        addattribute $P0, 'form'

        $P0 = subclass "Program", "GlobalAssignment"
        addattribute $P0, 'variable'
        addattribute $P0, 'form'

        $P0 = subclass "Program", "DynamicAssignment"
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

        $P0 = subclass "Program", "Application"

        $P0 = subclass "Application", "RegularApplication"
        addattribute $P0, 'function'
        addattribute $P0, 'arguments'

        $P0 = subclass "Application", "PredefinedApplication"
        addattribute $P0, 'variable'
        addattribute $P0, 'arguments'

        $P0 = subclass "Program", "FixLet"
        addattribute $P0, 'variables'
        addattribute $P0, 'arguments'
        addattribute $P0, 'body'

        $P0 = subclass "Program", "Arguments"
        addattribute $P0, 'first'
        addattribute $P0, 'others'

        $P0 = subclass "Program", "NoArgument"

        $P0 = subclass "Object", "Variable"
        addattribute $P0, 'name'

        $P0 = subclass "Variable", "GlobalVariable"

        $P0 = subclass "Variable", "PredefiendVariable"
        addattribute $P0, 'description'

        $P0 = subclass "Variable", "LocalVariable"
        addattribute $P0, 'mutable?'
        addattribute $P0, 'dotted?'
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


.include "common-lisp.pir"
