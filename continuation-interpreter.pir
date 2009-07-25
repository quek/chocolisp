=head1 chocolisp

=cut

.HLL 'choco'

.namespace [ "CHIMACHO" ]

.macro nil
        .local pmc nil
        nil = get_root_namespace ["choco"; "CHIMACHO"]
        nil = nil.'find_var'("NIL")
.endm

.macro tailcall_eval(e, r, k)
        .local pmc _evaluate_
        _evaluate_ = get_root_namespace ["choco"; "CHIMACHO"]
        _evaluate_ = _evaluate_.'find_sub'("evaluate")
        .tailcall _evaluate_(.e, .r, .k)
.endm

.include "cnt-test.pir"
.include "cnt-primitive.pir"
.include "read.pir"


.macro define_reader (name, attr)
        .sub .name :method
                $P0 = getattribute self, .attr
                .return($P0)
        .end
.endm

 .macro define_writer (name, attr)
        .sub .name :method
                .param pmc new_value
                setattribute self, .attr, new_value
        .end
.endm


.sub 'main' :main
        '%initialize'()
        'run-test'()
.end

.sub '%initialize'
        '%define-classes'()

        .local pmc package
        package = '%make-package'("CHIMACHO")
        set_global "ROOT-PACKAGE", package
        $P1 = package.'intern'("*PACKAGE*")
        $P1.'setf-symbol-value'(package)
        set_global "*PACKAGE*", $P1

        .local pmc nil
        nil = new "NULL"
        nil.'setf-symbol-value'($P0)
        nil.'setf-symbol-name'("NIL")
        nil.'setf-symbol-package'(package)
        $P2 = package.'internal-symbols'()
        $P2["NIL"] = nil
        set_global "NIL", nil


        .local pmc t
        t = package.'intern'("T")
        t.'setf-symbol-value'(t)
        set_global "T", t

        .local pmc lambda
        lambda = package.'intern'("LAMBDA")
        set_global "LAMBDA", lambda

        'init-primitive'(package)

        'init-special-operator'(package)
.end

.sub 'define-special-operator'
        .param string name
        .param pmc package
        .local pmc object, klass

        klass = subclass "SYMBOL", name
        object = new name
        object.'setf-symbol-name'(name)
        object.'setf-symbol-package'(package)
        $P1 = package.'internal-symbols'()
        $P1[name] = object
.end

.sub 'init-special-operator'
        .param pmc package
        'define-special-operator'("QUOTE", package)
        'define-special-operator'("IF", package)
        'define-special-operator'("PROGN", package)
.end


.sub '%define-classes'

        $P0 = newclass "VALUE"

        $P0 = subclass "VALUE", "CONS"
        addattribute $P0, 'car'
        addattribute $P0, 'cdr'

        $P0 = subclass "VALUE", "ATOM"

        $P0 = subclass "ATOM", "SYMBOL"
        addattribute $P0, 'name'
        addattribute $P0, 'value'
        addattribute $P0, 'function'
        addattribute $P0, 'package'
        addattribute $P0, 'plist'

        $P0 = subclass "SYMBOL", "NULL"

        $P0 = subclass "ATOM", "FUNCTION"
        addattribute $P0, 'name'
        addattribute $P0, 'lambda-list'
        addattribute $P0, 'body'

        $P0 = subclass "FUNCTION", "SPECIAL-OPERATOR"

        $P0 = subclass "FUNCTION", "CLOSURE"
        addattribute $P0, 'venv'
        addattribute $P0, 'fenv'

        $P0 = subclass "CLOSURE", "MACRO"

        $P0 = subclass "ATOM", "PACKAGE"
        addattribute $P0, 'name'
        addattribute $P0, 'nick-names'
        addattribute $P0, 'use-list'
        addattribute $P0, 'external-symbols'
        addattribute $P0, 'internal-symbols'

        $P0 = subclass "ATOM", "CONTINUATION"
        addattribute $P0, 'k'

        $P0 = subclass "CONTINUATION", "BOTTOM-CONTINUATION"

        $P0 = subclass "CONTINUATION", "IF-CONT"
        addattribute $P0, 'et'
        addattribute $P0, 'ef'
        addattribute $P0, 'r'

        $P0 = subclass "ATOM", "ENVIRONMENT"
.end


.sub 'cons'
        .param pmc car
        .param pmc cdr

        $P0 = new "CONS"
        setattribute $P0, 'car', car
        setattribute $P0, 'cdr', cdr
        .return($P0)
.end


.sub '%make-package'
        .param string name
        $P0 = new "PACKAGE"
        $P0.'setf-name'(name)
        $P1 = new 'ResizablePMCArray'
        $P0.'setf-nick-names'($P1)
        $P1 = new 'ResizablePMCArray'
        $P0.'setf-use-list'($P1)
        $P1 = new 'Hash'
        $P0.'setf-external-symbols'($P1)
        $P1 = new 'Hash'
        $P0.'setf-internal-symbols'($P1)
        .return($P0)
.end


.sub 'evaluate'
        .param pmc e
        .param pmc r
        .param pmc k
        $I1 = isa e, "VALUE"
        if $I1 goto value
        .tailcall k.'resume'(e)
value:
        .tailcall e.'evaluate'(r, k)
.end



.namespace [ "VALUE" ]

.sub 'evaluate' :method
        .param pmc r
        .param pmc k
        .tailcall k.'resume'(self)
.end

.sub 'evaluate-application' :method
        .param pmc e
        .param pmc r
        .param pmc k
        $P0 = new "Exception"
        $S0 = "not applicable!"
        $P0 = $S0
        throw $P0
.end


.namespace [ "QUOTE" ]

.sub 'evaluate-application' :method
        .param pmc e
        .param pmc r
        .param pmc k
        .local pmc car
        car = e.'car'()
        .tailcall k.'resume'(car)
.end


.namespace [ "NULL" ]

.sub 'car' :method
        .return(self)
.end

.sub 'cdr' :method
        .return(self)
.end

.sub get_string :vtable :method
        .return("nil")
.end


.namespace [ "CONS" ]
.define_reader('car', 'car')
.define_reader('cdr', 'cdr')
.define_writer ('rplaca', 'car')
.define_writer ('rplacd', 'cdr')

.sub 'evaluate' :method
        .param pmc r
        .param pmc k
        .local pmc car, cdr
        car = self.'car'()
        cdr = self.'cdr'()
        .tailcall car.'evaluate-application'(cdr, r, k)
.end

.sub get_string :vtable :method
        .local pmc car
        .local pmc cdr
        car = self.'car'()
        cdr = self.'cdr'()
        $S0 = car
        $S1 = cdr
        $S2 = $S0 . " "
        $S2 .= $S1
        .return($S2)
.end


.namespace [ "SYMBOL" ]

.define_reader('symbol-name', 'name')
.define_reader('symbol-value', 'value')
.define_reader('symbol-function', 'function')
.define_reader('symbol-package', 'package')
.define_reader('symbol-plist', 'plist')
.define_writer('setf-symbol-name', 'name')
.define_writer('setf-symbol-value', 'value')
.define_writer('setf-symbol-function', 'function')
.define_writer('setf-symbol-package', 'package')
.define_writer('setf-symbol-plist', 'plist')

.sub get_string :vtable :method
        $S0 = self.'symbol-name'()
        .return($S0)
.end


.namespace [ "FUNCTION" ]
.define_reader('name', 'name')
.define_reader('body', 'body')
.define_reader('lambda-list', 'lambda-list')
.define_writer('setf-name', 'name')
.define_writer('setf-body', 'body')
.define_writer('setf-lambda-list', 'lambda-list')


.namespace [ "CLOSURE" ]
.define_reader('venv', 'venv')
.define_reader('fenv', 'fenv')
.define_writer('setf-venv', 'venv')
.define_writer('setf-fenv', 'fenv')


.namespace [ "PACKAGE" ]

.define_reader('name', 'name')
.define_reader('nick-names', 'nick-names')
.define_reader('use-list', 'use-list')
.define_reader('external-symbols', 'external-symbols')
.define_reader('internal-symbols', 'internal-symbols')
.define_writer('setf-name', 'name')
.define_writer('setf-nick-names', 'nick-names')
.define_writer('setf-use-list', 'use-list')
.define_writer('setf-external-symbols', 'external-symbols')
.define_writer('setf-internal-symbols', 'internal-symbols')

.sub '%find-symbol' :method
        .param string name
        .local pmc external_symbols
        external_symbols = self.'external-symbols'()
        $P0 = external_symbols[name]
        $I0 = isnull $P0
        if $I0 goto next1
        .return($P0)
next1:
        .local pmc internal_symbols
        internal_symbols = self.'internal-symbols'()
        $P0 = internal_symbols[name]
        $I0 = isnull $P0
        if $I0 goto next2
        .return($P0)
next2:
        null $P0
        .return($P0)
.end

.sub 'intern' :method
        .param string name
        $P0 = self.'%find-symbol'(name)
        $I0 = isnull $P0
        unless $I0 goto end
        $P0 = new "SYMBOL"
        $P0.'setf-symbol-name'(name)
        $P0.'setf-symbol-package'(self)
        $P1 = self.'internal-symbols'()
        $P1[name] = $P0
end:
        .return($P0)
.end


.namespace [ "CONTINUATION" ]
.define_reader('k', 'k')
.define_writer('setf-k', 'k')

.sub 'rusume' :method
        .param pmc v
        $P0 = new "Exception"
        $S0 = "unknown continuation!"
        $P0 = $S0
        throw $P0
.end


.namespace [ "BOTTOM-CONTINUATION" ]

.sub 'resume' :method
        .param pmc v
        ##say v
        .return(v)
.end


.namespace [ "IF" ]

.sub 'evaluate-application' :method
        .param pmc e
        .param pmc r
        .param pmc k
        .local pmc ec, et, ef, if_cont
        ec = e.'car'()
        e = e.'cdr'()
        et = e.'car'()
        e = e.'cdr'()
        ef = e.'car'()
        if_cont = new "IF-CONT"
        if_cont.'setf-k'(k)
        if_cont.'setf-et'(et)
        if_cont.'setf-ef'(ef)
        if_cont.'setf-r'(r)

        .tailcall_eval(ec, r, if_cont)
.end


.namespace [ "IF-CONT" ]
.define_reader('et', 'et')
.define_writer('setf-et', 'et')
.define_reader('ef', 'ef')
.define_writer('setf-ef', 'ef')
.define_reader('r', 'r')
.define_writer('setf-r', 'r')

.sub 'resume' :method
        .param pmc v
        .local pmc e, r, k
        .nil
        r = self.'r'()
        k = self.'k'()
        eq_addr nil, v, false
true:
        e = self.'et'()
        .tailcall_eval(e, r, k)
false:
        e = self.'ef'()
        ##        .tailcall self.'evaluate*'(e, r, k)
        .tailcall_eval(e, r, k)
.end
