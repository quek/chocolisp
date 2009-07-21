=head1 chocolisp

=cut

.HLL 'CHOCO'

.namespace ["CHOCO"]

.include "special-op.pir"
.include "bif.pir"
.include "test.pir"
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
        'a_test'()

        '%run-test'()
.end

.sub 'a_test'
        ##say "Let's que!"
        ##.local string my_str
        ##set my_str, "kamo"
        ##say my_str
        ##say utf8:unicode:"まみむめも♪"
.end

.sub '%initialize'
        '%define-classes'()

        .local pmc package
        package = '%make-package'("CHOCO")
        $P1 = package.'%intern'("*PACKAGE*")
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

        .local pmc lambda
        lambda = package.'%intern'("LAMBDA")
        set_global "LAMBDA", lambda

        'init-bif'()
        'init-special-operator'()
.end

.sub '%define-classes'

        $P0 = newclass "CONS"
        addattribute $P0, 'car'
        addattribute $P0, 'cdr'

        $P0 = newclass "SYMBOL"
        addattribute $P0, 'name'
        addattribute $P0, 'value'
        addattribute $P0, 'function'
        addattribute $P0, 'package'
        addattribute $P0, 'plist'

        $P0 = subclass "SYMBOL", "NULL"

        $P0 = newclass "FUNCTION"
        addattribute $P0, 'name'
        addattribute $P0, 'lambda-list'
        addattribute $P0, 'body'

        $P0 = subclass "FUNCTION", "SPECIAL-OPERATOR"

        $P0 = subclass "FUNCTION", "MACRO"

        $P0 = subclass "FUNCTION", "CLOSURE"
        addattribute $P0, 'venv'
        addattribute $P0, 'fenv'

        $P0 = newclass "PACKAGE"
        addattribute $P0, 'name'
        addattribute $P0, 'nick-names'
        addattribute $P0, 'use-list'
        addattribute $P0, 'external-symbols'
        addattribute $P0, 'internal-symbols'
.end


.sub '%cons'
        .param pmc car
        .param pmc cdr

        $P0 = new 'CONS'
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


.sub '%eval'
        .param pmc exp
        .param pmc venv
        .param pmc fenv
        $P0 = typeof exp
        if $P0 == 'CONS' goto cons
        goto atom
cons:
        $P0 = exp.'car'()
        $P1 = exp.'cdr'()
        $P2 = '%eval-application'($P0, $P1, venv, fenv)
        .return($P2)
atom:
        if $P0 == "SYMBOL" goto symbol
        .return(exp)
symbol:
        $P3 = 'lookup-value'(exp, venv)
        .return($P3)
.end

.sub '%eval-application'
        .param pmc f
        .param pmc args
        .param pmc venv
        .param pmc fenv
        $I0 = isa f, "SYMBOL"
        if $I0 goto symbol
        $I0 = 'lambdap'(f)
        if $I0 == 0 goto error
        .local pmc lambda_list
        .local pmc body
        lambda_list = f.'cdr'()
        lambda_list = lambda_list.'car'()
        body = f.'cdr'()
        body = body.'cdr'()
        venv = 'extend_env'(venv, lambda_list, args)
        $P1 = 'progn'(body, venv, fenv)
        .return($P1)
error:
        $P0 = new "Exception"
        throw $P0
symbol:
        $P0 = 'lookup-function'(f, fenv)
        $I0 = isa $P0, "SPECIAL-OPERATOR"
        if $I0 goto special_operator
        $I0 = isa $P0, "MACRO"
        if $I0 goto macro
        $I0 = isa $P0, "CLOSURE"
        if $I0 goto closure
        ## function
        args = '%eval-list'(args, venv, fenv)
        $P1 = '%apply'($P0, args)
        .return($P1)
special_operator:
        $P1 = 'invoke_special_op'($P0, args, venv, fenv)
        .return($P1)
macro:
        $P1 = 'invoke_macro'($P0, args, venv, fenv)
        .return($P1)
closure:
        .local pmc d_venv
        .local pmc lambda_list
        .local pmc body
        d_venv = $P0.'venv'()
        lambda_list = $P0.'lambda-list'()
        body = $P0.'body'()
        d_venv = 'extend_env'(d_venv, lambda_list, args)
        $P1 = 'progn'(body, d_venv, fenv)
        .return($P1)
.end

.sub 'lambdap'
        .param pmc f
        .local pmc car
        .local pmc lambda
        car = f.'car'()
        .local pmc package
        lambda = get_global "LAMBDA"
        eq_addr car, lambda, true
        .return(0)
true:
        .return(1)
.end

.sub '%eval-list'
        .param pmc list
        .param pmc venv
        .param pmc fenv
        .local pmc nil
        nil = get_global "NIL"
        eq_addr list, nil, endp
        .local pmc car
        .local pmc cdr
        car = list.'car'()
        cdr = list.'cdr'()
        $P0 = '%eval'(car, venv, fenv)
        $P1 = '%eval-list'(cdr, venv, fenv)
        $P2 = '%cons'($P0, $P1)
        .return($P2)
endp:
        .return(nil)
.end

.sub '%apply'
        .param pmc function
        .param pmc args
        $P0 = function.'body'()
        $P1 = $P0(args)
        .return($P1)
.end

.sub 'invoke_special_op'
        .param pmc special_op
        .param pmc args
        .param pmc venv
        .param pmc fenv
        $P0 = special_op.'body'()
        $P1 = $P0(args, venv, fenv)
        .return($P1)
.end

.sub 'invoke_macro'
        .param pmc macro
        .param pmc args
        .param pmc venv
        .param pmc fenv
        $P0 = macro.'body'()
        $P1 = $P0(args, venv, fenv)
        .return($P1)
.end


.sub 'make-null-venv'
        $P0 = get_global "NIL"
        .return($P0)
.end

.sub 'make-null-fenv'
        $P0 = get_global "NIL"
        .return($P0)
.end

.sub 'extend_env'
        .param pmc env
        .param pmc vars
        .param pmc vals
        .local pmc nil
        nil = get_global "NIL"
loop:
        eq_addr vars, nil, end
        $P0 = vars.'car'()
        $P1 = vals.'car'()
        $P2 = '%cons'($P0, $P1)
        env = '%cons'($P2, env)
        vars = vars.'cdr'()
        vals = vals.'cdr'()
        goto loop
end:
        .return(env)
.end

.sub 'lookup-value'
        .param pmc symbol
        .param pmc env
        .local pmc nil
        nil = get_global "NIL"
        eq_addr nil, env, global
        $P0 = env.'car'()
        $P1 = $P0.'car'()
        eq_addr $P1, symbol, found
        $P0 = env.'cdr'()
        $P2 = 'lookup-value'(symbol, $P0)
        .return($P2)
found:
        $P2 = $P0.'cdr'()
        .return($P2)
global:
        $P0 = symbol.'symbol-value'()
        $I0 = isnull $P0
        if $I0 goto error
        .return($P0)
error:
        $P0 = new "Exception"
        $S0 = symbol.'symbol-name'()
        $S0 .= " is unbound!"
        $P0 = $S0
        throw $P0
.end

.sub 'lookup-function'
        .param pmc symbol
        .param pmc env
        .local pmc nil
        nil = get_global "NIL"
        eq_addr nil, env, global
        $P0 = env.'car'()
        $P1 = $P0.'car'()
        eq_addr $P1, symbol, found
        $P0 = env.'cdr'()
        $P2 = 'lookup-function'(symbol, $P0)
        .return($P2)
found:
        $P2 = $P0.'cdr'()
        .return($P2)
global:
        $P0 = symbol.'symbol-function'()
        $I0 = isnull $P0
        if $I0 goto error
        .return($P0)
error:
        $P0 = new "Exception"
        $S0 = symbol.'symbol-name'()
        $S0 .= " is unbound!"
        $P0 = $S0
        throw $P0
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

.sub '%intern' :method
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
