=head1 関数

car
cdr
cons
quote
eq
atom
cond
defun等、関数を定義する命令

=head1 special operators

block      let*                  return-from
catch      load-time-value       setq
eval-when  locally               symbol-macrolet
flet       macrolet              tagbody
function   multiple-value-call   the
go         multiple-value-prog1  throw
if         progn                 unwind-protect
labels     progv
let        quote

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

        $P0 = new 'NULL'
        set_global "NIL", $P0

        $P0 = '%make-package'("CHOCO")
        $P1 = $P0.'%intern'("*PACKAGE*")
        $P1.'setf-symbol-value'($P0)
        set_global "*PACKAGE*", $P1

        'init-bif'()
.end

.sub '%define-classes'

        $P0 = newclass "NULL"

        $P0 = newclass "CONS"
        addattribute $P0, 'car'
        addattribute $P0, 'cdr'

        $P0 = newclass "SYMBOL"
        addattribute $P0, 'name'
        addattribute $P0, 'value'
        addattribute $P0, 'function'
        addattribute $P0, 'package'
        addattribute $P0, 'plist'

        $P0 = newclass "FUNCTION"
        addattribute $P0, 'name'
        addattribute $P0, 'args'
        addattribute $P0, 'body'

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
        .param pmc arg
        $P0 = typeof arg
        if $P0 == 'CONS' goto cons
        goto atom
cons:
        $P1 = arg.'car'()
        $P2 = arg.'cdr'()
        $P2 = '%eval-arg'($P2)
        $P3 = '%apply'($P1, $P2)
        .return($P3)
atom:
        .return(arg)
.end

.sub '%eval-arg'
        .param pmc args
        .local pmc nil
        nil = get_global "NIL"
        eq_addr args, nil, endp
        .local pmc car
        .local pmc cdr
        car = args.'car'()
        cdr = args.'cdr'()
        $P0 = '%eval'(car)
        $P1 = '%eval-arg'(cdr)
        $P2 = '%cons'($P0, $P1)
        .return($P2)
endp:
        .return(nil)
.end

.sub '%apply'
        .param pmc function
        .param pmc args
        $P0 = '%so-function'(function)
        $P0 = $P0.'body'()
        $P1 = $P0(args)
        .return($P1)
.end



.namespace [ "NULL" ]

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
.define_reader('args', 'args')
.define_writer('setf-name', 'name')
.define_writer('setf-body', 'body')
.define_writer('setf-args', 'args')


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
