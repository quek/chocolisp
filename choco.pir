.HLL 'CHOCO'

.namespace ["CHOCO"]

.include "test.pir"


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
        '%define-classes'()
        'a_test'()

        'test_cons'()
        'test_eval'()
.end

.sub 'a_test'
        #say "Let's que!"
        #.local string my_str
        #set my_str, "kamo"
        #say my_str
        #say utf8:unicode:"まみむめも♪"
.end


.sub '%define-classes'
        $P0 = newclass 'CONS'
        addattribute $P0, 'car'
        addattribute $P0, 'cdr'

        $P0 = newclass 'SYMBOL'
        addattribute $P0, 'name'
        addattribute $P0, 'value'
        addattribute $P0, 'function'
        addattribute $P0, 'package'
        addattribute $P0, 'plist'

        $P0 = newclass 'FUNCTION'
        addattribute $P0, 'args'
        addattribute $P0, 'body'
.end


.sub 'cons'
        .param pmc car
        .param pmc cdr

        $P0 = new 'CONS'
        setattribute $P0, 'car', car
        setattribute $P0, 'cdr', cdr
        .return($P0)
.end



.sub '%%eval'
        .param pmc arg
        $P0 = typeof arg
        if $P0 == 'CONS' goto cons
        goto atom
cons:
        $P1 = arg.'car'()
        $P2 = arg.'cdr'()
        $P2 = '%%eval-arg'($P2)
        $P3 = '%%apply'($P1, $P2)
        .return($P3)
atom:
        .return(arg)
.end


.namespace [ "CONS" ]
.define_reader('car', 'car')
.define_reader('cdr', 'cdr')
.define_writer ('rplaca', 'car')
.define_writer ('rplacd', 'cdr')


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
