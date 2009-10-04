.namespace [ "COMMON-LISP";"SYMBOL" ]

.sub '' :anon :init :load
        $P0 = subclass ["COMMON-LISP";"ATOM"], ["COMMON-LISP";"SYMBOL"]
        addattribute $P0, 'name'
        addattribute $P0, 'value'
        addattribute $P0, 'function'
        addattribute $P0, 'macro-function'
        addattribute $P0, 'plist'
        addattribute $P0, 'package'
        addattribute $P0, 'special-var-p'
.end

.sub init :vtable
        .nil
        setattribute self, 'plist', nil
        setattribute self, 'special-var-p', nil
        setattribute self, 'macro-function', nil
.end

.sub get_string :vtable
        $P0 = getattribute self, 'name'
        $S0 = $P0
        .return($S0)
.end

.sub set_string_native :vtable
        .param pmc str
        setattribute self, 'name', str
.end

.sub is_equal :method :multi(["COMMON-LISP";"SYMBOL"])
        .param pmc other
        eq_addr self, other, true
        .return(0)
true:
        .return(1)
.end

.sub specialize :method
        .t
        setattribute self, 'special-var-p', t
.end
