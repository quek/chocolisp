.namespace ["COMMON-LISP";"CHARACTER"]

.sub 'init' :anon :init  :load
        $P0 = subclass ["COMMON-LISP";"ATOM"], ["COMMON-LISP";"CHARACTER"]
        addattribute $P0, 'value'
.end

.sub get_string :vtable
        $P0 = getattribute self, 'value'
        $I0 = $P0
        $S0 = chr $I0
        $S0 = "#\\" . $S0
        .return($S0)
.end

.sub set_integer_native :vtable
        .param int n
        $P0 = box n
        setattribute self, 'value', $P0
.end

.sub get_integer :vtable
        $P0 = getattribute self, 'value'
        $I0 = $P0
        .return($I0)
.end
