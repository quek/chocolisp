.sub 'init' :anon :init :load
        $P0 = newclass ["COMMON-LISP";"T"]

        $P0 = subclass ["COMMON-LISP";"T"], ["COMMON-LISP";"CONS"]
        addattribute $P0, 'car'
        addattribute $P0, 'cdr'

        $P0 = subclass ["COMMON-LISP";"T"], ["COMMON-LISP";"ATOM"]
.end
