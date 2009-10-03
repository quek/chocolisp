.sub 'init' :anon :init :load
        $P0 = newclass ["COMMON-LISP";"T"]

        $P0 = subclass ["COMMON-LISP";"T"], ["COMMON-LISP";"CONS"]
        addattribute $P0, 'car'
        addattribute $P0, 'cdr'

        $P0 = subclass ["COMMON-LISP";"T"], ["COMMON-LISP";"ATOM"]

        $P0 = subclass ["COMMON-LISP";"ATOM"], ["COMMON-LISP";"STREAM"]

        $P0 = subclass "String", ["COMMON-LISP";"STRING-OUTPUT-STREAM"]
        $P1 = get_class ["COMMON-LISP";"STREAM"]
        addparent $P0, $P1
.end
