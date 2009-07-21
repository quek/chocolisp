.namespace ["CHOCO"]

.sub '%run-test'
        'test_cons'()
        'test_eval'()
        'test_read'()
        say "\nok"
.end

.sub 'assert'
        .param pmc x
        .param pmc y
        if x == y goto end
        $S0 = "================================\n"
        $S1 = x
        $S0 .= $S1
        $S0 .= " != "
        $S1 = y
        $S0 .= $S1
        $P0 = new 'Exception'
        $P0 = $S0
        throw $P0
end:
        print "."
.end

.sub 'test_FixedPMCArray'
        $P0 = subclass 'FixedPMCArray', 'CONS'
        $P0 = new $P0
        $P0 = 2
        $P0[0] = "Hello"
        $P0[1] = "Choco"
        $P1 = $P0[0]
        say $P1
        $P2 = $P0[1]
        say $P2
.end

.sub 'test_cons'
        .local pmc my_cons
        print "\ntest_cons"
        $P0 = '%cons'(123, "abc")
        $P1 = $P0.'car'()
        'assert'(123, $P1)
        $P1 = $P0.'cdr'()
        'assert'("abc", $P1)
        $P0.'rplaca'("xyz")
        $P1 = $P0.'car'()
        'assert'("xyz", $P1)
        $P0.'rplacd'(789)
        $P1 = $P0.'cdr'()
        'assert'(789, $P1)
.end

.sub 'test_eval'
        print "\ntest_eval"
        .local pmc nil
        .local pmc venv
        .local pmc fenv
        nil = get_global "NIL"
        venv = 'make-null-venv'()
        fenv = 'make-null-fenv'()
        .local pmc package
        package = get_global "*PACKAGE*"
        package = package.'symbol-value'()


        $P0 = '%eval'(1, venv, fenv)
        'assert'(1, $P0)


        $P0 = '%eval'("abc", venv, fenv)
        'assert'("abc", $P0)


        $P0 = new 'FUNCTION'
        $P1 = get_global '%+'
        $P0.'setf-body'($P1)

        $P1 = package.'%intern'("+")

        $P0 = '%cons'(11, nil)
        $P0 = '%cons'(22, $P0)
        $P0 = '%cons'($P1, $P0)
        $P0 = '%eval'($P0, venv, fenv)
        'assert'(33, $P0)
.end

.sub 'test_read'
        print "\ntest_read"

        .local pmc nil
        .local pmc venv
        .local pmc fenv
        nil = get_global "NIL"
        venv = 'make-null-venv'()
        fenv = 'make-null-fenv'()

        .local pmc fh
        .local pmc sexp
        fh = '%open'("a.lisp", "r")

        sexp = '%read'(fh)
        $P0 = '%eval'(sexp, venv, fenv)
        'assert'(30, $P0)

        sexp = '%read'(fh)
        $P0 = '%eval'(sexp, venv, fenv)
        'assert'("Hello", $P0)

        sexp = '%read'(fh)
        $P0 = '%eval'(sexp, venv, fenv)
        'assert'("10", $P0)

        sexp = '%read'(fh)
        $P0 = '%eval'(sexp, venv, fenv)
        'assert'("200", $P0)

        ## Good bye!
        sexp = '%read'(fh)
        $P0 = '%eval'(sexp, venv, fenv)

        '%close'(fh)
.end
