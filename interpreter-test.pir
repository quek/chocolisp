.sub '%run-test'
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

.sub 'test_read'
        print "\ntest_read"
        .nil
        .local pmc venv
        venv = get_global "r.init"

        .local pmc fh
        .local pmc sexp
        fh = '%open'("a.lisp", "r")

        sexp = '%read'(fh)
        $P0 = 'meaning'(sexp, venv)
        $P0 = $P0()
        'assert'(30, $P0)

##        sexp = '%read'(fh)
##        $P0 = '%eval'(sexp, venv, fenv)
##        'assert'("Hello", $P0)
##
##        sexp = '%read'(fh)
##        $P0 = '%eval'(sexp, venv, fenv)
##        'assert'("10", $P0)
##
##        sexp = '%read'(fh)
##        $P0 = '%eval'(sexp, venv, fenv)
##        'assert'("200", $P0)
##
##        ## Good bye!
##        sexp = '%read'(fh)
##        $P0 = '%eval'(sexp, venv, fenv)
##
        '%close'(fh)
.end
