.sub '%run-test'
        'test_load'()
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

.sub 'test_load'
        print "\ntest_load"
        .nil
        .package
        .local pmc r, e, m, load
        e = 'cons'("a.lisp", nil)
        load = package.'intern'("LOAD")
        e = 'cons'(load, e)
        m = 'meaning'(e, r)
        m()
.end
