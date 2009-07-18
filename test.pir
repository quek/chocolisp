.namespace ["CHOCO"]

.sub '%run-test'
        'cons_test'()
.end

.sub 'assert'
        .param pmc x
        .param pmc y
        if x == y goto end
        say "================================"
        print x
        print " != "
        print y
        say ""
        $P0 = new 'Exception'
        throw $P0
end:
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
        say 'test_cons'
        $P0 = 'cons'(123, "abc")
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
        say 'test_eval'
        $P0 = '%%eval'(1)
        'assert'(1, $P0)
        $P0 = '%%eval'("abc")
        'assert'("abc", $P0)
.end
