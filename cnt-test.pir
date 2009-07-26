.sub 'run-test'
        'test-quote'()
        'test-if'()
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

.sub 'test-quote'
        say "\ntest-quote"
        .local pmc quote, cons, package, k
        .nil

        package = get_global "ROOT-PACKAGE"
        quote = package.'intern'("QUOTE")
        cons = 'cons'(777, nil)
        cons = 'cons'(quote, cons)
        k = new "BOTTOM-CONTINUATION"
        $P0 = 'evaluate'(cons, nil, k)
        assert(777, $P0)
.end

.sub 'test-if'
        say "\ntest-if"
        .local pmc op_if, cons, foo, package, r, k
        .nil
        r = new "NULL-ENV"

        package = get_global "ROOT-PACKAGE"
        op_if = package.'intern'("IF")
        cons = 'cons'(222, nil)
        cons = 'cons'(111, cons)
        cons = 'cons'(777, cons)
        cons = 'cons'(op_if, cons)
        k = new "BOTTOM-CONTINUATION"
        $P0 = 'evaluate'(cons, r, k)
        assert(111, $P0)

        cons = 'cons'(222, nil)
        cons = 'cons'(111, cons)
        cons = 'cons'(nil, cons)
        cons = 'cons'(op_if, cons)
        $P0 = 'evaluate'(cons, r, k)
        assert(222, $P0)
.end
