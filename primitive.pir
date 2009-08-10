.sub init :load :init
        say "primitive..."
        .nil

        'init-prim'("CAR",  'car',  1)
        'init-prim'("CDR",  'cdr',  1)
        'init-prim'("CONS", 'cons', 2)
        'init-prim'("+",    '+',    2)
.end

.sub 'init-prim'
        .param string name
        .param string sub_name
        .param int arity
        .local pmc symbol, _sub
        .package
        symbol = package.'intern'(name)
        _sub = get_global sub_name
        'defprimitive'(symbol, _sub, arity)
.end

.sub 'car'
        .param pmc x
        .tailcall x.'car'()
.end

.sub 'cdr'
        .param pmc x
        .tailcall x.'cdr'()
.end

.sub '+'
        .param pmc x
        .param pmc y
        $P0 = x + y
        .return($P0)
.end
