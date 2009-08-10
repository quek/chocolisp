.sub init :load :init
        say "primitive..."

        .package
        .local pmc nil
        nil = package.'%intern'("NULL", "NIL")
        nil.'value!'(nil)
        set_hll_global ["CHIMACHO"], "NIL", nil
        'definitial'(nil, nil)

        .local pmc t
        t = package.'%intern'("SYMBOL", "T")
        t.'value!'(t)
        set_hll_global ["CHIMACHO"], "T", t
        'definitial'(t, t)

        'init-prim'("CAR",    'car',    1)
        'init-prim'("CDR",    'cdr',    1)
        'init-prim'("CONS",   'cons',   2)
        'init-prim'("+",      '+',      2)
        'init-prim'("PRINC",  'princ',  1)
        'init-prim'("TERPRI", 'terpri', 0)
        'init-prim'("LOAD",   'load',   1)
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

.sub 'princ'
        .param pmc x
        print x
        .return(x)
.end

.sub 'terpri'
        say ""
.end

.sub 'load'
        .param pmc path
        .local pmc fh, sexp, r
        r = get_global "r.init"
        fh = '%open'(path, "r")
loop:
        sexp = '%read'(fh)
        $I0 = isnull sexp
        if $I0 goto end
        $P0 = 'meaning'(sexp, r)
        $P0 = $P0()
        goto loop
end:
        '%close'(fh)
.end
