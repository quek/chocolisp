.sub init :load :init
        say "primitive..."

        .package
        .local pmc n

        n = package.'intern'("CAR")
        .const 'Sub' car = 'car'
        'defprimitive'(n, car, 1)

        n = package.'intern'("CDR")
        .const 'Sub' cdr = 'cdr'
        'defprimitive'(n, cdr, 1)

        n = package.'intern'("CONS")
        .const 'Sub' cons = 'cons'
        'defprimitive'(n, cons, 2)
.end

.sub 'car'
        .param pmc x
        .tailcall x.'car'()
.end

.sub 'cdr'
        .param pmc x
        .tailcall x.'cdr'()
.end
