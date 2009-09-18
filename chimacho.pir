.namespace [ "CHIMACHO" ]

.sub '' :anon :load :init
.end

.sub 'IS'
        .param pmc x
        .param pmc y
        eq x, y, true
        $S0 = "The assertion failed. ["
        $S1 = x
        $S0 .= $S1
        $S0 .= "] is not ["
        $S1 = y
        $S0 .= $S1
        $S0 .= "]."
        die $S0
true:
        .t
        .return(t)
.end
