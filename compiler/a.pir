.HLL "chocolisp"

.sub '#:|init902|' :anon :init :load
        $P1 = new "Integer"
        $P1 = 1
        $P3 = new 'ResizablePMCArray'
        $P2 = 'FOO'($P3 :flat)
        $P4 = new "Integer"
        $P4 = 2
        $P6 = new 'ResizablePMCArray'
        $P5 = 'FOO'($P6 :flat)
        .return($P5)
.end

.sub 'FOO'
        $P2 = new 'ResizablePMCArray'
        $P3 = new "Integer"
        $P3 = 1
        push $P2, $P3
        $P1 = 'BAR'($P2 :flat)
        .return($P1)
.end

.sub 'BAR'
        .param pmc p_N
        $P2 = new 'ResizablePMCArray'
        $P3 = p_N
        push $P2, $P3
        $P1 = 'PRINT'($P2 :flat)
        .return($P1)
.end

