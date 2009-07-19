.namespace [ "CHOCO" ]

.sub '%so-function'
        .param pmc arg
        ## TODO flet labels macrolet
        $P0 = arg.'symbol-function'()
        .return($P0)
.end
