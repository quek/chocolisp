.namespace [ "CHIMACHO" ]

.include "parrot-macro.pir"

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

.sub 'OPEN-INPUT-FILE'
        .param string path
        .tailcall '%OPEN'(path, "r")
.end

.sub 'OPEN-OUTPUT-FILE'
        .param string path
        .tailcall '%OPEN'(path, "w")
.end

.sub '%OPEN'
        .param string path
        .param string mode
        $P0 = open path, mode
        .return($P0)
.end

.sub 'CLOSE'
        .param pmc fh
        close fh
.end

.sub 'READ-CHAR'
        .param pmc fh
        $S0 = read fh, 1
        $I0 = $S0
        if $I0 goto ok
        .nil
        .return(nil)
ok:
        .return($S0)
.end

.sub 'READ-LINE'
        .param pmc fh
        $S0 = readline fh
        $I0 = $S0
        if $I0 goto ok
        .nil
        .return(nil)
ok:
        .return($S0)
.end

.sub 'PEEK-CHAR'
        .param pmc fh
        $S0 = peek $P0
        $I0 = $S0
        if $I0 goto ok
        .nil
        .return(nil)
ok:
        .return($S0)
.end

.sub 'STRING+'
        .param string x
        .param string y
        $S0 = x
        $S1 = y
        $S2 = x . y
        .return($S2)
.end

.sub 'STRING-TO-NUMBER'
        .param string x
        $I0 = x
        .return($I0)
.end

.sub 'ERROR'
        .param string x
        die x
.end

.sub 'FIND-EXPORT-SYMBOL'
        .param string package
        .param string name
        $P0 = find_package(package)
        $P1 = $P0.'find-external-symbol'(name)
        if_null $P1, error
        .return($P1)
error:
        $S0 = name . " is not found in "
        $S0 .= package
        die $S0
.end
