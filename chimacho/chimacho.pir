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
        .tailcall '$OPEN'(path, "r")
.end

.sub 'OPEN-OUTPUT-FILE'
        .param string path
        .tailcall '$OPEN'(path, "w")
.end

.sub '$OPEN'
        .param string path
        .param string mode
        $P0 = open path, mode
        .return($P0)
.end

.sub '$CLOSE'
        .param pmc fh
        close fh
.end

.sub '$READ-CHAR'
        .param pmc fh
        $S0 = read fh, 1
        eq_str $S0, "", eof
        .return($S0)
eof:
        .nil
        .return(nil)
.end

.sub '$READ-LINE'
        .param pmc fh
        $S0 = readline fh
        eq_str $S0, "", eof
        .return($S0)
eof:
        .nil
        .return(nil)
.end

.sub '$PEEK-CHAR'
        .param pmc fh
        $S0 = peek fh
        eq_str $S0, "", eof
        .return($S0)
eof:
        .nil
        .return(nil)
.end

.sub '$WRITE-STRING'
        .param pmc str
        .param pmc fh
        print fh, str
        .return(str)
.end

.sub '$TERPRI'
        .param pmc fh
        print fh, "\n"
        .nil
        .return(nil)
.end

.sub 'STRING+'
        .param pmc args :slurpy
        $S0 = ""
        $P0 = iter args
loop:
        unless $P0 goto end
        $P1 = shift $P0
        $S1 = $P1
        $S0 .= $S1
        goto loop
end:
        .return($S0)
.end

.sub 'STRING-TO-NUMBER'
        .param string x
        $I0 = x
        .return($I0)
.end

.sub '$CHAR'
        .param string str
        .param int idx
        $I0 = idx + 1
        $S0 = substr str, idx, $I0
        .return($S0)
.end

.sub '$CHAR-CODE'
        .param string str
        $I0 = ord str
        .return($I0)
.end

.sub '$ERROR'
        .param string x
        die x
.end

.sub '$FIND-EXPORT-SYMBOL'
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

.sub '$INTERN'
        .param string name
        .param pmc package
        $S0 = upcase name
        $P0 = package.'intern'($S0)
        .return($P0)
.end

.sub '$SET-ATTRIBUTE'
        .param pmc object
        .param string attribute
        .param pmc value
        setattribute object, attribute, value
        .return(value)
.end

.sub '$GET-ATTRIBUTE'
        .param pmc object
        .param string attribute
        $P0 = getattribute object, attribute
        .return($P0)
.end
