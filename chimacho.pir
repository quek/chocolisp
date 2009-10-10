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
        $P0 = new ["COMMON-LISP";"CHARACTER"]
        $P0 = $S0
        .return($P0)
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
        $P0 = new ["COMMON-LISP";"CHARACTER"]
        $P0 = $S0
        .return($P0)
eof:
        .nil
        .return(nil)
.end

.sub '%$WRITE-STRING' :multi(_, ["COMMON-LISP";"STRING-OUTPUT-STREAM"])
        .param pmc str
        .param pmc stream
        stream .= str
        .return(str)
.end

.sub '%$WRITE-STRING' :multi(_, 'FileHandle')
        .param pmc str
        .param pmc fh
        print fh, str
        .return(str)
.end

.sub '$WRITE-STRING'
        .param pmc str
        .param pmc stream
        .tailcall '%$WRITE-STRING'(str, stream)
.end

.sub '$TERPRI'
        .param pmc fh
        '$WRITE-STRING'("\n", fh)
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
        $I0 = isa $P1, ["COMMON-LISP";"CHARACTER"]
        unless $I0 goto L1
        $I0 = $P1
        $S1 = chr $I0
        goto L2
L1:
        $S1 = $P1
L2:
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

.sub '$EXPORT'
        .param pmc symbol
        .param pmc package
        package.'export'(symbol)
        .t
        .return(t)
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

.sub '$MAKE-STRING-OUTPUT-STREAM'
        $P0 = new ["COMMON-LISP";"STRING-OUTPUT-STREAM"]
        $P0 = ""
        .return($P0)
.end

.sub '$GET-OUTPUT-STREAM-STRING'
        .param pmc x
        $S0 = x
        .return($S0)
.end

.sub 'PIR-COMPILE'
        .param string src
        .local pmc compiler
        compiler = compreg "PIR"
        $P0 = compiler(src)
        .return($P0)
.end

.sub 'PIR-EVAL'
        .param string pir
        say "PIR-EVAL <= "
        say pir
        $P0 = 'PIR-COMPILE'(pir)
        $P1 = $P0()
        print "PIR-EVAL => "
        say $P1
        .return($P1)
.end

.sub '$MAKE-HASH-TABLE'
        new $P0, 'Hash'
        .return($P0)
.end

.sub '$GETHASH'
        .param pmc hash
        .param pmc key
        $P0 = hash[key]
        if_null $P0, L1
        .return($P0)
L1:
        .nil
        .return(nil)
.end

.sub '$SETHASH'
        .param pmc hash
        .param pmc key
        .param pmc value
        hash[key] = value
        .return(hash)
.end

.sub '$HASH-TABLE-P'
        .param pmc x
        $I0 = isa x, 'Hash'
        if $I0 goto true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub '$MAPHASH'
        .param pmc f
        .param pmc hash
        $P1 = iter hash
iter_loop:
        unless $P1 goto iter_end
        $S1 = shift $P1
        $P2 = hash[$S1]
        f($S1, $P2)
        goto iter_loop
iter_end:
.end
