.HLL "chocolisp"

.namespace [ "COMMON-LISP" ]

.include "parrot-macro.pir"

.sub '' :anon :load :init
.end

.sub 'ATOM'
        .param pmc x
        $I0 = isa x, ["CHOCO";"CONS"]
        if $I0 goto false
        .t
        .return(t)
false:
        .nil
        .return(nil)
.end

.sub 'CONS'
        .param pmc car
        .param pmc cdr
        .tailcall cons(car, cdr)
.end

.sub 'CAR'
        .param pmc cons
        $I0 = isa cons, ["CHOCO";"CONS"]
        if $I0 goto LCONS
        .nil
        eq_addr cons, nil, LNIL
        die "error"
LNIL:
        .return(nil)
LCONS:
        $P0 = getattribute cons, 'car'
        .return($P0)
.end

.sub 'CDR'
        .param pmc cons
        $I0 = isa cons, ["CHOCO";"CONS"]
        if $I0 goto LCONS
        .nil
        eq_addr cons, nil, LNIL
        die "error"
LNIL:
        .return(nil)
LCONS:
        $P0 = getattribute cons, 'cdr'
        .return($P0)
.end

.sub 'NULL'
        .param pmc x
        .nil
        eq_addr x, nil, true
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'FUNCALL'
        .param pmc f
        .param pmc args :slurpy
        .tailcall f(args :flat)
.end

.sub 'APPLY'
        .param pmc f
        .param pmc arg
        .param pmc rest :slurpy
        .nil
        $I0 = rest
        if $I0 goto rest_supplied
        eq_addr arg, nil, no_arg
        $I0 = isa arg, ["CHOCO";"CONS"]
        unless $I0 goto error
        $P0 = list_to_array(arg)
        .tailcall f($P0 :flat)
rest_supplied:
        .local pmc array
        array = new 'ResizablePMCArray'
        push array, arg
        .local pmc i
        i = iter rest
        $P0 = shift i
loop:
        unless i goto end
        push array, $P0
        $P0 = shift i
        goto loop
end:
        $P0 = list_to_array($P0)
        array.'append'($P0)
        .tailcall f(array :flat)
no_arg:
        .tailcall f()
error:
        die "arg is atom."
.end

.sub 'EQ'
        .param pmc x
        .param pmc y
        eq_addr x, y, true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'EQL' :multi('Number', 'Number')
        .param pmc x
        .param pmc y
        eq_num x, y, true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'EQL' :multi(_, _)
        .param pmc x
        .param pmc y
        .tailcall 'EQ'(x, y)
.end

.sub 'LENGTH' :multi('String')
        .param string x
        $I0 = length x
        .return($I0)
.end

.sub 'LENGTH' :multi(["CHOCO";"CONS"])
        .param pmc x
        .nil
        $I0 = 0
loop:
        eq_addr x, nil, end
        $I0 += 1
        x = getattribute x, 'cdr'
        goto loop
end:
        .return($I0)
.end

.sub 'SUBSEQ' :multi('String')
        .param string s
        .param int start
        .param int end :optional
        .param int has_end :opt_flag
        if has_end goto L1
        $S0 = substr s, start
        .return($S0)
L1:
        $S0 = substr s, start, end
        .return($S0)
.end


.sub '='
        .param pmc x
        .param pmc y
        eq_num x, y, true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'FIND-PACKAGE'
        .param pmc x
        $S0 = x
        $P0 = find_package($S0)
        .return($P0)
.end

.sub 'MACRO-FUNCTION'
        .param pmc symbol
        $P0 = getattribute symbol, 'macro-function'
        .return($P0)
.end

.sub 'FUNCTIONP'
        .param pmc x
        $I0 = isa x, 'Sub'
        if $I0 goto true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'STRING='
        .param string x
        .param string y
        eq_str x, y, true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'STRING<'
        .param string x
        .param string y
        lt_str x, y, true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'STRING<='
        .param string x
        .param string y
        le_str x, y, true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'TYPE-OF'
        .param pmc x
        .param pmc package
        package = find_package("COMMON-LISP")
        $I0 = isa x, ["CHOCO";"SYMBOL"]
        if $I0 goto SYMBOL
        $I0 = isa x, "String"
        if $I0 goto STRING
        $I0 = isa x, "Integer"
        if $I0 goto INTEGER
        .t
        .return(t)
SYMBOL:
        $P0 = package.'intern'("SYMBOL")
        .return($P0)
STRING:
        $P0 = package.'intern'("String")
        .return($P0)
INTEGER:
        $P0 = package.'intern'("Integer")
        .return($P0)
.end

.sub 'MAKE-SYMBOL'
        .param string name
        .local pmc symbol
        symbol = new ["CHOCO";"SYMBOL"]
        symbol = name
        .return(symbol)
.end

.sub 'SYMBOLP'
        .param pmc x
        $I0 = isa x, ["CHOCO";"SYMBOL"]
        if $I0 goto true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub 'KEYWORDP'
        .param pmc x
        $P0 = getattribute x, 'package'
        $P1 = find_package("KEYWORD")
        eq_addr $P0, $P1, true
        .nil
        .return(nil)
true:
        .t
        .return(t)
.end

.sub '+'
        .param pmc args :slurpy
        $P0 = box 0
loop:
        $I0 = args
        unless $I0 goto end
        $P1 = shift args
        $P0 += $P1
        goto loop
end:
        .return($P0)
.end

.sub '-'
        .param pmc first
        .param pmc rest :slurpy
        $I0 = rest
        if $I0 goto L1
        $P0 = neg first
        .return($P0)
L1:
        $P0 = first
        $P1 = iter rest
loop:
        unless $P1 goto end
        $P2 = shift $P1
        $P0 -= $P2
        goto loop
end:
        .return($P0)
.end

.sub 'PRINC-TO-STRING'
        .param pmc x
        $S0 = x
        .return($S0)
.end

.sub 'PRIN1-TO-STRING' :multi('String')
        .param pmc x
        x.'replace'("\"", "\\\"")
        $S0 = x
        $S0 = "\"" . $S0
        $S0 .= "\""
        .return($S0)
.end

.sub 'PRIN1-TO-STRING' :multi(_)
        .param pmc x
        $S0 = x
        .return($S0)
.end

.sub 'CADR'
        .param pmc x
        $P0 = 'CDR'(x)
        .tailcall 'CAR'($P0)
.end

.sub 'CADDR'
        .param pmc x
        $P0 = 'CDR'(x)
        $P0 = 'CDR'($P0)
        .tailcall 'CAR'($P0)
.end

.sub 'CADDDR'
        .param pmc x
        $P0 = 'CDR'(x)
        $P0 = 'CDR'($P0)
        $P0 = 'CDR'($P0)
        .tailcall 'CAR'($P0)
.end

.sub 'CAAR'
        .param pmc x
        $P0 = 'CAR'(x)
        .tailcall 'CAR'($P0)
.end

.sub 'CDDR'
        .param pmc x
        $P0 = 'CDR'(x)
        .tailcall 'CDR'($P0)
.end

.sub 'CDAR'
        .param pmc x
        $P0 = 'CAR'(x)
        .tailcall 'CDR'($P0)
.end

.sub 'PRINC'
        .param pmc x
        $P0 = 'PRINC-TO-STRING'(x)
        print $P0
        .return(x)
.end

.sub 'PRINT'
        .param pmc x
        say ""
        $P0 = 'PRIN1-TO-STRING'(x)
        print $P0
        print " "
        .return(x)
.end

##################################################################
## dummy
.sub 'MAKE-BROADCAST-STREAM'
        .param pmc args :slurpy
        $P0 = args[0]
        .return($P0)
.end

.sub 'PROCLAIM'
        .param pmc x
.end
