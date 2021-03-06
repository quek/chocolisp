.HLL "chocolisp"

.include "parrot-macro.pir"
.include "classes.pir"
.include "cons.pir"
.include "package.pir"
.include "symbol.pir"
.include "character.pir"
.include "chimacho.pir"

.namespace []

.sub cons
        .param pmc car
        .param pmc cdr
        $P0 = new ["COMMON-LISP";"CONS"]
        setattribute $P0, 'car', car
        setattribute $P0, 'cdr', cdr
        .return($P0)
.end

.sub array_to_list
        .param pmc array
        .nil
        $P0 = nil
loop:
        $I0 = array
        eq_num 0, $I0, end
        $P1 = pop array
        $P0 = cons($P1, $P0)
        goto loop
end:
        .return($P0)
.end

.sub list_to_array
        .param pmc list
        .nil
        .local pmc array, car
        array = new 'ResizablePMCArray'
loop:
        eq_addr list, nil, end
        car = getattribute list, 'car'
        push array, car
        list = getattribute list, 'cdr'
        goto loop
end:
        .return(array)
.end

.sub getf
        .param pmc plist
        .param pmc indicator
        .local pmc result
        .local pmc itr
        itr = iter plist
loop:
        unless itr goto end
        $P1 = shift itr
        $P2 = shift itr
        eq_addr $P1, indicator, found
        goto loop
found:
        result = $P2
end:
        .return(result)
.end

## TODO nickname
.sub find_package
        .param pmc name
        .local pmc all_packages
        all_packages = get_hll_global "*all-packages*"
        $P0 = all_packages[name]
        .return($P0)
.end

.sub make_package
        .param pmc name
        .param pmc nicknames :slurpy
        .local pmc package, nicknames
        package = new ["COMMON-LISP";"PACKAGE"]
        setattribute package, 'name', name
        $P0 = getattribute package, 'nick-names'
        $P1 = iter nicknames
loop:
        unless $P1 goto end
        $P2 = shift $P1
        push $P0, $P2
        goto loop
end:
        .local pmc all_packages
        all_packages = get_hll_global "*all-packages*"
        all_packages[name] = package
        .return(package)
.end

.sub get_dynamic_scope_value
        .param string var
        .param pmc package
        .param pmc symol_name
        $P0 = find_dynamic_lex var
        unless_null $P0, end
        $P0 = find_package(package)
        $P0 = $P0.'intern'(symol_name)
        $P0 = getattribute $P0, 'value'
end:
        .return($P0)
.end

.sub set_dynamic_scope_value
        .param string var
        .param pmc package_name
        .param pmc symol_name
        .param pmc value
        .local pmc package, symbol
        $P0 = find_dynamic_lex var
        if_null $P0, NOTFOUND
        store_dynamic_lex var, value
        .return(value)
NOTFOUND:
        package = find_package(package_name)
        symbol = package.'intern'(symol_name)
        setattribute symbol, 'value', value
        .return(value)
.end

.namespace [ "CHOCO" ]

.sub main :main
        .t
        .nil

        $P1 =  cons("car value", "cdr value")
        $P0 = get_hll_global [ "COMMON-LISP" ], "CAR"
        $P1 = $P0($P1)
        say $P1

        ##say "load compiler/a.pir"
        ##load_bytecode "compiler/a.pir"

        say "load compiler/ch-compiler.pir"
        load_bytecode "compiler/ch-compiler.pir"
        say "load compiler/back-quote.pir"
        load_bytecode "compiler/back-quote.pir"
        say "load compiler/read.pir"
        load_bytecode "compiler/read.pir"
        say "load compiler/compiler.pir"
        load_bytecode "compiler/compiler.pir"

        $P0 = get_hll_global ["CHIMACHO"], "PARROT-COMPILE-FILE"
        say "compiling cl0.lisp"
        $P0("cl0/cl0.lisp")
        load_bytecode "cl0/cl0.pir"
        say "compiling io.lisp"
        $P0("cl0/io.lisp")
        load_bytecode "cl0/io.pir"
        say "compiling repl.lisp"
        $P0("cl0/repl.lisp")
        load_bytecode "cl0/repl.pir"
        say "compiling a.lisp"
        $P0("/home/ancient/letter/parrot/chocolisp/compiler/a.lisp")
        load_bytecode "compiler/a.pir"
##
##        $P0 = get_hll_global ["CHIMACHO"], "REPL"
##        $P0()
.end

.sub '' :anon :load :init
        say "Chocolisp..."

        .local pmc all_packages
        all_packages = new 'Hash'
        set_hll_global "*all-packages*", all_packages


        .local pmc common_lisp_package
        common_lisp_package = make_package("COMMON-LISP", "CL")

        .local pmc nil
        nil = common_lisp_package.'intern'("NIL")
        common_lisp_package.'export'(nil)
        setattribute nil, 'value', nil
        set_hll_global "NIL", nil

        .local pmc t
        t = common_lisp_package.'intern'("T")
        common_lisp_package.'export'(t)
        setattribute t, 'value', t
        set_hll_global "T", t

        common_lisp_package.'intern-and-export'("IN-PACKAGE")

        .local pmc cl_user, use_list
        cl_user = make_package("COMMON-LISP-USER", "CL-USER")
        use_list = getattribute cl_user, 'use-list'
        push use_list, common_lisp_package

        $P0 = subclass ["COMMON-LISP";"PACKAGE"], ["COMMON-LISP";"KEYWORD-PACKAGE"]
        .local pmc keyword_package
        keyword_package = new ["COMMON-LISP";"KEYWORD-PACKAGE"]
        $P1 = box "KEYWORD"
        setattribute keyword_package, 'name', $P1
        all_packages["KEYWORD"] = keyword_package

        .local pmc chimacho
        chimacho = make_package("CHIMACHO")
        use_list = getattribute chimacho, 'use-list'
        push use_list, common_lisp_package
.end

.include "cl-symbols.pir"

.include "common-lisp.pir"
