.HLL "chocolisp"

.include "parrot-macro.pir"
.include "package.pir"
.include "symbol.pir"
.include "cons.pir"
.include "chimacho.pir"

.namespace []

.sub cons
        .param pmc car
        .param pmc cdr
        $P0 = new ["CHOCO";"CONS"]
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
        package = new ["CHOCO";"PACKAGE"]
        package = "COMMON-LISP"
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

.sub dynamic_scope_value
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

.namespace [ "CHOCO" ]

.sub main :main
        .t
        .nil

        $P1 =  cons("car value", "cdr value")
        $P0 = get_hll_global [ "COMMON-LISP" ], "CAR"
        $P1 = $P0($P1)
        say $P1

        say "==== start compiler/a.pir ===="
        load_bytecode "compiler/a.pir"
        say "==== end compiler/a.pir ===="
##        say "==== start compiler/parrot-compiler.pir ===="
##        load_bytecode "compiler/parrot-compiler.pir"
##        say "==== end compiler/parrot-compiler.pir ===="
.end

.sub '' :anon :load :init
        say "Chocolisp..."

        .local pmc all_packages
        all_packages = new 'Hash'
        set_hll_global "*all-packages*", all_packages

        $P0 = newclass ["CHOCO";"T"]

        $P0 = subclass ["CHOCO";"T"], ["CHOCO";"CONS"]
        addattribute $P0, 'car'
        addattribute $P0, 'cdr'

        $P0 = subclass ["CHOCO";"T"], "ATOM"

        $P0 = subclass "ATOM", ["CHOCO";"PACKAGE"]
        addattribute $P0, 'name'
        addattribute $P0, 'nick-names'
        addattribute $P0, 'use-list'
        addattribute $P0, 'external-symbols'
        addattribute $P0, 'internal-symbols'

        $P0 = subclass "ATOM", ["CHOCO";"SYMBOL"]
        addattribute $P0, 'name'
        addattribute $P0, 'value'
        addattribute $P0, 'function'
        addattribute $P0, 'plist'
        addattribute $P0, 'package'
        addattribute $P0, 'special-var-p'


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

        .local pmc cl_user, use_list
        cl_user = make_package("COMMON-LISP-USER", "CL-USER")
        use_list = getattribute cl_user, 'use-list'
        push use_list, common_lisp_package

        .local pmc chimacho
        chimacho = make_package("CHIMACHO")
        use_list = getattribute chimacho, 'use-list'
        push use_list, common_lisp_package
.end

## TODO nickname

.include "common-lisp.pir"
