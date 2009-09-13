.HLL "chocolisp"

.include "parrot-macro.pir"
.include "package.pir"
.include "symbol.pir"

.namespace [ "CHOCO" ]

.sub main :main
        .t
        .nil
        say t
        say nil

        $P0 = get_hll_global [ "COMMON-LISP" ], "CONS"
        $P1 = $P0("car value", "cdr value")
        $P0 = get_hll_global [ "COMMON-LISP" ], "CAR"
        $P1 = $P0($P1)
        say $P1

        say "==== start compiler/a.pbc ===="
        load_bytecode "compiler/a.pbc"
        say "==== end compiler/a.pbc ===="
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
        addattribute $P0, 'dynamic-values'


        .local pmc common_lisp_package
        common_lisp_package = make_package("COMMON-LISP", "CL")

        .local pmc nil
        nil = common_lisp_package.'intern'("NIL")
        common_lisp_package.'export'(nil)
        setattribute nil, 'value', nil
        set_global "NIL", nil

        .local pmc t
        t = common_lisp_package.'intern'("T")
        common_lisp_package.'export'(t)
        setattribute t, 'value', t
        set_global "T", t

        .local pmc cl_user, use_list
        cl_user = make_package("COMMON-LISP-USER", "CL-USER")
        use_list = getattribute cl_user, 'use-list'
        push use_list, common_lisp_package
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


.include "common-lisp.pir"
