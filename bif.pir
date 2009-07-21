=head1 組み込み関数

car
cdr
cons
quote(sp)
eq
atom
cond(sp, if)
defun等、関数を定義する命令

=cut

.namespace ["CHOCO"]

.sub 'init-bif'
        .local pmc package
        package = get_global "*PACKAGE*"
        package = package.'symbol-value'()

        '%init-bif'(package, "CONS",     'cons')
        '%init-bif'(package, "CAR",      'car')
        '%init-bif'(package, "CDR",      'cdr')
        '%init-bif'(package, "RPLACA",   'rplaca')
        '%init-bif'(package, "RPLACD",   'rplacd')
        '%init-bif'(package, "+",        '+')
        '%init-bif'(package, "-",        '-')
        '%init-bif'(package, "PRINT",    'print')
.end

.sub '%init-bif'
        .param pmc package
        .param string symbol_name
        .param string sub_name
        .local pmc symbol
        .local pmc sub
        .local pmc function
        symbol = package.'%intern'(symbol_name)
        sub = get_global sub_name
        function = new "FUNCTION"
        function.'setf-name'(symbol_name)
        function.'setf-body'(sub)
        symbol.'setf-symbol-function'(function)
.end

.sub 'cons'
        .param pmc arg
        .local pmc x
        .local pmc y

        x = arg.'car'()
        y = arg.'cdr'()
        y = y.'car'()
        $P0 = '%cons'(x, y)
        .return($P0)
.end

.sub 'car'
        .param pmc arg
        .local pmc x
        x = arg.'car'()
        x = x.'car'()
        .return(x)
.end

.sub 'cdr'
        .param pmc arg
        .local pmc x
        x = arg.'car'()
        x = x.'cdr'()
        .return(x)
.end

.sub 'rplaca'
        .param pmc arg
        .local pmc cons
        .local pmc value
        cons = arg.'car'()
        value = arg.'cdr'()
        value = value.'car'()
        cons.'rplaca'(value)
        .return(cons)
.end

.sub 'rplacd'
        .param pmc arg
        .local pmc cons
        .local pmc value
        cons = arg.'car'()
        value = arg.'cdr'()
        value = value.'car'()
        cons.'rplacd'(value)
        .return(cons)
.end

.sub '+'
        .param pmc arg
        $P0 = arg.'car'()
        $P1 = arg.'cdr'()
        $P2 = $P1.'car'()
        $P3 = $P0 + $P2
        .return($P3)
.end

.sub '-'
        .param pmc arg
        $P0 = arg.'car'()
        $P1 = arg.'cdr'()
        $P2 = $P1.'car'()
        $P3 = $P0 - $P2
        .return($P3)
.end

.sub 'print'
        .param pmc arg
        $P0 = arg.'car'()
        say $P0
        .return($P0)
.end
