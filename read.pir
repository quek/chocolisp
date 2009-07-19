.namespace ["CHOCO"]

.sub '%open'
        .param string file
        .param string mode
        $P0 = open file, mode
        .return($P0)
.end

.sub '%close'
        .param pmc fh
        close fh
.end

.sub '%read-char'
        .param pmc fh
        $S0 = read fh, 1
        $I0 = length $S0
        if $I0 goto end
        $P0 = null
end:
        .return($P0)
.end

.sub '%skip-whitespace'
        .param pmc fh
        $S0 = peek fh
        eq_str "", $S0, end
        le_str " ", $S0, end
        '%skip-whitespace'()
end:
.end

.sub '%read-atom'
        .param pmc fh
        .local string buffer
loop:
        $S0 = '%read-char'(fh)
        $I0 = isnull $S0
        if $I0 goto loop_end
        buffer .= $S0
loop_end:
        $S1 = substr buffer, 0, 1
        lt $S1, "0", symbol
        gt $S1, "9", symbol
        $I0 = buffer
        .return($I0)
symbol:
        .local pmc _package_
        .local pmc package
        _package_ = get_global "*PACKAGE*"
        package = _package_.'symbol-value'()
        $P0 = package.'%intern'(buffer)
        .return($P0)
.end

.sub '%read-list'
        .param pmc fh
        '%skip-whitespace'()
        $S0 = peek fh
        eq "", $S0, error
        eq ")", $S0, ret_nil
        eq "(", $S0, read_list
        $P0 = '%read-atom'(fh)
        $P1 = '%read-list'(fh)
        $P2 = 'cons'($P0, $P1)
        .return($P2)
read_list:
        '%read-char'(fh)
        $P0 = '%read-list'(fh)
        $P1 = '%read-list'(fh)
        $P2 = 'cons'($P0, $P1)
        .return($P2)
ret_nil:
        '%read-char'(fh)
        .local pmc nil
        nil = get_global "NIL"
        .return(nil)
error:
        $P0 = new 'Exception'
        throw $P0
.end
