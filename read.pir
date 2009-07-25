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
        .return($S0)
.end

.sub '%skip-whitespace'
        .param pmc fh
        $S0 = peek fh
        eq "", $S0, end
        gt $S0, " ", end
        '%read-char'(fh)
        '%skip-whitespace'(fh)
end:
.end

.sub '%read-atom'
        .param pmc fh
        .local string buffer
loop:
        $S0 = peek fh
        eq "",  $S0, loop_end
        eq "(", $S0, loop_end
        eq ")", $S0, loop_end
        ge " ", $S0, loop_end
        eq "\"", $S0, loop_string
        '%read-char'(fh)
        buffer .= $S0
        goto loop
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
        buffer = upcase buffer
        $P0 = package.'%intern'(buffer)
        .return($P0)
loop_string:
        '%read-char'(fh)
        $S0 = peek fh
        eq "\"", $S0, string_end
        buffer .= $S0
        goto loop_string
string_end:
        '%read-char'(fh)
        .return(buffer)
.end

.sub '%read-list'
        .param pmc fh
start:
        '%skip-whitespace'(fh)
        $S0 = peek fh
        eq "", $S0, error
        eq ";", $S0, comment
        eq ")", $S0, ret_nil
        eq "(", $S0, read_list
        $P0 = '%read-atom'(fh)
        $P1 = '%read-list'(fh)
        $P2 = '%cons'($P0, $P1)
        .return($P2)
comment:
        '%read-comment'(fh)
        goto start
read_list:
        '%read-char'(fh)
        $P0 = '%read-list'(fh)
        $P1 = '%read-list'(fh)
        $P2 = '%cons'($P0, $P1)
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

.sub '%read-comment'
        .param pmc fh
start:
        $S0 = '%read-char'(fh)
        eq "\n", $S0, end
        goto start
end:
.end

.sub '%read'
        .param pmc fh
start:
        '%skip-whitespace'(fh)
        $S0 = peek fh
        eq "", $S0, error
        eq "(", $S0, read_list
        eq ";", $S0, comment
        $P0 = '%read-atom'(fh)
        .return($P0)
read_list:
        '%read-char'(fh)
        $P0 = '%read-list'(fh)
        .return($P0)
comment:
        '%read-comment'(fh)
        goto start
error:
        $P0 = new "Exception"
        $P0 = "unexpected EOF!"
        throw $P0
.end
