=head1 special operators

block      let*                  return-from
catch      load-time-value       setq
eval-when  locally               symbol-macrolet
flet       macrolet              tagbody
function   multiple-value-call   the
go         multiple-value-prog1  throw
if         progn                 unwind-protect
labels     progv
let        quote

lambda は違うんだ。
=cut

.namespace [ "CHOCO" ]

.sub 'init-special-operator'
        .local pmc package
        package = get_global "*PACKAGE*"
        package = package.'symbol-value'()

        '%init-special-operator'(package, "QUOTE",     'quote')
        '%init-special-operator'(package, "PROGN",     'progn')
        '%init-special-operator'(package, "LAMBDA",    'lambda')
.end

.sub '%init-special-operator'
        .param pmc package
        .param string symbol_name
        .param string op_name
        .local pmc symbol
        .local pmc op
        .local pmc op_sub
        symbol = package.'%intern'(symbol_name)
        op = new "SPECIAL-OPERATOR"
        op.'setf-name'(symbol_name)
        op_sub = get_global op_name
        op.'setf-body'(op_sub)
        symbol.'setf-symbol-function'(op)
.end

.sub 'quote'
        .param pmc arg
        .param pmc venv
        .param pmc fenv
        $P0 = arg.'car'()
        .return($P0)
.end

.sub 'progn'
        .param pmc arg
        .param pmc venv
        .param pmc fenv
        .local pmc nil
        nil = get_global "NIL"
        eq_addr arg, nil, no_body
        .local pmc car
        .local pmc cdr
        car = arg.'car'()
        cdr = arg.'cdr'()
        $P0 = '%eval'(car, venv, fenv)
        eq_addr cdr, nil, last_exp
        $P0 = 'progn'(cdr, venv, fenv)
        .return($P0)
last_exp:
        .return($P0)
no_body:
        .return(nil)
.end

## lambda はスペシャルオペレータじゃないんだけど。。。
.sub 'lambda'
        .param pmc arg
        .param pmc venv
        .param pmc fenv
        .local pmc lambda
        .local pmc lambda_list
        .local pmc body
        lambda = new "CLOSURE"
        lambda.'setf-name'("LAMBDA")
        lambda_list = arg.'car'()
        lambda.'setf-lambda-list'(lambda_list)
        body = arg.'cdr'()
        lambda.'setf-body'(body)
        lambda.'setf-venv'(venv)
        lambda.'setf-fenv'(fenv)
        .return(lambda)
.end


##.sub 'function'
##        .param pmc arg
##        ## TODO flet labels macrolet
##        $P0 = arg.'car'()
##        $P0 = $P0.'symbol-function'()
##        .return($P0)
##.end
