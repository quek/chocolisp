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
        '%init-special-operator'(package, "FUNCTION",  'function')
.end

.sub '%init-special-operator'
        .param pmc package
        .param string symbol_name
        .param string op_name
        .local pmc symbol
        .local pmc op
        symbol = package.'%intern'(symbol_name)
        op = get_global op_name
        op = new "SPECIAL-OPERATOR"
        op.'setf-name'(symbol_name)
        op.'setf-body'(op)
        symbol.'setf-symbol-function'(op)
.end

.sub 'quote'
        .param pmc arg
        $P0 = arg.'car'()
        .return($P0)
.end

.sub 'function'
        .param pmc arg
        ## TODO flet labels macrolet
        $P0 = arg.'car'()
        $P0 = $P0.'symbol-function'()
        .return($P0)
.end
