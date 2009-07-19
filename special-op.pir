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

.sub '%so-function'
        .param pmc arg
        ## TODO flet labels macrolet
        $P0 = arg.'symbol-function'()
        .return($P0)
.end
