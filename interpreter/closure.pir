.namespace [ "CLOSURE" ]

.sub 'invoke' :method
        .param pmc vs
        .local pmc code, env
        code = getattribute self, 'code'
        env = getattribute self, 'closed-environment'
        .tailcall code(vs, env)
        ## :flat が使える？
        .tailcall code(env, vs :flat)
.end
