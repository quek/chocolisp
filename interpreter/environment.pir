.sub init :load :init
        say "environment..."
        .nil

        $P0 = new 'ResizablePMCArray'
        set_global "sg.current", $P0

        $P0 = new 'ResizablePMCArray'
        set_global "sg.init", $P0

        $P0 = new 'Hash'
        set_global "g.current", $P0

        $P0 = new 'Hash'
        set_global "g.init", $P0

        set_global "r.init", nil

        .local pmc sr_init
        sr_init = nil
        set_global "sr.init", sr_init
        set_hll_global ["CHIMACHO"], "*env*", sr_init

        $P0 = new 'Hash'
        set_global "desc.init", $P0
.end

.sub 'g.current-extend!'
        .param pmc n
        .local pmc g_current, x
        g_current = get_global "g.current"
        .local int level
        level = g_current
        x = 'cons'("global", level)
        g_current[n] = x
        .return(level)
.end

.sub 'g.init-extend!'
        .param pmc n
        .local pmc g_init, x
        .local int level
        g_init = get_global "g.init"
        level = g_init
        x = 'cons'("predefined", level)
        g_init[n] = x
        .return(level)
.end

.sub 'global-fetch'
        .param int i
        .local pmc sg_current
        sg_current = get_global "sg.current"
        $P0 = sg_current[i]
        .return($P0)
.end

.sub 'global-update!'
        .param int i
        .param pmc v
        .local pmc sg_current
        sg_current = get_global "sg.current"
        sg_current[i] = v
.end

.sub 'predefined-fetch'
        .param int i
        .local pmc sg_init
        sg_init = get_global "sg.init"
        $P0 = sg_init[i]
        .return($P0)
.end

.sub 'g.current-initialize!'
        .param string name
        .local pmc r_init, kind
        .local int idx
        .local pmc sg_current
        sg_current = get_global "sg.current"
        r_init = get_global "r.init"
        kind = 'compute-kind'(r_init, name)
        $I0 = isnull kind
        if $I0 goto define
redifen:
        .local pmc type
        type = kind.'car'()
        if type != "global" goto error
        idx = kind.'cdr'()
        sg_current[idx] = "undefined-value"
        goto end
define:
        idx = 'g.current-extend!'(name)
        sg_current[idx] = "undefined-value"
        goto end
error:
        $P0 = new "STATIC-WRONG"
        $P0 = "Wrong redefinition "
        $P0 .= name
        throw $P0
end:
.end

.sub 'g.init-initialize!'
        .param pmc symbol
        .param pmc value
        .local pmc kind, r_init, type
        .local pmc sg_init
        sg_init = get_global "sg.init"
        r_init = get_global "r.init"
        kind = 'compute-kind'(r_init, symbol)
        $I0 = isnull kind
        if $I0 goto define
        type = kind.'car'()
        if type != "predefined" goto error
redefine:
        .local pmc idx
        idx = kind.'cdr'()
        sg_init[idx] = value
        .return(symbol)
define:
        'g.init-extend!'(symbol)
        sg_init.'push'(value)
        .return(symbol)
error:
        $P0 = new "STATIC-WRONG"
        $P0 = "Wrong redefinition "
        $S0 = symbol
        $P0 .= $S0
        throw $P0
.end

.sub 'compute-kind'
        .param pmc r
        .param pmc n
        .local pmc ret
        ret = 'local-variable?'(r, 0, n)
        $I0 = isnull ret
        if $I0 goto l_current
        .return(ret)
l_current:
        .local pmc g_current
        g_current = get_global "g.current"
        ret = 'global-variable?'(g_current, n)
        $I0 = isnull ret
        if $I0 goto l_init
        .return(ret)
l_init:
        .local pmc g_init
        g_init = get_global "g.init"
        .tailcall 'global-variable?'(g_init, n)
.end

.sub 'local-variable?'
        .param pmc r
        .param int i
        .param pmc n
        .local pmc nul
        null nul
        $I0 = 'consp'(r)
        if $I0 goto scan
        .return(nul)
scan:
        .local pmc names
        names = r.'car'()
        .local int j
        j = 0
loop:
        $I0 = 'consp'(names)
        if $I0 goto cons
        $I0 = 'null'(names)
        if $I0 goto next
        eq_addr n, names, true
        .return(nul)
cons:
        $P0 = names.'car'()
        eq_addr $P0, n, true
        names = names.'cdr'()
        j += 1
        goto loop
next:
        r = r.'cdr'()
        i += 1
        .tailcall 'local-variable?'(r, i, n)
true:
        $P0 = 'cons'(i, j)
        $P0 = 'cons'("local", $P0)
        .return($P0)
.end

.sub 'global-variable?'
        .param pmc g
        .param pmc n
        $P0 = g[n]
        .return($P0)
.end

.sub 'description-extend!'
        .param pmc symbol
        .param pmc description
        .local pmc desc_init, x
        desc_init = get_global "desc.init"
        desc_init[symbol] = description
        .return(symbol)
.end

.sub 'get-description'
        .param string name
        .local pmc desc_init, description
        desc_init = get_global "desc.init"
        description = desc_init[name]
        .return(description)
.end
