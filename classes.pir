.sub 'initialize-classes' :load :init
        say 'initialize-classes'
        $P0 = newclass "T"

        $P0 = subclass "T", "ENVIRONMENT"
        addattribute $P0, 'next'

        $P0 = subclass "ENVIRONMENT", "ACTIVATION-FRAME"
        addattribute $P0, 'argument'

        $P0 = subclass "T", "CONS"
        addattribute $P0, 'car'
        addattribute $P0, 'cdr'

        $P0 = subclass "T", "ATOM"

        $P0 = subclass "ATOM", "PACKAGE"
        addattribute $P0, 'name'
        addattribute $P0, 'use'
        addattribute $P0, 'external-symbols'
        addattribute $P0, 'internal-symbols'

        $P0 = subclass "ATOM", "SYMBOL"
        addattribute $P0, 'name'
        addattribute $P0, 'value'
        addattribute $P0, 'function'
        addattribute $P0, 'plist'
        addattribute $P0, 'package'

        $P0 = subclass "SYMBOL", "NULL"

        $P0 = subclass "SYMBOL", "QUOTE"

        $P0 = subclass "SYMBOL", "LAMBDA"

        $P0 = subclass "SYMBOL", "IF"

        $P0 = subclass "SYMBOL", "PROGN"

        $P0 = subclass "SYMBOL", "SETQ"
.end
