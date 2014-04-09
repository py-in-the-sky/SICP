def is_even(n): return n % 2 == 0

def expmod(base, exp, m):
    ## almost identical to fast_expt, except for the additional "% m" at each
    ## step back up the stack (linear recursion)
    if exp == 0:        return 1
    if is_even(exp):    return expmod(base, exp/2, m)**2 % m
    else:               return (base * expmod(base, exp-1, m)) % m

def expmod_alt(base, exp, m):
    ## for a**n % n, you'll first calculate a**n and then divmod this by n
    return fast_expt(base, exp) % m

def fast_expt(base, exp):
    if exp == 0:        return 1
    if is_even(exp):    return fast_expt(base, exp/2)**2
    else:               return base * fast_expt(base, exp-1)
