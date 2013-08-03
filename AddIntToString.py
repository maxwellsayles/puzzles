def addCont(s, i): 
    def last(carry):
        if carry != 0:
            return str(carry)
        else:
            return ""
    def step(k, x):
        def f(carry):
            y      = int(x) + carry % 10
            carry2 = carry / 10 + y / 10
            x2     = str(y % 10) 
            return k(carry2) + x2
        return f
    return reduce(step, s, last)(i)

print addCont("234172387421873412341", 1234)
print addCont("99", 10000)
