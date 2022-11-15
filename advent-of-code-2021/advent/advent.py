# def iterate(fun, arg, n):
#     while n > 0:
#         arg = fun(arg)
#         n -= 1

#     return arg
def iterate(fun, arg, n):
    for i in range(n):
        arg = fun(arg)

    return arg

        
