# See PostOrderIter.fs or PostOrderIter.scala for an explanation

class Tree:
    def __init__(self, l, v, r):
        self.l = l
        self.v = v
        self.r = r

    def __str__(self):
        return str(t.l) + ' ' + str(t.v) + ' ' + str(t.r)

def singleton(v):
    return Tree(None, v, None)

def contPostOrder(f, t, k):
    if t is None:
        return k()
    def kRight():
        def kFinished():
            f(t.v)
            k()
        contPostOrder(f, t.r, kFinished)
    contPostOrder(f, t.l, kRight)
    
if __name__ == '__main__':
    t = Tree(Tree(singleton(4), 2, singleton(5)), 1, Tree(singleton(6), 3, singleton(7)))
    def knull():
        return
    def printit(x):
        print x,
    contPostOrder(printit, t, knull)
