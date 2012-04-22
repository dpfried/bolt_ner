import sys

def print_struc(struc):
    if type(struc) is list:
        print '(',
        for elem in struc:
            print_struc(elem)
        print ')',
    elif type(struc) is dict:
        print '(',
        for key,elem in struc.iteritems():
            print '(',
            print_struc(key)
            print '.',
            print_struc(elem)
            print ')',
        print ')',
    else:
        print struc,

struc = eval(sys.stdin.read())
print_struc(struc)
