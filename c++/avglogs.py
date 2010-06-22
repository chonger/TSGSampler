import sys, codecs


def main(args):
    base = args[1]

    outfile = codecs.open(base + "avg",'w','utf8')

    arr = []
    
    for i in (1,2,3,4,5):
        
        infile = codecs.open(base + str(i),'r','utf8')

        ind = 0
        for line in infile.readlines():
            if(i == 1):
                arr.append(0)
            arr[ind] += float(line) / float(5)
            ind = ind + 1
            
        infile.close()

    for a in arr:
        outfile.write(str(a) + "\n")
        
    outfile.close()

if __name__ == '__main__':
    sys.exit(main(sys.argv))



