infile = r"C:\Users\Public\Documents\Amiga Files\WinUAE\maze_dots.bin"

d = {0:" ",1:'U',2:'T',3:'F'}

with open(infile,"rb") as f:
    contents = f.read()
    idx = 0
    for j in range(0,27):
        for i in range(0,26):
            print(d[contents[idx]],end="")
            idx+=1
        print()
