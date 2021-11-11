import os,bitplanelib,json
from PIL import Image

sprites_dir = "sprites"

#for level 2 (paint)
square_scores = [x//10 for x in [500,350,300,250,280,150,100,450,400,450,280,230,200,300,
700,450,280,230,200,150,100,450,400,330,250,280,230,150,300,
500,300,330,250,230,180,150,100]]

def process_alt_maze(name,nb_rows):
    img = Image.open("{}_maze.png".format(name))

    # now the big time saver: detect maze walls & dots from MAME screenshots

    rect_id = 0

    NB_H_TILES = 26
    dot_matrix = [[0]*NB_H_TILES for _ in range(nb_rows)]
    rows = []

    for i,x in enumerate(range(8,208,40)):
        row = []
        for j,y in enumerate(range(4,nb_rows*8,8)):
            ii = i*5
            dot_matrix[j][ii] = 1
            dot_matrix[j][-1] = 1
            # if not black then there's an horizontal segment here
            p = img.getpixel((x+4,y+3))  # this image is palletized: contains black (0) and green (1)
            if p:
                dot_matrix[j][ii:ii+6] = [1]*6
                row.append(j)
        rows.append(row)





    with open("../src/intro_maze_data.s","w")as fw:

        fw.write("maze_{}_vertical_table:\n".format(name))
        for row in rows:
            fw.write("\tdc.b\t")
            fw.write(",".join(str(x) for x in row))
            fw.write(",-1\n")
        fw.write("\tdc.b\t-2\n\teven\n\n")



        fw.write("\nmaze_{}_wall_table:\n".format(name))
        # binary table with all walkable tiles set to 1
        for row in dot_matrix:
            fw.write("\tdc.b\t")
            fw.write(",".join(str(x) for x in row))
            fw.write("\n")

def process_intro_maze():
    process_alt_maze("intro",14)


def process_maze():
    maze = Image.open("level1.png")

    # now the big time saver: detect maze walls & dots from MAME screenshots
    img = Image.new("RGB",(maze.size[0],maze.size[1]-24-16))
    img.paste(maze,(0,-24))
    rows = []
    rect_id = 0

    current_rect = None
    for i,x in enumerate(range(8,208,40)):
        row = []
        for j,y in enumerate(range(4,210,8)):
            # if not black then there's an horizontal segment here
            p = img.getpixel((x+4,y+3))
            if p != (0,0,0):
                if current_rect:
                    # complete current rect
                    current_rect["height"] = j-current_rect["row"]

                current_rect = {"x":x-8,"y":y-4,"row":j,"id":rect_id}
                rect_id += 1
                row.append(current_rect)
        current_rect["height"] = 26-current_rect["row"]
        current_rect = None
        rows.append(row)

    NB_H_TILES = 26

    last_row = [1]*NB_H_TILES
    last_row[10:16] = [0]*6
    dot_matrix = [[0]*NB_H_TILES for _ in range(26)]
    for row in dot_matrix:
        for i in range(0,NB_H_TILES,5):
            row[i] = 1
        row[-1]=1


    # create wall table
    dot_matrix.append(last_row)

    for i,row in enumerate(rows):
        for r in row:
            dr = dot_matrix[r["row"]]
            for j in range(6):
                dr[j+i*5] = 1

    i=1
    with open("../src/maze_data.s","w")as fw:
        fw.write("""    STRUCTURE   SRectangle,0
    UWORD   xrect
    UWORD   yrect
    UWORD   hrect
    UWORD   mdots
    UWORD   cdots
    UWORD   points
    UWORD   specrect
    LABEL   SRectangle_SIZEOF

""")
        fw.write("maze_{}_vertical_table:\n".format(i))
        for row in rows:
            fw.write("\tdc.b\t")
            fw.write(",".join(str(x["row"]) for x in row))
            fw.write(",-1\n")
        fw.write("\tdc.b\t-2\n\teven\n\n")

        rectdict = {}
        for ri,row in enumerate(rows):
            for rj,rect in enumerate(row):
                rect["name"] = "rect_{}_{:02}".format(i,rect["id"])
                nb_dots = 2*(5+rect["height"])

                special_rectangle = (ri == 0 or ri == 4) and (rj == 0 or rj == len(row)-1)

                if rj==len(row)-1:
                    if ri==2:
                        nb_dots -= 6    # start point
                    elif ri==1 or ri==3:
                        nb_dots -= 1
                rectdict[rect["name"]] = rect
                fw.write("{}:\n".format(rect["name"]))
                fw.write("\tdc.w\t{} ; x\n".format(rect["x"]))
                fw.write("\tdc.w\t{} ; y\n".format(rect["y"]))
                fw.write("\tdc.w\t{} ; height\n".format(rect["height"]))
                fw.write("\tdc.w\t{} ; max nb dots\n".format(nb_dots))
                fw.write("\tdc.w\t0 ; current nb dots\n")
                fw.write("\tdc.w\t0 ; points\n")
                fw.write("\tdc.w\t{} ; special rectangle\n".format(int(special_rectangle)))

        fw.write("\nrectlist_{}:\n".format(i))
        for rn in sorted(rectdict):
            fw.write("\tdc.l\t{}\n".format(rn))
        fw.write("\tdc.l\t0\n")


        dot_rect_matrix = [[0]*4*NB_H_TILES for _ in range(27)]
        # for each rectangle, compute coordinates and add the rectangle as a reference (or not)
        def mark(i,j,m):
            row = dot_rect_matrix[j]
            fouri = 4*i
            name = m["name"]
            for l in range(fouri,fouri+4):
                if row[l]==name:
                    break
                if not row[l]:
                    row[l] = m["name"]
                    break

        for row in rows:
            for rect in row:
                # horizontal
                xstart = rect["x"]//8
                ystart = rect["y"]//8
                h = rect["height"]
                for xt in range(6):
                    mark(xt+xstart,ystart,rect)
                    mark(xt+xstart,ystart+h,rect)
                for yt in range(h):
                    mark(xstart+5,ystart+yt,rect)
                    mark(xstart,ystart+yt,rect)

        fw.write("maze_{}_dot_table_read_only:\n".format(i))
        dot_rect_matrix[-1][40:64] = [0]*24
        for row in dot_rect_matrix:
            fw.write("\tdc.l\t")
            fw.write(",".join("{}".format(str(x)) for x in row))
            fw.write("\n")


        fw.write("\nmaze_{}_wall_table:\n".format(i))
        # binary table with all walkable tiles set to 1
        dot_matrix[-1] = [1]*26
        for row in dot_matrix:
            fw.write("\tdc.b\t")
            fw.write(",".join(str(x) for x in row))
            fw.write("\n")

##            def torgb4(t):
##                return ((t[0]&0xF0)<<4)+((t[1]&0xF0))+(t[2]>>4)
##            fw.write("\n\teven\nmaze_{}_misc:\n".format(i))
##            fw.write("\tdc.w\t${:x}  ; dots\n".format(torgb4(dot_color)))
##            fw.write("\tdc.w\t${:x}  ; outline\n".format(torgb4(outline_color)))
##            fw.write("\tdc.w\t${:x}  ; fill\n".format(torgb4(fill_color)))
##            fw.write("\tdc.w\t{}    ; total nb dots\n\n".format(nb_dots[i-1]))
##
##
##            # now dump each maze with its own palette
##            maze_palette = [(0,0,0),(0,0,0),outline_color,fill_color]  # black, dot color, maze colors



        # pixel 1,5 holds the fill color of the maze wall
        #maze_img.save("dumps/maze_{}.png".format(i))
        #maze_palette = bitplanelib.palette_extract(maze,0xF0)
        #


# palette order matters
# some key colors are located at a 2**n position
# or even value so they can be drawn without having to change the first plane (dot)
# the 2 first planes are somehow good to be left alone most of the time cos it avoids the need
# to redraw the dots.

game_palette_level_1 = """
	dc.w	$0000     ; black
    dc.w    $E00      ; red maze
    dc.w    $CC9      ; beige (dots)
    dc.w    $CC9      ; beige (dots+maze)

	dc.w	$0ff0    ; yellow (pos 4)
    dc.w    $000f   ; blue
    dc.w    $00ff       ; cyan
    dc.w    $0c0f       ; magenta1
    dc.w    $00f0   ; green1    (pos 8)

    dc.w    $0f7f       ; magenta2
    dc.w    $0f91       ; brown
    dc.w    $0fff   ; white
    dc.w    $0f00       ; red   (pos 12)

    dc.w   $0E5D,$EED,$0E84
"""

game_palette_level_1_sprites = """
    ; sprites
    dc.w    $000,$0F0,$F91,$FF0     ; guard
    dc.w    $000,$0F0,$F91,$FF0     ; guard
    dc.w    $000,$0F0,$F91,$FF0     ; guard
    dc.w    $000,$00ff,$0fff,$0f00  ; master guard

"""

game_palette_txt = game_palette_level_1+game_palette_level_1_sprites
game_palette = bitplanelib.palette_dcw2palette(game_palette_txt)
bitplanelib.palette_dump(game_palette,r"../src/palette.s",as_copperlist=False)
game_palette_16 = game_palette[0:16]

outdir = "dumps"

def process_tiles():
    json_file = "tiles.json"
    with open(json_file) as f:
        tiles = json.load(f)

    default_width = tiles["width"]
    default_height = tiles["height"]
    default_horizontal = tiles["horizontal"]

    x_offset = tiles["x_offset"]
    y_offset = tiles["y_offset"]

    sprite_page = tiles["source"]

    sprites = Image.open(sprite_page)

    name_dict = {"scores_{}".format(i):"scores_"+n for i,n in enumerate(["200","400","800","1600","3200"])}
    # we first did that to get the palette but we need to control
    # the order of the palette





    for object in tiles["objects"]:
        if object.get("ignore"):
            continue
        generate_mask = object.get("generate_mask",False)

        blit_pad = object.get("blit_pad",True)
        name = object["name"]

        start_x = object["start_x"]+x_offset
        start_y = object["start_y"]+y_offset
        horizontal = object.get("horizontal",default_horizontal)
        width = object.get("width",default_width)
        height = object.get("height",default_height)


        nb_frames = object["frames"]
        for i in range(nb_frames):
            if horizontal:
                x = i*width+start_x
                y = start_y
            else:
                x = start_x
                y = i*height+start_y

            area = (x, y, x + width, y + height)
            cropped_img = sprites.crop(area)
            if nb_frames == 1:
                cropped_name = os.path.join(outdir,"{}.png".format(name))
            else:
                cropped_name = os.path.join(outdir,"{}_{}.png".format(name,i))
            cropped_img.save(cropped_name)

            # save
            x_size = cropped_img.size[0]
            sprite_number = object.get("sprite_number")
            sprite_palette = object.get("sprite_palette")
            if sprite_number is not None:
                if x_size != 16:
                    raise Exception("{} (frame #{}) width (as sprite) should 16, found {}".format(name,i,x_size))
                if sprite_palette:
                    sprite_palette = [tuple(x) for x in sprite_palette]
                    bitplanelib.palette_dump(sprite_palette,"../{}/{}.s".format("src",name))
                else:
                    sprite_palette_offset = 16+(sprite_number//2)*4
                    sprite_palette = game_palette[sprite_palette_offset:sprite_palette_offset+4]
                bin_base = "../{}/{}_{}.bin".format(sprites_dir,name,i) if nb_frames != 1 else "../{}/{}.bin".format(sprites_dir,name)
                bitplanelib.palette_image2sprite(cropped_img,bin_base,
                    sprite_palette,palette_precision_mask=0xF0)
            else:
                # blitter object
##                if x_size % 16:
##                    raise Exception("{} (frame #{}) with should be a multiple of 16, found {}".format(name,i,x_size))
                # pacman is special: 1 plane
                p = bitplanelib.palette_extract(cropped_img,palette_precision_mask=0xF0)
                # add 16 pixelsblit_pad
                img_x = x_size+16 if blit_pad else x_size
                img = Image.new("RGB",(img_x,cropped_img.size[1]))
                img.paste(cropped_img)
                # if 1 plane, pacman frames, save only 1 plane, else save all 4 planes
                one_plane = False  #len(p)==2
                used_palette = p if one_plane else game_palette_16

                namei = "{}_{}".format(name,i) if nb_frames!=1 else name

                print(name)
                bitplanelib.palette_image2raw(img,"../{}/{}.bin".format(sprites_dir,name_dict.get(namei,namei)),used_palette,
                palette_precision_mask=0xF0,generate_mask=generate_mask)

def process_fonts(dump=False):
    json_file = "fonts.json"
    with open(json_file) as f:
        tiles = json.load(f)

    default_width = tiles["width"]
    default_height = tiles["height"]
    default_horizontal = tiles["horizontal"]

    x_offset = tiles["x_offset"]
    y_offset = tiles["y_offset"]

    sprite_page = tiles["source"]

    sprites = Image.open(sprite_page)



    name_dict = {"letter_row_0_{}".format(i):chr(ord('A')+i) for i in range(0,16)}
    name_dict.update({"letter_row_1_{}".format(i):chr(ord('P')+i) for i in range(0,11)})
    name_dict["letter_row_1_11"] = "exclamation"
    name_dict.update({"digit_row_0_{}".format(i):chr(ord('0')+i) for i in range(0,10)})
    name_dict["digit_row_0_10"] = "slash"
    name_dict["digit_row_0_11"] = "dash"
    name_dict["digit_row_0_12"] = "quote"
    name_dict["digit_row_0_13"] = "quote2"
    # we first did that to get the palette but we need to control
    # the order of the palette



    for object in tiles["objects"]:
        if object.get("ignore"):
            continue
        name = object["name"]
        start_x = object["start_x"]+x_offset
        start_y = object["start_y"]+y_offset
        horizontal = object.get("horizontal",default_horizontal)
        width = object.get("width",default_width)
        height = object.get("height",default_height)

        nb_frames = object["frames"]
        for i in range(nb_frames):
            if horizontal:
                x = i*width+start_x
                y = start_y
            else:
                x = start_x
                y = i*height+start_y

            area = (x, y, x + width, y + height)
            cropped_img = sprites.crop(area)
            if dump:
                bn = "{}_{}.png".format(name,i) if nb_frames != 1 else name+".png"
                cropped_name = os.path.join(outdir,bn)
                cropped_img.save(cropped_name)

            # save
            x_size = cropped_img.size[0]

            # blitter object
            if x_size % 8:
                raise Exception("{} (frame #{}) with should be a multiple of 8, found {}".format(name,i,x_size))
            # pacman is special: 1 plane
            p = bitplanelib.palette_extract(cropped_img,palette_precision_mask=0xF0)
            # add 16 pixels if multiple of 16 (bob)
            img_x = x_size+16 if x_size%16==0 else x_size
            img = Image.new("RGB",(img_x,cropped_img.size[1]))
            img.paste(cropped_img)
            # if 1 plane, pacman frames, save only 1 plane, else save all 4 planes
            used_palette = p if len(p)==2 else game_palette

            namei = "{}_{}".format(name,i) if nb_frames != 1 else name
            bitplanelib.palette_image2raw(img,"../{}/{}.bin".format(sprites_dir,name_dict.get(namei,namei)),used_palette,palette_precision_mask=0xF0)

process_intro_maze()
#process_maze()

#process_tiles()

#process_fonts()
