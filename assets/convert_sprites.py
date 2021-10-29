import os,bitplanelib,json
from PIL import Image

sprites_dir = "sprites"

#for level 2 (paint)
square_scores = [500,350,300,250,280,150,100,450,400,450,280,230,200,300,
700,450,280,230,200,150,100,450,400,330,250,280,230,150,300,
500,300,330,250,230,180,150,100]

def process_maze():
    maze = Image.open("level1.png")

    # now the big time saver: detect maze walls & dots from MAME screenshots
    img = Image.new("RGB",(maze.size[0],maze.size[1]-24-16))
    img.paste(maze,(0,-24))
    img.save("try.png")
    for i,x in enumerate(range(8,208,40)):
        for j,y in enumerate(range(4,210,8)):
            # if not black then there's an horizontal segment here
            p = img.getpixel((x+4,y+3))
            if p != (0,0,0):
                print(i,j)
    if False:
            fill_color = img.getpixel((1,y))
            if outline_color != (0,0,0):
                pass

            maze_img = Image.new("RGB",(img.size[0],img.size[1]))

            # draw an image with just the maze
            # collect dot positions
            # add 3 rows for virtual coords (ghost targets)
            # add 2 columns for virtual coords (tunnel, keep positive logical coords)
            dot_matrix = [[0]*(img.size[0]//8 + 4) for _ in range(img.size[1]//8 + 3)]
            wall_matrix = [['W' if  y < 3 else 'O']*(img.size[0]//8 + 4) for y in range(img.size[1]//8 + 3)]


            for y in range(0,img.size[1]):
                ygrid = y//8+3
                dot_row = dot_matrix[ygrid]
                maze_row = wall_matrix[ygrid]

                maze_row[0] = maze_row[1] = 'W'
                maze_row[-1] = maze_row[-2] = 'W'
                for x in range(0,img.size[0]):
                    xgrid = x//8 + 2
                    p = img.getpixel((x,y))
                    if p in (fill_color,outline_color):
                        maze_img.putpixel((x,y),p)
                        # note down the wall
                        maze_row[xgrid] = 'W'
                    elif p==dot_color:
                        # either power dot or simple dot
                        # check if already determined
                        v = dot_row[xgrid]
                        if not v:
                            p2 = img.getpixel((x+3,y))
                            if p==p2:
                                # big dot
                                dot_row[xgrid]=2
                            else:
                                dot_row[xgrid]=1
            # tunnels & pen are added afterwards
            # pen is always at the same location
            wall_matrix[15][15:17] = ['P','P']
            for j in range(3):
                wall_matrix[16+j][13:19] = ["P"]*6
            # tunnels
            for y,w in tunnel:
                w += 2
                y += 3
                wall_matrix[y][0:w] = ["T"]*w
                wall_matrix[y][-w:] = ["T"]*w


            fw.write("maze_{}_dot_table_read_only:\n".format(i))
            for row in dot_matrix:
                fw.write("\tdc.b\t")
                fw.write(",".join(str(x) for x in row))
                fw.write("\n")
            fw.write("\nmaze_{}_wall_table:\n".format(i))
            for row in wall_matrix:
                fw.write("\tdc.b\t")
                fw.write(",".join(str(x) for x in row))
                fw.write("\n")

            def torgb4(t):
                return ((t[0]&0xF0)<<4)+((t[1]&0xF0))+(t[2]>>4)
            fw.write("\n\teven\nmaze_{}_misc:\n".format(i))
            fw.write("\tdc.w\t${:x}  ; dots\n".format(torgb4(dot_color)))
            fw.write("\tdc.w\t${:x}  ; outline\n".format(torgb4(outline_color)))
            fw.write("\tdc.w\t${:x}  ; fill\n".format(torgb4(fill_color)))
            fw.write("\tdc.w\t{}    ; total nb dots\n\n".format(nb_dots[i-1]))


            # now dump each maze with its own palette
            maze_palette = [(0,0,0),(0,0,0),outline_color,fill_color]  # black, dot color, maze colors



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
    dc.w    $CC9      ; beige (dots)

	dc.w	$0ff0    ; yellow (pos 4)
    dc.w    $00f0   ; green2
    dc.w    $00ff       ; cyan
    dc.w    $0c0f       ; magenta1
    dc.w    $00e0   ; green1    (pos 8)

    dc.w    $0f7f       ; magenta2
    dc.w    $0f91       ; brown
    dc.w    $0fff   ; white
    dc.w    $0f00       ; red   (pos 12)

    dc.w   $0E5D,$EED,$0
"""

game_palette_level_1_sprites = """
    ; sprites
    dc.w    $000,$0E0,$F91,$FF0     ; guard
    dc.w    $000,$0E0,$F91,$FF0     ; guard
    dc.w    $000,$0E0,$F91,$FF0     ; guard
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
        print(name)
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
                one_plane = len(p)==2
                used_palette = p if one_plane else game_palette_16

                namei = "{}_{}".format(name,i) if nb_frames!=1 else name

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

process_maze()

process_tiles()

process_fonts()
