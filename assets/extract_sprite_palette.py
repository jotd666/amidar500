import bitplanelib
from PIL import Image

img = Image.open("sprites.png")
p = bitplanelib.palette_extract(img,0xf0)
s = bitplanelib.palette_dump(p,"palette.s")