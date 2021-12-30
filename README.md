Amidar 500

This is a (successful) attempt by jotd to create a 1:1 port of the famous arcade game on Amiga 500 using 100% 68k assembly.

The display is 4:3 so scores, lives, bonuses are on the side rather than on top/botton. The gameplay layout is 1:1 vs
the original, though.

REQUIRES:

- any 68k CPU
- Kickstart 1.3, 512k memory or
- Kickstart 2.0, 1MB memory

FEATURES:

- original visual & sounds
- faithful enemy behaviour & speed & increasing difficulty
- 50 frames per second (PAL) even on a 68000 A500
- all levels & bonus levels
- original intro
- joystick controlled (port 1) or keyboard controls (arrows + space)
- can run directly from shell or from whdload (fast machines/complex configurations)

CONTROLS:

- joystick directions/arrows: move player
- space/fire button: start game
- P/second button: pause
- F10 (or quitkey): quit

ISSUES:

- turn correction not perfect, sometimes character blocks or goes the wrong way
- level 1: thief palette is wrong: red & white are inverted
- level 2: fright mode is green
- level 2: fall frames don't match (match level 1)
- level 2: normal music loop is wrong
- more than 4 normal amidars => wrong palette (blue)
- 6 normal amidars (level 15) => no more tracer visible (but still deadly)!!!

MINOR ISSUES

- bonus level: music loop too long only sometimes
- intro: wrong start move, maze check rework probably caused that
- undo_paint: not perfect but pretty close
- paint: missing bottom right corner
- can paint corner with no painted neighbours
- paint bonus level: paint when moving, not on update
  (avoids tricky stuff & missing paint)

TODO:

- reorganize sprites to match various palettes when an enemy is killed (tricky!)
- thief attack pause length is different depending of the time
  it took to reach the first target tile


CREDITS:

- jotd: code and gfx/sfx conversion
- no9: music conversion
- phx: sfx/module player
- meynaf: random routine
- eab forum: useful advice & support
- Superjustinbros: sprite rips https://www.spriters-resource.com/arcade/amidar
- konami: original game :)

BUILDING FROM SOURCES:

Prerequesites:

- Windows
- python
- Amiga NDK
- sox (included)
- vasm 68k (included)

* besides the .bin files created from png by python, the rest of the process could be built on an amiga with phxass
 or some other assembler and sox for the amiga, but you have to be really mad to attempt it in 2021...)
* could be done on Linux, just rebuild vasm

Build process:

- To create the ".bin" files and some palette .s asm files, from "assets" subdir, 
  just run the "convert_sprites.py" python script, then use the "convert_sounds.py"
  python script (audio).
- python and sox must be installed to be able to perform the wav2raw conversions
- get "bitplanelib.py" (asset conversion tool needs it) at https://github.com/jotd666/amiga68ktools.git

Binary assets must be created first, then makefile must be called to create the "mspacman" program


