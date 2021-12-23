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
- faithful enemy behaviour & speed
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
- level 2: thief palette is wrong (fright mode is green)
- level 2: fall frames don't match (match level 1)
- level 1/2: normal music loop is wrong
- collision: sometimes player goes through enemy
- intro: wrong start move, maze check rework probably caused that
- music loops wrong ... even in bonus mode sometimes
- level 3: music doesn't start immediately
- undo_paint: not perfect
- sometimes rollbacks instead of commiting (semi-random), still unexpected transition states
  => impossible to end the level

TODO:

- gameover with score: go to score table directly with score flashing
- thief attack pauses, then more or less random, then follow
  level 1: thief almost 8 loops: attacks
  level 2: thief almost 7 loops: 80 seconds
  level 3: 65 seconds
        4: 55
        5: 53
        6: 25
        7: 12
        8: 10
        9: 8
        10: 5
        11: 4
        12: 3
        13: 2
        14: 0
        
- enemy speed increasing, level 1-2: 20/20 level 3-4: 20/19 levl 5-6: 20/18
  level 8: 20/16 level 9-13: 20/15, at level 15 reaches 20/13 speed (max)
- reorganize sprites to match various palettes when an enemy is killed (tricky!)
- 
- commit_paint: re-paint rectangle bounds too

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


