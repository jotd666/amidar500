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
- F10 (or quitkey): quit (and save scores)

HOW TO PLAY:

- avoid enemies
- eat all dots (odd levels) to complete level
- paint all zones (even levels) to complete level. You cannot start painting from an unpainted zone
- if cornered use jump to get past the enemies
- fill all 4 corner regions to get a temporary "power state" where you can kill the
  enemies
- after a while, the white thief (aka tracer) will start following you. Don't wait too long!

ISSUES:

- reorganize sprites to match various palettes when an enemy is killed (tricky!)
- no animation when falling after normal music resumes, should be continuing falling
- sometimes no bonus level (very rare)


CREDITS:

- Jean-Francois Fabre (aka jotd): code and gfx/sfx conversion
- Andrzej Dobrowolski (aka no9): music conversion
- Frank Wille (aka phx): sfx/module player
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
- gnu make

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


