	include	"exec/types.i"
	include	"exec/memory.i"
	include	"exec/libraries.i"
	include	"exec/execbase.i"

	include "dos/dos.i"
	include "dos/var.i"
	include "dos/dostags.i"
	include "dos/dosextens.i"
	include "intuition/intuition.i"
	include	"hardware/cia.i"
	include	"hardware/custom.i"
	include	"hardware/intbits.i"
	include	"graphics/gfxbase.i"
	include	"graphics/videocontrol.i"
	include	"graphics/view.i"
	include	"devices/console.i"
	include	"devices/conunit.i"
	include	"libraries/lowlevel.i"
	INCLUDE	"workbench/workbench.i"
	INCLUDE	"workbench/startup.i"
	
	include "lvo/exec.i"
	include "lvo/dos.i"
	include "lvo/lowlevel.i"
	include "lvo/graphics.i"
	
    
    include "whdload.i"
    include "whdmacros.i"

    incdir "../sprites"
    incdir "../sounds"


INTERRUPTS_ON_MASK = $E038

    STRUCTURE   SpritePalette,0
    UWORD   color0
    UWORD   color1
    UWORD   color2
    UWORD   color3
    LABEL   SpritePalette_SIZEOF
    
	STRUCTURE	Character,0
    ULONG   character_id
	UWORD	xpos
	UWORD	ypos
    UWORD   h_speed
    UWORD   v_speed
	UWORD	direction   ; sprite orientation
	UWORD	previous_direction   ; previous sprite orientation
    UWORD   frame
    UWORD   turn_lock
	LABEL	Character_SIZEOF

	STRUCTURE	Player,0
	STRUCT      BaseCharacter1,Character_SIZEOF
    UWORD   prepost_turn
    LABEL   Player_SIZEOF
    
	STRUCTURE	Enemy,0
	STRUCT      BaseCharacter2,Character_SIZEOF
	STRUCT      palette,SpritePalette_SIZEOF
    APTR     frame_table
    APTR     copperlist_address
    APTR     color_register
    UWORD   speed_table_index
    UWORD   previous_xpos
    UWORD   previous_ypos
    UWORD   score_frame
    UWORD    mode_timer     ; number of 1/50th to stay in the current mode (thief only)
    UWORD    mode           ; current mode
    UWORD    previous_mode           ; previous mode
    UWORD    score_display_timer
    UWORD    fall_hang_timer
    UWORD    fall_hang_toggle
	UBYTE	 fright_mode
	UBYTE	 pad
	LABEL	 Enemy_SIZEOF
    
    ;Exec Library Base Offsets


;graphics base

StartList = 38

Execbase  = 4


; ******************** start test defines *********************************

; ---------------debug/adjustable variables

; if set skips intro, game starts immediately
;DIRECT_GAME_START

; if set, only thief is in play, and attacks immediately

;THIEF_AI_TEST

; test bonus screen 
;BONUS_SCREEN_TEST

; enemies not moving/no collision detection
;NO_ENEMIES

;HIGHSCORES_TEST

; 
;START_NB_LIVES = 1
;START_SCORE = 1000/10
;START_LEVEL = 4

; temp if nonzero, then records game input, intro music doesn't play
; and when one life is lost, blitzes and a0 points to move record table
; a1 points to the end of the table
; 100 means 100 seconds of recording at least (not counting the times where
; the player (me :)) isn't pressing any direction at all.
;RECORD_INPUT_TABLE_SIZE = 100*ORIGINAL_TICKS_PER_SEC
; 1 or 2, 2 is default, 1 is to record level 1 demo moves
;INIT_DEMO_LEVEL_NUMBER = 1

; ******************** end test defines *********************************

; don't change the values below, change them above to test!!

	IFD	HIGHSCORES_TEST
EXTRA_LIFE_SCORE = 3000/10
EXTRA_LIFE_PERIOD = 7000/10
DEFAULT_HIGH_SCORE = 10000/10
	ELSE
EXTRA_LIFE_SCORE = 30000/10
EXTRA_LIFE_PERIOD = 70000/10
DEFAULT_HIGH_SCORE = 10000/10
	ENDC
NB_HIGH_SCORES = 10

	IFND	INIT_DEMO_LEVEL_NUMBER
INIT_DEMO_LEVEL_NUMBER = 2
	ENDC
	
	IFND	START_SCORE
START_SCORE = 0
	ENDC
	IFND	START_NB_LIVES
START_NB_LIVES = 3+1
	ENDC
	IFND	START_LEVEL
		IFD		RECORD_INPUT_TABLE_SIZE
START_LEVEL = INIT_DEMO_LEVEL_NUMBER
		ELSE
START_LEVEL = 1
		ENDC
	ENDC
	
N = 0;  if no maze, 
U = 1;                  if has dot (unpainted)
T = 2;                  if temp paint
F = 3;    if fully painted

TSHIFT = 2
; 16 transition values total (including N state which is not possible)
; so 9 transition possible values
TU = (T<<TSHIFT)+U
FF = (F<<TSHIFT)+F
UU = (U<<TSHIFT)+U
TT = (T<<TSHIFT)+T
FU = (F<<TSHIFT)+U
UF = (U<<TSHIFT)+F
TF = (T<<TSHIFT)+F
UT = (U<<TSHIFT)+T
FT = (F<<TSHIFT)+T

NB_RECORDED_MOVES = 100

BONUS_PENDING = 0
BONUS_WON = 1
BONUS_LOST = 2

; caution:
; 1. do not change the order of those enumerates
;    some parts of the code check against mode <= HANG
; 2. do not change the values without reordering the draw & update jump tables
;
MODE_NORMAL = 0     ; police/cattle only. normal amidar movement
MODE_STANDBY = 1<<2     ; thief only. thief ventures in the maze
MODE_BORDER_PATROL = 2<<2 ; thief only. thief moves along maze borders
MODE_CHASE = 3<<2      ; thief only. thief chases player
MODE_HANG = 4<<2       ; enemies hang on the maze
MODE_FALL = 5<<2       ; enemies fall down
MODE_JUMP = 6<<2
MODE_KILL = 7<<2
MODE_KILLED = 8<<2
MODE_CRASH = 9<<2      ; enemy just hit the ground after falling
MODE_LAST_ITEM = 10<<2

; --------------- end debug/adjustable variables

; actual nb ticks (PAL)
NB_TICKS_PER_SEC = 50
; game logic ticks
ORIGINAL_TICKS_PER_SEC = 60


NB_BYTES_PER_LINE = 40
NB_BYTES_PER_MAZE_LINE = 26
BOB_16X16_PLANE_SIZE = 64
BOB_8X8_PLANE_SIZE = 16
MAZE_PLANE_SIZE = NB_BYTES_PER_LINE*NB_LINES
NB_LINES = 31*8
SCREEN_PLANE_SIZE = 40*NB_LINES
NB_PLANES   = 4

NB_TILES_PER_LINE = NB_BYTES_PER_MAZE_LINE
NB_TILE_LINES = 27
MAZE_HEIGHT = NB_TILES_PER_LINE*8
MAZE_ADDRESS_OFFSET = 6*NB_BYTES_PER_LINE+1
INTRO_MAZE_HEIGHT = 14*8-2
INTRO_MAZE_ADDRESS_OFFSET = 72*NB_BYTES_PER_LINE+1
BONUS_MAZE_ADDRESS_OFFSET = 14*NB_BYTES_PER_LINE+1

Y_MAX = MAZE_HEIGHT
X_MAX = (NB_BYTES_PER_MAZE_LINE-1)*8

STARS_OFFSET = NB_BYTES_PER_MAZE_LINE-4+(NB_BYTES_PER_LINE)*(MAZE_HEIGHT+18)

; maybe too many slots...
NB_ROLLBACK_SLOTS = 80
; messages from update routine to display routine
MSG_NONE = 0
MSG_SHOW = 1
MSG_HIDE = 2


PLAYER_KILL_TIMER = ORIGINAL_TICKS_PER_SEC*2
ENEMY_KILL_TIMER = ORIGINAL_TICKS_PER_SEC*2
GAME_OVER_TIMER = ORIGINAL_TICKS_PER_SEC*3

; direction enumerates, follows order of enemies in the sprite sheet
RIGHT = 0
LEFT = 1<<2
UP = 2<<2
DOWN = 3<<2
; one extra enumerate for fire (demo mode)
FIRE = 4

; possible direction bits, clockwise
DIRB_RIGHT = 0
DIRB_DOWN = 1
DIRB_LEFT = 2
DIRB_UP = 3
; direction masks
DIRF_RIGHT = 1<<DIRB_RIGHT
DIRF_DOWN = 1<<DIRB_DOWN
DIRF_LEFT = 1<<DIRB_LEFT
DIRF_UP = 1<<DIRB_UP

; states, 4 by 4, starting by 0

STATE_PLAYING = 0
STATE_GAME_OVER = 1*4
STATE_BONUS_SCREEN = 2*4
STATE_NEXT_LEVEL = 3*4
STATE_LIFE_LOST = 4*4
STATE_INTRO_SCREEN = 5*4
STATE_GAME_START_SCREEN = 6*4


; offset for enemy animations

JUMP_FIRST_FRAME = police1_jump_frame_table-police1_frame_table
HANG_FIRST_FRAME = police1_hang_frame_table-police1_frame_table
KILL_FIRST_FRAME = police1_kill_frame_table-police1_frame_table
FALL_FIRST_FRAME = police1_fall_frame_table-police1_frame_table
CRASH_FIRST_FRAME = FALL_FIRST_FRAME+8
SCORE_FIRST_FRAME = police1_score_frame_table-police1_frame_table

; jump table macro, used in draw and update
DEF_STATE_CASE_TABLE:MACRO
    move.w  current_state(pc),d0
    lea     .case_table(pc),a0
    move.l     (a0,d0.w),a0
    jmp (a0)
    
.case_table
    dc.l    .playing
    dc.l    .game_over
    dc.l    .bonus_screen
    dc.l    .next_level
    dc.l    .life_lost
    dc.l    .intro_screen
    dc.l    .game_start_screen

    ENDM
    
; write current PC value to some address
LOGPC:MACRO
     bsr    .next_\1
.next_\1
      addq.l    #6,(a7) ; skip this & next instruction
      move.l    (a7)+,$\1
      ENDM

MUL_TABLE:MACRO
mul\1_table
	rept	256
	dc.w	REPTN*\1
	endr
    ENDM
    
ADD_XY_TO_A1:MACRO
    lea mul40_table(pc),\1
    add.w   d1,d1
    lsr.w   #3,d0
    move.w  (\1,d1.w),d1
    add.w   d0,a1       ; plane address
    add.w   d1,a1       ; plane address
    ENDM


    
Start:
        ; if D0 contains "WHDL"
        ; A0 contains resload
        
    cmp.l   #'WHDL',D0
    bne.b   .standard
    move.l a0,_resload
    move.b  d1,_keyexit
    ;move.l  a0,a2
    ;lea	_tags(pc),a0
    ;jsr	resload_Control(a2)

    bsr load_highscores
    
    bra.b   .startup
.standard
    ; open dos library, graphics library
    move.l  $4.W,a6
    lea dosname(pc),a1
    moveq.l #0,d0
    jsr _LVOOpenLibrary(a6)
    move.l  d0,_dosbase
    lea graphicsname(pc),a1
    moveq.l #0,d0
    jsr _LVOOpenLibrary(a6)
    move.l  d0,_gfxbase

    bsr load_highscores

    ; check if "floppy" file is here
    
    move.l  _dosbase(pc),a6
    move.l   #floppy_file,d1
    move.l  #MODE_OLDFILE,d2
    jsr     _LVOOpen(a6)
    move.l  d0,d1
    beq.b   .startup
    
    ; "floppy" file found
    jsr     _LVOClose(a6)
    ; wait 2 seconds for floppy drive to switch off
    move.l  #100,d1
    jsr     _LVODelay(a6)
.startup

    lea  _custom,a5
    move.b  #0,controller_joypad_1
    

; no multitask
    tst.l   _resload
    bne.b   .no_forbid
    move.l  _gfxbase(pc),a4
    move.l StartList(a4),gfxbase_copperlist

    move.l  4,a6
    jsr _LVOForbid(a6)
    
	sub.l	A1,A1
	jsr	_LVOFindTask(a6)		;find ourselves
	move.l	D0,A0
	move.l	#-1,pr_WindowPtr(A0)	; no more system requesters (insert volume, write protected...)

    
.no_forbid
    
;    sub.l   a1,a1
;    move.l  a4,a6
;    jsr (_LVOLoadView,a6)
;    jsr (_LVOWaitTOF,a6)
;    jsr (_LVOWaitTOF,a6)

    move.w  #STATE_INTRO_SCREEN,current_state
    
    
    IFND    RECORD_INPUT_TABLE_SIZE
    ; uncomment to test demo mode right now
    ;;st.b    demo_mode
    ENDC
    
    move.w  #-1,high_score_position
    
    bsr init_state_transition_table
    bsr init_sound
    
    ; shut off dma
    lea _custom,a5
    move.w  #$7FFF,(intena,a5)
    move.w  #$7FFF,(intreq,a5)
    move.w #$03E0,dmacon(A5)

    bsr init_interrupts
    ; intro screen
    
    
    moveq #NB_PLANES-1,d4
    lea	bitplanes,a0              ; adresse de la Copper-List dans a0
    move.l #screen_data,d1
    move.w #bplpt,d3        ; premier registre dans d3

		; 8 bytes per plane:32 + end + bplcontrol
.mkcl:
    move.w d3,(a0)+           ; BPLxPTH
    addq.w #2,d3              ; next register
    swap d1
    move.w d1,(a0)+           ; 
    move.w d3,(a0)+           ; BPLxPTL
    addq.w #2,d3              ; next register
    swap d1
    move.w d1,(a0)+           ; 
    add.l #SCREEN_PLANE_SIZE,d1       ; next plane of maze

    dbf d4,.mkcl
    

    lea game_palette,a0
    lea _custom+color,a1
    move.w  #31,d0
.copy
    move.w  (a0)+,(a1)+
    dbf d0,.copy
;COPPER init
		
    move.l	#coplist,cop1lc(a5)
    clr.w copjmp1(a5)

;playfield init

    move.w #$3081,diwstrt(a5)             ; valeurs standard pour
    move.w #$30C1,diwstop(a5)             ; la fenêtre écran
    move.w #$0038,ddfstrt(a5)             ; et le DMA bitplane
    move.w #$00D0,ddfstop(a5)
    move.w #$4200,bplcon0(a5) ; 4 bitplanes
    clr.w bplcon1(a5)                     ; no scrolling
    clr.w bplcon2(a5)                     ; pas de priorité
    move.w #0,bpl1mod(a5)                ; modulo de tous les plans = 40
    move.w #0,bpl2mod(a5)

intro:
    lea _custom,a5
    move.w  #$7FFF,(intena,a5)
    move.w  #$7FFF,(intreq,a5)

    
    bsr hide_sprites

    bsr clear_screen
    
    bsr draw_score

    clr.l  state_timer
    clr.w  vbl_counter

   
    bsr wait_bof
    ; init sprite, bitplane, whatever dma
    move.w #$83E0,dmacon(a5)
    move.w #INTERRUPTS_ON_MASK,intena(a5)    ; enable level 6!!
    
    IFD DIRECT_GAME_START
	move.w	#1,cheat_keys	; enable cheat in that mode, we need to test the game
    bra.b   .restart
    ENDC
    
.intro_loop    
    cmp.w   #STATE_INTRO_SCREEN,current_state
    bne.b   .out_intro
    tst.b   quit_flag
    bne.b   .out
    move.l  joystick_state(pc),d0
    btst    #JPB_BTN_RED,d0
    beq.b   .intro_loop
    clr.b   demo_mode
.out_intro    


    clr.l   state_timer
    move.w  #STATE_GAME_START_SCREEN,current_state
    
.release
    move.l  joystick_state(pc),d0
    btst    #JPB_BTN_RED,d0
    bne.b   .release

    tst.b   demo_mode
    bne.b   .no_credit
    lea credit_sound(pc),a0
    bsr play_fx

.game_start_loop
    bsr random      ; so the enemies aren't going to do the same things at first game
    move.l  joystick_state(pc),d0
    tst.b   quit_flag
    bne.b   .out
    btst    #JPB_BTN_RED,d0
    beq.b   .game_start_loop

.no_credit

.wait_fire_release
    move.l  joystick_state(pc),d0
    btst    #JPB_BTN_RED,d0
    bne.b   .wait_fire_release    
.restart    
    lea _custom,a5
    move.w  #$7FFF,(intena,a5)
    
    bsr init_new_play

.new_level  
    bsr clear_screen
    bsr draw_score    
    bsr init_level
    lea _custom,a5
    move.w  #$7FFF,(intena,a5)

    bsr wait_bof
    
    bsr draw_score
    tst.b   next_level_is_bonus_level
    beq.b   .normal_level

    bsr init_player     ; at least reset 3 stars

    bsr wait_bof

    move.w  #STATE_BONUS_SCREEN,current_state
    move.w #INTERRUPTS_ON_MASK,intena(a5)
    
    bra.b   .mainloop
.normal_level    
    ; for debug
    ;;bsr draw_bounds
    
    bsr hide_sprites
    move.w  level_number(pc),d0
    btst    #0,d0
    beq.b   .dots
    bsr     init_paint
    bra.b   .dotsorpaint
.dots:
    bsr init_dots
.dotsorpaint

    ; enable copper interrupts, mainly
    moveq.l #0,d0
    bra.b   .from_level_start
.new_life
    moveq.l #1,d0
.from_level_start
    move.b  d0,new_life_restart ; used by init player
    bsr init_enemies
    bsr init_player
    
    bsr wait_bof

    tst.b   new_life_restart
    bne.b   .nodots
    bsr draw_maze
    move.w  level_number(pc),d0
    btst    #0,d0
    bne.b   .nodots
    bsr draw_dots
.nodots
    bsr draw_lives
    bsr draw_stars
    move.w  #STATE_PLAYING,current_state
    move.w #INTERRUPTS_ON_MASK,intena(a5)
.mainloop
    tst.b   quit_flag
    bne.b   .out
    DEF_STATE_CASE_TABLE
    
.game_start_screen
.intro_screen       ; not reachable from mainloop
    bra.b   intro
.bonus_screen
.playing
    bra.b   .mainloop

.game_over
    bra.b   .mainloop
.next_level
    add.w   #1,level_number
    bra.b   .new_level
.life_lost
    IFD    RECORD_INPUT_TABLE_SIZE
    lea record_input_table,a0
    move.l  record_data_pointer(pc),a1
    ; pause so debugger can grab data
    blitz
    ENDC

    tst.b   demo_mode
    beq.b   .no_demo
    ; lose one life in demo mode: return to intro
    move.w  #STATE_GAME_OVER,current_state
    move.l  #1,state_timer
    bra.b   .game_over
.no_demo
   
    tst.b   infinite_lives_cheat_flag
    bne.b   .new_life
    subq.b   #1,nb_lives
    bne.b   .new_life

    ; game over: check if score is high enough 
    ; to be inserted in high score table
    move.l  score(pc),d0
    lea     hiscore_table(pc),a0
	move.l	a0,$110
    moveq.w  #NB_HIGH_SCORES-1,d1
    move.w   #-1,high_score_position
.hiloop
    cmp.l  (a0)+,d0
    bcs.b   .lower
    ; higher or equal to a score
    ; shift all scores below to insert ours
    st.b    highscore_needs_saving
    move.l  a0,a1
    subq.w  #4,a0
    move.l  a0,a2   ; store for later
    tst.w   d1
    beq.b   .storesc    ; no lower scores: exit (else crash memory!)
	move.w	d1,d2
	; set a0 and a1 at the end of the score memory
	subq.w	#1,d2
	lsl.w	#2,d2
	add.w	d2,a1
	add.w	d2,a0	
    move.w  d1,d2       ; store insertion position
	addq.w	#4,a0
	addq.w	#4,a1
.hishift_loop
    move.l  -(a0),-(a1)
    dbf d2,.hishift_loop
.storesc
    move.l  d0,(a2)
    ; store the position of the highscore just obtained
    neg.w   d1
    add.w   #NB_HIGH_SCORES-1,d1
    move.w  d1,high_score_position
    bra.b   .hiout
.lower
    dbf d1,.hiloop
.hiout    
        ; high score

    ; save highscores if whdload
    tst.b   highscore_needs_saving
    beq.b   .no_save
    tst.l   _resload
    beq.b   .no_save
    tst.w   cheat_keys
    bne.b   .no_save
    bsr     save_highscores
.no_save
    ; 3 seconds
    move.l  #GAME_OVER_TIMER,state_timer
    move.w  #STATE_GAME_OVER,current_state
    bra.b   .game_over
.out      
    ; quit
    tst.l   _resload
    beq.b   .normal_end
    
    ; quit whdload
	pea	TDREASON_OK
	move.l	_resload(pc),-(a7)
	addq.l	#resload_Abort,(a7)
	rts
    
.normal_end
    bsr     restore_interrupts
    bsr     wait_blit
    bsr     finalize_sound
    bsr     save_highscores

    lea _custom,a5
    move.l  _gfxbase,a1
    move.l  gfxbase_copperlist,StartList(a1) ; adresse du début de la liste
    move.l  gfxbase_copperlist,cop1lc(a5) ; adresse du début de la liste
    clr.w  copjmp1(a5)
    ;;move.w #$8060,dmacon(a5)        ; réinitialisation du canal DMA
    
    move.l  4.W,A6
    move.l  _gfxbase,a1
    jsr _LVOCloseLibrary(a6)
    move.l  _dosbase,a1
    jsr _LVOCloseLibrary(a6)
    
    jsr _LVOPermit(a6)                  ; Task Switching autorisé
    moveq.l #0,d0
    rts

wait_bof
	move.l	d0,-(a7)
.wait	move.l	$dff004,d0
	and.l	#$1ff00,d0
	cmp.l	#260<<8,d0
	bne.b	.wait
.wait2	move.l	$dff004,d0
	and.l	#$1ff00,d0
	cmp.l	#260<<8,d0
	beq.b	.wait2
	move.l	(a7)+,d0
	rts    
    
clear_debug_screen
    movem.l d0-d1/a1,-(a7)
    lea	screen_data+SCREEN_PLANE_SIZE*3,a1 
    move.w  #NB_LINES-1,d1
.c0
    move.w  #NB_BYTES_PER_MAZE_LINE/4-1,d0
.cl
    clr.l   (a1)+
    dbf d0,.cl
    add.w   #NB_BYTES_PER_LINE-NB_BYTES_PER_MAZE_LINE,a1
    dbf d1,.c0
    movem.l (a7)+,d0-d1/a1
    rts
    
clear_screen
    lea screen_data,a1
    moveq.l #3,d0
.cp
    move.w  #(NB_BYTES_PER_LINE*NB_LINES)/4-1,d1
    move.l  a1,a2
.cl
    clr.l   (a2)+
    dbf d1,.cl
    add.l   #SCREEN_PLANE_SIZE,a1
    dbf d0,.cp
    rts

ISTT:MACRO
    move.l  #f_\1,(\2*4,a0)
    ENDM
    
init_state_transition_table
    lea state_transition_table(pc),a0
    ISTT    temp_to_unpainted,TU 
    ISTT    remains_painted,FF      ; do nothing
    ISTT    remains_unpainted,UU    ; do nothing
    ISTT    remains_temp,TT         ; possible (going backwards)
    ISTT    unpainted_to_temp,UT    ; not possible
    ISTT    painted_to_temp,FT      ; not possible
    ISTT    temp_to_painted,TF      ; 
    ISTT    unpainted_to_painted,UF 
    ISTT    painted_to_unpainted,FU 
    rts


; all the routines below are called with
; < A0: pointer on current tile (to change it if needed)
; < D0-D1: x,y
; trash: whatever register they need

f_temp_to_unpainted
    ; check pending rectangles
    ; validate if some full paints are pending
    move.l  pending_paint_rectangle_pointer(pc),a2
    lea     pending_paint_rectangle_buffer,a1
    cmp.l   a2,a1
    beq.b   .nothing_to_validate

    movem.w d0-d1,-(a7)
    bsr commit_paint
    movem.w (a7)+,d0-d1
.nothing_to_validate  
	; continues to painted => unpainted state now that we commited the paint
f_painted_to_unpainted
	; store previous direction, useful then in f_remains_temp to know
	; when to stop playing paint sound (when player reverses direction)
	move.w  player+direction(pc),previous_temp_paint_direction
    ; keeps painting
    bsr dot_painted_temp
    ; update compatible rectangles (intersection)
    ; rebuild the intersection in a temp list
    bsr get_dot_rectangles
    clr.l   -(a7)
    clr.l   -(a7)
    clr.l   -(a7)
    move.l  a7,a1
	clr.w	nb_compatible_rectangles
    lea compatible_rectangles(pc),a0
    
    move.l  d4,d0
    beq.b   .no_d4
    bsr     .lookup_rect
    bne.b   .no_d4
    move.l  d4,(a1)+        ; found: store	
	addq.w	#1,nb_compatible_rectangles
.no_d4    
    move.l  d5,d0
    beq.b   .no_d5
    bsr     .lookup_rect
    bne.b   .no_d5
    move.l  d5,(a1)+        ; found: store
	addq.w	#1,nb_compatible_rectangles
.no_d5    
    move.l  d6,d0
    beq.b   .no_d6
    bsr     .lookup_rect
    bne.b   .no_d6
    move.l  d6,(a1)+        ; found: store
	addq.w	#1,nb_compatible_rectangles
    ; note that increasing a1 is required even for the last test
    ; because if a1 == a7 then there's no intersection registered
.no_d6    
    cmp.l  a7,a1
    ; nothing found: rollback
    beq.b   .rollback    
    
    ; found: update compatible rectangles (and restore a7)
    move.l  (a7)+,(a0)+
    move.l  (a7)+,(a0)+
    move.l  (a7)+,(a0)
    
    rts
.rollback
    add.l  #12,a7
    bsr rollback_paint
    rts
    
; < a0: rectangles pointer
; > CCR Z: 1 if found, 0 else
.lookup_rect
    cmp.l   (a0),d0
    beq.b   .found
    cmp.l   (4,a0),d0
    beq.b   .found
    cmp.l   (8,a0),d0
.found    
    rts
    
f_remains_temp	
    ; check if direction change
    ; if no direction change, don't attempt to validate
	; pending rectangles
    move.w  player+direction(pc),d2
    move.w   previous_temp_paint_direction(pc),d3
	cmp.w	d2,d3
	beq.b	.out
	; store for later
	move.w	d2,previous_temp_paint_direction
	
	; did player reverse direction?
	; check with a table
	lsr.b	#2,d2
	or.b	d3,d2
	lea		reverse_direction_table(pc),a1
	tst.b	(a1,d2.w)
	beq.b	.no_reverse
	; reverse direction: stop paint sound
	bsr		stop_paint_sound
.no_reverse
    ; do nothing unless some rects are pending
    ; (means that player just changed direction)
    move.l  pending_paint_rectangle_pointer(pc),a2
    lea     pending_paint_rectangle_buffer,a1
    cmp.l   a2,a1
    bne.b   .validate
.out
    rts
.validate
    bra commit_paint
    
f_temp_to_painted
	; if entering a painted zone in a different tile
	; than when exiting it in the first place, then validate paint
    bsr     stop_paint_sound
	
	move.w	d0,d2
	move.w	d1,d3
	lsr.w	#3,d2
	lsr.w	#3,d3
	cmp.w	leaving_paint_tile_x(pc),d2
	bne.b	.no_rollback
	cmp.w	leaving_paint_tile_y(pc),d3
	beq.b	rollback_paint	
.no_rollback
    ; cancels temp paint immediately
    ; unless some full paints are pending
    move.l  pending_paint_rectangle_pointer(pc),a2
    lea     pending_paint_rectangle_buffer,a1
    cmp.l   a2,a1
    beq.b   .out	; special case

    bsr dot_painted_full
    bra commit_paint
	
.out
	; just left temporary paint for full paint but
	; the rectangle isn't complete, there is another
	; discontinuous segment to paint
	
	; just paint the zone (screen)
	; compute A1
	bsr		adjust_dot_paint_xyl	
	move.w  direction(a4),d3 ; add direction
   bsr     paint_zone
    rts


; < A1: rectangles to commit
; < A2: max rect pointer to stop to
; trashes: most registers
commit_paint
    cmp.l   a2,a1
    beq.b   .out
.vloop
    ; fill rectangles
    move.l  (a1)+,a0
    moveq.l   #0,d0
    move.w  points(a0),d0
    bsr add_to_score        ; even levels: bonus awarded on rectfill    
    
    bsr     fill_rectangle
    subq.b  #1,nb_rectangles
    beq.b   level_completed ; bail out: level was completed
    cmp.l   a1,a2
    bne.b   .vloop
    
    ; convert temp paint to full paint
    move.b  #F,d0
    bsr     set_stored_tiles

    ; reset pointers to start of lists
    bsr reset_rollback_pointers

    ; play fill sound once even if several rectangles
    lea     filled_sound,a0
    bra     play_fx
.out
	; safety
	rts
	
f_unpainted_to_painted
f_remains_painted
	; store the tile we're starting from
	lsr.w	#3,d2
	lsr.w	#3,d3
	move.w	d2,leaving_paint_tile_x
	move.w	d3,leaving_paint_tile_y
;init_compatible_rectangles
    ; compute the initial compatible rectangles all the time
	; computing them when leaving the painted zone is too late
	; and results in being able to paint some rectangles that are
	; not strictly adjacent (like the 200 score rect in the center row:
	; up, left, then up, this was a tricky corner case)
    bsr get_dot_rectangles
    lea compatible_rectangles(pc),a1
    
	clr.w	nb_compatible_rectangles
    ; we have them in D4-D6, now store them
    move.l  d4,(a1)+
	beq.b	.1
	addq.w	#1,nb_compatible_rectangles
.1
    move.l  d5,(a1)+
	beq.b	.2
	addq.w	#1,nb_compatible_rectangles
.2
    move.l  d6,(a1)
	beq.b	.3
	addq.w	#1,nb_compatible_rectangles
.3
    rts
    
    
       
f_remains_unpainted
	rts

    
f_unpainted_to_temp
    blitz
    nop
    nop
    nop
    rts
f_painted_to_temp
	; can happen when reverting from an incompletely painted
	; rectangle with one remaining discontinuous segment
	; which is a rare case but happens
	;
	; ->*
	; F T T T T F
	; F         F
	; F         F
	; F U U U U F
	
	; nothing to do
    rts
    
f_unknown_transition
    IFD     DEBUG_MODE

    blitz
    illegal
    ENDC
    rts

; < A0: zone to paint temporarily
; < D0,D1: X,Y
; trashes: none

dot_painted_temp
    movem.l d0-d6/a0-a1,-(a7)
    ; store a0 for undo/rollback

    move.l  rollback_dot_table_pointer(pc),a1
    move.l  a0,(a1)+
    move.l  a1,rollback_dot_table_pointer

    ; save X/Y
    move.w  d0,d2
    move.w  d1,d3
     
    ; load D4-D6 for later
    bsr     get_dot_rectangles
    
    move.b  #T,d0
    bsr     dot_painted_shared
	
    move.w  d2,d0
    move.w  d3,d1

	bsr		adjust_dot_paint_xyl	
    
    move.l  d4,a0
    beq.b	.zz
    bsr     tile_painted
.zz
    tst.l   d5
    beq.b   .zz2
    move.l  d5,a0
    bsr     tile_painted
.zz2
    tst.l   d6
    beq.b   .zz3
    move.l  d6,a0
    bsr     tile_painted
.zz3
    move.w  direction(a4),d3

    bsr     paint_zone
    movem.l (a7)+,d0-d6/a0-a1
    rts
    
; < A0: zone to paint fully
; < D0,D1: X,Y
; trashes: none

dot_painted_full
    movem.l d0-d6/a0-a1,-(a7)

    move.b  #F,d0    
    bsr     dot_painted_shared
    
    movem.l (a7)+,d0-d6/a0-a1
    rts
        
    
    
dot_painted_shared
    move.b  d0,(a0)
    move.b  d0,previous_tile_type   ;  update for next time

    bsr play_paint_sound
    ; add 10 to the score
    moveq.l #1,d0
    bra.b add_to_score

; < D0: X
; < D1: Y
; < A4: player
; > A1: paint address

adjust_dot_paint_xyl    
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    addq.w  #8,d0
    addq.w  #1,d1
    cmp.w   #UP,direction(a4)
    beq.b   .radd9
    cmp.w   #DOWN,direction(a4)
    beq.b   .radd8
    ; horizontal
    addq.w  #5,d1
    cmp.w   #RIGHT,direction(a4)
    bne.b   .noradd
    subq.w  #1,d0   ; to right: 8 pixels left shift
    bra.b   .noradd
.radd8
    ; vertical down
    addq.l  #1,d1
    bra.b   .noradd
.radd9
    ; vertical up
    addq.l  #2,d1
.noradd

    ADD_XY_TO_A1    a0
	rts
	
    
clear_playfield_planes
    lea screen_data,a1
    bsr clear_playfield_plane
    add.w   #SCREEN_PLANE_SIZE,a1
    bsr clear_playfield_plane
    add.w   #SCREEN_PLANE_SIZE,a1
    bsr clear_playfield_plane
    add.w   #SCREEN_PLANE_SIZE,a1
    bra clear_playfield_plane
    
; < A1: plane start
clear_playfield_plane
    movem.l d0-d1/a0-a1,-(a7)
    move.w #NB_LINES-1,d0
.cp
    move.w  #NB_BYTES_PER_MAZE_LINE/4-1,d1
    move.l  a1,a0
.cl
    clr.l   (a0)+
    dbf d1,.cl
    clr.w   (a0)
    add.l   #NB_BYTES_PER_LINE,a1
    dbf d0,.cp
    movem.l (a7)+,d0-d1/a0-a1
    rts

clear_maze
    lea screen_data,a1
    bsr clear_maze_plane
    add.w   #SCREEN_PLANE_SIZE,a1
    bsr clear_maze_plane
    add.w   #SCREEN_PLANE_SIZE,a1
    bsr clear_maze_plane
    add.w   #SCREEN_PLANE_SIZE,a1
    bra clear_maze_plane
    
; < A1: plane start
clear_maze_plane
    movem.l d0-d1/a0-a1,-(a7)
    add.w   #NB_BYTES_PER_LINE*4,a1
    move.w #MAZE_HEIGHT+7+6,d0
.cp
    move.w  #NB_BYTES_PER_MAZE_LINE/4,d1
    move.l  a1,a0
.cl
    clr.l   (a0)+
    dbf d1,.cl
    add.l   #NB_BYTES_PER_LINE,a1
    dbf d0,.cp
    movem.l (a7)+,d0-d1/a0-a1
    rts

    
init_new_play:
	clr.b	previous_move
    clr.l   state_timer
    IFD BONUS_SCREEN_TEST
    st.b	next_level_is_bonus_level
    ELSE
    clr.b   next_level_is_bonus_level
    ENDC
 
    clr.w   can_eat_enemies_mode_pending
    move.b  #START_NB_LIVES,nb_lives
    clr.b   new_life_restart
    clr.b   extra_life_awarded
    clr.b    music_played
    move.l  #EXTRA_LIFE_SCORE,score_to_track
    move.w  #START_LEVEL-1,level_number
 
    ; global init at game start
	
	tst.b	demo_mode
	beq.b	.no_demo
	; toggle demo
	move.w	demo_level_number(pc),d0
	move.w	d0,level_number
	btst	#0,d0
	beq.b	.demo_level_1
	lea		demo_moves_2,a0
	lea		demo_moves_2_end,a1
	bra.b	.rset
.demo_level_1	
	lea		demo_moves_1,a0
	lea		demo_moves_1_end,a1
.rset
	move.l	a0,record_data_pointer
	move.l	a1,record_data_end
	eor.b	#1,d0
	move.w	d0,demo_level_number
	
.no_demo
    clr.b   bonus_sprites
    move.l  #START_SCORE,score
    clr.l   previous_score
    clr.l   displayed_score
    rts
    
init_level: 
    clr.w   completed_music_timer
    clr.b   nb_rectangles
	clr.l	state_timer
    ; sets initial number of dots

    clr.b    rustler_level
    lea rectlist_1,a0
    ; level
    move.w  level_number(pc),d2
    btst    #0,d2
    beq.b   .riloop
    st.b    rustler_level
    lea rectlist_2,a0
.riloop
    move.l  (a0)+,d0
    beq.b   .out
    addq.b  #1,nb_rectangles
    move.l  d0,a1
    move.w  mdots(a1),cdots(a1)
    bra.b   .riloop
    
.out
    
  
    
    clr.w   power_state_counter
    clr.w   power_song_countdown
    clr.b   elroy_mode_lock
    
    move.b   #4,nb_special_rectangles
    ; speed table, speed increasing every 2 levels up to level 15
    lea speed_table(pc),a1
    move.w  level_number(pc),d2
    lsr.w   #1,d2   ; divide
    cmp.w   #6,d2
    bcs.b   .no_max_speed
    move.w  #6,d2
.no_max_speed
    add.w   d2,d2
    add.w   d2,d2
    move.l  (a1,d2.w),a1    ; global speed table
    move.l  a1,global_speed_table

	
    lea nb_enemy_table(pc),a1
    move.w  level_number(pc),d2
    cmp.w   #5,d2
    bcs.b   .lower
    move.w  #5,d2
.lower
    add.w   d2,d2
    move.w  (a1,d2.w),nb_enemies_but_thief

	lea	standby_time_table(pc),a1
	move.w	(a1,d2.w),thief_standby_time
	
    IFD     THIEF_AI_TEST
    clr.w   nb_enemies_but_thief
    ENDC
    rts

; clear planes used for score (score hidden in acts)
clear_scores
    lea	screen_data+SCREEN_PLANE_SIZE*1,a1
    move.w  #232,d0
    move.w  #16,d1
    move.w  #8,d2
    move.w  #4,d3
.loop
    lea	screen_data+SCREEN_PLANE_SIZE*1,a1
    bsr clear_plane_any_blitter
    add.w	#SCREEN_PLANE_SIZE,a1
    bsr clear_plane_any_cpu
    add.w   #16,d1
    dbf d3,.loop
    rts
    
; draw score with titles and extra 0
draw_score:
    lea p1_string(pc),a0
    move.w  #232,d0
    move.w  #16,d1
    move.w  #$FF,d2
    bsr write_color_string
    lea score_string(pc),a0
    move.w  #$FFF,d2
    move.w  #232,d0
    add.w  #8,d1
    bsr write_color_string
    
    move.w  #$FF,d2
    lea high_score_string(pc),a0
    move.w  #232,d0
    move.w  #48,d1
    bsr write_color_string
    
    ; extra 0
    move.w  #$FFF,d2
    lea score_string(pc),a0
    move.w  #232,d0
    add.w  #8,d1
    bsr write_color_string

    move.l  score(pc),d2
    bsr     draw_current_score
    
    move.l  high_score(pc),d2
    bsr     draw_high_score

    lea level_string(pc),a0
    move.w  #232,d0
    move.w  #48+24,d1
    move.w  #$FF,d2
    bsr write_color_string

    moveq.l #1,d2
    add.w  level_number(pc),d2
    move.w  #232+48,d0
    move.w  #48+24+8,d1
    move.w  #3,d3
    move.w  #$FFF,d4
    bra write_color_decimal_number

    rts
    
; < D2 score
; trashes D0-D3
draw_current_score:
    move.w  #232+16,d0
    move.w  #24,d1
    move.w  #6,d3
    move.w  #$FFF,d4
    bra write_color_decimal_number
    
    
hide_sprites:
    move.w  #7,d1
    lea  sprites,a0
    lea empty_sprite,a1
.emptyspr

    move.l  a1,d0
    bsr store_sprite_copperlist
    addq.l  #8,a0
    dbf d1,.emptyspr
    rts
    
hide_enemy_sprites:
    move.w  #6,d1
    lea  sprites,a0
    lea empty_sprite,a1
.emptyspr

    move.l  a1,d0
    bsr store_sprite_copperlist
    add.w  #16,a0
    dbf d1,.emptyspr
    rts




    
init_enemies
    move.b  d0,d4
    lea enemies+Enemy_SIZEOF(pc),a0
    lea enemy_sprites,a1   ; the sprite part of the copperlist, sprite 1-7 are the ghost sprites
    
    ; palette depends on the level number
    lea alt_sprite_palette(pc),a3  ; the sprite part of the color palette 16-31
    move.l #cattle_fright_palette,fright_palette  ; the sprite part of the color palette 16-31
    move.l #cattle_fright_blink_palette,fright_blink_palette  ; the sprite part of the color palette 16-31
    tst.b   bonus_sprites
    bne.b   .rustler
    tst.b   rustler_level
    bne.b   .rustler
    lea game_palette+32(pc),a3  ; the sprite part of the color palette 16-31
    move.l #police_fright_palette,fright_palette  ; the sprite part of the color palette 16-31
    move.l #police_fright_blink_palette,fright_blink_palette  ; the sprite part of the color palette 16-31
.rustler


    move.w  level_number(pc),d0
    and.w   #3,d0
    lsl.w   #2,d0
    lea     enemy_start_position_table(pc),a2
    move.l  (a2,d0.w),a2
    
    move.w nb_enemies_but_thief(pc),d7
    beq.b   .no_other_enemies       ; just for test mode
    subq.w  #1,d7
    moveq.l #0,d0
    moveq.w #1,d1
    clr.w   d2
    
    lea enemies+Enemy_SIZEOF(pc),a0
.igloop
    ; copy all 4 "normal" colors
    move.l (a3),palette(a0)
    move.l (4,a3),palette+4(a0)

    
    move.l  a1,copperlist_address(a0)
    addq.l   #8,a1
    
    tst.b   d4
    bne.b   .no_reset

    clr.w   speed_table_index(a0)
.no_reset    
    ; all police try to go down and right or left alternately
    move.w  d1,h_speed(a0)
    neg.w   d1
    move.w  #1,v_speed(a0)

   
    clr.w   turn_lock(a0)
	move.w	(a2)+,xpos(a0)
    move.w  (a2)+,d0
    addq.w  #4,d0
	move.w	d0,ypos(a0)
	clr.b	fright_mode(a0)
    move.w  #DOWN,direction(a0)
    move.w  #MODE_NORMAL,mode(a0)
    move.w  #MODE_NORMAL,previous_mode(a0)
    ; WTF was I thinking with this code. This makes no sense and is completely
	; useless and buggy for 6+1 enemies
	; all palette entries are the same for amidar moving enemies
    ;eor.w   #1,d2
    ;beq.b   .forward_palette
    ;addq.w  #8,a3
;.forward_palette
    add.w   #Enemy_SIZEOF,a0
    dbf d7,.igloop
.no_other_enemies
  
    ; thief
    lea game_palette+56(pc),a3  ; 4 last colors
    tst.b   rustler_level
    beq.b   .no_rustler_2    
    lea alt_sprite_palette+24(pc),a3  ; 4 last colors
.no_rustler_2
    lea     enemies(pc),a0
    move.l (a3)+,palette(a0)
    move.l (a3)+,palette+4(a0)
    move.w  #UP,direction(a0)
    move.w  #MODE_BORDER_PATROL,mode(a0)
    move.w  #MODE_BORDER_PATROL,previous_mode(a0)
    clr.w   h_speed(a0)     ; moving up at start
    move.w  #-1,v_speed(a0)     ; moving up at start

    move.l  #thief_sprite,copperlist_address(a0)
	move.w	#Y_MAX,ypos(a0)
	move.w	#X_MAX,xpos(a0)

    bsr update_color_addresses
    ; all enemies normal palette
    bsr set_enemy_normal_palette

    move.w   #50,d1
    ; speeds & attack timeouts
    move.w  level_number(pc),d0
    cmp.w   #13,d0
    bcc.b   .immediately
    lea     attack_timeout_table(pc),a0
    add.w   d0,d0
    move.w  (a0,d0.w),d1
.immediately
    move.w  d1,attack_timeout

    clr.b   thief_attacks
    clr.b   player_move_record
    st.b    first_recorded_move
    
    lea enemies(pc),a0
    ; specific settings
    tst.b   bonus_sprites
    bne.b   .cattle
    tst.b   rustler_level
    beq.b   .police
.cattle    
    move.l  #cattle1_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l  #cattle2_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #cattle3_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #cattle4_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #cattle5_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #cattle6_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #cattle7_frame_table,frame_table(a0)
    
    rts
.police
    move.l  #police1_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l  #police2_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #police3_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #police4_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #police5_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #police6_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #police7_frame_table,frame_table(a0)
    
    rts

; from copperlist addresses update color addresses
; this allows to move sprites up/down the copperlist
; and recompute color addresses accordingly    
update_color_addresses
    
    lea   enemy_sprites,a2
    move.w nb_enemies_but_thief(pc),d7
    move.l #_custom+color+32,d1
    lea enemies(pc),a0
.loop
    move.l  copperlist_address(a0),d0
    sub.l   a2,d0   ; offset
    and.b   #$F0,d0   ; truncate to previous 16th divisor
    lsr.w   #1,d0     ; then divide
    move.l  d1,a1
    add.w   d0,a1
    move.l  a1,color_register(a0)
    add.w   #Enemy_SIZEOF,a0
    dbf d7,.loop
    rts
    
init_player:
	clr.w	leaving_paint_tile_x
	clr.w	leaving_paint_tile_y
    clr.l   previous_valid_direction
    clr.w   death_frame_offset
	
    tst.b   new_life_restart
    bne.b   .no_clear
    clr.l   previous_player_address   ; no previous position
.no_clear
    lea player(pc),a0
    tst.b   rustler_level
    bne.b   .level2
    move.l  #'COPI',character_id(a0)
    bra.b   .cont
.level2
    move.l  #'RUST',character_id(a0)
    move.b  #F,previous_tile_type       ; comes from "fully painted"
    bsr     reset_rollback_pointers
.cont
    
    move.w  #NB_TILES_PER_LINE*4-2,xpos(a0)
	move.w	#Y_MAX,ypos(a0)
    
    ;move.w  #0*4,xpos(a0)
	;move.w	#192,ypos(a0)
    
    
	move.w 	#LEFT,direction(a0)

    clr.w  speed_table_index(a0)
    move.w  #-1,h_speed(a0)
    clr.w   v_speed(a0)
    clr.w   prepost_turn(a0)
    move.w  #0,frame(a0)

    
    move.w  #ORIGINAL_TICKS_PER_SEC,D0   
    tst.b   music_played
    bne.b   .played
    st.b    music_played


    IFD    RECORD_INPUT_TABLE_SIZE
    ELSE
    IFND     DIRECT_GAME_START
    tst.b   demo_mode
    beq.b   .no_demo
    ENDC

.no_demo
    ENDC
.played
    IFD    RECORD_INPUT_TABLE_SIZE
    move.l  #record_input_table,record_data_pointer ; start of table
    move.l  #-1,prev_record_joystick_state	; impossible previous value, force record
    clr.l   previous_random
    ENDC

    clr.w   record_input_clock                      ; start of time
    

    move.w  #-1,player_killed_timer
    clr.w   next_enemy_iteration_score
    clr.w   fright_timer    
    move.b  #3,nb_stars
    move.w  #-1,jump_index
    move.w  #JUMP_FIRST_FRAME,jump_frame
    
    rts
    	    

    
DEBUG_X = 8     ; 232+8
DEBUG_Y = 8

ghost_debug
    lea enemies(pc),a2
    move.w  #DEBUG_X,d0
    move.w  #DEBUG_Y+100,d1
    lea	screen_data+SCREEN_PLANE_SIZE*3,a1 

    bsr .debug_ghost

    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .elroy(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.l  a2,a0

    
;    move.w  #DEBUG_X,d0
;    add.w  #8,d1
;    lea .dir(pc),a0
;    bsr write_string
;    lsl.w   #3,d0
;    add.w  #DEBUG_X,d0
;    clr.l   d2
;    move.w  direction(a2),d2
;    move.w  #0,d3
;    bsr write_decimal_number
;
;    move.w  #DEBUG_X,d0
;    add.w  #8,d1
;    lea .pdir(pc),a0
;    bsr write_string
;    lsl.w   #3,d0
;    add.w  #DEBUG_X,d0
;    clr.l   d2
;    move.w  possible_directions,d2
;    move.w  #4,d3
;    bsr write_hexadecimal_number
    rts
.debug_ghost
    rts
    
.mode
        dc.b    "MODE ",0

.elroy:
    dc.b    "ELROY ",0

.gx
        dc.b    "GX ",0
.gy
        dc.b    "GY ",0
        even

        
draw_debug
    lea player(pc),a2
    move.w  #DEBUG_X,d0
    move.w  #DEBUG_Y,d1
    lea	screen_data+SCREEN_PLANE_SIZE,a1 
    lea .px(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w xpos(a2),d2
    move.w  #5,d3
    bsr write_decimal_number
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    move.l  d0,d4
    lea .py(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w ypos(a2),d2
    move.w  #3,d3
    bsr write_decimal_number
    move.l  d4,d0
    ;;
    add.w  #8,d1
    lea .ph(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w previous_valid_direction(pc),d2
    move.w  #5,d3
    bsr write_decimal_number
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    move.l  d0,d4
    lea .pv(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w previous_valid_direction+2(pc),d2
    move.w  #3,d3
    bsr write_decimal_number
    move.l  d4,d0
	
        IFEQ    1
    add.w  #8,d1
    lea .tx(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w xpos+enemies(pc),d2
    move.w  #5,d3
    bsr write_decimal_number
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    move.l  d0,d4
    lea .ty(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w ypos+enemies(pc),d2
    move.w  #3,d3
    bsr write_decimal_number
    move.l  d4,d0
    ENDC
    ;;
    ;;
    add.w  #8,d1
    lea .ato(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w  attack_timeout(pc),d2
    move.w  #4,d3
    bsr write_decimal_number
	
    add.w  #8,d1
    lea .pmi(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w  player_move_index(pc),d2
    move.w  #4,d3
    bsr write_decimal_number
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    move.l  d0,d4
    lea .tmi(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w thief_move_index(pc),d2
    move.w  #4,d3
    bsr write_decimal_number
    move.l  d4,d0
    
    add.w  #8,d1
    lea .diff(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w  player_move_index(pc),d2
    sub.w thief_move_index(pc),d2
    move.w  #4,d3
    bsr write_decimal_number
    move.l  d4,d0
    ;;

    clr.l   d2
    ; ---
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .bottom_rect_string(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    move.l  d0,d3
    lea rect_2_20,a0
    move.w  cdots(a0),d2
    move.w  #3,d3
    bsr write_decimal_number
    
    ;;
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .bonus(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2

    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .dottable(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    move.l	maze_wall_table(pc),d2
    move.w  #8,d3
    bsr write_hexadecimal_number
	
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .nbrects(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
	clr.l	d2
    move.w	nb_compatible_rectangles(pc),d2
    move.w  #2,d3
    bsr write_hexadecimal_number

    rts
    
.px
        dc.b    "PX ",0
.py
        dc.b    "PY ",0
.ph
		dc.b	"PREVH ",0
.pv
		dc.b	"PREVV ",0
.ato
		dc.b "ATO ",0
.tx
        dc.b    "TX ",0
.ty
        dc.b    "TY ",0

.pmi
        dc.b    "PMI ",0
.tmi
        dc.b    "TMI ",0
.diff
        dc.b    "DIFF ",0
.bottom_rect_string
        dc.b    "R2 20 ",0
.bonus
        dc.b    "BT ",0
.dottable:
        dc.b    "DOTS ",0
.nbrects
		dc.b	"NBRECTS ",0
        even

draw_enemies:
    lea enemies(pc),a0
    move.w  nb_enemies_but_thief(pc),d7   ; +thief
.gloop
    bsr .draw_enemy
.next_ghost_iteration
    add.l   #Enemy_SIZEOF,a0
    dbf d7,.gloop
    
    rts

.eyes
 

    bra.b   .end_anim

.draw_enemy
    move.w  xpos(a0),d0
    addq.w  #1,d0       ; compensate
    move.w  ypos(a0),d1
    addq.w  #3,d1   ; compensate
    ; center => top left
    bsr store_sprite_pos

    ; we cannot have white color for score
    ; that would trash the other enemy
    ;;move.w  #$00ff,_custom+color+32+8+2

    move.w  mode(a0),d3 ; normal/chase/fright/fall..
    IFD     DEBUG_MODE
    cmp.w   #MODE_LAST_ITEM,d3
    bcs.b   .in_range
    blitz
    illegal
.in_range
    ENDC
    lea     palette(a0),a2      ; normal ghost colors
    lea     .jump_table(pc),a1
    move.l  (a1,d3.w),a1
    jmp     (a1)
    
.draw_hang
    addq.w  #8,d1   ; compensate
    move.w  fall_hang_toggle(a0),d2
    add.w   #HANG_FIRST_FRAME,d2
    bra.b   .get_frame
.draw_fall
    tst.w   power_state_counter
    beq.b   .draw_normal    ; power has faded: normal sprites
    move.w  fall_hang_toggle(a0),d2
    add.w   #FALL_FIRST_FRAME,d2
    bra.b   .get_frame
.draw_crash
    move.w  fall_hang_toggle(a0),d2
    add.w   #CRASH_FIRST_FRAME,d2
    bra.b   .get_frame
.draw_jump
    ; choose the frame among jump/hang/fall
    move.w  jump_frame(pc),d2
    bra.b   .get_frame
.draw_killed
    move.w  score_frame(a0),d2
    bra.b   .get_frame
.draw_kill
    move.w  enemy_kill_frame(pc),d2
    bra.b   .get_frame
.draw_normal
    move.w  frame(a0),d2
    
    lsr.w   #2,d2   ; 8 divide to get 0,1
    bclr    #0,d2   ; even
    add.w   d2,d2       ; times 2
.end_anim

.get_frame
    move.l  frame_table(a0),a1
    ; get proper frame from proper frame set
    move.l  (a1,d2.w),a1
    ; now if D6 is non-zero, handle shift
.store_sprite_pos
    move.l  d0,(a1)     ; store control word
    move.l  a1,d2    
    move.l  copperlist_address(a0),a1
    move.w  d2,(6,a1)
    swap    d2
    move.w  d2,(2,a1)    
    rts
.jump_table
    dc.l    .draw_normal
    dc.l    .draw_normal
    dc.l    .draw_normal
    dc.l    .draw_normal
    dc.l    .draw_hang
    dc.l    .draw_fall
    dc.l    .draw_jump
    dc.l    .draw_kill
    dc.l    .draw_killed
    dc.l    .draw_crash
    
     
draw_all
    DEF_STATE_CASE_TABLE

; draw intro screen
.intro_screen
    bra.b   draw_intro_screen
; draw bonus screen
.bonus_screen
    tst.w   bonus_text_screen_countdown
    beq.b   .maze_part
    bsr     hide_sprites
    move.w  #72,d0
    move.w  #40,d1
    lea     .bonus_stage_text(pc),a0
    move.w  #$FF,d2
    bsr     write_color_string
    move.w  #80,d0
    move.w  #96,d1
    lea     .5000_points_text(pc),a0
    move.w  #$FF0,d2
    bsr     write_color_string
    move.w  #48,d0
    move.w  #136,d1
    lea     .push_jump_button_text(pc),a0
    move.w  #$F0,d2
    bsr     write_color_string
    ; banana sprite at x=60
    move.w  #60,d0
    move.w  #96,d1
    bsr store_sprite_pos

    ; write control word
    lea banana_sprite,a0
    move.l  d0,(a0)
    move.l  a0,d0
    lea bonus_banana,a0
    bsr store_sprite_copperlist

    ; banana colors (yellow)
    lea _custom+color+32,a1  ; sprite 0-1 colors
    lea banana_sprite_palette(pc),a0
    move.l  (a0)+,(a1)+
    move.l  (a0)+,(a1)+

    bsr draw_lives
    bsr draw_stars
.skip_draw
    rts
.maze_part
    cmp.l   #1,state_timer      ; init done at first tick of update_intro_screen
    beq.b   draw_bonus_maze
    
    ; draw cattle
    lea enemies+Enemy_SIZEOF(pc),a0
    move.w  xpos(a0),d0
    add.w   #2,d0
    move.w  ypos(a0),d1
    add.w   #3-4,d1
    bsr store_sprite_pos
    
    move.w   bottom_reached(pc),d2
    beq.b   .normal
    cmp.w   #BONUS_LOST,d2
    beq.b   .lost
    ; BONUS_WON
    ; switch banana for score
    move.l  d0,d2
    lea _custom+color+32,a1  ; sprite 0-1 colors
    move.l  #$0FFF0FFF,d0
    move.l  d0,(a1)+
    move.l  d0,(a1)+
    
    move.w  banana_x(pc),d0
    move.w  #200,d1
    bsr store_sprite_pos    
    
    ; write control word
    lea score_5000,a0
    move.l  d0,(a0)
    move.l  a0,d0
    lea bonus_banana,a0
    bsr store_sprite_copperlist
    
    move.l  d2,d0
    
    ; change sprite to first cattle jump
    lea cattle1_jump_0,a1
    bra.b  .store_cw
.lost
    lea cattle1_hang_1,a1
    ; change sprite to second cattle hang
    bra.b  .store_cw
.normal
    move.l  frame_table(a0),a1
    move.w  frame(a0),d2
    lsr.w   #2,d2   ; 8 divide to get 0,1
    bclr    #0,d2   ; even
    add.w   d2,d2       ; times 2

    ; get proper frame from proper frame set
    move.l  (a1,d2.w),a1

.store_cw
    move.l  d0,(a1)     ; store control word
    move.l  a1,d0
    lea intro_cattle_pink,a0
    bsr store_sprite_copperlist

    lea enemies+Enemy_SIZEOF(pc),a0

    move.w  xpos(a0),d0
    move.w  ypos(a0),d1

    ; don't bother about oring shit or whatnot: just copy the first plane into the second plane
    move.w  previous_xpos(a0),d2
    bmi.b   .no_diagonal
    cmp.w   d0,d2
    beq.b   .no_diagonal
    move.w  previous_ypos(a0),d3
    cmp.w   d1,d3
    beq.b   .no_diagonal
    ; both x and y are different because update skipped a frame and went diagonal
    ; we have to fix that else the line will be buggy
        ; y is different
    movem.w d0-d1,-(a7)
    move.w   d3,d1  ; previous y
    add.w   #2,d1
    bsr .copyit
    movem.w (a7),d0-d1
    move.w   d2,d0  ; previous y
    add.w   #2,d1
    bsr .copyit
    movem.w (a7)+,d0-d1
    
.no_diagonal
    
    move.w  d0,previous_xpos(a0)
    move.w  d1,previous_ypos(a0)
    
    add.w   #2,d1
    
    bsr .copyit
    
    rts
    
.copyit:
    lea screen_data,a1    
    ADD_XY_TO_A1    a2
    lea (SCREEN_PLANE_SIZE,a1),a2
    cmp.w   #LEFT,direction(a0)
    beq.b   .skipleft
    move.b  (a1),(a2)
    move.b  (NB_BYTES_PER_LINE,a1),(NB_BYTES_PER_LINE,a2)
.skipleft
    move.b  (1,a1),(1,a2)
    move.b  (NB_BYTES_PER_LINE+1,a1),(NB_BYTES_PER_LINE+1,a2)
   
    rts
    
.bonus_stage_text
    dc.b    "BONUS  STAGE",0
.5000_points_text
    dc.b    ".... 5000 PTS",0
.push_jump_button_text:
    dc.b    "PUSH JUMP  BUTTON",0
    even
    
.game_start_screen
    tst.l   state_timer
    beq.b   draw_start_screen
    rts
    
.life_lost
.next_level

    ; don't do anything
    rts
PLAYER_ONE_X = 72
PLAYER_ONE_Y = 102-14

    
.game_over
    cmp.l   #GAME_OVER_TIMER,state_timer
    bne.b   .draw_complete
    bsr hide_sprites
    bsr clear_maze

    move.w  #72,d0
    move.w  #136,d1
    move.w  #$0f00,d2   ; red
    lea player_one_string(pc),a0
    bsr write_color_string
    move.w  #72,d0
    add.w   #16,d1
    lea game_over_string(pc),a0
    bsr write_color_string
    
    bra.b   .draw_complete
.playing
    tst.b   delete_last_star_message
    beq.b   .no_del_star
    bsr delete_last_star
    clr.b   delete_last_star_message
.no_del_star

    bsr draw_player
    bsr draw_enemies
   
    
.after_draw
        
    ; timer not running, animate

    cmp.w   #MSG_SHOW,extra_life_message
    bne.b   .no_extra_life
    clr.w   extra_life_message
    bsr     draw_last_life
.no_extra_life


    ; score
    lea	screen_data+SCREEN_PLANE_SIZE*3,a1  ; white
    
    move.l  score(pc),d0
    move.l  displayed_score(pc),d1
    cmp.l   d0,d1
    beq.b   .no_score_update
    
    move.l  d0,displayed_score

    move.l  d0,d2
    bsr draw_current_score
    
    ; handle highscore in draw routine eek
    move.l  high_score(pc),d4
    cmp.l   d2,d4
    bcc.b   .no_score_update
    
    move.l  d2,high_score
    bsr draw_high_score
.no_score_update
.draw_complete
    rts

stop_sounds
    bsr stop_paint_sound
    lea _custom,a6
    clr.b   music_playing
    bra _mt_end



; < D2: highscore
draw_high_score
    move.w  #232+16,d0
    move.w  #24+32,d1
    move.w  #6,d3
    move.w  #$FFF,d4    
    bra write_color_decimal_number


    
; < D0: score (/10)
; trashes: D0,D1
add_to_score:
	tst.b	demo_mode
	bne.b	.below
    move.l  score(pc),previous_score

    add.l   d0,score
    move.l  score_to_track(pc),d1
    ; was below, check new score
    cmp.l   score(pc),d1    ; is current score above xtra life score
    bcc.b   .below        ; not yet
    ; above next extra life score
    cmp.l   previous_score(pc),d1
    bcs.b   .below
    
    add.l   #EXTRA_LIFE_PERIOD,d1
    move.l  d1,score_to_track
    
    move.w  #MSG_SHOW,extra_life_message
    addq.b   #1,nb_lives
	move.l	a0,d1	; save A0
    lea     extra_life_sound,a0
    bsr play_fx
	move.l	d1,a0	; restore A0
.below
    rts
    
random:
    move.l  previous_random(pc),d0
	;;; EAB simple random generator
    ; thanks meynaf
    mulu #$a57b,d0
    addi.l #$bb40e62d,d0
    rol.l #6,d0
    move.l  d0,previous_random
    rts

    
draw_start_screen
    bsr hide_sprites
    bsr clear_screen
    
    bsr draw_title
    
	
    lea .psb_string(pc),a0
    move.w  #48,d0
    move.w  #96,d1
    move.w  #$0F0,d2
    bsr write_color_string
    
    lea .opo_string(pc),a0
    move.w  #48+16,d0
    move.w  #116,d1
    move.w  #$0f00,d2
	
    bsr write_color_string
    lea .bp1_string(pc),a0
    move.w  #16,d0
    move.w  #148,d1
    move.w  #$0FF,d2
    bsr write_color_string
    lea .bp2_string(pc),a0
    move.w  #16,d0
    move.w  #192-24,d1
    move.w  #$FFF,d2
    bsr write_color_string
    
    rts
    
.psb_string
    dc.b    "PUSH START BUTTON",0
.opo_string:
    dc.b    "1 PLAYER ONLY",0
.bp1_string
    dc.b    "1ST BONUS AFTER 30000 PTS",0
.bp2_string
    dc.b    "AND BONUS EVERY 70000 PTS",0
    even
    
    
INTRO_Y_SHIFT=68
ENEMY_Y_SPACING = 24

draw_intro_screen
    tst.b   intro_state_change
    beq.b   .no_change
    clr.b   intro_state_change
    move.b  intro_step(pc),d0
    cmp.b   #1,d0
    beq.b   .init1
    cmp.b   #2,d0
    beq.b   .init2
    cmp.b   #3,d0
    beq.b   .init3
    bra.b   .no_change  ; should not be reached
.init1    
    bsr clear_screen
    bsr hide_sprites
    
    bsr draw_intro_maze
        
    lea    .play(pc),a0
    move.w  #96,d0
    move.w  #48-24,d1
    move.w  #$0f0,d2
    bsr write_color_string    
    bsr draw_title
    ; first update, don't draw enemies or anything as they're not initialized
    ; (draw routine is called first)
    rts
.init2
    bsr hide_sprites
    bsr clear_screen
    bsr draw_score
    ; high scores
    
    move.w  #40,d0
    move.w  #8,d1
    lea .score_ranking(pc),a0
    move.w  #$0F0,d2
    bsr     write_color_string
    
    ; write high scores & position
    move.w  #24,D1
    lea     .color_table(pc),a2
    lea     .pos_table(pc),a3
    lea     hiscore_table(pc),a4
    move.w  #9,d5
.ws
    move.w  (a2)+,d2    ; color
    move.l  (a3)+,a0
    move.w  #32,d0
    bsr write_color_string
    
    move.w  d2,d4
    move.w  #64,d0
    move.l  (a4)+,d2
    move.w  #7,d3
    bsr write_color_decimal_number
    
    move.w  d4,d2
    move.w  #120,d0
    lea .pts(pc),a0
    bsr write_color_string
    
    add.w   #16,d1
    dbf d5,.ws
    
    bra draw_copyright
    
.init3
    bsr clear_screen
    ; characters
    move.w  #56,d0
    move.w  #56-24,d1
    lea     .characters(pc),a0
    move.w  #$0F0,d2
    bsr write_color_string
    bsr hide_sprites

    ; not the same configuration as game sprites:
    ; each sprite is there simultaneously

    lea game_palette+32(pc),a0  ; the sprite part of the color palette 16-31    
    moveq.w #0,d0
    ; first sprite palette
    bsr .load_palette

    lea game_palette+32+24(pc),a0  ; we cheat, use sprite 4 with palette of 6-7
    moveq.w #2,d0
    ; thief guard sprite palette
    bsr .load_palette
    
    lea alt_sprite_palette+8(pc),a0  ; we cheat, use sprite 4 with palette of 6-7
    ; thief guard sprite palette
    moveq.w #4,d0
    bsr .load_palette
    
    bra draw_copyright
    

    ;;move.l  a3,a0
    
.no_change
    ; just draw single cattle
    move.b  intro_step(pc),d0
    cmp.b   #1,d0
    bne.b   .no_part1

    ; part 1: cattle drawing path in intro maze
    lea enemies+Enemy_SIZEOF(pc),a0
    
    move.w  xpos(a0),d0
    addq.w  #2,d0       ; compensate

    move.w  ypos(a0),d1
    add.w  #INTRO_Y_SHIFT+5,d1   ; compensate + add offset so logic coords match intro maze
    ; center => top left
    bsr store_sprite_pos

    move.l  frame_table(a0),a1
    move.w  frame(a0),d2
    lsr.w   #2,d2   ; 8 divide to get 0,1
    bclr    #0,d2   ; even
    add.w   d2,d2       ; times 2

    ; get proper frame from proper frame set
    move.l  (a1,d2.w),a1

    move.l  d0,(a1)     ; store control word
    move.l  a1,d2    
    move.l  copperlist_address(a0),a1
    move.w  d2,(6,a1)
    swap    d2
    move.w  d2,(2,a1)  
    
    ; paint is done in the update part
	; the draw part misses bits because it's updated at 50 Hz
	; where the update part is updated at 60 Hz to follow original
	; game speed
    
.no_part1
    
    cmp.b   #3,d0
    bne.b   .no_part3
    ; blit characters
    move.w  #56,d3
    move.w  #72-24,d4
    move.w  d3,d0
    move.w  d4,d1
    lea copier_anim_right,a0
    move.w  #$F,d2
    bsr .draw_bob

    add.w   #ENEMY_Y_SPACING,d4
    move.w  d4,d1
    add.w   #3,d1
    lea police1_frame_table,a0
    lea intro_green_police,a1
    move.w  #3,d2   ; 4 frames
    bsr .load_sprite
    
    move.w  d3,d0
    add.w   #ENEMY_Y_SPACING,d4
    move.w  d4,d1
    add.w   #3,d1
    lea police2_frame_table,a0
    lea thief_sprite,a1
    move.w  #3,d2   ; 4 frames
    bsr .load_sprite
    
    move.w  d3,d0
    add.w   #ENEMY_Y_SPACING,d4
    move.w  d4,d1
    
    lea rustler_anim_right,a0
    move.w  #$F,d2
    bsr .draw_bob
    
    move.w  d3,d0
    add.w   #ENEMY_Y_SPACING,d4
    move.w  d4,d1
    add.w   #3,d1
    lea cattle1_frame_table,a0
    lea intro_cattle_pink,a1
    move.w  #1,d2
    bsr .load_sprite
    
    move.w  d3,d0
    add.w   #ENEMY_Y_SPACING,d4
    move.w  d4,d1
    add.w   #3,d1
    lea cattle2_frame_table,a0
    lea intro_cyan_cattle,a1
    move.w  #1,d2
    bsr .load_sprite
    
    lea draw_char_command(pc),a1
    tst.b   (5,a1)
    beq.b   .nothing_to_print

    lea .onechar(pc),a0
    move.w  (a1)+,d0
    move.w  (a1)+,d1
    move.b  (a1)+,(a0)
    clr.b   (a1)    ; ack
    move.w  #$FF,d2
    bsr write_color_string
.nothing_to_print
    rts
    
.no_part3
; part 2 highscores
    tst.w   high_score_position
    bmi.b   .out3
    
    lea high_score_highlight_color_table(pc),a0
    move.w  high_score_highlight_color_index(pc),d0
    add.w   d0,d0
    move.w  (a0,d0.w),d2
    
    move.w  d2,d4
    move.w  #32,d0

    lea     .pos_table(pc),a3
    move.w  high_score_position(pc),d5
    add.w   d5,d5
    add.w   d5,d5
    move.l  (a3,d5.w),a0
    move.w  high_score_highlight_y(pc),d1
    bsr     write_blanked_color_string
    
    lea     hiscore_table(pc),a4
    move.l  (a4,d5.w),d2
    
    move.w  #64,d0
    move.w  #7,d3
    bsr write_blanked_color_decimal_number

    move.w  d4,d2
    move.w  #120,d0
    lea .pts(pc),a0
    bsr write_blanked_color_string

.out3
    rts
.draw_bob
    move.w intro_frame_index(pc),d6
    and.w   d2,d6
    add.w   d6,d6
    add.w   d6,d6
    move.l  (a0,d6.w),a0
    
    bsr blit_4_planes
    rts
    
.load_sprite
    bsr .get_frame
    move.l  a0,d2
    move.w  d2,(6,a1)
    swap    d2
    move.w  d2,(2,a1)
    bsr store_sprite_pos
    move.l  d0,(a0)
    
    rts
.get_frame
    move.w intro_frame_index(pc),d6
    lsr.w   #3,d6
    and.w   d2,d6
    add.w   d6,d6
    add.w   d6,d6
    move.l  (a0,d6.w),a0
    rts
    
.load_palette
    lea _custom+color+32,a1
    lsr.w   #1,d0
    lsl.w   #3,d0
    add.w   d0,a1

    move.l  (a0,d0.w),(a1)+
    move.l  (4,a0,d0.w),(a1)
    rts


.color_table
    dc.w    $0FF,$0FF,$FFF,$FFF,$FF0,$FF0,$0F0,$0F0,$F00,$F00
.pos_table  
    dc.l    .pos1
    dc.l    .pos2
    dc.l    .pos3
    dc.l    .pos4
    dc.l    .pos5
    dc.l    .pos6
    dc.l    .pos7
    dc.l    .pos8
    dc.l    .pos9
    dc.l    .pos10
    

.onechar
    dc.b    0,0
.toggle
    dc.b    0
.characters
    dc.b    "-  CHARACTER  -",0
.play
    dc.b    'PLAY',0
.pts
    dc.b    "0 PTS  hhh",0
    
.pos1
    dc.b    "1ST",0
.pos2
    dc.b    "2ND",0
.pos3
    dc.b    "3RD",0
.pos4
    dc.b    "4TH",0
.pos5
    dc.b    "5TH",0
.pos6
    dc.b    "6TH",0
.pos7
    dc.b    "7TH",0
.pos8
    dc.b    "8TH",0
.pos9
    dc.b    "9TH",0
.pos10
    dc.b    "10TH",0
    
.score_ranking
    dc.b    "- SCORE RANKING -",0
    even

high_score_position
    dc.w    0
high_score_highlight_y
    dc.w    0
high_score_highlight_timer
    dc.w    0
high_score_highlight_color_index
    dc.w    0
high_score_highlight_color_table
    dc.w    $0FF
    dc.w    $0F0
    dc.w    $FF0
    dc.w    $FFF
high_score
    dc.l    DEFAULT_HIGH_SCORE
	dc.l	$DEADBEEF
hiscore_table:
    REPT    NB_HIGH_SCORES
	IFD		HIGHSCORES_TEST
    dc.l    (DEFAULT_HIGH_SCORE/10)*(10-REPTN)   ; decreasing score for testing	
	ELSE
    dc.l    DEFAULT_HIGH_SCORE
	ENDC
    ENDR
	dc.l	$DEADBEEF

draw_char_command
    dc.w    0,0 ; X,Y
    dc.b    0   ; char
    dc.b    0   ; command set (0: no, $FF: yes)
intro_frame_index
    dc.w    0
intro_step
    dc.b    0
intro_state_change
    dc.b    0
    even
    
draw_title
    lea    .alt_title(pc),a0
	tst.w	demo_level_number
	beq.b	.w
    lea    .title(pc),a0
.w
    move.w  #64,d0
    move.w  #72-24,d1
    move.w  #$0ff0,d2
    bsr write_color_string 
    bra.b   draw_copyright

.title
    dc.b    '-  AMIDAR  -',0
.alt_title
    dc.b    '-  AMIGAR  -',0
    even
draw_copyright
    lea    .copyright(pc),a0
    move.w  #64,d0
    move.w  #222-24,d1
    move.w  #$0fff,d2
    bra write_color_string    
.copyright
    dc.b    'c KONAMI  1982',0
    even

; what: clears a plane of any width (not using blitter, no shifting, start is multiple of 8), 16 height
; args:
; < A1: dest (must be even)
; < D0: X (multiple of 8)
; < D1: Y
; < D2: blit width in bytes (even, 2 must be added same interface as blitter)
; trashes: none

clear_plane_any_cpu
    move.w  d3,-(a7)
    move.w  #16,d3
    bsr     clear_plane_any_cpu_any_height
    move.w  (a7)+,d3
    rts
    
clear_plane_any_cpu_any_height 
    movem.l d0-D3/a0-a2,-(a7)
    subq.w  #1,d3
    bmi.b   .out
    lea mul40_table(pc),a2
    add.w   d1,d1
    beq.b   .no_add
    move.w  (a2,d1.w),d1
    add.w   d1,a1
.no_add

    lsr.w   #3,d0
    add.w   d0,a1
	move.l	a1,d1
    btst    #0,d1
    bne.b   .odd
    cmp.w   #4,d2
    bcs.b   .odd
	btst	#0,d2
	bne.b	.odd
	btst	#1,d2
	beq.b	.even
.odd    
    ; odd address
    move.w  d3,d0
    subq.w  #1,d2
.yloop
    move.l  a1,a0
    move.w  d2,d1   ; reload d1
.xloop
    clr.b   (a0)+
    dbf d1,.xloop
    ; next line
    add.w   #NB_BYTES_PER_LINE,a1
    dbf d0,.yloop
.out
    movem.l (a7)+,d0-D3/a0-a2
    rts

.even
    ; even address, big width: can use longword erase
    move.w  d3,d0
    lsr.w   #2,d2
    subq.w  #1,d2
.yloop2
    move.l  a1,a0
    move.w  d2,d1
.xloop2
    clr.l   (a0)+
    dbf d1,.xloop2
    ; next line
    add.w   #NB_BYTES_PER_LINE,a1
    dbf d0,.yloop2
    bra.b   .out
    
; what: clears a plane of any width (using blitter), 16 height
; args:
; < A1: dest
; < D0: X (not necessarily multiple of 8)
; < D1: Y
; < D2: rect width in bytes (2 is added)
; trashes: none
    
clear_plane_any_blitter:
    movem.l d0-d6/a1/a5,-(a7)
    lea _custom,a5
    moveq.l #-1,d3
    move.w  #16,d4
    bsr clear_plane_any_blitter_internal
    movem.l (a7)+,d0-d6/a1/a5
    rts


;; C version
;;   UWORD minterm = 0xA;
;;
;;    if (mask_base) {
;;      minterm |= set_bits ? 0xB0 : 0x80;
;;    }
;;    else {
;;      minterm |= set_bits ? 0xF0 : 0x00;
;;    }
;;
;;    wait_blit();
;;
;;    // A = Mask of bits inside copy region
;;    // B = Optional bitplane mask
;;    // C = Destination data (for region outside mask)
;;    // D = Destination data
;;    custom.bltcon0 = BLTCON0_USEC | BLTCON0_USED | (mask_base ? BLTCON0_USEB : 0) | minterm;
;;    custom.bltcon1 = 0;
;;    custom.bltbmod = mask_mod_b;
;;    custom.bltcmod = dst_mod_b;
;;    custom.bltdmod = dst_mod_b;
;;    custom.bltafwm = left_word_mask;
;;    custom.bltalwm = right_word_mask;
;;    custom.bltadat = 0xFFFF;
;;    custom.bltbpt = (APTR)mask_start_b;
;;    custom.bltcpt = (APTR)dst_start_b;
;;    custom.bltdpt = (APTR)dst_start_b;
;;    custom.bltsize = (height << BLTSIZE_H0_SHF) | width_words;
;;  }
  
; < A5: custom
; < D0,D1: x,y
; < A1: plane pointer
; < D2: width in bytes (inc. 2 extra for shifting)
; < D3: blit mask
; < D4: blit height
; trashes D0-D6
; > A1: even address where blit was done
clear_plane_any_blitter_internal:
    ; pre-compute the maximum of shit here
    lea mul40_table(pc),a2
    add.w   d1,d1
    beq.b   .d1_zero    ; optim
    move.w  (a2,d1.w),d1
    swap    d1
    clr.w   d1
    swap    d1
.d1_zero
    move.l  #$030A0000,d5   ; minterm useC useD & rect clear (0xA) 
    move    d0,d6
    beq.b   .d0_zero
    and.w   #$F,d6
    and.w   #$1F0,d0
    lsr.w   #3,d0
    add.w   d0,d1

    swap    d6
    clr.w   d6
    lsl.l   #8,d6
    lsl.l   #4,d6
    or.l    d6,d5            ; add shift
.d0_zero    
    add.l   d1,a1       ; plane position (always even)

	move.w #NB_BYTES_PER_LINE,d0
    sub.w   d2,d0       ; blit width

    lsl.w   #6,d4
    lsr.w   #1,d2
    add.w   d2,d4       ; blit height


    ; now just wait for blitter ready to write all registers
	bsr	wait_blit
    
    ; blitter registers set
    move.l  d3,bltafwm(a5)
	move.l d5,bltcon0(a5)	
    move.w  d0,bltdmod(a5)	;D modulo
	move.w  #-1,bltadat(a5)	;source graphic top left corner
	move.l a1,bltcpt(a5)	;destination top left corner
	move.l a1,bltdpt(a5)	;destination top left corner
	move.w  d4,bltsize(a5)	;rectangle size, starts blit
    rts

    
    
delete_last_star
    move.b  nb_stars(pc),d7
    ext     d7
    lea	screen_data+STARS_OFFSET,a1
    add.w   d7,a1
    moveq   #3,d2
.ploop
    move.l  a1,a2
    REPT    8
    clr.b   (a2)
    add.w   #NB_BYTES_PER_LINE,a2
    ENDR
    add.w   #SCREEN_PLANE_SIZE,a1
    dbf     d2,.ploop    
    rts
    
draw_stars:
    move.b  nb_stars(pc),d7
    subq.b  #1,d7
    ext     d7    
.lloop
    lea star,a0
    lea	screen_data+STARS_OFFSET,a1
    add.l   d7,a1
    moveq   #3,d2
.ploop
    move.l  a1,a2
    REPT    8
    move.b  (a0)+,(a2)
    add.w   #NB_BYTES_PER_LINE,a2
    ENDR
    add.w   #SCREEN_PLANE_SIZE,a1
    dbf     d2,.ploop
    dbf d7,.lloop
.out
    lea     .jump(pc),a0
    move.w  #(NB_BYTES_PER_MAZE_LINE-9)*8,d0
    move.w  #MAZE_HEIGHT+18,d1
    move.w  #$0F0,d2
    bsr     write_color_string
    rts
    
.jump
        dc.b    "JUMP",0
        even
        
LIVES_OFFSET = (MAZE_HEIGHT+18)*NB_BYTES_PER_LINE+1

draw_last_life
    move.w   #1,d0      ; draw only last life
    bra.b   draw_the_lives
    
draw_lives:
    moveq.w #3,d7
    lea	screen_data+LIVES_OFFSET,a1
.cloop
    moveq.l #0,d0
    moveq.l #0,d1
    move.l  #12,d2
    bsr clear_plane_any_cpu
    add.w   #SCREEN_PLANE_SIZE,a1
    dbf d7,.cloop
    
    clr D0
	
draw_the_lives
    move.b  nb_lives(pc),d7
    ext     d7
    subq.w  #2,d7
    bmi.b   .out
	cmp.w	#8,d7
	bcs.b	.lloop
	move.w	#8,d7	; no more than 8 lives displayed
.lloop
    lea lives,a0
    lea	screen_data+LIVES_OFFSET,a1
    add.w   d7,a1
    moveq   #3,d2    
.ploop
    move.l  a1,a2
    REPT    8
    move.b  (a0)+,(a2)
    add.w   #NB_BYTES_PER_LINE,a2
    ENDR
    add.w   #SCREEN_PLANE_SIZE,a1
    dbf     d2,.ploop
    tst d0
    bne.b   .out    ; just draw last life
    dbf d7,.lloop
.out
    rts
    
draw_bonuses:
    move.w #NB_BYTES_PER_MAZE_LINE*8,d0
    move.w #248-32,d1
    move.w  level_number(pc),d2
    cmp.w   #6,d2
    bcs.b   .ok
    move.w  #6,d2 
.ok
    move.w  #1,d4
.dbloopy
    move.w  #5,d3
.dbloopx
    ;;bsr draw_bonus
    subq.w  #1,d2
    bmi.b   .outb
    add.w   #16,d0
    dbf d3,.dbloopx
    move.w #NB_BYTES_PER_MAZE_LINE*8,d0
    add.w   #16,d1
    dbf d4,.dbloopy
.outb
    rts
    
maze_misc
    dc.l    level_1_maze,level_2_maze
    dc.l    level_3_maze,level_4_maze
    
level_1_maze
    dc.w    $F00,$CC9,$00F
level_2_maze
    dc.w    $0F0,$FF0,$F00
level_3_maze
    dc.w    $0F0,$f91,$F0F
level_4_maze
    dc.w    $F00,$FF0,$0F0
    
draw_maze:
    bsr wait_blit
    
    ; set colors
    ; the trick with dots is to leave them one plane 1 alone
    ; when the bits intersect with maze lines, we get the same color
    ; because the color entry is duplicated
    ;
    ; this allows to blit main character on planes 0, 2, 3 without any interaction
    ; (except very marginal visual color change) on plane 1
    lea _custom+color,a0
    move.w  level_number(pc),d0
    and.w   #3,d0
    add.w   d0,d0
    add.w   d0,d0
    lea  maze_misc(pc),a1
    move.l  (a1,d0.w),a1
    
    move.w  (a1)+,(2,a0)  ; dots, color 1
	move.w  (a1)+,d0
    move.w  d0,(4,a0)  ; dots 
    move.w  d0,(6,a0)  ; dots+outline
	move.w  (a1),(10,a0)    ; rect fill color

    ;;move.b  (1,a1),total_number_of_dots
    
    bsr clear_playfield_planes
    
    lea screen_data+MAZE_ADDRESS_OFFSET,a1
    
    st.b  (NB_BYTES_PER_LINE*(MAZE_HEIGHT+1),a1)
    st.b  (NB_BYTES_PER_LINE*MAZE_HEIGHT,a1)
    st.b  (NB_BYTES_PER_LINE,a1)
    st.b  (a1)+

    ; draw frame
    move.w  #NB_BYTES_PER_MAZE_LINE-3,d1
.hloop
    st.b  (NB_BYTES_PER_LINE*(MAZE_HEIGHT+1),a1)
    st.b  (NB_BYTES_PER_LINE*MAZE_HEIGHT,a1)
    st.b  (NB_BYTES_PER_LINE,a1)
    st.b  (a1)+
    dbf     d1,.hloop
    ; rightmost end, 2 bits not drawn
    move.b  #$C0,d0
    move.b  d0,(NB_BYTES_PER_LINE*(MAZE_HEIGHT+1),a1)
    move.b  d0,(NB_BYTES_PER_LINE*MAZE_HEIGHT,a1)
    move.b  d0,(NB_BYTES_PER_LINE,a1)
    move.b  d0,(a1)
    
    ; vertical edges
    lea screen_data+MAZE_ADDRESS_OFFSET,a1
    move.w  #MAZE_HEIGHT-1,d1
    bsr draw_maze_vertical_edges
    
     lea screen_data+MAZE_ADDRESS_OFFSET,a1
   ; horizontal separations
    tst.b   rustler_level
    bne.b   .scores
    ; level 1
    lea maze_1_vertical_table(pc),a0
    bsr draw_maze_horizontal_lines
    bra.b   .noscores
.scores    
   
    ; we need the height of the rects to center the scores
    lea maze_2_vertical_table,a0
    bsr draw_maze_horizontal_lines
    
    lea rectlist_2,a3 ; rects     
    lea maze_2_vertical_table,a0
    move.w  #16,d0      ; start X
.seploop
    
    moveq   #0,d1
    move.b  (a0)+,d1
    bpl.b   .cont
    cmp.b   #-2,d1
    beq.b   .out
    add.w   #40,d0
    bra.b   .seploop
.cont
    move.l  (a3)+,a4
    ; draw score
    move.w  hrect(a4),d2
    lsl.w   #3,d1   ; times 8
    lsl.w   #2,d2   ; times 4
    add.w   d2,d1
    add.w   #4,d1   ; offset 8 & subtract half font size
    
    moveq.l #0,d2
    move.w  points(a4),d2
    move.w  #0,d3
    move.w  #$0e00,d4    ; second entry in palette, but not the actual color
    move.w  d0,d5
    move.l  a0,-(a7)
    bsr write_color_decimal_number    
    move.w  d5,d0
    add.w   #16,d0
    lea .zero(pc),a0
    move.w  d4,d2
    bsr write_color_string
    move.l  (a7)+,a0
    move.w  d5,d0
    
    bra.b   .seploop
.out
  
.noscores
    ; backup this plane so we can restore background
    lea grid_backup_plane,a1
    move.l  #SCREEN_PLANE_SIZE/4-1,d0
    lea screen_data,a0
.bup
    move.l  (a0)+,(a1)+
    dbf d0,.bup
    ; clear this plane at start, there's nothing drawn yet
    ; (when game is running, this holds the second plane of the filled
    ; rectangles)
    lea rect_backup_plane,a1
    move.l  #SCREEN_PLANE_SIZE/4-1,d0
.bup2
    clr.l  (a1)+
    dbf d0,.bup2
    tst.b   rustler_level
    beq.b   .no_clr
    ; rustler (paint) level needs full 4-plane blit & restore
    ; other level can manage to avoid that plane because character uses 3 other planes
    ; thus preserving the dots and saving the need for saving/restoring them

    ; clear this plane at start, there's nothing drawn yet
    lea paint_backup_plane,a1
    move.l  #SCREEN_PLANE_SIZE/4-1,d0
.bup3
    clr.l  (a1)+
    dbf d0,.bup3
    ; but paint the segment where the player is
    move.w  #88,d0
    move.w  #Y_MAX+6,d1
    bsr     draw_horizontal_segment
    
    
    
.no_clr
    rts    
.zero
    dc.b    "0",0
    even

; draw horiz segment in screen & back buffer
; < D0: X start
; < D1: Y start

draw_horizontal_segment
    movem.l d0-d4/a0-a3,-(a7)
    lea mul40_table(pc),a2
    lsr.w   #3,d0   ; 8 divide
    
    ; draw horizontal separation
    add.w   d1,d1
    move.w  (a2,d1.w),d1    ; times40
    add.w   d0,d1           ; plus X

    lea paint_backup_plane,a1
    move.w  d1,d4
    bsr .draw
    move.w  d4,d1
    lea screen_data+SCREEN_PLANE_SIZE,a1
    bsr .draw
        
    movem.l (a7)+,d0-d4/a0-a3
    rts
.draw
    st.b    (A1,d1)
    st.b    (1,A1,d1)
    st.b    (2,A1,d1)
    st.b    (3,A1,d1)
    st.b    (4,A1,d1)
    add.w   #NB_BYTES_PER_LINE,d1
    st.b    (A1,d1)
    st.b    (1,A1,d1)
    st.b    (2,A1,d1)
    st.b    (3,A1,d1)
    st.b    (4,A1,d1)
    rts
    
draw_intro_maze:
    bsr wait_blit
    
    ; set colors
    ; the trick with dots is to leave them one plane 1 alone
    ; when the bits intersect with maze lines, we get the same color
    ; because the color entry is duplicated
    ;
    ; this allows to blit main character on planes 0, 2, 3 without any interaction
    ; (except very marginal visual color change) on plane 1
    lea _custom+color,a0
    
    move.w  #$0F0,(2,a0)  ; green outline
    move.w  #$FF0,(6,a0)  ; painted outline
;	move.w  (a1)+,d0
;    move.w  d0,(4,a0)  ; dots 
;    move.w  d0,(6,a0)  ; dots+outline

    ;;move.b  (1,a1),total_number_of_dots
    
    lea screen_data,a1
    bsr clear_playfield_plane
    add.w   #SCREEN_PLANE_SIZE*2,a1
    bsr clear_playfield_plane
    add.w   #SCREEN_PLANE_SIZE,a1
    bsr clear_playfield_plane
        
    ; vertical edges
    lea screen_data+INTRO_MAZE_ADDRESS_OFFSET,a1
    move.w  #INTRO_MAZE_HEIGHT-1,d1

    bsr draw_maze_vertical_edges
   
    ; horizontal separations
    lea maze_intro_vertical_table(pc),a0
    lea screen_data+INTRO_MAZE_ADDRESS_OFFSET+4*NB_BYTES_PER_LINE,a1
    bsr draw_maze_horizontal_lines
    
    rts    

draw_bonus_maze:
    bsr wait_blit
    
    ; set colors
    ; the trick with dots is to leave them one plane 1 alone
    ; when the bits intersect with maze lines, we get the same color
    ; because the color entry is duplicated
    ;
    ; this allows to blit main character on planes 0, 2, 3 without any interaction
    ; (except very marginal visual color change) on plane 1
    lea _custom+color,a0
    
    move.w  #$F00,(2,a0)  ; red outline
    move.w  #$FF0,(6,a0)  ; yellow fill

    bsr clear_playfield_planes        
        
    ; vertical edges
    lea screen_data+BONUS_MAZE_ADDRESS_OFFSET,a1
    move.w  #MAZE_HEIGHT-9-8,d1

    bsr draw_maze_vertical_edges
   
    ; horizontal separations
    move.l  bonus_vertical_table(pc),a0
    lea screen_data+BONUS_MAZE_ADDRESS_OFFSET-12*NB_BYTES_PER_LINE,a1
    bsr draw_maze_horizontal_lines

    ; I realise now that
    ; handling of palette is not super good all throughout the game
    ; that last way is pretty good: using palette parts
    ; to load sprite palettes, instead of hardcoding or
    ; copying in bulk, which isn't compatible with bonus parts
    ; or intro parts which don't respect the game palette logic
    ; (guard/cattle)
    
    lea _custom+color+32,a1  ; sprite 0-1 colors
    lea banana_sprite_palette(pc),a0
    move.l  (a0)+,(a1)+
    move.l  (a0)+,(a1)+
    lea cattle_sprite_palette(pc),a0    ; sprite 2-3 colors
    move.l  (a0)+,(a1)+
    move.l  (a0)+,(a1)+
    
    ; banana, we're going to use a sprite
    move.w  banana_x(pc),d0
    move.w  #199,d1
    bsr store_sprite_pos

    ; write control word
    lea banana_sprite,a0
    move.l  d0,(a0)
    move.l  a0,d0
    lea bonus_banana,a0
    bsr store_sprite_copperlist

    bsr draw_lives
    bsr draw_stars

    lea enemies+Enemy_SIZEOF(pc),a0
    
    move.w   #-1,previous_xpos(a0)
    move.w   #-1,previous_ypos(a0)
    rts    


; used for all mazes
draw_maze_vertical_edges
    move.b  #$60<<1,d0
.vloop
    or.b    d0,(a1)
    or.b    d0,(5,a1)
    or.b    d0,(10,a1)
    or.b    d0,(15,a1)
    or.b    d0,(20,a1)
    or.b    d0,(NB_BYTES_PER_MAZE_LINE-1,a1)
    add.w  #NB_BYTES_PER_LINE,a1
    dbf     d1,.vloop
    rts
    
draw_maze_horizontal_lines:
    lea mul40_table(pc),a2
    moveq   #-1,d2
    move.b  #$7F,d3
.seploop
    moveq   #0,d1
    move.b  (a0)+,d1
    bpl.b   .cont
    cmp.b   #-2,d1
    beq.b   .out
    add.w   #5,a1
    bra.b   .seploop
.cont
    ; draw horizontal separation
    lsl.w   #2,d1
    move.w  (a2,d1.w),d1    ; times40
    lsl.w   #2,d1           ; times4
    
    or.b    d3,(A1,d1)
    or.b    d2,(1,A1,d1)
    or.b    d2,(2,A1,d1)
    or.b    d2,(3,A1,d1)
    or.b    d2,(4,A1,d1)
    or.b    #$80,(5,A1,d1)
    add.w   #NB_BYTES_PER_LINE,d1
    or.b    d3,(A1,d1)
    or.b    d2,(1,A1,d1)
    or.b    d2,(2,A1,d1)
    or.b    d2,(3,A1,d1)
    or.b    d2,(4,A1,d1)
    or.b    #$80,(5,A1,d1)
    bra.b   .seploop
.out
    rts

store_sprite_copperlist    
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)
    rts


   ; copy the maze initial data that is destroyed when playing
    lea     maze_2_wall_table,a1
   lea     maze_wall_table_copy,a0
    move.l  a0,maze_wall_table
    move.w  #(NB_TILES_PER_LINE*NB_TILE_LINES)-1,d0
.copy
    move.b  (a1)+,(a0)+
    dbf     d0,.copy
    
init_dots:
    ; init dots
    move.l  #maze_1_dot_table_read_only,dot_table
    lea     maze_1_wall_table,a0
copy_maze_data
    lea     maze_wall_table_copy,a1
    move.l  a1,maze_wall_table
    move.l  #NB_TILE_LINES*NB_TILES_PER_LINE-1,d0
.copy
    move.b  (a0)+,(a1)+
    dbf d0,.copy
    rts
    

init_paint:
    ; init dots
    move.l  #maze_2_dot_table_read_only,dot_table
    lea     maze_2_wall_table,a0
    bra     copy_maze_data
    
    
draw_dots:
    lea screen_data+MAZE_ADDRESS_OFFSET+SCREEN_PLANE_SIZE-NB_BYTES_PER_LINE,a2
    lea maze_1_dot_table_read_only,a0
    move.w  #NB_TILE_LINES-1,d1
.vloop
    move.l  a2,a1
    ; the table contains 4 pointers on containing rectangles for each dot
    ; on the board, there can be max 3 containing rectangles because rectangles
    ; aren't aligned but 16 bytes per dot is simpler to multiply by
    move.w  #NB_BYTES_PER_MAZE_LINE-1,d0
.hloop
    tst.l  (a0)+
    beq.b   .no_draw
    bsr draw_dot
.no_draw
    add.w  #12,a0
    
    addq.w  #1,a1
    dbf d0,.hloop
    add.w   #NB_BYTES_PER_LINE*8,a2
    dbf d1,.vloop
    rts


    

draw_dot:
    move.b  #%11000000,(a1)
    move.b  #%11100000,(NB_BYTES_PER_LINE,a1)
    move.b  #%11100000,(NB_BYTES_PER_LINE*2,a1)
    move.b  #%11000000,(NB_BYTES_PER_LINE*3,a1)
    or.b  #1,(NB_BYTES_PER_LINE-1,a1)
    or.b  #1,(NB_BYTES_PER_LINE*2-1,a1)
    rts

; < A1 address
; < D3 direction
; < D4 previous direction

paint_zone:
	clr.b	corner_sideeffect_paint
    ; just copy the data of the backup plane
    ; for this we have to compute what is A1 address offset
    ; compared to second plane
    ; probably faster than recomputing that from X,Y
    movem.l d0-d5/a0/a2,-(a7)
    move.l  rollback_paint_zone_pointer(pc),a0
    move.l  a1,(a0)+
    move.w  d3,(a0)+

    move.l  a0,rollback_paint_zone_pointer
    
    bsr     prepare_paint_zone
    ; > A0,A2,D2,D5 set
	
.loop
    move.b  (a0),d0
    cmp.w #LEFT,d3
    beq.b   .horiz
    cmp.w #RIGHT,d3
    beq.b   .horiz
	; vertical
    and.b   #$C0,d0 ; remove horizontal rightmost pixels	
.horiz

	; workaround for remaining unpainted bottom right corner (2x2 square)
	; because when painter turns this area is neither handled by horizontal
	; paint nor vertical paint. Doing so paints too much in other cases
	; this kludge works: check if there's a painted segment on the bottom left
	
	cmp.b	#-1,(-1,a0,d5.w) ; check grid horizontal full line
	bne.b	.no_extra_paint
	tst.b	(-1,a1,d5.w)
	beq.b	.no_extra_paint
	or.b	#$C0,(a1,d5.w)
	or.b	#$C0,(a2,d5.w)
	; set a flag to avoid unpainting too much if kludge wasn't applied
	; it works properly because rollback unpaint is done in reverse order
	; (else it could break the side effect and unpaint some other corner)
	st.b	corner_sideeffect_paint
.no_extra_paint
    or.b  d0,(a1)
    or.b  d0,(a2)


    add.w   d4,a0
    add.w   d4,a1
    add.w   d4,a2
    dbf     d2,.loop
    movem.l (a7)+,d0-d5/a0/a2
    rts
   
; < A1: screen destination address   
; < D3: direction
; < D4: previous direction
unpaint_zone:
    ; just copy the data of the backup plane
    ; for this we have to compute what is A1 address offset
    ; compared to second plane
    ; probably faster than recomputing that from X,Y
    movem.l d0/d4-d5/a0/a2,-(a7)
    bsr prepare_paint_zone
	; out A1: screen
	; out A2: paint backup plane
.loop
    move.b  (a0),d0
	not.b	d0
	
    cmp.w   #LEFT,d3
    beq.b   .horiz
    cmp.w   #RIGHT,d3
    beq.b   .horiz
    and.b   #$0f,d0 ; remove horizontal bar (negated)
.horiz
	; special case to consider if rollbacking with left direction
	; (painting has been done from right, remember)
	; we don't want to remove paint from the vertical painted segment
	; if it's already painted
    cmp.w   #RIGHT,d3
    bne.b   .no_right
	; checking if above or below is painted. If painted, don't
	; clear the 2x2 pixels of the intersection
	btst	#6,(NB_BYTES_PER_LINE*2,a2)
	bne.b	.enhance_mask
	btst	#6,(-NB_BYTES_PER_LINE*2,a2)
	beq.b	.no_right
.enhance_mask
	or.b	#$E0,d0
.no_right
	; this removes the 2x2 corner too many times
	tst.b	corner_sideeffect_paint
	beq.b	.no_extra_unpaint
	cmp.b	#-1,(-1,a0,d5.w) ; check grid horizontal full line
	bne.b	.no_extra_unpaint
	tst.b	(-1,a1,d5.w)
	beq.b	.no_extra_unpaint
	clr.b	corner_sideeffect_paint
	and.b	#$3F,(a1,d5.w)
	and.b	#$3F,(a2,d5.w)
	and.b	#$3F,(NB_BYTES_PER_LINE,a1,d5.w)
	and.b	#$3F,(NB_BYTES_PER_LINE,a2,d5.w)
.no_extra_unpaint
    and.b  d0,(a1)
    and.b  d0,(a2)
    add.w   d4,a0
    add.w   d4,a1
    add.w   d4,a2
    dbf     d2,.loop
    movem.l (a7)+,d0/d4-d5/a0/a2
    rts
    
; unpaint/paint register shared computations
prepare_paint_zone
    move.l  a1,d0
    lea     screen_data,a0
    sub.l   a0,d0
    sub.l   #SCREEN_PLANE_SIZE,d0
    ; now D0 is the offset of any plane
    lea     grid_backup_plane,a0
    add.l   d0,a0       ; offset of data to copy
    lea     paint_backup_plane,a2
    add.l   d0,a2
    cmp.w   #LEFT,d3
    beq.b   .horiz
    cmp.w   #RIGHT,d3
    beq.b   .horiz
	; vertical
    moveq.w #7,d2
    bra.b   .cont
.horiz
    moveq.w #1,d2
.cont
	; the magic constants
    move.w  #NB_BYTES_PER_LINE,d4
	move.w	#NB_BYTES_PER_LINE*2,d5
    rts
    
; < A1 address
clear_dot
    REPT    6
    and.b  #$1,(NB_BYTES_PER_LINE*REPTN,a1)
    bclr.b  #0,(NB_BYTES_PER_LINE*REPTN-1,a1)
    ENDR
    
    rts

        
init_sound
    ; init phx ptplayer, needs a6 as custom, a0 as vbr (which is zero)
    sub.l   a0,a0
    moveq.l #1,d0
    lea _custom,a6
    jsr _mt_install_cia
    rts
    
init_interrupts
    lea _custom,a6
    sub.l   a0,a0

    move.w  (dmaconr,a6),saved_dmacon
    move.w  (intenar,a6),saved_intena

    sub.l   a0,a0
    ; assuming VBR at 0
    lea saved_vectors(pc),a1
    move.l  ($8,a0),(a1)+
    move.l  ($c,a0),(a1)+
    move.l  ($10,a0),(a1)+
    move.l  ($68,a0),(a1)+
    move.l  ($6C,a0),(a1)+

    lea   exc8(pc),a1
    move.l  a1,($8,a0)
    lea   excc(pc),a1
    move.l  a1,($c,a0)
    lea   exc10(pc),a1
    move.l  a1,($10,a0)
    
    lea level2_interrupt(pc),a1
    move.l  a1,($68,a0)
    
    lea level3_interrupt(pc),a1
    move.l  a1,($6C,a0)
    
    
    rts
    
exc8
    lea .bus_error(pc),a0
    bra.b lockup
.bus_error:
    dc.b    "BUS ERROR AT",0
    even
excc
    lea .linea_error(pc),a0
    bra.b lockup
.linea_error:
    dc.b    "LINEA ERROR AT",0
    even

exc10
    lea .illegal_error(pc),a0
    bra.b lockup
.illegal_error:
    dc.b    "ILLEGAL INSTRUCTION AT",0
    even

lockup
    move.l  (2,a7),d3
    move.w  #$FFF,d2
    clr.w   d0
    clr.w   d1
    bsr write_color_string

    lsl.w   #3,d0
    lea screen_data,a1
    move.l  d3,d2
    moveq.w #8,d3
    bsr write_hexadecimal_number    
.lockup
    bra.b   .lockup
finalize_sound
    bsr stop_sounds
    ; assuming VBR at 0
    sub.l   a0,a0
    lea _custom,a6
    jsr _mt_remove_cia
    move.w  #$F,dmacon(a6)   ; stop sound
    rts
    
restore_interrupts:
    ; assuming VBR at 0
    sub.l   a0,a0
    
    lea saved_vectors(pc),a1
    move.l  (a1)+,($8,a0)
    move.l  (a1)+,($c,a0)
    move.l  (a1)+,($10,a0)
    move.l  (a1)+,($68,a0)
    move.l  (a1)+,($6C,a0)


    lea _custom,a6

    move.w  saved_dmacon,d0
    bset    #15,d0
    move.w  d0,(dmacon,a6)
    move.w  saved_intena,d0
    bset    #15,d0
    move.w  d0,(intena,a6)


    rts
    
saved_vectors
        dc.l    0,0,0   ; some exceptions
        dc.l    0   ; keyboard
        dc.l    0   ; vblank
        dc.l    0   ; cia b
saved_dmacon
    dc.w    0
saved_intena
    dc.w    0

; what: level 2 interrupt (keyboard)
; args: none
; trashes: none
;
; cheat keys
; F1: skip level
; F2: toggle invincibility
; F3: toggle infinite lives
; F4: show debug info
; F5: toggle power sequence
; F6: make power sequence longer
; F8: dump maze dot data (whdload only)
; F9: thief attacks now

level2_interrupt:
	movem.l	D0/A0/A5,-(a7)
	LEA	$00BFD000,A5
	MOVEQ	#$08,D0
	AND.B	$1D01(A5),D0
	BEQ	.nokey
	MOVE.B	$1C01(A5),D0
	NOT.B	D0
	ROR.B	#1,D0		; raw key code here
    
    lea keyboard_table(pc),a0
    and.w   #$FF,d0
    bclr    #7,d0
    seq (a0,d0.w)       ; updates keyboard table
    bne.b   .no_playing     ; we don't care about key release
    ; cheat key activation sequence
    move.l  cheat_sequence_pointer(pc),a0
    cmp.b   (a0)+,d0
    bne.b   .reset_cheat
    move.l  a0,cheat_sequence_pointer
    tst.b   (a0)
    bne.b   .cheat_end
    move.w  #$0FF,_custom+color    
    st.b    cheat_keys
	; in case cheat is enabled after a legit hiscore
	clr.b	highscore_needs_saving
.reset_cheat
    move.l  #cheat_sequence,cheat_sequence_pointer
.cheat_end
    
    cmp.b   #$45,d0
    bne.b   .no_esc
    cmp.w   #STATE_INTRO_SCREEN,current_state
    beq.b   .no_esc
    cmp.w   #STATE_GAME_START_SCREEN,current_state
    beq.b   .no_esc
    move.l  #ORIGINAL_TICKS_PER_SEC*2,state_timer
    move.w  #STATE_GAME_OVER,current_state
.no_esc
    
    cmp.w   #STATE_PLAYING,current_state
    bne.b   .no_playing
    tst.b   demo_mode
    bne.b   .no_pause
    cmp.b   #$19,d0
    bne.b   .no_pause
	; in that game we need pause even if music
	; is playing, obviously
;    tst.b   music_playing
;    bne.b   .no_pause
    bsr	toggle_pause
.no_pause
    tst.w   cheat_keys
    beq.b   .no_playing
        
    cmp.b   #$50,d0
    bne.b   .no_lskip
    bsr     level_completed
.no_lskip
    cmp.b   #$51,d0
    bne.b   .no_invincible
    eor.b   #1,invincible_cheat_flag
    move.b  invincible_cheat_flag(pc),d0
    beq.b   .x
    move.w  #$F,d0
.x
    and.w   #$FF,d0
    or.w  #$0F0,d0
    move.w  d0,_custom+color
    bra.b   .no_playing
.no_invincible
    cmp.b   #$52,d0
    bne.b   .no_infinite_lives
    eor.b   #1,infinite_lives_cheat_flag
    move.b  infinite_lives_cheat_flag(pc),d0
    beq.b   .y
    move.w  #$F,d0
.y
    and.w   #$FF,d0
    or.w  #$0F0,d0
    move.w  d0,_custom+color
    bra.b   .no_playing
.no_infinite_lives
    cmp.b   #$53,d0     ; F4
    bne.b   .no_debug
    ; show/hide debug info
    eor.b   #1,debug_flag
    ; clear left part of white plane screen
    bsr     clear_debug_screen
    bra.b   .no_playing
.no_debug
    cmp.b   #$54,d0     ; F5
    bne.b   .no_bonus
    ; activate the "power pill" sequence or shut it
    tst.b  enemies+Enemy_SIZEOF+fright_mode
    bne.b   .shut_power
    move.w  #1,can_eat_enemies_mode_pending
    bra.b   .no_playing
.shut_power
	move.w	#1,power_state_counter
.no_bonus
    cmp.b   #$55,d0     ; F6
    bne.b   .no_longer_bonus
    ; make the "power pill" sequence longer only if power mode
    tst.w     power_state_counter
    beq.b   .no_longer_bonus
    move.w  #POWER_STATE_LENGTH,power_state_counter
    bra.b   .no_playing
.no_longer_bonus
    cmp.b   #$56,d0     ; F7
    bne.b   .no_add_to_score
	move.w	#500,d0
	bsr		add_to_score
.no_add_to_score
    cmp.b   #$57,d0     ; F8
    bne.b   .no_maze_dump
    tst.l   _resload
    beq.b   .no_maze_dump
    movem.l d0-d1/a0-a2,-(a7)
    move.l maze_wall_table(pc),a1
    lea     maze_dump_file,a0
    move.l  _resload(pc),a2
    move.l  #26*27,D0
    jsr     (resload_SaveFile,a2)
    movem.l (a7)+,d0-d1/a0-a2
.no_maze_dump
    cmp.b   #$58,d0     ; F9
    bne.b   .no_attack
    move.w  #1,attack_timeout
.no_attack


.no_playing

    cmp.b   _keyexit(pc),d0
    bne.b   .no_quit
    st.b    quit_flag
.no_quit

	BSET	#$06,$1E01(A5)
	move.l	#2,d0
	bsr	beamdelay
	BCLR	#$06,$1E01(A5)	; acknowledge key

.nokey
	movem.l	(a7)+,d0/a0/a5
	move.w	#8,_custom+intreq
	rte
	
toggle_pause
	eor.b   #1,pause_flag
	beq.b	.out
	bsr		stop_sounds
	move.w	#1,start_music_countdown	; music will resume when unpaused
.out
	rts
	
    
; < D0: numbers of vertical positions to wait
beamdelay
.bd_loop1
	move.w  d0,-(a7)
    move.b	$dff006,d0	; VPOS
.bd_loop2
	cmp.b	$dff006,d0
	beq.s	.bd_loop2
	move.w	(a7)+,d0
	dbf	d0,.bd_loop1
	rts

    
; what: level 3 interrupt (vblank/copper)
; args: none
; trashes: none
    
level3_interrupt:
    movem.l d0-a6,-(a7)
    lea  _custom,a5
    move.w  (intreqr,a5),d0
    btst    #5,d0
    bne.b   .vblank
    move.w  (intreqr,a5),d0
    btst    #4,d0
    beq.b   .blitter
    tst.b   demo_mode
    bne.b   .no_pause
    tst.b   pause_flag
    bne.b   .outcop
.no_pause
    ; copper
    bsr draw_all
    tst.b   debug_flag
    beq.b   .no_debug
    bsr draw_debug
.no_debug
    bsr update_all
    move.w  vbl_counter(pc),d0
    addq.w  #1,d0
    cmp.w   #5,d0
    bne.b   .normal
    ; update a second time, simulate 60Hz
    bsr update_all
    moveq.w #0,d0    
.normal
    move.w  d0,vbl_counter
.outcop
    move.w  #$0010,(intreq,a5) 
    movem.l (a7)+,d0-a6
    rte    
.vblank
    moveq.l #1,d0
    bsr _read_joystick
    
    
    btst    #JPB_BTN_BLU,d0
    beq.b   .no_second
    move.l  joystick_state(pc),d2
    btst    #JPB_BTN_BLU,d2
    bne.b   .no_second

    ; no pause if not in game
    cmp.w   #STATE_PLAYING,current_state
    bne.b   .no_second
    tst.b   demo_mode
    bne.b   .no_second
    
    bsr		toggle_pause
.no_second
    lea keyboard_table(pc),a0
    tst.b   ($40,a0)    ; up key
    beq.b   .no_fire
    bset    #JPB_BTN_RED,d0
.no_fire 
    tst.b   ($4C,a0)    ; up key
    beq.b   .no_up
    bset    #JPB_BTN_UP,d0
    bra.b   .no_down
.no_up    
    tst.b   ($4D,a0)    ; down key
    beq.b   .no_down
	; set DOWN
    bset    #JPB_BTN_DOWN,d0
.no_down    
    tst.b   ($4F,a0)    ; left key
    beq.b   .no_left
	; set LEFT
    bset    #JPB_BTN_LEFT,d0
    bra.b   .no_right   
.no_left
    tst.b   ($4E,a0)    ; right key
    beq.b   .no_right
	; set RIGHT
    bset    #JPB_BTN_RIGHT,d0
.no_right    
    move.l  d0,joystick_state
    move.w  #$0020,(intreq,a5)
    movem.l (a7)+,d0-a6
    rte
.blitter
    move.w  #$0040,(intreq,a5) 
    movem.l (a7)+,d0-a6
    rte

vbl_counter:
    dc.w    0


SONG_1_LENGTH = ORIGINAL_TICKS_PER_SEC*17+ORIGINAL_TICKS_PER_SEC/2+6
SONG_2_LENGTH = ORIGINAL_TICKS_PER_SEC*13+14
BONUS_SONG_LENGTH = ORIGINAL_TICKS_PER_SEC*8-8
POWER_SONG_LENGTH = ORIGINAL_TICKS_PER_SEC*4+ORIGINAL_TICKS_PER_SEC/2-4

POWER_STATE_LENGTH = POWER_SONG_LENGTH*2+POWER_SONG_LENGTH/4

; what: updates game state
; args: none
; trashes: potentially all registers

update_all

    DEF_STATE_CASE_TABLE

.intro_screen
    bra update_intro_screen
    
    ; update_bonus_screen
.bonus_screen
  
    tst.l   state_timer
    bne.b   .no_first_bonus_tick

    addq.l  #1,state_timer
   
    clr.w   bonus_level_lane_select_subcounter
    move.w  #ORIGINAL_TICKS_PER_SEC*4,bonus_text_screen_countdown
.no_first_bonus_tick

    move.w  bonus_text_screen_countdown(pc),d0
    tst.w   d0
    beq.b   .bonus_playing
    subq.w  #1,d0
    move.w  d0,bonus_text_screen_countdown
    bne.b   .no_bonus_init  ; still in bonus pre-screen
    
    ; make up for level increase
    subq.w  #1,level_number
     ; pick a maze randomly. There are 3 different mazes
    
.random_loop
    bsr random
    and.w   #$3,d0
    beq.b   .random_loop    ; 0 ruled out so we get 1,2,3 only
    subq.w  #1,d0
    lsl.w   #3,d0   ; times 8
    lea maze_bonus_table(pc),a0
    add.w   d0,a0       ; pointers on maze walls
    move.l  (a0)+,maze_wall_table
    move.l  (a0)+,bonus_vertical_table
    clr.b   bonus_cattle_moving
    clr.w   bottom_reached
    ; pick a position for the banana 0-5
.random_loop2
    bsr random
    and.w   #$7,d0
    cmp.w   #6,d0
    bcc.b   .random_loop2
    ; multiply by 40 (how convenient) to get coordinate
    lea mul40_table(pc),a1
    add.w   d0,d0
    move.w  (a1,d0.w),banana_x
    
    st.b    bonus_sprites
    moveq.l #0,d0
    move.w  #1,nb_enemies_but_thief    
    bsr init_enemies
    lea enemies+Enemy_SIZEOF(pc),a0

    move.w  #0,xpos(a0)
    move.w  #8,ypos(a0)
    move.w  #DOWN,direction(a0)
    move.l  #$FFFF0001,h_speed(a0)   
.no_bonus_init
    rts
    
.bonus_playing
    addq.l  #1,state_timer
    
    tst.b   bonus_cattle_moving
    bne.b   .moving
    
    move.l  joystick_state(pc),d0
    cmp.l   #ORIGINAL_TICKS_PER_SEC*10,state_timer
    bne.b   .no_timeout
    ; timeout (if not already triggered)
    bset    #JPB_BTN_RED,d0
.no_timeout
    btst    #JPB_BTN_RED,d0
    beq.b   .no_fire
    move.w  #3,d0
    bsr     play_music
    st.b    bonus_cattle_moving
    move.w  #BONUS_SONG_LENGTH,bonus_music_replay_timer
.no_fire
    ; just selecting lane
    addq.w  #1,bonus_level_lane_select_subcounter
    cmp.w   #10,bonus_level_lane_select_subcounter
    bne.b   .do_nothing
    clr.w   bonus_level_lane_select_subcounter
    ; play ping sound
    lea ping_sound,a0
    bsr play_fx
    ; advance lane
    lea enemies+Enemy_SIZEOF(pc),a0
    move.w  xpos(a0),d0
    add.w   #40,d0
    cmp.w   #240,d0
    bne.b   .no_wrap
    clr.w   d0
.no_wrap
    move.w  d0,xpos(a0)
.do_nothing
    rts
    
.moving
    subq.w  #1,bonus_music_replay_timer
    bne.b   .no_replay
    move.w  #3,d0
    bsr     play_music
    move.w  #BONUS_SONG_LENGTH,bonus_music_replay_timer
    
.no_replay
    lea enemies+Enemy_SIZEOF(pc),a4

    move.w  ypos(a4),d0
    bmi.b   .down   ; not in the maze yet
    cmp.w   #200,d0
    beq.b   .out
    cmp.w   #190,d0
    bcc.b   .down   ; out of the maze
    bra move_normal
.no_animate
    rts
.down
    bsr animate_enemy
    addq.w  #1,ypos(a4)
    rts    
   
    
    
.game_start_screen
    tst.l   state_timer
    bne.b   .out
    addq.l   #1,state_timer
.out
    ; are we already in the bottom?
    tst.w   bottom_reached
    bne.b   .last_timeout
    ; stop music whatever the outcome
    bsr stop_sounds
    ; bottom is reached
    ; see if won
    ; arm timeout
    lea enemies+Enemy_SIZEOF(pc),a4
	
    move.w  xpos(a4),d0
    cmp.w   banana_x(pc),d0
    bne.b   .lose
    ; win
    move.w  #BONUS_WON,bottom_reached
    move.w  #ORIGINAL_TICKS_PER_SEC*4,last_bonus_timeout
    moveq.l #7,d0
    bsr     play_music
    move.l  #500,d0     ; 5000 points
    bra add_to_score
.lose
    move.w  #BONUS_LOST,bottom_reached
    move.w  #ORIGINAL_TICKS_PER_SEC*3,last_bonus_timeout
    lea lose_bonus_sound,a0
    bsr play_fx
    rts
.last_timeout
    move.w  last_bonus_timeout(pc),d0
    cmp.w   #ORIGINAL_TICKS_PER_SEC*2,d0
    bne.b   .no_stopmus
    cmp.w  #BONUS_LOST,bottom_reached
    beq.b   .no_stopmus
    bsr stop_sounds
.no_stopmus
    subq.w  #1,last_bonus_timeout
    bne.b   .continue
    ; next level
    bsr .bonus_level_completed
.continue
    rts
    
.life_lost
    rts

.bonus_level_completed
    bsr hide_sprites
    bsr     stop_sounds
.next_level
     move.w  #STATE_NEXT_LEVEL,current_state
     clr.b  next_level_is_bonus_level
     clr.b  bonus_sprites
     rts
     
.game_over
    cmp.l   #GAME_OVER_TIMER,state_timer
    bne.b   .no_first
    bsr stop_sounds
    moveq.l  #10,d0
    bsr     play_music
.no_first
    tst.l   state_timer
    bne.b   .cont
    bsr stop_sounds
    move.w  #STATE_INTRO_SCREEN,current_state
.cont
    subq.l  #1,state_timer
    rts
    ; update
.playing
    move.w   completed_music_timer(pc),d0
    beq.b   .no_completed
    subq.w  #1,d0
    move.w  d0,completed_music_timer
    bne.b   .completed_music_playing
    
    bsr stop_sounds
    move.w  #STATE_NEXT_LEVEL,current_state
    clr.l   state_timer     ; without this, bonus level isn't drawn
    bsr     hide_sprites    ; hide sprites as bonus level only uses 1 or 2 sprites
.completed_music_playing
    rts
.no_completed

    tst.l   state_timer
    bne.b   .no_first_tick
    st.b   .intro_music_played
    moveq.w   #1,d0
    tst.w  level_number
    bne.b   .no_delay
    ; first level: play start music
    clr.b   .intro_music_played
    
    moveq.l #6,d0
    bsr     play_music
    move.w  #ORIGINAL_TICKS_PER_SEC*5,d0
.no_delay
    move.w  d0,start_music_countdown
.no_first_tick
    ; for demo mode
    addq.w  #1,record_input_clock

    bsr update_player
    
    IFND    NO_ENEMIES
    tst.w   player_killed_timer
    bpl.b   .skip_cc     ; player killed, no collisions	
    bsr check_collisions
.skip_cc
    bsr update_enemies
    
    tst.w   player_killed_timer
    bpl.b   .skip_a_lot     ; player killed, no music management, no collisions
    
    bsr check_collisions
    ENDC
    
    move.w   power_state_counter(pc),d0
    bne.b   .power_music
    
    move.w  start_music_countdown(pc),d0
    subq.w  #1,d0    ; starts after a few seconds
    bne.b   .no_start_music
.normal_music
    clr.w   start_music_countdown
    tst.b   rustler_level
    beq.b   .copier_level
    ; ruslter level
    moveq.l #4,d0
    bsr play_music
    move.w  #SONG_2_LENGTH,d0
    bra.b   .no_start_music
.copier_level
    moveq.l #0,d0
    bsr play_music
    move.w  #SONG_1_LENGTH,d0
.no_start_music
    move.w  d0,start_music_countdown
    cmp.w   #ORIGINAL_TICKS_PER_SEC,d0
    bne.b   .music_out
    tst.b   .intro_music_played
    bne.b   .music_out
    st.b   .intro_music_played
    bsr stop_sounds
    bra.b   .music_out
.power_music
    subq.w  #1,d0
    move.w  d0,power_state_counter
    bne.b   .power_continues

	; reset enemies to lethal
	lea	enemies(pc),a0

	; save for later
	move.w	mode(a0),d4
	move.w	previous_mode(a0),d5
	

	move.w	nb_enemies_but_thief(pc),d1
.gloop
    ; no power state, reset old mode
    move.w  mode(a0),d2
	cmp.w	#MODE_HANG,d2
	bne.b	.no_hang
    ; subtract 8 again
    subq.w  #8,ypos(a0)
.no_hang
	cmp.w	#MODE_FALL,d2
	beq.b	.no_reset_mode
	move.w	previous_mode(a0),mode(a0)
.no_reset_mode
	clr.b	fright_mode(a0)
	add.w	#Enemy_SIZEOF,a0
	dbf		d1,.gloop

	cmp.w	#MODE_HANG,d4
	bcs.b	.tracer_not_killed
	cmp.w	#MODE_CHASE,d5
	bne.b	.tracer_not_killed
	; killed and was in chase mode: shortcut chase mode
	; to avoid that tracer/thief goes through the path that
	; player took to eat enemies directly, without first step
	st.b    player_move_record
    clr.b    first_thief_objective	; skip first objective
    bsr		reset_thief_attack_mode
	lea	enemies(pc),a0	
	bsr	trigger_chase_mode	
.tracer_not_killed


.skip_a_lot
    ; reset enemies palette

    bsr set_enemy_normal_palette

    tst.w   player_killed_timer
    bmi.b   .normal_music     ; player killed, no music management
    move.w  #1,start_music_countdown       ; restart music as soon as new life starts
    bra.b   .music_out
.power_continues

BLINK_BASE_TIME = POWER_SONG_LENGTH+POWER_SONG_LENGTH/2

    
    ; special values where we blink
    cmp.w   #POWER_STATE_LENGTH-BLINK_BASE_TIME,d0
    bne.b   .nb1
.pb
    ; first blink
    bsr set_enemy_power_blink_palette
    bra.b   .bend
.nb1
    cmp.w   #POWER_STATE_LENGTH-(BLINK_BASE_TIME+ORIGINAL_TICKS_PER_SEC/2),d0
    bne.b   .nb2
.pub
    ; first blink, revert
    bsr set_enemy_power_state_palette
    bra.b   .bend
.nb2
    cmp.w   #POWER_STATE_LENGTH-(BLINK_BASE_TIME+ORIGINAL_TICKS_PER_SEC+ORIGINAL_TICKS_PER_SEC/2),d0
    beq.b   .pb
    cmp.w   #POWER_STATE_LENGTH-(BLINK_BASE_TIME+2*ORIGINAL_TICKS_PER_SEC),d0
    beq.b   .pub
    
    cmp.w   #POWER_STATE_LENGTH-(BLINK_BASE_TIME+2*ORIGINAL_TICKS_PER_SEC+ORIGINAL_TICKS_PER_SEC/2),d0
    beq.b   .pb
    cmp.w   #POWER_STATE_LENGTH-(BLINK_BASE_TIME+3*ORIGINAL_TICKS_PER_SEC),d0
    beq.b   .pub


.bend
    ; power music countdown
    move.w  power_song_countdown(pc),d0
    subq.w  #1,d0
    bne.b   .continue_power
    ; restart music
    moveq.l #2,d0
    bsr play_music
    move.w  #POWER_SONG_LENGTH,d0
.continue_power
    move.w  d0,power_song_countdown
    
.music_out
    addq.l  #1,state_timer
    rts
.ready_off


    rts

.intro_music_played
    dc.b    0
    even
start_music_countdown
    dc.w    0

COLLISION_SHIFTING_PRECISION = 3

set_thief_attack_mode
    st.b    thief_attacks
    clr.b   thief_up_move_lock
    lea     enemies(pc),a0
    move.w  #MODE_STANDBY,mode(a0)
reset_thief_attack_mode
    ; set timer for full attack mode
    move.b  #4,thief_attack_sound_count
    move.w  #1,thief_attack_sound_count_timer
	; always a short time before heads for first objective
    move.w  #ORIGINAL_TICKS_PER_SEC,thief_standby_timer
    rts
    
check_collisions
    lea player(pc),a3
    move.w  xpos(a3),d0
    move.w  ypos(a3),d1
    lsr.w   #COLLISION_SHIFTING_PRECISION,d0
    lsr.w   #COLLISION_SHIFTING_PRECISION,d1
    
    lea enemies(pc),a4
    move.w  nb_enemies_but_thief(pc),d7    ; plus one
.gloop
    move.w  xpos(a4),d2
    move.w  ypos(a4),d3
    lsr.w   #COLLISION_SHIFTING_PRECISION,d2
    lsr.w   #COLLISION_SHIFTING_PRECISION,d3
    cmp.w   d2,d0
    bne.b   .nomatch
    cmp.w   d3,d1
    beq.b   .collision
.nomatch
    add.w   #Enemy_SIZEOF,a4
    dbf d7,.gloop
    rts
.collision
    ; is the enemy falling, hanging, whatever...
	cmp.w	#MODE_HANG,mode(a4)
	bcc.b	.nomatch	; ignore those killed modes
    tst.b   fright_mode(a4)
    bne.b   .pac_eats_ghost
	
    ; player is killed
    tst.b   invincible_cheat_flag
    bne.b   .nomatch
    move.w  #MODE_KILL,mode(a4)
    move.w  #PLAYER_KILL_TIMER,player_killed_timer
    clr.w   enemy_kill_timer
    move.w  #KILL_FIRST_FRAME,enemy_kill_frame
	tst.b rustler_level
	beq.b	.no_rollback
	bsr	rollback_paint
.no_rollback
    bsr stop_sounds
    lea     player_killed_sound(pc),a0
    bra     play_fx
   

.pac_eats_ghost:   
    
    ; depending on the x coordinate, hang or fall
    move.w  #MODE_KILLED,mode(a4)    
    ; display score (2 seconds)
    move.w  #ENEMY_KILL_TIMER,score_display_timer(a4)
    
    cmp.w   #STATE_PLAYING,current_state
    bne.b   .no_sound
    lea     enemy_killed_sound(pc),a0
    bsr     play_fx
.no_sound
    sub.w   #1,nb_enemies_to_eat
    move.w   next_enemy_iteration_score(pc),d1
    move.l  d1,d0
    add.w   #SCORE_FIRST_FRAME,d0
	cmp.w	#4,nb_enemies_but_thief
	beq.b	.still_enemies	; not when there are only 4+1 enemies
    tst.w   nb_enemies_to_eat
    bne.b   .still_enemies
    addq.w  #4,d0       ; 3200 score frame
.still_enemies
    move.w  d0,score_frame(a4)
    
    lea  score_value_table(pc),a0
    move.l  (a0,d1.w),d0
    cmp.l   #160,d0
    bne.b   .not_max
    ; re-award 1600 unless only 1 enemy left
	cmp.w	#4,nb_enemies_but_thief
	beq.b	add_to_score	; not when there are only 4+1 enemies
    tst.w   nb_enemies_to_eat
    bne.b   add_to_score
    ; award 3200 points
    move.l  #320,d0
    bra.b   add_to_score
.not_max
    addq.w  #4,d1
    ; advance score
    move.w  d1,next_enemy_iteration_score
    bra add_to_score
    ; exits as soon as a collision is found

    
CHARACTER_X_START = 88

update_intro_screen
    move.l   state_timer(pc),d0
    bne.b   .no_first
    
.first
	move.l	speed_table(pc),global_speed_table
    tst.w   high_score_position
    bpl.b   .second
    
    move.b  #1,intro_step
    st.b    intro_state_change

    move.w  #1,nb_enemies_but_thief
    st.b    bonus_sprites
    clr.l	d0
    bsr init_enemies
    
    lea enemies+Enemy_SIZEOF(pc),a0

    move.w   #-1,previous_xpos(a0)
    move.w   #-1,previous_ypos(a0)
	lea		.cattle_x_table(pc),a1
	; pick a random start position
	bsr		random
	and.w	#3,d0
	add.w	d0,d0
	move.w	(a1,d0.w),d0
    move.w  d0,xpos(a0)
    move.w  #-8,ypos(a0)     ; this is the logical coordinate
    move.l  #maze_intro_wall_table,maze_wall_table
    move.w  #DOWN,direction(a0)
    move.l  #$FFFF0001,h_speed(a0)
    
    bra.b   .cont
.no_first 
    cmp.l   #ORIGINAL_TICKS_PER_SEC*9,d0
    bne.b   .no_second
.second
    move.w   high_score_position(pc),d0
    bmi.b   .no_init_second
    lsl.w   #4,d0   ; times 16
    add.w   #24,d0  ; plus offset
    move.w  d0,high_score_highlight_y
    clr.w   high_score_highlight_timer
    clr.w   high_score_highlight_color_index
.no_init_second
    move.b  #2,intro_step
    st.b    intro_state_change
    bra.b   .cont
.no_second
    cmp.l   #ORIGINAL_TICKS_PER_SEC*12,d0
    bne.b   .cont
.third
    ; highscore highlight => first screen
    tst.w   high_score_position
    bmi.b   .really_third
    bra.b   .reset_first
.really_third
    ; third screen init
    st.b    intro_state_change
    move.b  #3,intro_step
    clr.w   intro_frame_index

    move.w  #ORIGINAL_TICKS_PER_SEC,.cct_countdown
    move.w  #CHARACTER_X_START,.cct_x
    move.w  #80-24,.cct_y

    clr.w   .cct_text_index
    move.w   #6,.cct_counter
    clr.w   .cct_char_index
   
.cont    
    move.l  state_timer(pc),d0
    add.l   #1,D0
    cmp.l   #ORIGINAL_TICKS_PER_SEC*22,d0
    bne.b   .no3end
.reset_first
	clr.l	state_timer
	; test if game was just played
	; with a hiscore highlight
	
	tst.w   high_score_position
    bmi.b   .demo		  ; screen 3 end => demo mode
    move.w  #-1,high_score_position	
    bra.b	.first ; from highscore highlight: just revert to title
.no3end
    move.l  d0,state_timer
    
    cmp.b   #2,intro_step
    beq.b   .step2
    cmp.b   #3,intro_step
    beq.b   .step3
    
    cmp.l   #ORIGINAL_TICKS_PER_SEC,d0
    bcs.b   .no_animate
    cmp.l   #ORIGINAL_TICKS_PER_SEC*8,d0
    bcc.b   .no_animate
    
    lea enemies+Enemy_SIZEOF(pc),a4

	; paint here
    move.w  xpos(a4),d0
    move.w  ypos(a4),d1

    lea screen_data,a1
    add.w   #INTRO_Y_SHIFT+8,d1
    ADD_XY_TO_A1    a2
    lea (SCREEN_PLANE_SIZE,a1),a2
    cmp.w   #LEFT,direction(a4)
    beq.b   .skipleft
    move.b  (a1),(a2)
    move.b  (NB_BYTES_PER_LINE,a1),(NB_BYTES_PER_LINE,a2)
.skipleft
    move.b  (1,a1),(1,a2)
    move.b  (NB_BYTES_PER_LINE+1,a1),(NB_BYTES_PER_LINE+1,a2)


    move.w  ypos(a4),d0
    bmi.b   .down   ; not in the maze yet
	cmp.w	#4,d0
	bcs.b	.down
    cmp.w   #112,d0
    beq.b   .out
    cmp.w   #108,d0
    bcc.b   .down   ; out of the maze
    bra move_normal
.no_animate
    rts
.horiz
    addq.w  #1,xpos(a4)
    rts
.down
    bsr animate_enemy
    addq.w  #1,ypos(a4)
    rts
.step2
    tst.w   high_score_position
    bmi.b   .out
    add.w   #1,high_score_highlight_timer
    cmp.w   #4,high_score_highlight_timer
    bne.b   .out
    clr.w   high_score_highlight_timer
    add.w   #1,high_score_highlight_color_index
    cmp.w   #4,high_score_highlight_color_index
    bne.b   .out
    clr.w   high_score_highlight_color_index
    rts
.step3
    add.w   #1,intro_frame_index
    move.w  .cct_countdown(pc),d0
    beq.b   .text_print
    subq.w  #1,d0
    move.w  d0,.cct_countdown
    rts
.text_print
    cmp.w   #24,.cct_text_index
    beq.b   .no_text        ; stop printing
    
    subq.w  #1,.cct_counter
    bne.b   .no_text
    ; reload
    move.w  #6,.cct_counter
    ; print a character
    move.w  .cct_text_index(pc),d0
    lea .text_table(pc),a0
    move.l  (a0,d0.w),a0        ; current text
    move.w  .cct_char_index(pc),d1
    add.w   d1,a0   ; current text char
    move.b  (a0),d2
    beq.b   .next_text
    
    lea draw_char_command(pc),a1
    move.l  .cct_x(pc),(a1)+    ; X & Y
    move.b  d2,(a1)+
    st.b    (a1)    ; enable
    add.w   #8,.cct_x
    add.w   #1,d1
    move.w  d1,.cct_char_index
    rts
    
.next_text
    addq.w  #4,.cct_text_index    
    add.w   #24,.cct_y
    move.w  #CHARACTER_X_START,.cct_x
    clr.w   .cct_char_index
.out    
.no_text
    rts


.demo
    ; change state
    clr.l   state_timer
    move.w  #STATE_PLAYING,current_state
    ; in demo mode
    st.b    demo_mode
    rts

; not all start positions work properly
; but who cares? just omit the ones that fail
.cattle_x_table:
	dc.w	40,80,120,160
.cct_countdown
    dc.w    0
.cct_x:
    dc.w    0
.cct_y:
    dc.w    0
.cct_text_index:
    dc.w    0
.cct_counter:
    dc.w    0
.cct_char_index
    dc.w    0
.text_table
    dc.l    .text1
    dc.l    .text2
    dc.l    .text3
    dc.l    .text4
    dc.l    .text5
    dc.l    .text3
.text1:
    dc.b    "hhh  COPIER",0
.text2:
    dc.b    "hhh  POLICE",0
.text3:
    dc.b    "hhh  THIEF",0
.text4:
    dc.b    "hhh  RUSTLER",0
.text5:
    dc.b    "hhh  CATTLE",0
    even

    
    
update_enemies:
    tst.b   thief_attacks
    beq.b   .no_attack_sound
    tst.b   thief_attack_sound_count
    beq.b   .no_attack_sound
    move.w  thief_attack_sound_count_timer(pc),d0
    subq.w  #1,d0
    bne.b   .no_sound_timer
    subq.b  #1,thief_attack_sound_count
    ; play sound
    lea thief_attacks_sound,a0
    bsr play_fx
    move.w  #ORIGINAL_TICKS_PER_SEC-15,d0
.no_sound_timer
    move.w   d0,thief_attack_sound_count_timer
    
.no_attack_sound
    lea enemies(pc),a4
    tst.w   can_eat_enemies_mode_pending
    beq.b   .no_eat_mode_pending
    ; test if not jumping
    cmp.w   #MODE_JUMP,mode(a4)
    beq.b   .no_eat_mode_pending
    clr.w   can_eat_enemies_mode_pending
    bsr     all_four_corners_done
.no_eat_mode_pending
    
    move.w nb_enemies_but_thief(pc),d7
    move.w  player_killed_timer(pc),d6
    bmi.b   .gloop
    subq.w  #1,player_killed_timer
    bne.b   .gloop
    ; end current life & restart
    move.w  #STATE_LIFE_LOST,current_state
    rts
.glkill
    bsr animate_enemy
    add.w   #Enemy_SIZEOF,a4
    dbf d7,.glkill
    rts
    
.gloop
    move.w  mode(a4),d0
    move.l d7,-(a7)
    lea     enemy_move_table(pc),a0
    move.l  (a0,d0.w),a0
    jsr (a0)
    move.l  (a7)+,d7
    add.w   #Enemy_SIZEOF,a4
    dbf d7,.gloop
    rts

     
animate_enemy
    move.w  frame(a4),d1
    addq.w  #1,d1
    and.w   #$F,d1
    move.w  d1,frame(a4)
    rts
enemy_move_table
    dc.l    move_normal
    dc.l    move_standby
    dc.l    move_border_patrol
    dc.l    move_chase
    dc.l    move_hang
    dc.l    move_fall
    dc.l    move_jump
    dc.l    move_kill
    dc.l    move_killed
    dc.l    move_crash


BREAKIF:MACRO
    cmp.w   #\1,d2
    bne.b   .zap
    cmp.w   #\2,d3
    bne.b   .zap
    blitz
    nop
.zap
    ENDM

move_jump
    move.w  jump_index(pc),d1
    lea jump_height_table(pc),a0
    move.b  (a0,d1.w),d2
    ext d2
    add.w   d2,ypos(a4)
    rts
    
; < a4: enemy structure
; trashes: most registers
; todo: loop according to instant speed, ATM speed=1
move_normal
    bsr animate_enemy
    bsr get_next_speed_index
    cmp.b   #1,d0
    beq.b   .one_iteration      ; do it once
    bsr     .one_iteration      ; do it twice
.one_iteration
    
    move.w  xpos(a4),d2
    move.w  ypos(a4),d3
    move.w   h_speed(a4),d4
    move.w   v_speed(a4),d5

    move.w  direction(a4),d6
    cmp.w   #UP,d6
    bcc.b   .hfirst ; UP or DOWN
.vfirst
    ; left or right
    ; vertical first unless turn lock is set
    tst.w   turn_lock(a4)
    beq.b   .normal_vtest
    subq.w  #1,turn_lock(a4)    ; decrease
    ; no vertical attempt
    clr.w   d5
    bra.b   .direct_htest
.normal_vtest
    bsr enemy_try_vertical
    tst.w   d0
    bne.b   .done
.direct_htest
    bsr enemy_try_horizontal
    tst.w   d0
    bne.b   .done
    ; cannot reach that point unless blocked
    ; last chance: clear turn lock and retry
    ; else it's going to loop forever
    clr.w   turn_lock(a4)
    bra.b   .vfirst
    
.hfirst
    ; first try to move horizontally
    bsr enemy_try_horizontal
    tst.w   d0
    bne.b   .done
    ; then try to move vertically
    bsr enemy_try_vertical
    tst.w   d0
    bne.b   .done
    ; cannot reach that point unless blocked
    ; last chance: clear turn lock and retry
    ; else it's going to loop forever
    clr.w   turn_lock(a4)
    bra.b   .hfirst

.done

    ; update direction in the end
    tst.w   d4
    beq.b   .uvert
    bmi.b   .uleft
    move.w  #RIGHT,direction(a4)
    rts
    
.uleft
    move.w  #LEFT,direction(a4)
    rts

.uvert
    tst.w   d5
    bmi.b   .uup
    move.w  #DOWN,direction(a4)
    rts
.uup
    move.w  #UP,direction(a4)
    
    rts

; try to move horizontally
enemy_try_horizontal
    move.w  d3,d1
    ; don't try if not aligned y-wise
    and.w   #7,d1
    bne.b   .no_horizontal
    move.w  d2,d0
    move.w  d3,d1
    

    add.w   d4,d0

    ; within maze: check if can move horizontally
    ; both left & right, regardless of direction, but with priority
    ; to the current direction
    
    moveq.w #1,d6   ; pass 1
    
    tst.w   d4
    bmi.b   .test_left
    ; right first 
.test_right

    cmp.w   #X_MAX+1,d0
    beq.b   .no_horizontal_revert
    cmp.w   #X_MAX,d2
    beq.b   .no_horizontal

    move.w  d2,d0
    
    add.w  #8,d0
    cmp.w   #X_MAX+1,d0
    bcc.b   .right_ok
    ; right test
    bsr     is_location_legal
    tst.b   d0
    beq.b   .maybe_test_left
.right_ok
    ; right is valid: set this as h speed
    move.w  #1,d4
    move.w  d4,h_speed(a4)
    bra.b   .hok
.maybe_test_left
    tst.w   d6
    beq.b   .no_horizontal
.test_left_pass2
    clr.w   d6      ; pass 2
    move.w  d3,d1   ; restore y coord
.test_left
    tst.w   d0
    bmi.b   .no_horizontal_revert    ; off limits
    tst.w   d2
    beq.b   .no_horizontal
    move.w  d2,d0
    subq.w   #1,d0
.htest
    bsr     is_location_legal
    tst.b   d0
    beq.b   .maybe_test_right
    ; left is valid: set it as h speed
    moveq.w  #-1,d4
    move.w  d4,h_speed(a4)

.hok
    ; can move: validate
    add.w  d4,xpos(a4)
    clr.w   d5
    moveq.w #1,d0
    rts
.maybe_test_right
    tst.w   d6
    beq.b   .no_horizontal
    clr.w   d6      ; pass 2
    ; restore y coord and test right
    move.w  d3,d1
    bra.b   .test_right
    
.no_horizontal
    clr.w   d4
    clr.w   d0
    rts
.no_horizontal_revert
    ; off limits / reverse speed
    neg.w   d4
    move.w  d4,h_speed(a4)
    bra.b   .no_horizontal
    
enemy_try_vertical
    move.w  d2,d0
    ; don't try if not aligned x-wise
    and.w   #7,d0
    bne.b   .no_vertical
    move.w  d2,d0

    move.w  d3,d1
    add.w   d5,d1
    bmi.b   .no_vertical_revert    ; off limits
    cmp.w   #Y_MAX+1,d1
    beq.b   .no_vertical_revert
    ; within maze: check if can move horizontally
    tst.w   d5
    bmi.b   .to_up
    ; down
    add.w   #7,d1
    bra.b   .htest
.to_up
.htest
    bsr     is_location_legal
    tst.b   d0
    beq.b   .no_vertical
.vok
    ; can move: validate
    add.w  d5,ypos(a4)
    clr.w   d4
    moveq.w #1,d0
    rts
.no_vertical
    clr.w   d5
    clr.w   d0
    rts
.no_vertical_revert
    ; off limits, reverse speed
    ; there's a special case here, the "amidar movement" rule is not
    ; 100% respected (checked footage of the arcade game)
    ; if enemy is at min/max x then it's respected, otherwise it's not,
    ; and the enemy can miss the next up/down turn
    ;
    ; X is not corner but corner +/- 8 because vertical test is done at this
    ; moment
    cmp.w   #X_MAX-8,d2
    beq.b   .no_turn_lock
    cmp.w   #8,d2
    beq.b   .no_turn_lock
    move.w  #70,turn_lock(a4)   ; > 40 and < 72 will do
.no_turn_lock
    neg.w   d5
    move.w  d5,v_speed(a4)
    bra.b   .no_vertical

get_next_speed_index
    move.w  speed_table_index(a4),d0
    move.w  d0,d1
    add.w   #1,d0
    cmp.w   #20,d0
    bne.b   .no_wrap
    clr.w   d0
.no_wrap
    move.w  d0,speed_table_index(a4)
    
    move.l  global_speed_table(pc),a0
    move.b  (a0,d1.w),d0
    rts    
    
move_standby

    bsr     animate_enemy
    subq.w  #1,thief_standby_timer
    bne.b   .out
	move.l	a4,a0
	bsr		trigger_chase_mode
.out
    rts

; what: switch to chase mode
; < A4: enemy struct
trigger_chase_mode
    move.w  #MODE_CHASE,mode(a0)    ; attack player!
    ; timeout reached. If player move recorder
    ; isn't started, start it
    tst.b   player_move_record
    bne.b   store_player_tile
    st.b    player_move_record
    ; will re-switch to standby when first objective is reached
    st.b    first_thief_objective
    st.b    first_recorded_move
    clr.w   thief_move_index
    clr.w   player_move_index
    ; now record player moves
    bra     store_player_tile


; store player tile only if the same as previously
; or the first one
store_player_tile
    lea     player_move_buffer,a1
    lea     player(pc),a0
    move.w  xpos(a0),d2
    lsr.w   #3,d2
    move.w  ypos(a0),d3
    lsr.w   #3,d3
    tst.b   first_recorded_move
    bne.b   .store
    ; get previous values
    bsr     get_latest_player_tile
    cmp.w   d0,d2
    bne.b   .store2
    cmp.w   d1,d3
    beq.b   .out        ; same tile as earlier: don't record
.store
    clr.b   first_recorded_move
.store2
    ; store current player position
    move.w  player_move_index(pc),d1
    move.w  d2,(a1,d1.w)
    move.w  d3,2(a1,d1.w)
    addq.w  #4,d1
    cmp.w   #NB_RECORDED_MOVES*4,d1
    bne.b   .no_wrap
    clr.w   d1
.no_wrap
    move.w  d1,player_move_index
.out
    rts
    
get_latest_player_tile
    lea     player_move_buffer,a1
    move.w  player_move_index(pc),d1
    bne.b   .no_wrap
    add.w   #NB_RECORDED_MOVES*4,d1
.no_wrap
    move.w  -4(a1,d1.w),d0
    move.w  -2(a1,d1.w),d1
    rts

    
move_border_patrol    
    bsr animate_enemy
.retry
    move.w  xpos(a4),d2
    move.w  ypos(a4),d3
    move.w   h_speed(a4),d4
    move.w   v_speed(a4),d5
    move.w  d2,d0
    move.w  d3,d1
    add.w   d4,d0
    bmi.b   .change
    cmp.w   #X_MAX+1,d0
    beq.b   .change
    add.w   d5,d1
    bmi.b   .change
    cmp.w   #Y_MAX+1,d1
    beq.b   .change
    
    add.w   d4,xpos(a4)
    add.w   d5,ypos(a4)
    
    ; change mode to chase after a while because of mode timer
    sub.w  #1,attack_timeout
    bne.b   .no_attack
    bsr     set_thief_attack_mode
.no_attack    
    rts
.change	
    tst.w   d4
    bmi.b   .down
    bne.b   .up
    ; horizontal
    tst.w   d5
    bpl.b   .right
    ; left
    move.l  #$FFFF0000,h_speed(a4)   ; change to left
    move.w  #LEFT,direction(a4)
    bra.b   .retry
.right
    move.l  #$00010000,h_speed(a4)   ; change to right
    move.w  #RIGHT,direction(a4)
    bra.b   .retry

.down
    move.l  #1,h_speed(a4)   ; change to down
    move.w  #DOWN,direction(a4)
    bra.b   .retry
.up
    move.l  #$0000FFFF,h_speed(a4)   ; change to up
    move.w  #UP,direction(a4)
    bra.b   .retry
    
move_killed
    ; just display score for a while, decrease a counter
    ; then change to fall or hang
    subq.w  #1,score_display_timer(a4)
    bne.b   .keep_going
    ; now get x coord and see if it's close to vertical lanes    
    move.w  xpos(a4),d0
    lea hangfall_table(pc),a0
    add.w   d0,d0
    move.w  (a0,d0.w),d0
    move.w  d0,mode(a4)
	IFD		DEBUG_MODE
    cmp.w   #MODE_LAST_ITEM,d0
    bcs.b   .ok
    blitz 
.ok
	ENDC
    cmp.w   #MODE_FALL,d0
    bne.b   .no_fall
    lea     enemy_falling_sound,a0
    bsr     play_fx
.no_fall
    ; init timers & flags
    clr.w   fall_hang_timer(a4)
    clr.w   fall_hang_toggle(a4)
    clr.w   previous_ypos(a4)
    addq.w  #8,ypos(a4)
    rts
    
    
.keep_going
    rts

    
state_transition_table
    REPT    16
    dc.l    f_unknown_transition
    ENDR
    
; close to vertical lines: fall, else hang
; add margin as last coord is 200 and would read outside bounds
hangfall_table:
    REPT    5
    dc.w	MODE_FALL,MODE_FALL,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG
    dc.w    MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG
    dc.w    MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG
    dc.w    MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG,MODE_HANG
    dc.w    MODE_HANG,MODE_HANG,MODE_FALL,MODE_FALL
    ENDR
    dc.w    MODE_FALL,MODE_FALL
    
move_kill:
    ; animating the enemy killing the player
    move.w  enemy_kill_timer(pc),d0
    addq.w  #1,d0
    cmp.w   #10,d0
    bne.b   .no_change
    clr.w   d0
	; 4 frames
    move.w  enemy_kill_frame(pc),d1
	addq.w	#4,d1
    cmp.w   #KILL_FIRST_FRAME+16,d1
    bne.b   .out
    move.w  #KILL_FIRST_FRAME,d1
.out
    move.w  d1,enemy_kill_frame
.no_change
    move.w  d0,enemy_kill_timer
    rts
    
move_chase    
    ; record player movements
    bsr    store_player_tile

    
    bsr     animate_enemy
    ; enemy tries to reach objective
.loop
    lea player_move_buffer,a2
    
    move.w  thief_move_index(pc),d0
    ; objective
    move.w  (a2,d0.w),d2
    move.w  2(a2,d0.w),d3

    ; enemy position
    move.w  (xpos,a4),d0
    move.w  d0,d4
    lsr.w   #3,d4
    move.w  (ypos,a4),d1
    move.w  d1,d5
    lsr.w   #3,d5

    move.w  d0,d6   ; save d0
    move.w  d1,d7   ; save d1
    tst.b   thief_up_move_lock
    bmi.b   .enemy_on_the_right  ; < 0
    bne.b   .left_test ; > 0
.normal_vert_first

    ; first test y tile
    cmp.w   d5,d3
    beq.b   .try_x
    ; don't try if not aligned x-wise
    and.w   #7,d0
    bne.b   .try_x

    ; not y aligned
    cmp.w   d5,d3
    ; check if enemy is below target
    bcs.b   .enemy_below
    ; enemy is above player
    ; can it move down?

    move.w  d6,d0
    addq.w  #8,d1
    bsr     is_location_legal
    tst.b   d0
    beq.b   .cant_move_vertically
    ; can move up: validate it
    move.w  #DOWN,direction(a4)
    move.w  d7,d1
    addq.w  #1,d1
    move.w  d1,ypos(a4)
    bra.b   .out
.enemy_below
    ; can it move up?
    move.w  d6,d0
    subq.w  #8,d1
    bsr     is_location_legal
    tst.b   d0
    beq.b   .cant_move_vertically
    ; can move up: validate it
    move.w  #UP,direction(a4)
    move.w  d7,d1
    subq.w  #1,d1
    move.w  d1,ypos(a4)
    bra.b   .out
.cant_move_vertically
    ; reset d0,d1 and try x
    move.w  d6,d0
    move.w  d7,d1
.try_x
    ; don't try if not aligned y-wise

    cmp.w   d5,d3
    bne.b   .keep_going
    cmp.w   d4,d2
    bne.b   .objective_not_reached
    bsr.b   .objective_reached
    tst.b   d0
    bne.b   .loop       ; objective changed: retry
    bra.b   .out        ; objective in the same horizon as player: quit
    ; objective reached, but don't lose 1 move: keep trying to move
.objective_not_reached
    move.w  d7,d1   ; restore D1
    cmp.w   d4,d2
    ; check if enemy is on right of target
    bcs.b   .enemy_on_the_right
    ; enemy is on the left of the target
    ; can it move right?
.left_test
    move.w  d6,d0       ; restore d0
    and.w   #7,d1
    bne.b   .cant_move_right
    addq.w  #1,d0
    move.w  d0,d6   ; backup
    move.w  d7,d1   ; restore d1
    bsr     is_location_legal
    tst.b   d0
    beq.b   .cant_move_right
    move.w  #RIGHT,direction(a4)
    bra.b   .can_move_laterally
.enemy_on_the_right
    move.w  d6,d0       ; restore d0
    and.w   #7,d1
    bne.b   .cant_move_left
    
    ; can it move left?
    subq.w  #1,d0
    move.w  d0,d6   ; backup
    move.w  d7,d1   ; restore d1
    bsr     is_location_legal
    tst.b   d0
    beq.b   .cant_move_left
    move.w  #LEFT,direction(a4)
    ; can move laterally: validate it
.can_move_laterally
    clr.b   thief_up_move_lock  ; remove lock
    move.w  d6,xpos(a4)
    bra.b   .out
    
; < D0: 0 if reached player tile and keep going "blindly"
;       not 0: normal
.objective_reached
    ; is that the first objective ?
    tst.b   first_thief_objective
    bne.b   .first_objective
    
    ; now following player trail
    
    move.w   player_move_index(pc),d1
    bne.b   .no_wrap0
    move.w   #NB_RECORDED_MOVES*4,d1
.no_wrap0
    subq.w  #4,d1
    ; thief move index must not be equal to
    ; (or ahead of) player move index
    
    move.w  thief_move_index(pc),d0
    cmp.w   d0,d1
    beq.b   .keep_going        ; same tile, last tile, but stuck
    addq.w   #4,d0
    cmp.w   #NB_RECORDED_MOVES*4,d0
    bne.b   .no_wrap
    clr.w   d0
.no_wrap
    move.w  d0,thief_move_index
    st.b    d0
    rts
.keep_going
    ; continue in the previous direction
    ; should be enough to resolve the situation
    move.w  direction(a4),d0
    lea     .keep_going_table(pc),a0
    move.l  (a0,d0.w),a0
    jsr     (a0)
    clr.w   d0
    rts
    
    
.keep_going_table
    dc.l    .keep_going_right
    dc.l    .keep_going_left
    dc.l    .keep_going_up
    dc.l    .keep_going_down
    dc.l    .keep_going_impossible
.keep_going_impossible
    move.w  #$F00,$DFF180
    rts
    
.keep_going_right
    addq.w   #1,xpos(a4)
    rts
.keep_going_left
    subq.w   #1,xpos(a4)
    rts
.keep_going_up
    subq.w   #1,ypos(a4)
    rts
.keep_going_down
    addq.w   #1,ypos(a4)
    rts
    
    
.first_objective:
    ; pause again
    clr.b   first_thief_objective
    move.w  #MODE_STANDBY,mode(a4)
    move.w  thief_standby_time,thief_standby_timer

    rts
.out
    rts
.cant_move_right
    ; can happen at start when thief tries to reach first objective
    ; same y but no direct path: move up (always possible)
    ; and lock until can move in the required lateral direction
    subq.w   #1,ypos(a4)
    move.b  #1,thief_up_move_lock
    rts
.cant_move_left
    ; can happen at start when thief tries to reach first objective
    ; same y but no direct path: move up (always possible)
    ; and lock until can move in the required lateral direction
    subq.w   #1,ypos(a4)
    st.b    thief_up_move_lock
    rts
    

move_hang
    ; does nothing, just increases timer for hang animation
    move.w  fall_hang_timer(a4),d0
    addq.w  #1,d0
    cmp.w   #30,d0
    bne.b   .no_change
    clr.w   d0
    eor.w  #4,fall_hang_toggle(a4)
.no_change
    move.w  d0,fall_hang_timer(a4)
    rts

move_crash
    move.w  fall_hang_timer(a4),d0
    addq.w  #1,d0
    cmp.w   #12,d0
    bne.b   .no_change
    clr.w   d0
    eor.w  #4,fall_hang_toggle(a4)
.no_change
    move.w  d0,fall_hang_timer(a4)
    rts
    
move_fall
    ; fall and crater or keep falling and respawn on top if power state ends
    ; before enemy hits the ground
    ; use timer to animate   

    move.w  fall_hang_timer(a4),d0
    addq.w  #1,d0
    cmp.w   #12,d0
    bne.b   .no_change
    clr.w   d0
    eor.w  #4,fall_hang_toggle(a4)
.no_change
    move.w  d0,fall_hang_timer(a4)
    ; add 1 or 2
    move.w  ypos(a4),d1
    tst.w   power_state_counter
    beq.b   .no_power
    cmp.w   #MAZE_HEIGHT,d1
    bcc.b   .crash
    bra.b   .no_wrap
.no_power
    ; power state is already ended
    move.w  d1,previous_ypos(a4)    
    tst.w   d1
    bpl.b   .no_neg
    ; negative: no double fall speed
    clr.w   d0
.no_neg
    ; make sure we get the proper range
    cmp.w   #MAZE_HEIGHT+12,d1
    beq.b   .wrap
    cmp.w   #MAZE_HEIGHT+13,d1
    bne.b   .no_wrap
.wrap
    move.w #-8,d1       ; wrap up
.no_wrap    
    btst    #0,d0
    beq.b   .one
    add.w #1,d1
.one
    add.w  #1,d1
    move.w  d1,ypos(a4)
    bne.b   .no_zero
    ; to the top, reset
    move.w  previous_mode(a4),mode(a4)
.no_zero
    rts
.crash
    move.w  #MAZE_HEIGHT,ypos(a4)
    move.w  #MODE_CRASH,mode(a4)
    lea enemy_hit_sound,a0
    bsr play_fx
.no_first
    rts
    
    
    
play_loop_fx
    tst.b   demo_mode
    bne.b   .nosfx
    lea _custom,a6
    bra _mt_loopfx
.nosfx
    rts
    
; what: sets game state when all 4 corners are completed
; trashes: A0,D0
all_four_corners_done
    movem.l  d1-d2/a1,-(a7)
    ; resets next enemy eaten score
    clr.w  next_enemy_iteration_score
    lea enemies(pc),a0
    move.w  nb_enemies_but_thief(pc),d7
    addq.w  #1,d7
    move.w  d7,nb_enemies_to_eat
    subq.w  #1,d7
.gloop
    move.w  mode(a0),d0
	cmp.w	#MODE_HANG,d0
	bcc.b	.skip		; can only happen with cheat keys
	move.w	d0,previous_mode(a0)
    st.b  fright_mode(a0)
.skip
    add.w   #Enemy_SIZEOF,a0    
    dbf d7,.gloop
    
    ; change music
    move.l  #2,d0
    bsr play_music
    move.w  #POWER_SONG_LENGTH,power_song_countdown
    ; timer depends on level I suppose...
    move.w  #POWER_STATE_LENGTH,power_state_counter

    bsr set_enemy_power_state_palette
    movem.l (a7)+,d1-d2/a1
    rts
    
set_enemy_power_state_palette
    movem.l  d1/a1/a2,-(a7)
    lea enemies(pc),a0
    move.l  fright_palette(pc),a2   ; same for all enemies
    move.w  nb_enemies_but_thief(pc),d0       ; plus one
.gloop
    tst.b	fright_mode(a0)
    beq.b   .next
    move.l  color_register(a0),a1
    ; set/reset palette
    move.l  (a2),(a1)+
    move.l  (4,a2),(a1)
.next
    add.w   #Enemy_SIZEOF,a0
    dbf d0,.gloop
    movem.l  (a7)+,d1/a1/a2
    rts
    
set_enemy_power_blink_palette
    movem.l  d1/a1/a2,-(a7)
    lea enemies(pc),a0
    move.l  fright_blink_palette(pc),a2   ; same for all enemies
    move.w  nb_enemies_but_thief(pc),d0       ; plus one
.gloop
    tst.b	fright_mode(a0)
    beq.b   .next
    move.l  color_register(a0),a1
    ; set/reset palette
    move.l  (a2),(a1)+
    move.l  (4,a2),(a1)
.next
    add.w   #Enemy_SIZEOF,a0
    dbf d0,.gloop
    movem.l  (a7)+,d1/a1/a2
    rts

; < A0: ghost structure
; trashes: D0,A0,A1
set_enemy_normal_palette
    move.w  nb_enemies_but_thief(pc),d0
    lea enemies(pc),a0
.gloop
    move.l  color_register(a0),a1
    ; set/reset palette
    move.l  palette(a0),(a1)+
    move.l  palette+4(a0),(a1)
.next
    add.w   #Enemy_SIZEOF,a0
    dbf d0,.gloop
    rts
    
update_player
    lea     player(pc),a4
    ; no moves (zeroes horiz & vert)
    clr.l  h_speed(a4)  

    move.w  player_killed_timer(pc),d6
    bmi.b   .alive
    moveq.w #8,d0
    cmp.w   #2*PLAYER_KILL_TIMER/3,d6
    bcs.b   .no_first_frame
    moveq.w #4,d0
    bra.b   .frame_done
.no_first_frame
    cmp.w   #PLAYER_KILL_TIMER/3,d6
    bcs.b   .no_second_frame
    moveq.w #0,d0
.no_second_frame

.frame_done    
    move.w  d0,death_frame_offset   ; 0,4,8
    rts
.alive
    tst.w   fright_timer
    beq.b   .no_fright1
    sub.w   #1,fright_timer
    bne.b   .no_fright1
    ; fright mode just ended: resume normal sound loop
    nop
.no_fright1

    
.okmove

    move.l  joystick_state(pc),d0
    IFD    RECORD_INPUT_TABLE_SIZE
    bsr     record_input
    ENDC
    tst.b   demo_mode
    beq.b   .no_demo
    ; if fire is pressed, end demo, goto start screen
    btst    #JPB_BTN_RED,d0
    beq.b   .no_demo_end
    clr.b   demo_mode
    move.w  #STATE_GAME_START_SCREEN,current_state
    rts
.no_demo_end
    clr.l   d0
    ; demo running
    ; read next timestamp
    move.l  record_data_pointer(pc),a0
    cmp.l   record_data_end(pc),a0
    bcc.b   .no_demo        ; no more input
    move.b  (a0),d2
    lsl.w   #8,d2
    move.b  (1,a0),d2
    ;;add.b   #3,d2   ; correction???
    cmp.w  record_input_clock(pc),d2
    bne.b   .repeat        ; don't do anything now
    ; new event
    move.b  (2,a0),d2
    addq.w  #3,a0
    move.l  a0,record_data_pointer
	move.b	d2,previous_move
	bra.b	.cont
.repeat
	move.b	previous_move(pc),d2
.cont
    btst    #LEFT>>2,d2
    beq.b   .no_auto_left
    bset    #JPB_BTN_LEFT,d0
    bra.b   .no_auto_right
.no_auto_left
    btst    #RIGHT>>2,d2
    beq.b   .no_auto_right
    bset    #JPB_BTN_RIGHT,d0
.no_auto_right
    btst    #UP>>2,d2
    beq.b   .no_auto_up
    bset    #JPB_BTN_UP,d0
    bra.b   .no_auto_down
.no_auto_up
    btst    #DOWN>>2,d2
    beq.b   .no_auto_down
    bset    #JPB_BTN_DOWN,d0
.no_auto_down
    btst    #FIRE,d2
    beq.b   .no_auto_fire
    bset    #JPB_BTN_RED,d0
.no_auto_fire
    
    ; read live or recorded controls
.no_demo
    tst.w   jump_index
    bpl.b   .while_jumping

    tst.l   d0
    beq.b   .out        ; nothing is currently pressed: optimize
    btst    #JPB_BTN_RED,d0
    beq.b   .no_jump
    tst.w   power_state_counter
    bne.b   .no_jump    ; can't jump when power state is active
    move.b  nb_stars(pc),d1
    beq.b   .no_jump

    subq.b  #1,d1
    move.b  d1,nb_stars
    st.b    delete_last_star_message
    lea     jump_sound,a0
    move.l  d0,-(a7)
    bsr     play_fx
    move.l  (a7)+,d0
    bsr     enemies_jump
    ; first time index is -1, and becomes 0
.while_jumping
    move.w  jump_index(pc),d1
    add.w   #1,d1
    cmp.w   #jump_height_table_end-jump_height_table,d1
    bne.b   .keep_jump
    bsr.b   enemies_previous_state
    moveq   #-1,d1
.keep_jump
    move.w  d1,jump_index
.no_jump
    btst    #JPB_BTN_RIGHT,d0
    beq.b   .no_right
    move.w  #1,h_speed(a4)
    bra.b   .vertical
.no_right
    btst    #JPB_BTN_LEFT,d0
    beq.b   .vertical
    move.w  #-1,h_speed(a4)  
.vertical
    btst    #JPB_BTN_UP,d0
    beq.b   .no_up
    move.w  #-1,v_speed(a4)
    bra.b   .out
.no_up
    btst    #JPB_BTN_DOWN,d0
    beq.b   .no_down
    move.w  #1,v_speed(a4)
.no_down    
.out
    bsr     .move_attempt
    tst.w   d5
    beq.b   .no_move
    
    cmp.w   #3,d5
    beq.b   .valid_move
    ; invalid move
    ; first pass: check if there's an intersection nearby past the player
    ; for that we have to check against every facing direction
    move.w  direction(a4),d6
    lea     dircheck_table(pc),a0
    move.l  (a0,d6.w),a0
    jsr     (a0)
    
    ; second pass just try to see if latest move would work ("corner cut")
    move.l  previous_valid_direction(pc),d6
    beq.b   .no_move
	; it would work, store it
    move.l  d6,h_speed(a4)
    bsr     .move_attempt
    cmp.w   #3,d5
    beq.b   .valid_move
    ; nothing to do, previous move wasn't valid
    bra.b   .no_move
.valid_move
	; move is valid
    ; store for later
    move.l  h_speed(a4),previous_valid_direction
    bsr animate_player    
    move.w  d2,xpos(a4)
    move.w  d3,ypos(a4)

    ; check if there are dots to eat
    move.w  d2,d0
    move.w  d3,d1
    
    tst.b   rustler_level
    bne.b   .rustler
    bsr get_tile_type
    cmp.b   #U,(a0)
    bne.b   .z3
    ; eat dot
    move.b   #F,(a0)
    ; restore x,y to get rectangles
    move.w  d2,d0
    move.w  d3,d1

    ; set registers d4-d6 with pointers on rectangles
    bsr  get_dot_rectangles

    ; clear dot
    lea    eat_sound,a0
    bsr     play_fx
    ; add 10 to the score
    moveq.l #1,d0
    bsr add_to_score

    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.w  d2,d0
    move.w  d3,d1
    addq.w  #8,d0
    addq.w  #1,d1
    cmp.w   #UP,direction(a4)
    beq.b   .noadd
    cmp.w   #DOWN,direction(a4)
    beq.b   .add8
    addq.w  #2,d1
    bra.b   .noadd
.add8    
    addq.l  #8,d1
.noadd

    ADD_XY_TO_A1    a0
    
    tst.l   d4
    beq.b   .z	; can happen at startup
    bsr clear_dot
    move.l  d4,a0
    bsr     count_dot
.z
    tst.l   d5
    beq.b   .z2
    move.l  d5,a0
    bsr     count_dot
.z2
    tst.l   d6
    beq.b   .z3
    move.l  d6,a0
    bsr     count_dot
.z3
    rts
    
    ; this is the tough part :)
    ; paint management
.rustler
    ; enters here with d0-D1 and D2-D3 as x,y
    ; handle paint
    
    ; get current tile type
    bsr get_tile_type
    clr.w   d0

    
    ; a0 points on tile type
    ; compose transition id by combining previous & current tile type
    move.b  previous_tile_type(pc),d0
    lsl.w   #2,d0
    move.b  (a0),d1     ; current tile type

    or.b  d1,d0
    move.b  d1,previous_tile_type
    
    IFD     DEBUG_MODE
    cmp.w   #16,d0
    bcs.b   .limitsok
    blitz
    illegal
.limitsok
    ENDC
    ; transitions
    add.w   d0,d0
    add.w   d0,d0
    lea     state_transition_table(pc),a1
    move.l  (a1,d0.w),a1
    move.w  d2,d0
    move.w  d3,d1
	
    jmp     (a1)
        

.no_move
    tst.b   rustler_level
    bne.b   stop_paint_sound
    
    rts

    
.move_attempt
    ; cache xy in regs / save them
    move.w  xpos(a4),d2
    move.w  ypos(a4),d3

    clr.w   d5
    ;;move.w  direction(a4),d6
    ; test if player has requested an "up" move
    move.w  v_speed(a4),d6
    beq.b   .no_vertical
    bset    #0,d5   ; note that we attempted a move
    ; up/down move requested
    move.w  d2,d0
    and.b   #7,d0
    bne.b   .no_vertical    ; invalidated: not aligned in x
    move.w  d2,d0
    move.w  d3,d1
    add.w   d6,d1  ; change y
    bmi.b   .no_vertical
    cmp.w   #Y_MAX+1,d1
    bcc.b   .no_vertical
    tst.w   d6
    bmi.b   .up1
    ; to the right: add 7
    addq.w  #7,d1
.up1
    bsr     is_location_legal
    tst.b   d0
    beq.b   .no_vertical
    ; validate
	move.w	direction(a4),previous_direction(a4)	
    add.w   d6,d3  ; change y
    bset    #1,d5
    tst.w   d6
    bmi.b   .up
    move.w  #DOWN,direction(a4)
    bra.b   .no_vertical
.up
    move.w  #UP,direction(a4)
.no_vertical
    move.w   h_speed(a4),d6
    beq.b   .no_horizontal
    bset    #0,d5   ; note that we attempted a move
    ; left/right move requested
    move.w  d3,d1
    and.b   #7,d1
    bne.b   .no_horizontal    ; invalidated: not aligned in y
    move.w  d2,d0
    move.w  d3,d1
    add.w   d6,d0  ; change x
    bmi.b   .no_horizontal
    cmp.w   #X_MAX+1,d0
    bcc.b   .no_horizontal
    tst.w   d6
    bmi.b   .left1
    ; to the right: add 7
    addq.w  #7,d0
.left1
    bsr     is_location_legal
    tst.b   d0
    beq.b   .no_horizontal
    ; validate
	move.w	direction(a4),previous_direction(a4)	
    add.w   d6,d2  ; change x
    bset    #1,d5
    tst.w   d6
    bmi.b   .left
    move.w  #RIGHT,direction(a4)
    bra.b   .no_horizontal
.left
    move.w  #LEFT,direction(a4)
.no_horizontal
    rts

dircheck_table
    dc.l    dircheck_right
    dc.l    dircheck_left
    dc.l    dircheck_up
    dc.l    dircheck_down

CORRECTIVE_TEST_OFFSET = 8

	
; d2 contains X
; d3 contains Y
; we use d4
; those routines switch signs on previous_valid_direction if
; game senses that the player has missed a turn
; which complements the "continue" move when a turn is anticipated
;
; both mechanisms ensure that the player never misses a turn, which can
; be fatal in that kind of game
;
; this is still not right
dircheck_right
    move.w  v_speed(a4),d4
    beq.b   just_rts
    move.w  d2,d0
    move.w  d3,d1
    ; align on previous x tile
    and.w   #$F8,d0
    bra.b	dircheck_horiz
	
dircheck_left
    move.w  v_speed(a4),d4
    beq.b   just_rts
    move.w  d2,d0
    move.w  d3,d1
    ; align on previous x tile
    addq.w   #8,d0
dircheck_horiz    
    cmp.w   #1,d4
    bne.b   .no_down
    addq.w  #CORRECTIVE_TEST_OFFSET,d1
    bsr     is_location_legal
    tst.b   d0
    bne.b   .reverse_horiz_direction
    rts
.no_down
    ; has to be "up"
    ; up attempt
    subq.w  #CORRECTIVE_TEST_OFFSET,d1
    bsr     is_location_legal
    tst.b   d0
    bne.b	.reverse_horiz_direction
.no_up
    rts

.reverse_horiz_direction
    ; looks like player went past the "up" intersection: change direction
    neg.w  previous_valid_direction
	rts
	
dircheck_up
    move.w  h_speed(a4),d4
    beq.b   just_rts
    move.w  d2,d0
    move.w  d3,d1
    ; align on lower y tile
    
    addq.w   #8,d1
	bra.b	dircheck_vert
	
dircheck_down
    move.w  h_speed(a4),d4
    beq.b   just_rts
    move.w  d2,d0
    move.w  d3,d1
    ; align on upper y tile
    
    and.w   #$F8,d1

dircheck_vert
    cmp.w   #1,d4
    bne.b   .no_right
    addq.w  #CORRECTIVE_TEST_OFFSET,d0
    bsr     is_location_legal
    tst.b   d0
    bne.b   .reverse_vert_direction
    rts
.no_right
    ; has to be "left"
    ; left attempt
    subq.w  #CORRECTIVE_TEST_OFFSET,d0
    bsr     is_location_legal
    tst.b   d0
    bne.b   .reverse_vert_direction
    rts	

.reverse_vert_direction
    ; looks like player went past the "up" intersection: change direction
    neg.w  previous_valid_direction+2
	rts    

just_rts
	rts
	


play_paint_sound
    tst.b   was_playing_paint_sound
    bne.b   .already_painting
    move.l  a0,-(a7)
    lea paint_sound,a0
    bsr play_loop_fx
    move.l  (a7)+,a0
    st.b    was_playing_paint_sound
.already_painting
    rts
    
   
stop_paint_sound:
    movem.l d0/a6,-(a7)
    lea _custom,a6
    moveq.l #3,d0
    bsr     _mt_stopfx      ; stop paint loop
    clr.b   was_playing_paint_sound
    movem.l (a7)+,d0/a6
    rts

set_stored_tiles:
    move.b  d0,previous_tile_type
    lea     rollback_dot_table_buffer,a0
    move.l  rollback_dot_table_pointer(pc),d1
.ssloop
    cmp.l   a0,d1
    beq.b   .t2f_out
    move.l  (a0)+,a1
    move.b  d0,(a1)
    bra.b   .ssloop
.t2f_out
    rts
    

    
rollback_paint:
    bsr stop_paint_sound
    ; convert temp paint to no paint
    move.b  #U,d0
    bsr     set_stored_tiles

    ; re-add one dot to each rectangle pointer stored
    lea     rollback_rectangle_buffer,a0
    move.l  rollback_rectangle_pointer(pc),d1
.readd_dot
    cmp.l   a0,d1
    beq.b   .rad_out
    move.l  (a0)+,a1
    addq.w  #1,cdots(a1)  
    bra.b   .readd_dot
.rad_out    
    ; unpaint zone, in the inverse order it was painted to avoid
	; side effect issues. This is just slightly trickier because of
	; boundary check which must be done at 2/3 different places
    lea     rollback_paint_zone_buffer,a0
	move.l	a0,d1
    move.l  rollback_paint_zone_pointer(pc),a0
    cmp.l   a0,d1	; check end of list immediately in case it's empty!
    beq.b   .up_out
    clr.l   d5
.unpaint:
    move.w  -(a0),d3
    move.l  -(a0),a1
    cmp.l   a1,d5
    bne.b   .no_same
    ; same means left upper corner
    ; add y offset and reduce height
    ;clr.l   d4
    ;bra.b   .unpaint
    ; case 1
    ; up then left with same address
    ; change mask of horizontal

    cmp.w   #RIGHT,d3
    beq.b   .horiz
    cmp.w   #DOWN,d3
    beq.b   .down
    bra.b   .no_same
.horiz
    ; handle unpainting the corner
    ; plane and backup plane
    ; (even if backup plane doesn't seem
    ; needed since player is far away when unpaint happens)
    clr.b   (a1)
    clr.b   (NB_BYTES_PER_LINE,a1)
    
    move.l  a1,d0
    lea     screen_data,a2
    sub.l   a2,d0
    sub.l   #SCREEN_PLANE_SIZE,d0
    ; now D0 is the offset of any plane
    lea     paint_backup_plane,a2
    add.l   d0,a2       ; offset of data to copy    
    clr.b   (a2)
    clr.b   (NB_BYTES_PER_LINE,a2)    

    cmp.l   a0,d1	; check end of list
    beq.b   .up_out
    
    bra.b   .unpaint
.down
    add.w  #NB_BYTES_PER_LINE*2,a1
    ;;st.b    smaller_vertical_paint
.no_same
    move.l  a1,d5
    bsr     unpaint_zone
    cmp.l   a0,d1	; check end of list
    bne.b   .unpaint
.up_out
    
    ; reset pointers to start of lists
    ;;bra     reset_rollback_pointers
    
reset_rollback_pointers:
    move.l  #rollback_paint_zone_buffer,rollback_paint_zone_pointer
    move.l  #rollback_rectangle_buffer,rollback_rectangle_pointer
    move.l  #rollback_dot_table_buffer,rollback_dot_table_pointer
    move.l  #pending_paint_rectangle_buffer,pending_paint_rectangle_pointer
    rts
    
enemies_jump
    ; cycle jump frames for each jump
    move.w  jump_frame(pc),d7
    add.w   #4,d7       ; four by four avoids shifting to get offset
    cmp.w   #JUMP_FIRST_FRAME+7*4,d7
    bne.b   .nowrap
    move.w  #JUMP_FIRST_FRAME,d7
.nowrap
    move.w  d7,jump_frame
    
    lea enemies(pc),a0
    move.w  nb_enemies_but_thief(pc),d7
.jumploop
    move.w   mode(a0),previous_mode(a0)
    move.w  #MODE_JUMP,mode(a0)
    add.w   #Enemy_SIZEOF,a0
    dbf d7,.jumploop
    rts
    
enemies_previous_state
    lea enemies(pc),a0
    move.w  nb_enemies_but_thief(pc),d7
.jumploop
    move.w   previous_mode(a0),mode(a0)
    add.w   #Enemy_SIZEOF,a0
    dbf d7,.jumploop
    rts
    
; what: count dots and draws the rectangle if 0 dots
; (dirty: draws during compute phase)
; < A0: pointer on rectangle
; destroys: D0
count_dot
    move.w  cdots(a0),d0
    beq.b   .no_dots
    subq.w    #1,d0
    bne.b   .still_dots
    bsr     fill_rectangle
    subq.b  #1,nb_rectangles
    beq.b   level_completed
    clr     d0
.still_dots
    move.w  d0,cdots(a0)
.no_dots
    rts

; what: count dots and draws the rectangle if all surface is painted
; (dirty: draws during compute phase)
; < A0: pointer on rectangle
; destroys: D0

tile_painted
    move.w  cdots(a0),d0
    beq.b   .no_dots

    ; store A0 in list for rollback
    bsr store_rectangle_rollback_address

    subq.w  #1,cdots(a0)
    bne.b   .still_dots
    
    ; no more dots
    ; almost time to fill the rectangle
    ;
    ; put the rectangle in pending paint
    ; will be validated when player enters a painted zone

    bsr store_rectangle_pending_paint_address
    
.still_dots
.no_dots
    rts

; < A0: rectangle address to store for rollback
store_rectangle_rollback_address
    move.l  a1,-(a7)
    move.l  rollback_rectangle_pointer(pc),a1
    move.l  a0,(a1)+
    IFD     DEBUG_MODE
    cmp.l   #rollback_rectangle_buffer_end,a1
    bcs.b   .ok
    blitz
    illegal
.ok
    ENDC
    move.l  a1,rollback_rectangle_pointer
    move.l  (a7)+,a1
    rts
    
; < A0: rectangle address to store for pending paint
store_rectangle_pending_paint_address
    move.l  a1,-(a7)
    move.l  pending_paint_rectangle_pointer(pc),a1
    move.l  a0,(a1)+
    move.l  a1,pending_paint_rectangle_pointer
    move.l  (a7)+,a1
    rts
    
; draw a line inside the rectangle

DRAW_RECT_LINE:MACRO
    or.b  #$3F,(\2,\1)
    st  (\2+1,\1)
    st   (\2+2,\1)
    st  (\2+3,\1)
    st  (\2+4,\1)
    ENDM
    
; draw horizontal outline
DRAW_RECT_HORIZ_OUTLINE:MACRO
    or.b  #$7F,(\2,\1)
    st  (\2+1,\1)
    st  (\2+2,\1)
    st  (\2+3,\1)
    st  (\2+4,\1)
    or.b  #$C0,(\2+5,\1)
    ENDM
; draw vertical outline
DRAW_RECT_VERT_OUTLINE:MACRO
    or.b  #$C0,(\2,\1)		; left
    or.b  #$C0,(\2+5,\1)		; right
    ENDM
    
; < A0: pointer to rectangle structure

fill_rectangle: 
    ; fill rectangle with color (plane 1)
    movem.l d1-d2/a0-a4,-(a7)
    tst.w   specrect(a0)
    beq.b   .no_specrect
    subq.b  #1,nb_special_rectangles
    bne.b   .no_specrect
    ; can eat enemies
    move.w  #1,can_eat_enemies_mode_pending
.no_specrect
    ; plane inc.8,8 offset
    lea screen_data+1+(8*NB_BYTES_PER_LINE),a1
    move.w  hrect(a0),d2
    lsl.w   #3,d2
    subq.w  #3,d2   ; not that wide
    move.w  xrect(a0),d0
    move.w  yrect(a0),d1
    ADD_XY_TO_A1       a0
    move.l  a1,d0
    sub.l   #screen_data,d0 ; offset
    lea grid_backup_plane,a2
    lea rect_backup_plane,a3
    lea paint_backup_plane,a4
    add.l   d0,a2
    add.l   d0,a3
    add.l   d0,a4
    move.w  #NB_BYTES_PER_LINE,d1
    tst.b   rustler_level
    beq.b   .filly
    DRAW_RECT_HORIZ_OUTLINE  a1,SCREEN_PLANE_SIZE-NB_BYTES_PER_LINE*1
    DRAW_RECT_HORIZ_OUTLINE  a1,SCREEN_PLANE_SIZE-NB_BYTES_PER_LINE*2
    DRAW_RECT_HORIZ_OUTLINE  a4,-NB_BYTES_PER_LINE*1
    DRAW_RECT_HORIZ_OUTLINE  a4,-NB_BYTES_PER_LINE*2
.filly

    DRAW_RECT_LINE  a1,0
    DRAW_RECT_LINE  a2,0
    DRAW_RECT_LINE  a3,0
    DRAW_RECT_LINE  a1,SCREEN_PLANE_SIZE*2
    
    tst.b   rustler_level
    beq.b   .no_vert_outline
    DRAW_RECT_VERT_OUTLINE  a1,0
    DRAW_RECT_VERT_OUTLINE  a4,0
    add.w   d1,a4
.no_vert_outline
    add.w   d1,a1
    add.w   d1,a2
    add.w   d1,a3
    dbf d2,.filly
    
    tst.b   rustler_level
    beq.b   .no_outline
    DRAW_RECT_HORIZ_OUTLINE  a4,0
    DRAW_RECT_HORIZ_OUTLINE  a4,NB_BYTES_PER_LINE
    DRAW_RECT_HORIZ_OUTLINE  a1,SCREEN_PLANE_SIZE
    DRAW_RECT_HORIZ_OUTLINE  a1,SCREEN_PLANE_SIZE+NB_BYTES_PER_LINE
.no_outline
    movem.l (a7)+,d1-d2/a0-a4
    
    rts
    
    IFD    RECORD_INPUT_TABLE_SIZE
record_input:
	cmp.l	prev_record_joystick_state(pc),d0
	beq.b	.no_input	; no need to re-record same input
	tst.l	d0
	bne.b	.store
    ; no input twice: ignore (saves space, same result)
    tst.l   prev_record_joystick_state
    beq.b   .no_input
.store
    move.l  d0,prev_record_joystick_state
    clr.b   d1
    ; now store clock & joystick state, "compressed" to 5 bits (up,down,left,right,fire)
    btst    #JPB_BTN_RIGHT,d0
    beq.b   .norr
    bset    #RIGHT>>2,d1
    bra.b   .norl
.norr
    btst    #JPB_BTN_LEFT,d0
    beq.b   .norl
    bset    #LEFT>>2,d1
.norl
    btst    #JPB_BTN_UP,d0
    beq.b   .noru
    bset    #UP>>2,d1
    bra.b   .nord
.noru
    btst    #JPB_BTN_DOWN,d0
    beq.b   .nord
    bset    #DOWN>>2,d1
.nord
    btst    #JPB_BTN_RED,d0
    beq.b   .norf
    bset    #FIRE,d1
.norf
    move.l record_data_pointer(pc),a0
    cmp.l   #record_input_table+RECORD_INPUT_TABLE_SIZE-4,a0
    bcc.b   .no_input       ; overflow!!!
    
    ; store clock
    move.b  record_input_clock(pc),(a0)+
    move.b  record_input_clock+1(pc),(a0)+
	; store move
    move.b  d1,(a0)+
    ; update pointer
    move.l  a0,record_data_pointer
.no_input
    rts
    ENDC
    
; called when pacman moves
; < A4: pac player
animate_player
    addq.w  #1,frame(a4)
    cmp.w   #(rustler_anim_left_end-rustler_anim_left)/4,frame(a4)
    bne.b   .no_floop
    clr.w   frame(a4)
.no_floop
    rts


level_completed:
	; avoids bonus music when level is completed with
	; one of the corners
	clr.w	can_eat_enemies_mode_pending
    bsr stop_sounds
    moveq.l  #8,d0
    bsr play_music
    st.b    next_level_is_bonus_level
    move.w  #ORIGINAL_TICKS_PER_SEC*6,completed_music_timer
    rts

    
; the palette is organized so we only need to blit planes 0, 2 and 3 (not 1)
; plane 1 contains dots so it avoids to redraw it
; plane 0 contains the grid, that has been backed up
; plane 3 contains filled up rectangles, needs backing up too
draw_player:
    move.l  previous_player_address(pc),d5
    bne.b   .not_first_draw
    moveq.l #-1,d5
.not_first_draw
    ; first, restore plane 0
    tst.l   d5    
    bmi.b   .no_erase
    ; restore plane 0 using CPU
    lea grid_backup_plane,a0    
    lea screen_data,a1
    sub.l   a1,d5       ; d5 is now the offset
    ; d0 is the offset: add it
    add.l   d5,a1
    add.l   d5,a0
    ; now copy a rectangle of the saved screen
    REPT    18
    move.l   ((REPTN-1)*NB_BYTES_PER_LINE,a0),((REPTN-1)*NB_BYTES_PER_LINE,a1)
    ENDR

    tst.b   rustler_level
    beq.b   .no_erase
    add.w   #SCREEN_PLANE_SIZE,a1
    lea     paint_backup_plane,a0
    add.l   d5,a0
    ; now copy a rectangle of the saved screen
    REPT    18
    move.l   ((REPTN-1)*NB_BYTES_PER_LINE,a0),((REPTN-1)*NB_BYTES_PER_LINE,a1)
    ENDR
    
.no_erase

    lea     player(pc),a2
    tst.w  player_killed_timer
    bmi.b   .normal
    lea     copier_dead_table,a0
    tst.b   rustler_level
    beq.b   .no_rust
    lea     rustler_dead_table,a0
.no_rust
    move.w  death_frame_offset(pc),d0
    add.w   d0,a0       ; proper frame to blit
    move.l  (a0),a0
    bra.b   .pacblit
.normal

    move.w  direction(a2),d0
    lea  copier_dir_table(pc),a0
    tst.b   rustler_level
    beq.b   .cont
    lea  rustler_dir_table(pc),a0    
.cont
    move.l  (a0,d0.w),a0
    move.w  frame(a2),d0
    add.w   d0,d0
    add.w   d0,d0
    move.l  (a0,d0.w),a0
.pacblit

    move.w  xpos(a2),d3    
	addq.w	#1,d3	; X-offset
    move.w  ypos(a2),d4
    ; center => top left
    moveq.l #-1,d2 ; mask

    lea	screen_data,a1

    ; apply X offset
    addq.w #2,d0
    
    move.l  a1,a6
    move.w d3,d0
    move.w d4,d1

    ; plane 0
    move.l  a1,a2
    lea (BOB_16X16_PLANE_SIZE*4,a0),a3
    bsr blit_plane_cookie_cut
    move.l  a1,previous_player_address
    
    ; remove previous second plane before blitting the new one
    ; nice as it works in parallel with the first plane blit started above
    tst.l   d5
    bmi.b   .no_erase2
        
    ; restore plane 2
    lea   screen_data+SCREEN_PLANE_SIZE*2,a1
    lea rect_backup_plane,a4    
    add.l   d5,a4
    add.l   d5,a1
    ; now copy a rectangle of the saved screen
    REPT    18
    move.l   ((REPTN-1)*NB_BYTES_PER_LINE,a4),((REPTN-1)*NB_BYTES_PER_LINE,a1)
    ENDR

.no_erase2    
    tst.b   rustler_level
    beq.b   .no_plane_1
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.l  a1,a2   ; just restored background
    ; plane 2
    ; a3 is already computed from first cookie cut blit
    lea (BOB_16X16_PLANE_SIZE,a0),a0
    move.l  a1,a6
    move.w d3,d0
    move.w d4,d1

    bsr blit_plane_cookie_cut
    lea (BOB_16X16_PLANE_SIZE,a0),a0
    bra.b   .plane_1
.no_plane_1
    
    lea (BOB_16X16_PLANE_SIZE*2,a0),a0
.plane_1
    lea	screen_data+SCREEN_PLANE_SIZE*2,a1
    move.l  a1,a2   ; just restored background
    ; plane 2
    ; a3 is already computed from first cookie cut blit
    move.l  a1,a6
    move.w d3,d0
    move.w d4,d1

    bsr blit_plane_cookie_cut
    
    ; delete third plane too
    tst.l   d5    
    bmi.b   .no_erase3
    
    ; clear plane 3
    lea   screen_data+SCREEN_PLANE_SIZE*3,a1
    add.l   d5,a1
    ; now copy a rectangle of the saved screen
    REPT    18
    clr.l   ((REPTN-1)*NB_BYTES_PER_LINE,a1)
    ENDR

.no_erase3
    
    move.w d3,d0
    move.w d4,d1

    lea (SCREEN_PLANE_SIZE,a6),a1
    lea (BOB_16X16_PLANE_SIZE,a0),a0    ; next plane for bitmap
    ; plane 3
    bra blit_plane

    
; < d0.w: x
; < d1.w: y
; > d0.L: control word
store_sprite_pos
    movem.l  d1/a0/a1,-(a7)

    lea	HW_SpriteXTable(pc),a0
    lea	HW_SpriteYTable(pc),a1

    add.w	d0,d0
    add.w	d0,d0
    move.l	(a0,d0.w),d0
    add.w	d1,d1
    add.w	d1,d1
    or.l	(a1,d1.w),d0
    movem.l  (a7)+,d1/a0/a1
    rts


direction_speed_table
    ; right
    dc.w    1,0
    ; left
    dc.w    -1,0
    ; up
    dc.w    0,-1
    ; down
    dc.w    0,1
    
grid_align_table
    REPT    320
    dc.w    (REPTN&$1F8)+4
    ENDR
    
HW_SpriteXTable
  rept 320
x   set REPTN+$80
    dc.b  0, x>>1, 0, x&1
  endr


HW_SpriteYTable
  rept 260
ys  set REPTN+$2c
ye  set ys+16       ; size = 16
    dc.b  ys&255, 0, ye&255, ((ys>>6)&%100) | ((ye>>7)&%10)
  endr

 
    
; what: returns which rectangle(s) contain the current x,y
; 
; args:
; < d0 : x (screen coords)
; < d1 : y
; > d4,d5,d6: pointers on linked rectangles (either can be NULL)
; trashes: none

get_dot_rectangles:
    cmp.w   #Y_MAX+1,d1
    bcc.b   .out_of_bounds
    cmp.w   #X_MAX+1,d0
    bcc.b   .out_of_bounds
    ; no need to test sign (bmi) as bcc works unsigned so works on negative!
    ; apply x,y offset
    add.w   #4,d1       ; center
    
    lsr.w   #3,d1       ; 8 divide : tile
    move.l  a0,-(a7)
    lea     mul26_table(pc),a0
    add.w   d1,d1
    move.w  (a0,d1.w),d1    ; times 26
    lsl.w   #4,d1   ; times 16 (4 32 bit longwords per slot)
    move.l dot_table(pc),a0
    add.w   d1,a0
    and.b   #$F8,d0   ; align on 8 (2 32 bit longwords per slot)
    
    add.w   d0,a0
    add.w   d0,a0
    
    move.l  (a0)+,D4    ; retrieve value of first pointer
    move.l  (a0)+,D5    ; retrieve value of second pointer
    move.l  (a0),D6    ; retrieve value of third pointer
    move.l  (a7)+,a0
    
    rts
.out_of_bounds
    moveq.l   #0,d0
    moveq.l   #0,d1
    moveq.l   #0,d2
    rts
    
; what: checks if x,y collides with maze
; returns valid location out of the maze
; (allows to handle edges, with a limit given by
; the move methods)
; args:
; < d0 : x (screen coords)
; < d1 : y
; > d0.b : not 0 if maze, 0 if no maze
; out of bounds returns -1 which makes it legal to move to (edges)
; trashes: a0,a1,d1

is_location_legal:
    ; make up for center
    ; this is to simulate old behaviour
    sub.w   #4,d1       
    ;
    ; now get_tile_type is used to get the type of the tile
    ; and 4 is added to y like in rectangle fetch
    ;
    ; get_tile_type has slightly different requirements. But subtracting
    ; 4 to Y before calling it allows to re-use it in here
    bsr get_tile_type
    move.b  (a0),d0    ; retrieve value
    rts
    
; what: checks what is below x,y
; returns 0 out of the maze
; (allows to handle edges, with a limit given by
; the move methods)
; args:
; < d0 : x (screen coords)
; < d1 : y
; > a0: points on byte value to read (can be written to unless it points on negative value!!)
; which is 0 if no maze, 
;                  1 if has dot (or needs painting)
;                  2 if temp paint or dot eaten
;                  3 if fully painted
; trashes: a1,d0,d1

get_tile_type:
    cmp.w   #Y_MAX+1,d1
    bcc.b   .out_of_bounds
    cmp.w   #X_MAX+1,d0
    bcc.b   .out_of_bounds
    ; no need to test sign (bmi) as bcc works unsigned so works on negative!
    ; apply x,y offset
    add.w   #4,d1       ; center

    lsr.w   #3,d1       ; 8 divide : tile
    lea     mul26_table(pc),a0
    add.w   d1,d1
    move.w  (a0,d1.w),d1    ; times 26
    move.l maze_wall_table(pc),a0
    
    
    add.w   d1,a0
    lsr.w   #3,d0   ; 8 divide
    add.w   d0,a0
    move.b  (a0),d0    ; retrieve value
    rts
.out_of_bounds
    lea .minus_one(pc),a0  ; allowed, the move routine already has bounds, points on -1
    rts
   
.minus_one:
    dc.b    -1
    even
    
; what: blits 16x16 data on one plane
; args:
; < A0: data (16x16)
; < A1: plane
; < D0: X
; < D1: Y
; < D2: blit mask
; trashes: D0-D1
; returns: A1 as start of destination (A1 = orig A1+40*D1+D0/8)

blit_plane
    movem.l d2-d6/a2-a5,-(a7)
    lea $DFF000,A5
	move.l d2,d3
    move.w  #4,d2       ; 16 pixels + 2 shift bytes
    move.w  #16,d4      ; 16 pixels height
    bsr blit_plane_any_internal
    movem.l (a7)+,d2-d6/a2-a5
    rts
    
; what: blits 16x16 data on one plane, cookie cut
; args:
; < A0: data (16x16)
; < A1: plane  (40 rows)
; < A2: background (40 rows) to mix with cookie cut
; < A3: source mask for cookie cut (16x16)
; < D0: X
; < D1: Y
; < D2: blit mask
; trashes: D0-D1
; returns: A1 as start of destination (A1 = orig A1+40*D1+D0/16)

blit_plane_cookie_cut
    movem.l d2-d7/a2-a5,-(a7)
    lea $DFF000,A5
	move.l d2,d3	;masking of first/last word    
    move.w  #4,d2       ; 16 pixels + 2 shift bytes
    move.w  #16,d4      ; 16 pixels height   
    bsr blit_plane_any_internal_cookie_cut
    movem.l (a7)+,d2-d7/a2-a5
    rts
    
    
; what: blits (any width)x(any height) data on one plane
; args:
; < A0: data (width x height)
; < A1: plane
; < D0: X
; < D1: Y
; < D2: blit width in bytes (+2)
; < D3: blit mask
; < D4: blit height
; trashes: D0-D1, A1
;
; if A1 is already computed with X/Y offset and no shifting, an optimization
; skips the XY offset computation

blit_plane_any:
    movem.l d2-d6/a2-a5,-(a7)
    lea $DFF000,A5
    bsr blit_plane_any_internal
    movem.l (a7)+,d2-d6/a2-a5
    rts

; < A5: custom
; < D0,D1: x,y
; < A0: source
; < A1: plane pointer
; < D2: width in bytes (inc. 2 extra for shifting)
; < D3: blit mask
; < D4: blit height
; trashes D0-D6
; > A1: even address where blit was done
blit_plane_any_internal:
    ; pre-compute the maximum of shit here
    lea mul40_table(pc),a2
    swap    d1
    clr.w   d1
    swap    d1
    add.w   d1,d1
    beq.b   .d1_zero    ; optim
    move.w  (a2,d1.w),d1
.d1_zero
    move.l  #$09f00000,d5    ;A->D copy, ascending mode
    move    d0,d6
    beq.b   .d0_zero
    and.w   #$F,d6
    and.w   #$1F0,d0
    lsr.w   #3,d0
    add.w   d0,d1

    swap    d6
    clr.w   d6
    lsl.l   #8,d6
    lsl.l   #4,d6
    or.l    d6,d5            ; add shift
.d0_zero    
    add.l   d1,a1       ; plane position (always even)

	move.w #NB_BYTES_PER_LINE,d0
    sub.w   d2,d0       ; blit width

    lsl.w   #6,d4
    lsr.w   #1,d2
    add.w   d2,d4       ; blit height


    ; now just wait for blitter ready to write all registers
	bsr	wait_blit
    
    ; blitter registers set
    move.l  d3,bltafwm(a5)
	move.l d5,bltcon0(a5)	
	clr.w bltamod(a5)		;A modulo=bytes to skip between lines
    move.w  d0,bltdmod(a5)	;D modulo
	move.l a0,bltapt(a5)	;source graphic top left corner
	move.l a1,bltdpt(a5)	;destination top left corner
	move.w  d4,bltsize(a5)	;rectangle size, starts blit
    rts


; quoting mcgeezer:
; "You have to feed the blitter with a mask of your sprite through channel A,
; you feed your actual bob bitmap through channel B,
; and you feed your pristine background through channel C."

; < A5: custom
; < D0.W,D1.W: x,y
; < A0: source
; < A1: destination
; < A2: background to mix with cookie cut
; < A3: source mask for cookie cut
; < D2: width in bytes (inc. 2 extra for shifting)
; < D3: blit mask
; < D4: height
; blit mask set
; returns: start of destination in A1 (computed from old A1+X,Y)
; trashes: nothing

blit_plane_any_internal_cookie_cut:
    movem.l d0-d7,-(a7)
    ; pre-compute the maximum of shit here
    lea mul40_table(pc),a4
    swap    d1
    clr.w   d1
    swap    d1
    add.w   d1,d1
    move.w  d1,d6   ; save it
    beq.b   .d1_zero    ; optim
    move.w  (a4,d1.w),d1
.d1_zero
    move.l  #$0fca0000,d5    ;B+C-A->D cookie cut   

    move    d0,d7
    beq.b   .d0_zero
    and.w   #$F,d7
    and.w   #$1F0,d0
    lsr.w   #3,d0

    lsl.l   #8,d7
    lsl.l   #4,d7
    or.w    d7,d5            ; add shift to mask (bplcon1)
    swap    d7
    clr.w   d7
    or.l    d7,d5            ; add shift
    
    move.w  d0,d7
    add.w   d0,d1
    
.d0_zero
    ; make offset even. Blitter will ignore odd address
    ; but a 68000 CPU doesn't and since we RETURN A1...
    bclr    #0,d1
    add.l   d1,a1       ; plane position (long: allow unsigned D1)

    ; a4 is a multiplication table
    ;;beq.b   .d1_zero    ; optim
    move.w  (a4,d6.w),d1
    add.w   d7,a2       ; X
;;.d1_zero    
    ; compute offset for maze plane
    add.l   d1,a2       ; Y maze plane position

	move.w #NB_BYTES_PER_LINE,d0

    sub.w   d2,d0       ; blit width

    lsl.w   #6,d4
    lsr.w   #1,d2
    add.w   d2,d4       ; blit height

    ; always the same settings (ATM)

    ; now just wait for blitter ready to write all registers
	bsr	wait_blit
    
    ; blitter registers set

    move.l  d3,bltafwm(a5)
	clr.w bltamod(a5)		;A modulo=bytes to skip between lines
	clr.w bltbmod(a5)		;A modulo=bytes to skip between lines
	move.l d5,bltcon0(a5)	; sets con0 and con1

    move.w  d0,bltcmod(a5)	;C modulo (maze width != screen width but we made it match)
    move.w  d0,bltdmod(a5)	;D modulo

	move.l a3,bltapt(a5)	;source graphic top left corner (mask)
	move.l a0,bltbpt(a5)	;source graphic top left corner
	move.l a2,bltcpt(a5)	;pristine background
	move.l a1,bltdpt(a5)	;destination top left corner
	move.w  d4,bltsize(a5)	;rectangle size, starts blit
    
    movem.l (a7)+,d0-d7
    rts


; what: blits 16(32)x16 data on 4 planes (for bonuses), full mask
; args:
; < A0: data (16x16)
; < D0: X
; < D1: Y
; trashes: D0-D1

blit_4_planes
    movem.l d2-d6/a0-a1/a5,-(a7)
    lea $DFF000,A5
    lea     screen_data,a1
    moveq.l #3,d7
.loop
    movem.l d0-d1/a1,-(a7)
    move.w  #4,d2       ; 16 pixels + 2 shift bytes
    moveq.l #-1,d3  ; mask
    move.w  #16,d4      ; height
    bsr blit_plane_any_internal
    movem.l (a7)+,d0-d1/a1
    add.l   #SCREEN_PLANE_SIZE,a1
    add.l   #64,a0      ; 32 but shifting!
    dbf d7,.loop
    movem.l (a7)+,d2-d6/a0-a1/a5
    rts
    
wait_blit
	TST.B	$BFE001
.wait
	BTST	#6,dmaconr+$DFF000
	BNE.S	.wait
	rts

; what: writes an hexadecimal number (or BCD) in a single plane
; args:
; < A1: plane
; < D0: X (multiple of 8)
; < D1: Y
; < D2: number value
; < D3: number of padding zeroes
; > D0: number of characters written

write_hexadecimal_number

    movem.l A0/D2-d5,-(a7)
    cmp.w   #7,d3
    bcs.b   .padok
    move.w  #7,d3
.padok
    bsr     .write_num
    movem.l (a7)+,A0/D2-d5
    rts
.write_num
    lea .buf+8(pc),a0

    
.loop
    subq    #1,d3    
    move.b  d2,d5
    and.b   #$F,d5
    cmp.b   #10,d5
    bcc.b   .letter
    add.b   #'0',d5
    bra.b   .ok
.letter
    add.b   #'A'-10,d5
.ok
    move.b  d5,-(a0)
    lsr.l   #4,d2
    beq.b   .write
    bra.b   .loop
.write
    tst.b   d3
    beq.b   .w
    bmi.b   .w
    subq    #1,d3
.pad
    move.b  #' ',-(a0)
    dbf d3,.pad
.w
    bra write_string
.buf
    ds.b    8
    dc.b    0
    even
    
; what: writes an decimal number in a single plane
; args:
; < A1: plane
; < D0: X (multiple of 8)
; < D1: Y
; < D2: number value
; < D3: number of padding zeroes
; > D0: number of characters written
    
write_decimal_number
    movem.l A0/D2-d5,-(a7)
    cmp.w   #18,d3
    bcs.b   .padok
    move.w  #18,d3
.padok
    cmp.l   #655361,d2
    bcs.b   .one
    sub.l   #4,d3
    move.w  d0,d5
    ; first write high part    
    divu    #10000,d2
    swap    d2
    moveq.l #0,d4
    move.w   d2,d4
    clr.w   d2
    swap    d2
    bsr     .write_num
    lsl.w   #3,d0
    add.w   d5,d0   ; new xpos
    
    move.l  d4,d2
    moveq   #4,d3   ; pad to 4
.one
    bsr     .write_num
    movem.l (a7)+,A0/D2-d5
    rts
.write_num
    bsr convert_number
    bra write_string
    
write_color_decimal_number
    movem.l A0-A1/D2-d6,-(a7)
    lea     write_color_string(pc),a1
    bsr.b     write_color_decimal_number_internal
    movem.l (a7)+,A0-A1/D2-d6
    rts
write_blanked_color_decimal_number
    movem.l A0-A1/D2-d6,-(a7)
    lea     write_blanked_color_string(pc),a1
    bsr.b     write_color_decimal_number_internal
    movem.l (a7)+,A0-A1/D2-d6
    rts
; what: writes an decimal number with a given color
; args:
; < D0: X (multiple of 8)
; < D1: Y
; < D2: number value
; < D3: number of padding zeroes
; < D4: RGB4 color
; > D0: number of characters written
    
write_color_decimal_number_internal
    cmp.w   #18,d3
    bcs.b   .padok
    move.w  #18,d3
.padok
    cmp.l   #655361,d2
    bcs.b   .one
    sub.l   #4,d3
    move.w  d0,d5
    ; first write high part    
    divu    #10000,d2
    swap    d2
    moveq.l #0,d6
    move.w   d2,d6
    clr.w   d2
    swap    d2
    bsr     .write_num
    lsl.w   #3,d0
    add.w   d5,d0   ; new xpos
    
    move.l  d6,d2
    moveq   #4,d3   ; pad to 4
.one
    bsr     .write_num
    rts
.write_num
    bsr convert_number
    move.w  d4,d2
    jmp     (a1) 
    
    
; < D2: value
; > A0: buffer on converted number
convert_number
    lea .buf+20(pc),a0
    tst.w   d2
    beq.b   .zero
.loop
    divu    #10,d2
    swap    d2
    add.b   #'0',d2
    subq    #1,d3
    move.b  d2,-(a0)
    clr.w   d2
    swap    d2
    tst.w   d2
    beq.b   .write
    bra.b   .loop
.zero
    subq    #1,d3
    move.b  #'0',-(a0)
.write
    tst.b   d3
    beq.b   .w
    bmi.b   .w
    subq    #1,d3
.pad
    move.b  #' ',-(a0)
    dbf d3,.pad
.w
    rts
    
.buf
    ds.b    20
    dc.b    0
    even
    

; what: writes a text in a given color, clears
; non-written planes (just in case another color was
; written earlier)
; args:
; < A0: c string
; < D0: X (multiple of 8)
; < D1: Y
; < D2: RGB4 color (must be in palette!)
; > D0: number of characters written
; trashes: none

write_blanked_color_string:
    movem.l D1-D6/A1,-(a7)
    ; compute string length first in D6
    clr.w   d6
.strlen
    tst.b   (a0,d6.w)
    beq.b   .outstrlen
    addq.w  #1,d6
    bra.b   .strlen
.outstrlen
    ; D6 has string length
    lea game_palette(pc),a1
    moveq   #15,d3
    moveq   #0,d5
.search
    move.w  (a1)+,d4
    cmp.w   d4,d2
    beq.b   .color_found
    addq.w  #1,d5
    dbf d3,.search
    moveq   #0,d0   ; nothing written
    bra.b   .out
.color_found
    ; d5: color index
    lea screen_data,a1
    moveq   #3,d3
    move.w  d0,d4
.plane_loop
; < A0: c string
; < A1: plane
; < D0: X (multiple of 8)
; < D1: Y
; > D0: number of characters written
    move.w  d4,d0
    btst    #0,d5
    beq.b   .clear_plane
    bsr write_string
    bra.b   .next_plane
.clear_plane
    movem.l d0-d6/a1/a5,-(a7)
    move.w  d6,d2   ; width in bytes = string length
    ;lea _custom,a5
    ;moveq.l #-1,d3
    move.w  #8,d3

    bsr clear_plane_any_cpu_any_height
    movem.l (a7)+,d0-d6/a1/a5
.next_plane
    lsr.w   #1,d5
    add.l   #SCREEN_PLANE_SIZE,a1
    dbf d3,.plane_loop
.out
    movem.l (a7)+,D1-D6/A1
    rts
    
; what: writes a text in a given color
; args:
; < A0: c string
; < D0: X (multiple of 8)
; < D1: Y
; < D2: RGB4 color (must be in palette!)
; > D0: number of characters written
; trashes: none

write_color_string:
    movem.l D1-D5/A1,-(a7)
    lea game_palette(pc),a1
    moveq   #15,d3
    moveq   #0,d5
.search
    move.w  (a1)+,d4
    cmp.w   d4,d2
    beq.b   .color_found
    addq.w  #1,d5
    dbf d3,.search
    moveq   #0,d0   ; nothing written
    bra.b   .out
.color_found
    ; d5: color index
    lea screen_data,a1
    moveq   #3,d3
    move.w  d0,d4
.plane_loop
; < A0: c string
; < A1: plane
; < D0: X (multiple of 8)
; < D1: Y
; > D0: number of characters written
    btst    #0,d5
    beq.b   .skip_plane
    move.w  d4,d0
    bsr write_string
.skip_plane
    lsr.w   #1,d5
    add.l   #SCREEN_PLANE_SIZE,a1
    dbf d3,.plane_loop
.out
    movem.l (a7)+,D1-D5/A1
    rts
    
; what: writes a text in a single plane
; args:
; < A0: c string
; < A1: plane
; < D0: X (multiple of 8 else it's rounded)
; < D1: Y
; > D0: number of characters written
; trashes: none

write_string:
    movem.l A0-A2/d1-D2,-(a7)
    clr.w   d2
    ADD_XY_TO_A1    a2
    moveq.l #0,d0
.loop
    move.b  (a0)+,d2
    beq.b   .end
    addq.l  #1,d0

    cmp.b   #'0',d2
    bcs.b   .special
    cmp.b   #'9'+1,d2
    bcc.b   .try_letters
    ; digits
    lea digits(pc),a2
    sub.b   #'0',d2
    bra.b   .wl
    
.try_letters: 
    cmp.b   #'A',d2
    bcs.b   .special
    cmp.b   #'Z'+1,d2
    bcc.b   .special
    lea letters(pc),a2
    sub.b   #'A',d2
.wl
    lsl.w   #3,d2   ; *8
    add.w   d2,a2
    move.b  (a2)+,(a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE,a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE*2,a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE*3,a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE*4,a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE*5,a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE*6,a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE*7,a1)
    bra.b   .next
.special
    cmp.b   #' ',d2
    bne.b   .nospace
    lea space(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.nospace    
    cmp.b   #'!',d2
    bne.b   .noexcl
    lea exclamation(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.noexcl
    cmp.b   #'/',d2
    bne.b   .noslash
    lea slash(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.noslash
    cmp.b   #'-',d2
    bne.b   .nodash
    lea dash(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.nodash
    cmp.b   #'.',d2
    bne.b   .nodot
    lea dot(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.nodot
    cmp.b   #'"',d2
    bne.b   .noquote
    lea quote(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.noquote
    cmp.b   #'h',d2
    bne.b   .noheart
    lea heart(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.noheart
    cmp.b   #'c',d2
    bne.b   .nocopy
    lea copyright(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.nocopy



.next   
    addq.l  #1,a1
    bra.b   .loop
.end
    movem.l (a7)+,A0-A2/d1-D2
    rts

	IFD		HIGHSCORES_TEST
load_highscores
save_highscores
	rts
	ELSE
    
load_highscores
    lea scores_name(pc),a0
    move.l  _resload(pc),d0
    beq.b   .standard
    move.l  d0,a2
    jsr (resload_GetFileSize,a2)
    tst.l   d0
    beq.b   .no_file
    ; file is present, read it
    lea scores_name(pc),a0    
    lea hiscore_table(pc),a1
    move.l #40,d0   ; size
    moveq.l #0,d1   ; offset
    jsr  (resload_LoadFileOffset,a2)
    bra.b	.update_highest
.standard
    move.l  _dosbase(pc),a6
    move.l  a0,d1
    move.l  #MODE_OLDFILE,d2
    jsr     (_LVOOpen,a6)
    move.l  d0,d1
    beq.b   .no_file
    move.l  d1,d4
    move.l  #4,d3
    move.l  #hiscore_table,d2
    jsr (_LVORead,a6)
    move.l  d4,d1
    jsr (_LVOClose,a6)
.update_highest
	move.l	hiscore_table(pc),high_score
.no_file
    rts
    
save_highscores
    tst.w   cheat_keys
    bne.b   .out
    tst.b   highscore_needs_saving
    beq.b   .out
    lea scores_name(pc),a0
    move.l  _resload(pc),d0
    beq.b   .standard
    move.l  d0,a2
    lea scores_name(pc),a0    
    lea hiscore_table(pc),a1
    move.l #4*NB_HIGH_SCORES,d0   ; size
    jmp  (resload_SaveFile,a2)
.standard
    move.l  _dosbase(pc),a6
    move.l  a0,d1
    move.l  #MODE_NEWFILE,d2
    jsr     (_LVOOpen,a6)
    move.l  d0,d1
    beq.b   .out
    move.l  d1,d4
    move.l  #40,d3
    move.l  #hiscore_table,d2
    jsr (_LVOWrite,a6)
    move.l  d4,d1
    jsr (_LVOClose,a6)    
.out
    rts
    ENDC
    
_dosbase
    dc.l    0
_gfxbase
    dc.l    0
_resload
    dc.l    0
_keyexit
    dc.b    $59
scores_name
    dc.b    "amidar.high",0
highscore_needs_saving
    dc.b    0
graphicsname:   dc.b "graphics.library",0
dosname
        dc.b    "dos.library",0
            even

    include ReadJoyPad.s
    
    ; variables
gfxbase_copperlist
    dc.l    0
    
previous_random
    dc.l    0
joystick_state
    dc.l    0
record_data_pointer
    dc.l    0
record_data_end
	dc.l	0
record_input_clock
    dc.w    0
previous_move
	dc.b	0
	even
    IFD    RECORD_INPUT_TABLE_SIZE
prev_record_joystick_state
    dc.l    0

    ENDC

  
current_state:
    dc.w    0
score:
    dc.l    0
displayed_score:
    dc.l    0
previous_score:
    dc.l    0
score_to_track:
    dc.l    0


; general purpose timer for non-game states (intro, game over...)
state_timer:
    dc.l    0
intro_text_message:
    dc.w    0
last_ghost_eaten_state_timer
    dc.w    0
fruit_score_index:
    dc.w    0
next_enemy_iteration_score
    dc.w    0
previous_player_address
    dc.l    0
previous_valid_direction
    dc.l    0

global_speed_table
    dc.l    0
dot_table:
    dc.l    0
maze_wall_table:
    dc.l    0

maze_bitmap_plane_1
    dc.l    0
maze_bitmap_plane_2
    dc.l    0
extra_life_sound_counter
    dc.w    0
extra_life_sound_timer
    dc.w    0
; 0: level 1
level_number:
    dc.w    0
demo_level_number:
    dc.w    INIT_DEMO_LEVEL_NUMBER-1
enemy_kill_timer
    dc.w    0
player_killed_timer:
    dc.w    -1
bonus_score_timer:
    dc.w    0
fright_timer:
    dc.w    0
cheat_sequence_pointer
    dc.l    cheat_sequence

cheat_keys
    dc.w    0
death_frame_offset
    dc.w    0

power_state_counter
    dc.w    0
; move every 10 ticks
bonus_level_lane_select_subcounter:
    dc.w    0
bonus_text_screen_countdown
    dc.w    0
bonus_music_replay_timer
    dc.w    0
player_move_index
    dc.w    0
thief_move_index
    dc.w    0

compatible_rectangles
    dc.l    0,0,0,0
bonus_vertical_table
    dc.l    0
bottom_reached
    dc.w    0
last_bonus_timeout
    dc.w    0
banana_x
    dc.w    0
nb_enemies_but_thief
    dc.w    0
nb_enemies_to_eat
    dc.w    0
jump_index
    dc.w   0
jump_frame
    dc.w    0
enemy_kill_frame
    dc.w    0
completed_music_timer:
    dc.w    0
maze_outline_color
    dc.w    0
can_eat_enemies_mode_pending
    dc.w    0
maze_fill_color
    dc.w    0
previous_temp_paint_direction:
    dc.w    0
attack_timeout:
    dc.w    0
nb_compatible_rectangles
	dc.w	0

thief_target_tile_x
    dc.w    0
thief_target_tile_y
    dc.w    0
thief_standby_timer:
    dc.w    0
; initial time for standby
thief_standby_time:
    dc.w    0
thief_attack_sound_count_timer:
    dc.w    0
thief_attacks:
    dc.b    0
thief_up_move_lock:
    dc.b    0
thief_attack_sound_count:
    dc.b    0
total_number_of_dots:
    dc.b    0
bonus_cattle_moving:
    dc.b    0
player_move_record
    dc.b    0
first_recorded_move
    dc.b    0
first_thief_objective
    dc.b    0
next_level_is_bonus_level
    dc.b    0
nb_lives:
    dc.b    0
rustler_level:
    dc.b    0
corner_sideeffect_paint
	dc.b	0
previous_tile_type:
    dc.b    0

was_playing_paint_sound:
    dc.b    0
new_life_restart:
    dc.b    0
bonus_sprites:
    dc.b    0
nb_stars:
    dc.b    0
nb_special_rectangles:
    dc.b    0
nb_rectangles:
    dc.b    0
music_playing:    
    dc.b    0
pause_flag
    dc.b    0
quit_flag
    dc.b    0
elroy_mode_lock:
    dc.b    0


invincible_cheat_flag
    dc.b    0
infinite_lives_cheat_flag
    dc.b    0
debug_flag
    dc.b    0
demo_mode
    dc.b    0
extra_life_awarded
    dc.b    0
music_played
    dc.b    0
delete_last_star_message
    dc.b    0

    even

power_song_countdown
     dc.w   0
bonus_score_display_message:
    dc.w    0
extra_life_message:
    dc.w    0
    
score_table
    dc.w    0,1,5
nb_enemy_table
    dc.w    4,4,5,5,6,6
	; not sure it's accurate, well, doesn't matter, as the arcade
	; game came with different versions and skill levels.
standby_time_table
	dc.w	5*ORIGINAL_TICKS_PER_SEC
	dc.w	4*ORIGINAL_TICKS_PER_SEC
	dc.w	3*ORIGINAL_TICKS_PER_SEC
	dc.w	2*ORIGINAL_TICKS_PER_SEC
	dc.w	1*ORIGINAL_TICKS_PER_SEC
	dc.w	1	; 1 frame: no standby
	
	
fruit_score     ; must follow score_table
    dc.w    10
loop_array:
    dc.l    0,0,0,0
    
police_fright_palette
    dc.w    $0000,$0f00,$00F0,$0ff0
cattle_fright_palette
    dc.w    $0000,$F91,$0F0,$0c0f
police_fright_blink_palette
    dc.w    $0000,$0fFF,$0F00,$f91
cattle_fright_blink_palette
    dc.w    $0000,$0fff,$00ff,$0f00
    
fright_palette
    dc.l    0
fright_blink_palette
    dc.l    0
    
player_kill_anim_table:
    REPT    ORIGINAL_TICKS_PER_SEC/2
    dc.b    0
    ENDR
    REPT    ORIGINAL_TICKS_PER_SEC/2
    dc.b    1
    ENDR
    REPT    ORIGINAL_TICKS_PER_SEC/2
    dc.b    2
    ENDR
    even
    
    even

    

cheat_sequence
    dc.b    $26,$18,$14,$22,0
    even

copier_dir_table
    dc.l    copier_anim_right,copier_anim_left,copier_anim_up,copier_anim_down
rustler_dir_table
    dc.l    rustler_anim_right,rustler_anim_left,rustler_anim_up,rustler_anim_down
    
COPIER_ANIM_TABLE:MACRO
copier_anim_\1

    dc.l    copier_\1_0,copier_\1_0,copier_\1_0,copier_\1_0,copier_\1_0,copier_\1_0,copier_\1_0,copier_\1_0
    dc.l    copier_\1_1,copier_\1_1,copier_\1_1,copier_\1_1,copier_\1_1,copier_\1_1,copier_\1_1,copier_\1_1
copier_anim_\1_end
    ENDM
    
RUSTLER_ANIM_TABLE:MACRO
rustler_anim_\1
    dc.l    rustler_\1_0,rustler_\1_0,rustler_\1_0,rustler_\1_0,rustler_\1_0,rustler_\1_0,rustler_\1_0,rustler_\1_0
    dc.l    rustler_\1_1,rustler_\1_1,rustler_\1_1,rustler_\1_1,rustler_\1_1,rustler_\1_1,rustler_\1_1,rustler_\1_1
rustler_anim_\1_end
    ENDM
    
    COPIER_ANIM_TABLE  right
    COPIER_ANIM_TABLE  left
    COPIER_ANIM_TABLE  up
    COPIER_ANIM_TABLE  down
    
    RUSTLER_ANIM_TABLE  right
    RUSTLER_ANIM_TABLE  left
    RUSTLER_ANIM_TABLE  up
    RUSTLER_ANIM_TABLE  down



digits:
    incbin  "0.bin"
    incbin  "1.bin"
    incbin  "2.bin"
    incbin  "3.bin"
    incbin  "4.bin"
    incbin  "5.bin"
    incbin  "6.bin"
    incbin  "7.bin"
    incbin  "8.bin"
    incbin  "9.bin"
letters
    incbin	"A.bin"
    incbin	"B.bin"
    incbin	"C.bin"
    incbin	"D.bin"
    incbin	"E.bin"
    incbin	"F.bin"
    incbin	"G.bin"
    incbin	"H.bin"
    incbin	"I.bin"
    incbin	"J.bin"
    incbin	"K.bin"
    incbin	"L.bin"
    incbin	"M.bin"
    incbin	"N.bin"
    incbin	"O.bin"
    incbin	"P.bin"
    incbin	"Q.bin"
    incbin	"R.bin"
    incbin	"S.bin"
    incbin	"T.bin"
    incbin	"U.bin"
    incbin	"V.bin"
    incbin	"W.bin"
    incbin	"X.bin"
    incbin	"Y.bin"
    incbin	"Z.bin"    
exclamation
    incbin  "exclamation.bin"
slash
    incbin  "slash.bin"
dash
    incbin  "dash.bin"
dot
    incbin  "dot.bin"
quote
    incbin  "quote.bin"
heart
    incbin  "heart.bin"
copyright
    incbin  "copyright.bin"
space
    ds.b    8,0
    
high_score_string
    dc.b    " HIGH SCORE",0
p1_string
    dc.b    "     1UP",0
level_string
    dc.b    "   LEVEL",0
score_string
    dc.b    "       00",0
game_over_string
    dc.b    "GAME##OVER",0
player_one_string
    dc.b    "PLAYER ONE",0
player_one_string_clear
    dc.b    "          ",0



    even

    MUL_TABLE   40
    MUL_TABLE   26

square_table:
	rept	256
	dc.w	REPTN*REPTN
	endr
   
; truth table to avoid testing for several directions where there's only once choice
; (one bit set)
no_direction_choice_table:
    dc.b    $ff   ; 0=not possible
    dc.b    RIGHT   ; 1
    dc.b    DOWN   ; 2
    dc.b    $ff   ; 3=composite
    dc.b    LEFT   ; 4=UP
    dc.b    $ff   ; 5=composite
    dc.b    $ff   ; idem
    dc.b    $ff   ; idem
    dc.b    UP   ; 8
    ; all the rest is composite or invalid
    REPT    7
    dc.b    $ff
    ENDR
    even
    
    
score_value_table
    dc.l    20,40,80,160




	STRUCTURE	Sound,0
    ; matches ptplayer
    APTR    ss_data
    UWORD   ss_len
    UWORD   ss_per
    UWORD   ss_vol
    UBYTE   ss_channel
    UBYTE   ss_pri
    LABEL   Sound_SIZEOF
    
; < D0: track start number
play_music
	tst.b	demo_mode
	bne.b	.out
    movem.l d0-a6,-(a7)
    lea _custom,a6
    lea music,a0
    sub.l   a1,a1
    bsr _mt_init
    ; set master volume a little less loud
    ; supposed to be max at 64 but actually 20 is already
    ; super loud...
    move.w  #12,d0
    bsr _mt_mastervol
    
    bsr _mt_start
    st.b    music_playing
    movem.l (a7)+,d0-a6
.out
    rts
    
; < A0: sound struct
play_fx
    tst.b   demo_mode
    bne.b   .no_sound
    lea _custom,a6
    bra _mt_playfx
.no_sound
    rts
    

    
    

    
       
;base addr, len, per, vol, channel<<8 + pri, loop timer, number of repeats (or -1), current repeat, current vbl

FXFREQBASE = 3579564
SOUNDFREQ = 22050

SOUND_ENTRY:MACRO
\1_sound
    dc.l    \1_raw
    dc.w    (\1_raw_end-\1_raw)/2,FXFREQBASE/\3,64
    dc.b    \2
    dc.b    $01
    ENDM
    
    ; radix, ,channel (0-3)
    SOUND_ENTRY lose_bonus,1,SOUNDFREQ
    SOUND_ENTRY enemy_hit,1,SOUNDFREQ
    SOUND_ENTRY enemy_falling,2,SOUNDFREQ
    SOUND_ENTRY enemy_killed,2,SOUNDFREQ
    SOUND_ENTRY extra_life,2,SOUNDFREQ
    SOUND_ENTRY thief_attacks,2,SOUNDFREQ
    SOUND_ENTRY player_killed,2,SOUNDFREQ
    SOUND_ENTRY credit,1,SOUNDFREQ
    SOUND_ENTRY eat,3,SOUNDFREQ
    SOUND_ENTRY ping,3,SOUNDFREQ
    SOUND_ENTRY paint,3,SOUNDFREQ
    SOUND_ENTRY filled,0,SOUNDFREQ
    SOUND_ENTRY jump,1,SOUNDFREQ

; 0-49 divisibility table
divisible_by_5_table
    REPT    10
    dc.b    1
    dc.b    0,0,0,0
    ENDR
    
; ORed previous/current direction masks which allow
; a quick check to see if player just reversed direction
reverse_direction_table
	;		R,L,U,D
	dc.b	0,1,0,0	; R	
	dc.b	1,0,0,0	; L
	dc.b	0,0,0,1	; U
	dc.b	0,0,1,0	; D
	
jump_height_table
	dc.b	-2,-2,-2,-2,-2,-2,-1,-2,-1,-2,-1,-1,-1,0,-1,-1,0,0,0,-1
	dc.b	1,0,0,0,1,1,0,1,1,1,2,1,2,1,2,2,2,2,2,2
jump_height_table_end    

; attack timeouts

attack_timeout_table
    IFD THIEF_AI_TEST
    dc.w    ORIGINAL_TICKS_PER_SEC*10
    dc.w    ORIGINAL_TICKS_PER_SEC*2
    ENDC
    dc.w    ORIGINAL_TICKS_PER_SEC*100
    dc.w    ORIGINAL_TICKS_PER_SEC*80
    dc.w    ORIGINAL_TICKS_PER_SEC*65
    dc.w    ORIGINAL_TICKS_PER_SEC*55
    dc.w    ORIGINAL_TICKS_PER_SEC*53
    dc.w    ORIGINAL_TICKS_PER_SEC*25
    dc.w    ORIGINAL_TICKS_PER_SEC*12
    dc.w    ORIGINAL_TICKS_PER_SEC*10
    dc.w    ORIGINAL_TICKS_PER_SEC*8
    dc.w    ORIGINAL_TICKS_PER_SEC*5
    dc.w    ORIGINAL_TICKS_PER_SEC*4
    dc.w    ORIGINAL_TICKS_PER_SEC*3
    dc.w    ORIGINAL_TICKS_PER_SEC*2
    dc.w    ORIGINAL_TICKS_PER_SEC*1


; enemy speed increasing, level 1-2: 20/20 level 3-4: 20/19 levl 5-6: 20/18
;  level 8: 20/16 level 9-13: 20/15, at level 15 reaches 20/13 speed (max)

 ; speed table at 60 Hz
speed_table:
    dc.l    speed_level_12,speed_level_34,speed_level_56,speed_level_78
    dc.l    speed_level_913,speed_level_913,speed_level_15
    
speed_level_12:
    REPT    20
    dc.b   1
    ENDR
    
speed_level_34:
    REPT    10
    dc.b   1
    ENDR
    dc.b    2
    REPT    9
    dc.b   1
    ENDR
    
speed_level_56:
    REPT    6
    dc.b   1
    ENDR
    dc.b    2
    REPT    6
    dc.b   1
    ENDR
    dc.b    2
    REPT    6
    dc.b   1
    ENDR
    
speed_level_78:
    REPT    4
    dc.b   1
    ENDR
    dc.b    2
    REPT    4
    dc.b   1
    ENDR
    dc.b    2
    REPT    4
    dc.b   1
    ENDR
    dc.b    2
    REPT    4
    dc.b   1
    ENDR
    
speed_level_913:
    REPT    3
    dc.b   1
    ENDR
    dc.b    2
    REPT    4
    dc.b   1
    ENDR
    dc.b    2
    REPT    3
    dc.b   1
    ENDR
    dc.b    2
    REPT    4
    dc.b   1
    ENDR
    dc.b    2
    REPT    3
    dc.b   1
    ENDR

speed_level_15:     ; x1.5 speed!!!
    REPT    10
    dc.b    1
    dc.b    2
    ENDR

    
enemy_start_position_table
    dc.l    .level1
    dc.l    .level2
    dc.l    .level3
    dc.l    .level1

.level1:
    REPT    6
    dc.w    REPTN*40,0
    ENDR
.level2
    dc.w    0,0
    dc.w    40,36
    dc.w    80,36+24
    dc.w    120,36+48
    dc.w    160,36+24*3
    dc.w    200,36+24*4
.level3
    dc.w    120,0
    dc.w    120,36
    dc.w    120,36+24
    dc.w    120,106
    dc.w    120,176-24
    dc.w    120,176

game_palette
    include "palette.s"
alt_sprite_palette
    include "alt_palette.s"
banana_sprite_palette:
    include "banana.s"
cattle_sprite_palette:
    include "cattle.s"
    
player:
    ds.b    Player_SIZEOF
    even

enemies:
    ds.b    Enemy_SIZEOF*7
    even

leaving_paint_tile_x
	dc.w	0
leaving_paint_tile_y
	dc.w	0
	
rollback_paint_zone_pointer:
    dc.l    0
rollback_rectangle_pointer:
    dc.l    0
rollback_dot_table_pointer:
    dc.l    0
pending_paint_rectangle_pointer:
    dc.l    0
    
keyboard_table:
    ds.b    $100,0
    
    include "bonus_maze_data.s"       ; generated by "convert_sprites.py" python script
    include "intro_maze_data.s"       ; generated by "convert_sprites.py" python script
    include "maze_data.s"       ; generated by "convert_sprites.py" python script

floppy_file
    dc.b    "floppy",0
maze_dump_file
    dc.b    "maze.bin",0
    even

; table with 2 bytes: 60hz clock, 1 byte: move mask for the demo
demo_moves_1:
    incbin  "moves_1.bin"
demo_moves_1_end:
demo_moves_2:
    incbin  "moves_2.bin"
demo_moves_2_end:
    even
	
; BSS --------------------------------------
    SECTION  S3,BSS
HWSPR_TAB_XPOS:	
	ds.l	512			

HWSPR_TAB_YPOS:
	ds.l	512
    
    IFD   RECORD_INPUT_TABLE_SIZE
record_input_table:
    ds.b    RECORD_INPUT_TABLE_SIZE
    ENDC
    
grid_backup_plane
    ds.b    SCREEN_PLANE_SIZE
rect_backup_plane
    ds.b    SCREEN_PLANE_SIZE
paint_backup_plane
    ds.b    SCREEN_PLANE_SIZE
maze_wall_table_copy
    ds.b    NB_TILE_LINES*NB_TILES_PER_LINE
    
rollback_paint_zone_buffer:
    ds.l    NB_ROLLBACK_SLOTS*2
rollback_paint_zone_buffer_end
rollback_rectangle_buffer:
    ds.l    NB_ROLLBACK_SLOTS
rollback_rectangle_buffer_end
rollback_dot_table_buffer:
    ds.l    NB_ROLLBACK_SLOTS
rollback_dot_table_buffer_end
pending_paint_rectangle_buffer:
    ds.l    6

player_move_buffer
    ds.l    NB_RECORDED_MOVES
    even
    
    
    SECTION  S4,CODE
    include ptplayer.s

    SECTION  S5,DATA,CHIP
; main copper list
coplist
   dc.l  $01080000
   dc.l  $010a0000
bitplanes:
   dc.l  $00e00000
   dc.l  $00e20000
   dc.l  $00e40000
   dc.l  $00e60000
   dc.l  $00e80000
   dc.l  $00ea0000
   dc.l  $00ec0000
   dc.l  $00ee0000
;   dc.l  $00f00000
;   dc.l  $00f20000

tunnel_color_reg = color+38

colors:
   dc.w color,0     ; fix black (so debug can flash color0)
sprites:
enemy_sprites:
intro_green_police:
bonus_banana:
    ; #0
    dc.w    sprpt+0,0
    dc.w    sprpt+2,0
    ; #1
    dc.w    sprpt+4,0
    dc.w    sprpt+6,0
intro_cattle_pink:
    ; #2
    dc.w    sprpt+8,0
    dc.w    sprpt+10,0
    ; #3
    dc.w    sprpt+12,0
    dc.w    sprpt+14,0
intro_cyan_cattle:    
    ; #4
    dc.w    sprpt+16,0
    dc.w    sprpt+18,0
    ; #5
    dc.w    sprpt+20,0
    dc.w    sprpt+22,0
    ; #6
thief_sprite:
    dc.w    sprpt+24,0
    dc.w    sprpt+26,0
    ; #7
    dc.w    sprpt+28,0
    dc.w    sprpt+30,0
end_color_copper:
   dc.w  diwstrt,$3081            ;  DIWSTRT
   dc.w  diwstop,$28c1            ;  DIWSTOP
   ; proper sprite priority: above bitplanes
   dc.w  $0102,$0000            ;  BPLCON1 := 0x0000
   dc.w  $0104,$0024            ;  BPLCON2 := 0x0024
   dc.w  $0092,$0038            ;  DDFSTRT := 0x0038
   dc.w  $0094,$00d0            ;  DDFSTOP := 0x00d0
   dc.w  $FFDF,$FFFE            ; PAL wait (256)
   dc.w  $2201,$FFFE            ; PAL extra wait (around 288)
   dc.w intreq,$8010            ; generate copper interrupt
    dc.l    -2

   
banana_sprite
    dc.l    0
    incbin  "banana.bin"
    dc.l    0
    
copier_left_0
    incbin  "copier_left_0.bin"
copier_left_1    
    incbin  "copier_left_1.bin"
copier_right_0
    incbin  "copier_right_0.bin"
copier_right_1    
    incbin  "copier_right_1.bin"
copier_up_0
copier_down_0
    incbin  "copier_updown_0.bin"
copier_up_1
copier_down_1
    incbin  "copier_updown_1.bin"
copier_dead_0
    incbin  "copier_dead_0.bin"
copier_dead_1
    incbin  "copier_dead_1.bin"
copier_dead_2
    incbin  "copier_dead_2.bin"
copier_dead_table
    dc.l    copier_dead_0,copier_dead_1,copier_dead_2


rustler_left_0
    incbin  "rustler_left_0.bin"
rustler_left_1    
    incbin  "rustler_left_1.bin"
rustler_right_0
    incbin  "rustler_right_0.bin"
rustler_right_1    
    incbin  "rustler_right_1.bin"
rustler_up_0
    incbin  "rustler_up_0.bin"
rustler_up_1
    incbin  "rustler_up_1.bin"
rustler_down_0
    incbin  "rustler_down_0.bin"
rustler_down_1
    incbin  "rustler_down_1.bin"
rustler_dead_0
    incbin  "rustler_dead_0.bin"
rustler_dead_1
    incbin  "rustler_dead_1.bin"
rustler_dead_2
    incbin  "rustler_dead_2.bin"
rustler_dead_table
    dc.l    rustler_dead_0,rustler_dead_1,rustler_dead_2

empty_16x16_bob
    ds.b    64*4,0

lives
    incbin  "life.bin"
star
    incbin  "star.bin"

    
DECL_POLICE:MACRO
police\1_frame_table:
    dc.l    police\1_0
    dc.l    police\1_1
    dc.l    police\1_2
    dc.l    police\1_3
police\1_end_frame_table:
police\1_jump_frame_table:
    dc.l    police\1_jump_0
    dc.l    police\1_jump_1
police\1_jump_end_frame_table:
police\1_hang_frame_table:
    dc.l    police\1_hang_0
    dc.l    police\1_hang_1
police\1_hang_end_frame_table:
police\1_kill_frame_table:
    dc.l    police\1_kill_0
    dc.l    police\1_kill_1
    dc.l    police\1_kill_0
    dc.l    police\1_kill_1
police\1_kill_end_frame_table:
police\1_fall_frame_table:
    dc.l    police\1_fall_0
    dc.l    police\1_fall_1
    dc.l    police\1_fall_2
    dc.l    police\1_fall_3
police\1_fall_end_frame_table:
police\1_score_frame_table:
    dc.l    police\1_score_200
    dc.l    police\1_score_400
    dc.l    police\1_score_800
    dc.l    police\1_score_1600
    dc.l    police\1_score_3200
    
    ; all enemies share the same graphics, only the colors are different
    ; but we need to replicate the graphics 8*4 times because of sprite control word
police\1_0
    dc.l    0
    incbin  "police_0.bin"
    dc.l    0
police\1_1
    dc.l    0
    incbin  "police_1.bin"
    dc.l    0
police\1_2
    dc.l    0
    incbin  "police_2.bin"
    dc.l    0
police\1_3
    dc.l    0
    incbin  "police_3.bin"
    dc.l    0
police\1_hang_0
    dc.l    0
    incbin  "police_hang_0.bin"
    dc.l    0
police\1_hang_1
    dc.l    0
    incbin  "police_hang_1.bin"
    dc.l    0
police\1_jump_0
    dc.l    0
    incbin  "police_jump_0.bin"
    dc.l    0
police\1_jump_1
    dc.l    0
    incbin  "police_jump_1.bin"
    dc.l    0
police\1_kill_0
police\1_fall_0
    dc.l    0
    incbin  "police_fall_0.bin"
    dc.l    0
police\1_fall_1
    dc.l    0
    incbin  "police_fall_1.bin"
    dc.l    0
police\1_fall_2
    dc.l    0
    incbin  "police_fall_2.bin"
    dc.l    0
police\1_fall_3
    dc.l    0
    incbin  "police_fall_3.bin"
    dc.l    0
police\1_kill_1
    dc.l    0
    incbin  "police_kill.bin"
    dc.l    0
police\1_score_200:
    dc.l    0
    incbin  "scores_200.bin"
    dc.l    0
police\1_score_400:
    dc.l    0
    incbin  "scores_400.bin"
    dc.l    0
police\1_score_800:
    dc.l    0
    incbin  "scores_800.bin"
    dc.l    0
police\1_score_1600:
    dc.l    0
    incbin  "scores_1600.bin"
    dc.l    0
police\1_score_3200:
    dc.l    0
    incbin  "scores_3200.bin"
    dc.l    0

    ENDM
        
    DECL_POLICE  1
    DECL_POLICE  2
    DECL_POLICE  3
    DECL_POLICE  4
    DECL_POLICE  5
    DECL_POLICE  6
    DECL_POLICE  7



    
DECL_CATTLE:MACRO
cattle\1_frame_table:
    dc.l    cattle\1_0
    dc.l    cattle\1_1
    dc.l    cattle\1_0
    dc.l    cattle\1_1
cattle\1_end_frame_table:
cattle\1_jump_frame_table:
    dc.l    cattle\1_jump_0
    dc.l    cattle\1_jump_1
cattle\1_jump_end_frame_table:
cattle\1_hang_frame_table:
    dc.l    cattle\1_hang_0
    dc.l    cattle\1_hang_1
cattle\1_hang_end_frame_table:
cattle\1_kill_frame_table:
    dc.l    cattle\1_kill_0
    dc.l    cattle\1_kill_1
    dc.l    cattle\1_kill_2
    dc.l    cattle\1_kill_3
cattle\1_kill_end_frame_table:
cattle\1_fall_frame_table:
    dc.l    cattle\1_fall_0
    dc.l    cattle\1_fall_1
    dc.l    cattle\1_fall_2
    dc.l    cattle\1_fall_3
cattle\1_fall_end_frame_table:
cattle\1_score_table:   ; no need to copy score sprites
    dc.l    police\1_score_200
    dc.l    police\1_score_400
    dc.l    police\1_score_800
    dc.l    police\1_score_1600
    dc.l    police\1_score_1600
    dc.l    police\1_score_3200

    
    ; all enemies share the same graphics, only the colors are different
    ; but we need to replicate the graphics 8*4 times because of sprite control word
cattle\1_0
    dc.l    0
    incbin  "cattle_0.bin"
    dc.l    0
cattle\1_1
    dc.l    0
    incbin  "cattle_1.bin"
    dc.l    0
cattle\1_hang_0
    dc.l    0
    incbin  "cattle_hang_0.bin"
    dc.l    0
cattle\1_hang_1
    dc.l    0
    incbin  "cattle_hang_1.bin"
    dc.l    0
cattle\1_kill_2
cattle\1_jump_0
    dc.l    0
    incbin  "cattle_jump_0.bin"
    dc.l    0
cattle\1_kill_3
cattle\1_jump_1
    dc.l    0
    incbin  "cattle_jump_1.bin"
    dc.l    0
cattle\1_fall_0
    dc.l    0
    incbin  "cattle_fall_0.bin"
    dc.l    0
cattle\1_kill_0
cattle\1_fall_1
    dc.l    0
    incbin  "cattle_fall_1.bin"
    dc.l    0
cattle\1_fall_2
    dc.l    0
    incbin  "cattle_fall_2.bin"
    dc.l    0
cattle\1_fall_3
    dc.l    0
    incbin  "cattle_fall_3.bin"
    dc.l    0
cattle\1_kill_1
    dc.l    0
    incbin  "cattle_kill.bin"
    dc.l    0

    ENDM
        
    DECL_CATTLE  1
    DECL_CATTLE  2
    DECL_CATTLE  3
    DECL_CATTLE  4
    DECL_CATTLE  5
    DECL_CATTLE  6
    DECL_CATTLE  7
    

score_5000:
    dc.l    0
    incbin  "scores_5000.bin"
    dc.l    0


thief_attacks_raw
    incbin  "thief_attacks.raw"
    even
thief_attacks_raw_end


credit_raw
    incbin  "credit.raw"
    even
credit_raw_end



lose_bonus_raw
    incbin  "lose_bonus.raw"
    even
lose_bonus_raw_end


ping_raw
    incbin  "ping.raw"
    even
ping_raw_end
enemy_hit_raw
    incbin  "enemy_hit.raw"
    even
enemy_hit_raw_end
enemy_killed_raw
    incbin  "enemy_killed.raw"
    even
enemy_killed_raw_end
extra_life_raw
    incbin  "extra_life.raw"
    even
extra_life_raw_end
enemy_falling_raw
    incbin  "enemy_falling.raw"
    even
enemy_falling_raw_end
player_killed_raw
    incbin  "player_killed.raw"
    even
player_killed_raw_end

eat_raw
    incbin  "eat.raw"
    even
eat_raw_end
filled_raw
    incbin  "filled.raw"
    even
filled_raw_end
paint_raw
    incbin  "paint.raw"
    even
paint_raw_end
jump_raw
    incbin  "jump.raw"
    even
jump_raw_end

    even

  
music:
    incbin  "amidar_music_conv.mod"
    
empty_sprite
    dc.l    0,0

    
    SECTION S_4,BSS,CHIP
    ; erase method erases one line above
    ; and character can be drawn at y=0 so add some memory
    ; to avoid corrupting memory
    ds.b    NB_BYTES_PER_LINE*NB_PLANES
screen_data:
    ds.b    SCREEN_PLANE_SIZE*NB_PLANES+NB_BYTES_PER_LINE,0

    
    	