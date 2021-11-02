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
    UWORD   frame
    UWORD   speed_table_index
	LABEL	Character_SIZEOF

	STRUCTURE	Player,0
	STRUCT      BaseCharacter1,Character_SIZEOF
    UWORD   prepost_turn
    LABEL   Player_SIZEOF
    
	STRUCTURE	Ghost,0
	STRUCT      BaseCharacter2,Character_SIZEOF
	STRUCT      palette,SpritePalette_SIZEOF
    APTR     frame_table
    APTR     frightened_ghost_white_frame_table
    APTR     frightened_ghost_blue_frame_table
    APTR     target_frame
    APTR     copperlist_address
    APTR     color_register
    UWORD    mode_timer     ; number of 1/50th to stay in the current mode
    UWORD    mode           ; current mode
    UWORD    flash_timer
    UWORD    flash_toggle_timer
    UBYTE    flashing_as_white
    UBYTE    active
	LABEL	 Enemy_SIZEOF
    
    ;Exec Library Base Offsets


;graphics base

StartList = 38

;autres labels

Execbase  = 4


; ---------------debug/adjustable variables

; uncomment to test intermission screen
;;INTERMISSION_TEST = THIRD_INTERMISSION_LEVEL

; if set skips intro and start music, game starts almost immediately
DIRECT_GAME_START

; temp if nonzero, then records game input, intro music doesn't play
; and when one life is lost, blitzes and a0 points to move record table
; a1 points to the end of the table
; 100 means 100 seconds of recording at least (not counting the times where
; the player (me :)) isn't pressing any direction at all.
;RECORD_INPUT_TABLE_SIZE = 100*ORIGINAL_TICKS_PER_SEC

EXTRA_LIFE_SCORE = 10000/10

START_LEVEL = 1

MODE_NORMAL = 1
MODE_FRIGHT = 2
MODE_HANGING = 3

; --------------- end debug/adjustable variables

; actual nb ticks (PAL)
NB_TICKS_PER_SEC = 50
; game logic ticks
ORIGINAL_TICKS_PER_SEC = 60

; wall tile types
W = 4   ; wall
P = 3   ; pen space (pac block)
T = 2   ; tunnel
B = 1   ; ghost up block
O = 0   ; empty


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

Y_MAX = MAZE_HEIGHT
X_MAX = (NB_BYTES_PER_MAZE_LINE-1)*8

; messages from update routine to display routine
MSG_NONE = 0
MSG_SHOW = 1
MSG_HIDE = 2


NB_FLASH_FRAMES = 14

; matches the pac kill animation
PLAYER_KILL_TIMER = NB_TICKS_PER_SEC+NB_TICKS_PER_SEC/2+(NB_TICKS_PER_SEC/8)*9+NB_TICKS_PER_SEC/4+NB_TICKS_PER_SEC
GHOST_KILL_TIMER = (NB_TICKS_PER_SEC*5)/6

PREPOST_TURN_LOCK = 4


; direction enumerates, follows order of enemies in the sprite sheet
RIGHT = 0
LEFT = 1<<2
UP = 2<<2
DOWN = 3<<2

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
STATE_LEVEL_COMPLETED = 2*4
STATE_NEXT_LEVEL = 3*4
STATE_LIFE_LOST = 4*4
STATE_INTRO_SCREEN = 5*4
STATE_GAME_START_SCREEN = 6*4
STATE_BONUS_SCREEN = 7*4

; jump table macro, used in draw and update
DEF_STATE_CASE_TABLE:MACRO
    move.w  current_state(pc),d0
    lea     .case_table(pc),a0
    move.l     (a0,d0.w),a0
    jmp (a0)
    
.case_table
    dc.l    .playing
    dc.l    .game_over
    dc.l    .level_completed
    dc.l    .next_level
    dc.l    .life_lost
    dc.l    .intro_screen
    dc.l    .game_start_screen
    dc.l    .bonus_screen
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
    
.startup
    bsr load_highscores
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
    
    bsr init_sound
    
    ; shut off dma
    lea _custom,a5
    move.w  #$7FFF,(intena,a5)
    move.w  #$7FFF,(intreq,a5)
    move.w #$03E0,dmacon(A5)

    bsr init_interrupts
    ; intro screen
    
    
    moveq #NB_PLANES,d4
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
    

    lea game_palette(pc),a0
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

    clr.w  state_timer
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
    clr.w   state_timer
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

    
    ; on some levels, there's an intermission sequence

    bsr wait_bof
    
    ; do it first, as the last bonus overwrites bottom left of screen
    ;;bsr draw_bonuses    
    bsr draw_score
    
    ; for debug
    ;;bsr draw_bounds
    
    bsr init_dots

    bsr hide_sprites

    ; enable copper interrupts, mainly
    moveq.l #0,d0
    bra.b   .from_level_start
.new_life
    moveq.l #1,d0
.from_level_start
    bsr init_enemies
    bsr init_player
    
    bsr wait_bof

    bsr draw_maze
    bsr draw_dots
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
.level_completed
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
    move.w  #1,state_timer
    bra.b   .game_over
.no_demo
    ; life lost, make next start a little easier by
    ; locking elroy mode
    st.b    elroy_mode_lock
    
    tst.b   infinite_lives_cheat_flag
    bne.b   .new_life
    subq.b   #1,nb_lives
    bne.b   .new_life

    ; save highscores if whdload
    tst.l   _resload
    beq.b   .no_save
    tst.w   cheat_keys
    bne.b   .no_save
    bsr     save_highscores
.no_save
    ; 3 seconds
    move.w  #ORIGINAL_TICKS_PER_SEC*3,state_timer
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
    add.l   #NB_BYTES_PER_LINE,a1
    dbf d0,.cp
    movem.l (a7)+,d0-d1/a0-a1
    rts
    
init_new_play:
    ; global init at game start
    move.l  #demo_moves,record_data_pointer
    clr.l   replayed_input_state
    move.b  #4,nb_lives
    clr.b   extra_life_awarded
    clr.b    music_played
    move.w  #START_LEVEL-1,level_number
    clr.l   score
    clr.l   displayed_score
    rts
    
init_level: 
    ; sets initial number of dots
    lea rectlist_1(pc),a0
.riloop
    move.l  (a0)+,d0
    beq.b   .out
    move.l  d0,a1
    move.w  mdots(a1),cdots(a1)
    bra.b   .riloop
    
.out
    
    ; level
    move.w  level_number,d2
    cmp.w   #21,d2
    bcs.b   .okay
    ; maxed out
    move.w  #20,d2
.okay

    
    
    clr.b  nb_dots_eaten
    clr.b   elroy_mode_lock
    
    
    ; speed table
    lea speed_table(pc),a1
    add.w   d2,d2
    add.w   d2,d2
    move.l  (a1,d2.w),a1    ; global speed table
    move.l  a1,global_speed_table


    rts

; clear planes used for score (score hidden in acts)
clear_scores
    lea	screen_data+SCREEN_PLANE_SIZE*1,a1
    move.w  #232,d0
    move.w  #16,d1
    move.w  #9,d2
    move.w  #4,d3
.loop
    lea	screen_data+SCREEN_PLANE_SIZE*1,a1
    bsr clear_plane_any
    add.w	#SCREEN_PLANE_SIZE,a1
    bsr clear_plane_any
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
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)
    addq.l  #8,a0
    dbf d1,.emptyspr
    rts
    
hide_enemy_sprites:
    move.w  #6,d1
    lea  sprites,a0
    lea empty_sprite,a1
.emptyspr

    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)
    add.w  #16,a0
    dbf d1,.emptyspr
    rts

; < A0: ghost structure
set_normal_ghost_palette
    move.l  a1,-(a7)
    move.l  color_register(a0),a1
    ; set/reset palette
    move.l  palette(a0),(a1)+
    move.l  palette+4(a0),(a1)
    move.l  (a7)+,a1
    rts
    

init_enemies
    move.b  d0,d4
    lea enemies+Enemy_SIZEOF(pc),a0
    lea enemy_sprites,a1   ; the sprite part of the copperlist, sprite 1-7 are the ghost sprites
    lea game_palette+32(pc),a3  ; the sprite part of the color palette 16-31
    ; shared settings
    moveq   #6,d7
    ; clear all enemies
.cloop
    clr.b   active(a0)
    add.w   #Enemy_SIZEOF,a0
    dbf d7,.cloop
    
    move.w nb_enemies(pc),d7
    subq.w  #1,d7
    lea _custom+color+32,a4
    moveq.l #2,d0

    lea enemies+Enemy_SIZEOF(pc),a0
.igloop
    ; copy all 4 colors (back them up)
    st.b    active(a0)
    move.l (a3)+,palette(a0)
    move.l (a3)+,palette+4(a0)
    move.l  a4,color_register(a0)
    ; set/reset palette
    bsr set_normal_ghost_palette
    
    addq.l  #8,a4   ; next color register range
    move.l  a1,copperlist_address(a0)
    add.l   #8,a1
    
    tst.b   d4
    bne.b   .no_reset

    clr.w   speed_table_index(a0)
.no_reset    
    clr.w   h_speed(a0)
    clr.w   v_speed(a0)

    clr.b   flashing_as_white(a0)
    
	move.w	#4,ypos(a0)
	move.w	d0,xpos(a0)
    add.w   #40,d0
    move.w  #DOWN,direction(a0)
    
    add.w   #Enemy_SIZEOF,a0
    dbf d7,.igloop
  
    ; thief
    
    lea     enemies,a0
    st.b    active(a0)
    move.l (a3)+,palette(a0)
    move.l (a3)+,palette+4(a0)
    move.l  a4,color_register(a0)
    move.w  #UP,direction(a0)
    ; set/reset palette
    ;;bsr set_normal_ghost_palette
    move.l  #thief_sprite,copperlist_address(a0)
	move.w	#MAZE_HEIGHT,ypos(a0)
	move.w	#NB_BYTES_PER_MAZE_LINE*8-6,xpos(a0)


    ; specific settings
    lea enemies(pc),a0
    move.l  #police1_police_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l  #police2_police_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #police3_police_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #police4_police_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #police5_police_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #police6_police_frame_table,frame_table(a0)
    add.w  #Enemy_SIZEOF,a0
    move.l #police7_police_frame_table,frame_table(a0)
    
    rts
    

init_player
    clr.l   previous_valid_direction
    clr.w   death_frame_offset
    clr.l   previous_player_address   ; no previous mspacman position
    ; if there was a bonus running, remove it

    
    lea player(pc),a0
    move.l  #maze_1_wall_table,maze_wall_table
    move.l  #'COPI',character_id(a0)
    move.w  #NB_TILES_PER_LINE*4,xpos(a0)
	move.w	#Y_MAX,ypos(a0)
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
    clr.l   prev_record_joystick_state
    clr.l   previous_random
    ENDC

    clr.w   record_input_clock                      ; start of time
    

    move.w  #-1,player_killed_timer
    move.w  #-1,ghost_eaten_timer
    clr.w   next_ghost_iteration_score
    clr.w   fright_timer    
   
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
.modec
        dc.b    "MODEC ",0
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
    lea	screen_data+SCREEN_PLANE_SIZE*3,a1 
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
    lea .dots(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.b nb_dots_eaten(pc),d2
    move.w  #3,d3
    bsr write_decimal_number
    ; ---
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .bottom_rect_string(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    move.l  d0,d3
    lea rect_1_21(pc),a0
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
    clr.l   d2
    move.l #dot_table,d2
    move.w  #8,d3
    bsr write_hexadecimal_number

    rts
    
.px
        dc.b    "PX ",0
.py
        dc.b    "PY ",0

.dots
        dc.b    "DOTC1 ",0
.bottom_rect_string
        dc.b    "R1 21 ",0
.bonus
        dc.b    "BT ",0
.dottable:
        dc.b    "DTA !",0
        even

draw_enemies:
    tst.w  ghost_eaten_timer
    bmi.b   .no_ghost_eat
    bsr hide_enemy_sprites
    
    ; store score
    lea player(pc),a4
    move.l  score_frame(pc),a0
    move.w  xpos(a4),d0
    sub.w   #24,d0
    move.w  ypos(a4),d1
    sub.w   #30,d1
    bsr store_sprite_pos      
    move.l  d0,(a0)
    lea score_sprite_entry,a1
    move.l  a0,d2
    move.w  d2,(6,a1)
    swap    d2
    move.w  d2,(2,a1)
    ; change color for score, ghost has disappeared anyway

    move.w  #$00ff,_custom+color+32+8+2
    ; don't place sprites
    rts
.no_ghost_eat
    move.w  player_killed_timer(pc),d6
    bmi.b   draw_enemies_normal
    cmp.w   #PLAYER_KILL_TIMER-NB_TICKS_PER_SEC,d6
    bcs.b   hide_sprites
    ; clear the enemies sprites after 1 second when pacman is killed
draw_enemies_normal
    lea enemies(pc),a0
    move.w  nb_enemies(pc),d7   ; +thief
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
    ; too on the right, don't draw sprite
.do_display
    move.w  ypos(a0),d1
    ; center => top left
    bsr store_sprite_pos
.ssp
    move.w  mode(a0),d3 ; normal/chase/fright/fall

    lea     palette(a0),a2      ; normal ghost colors

    move.l  frame_table(a0),a1

    move.w  frame(a0),d2
    
    lsr.w   #2,d2   ; 8 divide to get 0,1
    cmp.w   #MODE_FRIGHT,d3
    bne.b   .no_fright
    ; change palette for that sprite
    move.w  mode_timer(a0),d4

    cmp.w   flash_timer(a0),d4
    bcc.b   .no_flashing        ; flashing if mode_timer is below flash_timer
    ; now check the flash toggle
    move.w  flash_toggle_timer(a0),d4
    addq.w  #1,d4
    cmp.w   #NB_FLASH_FRAMES,d4
    bne.b   .no_toggle
    clr.w   d4
    eor.b   #1,flashing_as_white(a0)
.no_toggle
    move.w  d4,flash_toggle_timer(a0)
.no_flashing
    lea     frightened_enemies_blue_palette(pc),a2
    ; select proper palette (blue/white)
    tst.b   flashing_as_white(a0)
    beq.b   .no_white
    ; white flashing
    lea     frightened_enemies_white_palette(pc),a2
.no_white
    bra.b   .fright
.no_fright
    bra.b   .no_white2
.fright
    ; select proper fright sprite
    move.l  frightened_ghost_blue_frame_table(a0),a1
    tst.b   flashing_as_white(a0)
    beq.b   .no_white2
    move.l  frightened_ghost_white_frame_table(a0),a1    
.no_white2
    bclr    #0,d2   ; even
    add.w   d2,d2       ; times 2
.end_anim
    ; directly change color registers for that sprite
;    move.l  color_register(a0),a3
;    move.l  (a2)+,(a3)+
;    move.l  (a2)+,(a3)+

    ; get proper frame from proper frame set
    move.l  (a1,d2.w),a1
    ; now if D6 is non-zero, handle shift

    move.l  d0,(a1)     ; store control word
    move.l  a1,d2    
    move.l  copperlist_address(a0),a1
    move.w  d2,(6,a1)
    swap    d2
    move.w  d2,(2,a1)    
    rts
    
     
draw_all
    DEF_STATE_CASE_TABLE

; draw intro screen
.intro_screen
    bra.b   draw_intro_screen
; draw intro screen
.bonus_screen
    
    rts
    
.game_start_screen
    tst.w   state_timer
    beq.b   draw_start_screen
    rts
    
.level_completed
.life_lost
.next_level

    ; don't do anything
    rts
PLAYER_ONE_X = 72
PLAYER_ONE_Y = 102-14

    
.game_over
    bsr write_game_over
    bra.b   .draw_complete
.playing


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
    ; high score
    st.b    highscore_needs_saving
    
    move.l  d2,high_score
    bsr draw_high_score
.no_score_update
    tst.b   demo_mode
    beq.b   .no_demo
    ;;bsr wait_blit       ; wait for pacman to draw
    bsr write_game_over
.no_demo
.draw_complete
    rts

stop_sounds
    lea _custom,a6
    clr.b   music_playing
    bra _mt_end



    
HIGHSCORE_RESTORE_ADDRESS = screen_data+2*SCREEN_PLANE_SIZE+(24+34)*NB_BYTES_PER_LINE+36

; < D2: highscore
draw_high_score
    move.w  #232+16,d0
    move.w  #24+32,d1
    move.w  #6,d3
    move.w  #$FFF,d4    
    bsr write_color_decimal_number
    ; hack: save part of the last digits on the second plane
    ; so it's not overwritten by mspacman on level 1 tunnel
    ; also save the part of the "L" of "LEVEL"

    lea HIGHSCORE_RESTORE_ADDRESS,a1
    lea highscore_restore_buffer(pc),a0
    move.w  #4,d0
.save
    move.w  (a1),(a0)+
    move.w  (NB_BYTES_PER_LINE*14,a1),(a0)+
    move.w  (NB_BYTES_PER_LINE*20,a1),(a0)+
    add.w   #NB_BYTES_PER_LINE,a1
    dbf d0,.save

    rts

; repair the potential damage to highscore/level text
; done by pacman blitted in the tunnel    
; maybe it could have been fixed in some smarter way but that's
; cheap enough
restore_high_score:
    lea HIGHSCORE_RESTORE_ADDRESS,a1
    lea highscore_restore_buffer(pc),a0
    move.w  #4,d0
.rest
    move.w  (a0)+,(a1)
    move.w  (a0)+,(NB_BYTES_PER_LINE*14,a1)
    move.w  (a0)+,(NB_BYTES_PER_LINE*20,a1)
    add.w   #NB_BYTES_PER_LINE,a1
    dbf d0,.rest

    rts

highscore_restore_buffer
    ds.w    20
    
write_game_over
    move.w  #72,d0
    move.w  #136,d1
    move.w  #$0f00,d2   ; red
    lea game_over_string(pc),a0
    bra write_color_string
    
; < D0: score (/10)
add_to_score:
    add.l   d0,score
    tst.b  extra_life_awarded
    bne.b   .no_play
    ; was below, check new score
    cmp.l   #EXTRA_LIFE_SCORE,score    ; is current score above xtra life score
    bcs.b   .no_play        ; not yet
    
    move.b  #1,extra_life_awarded
    move.w  #MSG_SHOW,extra_life_message
    addq.b   #1,nb_lives
    move.l A0,-(a7)
    move.w  #10,extra_life_sound_counter
    clr.w   extra_life_sound_timer
    move.l  (a7)+,a0
.no_play
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

X_TEXT = 56
Y_TEXT = 16
GHOST_DESC_HEIGHT = 24

X_DOT = 120
Y_DOT = 160

Y_PAC_ANIM = 136
X_DEMO_POWER_PILL = 48
DEMO_PACMAN_TIMER = NB_TICKS_PER_SEC*14
DEMO_DOT_SCORE_TIMER = NB_TICKS_PER_SEC*12
DEMO_POWER_PILL_TIMER = NB_TICKS_PER_SEC*13

DRAW_GHOST_INFO:MACRO
    cmp.w   #NB_TICKS_PER_SEC*(\2*3+1),state_timer
    bne.b   .no_show_\1
    moveq.l #\2,d0
    bra.b .draw_ghost_bob
.no_show_\1
    cmp.w   #NB_TICKS_PER_SEC*(\2*3+2),state_timer
    bne.b   .no_show_\1_text
    moveq.l #\2,d0
    bsr .draw_ghost_text
    move.w  d0,.nb_written
    rts
.no_show_\1_text
    cmp.w   #NB_TICKS_PER_SEC*(\2*3+2)+(NB_TICKS_PER_SEC/2)+1,state_timer
    bne.b   .no_show_\1_text_2
    moveq.l #\2,d0
    bra .draw_ghost_text
.no_show_\1_text_2
    ENDM
    
draw_start_screen
    bsr hide_sprites
    bsr clear_screen
	
	; write mrspacman bob
    lea lives,a0
    moveq.l #-1,d2  ; mask
    move.w  #104,d0
    move.w #160,d1
    ;;bsr blit_4_planes
	
    lea .psb_string(pc),a0
    move.w  #48,d0
    move.w  #96,d1
    move.w  #$0fb5,d2
    bsr write_color_string
    
    lea .opo_string(pc),a0
    move.w  #48+16,d0
    move.w  #116,d1
    move.w  #$0fb5,d2
	
    bsr write_color_string
    lea .bp1_string(pc),a0
    move.w  #16,d0
    move.w  #192-24,d1
    move.w  #$d94,d2
    bsr write_color_string
    
    rts
    
.psb_string
    dc.b    "PUSH START BUTTON",0
.opo_string:
    dc.b    "1 PLAYER ONLY",0
.bp1_string
    dc.b    "ADDITIONAL ## AT 10000 pts",0
    even
    
    
WHITE_TEXT_X = 80
WHITE_TEXT_Y = 104
GHOST_TEXT_X = WHITE_TEXT_X+16
GHOST_TEXT_Y = WHITE_TEXT_Y+24

draw_intro_screen
    tst.w   state_timer
    bne.b   .no_first

    lea    .title(pc),a0
    move.w  #80,d0
    move.w  #56,d1
    move.w  #$0fb5,d2
    bsr write_color_string    

    
    ; first update, don't draw enemies or anything as they're not initialized
    ; (draw routine is called first)
    rts
    
.no_first    
    
    rts
    
.previous_ghost_color
    dc.w    0
.latest_text_message
    dc.w    0
.title
    dc.b    'AMIDAR',0

    even
    



; what: clears a plane of any width (ATM not using blitter), 16 height
; args:
; < A1: dest
; < D0: X (multiple of 8)
; < D1: Y
; < D2: blit width in bytes (+2)
; trashes: none

clear_plane_any
    movem.l d0-D2/a0-a2,-(a7)
    lsr.w   #3,d0
    add.w   d0,a1
    lea mul40_table(pc),a2
    add.w   d1,d1    
    move.w  (a2,d1.w),d1
    add.w   d1,a1
    move.l  a1,a0
    move.w  #15,d0
.yloop
    move.w  d2,d1
    addq.w  #1,d1   ; 2-1
.xloop
    clr.b   (a0)+
    dbf d1,.xloop
    ; next line
    add.l   #NB_BYTES_PER_LINE,a1
    move.l  a1,a0
    dbf d0,.yloop
.out
    movem.l (a7)+,d0-D2/a0-a2
    rts
    
draw_last_life:
    lea lives,a0
    move.b  nb_lives(pc),d3
    ext     d3
    subq.w  #2,d3
    bmi.b   .out
    lsl.w   #4,d3
    add.w #NB_BYTES_PER_MAZE_LINE*8,d3
    moveq.l #-1,d2  ; mask

    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.w  d3,d0
    moveq.l #0,d1
    bsr blit_plane
.out
    rts
    
draw_stars:
    moveq   #2,d7
.lloop
    lea star,a0
    lea	screen_data+NB_BYTES_PER_MAZE_LINE-4+(NB_BYTES_PER_LINE)*(MAZE_HEIGHT+18),a1
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
draw_lives:
    moveq.w #3,d7
    lea	screen_data+LIVES_OFFSET,a1
.cloop
    move.l #NB_BYTES_PER_MAZE_LINE*8,d0
    moveq.l #0,d1
    move.l  #8,d2
    bsr clear_plane_any
    add.w   #SCREEN_PLANE_SIZE,a1
    dbf d7,.cloop
    
draw_lives_no_clear:
    move.b  nb_lives(pc),d7
    ext     d7
    subq.w  #2,d7
    bmi.b   .out
    ext.l   d7
.lloop
    lea lives,a0
    lea	screen_data+LIVES_OFFSET,a1
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
    dc.l    level_1_maze
    
level_1_maze
    dc.w    $F00,$CC9
   
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
    move.l  maze_misc(pc),a1
    move.w  (a1)+,(2,a0)  ; dots, color 1
	move.w  (a1)+,d0
    move.w  d0,(4,a0)  ; dots 
    move.w  d0,(6,a0)  ; dots+outline

    ;;move.b  (1,a1),total_number_of_dots
    
    lea screen_data,a1
    bsr clear_playfield_plane
    add.w   #SCREEN_PLANE_SIZE*2,a1
    bsr clear_playfield_plane
    add.w   #SCREEN_PLANE_SIZE,a1
    bsr clear_playfield_plane
    
    lea screen_data+MAZE_ADDRESS_OFFSET,a1
    
    move.b #$7F,d0
    move.b  d0,(NB_BYTES_PER_LINE*(MAZE_HEIGHT+1),a1)
    move.b  d0,(NB_BYTES_PER_LINE*MAZE_HEIGHT,a1)
    move.b  d0,(NB_BYTES_PER_LINE,a1)
    move.b  d0,(a1)+

    ; draw frame
    move.w  #NB_BYTES_PER_MAZE_LINE-3,d1
    move.w  #-1,d0
.hloop
    move.b  d0,(NB_BYTES_PER_LINE*(MAZE_HEIGHT+1),a1)
    move.b  d0,(NB_BYTES_PER_LINE*MAZE_HEIGHT,a1)
    move.b  d0,(NB_BYTES_PER_LINE,a1)
    move.b  d0,(a1)+
    dbf     d1,.hloop
    
    move.b  #$F0,d0
    move.b  d0,(NB_BYTES_PER_LINE*(MAZE_HEIGHT+1),a1)
    move.b  d0,(NB_BYTES_PER_LINE*MAZE_HEIGHT,a1)
    move.b  d0,(NB_BYTES_PER_LINE,a1)
    move.b  d0,(a1)
    
    ; vertical edges
    lea screen_data+MAZE_ADDRESS_OFFSET,a1
    move.w  #MAZE_HEIGHT-1,d1
    move.b  #$60,d0
.vloop
    or.b    d0,(a1)
    or.b    d0,(5,a1)
    or.b    d0,(10,a1)
    or.b    d0,(15,a1)
    or.b    d0,(20,a1)
    or.b    d0,(NB_BYTES_PER_MAZE_LINE-1,a1)
    add.w  #NB_BYTES_PER_LINE,a1
    dbf     d1,.vloop
    
    ; horizontal separations
    lea maze_1_vertical_table(pc),a0
    lea mul40_table(pc),a2
    moveq   #0,d0
    lea screen_data+MAZE_ADDRESS_OFFSET,a1
.seploop
    moveq   #0,d1
    move.b  (a0)+,d1
    bpl.b   .cont
    cmp.b   #-2,d1
    beq.b   .out
    add.w   #5,a1
    bra.b   .seploop
.cont
    moveq   #-1,d2
    move.b  #$7F,d3
    ; draw horizontal separation
    lsl.w   #2,d1
    move.w  (a2,d1.w),d1    ; times40
    lsl.w   #2,d1           ; times4
    or.b    d3,(A1,d1)
    or.b    d2,(1,A1,d1)
    or.b    d2,(2,A1,d1)
    or.b    d2,(3,A1,d1)
    or.b    d2,(4,A1,d1)
    add.w   #NB_BYTES_PER_LINE,d1
    or.b    d3,(A1,d1)
    or.b    d2,(1,A1,d1)
    or.b    d2,(2,A1,d1)
    or.b    d2,(3,A1,d1)
    or.b    d2,(4,A1,d1)
    bra.b   .seploop
.out
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
    
    rts    



    
init_dots:
    ; init dots
    lea maze_1_dot_table_read_only(pc),a0
    lea dot_table,a1
    move.l  #NB_TILE_LINES*NB_TILES_PER_LINE-1,d0
.copy
    move.l  (a0)+,(a1)+
    move.l  (a0)+,(a1)+
    move.l  (a0)+,(a1)+
    move.l  (a0)+,(a1)+
    dbf d0,.copy
    rts
    
draw_dots:
    lea screen_data+MAZE_ADDRESS_OFFSET+SCREEN_PLANE_SIZE-NB_BYTES_PER_LINE,a2
    lea dot_table,a0
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
    move.b  #%01100000,(a1)
    move.b  #%11110000,(NB_BYTES_PER_LINE,a1)
    move.b  #%11110000,(NB_BYTES_PER_LINE*2,a1)
    move.b  #%01100000,(NB_BYTES_PER_LINE*3,a1)
    rts
    
; < A1 address
clear_dot
    clr.b  (a1)
    clr.b  (NB_BYTES_PER_LINE,a1)
    clr.b  (NB_BYTES_PER_LINE*2,a1)
    clr.b  (NB_BYTES_PER_LINE*3,a1)
    clr.b  (NB_BYTES_PER_LINE*4,a1) ; one more
    
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
    blitz
    nop
    rte
excc
    blitz
    nop
    nop
    rte
exc10
    blitz
    nop
    nop
    nop
    rte
    
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
.reset_cheat
    move.l  #cheat_sequence,cheat_sequence_pointer
.cheat_end
    
    cmp.b   #$45,d0
    bne.b   .no_esc
    cmp.w   #STATE_INTRO_SCREEN,current_state
    beq.b   .no_esc
    cmp.w   #STATE_GAME_START_SCREEN,current_state
    beq.b   .no_esc
    move.w  #ORIGINAL_TICKS_PER_SEC*2,state_timer
    move.w  #STATE_GAME_OVER,current_state
.no_esc
    
    cmp.w   #STATE_PLAYING,current_state
    bne.b   .no_playing
    tst.b   demo_mode
    bne.b   .no_pause
    cmp.b   #$19,d0
    bne.b   .no_pause
    tst.b   music_playing
    bne.b   .no_pause
    eor.b   #1,pause_flag
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

    bra.b   .no_playing
.no_bonus

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

    ; no pause if not in game or music is playing
    cmp.w   #STATE_PLAYING,current_state
    bne.b   .no_second
    tst.b   demo_mode
    bne.b   .no_second
    
    eor.b   #1,pause_flag
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


; what: updates game state
; args: none
; trashes: potentially all registers

update_all

    DEF_STATE_CASE_TABLE

.intro_screen
    bra update_intro_screen
.bonus_screen

    ; other levels
    rts
    
.game_start_screen
    tst.w   state_timer
    bne.b   .out
    add.w   #1,state_timer
.out    
    rts
    
.life_lost
    rts  ; bra update_power_pill_flashing

.level_completed
    bsr hide_sprites

    bsr     stop_sounds

    bra.b   .next_level


    
.next_level
     move.w  #STATE_NEXT_LEVEL,current_state
     
     rts
     
.game_over
    bsr stop_sounds

    tst.w   state_timer
    bne.b   .cont
    move.w  #STATE_INTRO_SCREEN,current_state
.cont
    subq.w  #1,state_timer
    rts
.playing
    tst.l   state_timer
    bne.b   .no_first_tick
    lea start_music_sound,a0
    bsr     play_fx
    ; for demo mode
    addq.w  #1,record_input_clock

    ;moveq.l #0,d0   ; start!!
    ;bsr play_music
.no_first_tick

.dec
    bsr update_player
    bsr update_enemies
    
    addq.l  #1,state_timer
    rts
.ready_off


    rts
.update_player_and_enemies

    

    bra check_pac_enemies_collisions
   


; is done after both pacman & enemies have been updated, maybe allowing for the
; "pass-through" bug at higher levels
check_pac_enemies_collisions
    tst.w   player_killed_timer
    bmi.b   .check
    rts
.check
    lea player(pc),a3
    move.w  xpos(a3),d0
    move.w  ypos(a3),d1
    lsr.w   #3,d0
    lsr.w   #3,d1
    
    lea enemies(pc),a4
    moveq.w #3,d7
.gloop
    move.w  xpos(a4),d2
    move.w  ypos(a4),d3
    lsr.w   #3,d2
    lsr.w   #3,d3
    cmp.w   d2,d0
    bne.b   .nomatch
    cmp.w   d3,d1
    beq.b   .collision
.nomatch
    add.w   #Enemy_SIZEOF,a4
    dbf d7,.gloop
    rts
.collision
    ; is the ghost frightened?
    move.w  mode(a4),d0
    cmp.w   #MODE_FRIGHT,d0
    beq.b   .pac_eats_ghost
    cmp.w   #MODE_HANGING,d0
    beq.b   .nomatch        ; ignore eyes
    ; pacman is killed
    tst.b   invincible_cheat_flag
    bne.b   .nomatch    
    move.w  #PLAYER_KILL_TIMER,player_killed_timer
    rts

    
.pac_eats_ghost:
a_ghost_was_eaten:
    move.w  #MODE_HANGING,mode(a4)
    ; test display score with the proper color (reusing pink sprite palette)
    move.w  #GHOST_KILL_TIMER,ghost_eaten_timer
    cmp.w   #STATE_PLAYING,current_state
    bne.b   .no_sound
    ;lea     ghost_eaten_sound(pc),a0
    ;bsr     play_fx
.no_sound
    
    move.w  next_ghost_iteration_score(pc),d0
    add.w   #1,next_ghost_iteration_score
    add.w   d0,d0
    add.w   d0,d0
    lea  score_value_table(pc),a0
    move.l  d0,-(a7)
    move.l   (a0,d0.w),d0
    bsr add_to_score
    move.l  (a7)+,d0
    lea  score_frame_table(pc),a0
    move.l  (a0,d0.w),d0
    move.l  d0,score_frame
    
    ; exits as soon as a collision is found
    rts
update_intro_screen
    tst.l   state_timer
    bne.b   .no_first
    ; first update: init everything
    ; 6 moving white dots, moving on a 34x16 grid (100 dots)
    ; spaced by roughly 16 dots
    ; 1-d coord (0-99) is enough to position them

    clr.w   intro_text_message
    
    lea dot_positions(pc),a0
    move.b  #7,d0
    move.w  #5,d1
.dotloop
    move.b  d0,(a0)+
    add.b   #16,d0
    dbf d1,.dotloop
    
    bsr init_player
   
    
    moveq.l #0,d0
    bsr init_enemies
    lea enemies(pc),a0
    moveq.w #3,d0
.loop
    move.w  #260,xpos(a0)
    move.w  #170,ypos(a0)
    move.w  #LEFT,direction(a0)
    add.w  #Enemy_SIZEOF,a0
    dbf d0,.loop
    
    move.w  #93,.y_target
    move.w   #4*Enemy_SIZEOF,.ghost_to_update    
.no_first
    add.l   #1,state_timer

    ; decrease dot positions (animate)
    lea dot_positions(pc),a0
    move.w  #5,d1
.dotloop2
    move.b  (a0),d0
    subq.b  #1,d0
    bpl.b   .no100
    move.b   #99,d0
.no100
    move.b  d0,(a0)+
    dbf d1,.dotloop2

    ; now enemies

    move.w  .ghost_to_update(pc),d0
    cmp.w   #4*Enemy_SIZEOF,d0
    beq.b   .no_ghost
    
    lea enemies(pc),a0
    add.w   d0,a0

    ; animate ghost
    move.w  frame(a0),d2
    addq.w  #1,d2
    and.w   #$F,d2
    move.w  d2,frame(a0)
    
    cmp.w   #LEFT,direction(a0)
    bne.b   .up
    sub.w   #1,xpos(a0)
    cmp.w   #80-16,xpos(a0)
    bne.b   .no_dirchange
    move.w  #UP,direction(a0)
    bra.b   .no_dirchange
.up
    move.w  ypos(a0),d0
    sub.w   #1,d0
    move.w  d0,ypos(a0)
    cmp.w   .y_target(pc),d0
    bne.b   .no_dirchange
.next_ghost_iteration
    add.w   #1,intro_text_message       ; next ghost / mspacman
    add.w   #Enemy_SIZEOF,.ghost_to_update
    add.w   #17,.y_target
.no_dirchange
    
.no_ghost
    cmp.w   #6,intro_text_message
    bne.b   .nomspac
    lea     player(pc),a4
    cmp.w   #132,xpos(a4)
    beq.b   .nomspac
    eor.w   #1,.anim_ms_pac
    beq.b   .no_anim
    bsr     animate_player
.no_anim
    subq.w  #1,xpos(a4)
.nomspac
    move.l  state_timer(pc),d0
    cmp.l   #$4E0,d0
    beq.b   .demo
    ; text handling
    cmp.w   #58,d0
    bne.b   .no_with
    move.w  #1,intro_text_message
    bra.b   .out_text
.no_with
    cmp.w   #60,d0
    bne.b   .no_blinky
    move.w  #2,intro_text_message
    clr.w   .ghost_to_update    ; start ghost moves
.no_blinky    
.out_text
    rts
.demo
    ; change state
    clr.l   state_timer
    move.w  #STATE_GAME_START_SCREEN,current_state
    ; in demo mode
    st.b    demo_mode
    rts
    
.ghost_to_update
    dc.w    0
.y_target
    dc.w    0
.anim_ms_pac
    dc.w    0


    
    
update_enemies:
    lea enemies(pc),a4
    move.w nb_enemies(pc),d7
    ;;subq.w  #1,d7
    move.w  player_killed_timer(pc),d6
    bmi.b   .gloop
    subq.w  #1,player_killed_timer
    bne.b   .glkill
    ; end current life & restart
    move.w  #STATE_LIFE_LOST,current_state
    rts
.glkill
    bra .animate
    
.gloop
    bra .animate
    rts

.animate
    ; player killed, just update ghost animations but don't move
    move.w  frame(a4),d1
    addq.w  #1,d1
    and.w   #$F,d1
    move.w  d1,frame(a4)
    add.w   #Enemy_SIZEOF,a4
    dbf d7,.animate
    rts
    
play_loop_fx
    tst.b   demo_mode
    bne.b   .nosfx
    lea _custom,a6
    bra _mt_loopfx
.nosfx
    rts
    
; what: sets game state when a power pill has been taken
; trashes: A0,A1,D0,D1
power_pill_taken
    move.l  d2,-(a7)
    ; resets next ghost eaten score
    clr.w  next_ghost_iteration_score

    ; TODO change music
    
    lea enemies(pc),a0
    moveq.w  #3,d0
.gloop

    move.l (a7)+,d2
    rts
    
update_player
    lea     player(pc),a4
    ; no moves
    clr.l  h_speed(a4)  

    move.w  player_killed_timer(pc),d6
    bmi.b   .alive
    moveq.w #0,d0
    move.w  #PLAYER_KILL_TIMER-NB_TICKS_PER_SEC,d5
    sub.w   d6,d5
    bne.b   .no_sound
    lea killed_sound(pc),a0
    bsr play_fx
    bra.b   .frame_done
.no_sound
    bcs.b   .frame_done     ; frame 0

    ; d5 is the timer starting from 0
    lea player_kill_anim_table(pc),a0
    move.b  (a0,d5.w),d0

    lsl.w   #2,d0   ; times 4
    move.w  d0,death_frame_offset
.frame_done    
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
    cmp.l   #demo_moves_end,a0
    bcc.b   .no_demo        ; no more input
    move.b  (a0),d2
    lsl.w   #8,d2
    move.b  (1,a0),d2
    ;;add.b   #3,d2   ; correction???
    cmp.w  record_input_clock(pc),d2
    bne.b   .no_demo        ; don't do anything now
    ; new event
    move.b  (2,a0),d2
    addq.w  #3,a0
    move.l  a0,record_data_pointer
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
    ; set replayed input state
    move.l  d0,replayed_input_state
    
    ; read live or recorded controls
.no_demo
    tst.l   d0
    beq.b   .out        ; nothing is currently pressed: optimize
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
    move.l  d6,h_speed(a4)
    bsr     .move_attempt
    cmp.w   #3,d5
    beq.b   .valid_move
    ; nothing to do, previous move wasn't valid
    bra.b   .no_move
.valid_move
    ; store for later
    move.l  h_speed(a4),previous_valid_direction
    bsr animate_player    
    move.w  d2,xpos(a4)
    move.w  d3,ypos(a4)

    ; check if there are dots to eat
    move.w  d2,d0
    move.w  d2,d6   ; save it
    move.w  d3,d1
    bsr  get_dot_rectangles
    ; set registers d4-d6 with pointers on rectangles²
    move.l  d1,d5
    exg.l   d2,d6
    move.l  d0,d4
    beq.b   .z
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
    addq.w  #5,d1

    ADD_XY_TO_A1    a0
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

    

.no_move
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

DIRCHECK_HORIZ:MACRO
    move.w  v_speed(a4),d4
    beq.b   .no_up
    move.w  d2,d0
    move.w  d3,d1
    ; align on next x tile
    add.w   #\1,d0
    and.w   #$F8,d0
    
    cmp.w   #1,d4
    bne.b   .no_down
    addq.w  #8,d1
    bsr     is_location_legal
    tst.b   d0
    bne.b   .go_right
    rts
.no_down
    ; has to be "up"
    ; up attempt
    subq.w  #8,d1
    bsr     is_location_legal
    tst.b   d0
    beq.b   .no_up
.go_right
    ; looks like player went past the "up" intersection: change direction
    neg.w  previous_valid_direction
.no_up
    rts
    ENDM

DIRCHECK_VERT:MACRO
    move.w  h_speed(a4),d4
    beq.b   .no_left
    move.w  d2,d0
    move.w  d3,d1
    ; align on next y tile
    add.w   #\1,d1
    and.w   #$F8,d1
    
    cmp.w   #1,d4
    bne.b   .no_right
    addq.w  #8,d0
    bsr     is_location_legal
    tst.b   d0
    bne.b   .reverse
    rts
.no_right
    ; has to be "left"
    ; left attempt
    subq.w  #8,d0
    bsr     is_location_legal
    tst.b   d0
    beq.b   .no_left
.reverse
    ; looks like player went past the horizontal intersection: change direction
    neg.w  previous_valid_direction+2
.no_left
    rts
    ENDM


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
    DIRCHECK_HORIZ  -7

dircheck_left
    DIRCHECK_HORIZ  7

dircheck_up
    DIRCHECK_VERT  7
 
dircheck_down
    DIRCHECK_VERT   -7
    
DRAW_RECT_LINE:MACRO
    move.b  #$1F,(\2,\1)
    move.b  #$FF,(\2+1,\1)
    move.b  #$FF,(\2+2,\1)
    move.b  #$FF,(\2+3,\1)
    move.b  #$FE,(\2+4,\1)
    ENDM
DRAW_RECT_LINE_OR:MACRO
    or.b  #$1F,(\2,\1)
    move.b  #$FF,(\2+1,\1)
    move.b  #$FF,(\2+2,\1)
    move.b  #$FF,(\2+3,\1)
    or.b  #$FE,(\2+4,\1)
    ENDM
; what: count dots and draws the rectangle if 0 dots
; (dirty: draws during compute phase)
; < A0: pointer on rectangle
; destroys: D0
count_dot
    move.w  cdots(a0),d0
    beq.b   .no_dots
    subq    #1,d0
    bne.b   .still_dots
    ; fill rectangle with color (plane 1)
    movem.l d1-d2/a0-a3,-(a7)
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
    add.l   d0,a2
    add.l   d0,a3
    move.w  #NB_BYTES_PER_LINE,d1
.filly
    DRAW_RECT_LINE_OR  a1,0
    DRAW_RECT_LINE_OR  a2,0
    DRAW_RECT_LINE  a3,0
    DRAW_RECT_LINE_OR  a1,SCREEN_PLANE_SIZE*2
    add.w   d1,a1
    add.w   d1,a2
    add.w   d1,a3
    dbf d2,.filly
    
    movem.l (a7)+,d1-d2/a0-a3
    moveq.l   #0,d0
    move.w  points(a0),d0
    beq.b   .still_dots
    bsr add_to_score        ; even levels: bonus awarded on rectfill
    clr d0
.still_dots
    move.w  d0,cdots(a0)
.no_dots
    rts
    
    IFD    RECORD_INPUT_TABLE_SIZE
record_input:
    tst.l   d0
    bne.b   .store
    ; 0 twice: ignore (saves space)
    cmp.l   prev_record_joystick_state(pc),d0
    beq.b   .no_input
.store
    move.l  d0,prev_record_joystick_state
    clr.b   d1
    ; now store clock & joystick state, "compressed" to 4 bits (up,down,left,right)
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
    move.l record_data_pointer(pc),a0
    cmp.l   #record_input_table+RECORD_INPUT_TABLE_SIZE-4,a0
    bcc.b   .no_input       ; overflow!!!
    
    ; store clock
    move.b  record_input_clock(pc),(a0)+
    move.b  record_input_clock+1(pc),(a0)+
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
    move.w  #STATE_LEVEL_COMPLETED,current_state
    rts

	
; the palette is organized so we only need to blit planes 0, 2 and 3 (not 1)
; plane 1 contains dots so it avoids to redraw it
; plane 0 contains the grid, that has been backed up
; plane 2 or 3 contains filled up rectangles, needs backing up too TODO
draw_player:
    move.l  previous_player_address(pc),d5    
    lea     player(pc),a2
    tst.w  ghost_eaten_timer
    bmi.b   .normal_pacdraw
    lea     empty_16x16_bob,a0
    bra.b   .pacblit
.normal_pacdraw
    tst.w  player_killed_timer
    bmi.b   .normal
    lea     copier_dead,a0
    move.w  death_frame_offset(pc),d0
    add.w   d0,a0       ; proper frame to blit
    move.l  (a0),a0
    bra.b   .pacblit

.normal
    ; first, restore plane 0
    tst.l   d5    
    beq.b   .no_erase
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

.no_erase

    move.w  direction(a2),d0
    lea  copier_dir_table(pc),a0
    move.l  (a0,d0.w),a0
    move.w  frame(a2),d0
    add.w   d0,d0
    add.w   d0,d0
    move.l  (a0,d0.w),a0
.pacblit

    move.w  xpos(a2),d3
    
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
    beq.b   .no_erase2
    
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
    lea	screen_data+SCREEN_PLANE_SIZE*2,a1
    move.l  a1,a2   ; just restored background
    ; plane 2
    ; a3 is already computed from first cookie cut blit
    lea (BOB_16X16_PLANE_SIZE*2,a0),a0
    move.l  a1,a6
    move.w d3,d0
    move.w d4,d1

    bsr blit_plane_cookie_cut
    
    ; delete third plane too
    tst.l   d5    
    beq.b   .no_erase3
    
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

    
; what: checks if x,y has a dot/fruit/power pill 
; marks the zone to -1 when read (to differentiate with 0 for redraw routine)
; args:
; < d0 : x (screen coords)
; < d1 : y
; > d0 : nonzero (1,2) if collision (dot,power pill), 0 if no collision
; trashes: a0,a1,d1

is_on_bonus:
    bra.b   .cleared
    lea dot_table,a0
    ; apply x,y offset
    lsr.w   #3,d1       ; 8 divide
    lsl.w   #5,d1       ; times 32
    add.w   d1,a0
    lsr.w   #3,d0   ; 8 divide
    add.w   d0,a0
    move.b  (a0),d0
    bmi.b   .cleared
    bne.b   .pill
    rts
.pill
    ; only once!
    st.b   (a0)
    rts
.cleared
    clr.b   d0
    rts
    
; what: checks if x,y collides with dot
; returns valid location out of the maze
; (allows to handle edges, with a limit given by
; the move methods)
; 
; args:
; < d0 : x (screen coords)
; < d1 : y
; > d0,d1: pointers on linked rectangles (either can be NULL)
; trashes: a0

get_dot_rectangles:
    cmp.w   #Y_MAX+1,d1
    bcc.b   .out_of_bounds
    cmp.w   #X_MAX+1,d0
    bcc.b   .out_of_bounds
    ; no need to test sign (bmi) as bcc works unsigned so works on negative!
    ; apply x,y offset
    
    lsr.w   #3,d1       ; 8 divide : tile
    lea     mul26_table(pc),a0
    add.w   d1,d1
    move.w  (a0,d1.w),d1    ; times 26
    lsl.w   #4,d1   ; times 16 (4 32 bit longwords per slot)
    lea dot_table,a0
    add.w   d1,a0
    and.b   #$F8,d0   ; align on 8 (2 32 bit longwords per slot)
    
    add.w   d0,a0
    add.w   d0,a0
    
    move.l  (a0),D0    ; retrieve value of first pointer
    move.l  (4,a0),D1    ; retrieve value of second pointer
    move.l  (8,a0),D2    ; retrieve value of second pointer
    ; now that it's been checked and returned, clear pointers
    clr.l   (a0)+
    clr.l   (a0)+
    clr.l   (a0)+
    rts
.out_of_bounds
    moveq.l   #0,d0
    moveq.l   #0,d1
    rts
    
; what: checks if x,y collides with maze
; returns valid location out of the maze
; (allows to handle edges, with a limit given by
; the move methods)
; args:
; < d0 : x (screen coords)
; < d1 : y
; > d0.b : 1 if collision, 0 if no collision
; trashes: a0,a1,d1

is_location_legal:
    cmp.w   #Y_MAX+1,d1
    bcc.b   .out_of_bounds
    cmp.w   #X_MAX+1,d0
    bcc.b   .out_of_bounds
    ; no need to test sign (bmi) as bcc works unsigned so works on negative!
    ; apply x,y offset
    lsr.w   #3,d1       ; 8 divide : tile
    lea     mul26_table(pc),a0
    add.w   d1,d1
    move.w  (a0,d1.w),d1    ; times 26
    move.l maze_wall_table(pc),a0
    add.w   d1,a0
    lsr.w   #3,d0   ; 8 divide
    move.b  (a0,d0.w),d0    ; retrieve value
    rts
.out_of_bounds
    move.b   #1,d0  ; allowed, the move routine already has bounds
    rts
   

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
; blit mask set
; trashes D0-D6
; > A1: even address where blit was done
blit_plane_any_internal:
    ; pre-compute the maximum of shit here
    lea mul40_table(pc),a2
    add.w   d1,d1
    beq.b   .d1_zero    ; optim
    move.w  (a2,d1.w),d1
    swap    d1
    clr.w   d1
    swap    d1
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
; < D0,D1: x,y
; < A0: source
; < A1: destination
; < A2: background to mix with cookie cut
; < A3: source mask for cookie cut
; < A4: multiplication table for background (x28 for maze, x40 for screen)
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
    add.w   d1,d1
    move.w  d1,d6   ; save it
    beq.b   .d1_zero    ; optim
    move.w  (a4,d1.w),d1
    swap    d1
    clr.w   d1
    swap    d1
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
    add.l   d1,a1       ; plane position

    ; a4 is a multiplication table
    ;;beq.b   .d1_zero    ; optim
    move.w  (a4,d6.w),d1
    swap    d1
    clr.w   d1
    swap    d1
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
    
; what: writes an decimal number with a given color
; args:
; < A1: plane
; < D0: X (multiple of 8)
; < D1: Y
; < D2: number value
; < D3: number of padding zeroes
; < D4: RGB4 color
; > D0: number of characters written
    
write_color_decimal_number
    movem.l A0/D2-d6,-(a7)
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
    movem.l (a7)+,A0/D2-d6
    rts
.write_num
    bsr convert_number
    move.w  d4,d2
    bra write_color_string
    
    
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
    
; what: writes a text in a given color
; args:
; < A0: c string
; < D0: X (multiple of 8)
; < D1: Y
; < D2: RGB4 color (must be in palette!)
; > D0: number of characters written
; trashes: none

write_color_string
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
    cmp.b   #'"',d2
    bne.b   .noquote
    lea quote(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.noquote
    cmp.b   #'$',d2
    bne.b   .noquote2
    lea quote2(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.noquote2
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
    lea high_score(pc),a1
    moveq.l #4,d0   ; size
    moveq.l #0,d1   ; offset
    jmp  (resload_LoadFileOffset,a2)
    
.standard
    move.l  _dosbase(pc),a6
    move.l  a0,d1
    move.l  #MODE_OLDFILE,d2
    jsr     (_LVOOpen,a6)
    move.l  d0,d1
    beq.b   .no_file
    move.l  d1,d4
    move.l  #4,d3
    move.l  #high_score,d2
    jsr (_LVORead,a6)
    move.l  d4,d1
    jsr (_LVOClose,a6)    
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
    lea high_score(pc),a1
    moveq.l #4,d0   ; size
    jmp  (resload_SaveFile,a2)
    
.standard
    move.l  _dosbase(pc),a6
    move.l  a0,d1
    move.l  #MODE_NEWFILE,d2
    jsr     (_LVOOpen,a6)
    move.l  d0,d1
    beq.b   .out
    move.l  d1,d4
    move.l  #4,d3
    move.l  #high_score,d2
    jsr (_LVOWrite,a6)
    move.l  d4,d1
    jsr (_LVOClose,a6)    
.out
    rts
    
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
replayed_input_state
    dc.l    0
record_data_pointer
    dc.l    0
record_input_clock
    dc.w    0    
    IFD    RECORD_INPUT_TABLE_SIZE
prev_record_joystick_state
    dc.l    0

    ENDC

  
current_state
    dc.w    0
score
    dc.l    0
displayed_score
    dc.l    0
high_score
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
next_ghost_iteration_score
    dc.w    0
previous_player_address
    dc.l    0
previous_valid_direction
    dc.l    0

score_frame
    dc.l    0
global_speed_table
    dc.l    0
dot_table_read_only:
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
player_killed_timer:
    dc.w    -1
ghost_eaten_timer:
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

nb_enemies
    dc.w    4
    
maze_outline_color
    dc.w    0
maze_fill_color
    dc.w    0
total_number_of_dots:
    dc.b    0

nb_lives:
    dc.b    0
music_playing:    
    dc.b    0
pause_flag
    dc.b    0
dot_positions
    ds.b    6,0
quit_flag
    dc.b    0
elroy_mode_lock:
    dc.b    0


nb_dots_eaten
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


    even

    
bonus_score_display_message:
    dc.w    0
extra_life_message:
    dc.w    0
    
score_table
    dc.w    0,1,5
fruit_score     ; must follow score_table
    dc.w    10
loop_array:
    dc.l    0,0,0,0
    

player_kill_anim_table:
    REPT    NB_TICKS_PER_SEC/4
    dc.b    1
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    2
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    3
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    4
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    5
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    6
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    7
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    8
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    9
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    10
    ENDR
    REPT    NB_TICKS_PER_SEC/4
    dc.b    11
    ENDR
    REPT    NB_TICKS_PER_SEC+NB_TICKS_PER_SEC/4
    dc.b    11		; TEMP
    ENDR
    even
    
    even

    

cheat_sequence
    dc.b    $26,$18,$14,$22,0
    even
; table with 2 bytes: 60hz clock, 1 byte: move mask for the demo
demo_moves:
    incbin  "moves.bin"
demo_moves_end:
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
    dc.l    rustler_\1_0,rustler_\1_0,rustler_\1_0,rustler_\1_0,rustler_\1_0,rustler_\1_0
    dc.l    rustler_\1_1,rustler_\1_1,rustler_\1_1,rustler_\1_1,rustler_\1_1,rustler_\1_1
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
quote
    incbin  "quote.bin"
quote2
    incbin  "quote2.bin"
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
    
    ; fright time + number of flashes. Each flash is 14 frames long
    ; 4 words: total number of frames for fright mode,
    ;          number of frames to start flashing
DEF_FRIGHT_ENTRY:MACRO
    dc.w    ORIGINAL_TICKS_PER_SEC*\1,NB_FLASH_FRAMES*\2*2
    ENDM
    

score_frame_table:
    dc.l    score_200,score_400,score_800,score_1600,score_1600,score_3200
score_value_table
    dc.l    20,40,80,160,160,320
    
; what: get current move speed for ghost
; < A0: ghost structure
; > D0.W: 0,1,2 depending on level, mode, etc...
; trashes: nothing
get_ghost_move_speed:   
    movem.l  d1-d2/a1/a4,-(a7)
    move.l  a0,a4
    move.w  speed_table_index(a4),d1
    add.w   #1,d1
    cmp.w   #16,d1
    bne.b   .nowrap
    moveq   #0,d1
.nowrap
    ; store
    move.w  d1,speed_table_index(a4)

    ; depends on the level???
    
.table_computed
    move.l  global_speed_table(pc),a1
    move.w  speed_table_index(a4),d0
    move.b  (a1,d0.w),d0            ; get speed index
    ext.w   d0
    movem.l (a7)+,d1-d2/a1/a4
    rts



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
    SOUND_ENTRY start_music,1,SOUNDFREQ
    SOUND_ENTRY enemy_hit,1,SOUNDFREQ
    SOUND_ENTRY enemy_killed,2,SOUNDFREQ
    SOUND_ENTRY killed,1,SOUNDFREQ
    SOUND_ENTRY credit,1,SOUNDFREQ
    SOUND_ENTRY eat,3,SOUNDFREQ




    dc.l    0
    


 ; speed table at 60 Hz
speed_table:  ; lifted from https://github.com/shaunlebron/pacman/blob/master/src/Actor.js
    dc.l    speeds_level1,speeds_level2_4,speeds_level2_4,speeds_level2_4
    dc.l    speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20
    dc.l    speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20
    dc.l    speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level21
    
speeds_level1:
                                            ; LEVEL 1
    dc.b   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ; pac-man (normal)
    dc.b   0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ; enemies (normal)
    dc.b   1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,1 ; pac-man (fright)
    dc.b   0,1,1,0,1,1,0,1,0,1,1,0,1,1,0,1 ; enemies (fright)
    dc.b   0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1 ; enemies (tunnel)
    dc.b   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ; elroy 1
    dc.b   1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1 ; elroy 2
speeds_level2_4:
                                           ; LEVELS 2-4
    dc.b   1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,1 ; pac-man (normal)
    dc.b   1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1 ; enemies (normal)
    dc.b   1,1,1,1,2,1,1,1,1,2,1,1,1,1,2,1 ; pac-man (fright)
    dc.b   0,1,1,0,1,1,0,1,1,0,1,1,0,1,1,1 ; enemies (fright)
    dc.b   0,1,1,0,1,0,1,0,1,1,0,1,0,1,0,1 ; enemies (tunnel)
    dc.b   1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,1 ; elroy 1
    dc.b   1,1,1,1,2,1,1,1,1,2,1,1,1,1,2,1 ; elroy 2
                                           ;
speeds_level5_20                                          
                                           ; LEVELS 5-20
    dc.b   1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1 ; pac-man (normal)
    dc.b   1,1,1,1,2,1,1,1,1,2,1,1,1,1,2,1 ; enemies (normal)
    dc.b   1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1 ; pac-man (fright) (N/A for levels 17, 19 & 20)
    dc.b   0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1 ; enemies (fright)  (N/A for levels 17, 19 & 20)
    dc.b   0,1,1,0,1,1,0,1,0,1,1,0,1,1,0,1 ; enemies (tunnel)
    dc.b   1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1 ; elroy 1
    dc.b   1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1 ; elroy 2
                                           ;
speeds_level21                                          
                                           ; LEVELS 21+
    dc.b   1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,1 ; pac-man (normal)
    dc.b   1,1,1,1,2,1,1,1,1,2,1,1,1,1,2,1 ; enemies (normal)
    dc.b   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; pac-man (fright) N/A
    dc.b   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; enemies (fright)  N/A
    dc.b   0,1,1,0,1,1,0,1,0,1,1,0,1,1,0,1 ; enemies (tunnel)
    dc.b   1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1 ; elroy 1
    dc.b   1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1; elroy 2


powerdots
    ds.l    4

; palette is different for frightened enemies & eyes
frightened_enemies_blue_palette
    
frightened_enemies_white_palette
    

game_palette
    include "palette.s"
    
player:
    ds.b    Player_SIZEOF
    even

enemies:
    ds.b    Enemy_SIZEOF*7
    even

    

keyboard_table:
    ds.b    $100,0
    
    include "maze_data.s"       ; generated by "convert_sprites.py" python script

    
; BSS --------------------------------------
    SECTION  S2,BSS
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
dot_table
    ds.b    NB_TILES_PER_LINE*NB_TILE_LINES*16
    
    even
    
    
    SECTION  S3,CODE
    include ptplayer.s

    SECTION  S4,DATA,CHIP
; main copper list
coplist
bitplanes:
   dc.l  $01080000
   dc.l  $010a0000
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
    ; #1
    dc.w    sprpt+0,0
    dc.w    sprpt+2,0
    ; #2
    dc.w    sprpt+4,0
    dc.w    sprpt+6,0
    ; #3
    dc.w    sprpt+8,0
    dc.w    sprpt+10,0
    ; #4
    dc.w    sprpt+12,0
    dc.w    sprpt+14,0
    ; #5
    dc.w    sprpt+16,0
    dc.w    sprpt+18,0
    ; #6
    dc.w    sprpt+20,0
    dc.w    sprpt+22,0
    ; #7
thief_sprite:
    dc.w    sprpt+24,0
    dc.w    sprpt+26,0
score_sprite_entry:     ; can use since there's white in thief sprite
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

    
     
ghost_bob_table:
    dc.l    .ghost_bobs,.ghost_bobs+320,.ghost_bobs+320*2,.ghost_bobs+320*3
.ghost_bobs:


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
copier_dead
    dc.l    copier_dead_0,copier_dead_1,copier_dead_2
    dc.l    copier_dead_0,copier_dead_1,copier_dead_2
    dc.l    copier_dead_0,copier_dead_1,copier_dead_2
    dc.l    empty_16x16_bob


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

empty_16x16_bob
    ds.b    64*4,0
rustler_dead
    dc.l    rustler_dead_0,rustler_dead_1,rustler_dead_2
    dc.l    rustler_dead_0,rustler_dead_1,rustler_dead_2
    dc.l    rustler_dead_0,rustler_dead_1,rustler_dead_2
    dc.l    empty_16x16_bob
lives
    incbin  "life.bin"
star
    incbin  "star.bin"



bonus_scores:
    incbin  "bonus_5000.bin"



  
    
    
DECL_GHOST:MACRO
\1_police_frame_table:
    dc.l    \1_police_0
    dc.l    \1_police_1
    dc.l    \1_police_2
    dc.l    \1_police_3
\1_police_end_frame_table:
\1_police_jump_frame_table:
    dc.l    \1_police_jump_0
    dc.l    \1_police_jump_1
\1_police_jump_end_frame_table:
\1_police_fall_frame_table:
    dc.l    \1_police_fall_0
    dc.l    \1_police_fall_1
    dc.l    \1_police_fall_2
    dc.l    \1_police_fall_3
\1_police_fall_end_frame_table:

    
    ; all enemies share the same graphics, only the colors are different
    ; but we need to replicate the graphics 8*4 times because of sprite control word
\1_police_0
    dc.l    0
    incbin  "police_0.bin"
    dc.l    0
\1_police_1
    dc.l    0
    incbin  "police_1.bin"
    dc.l    0
\1_police_2
    dc.l    0
    incbin  "police_2.bin"
    dc.l    0
\1_police_3
    dc.l    0
    incbin  "police_3.bin"
    dc.l    0
\1_police_hang_0
    dc.l    0
    incbin  "police_hang_0.bin"
    dc.l    0
\1_police_hang_1
    dc.l    0
    incbin  "police_hang_1.bin"
    dc.l    0
\1_police_jump_0
    dc.l    0
    incbin  "police_jump_0.bin"
    dc.l    0
\1_police_jump_1
    dc.l    0
    incbin  "police_jump_1.bin"
    dc.l    0
\1_police_fall_0
    dc.l    0
    incbin  "police_fall_0.bin"
    dc.l    0
\1_police_fall_1
    dc.l    0
    incbin  "police_fall_1.bin"
    dc.l    0
\1_police_fall_2
    dc.l    0
    incbin  "police_fall_2.bin"
    dc.l    0
\1_police_fall_3
    dc.l    0
    incbin  "police_fall_3.bin"
    dc.l    0


    ENDM
        
    DECL_GHOST  police1
    DECL_GHOST  police2
    DECL_GHOST  police3
    DECL_GHOST  police4
    DECL_GHOST  police5
    DECL_GHOST  police6
    DECL_GHOST  police7

; special sprites for intermissions

score_200:
    dc.l    0
    incbin  "scores_200.bin"      ; 64 bytes each, palette from pink sprite
    dc.l    0
score_400:
    dc.l    0
    incbin  "scores_400.bin"
    dc.l    0
score_800:
    dc.l    0
    incbin  "scores_800.bin"
    dc.l    0
score_1600:
    dc.l    0
    incbin  "scores_1600.bin"
    dc.l    0
score_3200:
    dc.l    0
    incbin  "scores_3200.bin"
    dc.l    0

killed_raw
    incbin  "pacman_killed.raw"
    even
killed_raw_end
credit_raw
    incbin  "credit.raw"
    even
credit_raw_end




start_music_raw
    incbin  "start_music.raw"
    even
start_music_raw_end
enemy_hit_raw
    incbin  "enemy_hit.raw"
    even
enemy_hit_raw_end
enemy_killed_raw
    incbin  "enemy_killed.raw"
    even
enemy_killed_raw_end

eat_raw
    incbin  "eat.raw"
    even
eat_raw_end

    even

  
music:

    
empty_sprite
    dc.l    0,0

    
    SECTION S_4,BSS,CHIP
; add small safety in case some blit goes beyond screen
screen_data:
    ds.b    SCREEN_PLANE_SIZE*NB_PLANES+NB_BYTES_PER_LINE,0

    
    	