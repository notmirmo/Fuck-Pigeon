INCLUDE "hardware.inc"
INCLUDE "charmap.inc"

def OBJ_1 equ 0
def OBJ_2 equ 4
def OBJ_3 equ 8

SECTION "Stat Interrupt", ROM0[$0048]
StatInterrupt:

    push af

    ; Check if we are on the first scanline
    ldh a, [rLYC]
    cp 0
    jp z, LYCEqualsZero

LYCEquals8:

    ; Don't call the next stat interrupt until scanline 8
    ld a, 0
    ldh [rLYC], a

    ; Turn the LCD on including sprites. But no window
    ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON | LCDCF_WINOFF | LCDCF_WIN9C00
    ldh [rLCDC], a

    jp EndStatInterrupts

LYCEqualsZero:

    ; Don't call the next stat interrupt until scanline 8
    ld a, 8
    ldh [rLYC], a

    ; Turn the LCD on including the window. But no sprites
    ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON | LCDCF_WINON | LCDCF_WIN9C00
    ldh [rLCDC], a


EndStatInterrupts:

    pop af

    reti;



SECTION "Header", ROM0[$100]

	jp EntryPoint

	ds $150 - @, 0

EntryPoint:
	ld a, 0
	ld [rNR52], a

  	; Do not turn the LCD off outside of VBlank
WaitVBlank:
	ld a, [rLY]
	cp 144
	jp c, WaitVBlank

	; Turn the LCD off
	ld a, 0
	ld [rLCDC], a

	;Copy Tiles in ram
	ld de, Tiles
	ld hl, $9000
	ld bc, TilesEnd - Tiles
	call Memcopy

	;Copy the tilemap
	ld de, TileMapTitle
	ld hl, $9800
	ld bc, TileMapTitleEnd - TileMapTitle
	call Memcopy

	ld a,6
	ld b,4
	ld hl, StringVar.PigeonString
	call CopyString

	ld a,14
	ld b,6
	ld hl, StringVar.PressString
	call CopyString

	;Copy the Window to ram
	ld de, WindowScore
	ld hl, $9C00
	ld bc, WindowScoreEnd - WindowScore
	call Memcopy
	ld a,80
	ld [rWX],a

	;Copy Plane in ram
	ld de, ObjectTiles
	ld hl, $8000
	ld bc, ObjectTilesEnd - ObjectTiles
	call Memcopy

	ld a, 0
	ld b, 160
	ld hl, _OAMRAM
ClearOam:
	ld [hli], a
	dec b
	jp nz, ClearOam

	;change OBP0 palette
	ld hl, $FF48
	ld a, %11100100
	ld [hl], a

	;initialyze the plane 1
	ld hl, _OAMRAM
	ld a, 16 + 50 ; y = 50
	ld [hli],a
	ld a, 8 + 4; x = 4
	ld [hli], a
	ld a, 0
	ld [hli], a
	ld [hli], a
	;plane 2
	ld a, 16 + 50 ; y = 50
	ld [hli],a
	ld a, 16 + 4; x = 4
	ld [hli], a
	ld a, 1
	ld [hli], a
	ld a, 0
	ld [hli], a
	;plane 3
	ld a, 16 + 50 ; y = 50
	ld [hli],a
	ld a, 24 + 4; x = 4
	ld [hli], a
	ld a, 2
	ld [hli], a
	ld a, 0
	ld [hli], a




	; Turn the LCD on
	ld a, LCDCF_ON | LCDCF_BGON
	ld [rLCDC], a

	; During the first (blank) frame, initialize display registers
	ld a, %11100100
	ld [rBGP], a

	ld a, 0
  	ld [wFrameCounter], a
	ld [wCurKeys], a
	ld [wNewKeys], a
	ld [wTimer], a
	ld [wTimer+1], a
	ld [wRandom], a
	ld [wBool], a
	ld [wGameRunning], a


Main:
	; Wait until it's not VBlank
	ld a, [rLY]
	cp 144
	jp nc, Main
WaitVBlank2:
	ld a,[rLY]
	cp 144
	jp c, WaitVBlank2

	call ChangeRandomVar
	ld a,[wGameRunning]
	cp 0
	jp nz, MainGame
	call UpdateKeys
	ld a, [wCurKeys]
	and PADF_A
	jp z, Main

	ld a, 0
	ld [rLCDC], a

	ld de, Tilemap
	ld hl, $9800
	ld bc, TilemapEnd - Tilemap
	call Memcopy

	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON | LCDCF_WINON | LCDCF_WIN9C00
	ld [rLCDC], a
	call InitStatInterrupts
	ld a,1
	ld [wGameRunning], a
	jp Main

MainGame:
	call UpdateBG
	call UpdateKeys
	call Timer
	call PigeonSpawn
	call ClearPigeon
	call PlaneHitPigeon


CheckUp:
	ld a, [wCurKeys]
	and a, PADF_UP
	jp z, CheckDown
Up:
	ld a, [_OAMRAM + 0]
	dec a
	dec a
	cp a, 30
	jp z, Main
	ld [_OAMRAM + 0], a
	ld a, [_OAMRAM + 4]
	dec a
	dec a
	ld [_OAMRAM + 4], a
	ld a, [_OAMRAM + 8]
	dec a
	dec a
	ld [_OAMRAM + 8], a
	jp Main

CheckDown:
	ld a, [wCurKeys]
	and a, PADF_DOWN
	jp z, Main
Down:
	;Move 1 pixel to down
	ld a, [_OAMRAM + 0]
	inc a
	inc a
	cp a, 140
	jp z, Main
	ld [_OAMRAM + 0], a
	ld a, [_OAMRAM + 4]
	inc a
	inc a
	ld [_OAMRAM + 4], a
	ld a, [_OAMRAM + 8]
	inc a
	inc a
	ld [_OAMRAM + 8], a
	jp Main


	jp Main





; FUNCTION SECTION
; FUNCTION SECTION
; FUNCTION SECTION


InitStatInterrupts::

    ld a, IEF_STAT
    ldh [rIE], a
    xor a, a ; This is equivalent to `ld a, 0`!
    ldh [rIF], a
    ei

    ; This makes our stat interrupts occur when the current scanline is equal to the rLYC register
    ld a, STATF_LYC
    ldh [rSTAT], a

    ; We'll start with the first scanline
    ; The first stat interrupt will call the next time rLY = 0
    ld a, 0
    ldh [rLYC], a

    ret



DrawScore: ; first digit $9C0A
	ld hl, $9C0B
DrawScorePlus:
	dec hl
	ld a,[hl]
	cp $29     ; saute si hl est != à '9'
	jp nz,DrawScoreEnd
	ld [hl], $20;load '0'
	jp DrawScorePlus
DrawScoreEnd:
	inc a
	ld [hl],a
    ret


PlaneHitPigeon:
	ld a, [_OAMRAM + 0]
	sub 16 ;IUHDuishfhsfiuhsdidshfdshfisnflkjsdnfdslkfnjkldnkjldfvlkjfdnvf
	srl a ;
	srl a ;
	srl a ; divise par 8 la position Y de l'avion
	ld h,0
	ld l,a
	add hl, hl ; *2
	add hl, hl ; *4
	add hl, hl ; *8
	add hl, hl ; *16
	add hl, hl ; multiplie hl par 32

	ld a,[rSCX]
	srl a ;
	srl a ;
	srl a ; divise par 8 rSCX
	ld b,0
	ld c,a
	add hl,bc
	ld bc,$9800
	add hl,bc
	ld a ,[hli]
	cp $1B
	jp z, GameOver
	ld a ,[hli]
	cp $1B
	jp z, GameOver
	ld a ,[hli]
	cp $1B
	jp z, GameOver	
	ret

GameOver:
	ld a, [rSCX]
	srl a ;
	srl a ;
	srl a ; a = how many tile to add for padding
	ld b,0
	ld c,a
	ld de, StringVar.Game_Over ; $9920 is where to print
	ld hl, $9906
	add hl,bc ;
	ld c,0
GameOverCopy: ; comp avec 9920
	ld a,l
	cp $20
	jp c, GameOverOk
	sub $20
	ld hl,$9900
	ld l,a


GameOverOk:
	ld a,[de]
	cp 255
	jp z, GameOver1
	ld [hli],a
	inc de
	jp GameOverCopy
GameOver1:
	call UpdateKeys
	ld a, [wCurKeys]
	and a, PADF_A
	jp nz, Replay
	jp GameOver1


Replay:
	ld a, [rLY]
	cp 144
	jp nc, Main
WaitVBlank3:
	ld a,[rLY]
	cp 144
	jp c, WaitVBlank3

	ld a, 0
	ld [rLCDC],a
	ld de, Tilemap
	ld hl,$9800
	ld bc, TilemapEnd - Tilemap
	call Memcopy
	ld a,0
	ld [wFrameCounter],a
	ld [wTimer],a
	ld hl, $9C07
	ld [hl], $20
	inc hl
	ld [hl], $20
	inc hl
	ld [hl], $20
	inc hl
	ld [hl], $20
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON | LCDCF_WINON | LCDCF_WIN9C00
	ld [rLCDC],a
	jp Main


PigeonSpawn:
	ld a,[wRandom]        ;
	cp 10                 ; 
	jp nc, PigeonSpawnEnd ; Spawn si le nombre random(max 0xFF ) < 10
	
	call ChangeRandomVar
	ld a,[wRandom]  ;
	ld b,16         ;
	call FuncModulo ; choisit un nombre aléatoire entre 2 et 18
	cp a, 2
	jp nc, PigeonSpawnJump
	or %00000010    ;
PigeonSpawnJump:    ;
	ld hl, $9800 ;
	ld de, 32    ;
	jp PMultEnd  ;
PMult:           ;
	add hl, de   ;
	dec a        ;
PMultEnd:        ;
	cp a,0       ;
	jp nz, PMult ;
	ld a, [rSCX] ;
	srl a        ;
	srl a        ;
	srl a        ;
	add 20
	ld b, 0      ;
	ld c,a       ;
	ld a, [rSCX] ;
	cp a, 96
	jp c, PigeonSpawnJump2
	ld a,c
	sub 32
	ld c, a
PigeonSpawnJump2:
	add hl,bc    ; Fait apparaitre le pigeon en dehors du cadre.
	ld [hl], $1B
PigeonSpawnEnd:
	ret


ClearPigeon:
	ld a, [rSCX]
	dec a
	dec a
	dec a
	dec a
	dec a
	srl a
	srl a
	srl a
	ld b,0
	ld c,a
	ld a, 2
	ld hl, $9800
	ld de,32
	add hl,de
	add hl,bc
ClearPigeonJump:
	cp 16
	jp nc,ClearPigeonEnd ; saut si a > 16
	add hl,de
	ld [hl], $00
	inc a
	jp ClearPigeonJump
ClearPigeonEnd:
	ret


; a % b = hl
;Modulo Fonction
FuncModulo:
	cp b           ; Comparer A avec B
	jr c, FuncModuloEnd   ; Sauter à DIVEND si A < B (division terminée)
	sub b          ; Soustraire B de A
	jr FuncModulo     ; Répéter la division
FuncModuloEnd:
	ret

;Timer function
Timer:
	jp TimerStart
TimerPlus2:
	ld a,0
	ld [wTimer],a
	ld a,[wTimer+1]
	inc a
	ld [wTimer+1],a
	jp TimerEnd
TimerPlus1:
	call DrawScore
	ld a, 0
	ld [wFrameCounter], a
	ld a, [wTimer]
	cp $FF
	jp z,TimerPlus2
	inc a
	ld [wTimer], a
	jp TimerEnd
TimerStart:
	ld a, [wFrameCounter]
	inc a
	ld [wFrameCounter], a
	cp a, 60
	jp z, TimerPlus1
TimerEnd:
	ret


; change la variable wRandom 
ChangeRandomVar:
	ld a, [wRandom]
	ld hl, wFrameCounter
	ld b, [hl]
	xor a,b
	xor a, $FF
	ld hl, wCurKeys
	xor a,[hl]
	ld hl,$FF04
	xor a,[hl]
	rlc a
	rlc a
	ld [wRandom],a
	ret


UpdateBG:
	ld a,[rSCX]
	add a,2
	jp Test2
Test1:
	ld a,0
Test2:
	cp a,255
	jp nc,Test1
	ld [rSCX],a
	ret

; Copy byte form one area to another
; de = Source (The data on the ROM)
; hl = Destination (Where to put on the ram)
; bc = Length
Memcopy:
	ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or a, c
    jp nz, Memcopy
    ret

; Print string to the screen
; a = y
; b = x
; hl = string location
CopyString:
	push hl
	ld hl, $9800
	ld de, 32
	jp MultEnd
Mult:
	add hl, de
	dec a
MultEnd:
	cp a,0
	jp nz, Mult
	ld d, 0
	ld e, b
	add hl, de
	pop de ; at this point we have de = where is the str, hl = where to load the str, and 'a' and 'b' is free to use
CopyString2:
	ld a, [de]
	cp a, 255
	jp z, CopyStringEnd
	ld [hli], a
	inc de
	jp CopyString2
CopyStringEnd:
	ret


UpdateKeys:
  	; Poll half the controller
  	ld a, P1F_GET_BTN
	call .onenibble
	ld b, a ; B7-4 = 1; B3-0 = unpressed buttons

	; Poll the other half
	ld a, P1F_GET_DPAD
	call .onenibble
	swap a ; A3-0 = unpressed directions; A7-4 = 1
	xor a, b ; A = pressed buttons + directions
	ld b, a ; B = pressed buttons + directions

	; And release the controller
	ld a, P1F_GET_NONE
	ldh [rP1], a

	; Combine with previous wCurKeys to make wNewKeys
	ld a, [wCurKeys]
	xor a, b ; A = keys that changed state
	and a, b ; A = keys that changed to pressed
	ld [wNewKeys], a
	ld a, b
	ld [wCurKeys], a
	ret

.onenibble
	ldh [rP1], a ; switch the key matrix
	call .knownret ; burn 10 cycles calling a known ret
	ldh a, [rP1] ; ignore value while waiting for the key matrix to settle
	ldh a, [rP1]
	ldh a, [rP1] ; this read counts
	or a, $F0 ; A7-4 = 1; A3-0 = unpressed keys
.knownret
	ret


; TILES AND TILEMAP DATA
; TILES AND TILEMAP DATA
; TILES AND TILEMAP DATA
SECTION "Tile data", ROM0

Tiles:
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; Blank (White) $00
	db $00, $00, $40, $3C, $84, $42, $84, $42, $84, $42, $80, $7E, $84, $42, $84, $42 ; A $01
	db $00, $00, $80, $7C, $84, $42, $84, $42, $80, $7C, $84, $42, $84, $42, $80, $7C ; B $02
	db $00, $00, $40, $3C, $84, $42, $80, $40, $80, $40, $80, $40, $84, $42, $40, $3C ; C $03
	db $00, $00, $80, $7C, $84, $42, $84, $42, $84, $42, $84, $42, $84, $42, $80, $7C ; D $04
	db $00, $00, $80, $7C, $80, $40, $80, $40, $80, $7C, $80, $40, $80, $40, $80, $7C ; E $05
	db $00, $00, $80, $7C, $80, $40, $80, $40, $80, $7C, $80, $40, $80, $40, $80, $40 ; F $06
	db $00, $00, $40, $38, $88, $44, $80, $40, $80, $40, $90, $4E, $88, $44, $40, $3C ; G $07
	db $00, $00, $84, $42, $84, $42, $84, $42, $80, $7E, $84, $42, $84, $42, $84, $42 ; H $08
	db $00, $00, $40, $3E, $10, $08, $10, $08, $10, $08, $10, $08, $10, $08, $40, $3E ; I $09
	db $00, $00, $40, $3E, $10, $08, $10, $08, $10, $08, $90, $48, $90, $48, $40, $30 ; J $0A
	db $00, $00, $88, $44, $90, $48, $A0, $50, $80, $60, $A0, $50, $90, $48, $88, $44 ; K $0B
	db $00, $00, $40, $20, $40, $20, $40, $20, $40, $20, $40, $20, $40, $20, $40, $3C ; L $0C
	db $00, $00, $88, $44, $90, $6C, $A8, $54, $88, $44, $88, $44, $88, $44, $88, $44 ; M $0D
	db $00, $00, $84, $42, $84, $62, $A4, $52, $94, $4A, $88, $46, $84, $42, $84, $42 ; N $0E
	db $00, $00, $40, $3C, $84, $42, $84, $42, $84, $42, $84, $42, $84, $42, $40, $3C ; O $0F
	db $00, $00, $80, $78, $88, $44, $88, $44, $80, $78, $80, $40, $80, $40, $80, $40 ; P $10
	db $00, $00, $40, $3C, $84, $42, $84, $42, $A4, $52, $94, $4A, $40, $3C, $04, $02 ; Q $11
	db $00, $00, $80, $78, $88, $44, $88, $44, $80, $78, $90, $48, $88, $44, $88, $44 ; R $12
	db $00, $00, $40, $3C, $80, $40, $80, $40, $40, $3C, $04, $02, $04, $02, $40, $3C ; S $13
	db $00, $00, $80, $7C, $20, $10, $20, $10, $20, $10, $20, $10, $20, $10, $20, $10 ; T $14
	db $00, $00, $84, $42, $84, $42, $84, $42, $84, $42, $84, $42, $84, $42, $40, $3C ; U $15
	db $00, $00, $88, $44, $88, $44, $50, $28, $50, $28, $50, $28, $20, $10, $20, $10 ; V $16
	db $00, $00, $94, $4A, $94, $4A, $94, $4A, $94, $4A, $94, $4A, $94, $4A, $48, $34 ; W $17
	db $00, $00, $82, $41, $44, $22, $28, $14, $10, $08, $28, $14, $44, $22, $82, $41 ; X $18
	db $00, $00, $44, $22, $28, $14, $10, $08, $10, $08, $10, $08, $10, $08, $10, $08 ; Y $19
	db $00, $00, $80, $7E, $08, $04, $10, $08, $20, $10, $40, $20, $80, $40, $80, $7E ; Z $1A
	db $00, $00, $31, $31, $7B, $5B, $7F, $FF, $3F, $3F, $3E, $3E, $1E, $0A, $00, $00 ; the fucking pigeon $1B
	db $00, $FF, $40, $BF, $06, $F9, $48, $B7, $40, $BF, $24, $DB, $00, $FF, $02, $FD ; Grass (down) $1C
	db $00, $00, $00, $00, $00, $00, $99, $00, $FF, $00, $62, $9D, $00, $FF, $00, $FF ; Grass (up) $1D
	db $FF, $00, $DF, $00, $6B, $00, $DF, $00, $7D, $00, $EB, $00, $FD, $00, $AD, $00 ; Cloud (up) $1E
	db $FF, $00, $DF, $00, $EB, $00, $DF, $00, $FD, $00, $FB, $00, $FF, $00, $EF, $00 ; Cloud (down) $1F
	db $00, $00, $40, $3C, $84, $42, $84, $42, $84, $42, $84, $42, $84, $42, $40, $3C ; 0 $20
	db $00, $00, $20, $10, $40, $30, $A0, $50, $20, $10, $20, $10, $20, $10, $80, $7C ; 1 $21
	db $00, $00, $40, $3C, $84, $42, $84, $42, $20, $1C, $40, $20, $80, $40, $80, $7E ; 2 $22
	db $00, $00, $80, $78, $08, $04, $08, $04, $80, $78, $08, $04, $08, $04, $80, $78 ; 3 $23
	db $00, $00, $90, $48, $90, $48, $90, $48, $90, $48, $80, $7E, $10, $08, $10, $08 ; 4 $24
	db $00, $00, $80, $7E, $80, $40, $80, $40, $40, $3C, $04, $02, $04, $02, $80, $7C ; 5 $25
	db $00, $00, $40, $3C, $80, $40, $80, $40, $80, $7C, $84, $42, $84, $42, $40, $3C ; 6 $26
	db $00, $00, $80, $7E, $04, $02, $08, $04, $10, $08, $20, $10, $40, $20, $80, $40 ; 7 $27
	db $00, $00, $40, $3C, $84, $42, $84, $42, $40, $3C, $84, $42, $84, $42, $40, $3C ; 8 $28
	db $00, $00, $40, $3C, $84, $42, $84, $42, $40, $3C, $04, $02, $04, $02, $40, $3C ; 9 $29
TilesEnd:

ObjectTiles:
	db $E0, $E0, $FF, $FF, $FF, $F4, $FF, $F4, $7F, $7F, $3F, $3F, $1F, $1F, $00, $00 ; back of plane
	db $00, $00, $FF, $FF, $FF, $92, $FF, $92, $FF, $FF, $FF, $FF, $FF, $FF, $7E, $7E ; middle of plane
	db $00, $00, $FE, $FE, $FF, $51, $FF, $59, $FF, $FD, $FF, $FF, $FE, $FE, $00, $00 ; front of plane
ObjectTilesEnd:

SECTION "Tilemap data", ROM0

Tilemap:
	db $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E,  $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
	db $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F,  $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D,  $1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D,$1D
	db $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  $1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C
TilemapEnd:

TileMapTitle:
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
TileMapTitleEnd:

WindowScore:
	db $13, $03, $0F, $12, $05, $00, $00, $20, $20, $20, $20
WindowScoreEnd:

SECTION "String data", ROM0

StringVar:
	.Game_Over db "game over", 255
	.PigeonString db "fuck pigeon", 255
	.PressString db "press a", 255


SECTION "Input Variables", WRAM0
wCurKeys: db
wNewKeys: db

SECTION "Counter", WRAM0
wFrameCounter: db
wTimer: dw
wRandom: db
wBool: db
wGameRunning:db
