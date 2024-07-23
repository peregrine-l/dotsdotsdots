                    INCDIR  "include"
                    INCLUDE "hardware.i"
                    INCLUDE "system.i"
                    INCLUDE "dos/dosextens.i"

;;;;;;;;;;;;;;;;;;; BEGIN SYMBOLIC CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BPWidth             EQU     320
BPHeight            EQU     256
Depth               EQU     1
BPSize              EQU     (BPWidth*BPHeight/8)
PlayfieldSize       EQU     (BPSize*Depth)
WWidth              EQU     320
WHeight             EQU     256
HOffset             EQU     ((BPWidth-WWidth)/2)
VOffset             EQU     ((BPHeight-WHeight)/2)
XStart              EQU     $81
XStop               EQU     (XStart+BPWidth)
YStart              EQU     $2c
YStop               EQU     (YStart+BPHeight)
OurDIWSTRT          EQU     ((YStart<<8)|XStart)
OurDIWSTOP          EQU     (((YStop-256)<<8)|(XStop-256))
OurDDFSTRT          EQU     $38
OurDDFSTOP          EQU     $d0
VBFlag              EQU     0
FPFractional        EQU     8
FPSize              EQU     16
EyeDepth            EQU     64 ;absolute value, real value = -64
MaxDepth            EQU     512
Radius              EQU     128
;;;;;;;;;;;;;;;;;;; END SYMBOLIC CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RESTORE_CTRL_REGS   MACRO ;RESTORE_CTRL_REGS <hardware control register>
                    move.w  #$7fff,\1(a5)                  ;disable current reg
                    move.w  Old\1,d0
                    ori.w   #$8000,d0                      ;enable restored reg
                    move.w  d0,\1(a5)
                    ENDM
CWAITD7             MACRO ;CWAITD7 <#h>,<>,<aX CL ptr> [4 bytes]
                    lsl.l   #8,d7
                    ori.w   #\1,d7
                    ori.w   #1,d7
                    move.w  d7,(\3)+
                    move.w  #$fffe,(\3)+
                    ENDM
CWAITI              MACRO ;CWAITI <#h>,<#val>,<aX CL ptr> [4 bytes]
                    move.l  #\2,d7
                    CWAITD7 \1,\2,\3
                    ENDM
CMOVEI              MACRO ;CMOVEI <#value>,<#register>,<aX CL ptr> [4 bytes]
                    move.w  #\2,(\3)+
                    move.w  #\1,(\3)+
                    ENDM
CMOVEP              MACRO ;CMOVEPTR <ptr>,<#reg name+num>,<aX CL ptr> [8 bytes]
                    move.l  \1,d7
                    move.w  #\2PTL,(\3)+
                    move.w  d7,(\3)+
                    swap    d7
                    move.w  #\2PTH,(\3)+
                    move.w  d7,(\3)+
                    ENDM
;;;;;;;;;;;;;;;;;;; END MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN MAIN ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    SECTION main.code,CODE_P
                    CNOP    0,4
Start:              movem.l d1-d7/a0-a6,-(sp)
Setup:              bsr.w   SystemSetup
                    bsr.w   Init
.mainLoop:          btst.b  #VBFlag,Flags
                    beq.s   .mainLoop
                    ;animation mode
                    move.l  DrawPF,a0
                    jsr     ClearBitplane
                    btst    #14,DMACONR(a5)         ;waitfor the Blitter, twice
.waitBlitter        btst    #14,DMACONR(a5)
                    bne.s   .waitBlitter
                    move.l  FrameCount,d0
                    andi.l  #1024-1,d0                          ;angle mod 2*PI
                    jsr     RotateX
                    jsr     Draw3DDots
                    jsr     SwapBuffers
                    bclr.b  #VBFlag,Flags
                    btst	#10,POTINP+CUSTOM               ;right mouse button
                    bne.s   .mainLoop
Cleanup:            bsr     SystemCleanup
Exit:               movem.l (sp)+,d1-d7/a0-a6
                    move.b  ErrNo,d0
                    rts
;;;;;;;;;;;;;;;;;;; END MAIN ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN DOUBLE BUFFERING ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;
SwapBuffers:        move.l  ViewPF,d0
                    move.l  DrawPF,d1
                    move.l  d0,DrawPF
                    move.l  d1,ViewPF
                    move.l  #Copperlist,a0
                    CMOVEP  d1,BPL1,a0
                    rts
;;;;;;;;;;;;;;;;;;; END DOUBLE BUFFERING ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN SYSTEM SETUP ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SystemSetup:        ;TODO: Workbench icon startup
                    ;TODO: input device sink
TaskPriority:       suba.l  a1,a1
                    CALLSYS SysBase,FindTask
                    move.l  d0,a1
                    move.l  d0,a5                                ;for next part
                    moveq   #127,d0
                    JUMPSYS SetTaskPri
CallEnvironment:	tst.l	pr_CLI(a5)	       ;pointer to calling CLI, or NULL
					bne.s	SetupIntServer
					lea		pr_MsgPort(a5),a0	 ;waiting for the WBStartup msg
					JUMPSYS	WaitPort
					lea		pr_MsgPort(a5),a0
					JUMPSYS	GetMsg
					move.l	d0,WBMessage
SetupIntServer:     moveq   #5,d0                         ;VERTB interrupt line
                    lea     VBlankIntStruct,a1
                    CALLSYS SysBase,AddIntServer      ;can we check for errors?
OpenGfxLibrary:     lea     GfxLibName,a1
                    moveq   #33,d0                              ;Kickstart 1.2+
                    JUMPSYS OpenLibrary
                    move.l  d0,GfxLibBase
                    beq.s   .error
                    bra.s   PALTest
.error              move.l  #11,ErrNo                  
                    move.l  #Exit,(sp)    ;bra to SystemCleanup and rts to Exit
                    bra.w   OpenGfxLibError
PALTest             cmpi.b  #50,VBlankFrequency(a6)               ;a6 = SysBase
                    bne.s   .error
                    bra.s   FlushView
.error              move.l  #12,ErrNo
                    move.l  #Exit,(sp)
                    bra.w   PALError
FlushView           move.l  GfxLibBase,a6
                    move.l  gb_ActiView(a6),OldView
                    move.l  gb_LOFlist(A6),OldCopperlist
.loop               suba.l  a1,a1
                    JUMPSYS LoadView
                    JUMPSYS WaitTOF
                    tst.l   gb_ActiView(a6)
                    bne.s   .loop
TakeBlitter:        lea     CUSTOM,a5
                    JUMPSYS OwnBlitter
                    clr.w   BLTCON0(a5)
                    clr.w   BLTCON1(a5)
SaveHWCtrlRegs:     move.w  DMACONR(a5),OldDMACON
                    move.w  INTENAR(a5),OldINTENA
                    move.w  INTREQR(a5),OldINTREQ
                    move.w  ADKCONR(a5),OldADKCON
                    rts
;;;;;;;;;;;;;;;;;;; END SYSTEM SETUP ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN SYSTEM CLEANUP ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SystemCleanup:      lea     CUSTOM,a5
RestoreHWCtrlRegs:  RESTORE_CTRL_REGS DMACON
                    RESTORE_CTRL_REGS INTENA
                    RESTORE_CTRL_REGS INTREQ
                    RESTORE_CTRL_REGS ADKCON
FreeBlitter:        CALLSYS GfxLibBase,WaitBlit
                    JUMPSYS DisownBlitter
RestoreView:        move.l  OldView,a1
                    move.l  OldCopperlist,gb_LOFlist(a6)
                    move.l  gb_copinit(a6),COP1LC(a5)
                    move.w  #0,COPJMP1(a5)
                    JUMPSYS LoadView
PALError:
CloseGfxLibrary:    move.l  GfxLibBase,a1
                    CALLSYS SysBase,CloseLibrary
OpenGfxLibError:    
RemoveIntServer:    moveq   #5,d0
                    lea     VBlankIntStruct,a1
                    CALLSYS SysBase,RemIntServer
CalledFromWB:       tst.l   WBMessage
                    beq.s   .exit
                    JUMPSYS Forbid
                    move.l  WBMessage,a1
                    JUMPSYS ReplyMsg                 ;automatically restarts OS
.exit               rts
;;;;;;;;;;;;;;;;;;; END SYSTEM CLEANUP ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN INITIALIZATION ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Init:               lea     CUSTOM,a5                       ;maybe useless here
                    move.l  #PlayfieldA,DrawPF
                    move.l  #PlayfieldB,ViewPF
Colors:             move.w  #$000,COLOR00(a5)
                    move.w  #$fff,COLOR01(a5)
                    jsr     CalcMulTables
                    jsr     Copy3DObject
MakeCopperlist:     move.l  #Copperlist,a0
                    move.l  ViewPF,a1
                    CMOVEP  a1,BPL1,a0                ;one bitplane for now
                    CWAITI  $07,YStart,a0
                    CMOVEI  $000,COLOR00,a0          
                    CWAITI  $ff,$ff,a0                ;end of copperlist, twice
                    CWAITI  $ff,$ff,a0
SysConfig:          move.w  #(CUSTOMCLR|COPEN|BPLEN|BLTEN|SPREN),DMACON(a5)
                    move.w  #(BPU0|COLOR),BPLCON0(a5)
					move.w	#0,BPLCON3(a5)		                    ;no AGA/ECS
					move.w	#0,FMODE(a5)                            ;ditto
					move.w	#0,BPL1MOD(a5)
					move.w	#0,BPL2MOD(a5)
                    move.w	#OurDIWSTRT,DIWSTRT(a5)
					move.w	#OurDIWSTOP,DIWSTOP(a5)
					move.w	#OurDDFSTRT,DDFSTRT(a5)
					move.w	#OurDDFSTOP,DDFSTOP(a5)
                    move.w	#(CUSTOMSET|DMAEN|COPEN|BPLEN|BLTEN),DMACON(a5)
					move.w	#(CUSTOMSET|INTEN|VERTB),INTENA(a5)
                    lea     PlayfieldA,a0
                    bsr.s   ClearBitplane
                    lea     PlayfieldB,a0
                    bsr.s   ClearBitplane
                    CALLSYS GfxLibBase,WaitTOF
                    JUMPSYS WaitTOF  
                    move.l	#Copperlist,COP1LC(a5)
					move.l	#0,COPJMP1(a5)
                    rts
;;;;;;;;;;;;;;;;;;; BEGIN INITIALIZATION ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN CLEAR BITPLANE ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ClearBitplane:      ;a0: bitplane address
                    lea     CUSTOM,a5
                    btst    #14,DMACONR(a5)        ;wait for the Blitter, twice
.waitBlitter        btst    #14,DMACONR(a5)
                    bne.s   .waitBlitter
                    move.l  #USED<<16,BLTCON0(a5)  ;clear mode
                    clr.w   BLTDMOD(a5)
                    move.l  a0,BLTDPTH(a5)
                    move.w  #((BPHeight<<6)|(BPWidth/16)),BLTSIZE(a5) ;in words
                    rts
;;;;;;;;;;;;;;;;;;; END CLEAR BITPLANE ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN GEOMETRY ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CosineSine:         move.w  d0,d1        ;input: angle in d0 (0..1023)(0..2*PI)
                    addi.w  #256,d0      ;+ PI/2
                    andi.w  #1023,d0     ;modulo 2*PI
                    add.w   d0,d0
                    move.w  (a2,d0.w),d0 ;output: cosine in d0 [Q2.14]
                    add.w   d1,d1
                    move.w  (a2,d1.w),d1 ;output: sine in d1 [Q2.14]
                    rts

Copy3DObject:       lea     Cube,a0        ;a0 = source, object definition
                    lea     Transform,a1   ;a1 = destination, object transforms
                    move.w  #(8-1)*3*2,d0  ;d0 = n points
.loop               move.w  (a0,d0.w),(a1,d0.w)
                    move.w  2(a0,d0.w),2(a1,d0.w)
                    move.w  4(a0,d0.w),4(a1,d0.w)
                    subi.w  #6,d0
                    bpl.s   .loop
                    rts

RotateX:            lea     Cube,a0
                    lea     Transform,a1
                    lea     SineTable,a2
                    ;angle in d0
                    bsr.s   CosineSine             ;d0 = cos, d1 = sin
                    moveq   #14,d2                 ;fixed point factor
                    move.w  #(8-1)*3*2,d7          ;number of points = 8
.loop               move.w  (a0,d7.w),d4           ;d4 = Z
                    move.w  2(a0,d7.w),d3          ;d3 = Y (X is invariant)
                    move.w  d4,d6                  ;save Z
                    move.w  d3,d5                  ;save Y
                    muls    d0,d4                  ;d3 = Z * cos
                    muls    d1,d3                  ;d4 = Y * sin
                    add.l   d4,d3                  ;d3 = Z * cos + Y * sin
                    asr.l   d2,d3                  ;reverse fixed point factor
                    move.w  d3,(a1,d7.w)           ;new Z
                    muls    d0,d5                  ;d5 = Y * cos
                    muls    d1,d6                  ;d6 = Z * sin
                    sub.l   d6,d5                  ;d5 = Y * cos - Z * sin
                    asr.l   d2,d5                  ;reverse fixed point factor
                    move.w  d5,2(a1,d7.w)          ;new Y
                    subi.w  #6,d7
                    bpl.w   .loop
                    rts
;;;;;;;;;;;;;;;;;;; END GEOMETRY ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN DOTS ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalcMulTables:      lea     PlotDotTable,a0
                    move.l  #((VOffset*BPWidth+HOffset)/8),d0
                    move.l  #(BPHeight-1),d7
.loopPlotDot        move.w  d0,(a0)+
                    addi.w  #(BPWidth/8),d0            ;so, it's a x40 table
                    dbf     d7,.loopPlotDot

                    lea     ProjectTable,a0
                    move.l  #(EyeDepth)<<15,d0
                    move.w  #(512-1),d7              ;0 < i < 511
.loopProject        move.l  d0,d1                      ;numerator
                    move.w  d7,d2                      
                    addi.w  #64,d2                   ;denominator
                    divu    d2,d1                      ;64<<15/(z+64)
                    andi.l  #$0000ffff,d1
                    move.w  d7,d3                      ;double i
                    add.w   d3,d3
                    move.w  d1,(a0,d3)                 ;quotient 2.14 to word
                    dbf     d7,.loopProject                
                    rts

PlotDot:            ;d0: X, d1: Y, a0: multiplication table, a1: bitplane
                    move.l  d0,d2
                    lsr.w   #3,d0        ;d0 <- X/8, offset byte
                    add.w   d1,d1        ;68000 doesn't support (a0,d1.w*2)
                    add.w   (a0,d1.w),d0 ;d0 <- X/8 + (Width/8) * Y + 
                                         ; ((VOffset * Width + HOffset)/8)
                    not.b   d2           ;endianness complement to start
                                         ;from low address
                    bset.b  d2,(a1,d0)
                    rts              ;Gloky suggests jmp(aX) instead of jsr/rts

DrawCircle:         lea     PlotDotTable,a0
                    move.l  DrawPF,a1
                    lea     SineTable,a2
                    moveq   #14,d3 ;PlotDot uses d2
                    move.w  #1024-1,d7
.loop               move.w  d7,d0
                    jsr     CosineSine
                    muls    #120,d0
                    asr.l   d3,d0
                    addi.w  #160,d0
                    muls    #120,d1
                    asr.l   d3,d1
                    addi.w  #128,d1
                    jsr     PlotDot
                    dbf     d7,.loop
                    rts

Draw3DDots:         lea     PlotDotTable,a0
                    move.l  DrawPF,a1
                    lea     ProjectTable,a2
                    lea     Transform,a3           ;a3 = 3D dots table
                    move.w  #(8-1)*3*2,d7          ;number of points = 8
                    ;moveq   #0,d1
.loop               move.w  (a3,d7.w),d2           ;Z
                    addi.w  #(MaxDepth/2),d2
                    move.w  2(a3,d7.w),d1          ;Y
                    move.w  4(a3,d7.w),d0          ;X
                    
                    add.w   d2,d2
                    move.w  (a2,d2.w),d2           ;64<<15/(64+Z) [Q1.15]
                    muls    d2,d0                  ;64<<15/(64+Z) * X
                    muls    d2,d1                  ;64<<15/(64+Z) * Y
                    moveq	#15,d2                 ;reverse fixed point factor
                    asr.l	d2,d0
                    asr.l   d2,d1
                    addi.w  #(WWidth/2),d0         ;64/(64+Z) * X + 160
                    addi.w  #(WHeight/2),d1        ;64/(64+Z) * Y + 128
                    jsr     PlotDot
                    subi.w  #6,d7
                    bpl.w   .loop
                    rts
;;;;;;;;;;;;;;;;;;; END DOTS ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN INTERRUPT HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    SECTION main.interrupts,CODE_P
                    CNOP    0,4
VBlankIntHandler:   ;movem.l d1-d7/a2-a6,-(sp)      ;sets a1 and returns a0, d0
                    add.l   #1,(a1)                               ;FrameCount++
                    bset.b  #VBFlag,4(a1)              ;set VBFlag bit of Flags
                    lea     CUSTOM,a0
                    move.w  #$cc00,POTGO(a0)  ;reconfigure right button each VB
                    move.w  #VERTB,INTREQ(a0)             ;clear interrupt flag
                    move.w  #VERTB,INTREQ(a0)         ;twice for compatibility?
                    ;movem.l (sp)+,d1-d7/a2-a6
                    moveq   #0,d0
                    rts
;;;;;;;;;;;;;;;;;;; END INTERRUPT HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN STATIC DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    SECTION main.data,DATA_P
                    CNOP    0,4
                    ;byte-length (strings)
                    dc.b    "$VER: dots 0.1",0             ;for Version command
VBlankIntName       dc.b    "main.interrupts.vblank",0
GfxLibName          dc.b    "graphics.library",0
ErrNo               dc.b    0
                    ;word-length
                    CNOP    0,2
OldDMACON           ds.l    1
OldINTENA           ds.l    1
OldINTREQ           ds.l    1
OldADKCON           ds.l    1
PlotDotTable        ds.w    (2*BPHeight)
SineTable           include "sinetable.i"
ProjectTable        ds.w    2*MaxDepth
Cube                dc.w    -Radius, -Radius, -Radius
                    dc.w    -Radius, -Radius,  Radius
                    dc.w    -Radius,  Radius, -Radius
                    dc.w    -Radius,  Radius,  Radius
                    dc.w     Radius, -Radius, -Radius
                    dc.w     Radius, -Radius,  Radius
                    dc.w     Radius,  Radius, -Radius
                    dc.w     Radius,  Radius,  Radius
Transform           ds.w    3*8
                    ;long-length
                    CNOP    0,4
WBMessage           dc.l    0
GfxLibBase          ds.l    1
OldView             ds.l    1
OldCopperlist       ds.l    1
ViewPF              ds.l    1
DrawPF              ds.l    1
                    ;mixed-length (structs)
                    CNOP    0,4
VBlankIntData       ;data shared with interrupt handler
FrameCount          dc.l    0
Flags               dc.b    0
                    ;AmigaOS' interrupt structure
                    CNOP    0,4
VBlankIntStruct     dc.l    0,0         ;LN_SUCC,LN_PRED, part of a linked list
                    dc.b    2                       ;LN_TYPE = NT_INTERRUPT = 2
                    dc.b    127                              ;LN_PRI to highest
                    dc.l    VBlankIntName
                    dc.l    VBlankIntData
                    dc.l    VBlankIntHandler

                    SECTION main.chipdata,DATA_C
                    CNOP    0,4
Copperlist          ds.b    128                       ;should be enough for now
PlayfieldA          ds.b    PlayfieldSize
PlayfieldB          ds.b    PlayfieldSize
;;;;;;;;;;;;;;;;;;; END STATIC DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;