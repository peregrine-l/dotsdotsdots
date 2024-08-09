                    INCDIR  "include"
                    INCLUDE "hardware.i"
                    INCLUDE "system.i"
                    INCLUDE "dos/dosextens.i"

;;;;;;;;;;;;;;;;;;; BEGIN SYMBOLIC CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BPWidth             EQU     320
BPHeight            EQU     256
Depth               EQU     3
WWidth              EQU     (BPWidth-80)
WHeight             EQU     (BPHeight-40)
HOffset             EQU     80
VOffset             EQU     0

BPSize              EQU     (BPWidth*BPHeight/8)
PFSize              EQU     (BPSize*Depth)
XStart              EQU     $81
XStop               EQU     (XStart+BPWidth)
YStart              EQU     $2c
YStop               EQU     (YStart+BPHeight)
OurDIWSTRT          EQU     ((YStart<<8)|XStart)
OurDIWSTOP          EQU     (((YStop-256)<<8)|(XStop-256))
OurDDFSTRT          EQU     $38
OurDDFSTOP          EQU     $d0
VBFlag              EQU     0

QuantizedTau        EQU     1024  ;degrees in 2*Pi radians
MaxDepth            EQU     512   ;world depth from screen
N                   EQU     256   ;number of dots
Radius              EQU     152   ;radius of sphere
SineLUTFactor       EQU     14    ;scale of sine table
ProjLUTFactor       EQU     15    ;scale of 3D projection table
LerpLUTFactor       EQU     15    ;scale of interpolation table
Sqrt22              EQU     $5a82 ;sqrt(2)/2 << 15
Sqrt22Factor        EQU     15    ;scale of sqrt(2)/2
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
CMOVEA              MACRO ;CMOVEA <aX: array ptr>,<aY: ptr to register>,<aZ CL>
                    move.w	\2,(\3)+
					move.w	(\1)+,(\3)+
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
                    bsr.w   DemoInit
Mainloop:           btst.b  #VBFlag,Flags
                    beq.s   Mainloop
                    move.l  BlitPF,a0
                    bsr     ClearDots
                    bsr     TransformDots
                    bsr     RollBuffers
                    bclr.b  #VBFlag,Flags
                    btst	#10,POTINP+CUSTOM               ;right mouse button
                    bne.s   Mainloop 
Cleanup:            bsr     DemoCleanup
                    bsr     SystemCleanup
Exit:               movem.l (sp)+,d1-d7/a0-a6
                    move.b  ErrNo,d0
                    rts
;;;;;;;;;;;;;;;;;;; END MAIN ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN DOUBLE BUFFERING ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;
RollBuffers:        move.l  BlitPF,a0
                    move.l  DrawPF,a1
                    move.l  ViewPF,a2
                    move.l  a0,DrawPF
                    move.l  a1,ViewPF
                    move.l  a2,BlitPF
                    move.l  #Copperlist,a0
                    CMOVEP  a1,BPL1,a0
                    add.l   #BPSize,a1
                    CMOVEP  a1,BPL2,a0
                    add.l   #BPSize,a1
                    CMOVEP  a1,BPL3,a0
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
DemoInit:           bsr     CalcPlotDotLUT
                    bsr     GenerateSphere
                    bsr     GenerateCube
AllocPlayfields:    move.l  #(MEMF_CHIP|MEMF_CLEAR),d1
                    move.l  #PFSize,d0
                    CALLSYS SysBase,AllocMem
                    move.l  d0,BlitPF
                    move.l  d0,a0
                    bsr     BlitLogo
                    move.l  #(MEMF_CHIP|MEMF_CLEAR),d1 ;AllocMem trashes d1
                    move.l  #PFSize,d0
                    JUMPSYS AllocMem
                    move.l  d0,DrawPF
                    move.l  d0,a0
                    bsr     BlitLogo
                    move.l  #(MEMF_CHIP|MEMF_CLEAR),d1
                    move.l  #PFSize,d0
                    JUMPSYS AllocMem
                    move.l  d0,ViewPF
                    move.l  d0,a0
                    bsr     BlitLogo

MakeCopperlist:     move.l  #Copperlist,a0
                    move.l  ViewPF,a1
                    CMOVEP  a1,BPL1,a0                ;3 bitplanes
                    add.l   #BPSize,a1
                    CMOVEP  a1,BPL2,a0
                    add.l   #BPSize,a1
                    CMOVEP  a1,BPL3,a0
                    CWAITI	$07,YStart,a0
                    lea     Palette,a1
                    lea     COLOR00,a2
                    moveq   #(8-1),d7
.loop               CMOVEA  a1,a2,a0
                    addq    #2,a2
                    dbf     d7,.loop
                    CWAITI	$df,$ff,a0          
                    CWAITI  $ff,$ff,a0                ;end of copperlist, twice
                    CWAITI  $ff,$ff,a0

SysConfig:          move.w  #(CUSTOMCLR|COPEN|BPLEN|BLTEN|SPREN),DMACON(a5)
                    move.w  #(BPU1|BPU0|COLOR),BPLCON0(a5)          ;3 bitplanes
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
                    CALLSYS GfxLibBase,WaitTOF
                    JUMPSYS WaitTOF 
                    move.l	#Copperlist,COP1LC(a5)
					move.l	#0,COPJMP1(a5)
                    ;Init LSP and start replay using easy CIA toolbox
			        lea		MusicData,a0
			        lea		SoundBank,a1
			        suba.l	a2,a2			;suppose VBR=0 (A500)
			        moveq	#0,d0			;suppose PAL machine
			        bsr		LSP_MusicDriver_CIA_Start
			        move.w	#$e000,$dff09a
                    rts
;;;;;;;;;;;;;;;;;;; BEGIN INITIALIZATION ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN CLEANUP ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DemoCleanup:        bsr     LSP_MusicDriver_CIA_Stop
                    lea     CUSTOM,a5
                    move.w  #(CUSTOMCLR|COPEN|BPLEN),DMACON(a5)
                    move.l  BlitPF,a1
                    move.w  #PFSize,d0
                    CALLSYS SysBase,FreeMem
                    move.l  DrawPF,a1
                    move.w  #PFSize,d0
                    JUMPSYS FreeMem
                    move.l  ViewPF,a1
                    move.w  #PFSize,d0
                    JUMPSYS FreeMem
                    rts
;;;;;;;;;;;;;;;;;;; END CLEANUP ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN BITPLANE PREP ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BlitLogo:           lea		CUSTOM,a5
					btst	#14,DMACONR(a5)					 ;wait for the blit
.waitblitA			btst	#14,DMACONR(a5)
					bne.s	.waitblitA
					move.w	#$ffff,BLTAFWM(a5)				       ;clear masks
					move.w	#$ffff,BLTALWM(a5)
					move.l	#(USEA|USED|$f0)<<16,BLTCON0(a5) ;A -> D, copy mode
					clr.w	BLTAMOD(a5)            ;copy everything from source
					move.w	#((BPWidth-80)/8),BLTDMOD(a5)			
                    lea     Logo,a1                              
                    moveq   #(Depth-1),d7
.loop				btst	#14,DMACONR(a5)	 ;TODO: put wait in the end of loop
.waitblitB			btst	#14,DMACONR(a5)
					bne.s	.waitblitB
					move.l	a1,BLTAPTH(a5)                              ;source
					move.l	a0,BLTDPTH(a5)                         ;destination						
					move.w	#((215<<6)|(80/16)),BLTSIZE(a5)		
					add.l	#(215*(80/8)),a1 ;height (in px) * width (in bytes)
					add.l	#BPSize,a0										
					dbf		d7,.loop

ClearDots:          ;a0: bitplane address
                    lea     CUSTOM,a5
                    btst    #14,DMACONR(a5)        ;wait for the Blitter, twice
.waitBlitter        btst    #14,DMACONR(a5)
                    bne.s   .waitBlitter
                    move.l  #USED<<16,BLTCON0(a5)  ;clear mode
                    move.w  #((BPWidth-WWidth)/8),BLTDMOD(a5)
                    add.l   #(HOffset/8),a0
                    move.l  a0,BLTDPTH(a5)
                    move.w  #((WHeight<<6)|(WWidth/16)),BLTSIZE(a5) ;in words
                    rts
;;;;;;;;;;;;;;;;;;; END BITPLANE PREP ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN DOTS ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalcPlotDotLUT:     lea     PlotDotLUT,a0
                    move.l  #((VOffset*BPWidth+HOffset)/8),d0
                    move.l  #(BPHeight-1),d7
.loopPlotDot        move.w  d0,(a0)+
                    addi.w  #(BPWidth/8),d0            ;so, it's a x40 table
                    dbf     d7,.loopPlotDot

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

GenerateSphere:     lea     InclineLUT,a0
                    lea     AzimuthLUT,a1
                    lea     SineLUT,a2
                    lea     Sphere,a3
                    moveq   #SineLUTFactor,d6
                    move.w  #2*(N-1),d7          
.loop               move.w  (a0,d7.w),d0         ;incline
                    move.w  d0,d1                ;save incline
                    add.w   d0,d0
                    move.w  (a2,d0.w),d0         ;sin(incline)
                    addi.w  #(QuantizedTau/4),d1 ;incline + pi/2
                    andi.w  #(QuantizedTau-1),d1 ;modulo 2*Pi
                    add.w   d1,d1
                    move.w  (a2,d1.w),d1         ;cos(incline)
                    move.w  (a1,d7.w),d2         ;azimuth
                    move.w  d2,d3                ;save azimuth
                    add.w   d2,d2
                    move.w  (a2,d2.w),d2         ;sin(azimuth)
                    addi.w  #(QuantizedTau/4),d3 ;azimuth + pi/2
                    andi.w  #(QuantizedTau-1),d3 ;modulo 2*Pi
                    add.w   d3,d3
                    move.w  (a2,d3.w),d3         ;cos(azimuth)
                    move.w  #Radius,d4
                    muls    d1,d4                ;r * cos(incline)
                    asr.l   d6,d4                ;scaled
                    move.w  d4,(a3)+             ;s.z
                    move.w  #Radius,d4
                    muls    d0,d4                ;r * sin(incline)
                    asr.l   d6,d4                ;scaled
                    muls    d3,d4                ;* cos(azimuth)
                    asr.l   d6,d4                ;scaled
                    move.w  d4,(a3)+             ;s.x
                    move.w  #Radius,d4
                    muls    d0,d4                ;r * sin(incline)
                    asr.l   d6,d4                ;scaled
                    muls    d2,d4                ;* sin(azimuth)
                    asr.l   d6,d4                ;scaled
                    move.w  d4,(a3)+             ;s.y
                    subi.w  #2,d7                ;i -= sizeof(word)
                    bpl     .loop
                    rts

GenerateCube:       move.w  #Radius,d5
                    move.w  #Sqrt22Factor,d6
                    mulu    #Sqrt22,d5       ;r * sqrt(2)/2
                    asr.l   d6,d5            ;scaled
                    lea     Sphere,a0
                    lea     Cube,a1
                    move.w  #(N-1),d7
.loop               move.w  (a0)+,d2         ;s.z
                    move.w  d2,d3            ;save s.z
                    bpl.s   .sz_positive     
                    neg.w   d3               ;abs(s.z)
.sz_positive        move.w  (a0)+,d0         ;s.x
                    move.w  d0,d1            ;save s.x
                    bpl.s   .sx_positive
                    neg.w   d1               ;abs(s.x)
.sx_positive        move.w  d1,d4
                    sub.w   d3,d4            ;abs(s.x) - abs(s.z)
                    bpl.s   .diff_positive
                    neg.w   d4               ;abs(abs(s.x) - abs(s.z))
.diff_positive      add.w   d1,d4            ;+ abs(s.x)
                    add.w   d3,d4            ;+ abs(s.z)
                    asr.l   #1,d4            ;/2 -> max(abs(s.x), abs(s.z))
                    move.w  (a0)+,d1         ;s.y
                    move.w  d1,d3            ;save s.y
                    bpl.s   .sy_positive
                    neg.w   d3               ;abs(s.y)
.sy_positive        move.w  d3,d6            ;save abs(s.y)
                    sub.w   d4,d6            ;abs(s.y) - max(abs(s.x), abs(s.z))
                    bpl.s   .diff_positive2
                    neg.w   d6               ;abs(previous difference)
.diff_positive2     add.w   d3,d6            ;+ abs(s.y)
                    add.w   d4,d6            ;+ max(abs(s.x), abs(s.z))
                    asr.l   #1,d6            ;/2 ;d6 = max_magnitude
                    muls    d5,d2            ;r * sqrt(2)/2 * s.z
                    divs    d6,d2
                    move.w  d2,(a1)+         ;c.z
                    muls    d5,d0
                    divs    d6,d0
                    move.w  d0,(a1)+         ;c.x
                    muls    d5,d1
                    divs    d6,d1
                    move.w  d1,(a1)+         ;c.y
                    dbf     d7,.loop
                    rts

TransformDots:      
SineCosine:         lea     SineLUT,a0
                    move.l  FrameCount,d5        ;get time
                    move.w  d5,d0                ;for next part
                    asl.l   #1,d5                ;adjusting time
                    andi.w  #(QuantizedTau-1),d5 ;into angle [0, Tau[
                    move.w  d5,d6                ;save angle
                    add.w   d6,d6                ;68000 limitation
                    move.w  (a0,d6.w),d4         ;get sine(angle)
                    addi.w  #(QuantizedTau/4),d5 ;angle + Tau/4
                    andi.w  #(QuantizedTau-1),d5 ;modulo Tau
                    add.w   d5,d5                ;68000 limitation
                    move.w  (a0,d5.w),d5         ;get cos(angle)

InterpolationT:     asr.l   #2,d0                ;adjusting time
                    andi.w  #(QuantizedTau-1),d0 ;modulo Tau
                    add.w   d0,d0
                    lea     LerpLUT,a0          
                    move.w  (a0,d0.w),-(sp)      ;push it on the stack

                    lea     PlotDotLUT,a0
                    move.l  DrawPF,a1
                    lea     ProjLUT,a2
                    lea     Sphere,a3
                    lea     Cube,a4
                    move.w  #(N-1),d7            ;i from 0 to N-1
TransformLoop: 
RotateSphereY:      move.w  (a3)+,d1             ;s.z
                    move.w  (a3)+,d0             ;s.x
                    move.w  d0,d2                ;save s.x
                    move.w  d1,d3                ;save s.z
                    moveq   #SineLUTFactor,d6
                    muls    d5,d2                ;s.x * cosine
                    asr.l   d6,d2                ;scaled
                    muls    d4,d3                ;s.z * sine
                    asr.l   d6,d3                ;scaled
                    add.w   d3,d2                ;sr.x
                    neg.w   d0                   ;-s.x
                    muls    d4,d0                ;-s.x * sine
                    asr.l   d6,d0                ;scaled
                    muls    d5,d1                ;s.z * cosine
                    asr.l   d6,d1                ;scaled
                    add.w   d0,d1                ;sr.z
                    move.w  d2,d0                ;sr.x
ProjectSphere:      addi.w  #(MaxDepth/2),d1     ;z-translation
                    add.w   d1,d1                ;68000 limitation
                    move.w  (a2,d1.w),d2         ;get pf(sr.z)
                    moveq   #ProjLUTFactor,d6
                    muls    d2,d0                ;sr.x * pf
                    asr.l   d6,d0                ;scaled
                    move.w  (a3)+,d1             ;sr.y = s.y
                    muls    d2,d1                ;sr.y * pf
                    asr.l   d6,d1                ;scaled 
                    move.w  d0,-(sp)             ;push sr.x on stack
                    move.w  d1,-(sp)             ;push sr.y on stack
RotateCubeY:        move.w  (a4)+,d1             ;c.z
                    move.w  (a4)+,d0             ;c.x
                    move.w  d0,d2                ;save c.x
                    move.w  d1,d3                ;save c.z
                    moveq   #SineLUTFactor,d6
                    muls    d5,d2                ;c.x * cosine
                    asr.l   d6,d2                ;scaled
                    muls    d4,d3                ;c.z * sine
                    asr.l   d6,d3                ;scaled
                    add.w   d3,d2                ;cr.x
                    neg.w   d0                   ;-c.x
                    muls    d4,d0                ;-c.x * sine
                    asr.l   d6,d0                ;scaled
                    muls    d5,d1                ;c.z * cosine
                    asr.l   d6,d1                ;scaled
                    add.w   d0,d1                ;cr.z
                    move.w  d2,d0                ;cr.x
ProjectCube:        addi.w  #(MaxDepth/2),d1     ;z-translation
                    add.w   d1,d1                ;68000 limitation
                    move.w  (a2,d1.w),d2         ;get pf(cr.z)
                    moveq   #ProjLUTFactor,d6
                    muls    d2,d0                ;cr.x * pf
                    asr.l   d6,d0                ;scaled
                    move.w  (a4)+,d1             ;cr.y = c.y
                    muls    d2,d1                ;cr.y * pf
                    asr.l   d6,d1                ;scaled
Interpolate:        move.w  4(sp),d2             ;peek interpolation factor
                    move.w  #LerpLUTFactor,d6
                    move.w  (sp)+,d3             ;pop sr.y from stack
                    sub.w   d3,d1                ;cr.y - sr.y
                    muls    d2,d1                ;* interpolation factor
                    asr.l   d6,d1                ;scaled
                    add.w   d3,d1                ;+ sr.y
                    move.w  (sp)+,d3             ;pop sr.x from stack
                    sub.w   d3,d0                ;cr.x - sr.x
                    muls    d2,d0                ;* interpolation factor
                    asr.l   d6,d0                ;scaled
                    add.w   d3,d0                ;+ sr.x
PlotDots:           addi.w  #(WWidth/2),d0       ;x-translation
                    addi.w  #(WHeight/2),d1      ;y-translation
                    bsr     PlotDot              ;trashes d2
                    dbf     d7,TransformLoop
                    move.w  (sp)+,d0             ;pop stack
                    rts
;;;;;;;;;;;;;;;;;;; END DOTS ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN INTERRUPT HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    SECTION main.interrupts,CODE_P
                    CNOP    0,4
VBlankIntHandler:   ;movem.l d1-d7/a2-a6,-(sp)      ;sets a1 and returns a0, d0
                    addi.l  #1,(a1)                               ;FrameCount++
                    bset.b  #VBFlag,4(a1)              ;set VBFlag bit of Flags
                    lea     CUSTOM,a0
                    move.w  #$cc00,POTGO(a0)  ;reconfigure right button each VB
                    move.w  #VERTB,INTREQ(a0)             ;clear interrupt flag
                    move.w  #VERTB,INTREQ(a0)         ;twice for compatibility?
                    ;movem.l (sp)+,d1-d7/a2-a6
                    moveq   #0,d0
                    rts
;;;;;;;;;;;;;;;;;;; END INTERRUPT HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN MUSIC PLAYER ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    include "LightSpeedPlayer_cia.asm"
                    include "LightSpeedPlayer.asm"
;;;;;;;;;;;;;;;;;;; END MUSIC PLAYER ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
MusicData           incbin  "dots5.lsmusic"
                    CNOP    0,2
OldDMACON           ds.l    1
OldINTENA           ds.l    1
OldINTREQ           ds.l    1
OldADKCON           ds.l    1
                    ;word-length arrays
Palette             dc.w    $614,$fff,$fae,$a45,$d79,$ff0,$a91,$431
                    ;ZXY order
Sphere              ds.w    N*3
Cube                ds.w    N*3
                    ;word-length LUTs
PlotDotLUT          ds.w    (2*BPHeight)
AzimuthLUT          include "azimuth_lut.i"
InclineLUT          include "incline_lut.i"
SineLUT             include "sine_lut.i"
ProjLUT             include "proj_lut.i"
LerpLUT             include "lerp_lut.i"
                    ;long-length
                    CNOP    0,4
WBMessage           dc.l    0
GfxLibBase          ds.l    1
OldView             ds.l    1
OldCopperlist       ds.l    1
ViewPF              ds.l    1
DrawPF              ds.l    1
BlitPF              ds.l    1
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
Logo                incbin  "logo.raw"
                    CNOP    0,4
SoundBank           incbin  "dots5.lsbank"
                    CNOP    0,4
Copperlist          ds.b    128                       ;should be enough for now
;;;;;;;;;;;;;;;;;;; END STATIC DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;