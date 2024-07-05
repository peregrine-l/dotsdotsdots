                    INCDIR  "include"
                    INCLUDE "hardware.i"
                    INCLUDE "system.i"
                    INCLUDe "dos/dosextens.i"

;;;;;;;;;;;;;;;;;;; BEGIN SYMBOLIC CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VBFlag              EQU     0
;;;;;;;;;;;;;;;;;;; END SYMBOLIC CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RESTORE_CTRL_REGS   MACRO ;RESTORE_CTRL_REGS <hardware control register>
                    move.w  #$7fff,\1(a5)                  ;disable current reg
                    move.w  Old\1,d0
                    ori.w   #$8000,d0                      ;enable restored reg
                    move.w  d0,\1(a5)
                    ENDM
;;;;;;;;;;;;;;;;;;; END MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN MAIN ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    SECTION main.code,CODE_P
                    CNOP    0,4
Start:              movem.l d1-d7/a0-a6,-(sp)
Setup:              bsr     SystemSetup
.mainLoop:          btst.b  #VBFlag,Flags
                    beq.s   .mainLoop
                    ;do stuff here
                    bclr.b  #VBFlag,Flags
                    btst.b  #FIR0,CIAA+CIAAPRA               ;left mouse button
                    bne.s   .mainLoop
Cleanup:            bsr     SystemCleanup
Exit:               movem.l (sp)+,d1-d7/a0-a6
                    move.b  ErrNo,d0
                    rts
;;;;;;;;;;;;;;;;;;; END MAIN ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                    JUMPSYS LoadView
                    JUMPSYS WaitTOF
                    JUMPSYS WaitTOF
                    move.l  gb_copinit(a6),COP1LC(a5)
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

;;;;;;;;;;;;;;;;;;; BEGIN INTERRUPT HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    SECTION main.interrupts,CODE_P
                    CNOP    0,4
VBlankIntHandler:   ;movem.l d1-d7/a2-a6,-(sp)      ;sets a1 and returns a0, d0
                    add.l   #1,(a1)                               ;FrameCount++
                    bset.b  #VBFlag,4(a1)              ;set VBFlag bit of Flags
                    lea     CUSTOM,a0
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
                    ;long-length
                    CNOP    0,4
WBMessage           dc.l    0
GfxLibBase          ds.l    1
OldView             ds.l    1
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
;;;;;;;;;;;;;;;;;;; END STATIC DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;