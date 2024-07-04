                    INCDIR  "include"
                    INCLUDE "hardware.i"
                    ;INCLUDE "system.i"
                    INCLUDE "LVOs.i"

;;;;;;;;;;;;;;;;;;; BEGIN SYMBOLIC CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SysBase				EQU	    4
VBFlag              EQU     0
;;;;;;;;;;;;;;;;;;; END SYMBOLIC CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CALLSYS             MACRO      ;CALLSYS <library base>,<library routine offset>
	                movea.l	\1,a6
	                jsr		_LVO\2(a6)
	                ENDM
JUMPSYS             MACRO                     ;JUMPSYS <library routine offset>
	                jsr		_LVO\1(a6)
	                ENDM
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
MainLoop:           btst.b  #VBFlag,Flags
                    beq.s   MainLoop
                    ;do stuff here
                    bclr.b  #VBFlag,Flags
                    btst.b  #FIR0,CIAA+CIAAPRA               ;left mouse button
                    bne.s   MainLoop
Cleanup:            bsr     SystemCleanup
Exit:               movem.l (sp)+,d1-d7/a0-a6
                    move.b  ErrNo,d0
                    rts
;;;;;;;;;;;;;;;;;;; END MAIN ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN SYSTEM SETUP ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SystemSetup:        moveq   #5,d0 ;VERTB interrupt line
                    lea     VBlankIntStruct,a1
                    CALLSYS SysBase,AddIntServer   ;alternative: SetIntVector()
                    rts
;;;;;;;;;;;;;;;;;;; END SYSTEM SETUP ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN SYSTEM CLEANUP ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SystemCleanup:      moveq   #5,d0
                    lea     VBlankIntStruct,a1
                    CALLSYS SysBase,RemIntServer
                    rts
;;;;;;;;;;;;;;;;;;; END SYSTEM CLEANUP ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN INTERRUPT HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    SECTION main.interrupts,CODE_P
                    CNOP    0,4
VBlankIntHandler:   movem.l d1-d7/a2-a6,-(sp)       ;sets a1 and returns a0, d0
                    add.l   #1,(a1)                               ;FrameCount++
                    bset.b  #VBFlag,4(a1)              ;set VBFlag bit of Flags
                    lea     CUSTOM,a0
                    move.w  #VERTB,INTREQ(a0)             ;clear interrupt flag
                    move.w  #VERTB,INTREQ(a0)         ;twice for compatibility?
                    movem.l (sp)+,d1-d7/a2-a6
                    moveq   #0,d0
                    rts
;;;;;;;;;;;;;;;;;;; END INTERRUPT HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; BEGIN STATIC DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    SECTION main.data,DATA_P
                    CNOP    0,4
                    ;byte-length (strings)
                    dc.b    "$VER: dots 0.1",0             ;for Version command
VBlankIntName       dc.b    "main.interrupts.vblank",0
ErrNo               dc.b    0
                    ;word-length
                    CNOP    0,2
OldINTENA           ds.l    1
OldINTREQ           ds.l    1
OldIntVector        ds.l    1                        ;for SetIntVector approach
                    ;long-length
                    CNOP    0,4
                    ;mixed-length (structs)
                    CNOP    0,4
VBlankIntData       ;data shared with interrupt handler
FrameCount          dc.l    0
Flags               dc.b    0
                    ;AmigaOS' interrupt struct
                    CNOP    0,4
VBlankIntStruct     dc.l    0,0                           ;LN_SUCC,LN_PRED
                    dc.b    2                           ;LN_TYPE = NT_INTERRUPT
                    dc.b    100                         ;LN_PRI to highest
                    dc.l    VBlankIntName
                    dc.l    VBlankIntData
                    dc.l    VBlankIntHandler
;;;;;;;;;;;;;;;;;;; END STATIC DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;