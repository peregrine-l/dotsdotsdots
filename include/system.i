; Amiga operating system calls and helpers

	INCLUDE		"LVOs.i"

;;;;;;;;;;;;;;;;;;;;;;; BEGIN Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; exec.library
;SysBase				    EQU	4	           ;pointer to exec.library
MEMF_CHIP				EQU	1<<1	       ;AllocMem(): mask for Chip RAM
MEMF_ANY				EQU	0	           ;AllocMem(): mask for data
MEMF_CLEAR				EQU	1<<16	       ;AllocMem(): mask for init w/ 0s
VBlankFrequency			EQU	530	           ;50 if PAL, 60 if NTSC
PowerSupplyFrequency	EQU	531


; graphics.library
GFXF_AA_ALICE			EQU	2	           ;bit set if AGA
gb_ChipRevBits0			EQU	$ec	           ;chip revision
gb_ActiView				EQU	34	           ;offset to ptr to current viewport
gb_copinit				EQU	38	           ;offset to ptr to current copperlist
;;;;;;;;;;;;;;;;;;;;;;; END Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; BEGIN Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CALLSYS         MACRO          ;CALLSYS <library base>,<library routine offset>
; 	movea.l		\1,a6
; 	jsr			_LVO\2(a6)
; 	ENDM

                ;use JUMPSYS if the library base is already in a6
JUMPSYS         MACRO                         ;JUMPSYS <library routine offset>
	jsr			_LVO\1(a6)
	ENDM
;;;;;;;;;;;;;;; END Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
