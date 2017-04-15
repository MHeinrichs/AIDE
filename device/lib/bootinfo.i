BLOCKSIZE   EQU   $200
   include "exec/memory.i"
MAXLENGTH   EQU  31
MEM_BASE    EQU  $600000
MEM_SIZE    EQU  $400000
MEM_FLAGS   EQU  MEMF_PUBLIC+MEMF_FAST+MEMF_24BITDMA
MEM_PRIO    EQU  10

MAX_BLOCK_SEARCH_RDB equ 16

BD_VERSION equ 1
BD_REVISION equ 0

    STRUCTURE MyParmPkt,0
   APTR     pp_dosName
   APTR     pp_execName
   ULONG    pp_unitnumber
   ULONG    pp_flags
   ULONG    pp_paramSize
   ULONG    pp_blockSize
   ULONG    pp_sectorOrigin
   ULONG    pp_surfaces
   ULONG    pp_sectorsPerBlock
   ULONG    pp_blocksPerTrack
   ULONG    pp_reservedBlocks
   ULONG    pp_preface
   ULONG    pp_interleave
   ULONG    pp_lowCyl
   ULONG    pp_highCyl
   ULONG    pp_numBuffer
   ULONG    pp_buffermemType
   ULONG    pp_maxTransfer
   ULONG    pp_mask
   LONG     pp_bootPrio
   ULONG    pp_dosType
   LABEL    MyParmPkt_Sizeof

    STRUCTURE BootDevice,0
   ULONG    bd_residentstructure
   ULONG    bd_buffermem
   ULONG    bd_rdbmem
   ULONG    bd_parametermem
   ULONG    bd_configdev
   ULONG    bd_expansionlib
   ULONG    bd_devicenode
   ULONG    bd_iohandler
   ULONG    bd_unitptr
   ULONG    bd_unitnum
   ULONG    bd_ATARdWtRoutine
   ULONG    bd_ioRequest
   LABEL    BootDevice_Sizeof


