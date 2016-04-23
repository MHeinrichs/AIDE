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
   ULONG    pp_unitNumber
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
   ULONG    pp_BufferMemType
   ULONG    pp_maxTransfer
   ULONG    pp_mask
   LONG     pp_bootPrio
   ULONG    pp_dosType
   LABEL    MyParmPkt_Sizeof

    STRUCTURE MyMemPkt,0
   ULONG    residentstructure
   ULONG    buffermem
   ULONG    rdbmem
   ULONG    parametermem
   ULONG    configdev
   ULONG    expansionlib
   ULONG    devicenode
   ULONG    iohandler
   ULONG    unitptr
   ULONG    unitnum
   ULONG    ATARdWtRoutine
   LABEL    MyMemPkt_Sizeof


