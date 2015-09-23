BLOCKSIZE   EQU   $FF


    STRUCTURE MyParmPkt,0
   ULONG    pp_dosName
   ULONG    pp_execName
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
