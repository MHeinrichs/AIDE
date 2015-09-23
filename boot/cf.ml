cfp: Device = cfpar.device
   /* If FastFileSystem is in KS-ROM (2.04+), comment the next line */
/*   FileSystem = L:FastFileSystem*/
   Unit = 0
   Flags = 0
   Surfaces = 2
   BlocksPerTrack = 40
   Reserved = 2
   Interleave = 0
   LowCyl = 2
   HighCyl = 9
   Buffers = 10
   GlobVec = -1
   BufMemType = 1
   DosType = 0x444F5301
   Mount = 1
#

fcc: Device = cfpar.device
   /* If FastFileSystem is in KS-ROM (2.04+), comment the next line */
/*   FileSystem = L:FastFileSystem*/
   Unit = 0
   Flags = 0
   Surfaces = 2
   BlocksPerTrack = 40
   Reserved = 2
   Interleave = 0
   LowCyl = 10
   HighCyl = 17
   Buffers = 10
   GlobVec = -1
   BufMemType = 1
   DosType = 0x444F5301
   Mount = 1
#

