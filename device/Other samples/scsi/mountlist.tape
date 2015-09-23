/*
 *  Mountlist fragment for the TAPE: handler.
 *
 *  StartUp = buffers/devicename/unitnum/openflags
 *
 *  buffers: number of 512-byte tape blocks to buffer (make this as large
 *           as you can afford given the amount of memory you have).
 *
 *  devicename,unitnum,openflags: used to open the device driver.
 *
 */
TAPE:      Handler = L:Tape-Handler
           StartUp = 512/scsi-disk.device/7/0
           Stacksize = 4000
           Priority = 5
           GlobVec = -1
#
