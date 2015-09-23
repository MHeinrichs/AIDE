
#include <exec/ports.h>

struct MyDBMsg {
	struct Message mdb_Message;
	UWORD		mdb_counter;			/*message counter*/
	UWORD		mdb_type;				/*type of message - see below*/
	UBYTE		mdb_data[16];			/*debug data*/
	UWORD		mdb_length;				/*length of debug data*/
};

/*mydbmsg types*/
#define DB_PACKET 1	/*scsi packet (16 bytes) and unit number (1 byte) in mdb_data*/
#define DB_EMULPACKET 2	/*emulated scsi packet*/
#define DB_COMMAND 3	/*dos command (2 bytes) and unit number (1 byte) in mdb_data*/
#define DB_OPENDEV 4	/*OpenDevice unit number (2 bytes) in mdb_data*/

#define CMD_MYCMD	29
