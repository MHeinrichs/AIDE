#define DATA_SIZE 392

struct block0 {
    long    signature[2];

    long    device_start;
    long    device_length;
    long    fs_start;
    long    fs_length;

    long    dosname;
    long    devname;
    long    unit;
    long    flags;
    long    tablesize;
    long    blocksize;
    long    mustbe0;
    long    heads;
    long    mustbe1;
    long    secpertrack;
    long    reserved;
    long    prealloc;
    long    interleave;
    long    lowcyl;
    long    highcyl;
    long    buffers;
    long    bufmemtype;
    long    maxtransfer;
    long    mask;
    long    bootpri;
    long    dostype;

    long    fs_stack;
    long    fs_pri;
    long    fs_globalv;

    char    data[DATA_SIZE];
};

#define BLK0 0x424c4b30
