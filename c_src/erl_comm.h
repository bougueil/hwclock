#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <alsa/asoundlib.h>


typedef unsigned char byte;

extern int read_cmd(byte *buf);
extern int write_cmd(byte *buf, int len);
extern byte * write_int(int arg, byte *buf);
extern byte read_byte(byte *buf);
extern int read_int(byte *buf);
