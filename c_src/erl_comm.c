/* erl_comm.c */

#include "erl_comm.h"


static int read_exact(byte *buf, int len);
static int write_exact(byte *buf, int len);

byte read_byte(byte *buf) {
  return  buf[0];
}

int read_int(byte *buf) {
  return (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
}

int read_cmd(byte *buf)
{
  int len;
  int nbytes = read_exact(buf, 2);

  //  fprintf(stderr, "read_cmd.  %d %d\n", buf[0], buf[1]);
  if (nbytes != 2) {
    /* fprintf(stderr, "ERROR: read_exact returns %d\n", nbytes); */
    return (-1);
  }
  len = (buf[0] << 8) | buf[1];

  return read_exact(buf, len);
}

int write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);

  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}


byte * write_int(int arg, byte *buf) {
  byte b;
  b = (arg >> 24) & 0xff;
  buf[0] = b;
  b = (arg >> 16) & 0xff;
  buf[1] = b;
  b = (arg >> 8) & 0xff;
  buf[2] = b;
  b = arg & 0xff;
  buf[3] = b;

  return buf + 4;
}


static int read_exact(byte *buf, int len)
{
  int i, got = 0;

  do {
    if ((i = read(0, buf + got, len - got)) <= 0)
      return (i);
    got += i;
  } while (got < len);

  return (len);
}

static int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf + wrote, len - wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote < len);

  return (len);
}
