/*
 *  RJEC concurrency functions
 */

#include "libmill/libmill.h"
#include <stdbool.h>

void yeet(int (*start_routine)(void *), void *args)
{
    go(start_routine(args));
}

void *makechan(char typ, int buf)
{
    chan ch;
    switch(typ) {
        case 'i':
            ch = chmake(int, buf);
            break;
        case 'c':
            ch = chmake(char, buf);
            break;
        case 'b':
            ch = chmake(bool, buf);
            break;
    }
    return ch;
}

void send(void *ch_void_ptr, char typ, int val)
{
    chan ch = ch_void_ptr;
    switch(typ) {
        case 'i':
            chs(ch, int, val);
            break;
        case 'c':
            chs(ch, char, (char)val);
            break;
        case 'b':
            chs(ch, bool, (bool)val);
            break;
    }
}

int recv_int(void *ch_void_ptr)
{
    chan ch = ch_void_ptr;
    return chr(ch, int);
}

bool recv_bool(void *ch_void_ptr)
{
    chan ch = ch_void_ptr;
    return chr(ch, bool);
}

char recv_char(void *ch_void_ptr)
{
    chan ch = ch_void_ptr;
    return chr(ch, char);
}