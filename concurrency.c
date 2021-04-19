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