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

void closechan(void *ch_void_ptr, char typ)
{
    chan ch = ch_void_ptr;
    switch(typ) {
        case 'i':
            chdone(ch, int, 0);
            break;
        case 'c':
            chdone(ch, char, 0);
            break;
        case 'b':
            chdone(ch, bool, 0);
            break;
    }
}

// int subroutine(void *args){
//     chan ch = args;
//     chs(ch, int, 1);
//     chs(ch, int, 2);
//     chs(ch, int, 3);

//     chdone(ch, int, 0);
    
//     // for (int i = 0 ; i < 5; i ++){
//     //     int a = chr(ch, int);
//     //     printf("%d\n", a);
//     // }
// }
// int main () {
//     chan ch = chmake(int, 10);
    
//     yeet(&subroutine, ch);
//     printf("%d\n", chr(ch, int));
//     printf("%d\n", chr(ch, int));
//     printf("%d\n", chr(ch, int));
//     printf("%d\n", chr(ch, int));
//     printf("%d\n", chr(ch, int));
// }