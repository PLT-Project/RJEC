/*
 *  RJEC concurrency functions
 */

#include "libmill/libmill.h"

void yeet(int (*start_routine)(void *), void *args)
{
    go(start_routine(args));
}