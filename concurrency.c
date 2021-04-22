/*
 *  RJEC concurrency functions
 */

#include "libmill/libmill.h"
#include "libmill/chan.h"
#include <stdbool.h>
#include <string.h>
#include <sys/time.h>
#include <stdint.h>

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

struct select_clause {
    char op;
    void *ch;
    void *val;
    long len;
};

int selectchan(struct select_clause *clauses, int nclauses)
{
    mill_choose_init_(MILL_HERE_);
    int mill_idx = -2;
    while(1) {
        struct mill_clause mill_clauses[nclauses];
        for (int i = 0; i < nclauses; ++i) {
            if (clauses[i].op == 's') {
                mill_choose_out_((void *) &mill_clauses[i], clauses[i].ch,
                    clauses[i].val, clauses[i].len, i);
            }
            else if (clauses[i].op == 'r') {
                mill_choose_in_((void *) &mill_clauses[i], clauses[i].ch,
                    clauses[i].len, i);
            }
            else {
                mill_panic("invalid clause type");
            }
        }
        mill_idx = mill_choose_wait_();

        if (mill_idx != -2) {
            if (mill_idx < 0 || mill_idx >= nclauses) {
                mill_panic("invalid clause index");
            }
            if (clauses[mill_idx].op == 'r') {
                memcpy(clauses[mill_idx].val,
                    mill_choose_val_(clauses[mill_idx].len),
                    clauses[mill_idx].len);
            }
            break;
        }
    }
    // printf("select: got index %d\n", mill_idx);
    return mill_idx;
}

int get_time()
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (((int64_t)tv.tv_sec * 1000000) + tv.tv_usec);
}

// test function for select
int food(chan ch1, chan ch2, chan quit)
{
    while (1) {
        bool q = 0;
        char val1 = 'a';
        int val2;

        /* libmill syntax:
        choose {
        out(ch1, char, val1):
            printf("Value %c sent.\n", val1);
        in(ch2, int, val2):
            printf("Value %d received.\n", val2);
        in(quit, bool, q):
            printf("Bool value %d received\n", q);
            if (q) {
                printf("quitting...\n");
            }
            return 0;
        end
        }*/

        struct select_clause clauses[] = {
            {'s', ch1, &val1, sizeof(val1)},
            {'r', ch2, &val2, sizeof(val2)},
            {'r', quit, &q, sizeof(q)}
        };
        int rc = selectchan(clauses, 3);
        if (rc == 0) {
            printf("Value %c sent.\n", val1);
        }
        if (rc == 1) {
            printf("Value %d received.\n", val2);
        }
        if (rc == 2) {
            printf("Bool value %d received\n", q);
            if (q) {
                printf("quitting...\n");
            }
            return 0;
        }
    }
    return 0;
}

/* test driver for select test:
int main()
{
    chan ch1 = chmake(char, 0);
    chan ch2 = chmake(int, 0);
    chan quit = chmake(bool, 10);
    go(food(ch1, ch2, quit));
    for (int i = 0; i < 5; ++i) {
        printf("%c\n", chr(ch1, char));
        chs(ch2, int, i);
    }
    chs(quit, bool, true);
}*/