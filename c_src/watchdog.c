/* GPIO control and read access + watchdog. */

#include <stdlib.h>
#include <stdio.h>
#include <poll.h>
#include <string.h>
#include <unistd.h>
#define LOG(...) fprintf(stderr, __VA_ARGS__)
#define ERROR(...) do { LOG(__VA_ARGS__); shutdown(1); } while(0)
#define ASSERT(x) if (!(x)) { ERROR("assert: " #x "\n"); }
#define STDIN_EVENTS (POLLHUP | POLLIN)
char line[64];
char gpio[64] = "";

void gpio_set(int state) {
    char buf[] = {'0' + !!state, '\n'};
    FILE *f = fopen(gpio, "w");
    if (f) {
        fwrite(buf, sizeof(buf), 1, f);
        fclose(f);
    }
    else {
        // Can't use ASSERT or ERROR here, as that calls gpio_set(0).
        // If it fails, we can't do anything about it.
        LOG("gpio_set: '%s' open failed\n", gpio);
        exit(1);
    }
}
void on(void)  { gpio_set(1); }
void off(void) { gpio_set(0); }

static void shutdown(int n) {
    off();
    exit(n);
}

void state(void) {
    char s;
    FILE *f;
    ASSERT(f = fopen(gpio, "r"));
    ASSERT(1 == fread(&s, 1, 1, f));
    printf("%s\n", !!(s-'0') ? "on" : "off");
    fflush(stdout);
}

static void readline(void) {
    int i=0;
    for(;;) {
        ASSERT(i < sizeof(line));
        char c;
        // fread and poll don't mix well, so use read
        ASSERT(1 == read(0, &c, 1));
        if (c == '\n') {
            line[i] = 0;
            return;
        }
        line[i++] = c;
    }
}

int main(int argc, char **argv) {
    ASSERT(argc == 3);
    strncpy(gpio, argv[1], sizeof(gpio)-1);
    int timeout_ms = 1000 * atoi(argv[2]);
    struct pollfd pfd;
    pfd.fd = 0;
    pfd.revents = 0;
    pfd.events = STDIN_EVENTS;

    for(;;) {
        int rv;
        ASSERT(-1 != (rv = poll(&pfd, 1, timeout_ms)));
        ASSERT(rv >= 0);
        if (rv == 0) {
            ERROR("timeout\n");
        }
        else {
            readline();
            // LOG("cmd: %s\n", line);
            if      (!strcmp("state",line)) { state(); }
            else if (!strcmp("on",line))    { on();    }
            else if (!strcmp("off",line))   { off();   }
            else {
                ERROR("unknown: %s\n", line);
            }
        }
    }
}
