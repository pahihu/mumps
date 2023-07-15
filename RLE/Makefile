CC = cc -g
CFLAGS = -Wall -ansi -pedantic -DTESTING

SRCS = rle.c
OBJS = $(SRCS:.c=.o)

all: rle

rle: $(OBJS)
	$(CC) -o $@ $(OBJS)

clean:
	$(RM) rle $(OBJS)
