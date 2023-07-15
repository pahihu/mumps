CC = cc -g
CFLAGS = -Wall

SRCS = rle.c
OBJS = $(SRCS:.c=.o)

all: rle

rle: $(OBJS)
	$(CC) -o $@ $(OBJS)

packbits.bin: packbits.hex
	xxd -r -p packbits.hex $@

test: rle packbits.bin
	./rle pack packbits.bin packbits.pak
	./rle unpack packbits.pak packbits.upk
	cmp packbits.bin packbits.upk

clean:
	$(RM) rle $(OBJS)
	$(RM) packbits.bin
	$(RM) packbits.pak packbits.upk
