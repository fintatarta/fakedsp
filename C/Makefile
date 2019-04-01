LIB_SRC=fakedsk.c  sampleio.c  semafori.c  waveio.c

lib: libfakedsk.a

libfakedsk.a: ${LIB_SRC:%.c=libfakedsk.a(%.o)}
	ranlib $@

%.o: %.c %.h
	$(CC) -c $(CFLAGS) $<

fakedsk.o: fakedsk-tmp.o
	$(LD) -r -L/usr/lib -o $@ $< -lm

.INTERMEDIATE: fakedsk-tmp.o

fakedsk-tmp.o: fakedsk.c fakedsk.h
	$(CC) -c $(CFLAGS) -o $@ $<