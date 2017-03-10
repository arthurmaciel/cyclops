util.o: util.sld
	cyclone util.sld

cyclops: cyclops.scm util.o
	cyclone cyclops.scm

.PHONY: clean

clean:
	rm -rf cyclops *.c *.o bin/ cyclone/ *.meta
