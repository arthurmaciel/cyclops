cyclops: cyclops.scm
	cyclone cyclops.scm

.PHONY: clean

clean:
	rm -rf cyclops *.c *.o bin/ cyclone/ *.meta