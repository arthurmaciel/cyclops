util.o: util.sld
	cyclone util.sld

cyclops: cyclops.scm util.o
	cyclone cyclops.scm

download: download.scm download-header.h
	cyclone -CE "cc download.o  /usr/local/share/cyclone/scheme/cyclone/common.o  /usr/local/share/cyclone/scheme/base.o  /usr/local/share/cyclone/scheme/write.o  -pthread -lcyclone -lck -lm -ltommath -lcurl -O2 -Wall -I/usr/local/include -L/usr/local/lib -o download" download.scm

.PHONY: clean

clean:
	rm -rf cyclops *.c *.o bin/ cyclone/ *.meta
