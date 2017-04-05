cyclops: cyclops.scm download.o util.o
# This was generated via 'cyclone -d cyclops.scm' and -lcurl was added manually
	cyclone -CE "cc cyclops.o  /usr/local/share/cyclone/scheme/cyclone/common.o  /usr/local/share/cyclone/scheme/base.o  /usr/local/share/cyclone/scheme/char.o  /usr/local/share/cyclone/scheme/cyclone/util.o  /usr/local/share/cyclone/scheme/process-context.o /usr/local/share/cyclone/scheme/write.o  util.o  download.o  /usr/local/share/cyclone/srfi/2.o  /usr/local/share/cyclone/scheme/file.o  /usr/local/share/cyclone/scheme/read.o  -pthread -lcyclone -lck -lm -ltommath -ldl -lcurl -O2 -Wall -I/usr/local/include -L/usr/local/lib -o cyclops" cyclops.scm

util.o: util.sld
	cyclone util.sld

download.o: download.sld download-header.h
	cyclone download.sld

.PHONY: clean

clean:
	rm -rf cyclops *.c *.o bin/ cyclone/ *.meta
