# OFFSET  : Where to load. 
# ZPINIT  : first ZP address for xkim1541 use. $DD is the first
#           unused page 0 location if using KB9
# BSSINIT : Start address for xkim1541 variables in the application RAM
#
OFFSET = F000
ZPINIT = E9
BSSINIT= 17D6

TARGETS = xkim1541.hex iecproto.inc

all: $(TARGETS)

iecproto.inc: xkim1541.map xkim1541.inc
	./includes.py --mapfile $< --include $(basename $<).inc > $@

xkim1541.hex: xkim1541.bin
	srec_cat $< -binary -offset 0x$(OFFSET) -o $@ -Intel -address_length=2

xkim1541.bin xkim1541.map: xkim1541.o xkim1541.cfg
	ld65 -C $(basename $<).cfg -o $@ -vm -m $(basename $<).map $<

xkim1541.cfg: xkim1541.cfg.in Makefile
	sed 's/%%OFFSET%%/$$$(OFFSET)/; s/%%ZPINIT%%/$$$(ZPINIT)/ ; s/%%BSSINIT%%/$$$(BSSINIT)/' $< > $@

xkim1541.o: xkim1541.inc

clean:
	$(RM) *.o *.lst *.map *.bin *.cfg

distclean: clean
	$(RM) $(TARGETS)

.s.o: 
	ca65 -g -l $(basename $<).lst $<

