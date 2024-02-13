## eXtended 1541 support for the KIM-1

THIS CODE IS EXPERIMENTAL AND A WORK IN PROGRESS. USE AT YOUR OWN RISK!!!

Based on the original work by Dave McMurtrie <dave@commodore.international>, which in turn is a subset of the original c64 kernal code.

Modified by Netzherpes <<webdoktor@netzherpes.de>> to start code at $A000 and compile with CC65.

Modified 30 Jan 2024 by Eduardo Casino <<mail@eduardocasino.es>>:
* General cleanup
* Implement dir listing, full file names, send commands and msg printing
           
Dir listing and send command adapted from https://codebase64.org/

Original program by Dave McMurtrie:<br>
https://commodore.international/kim-iec/kim1541_public.asm<br>
https://commodore.international/kim-iec/kim1541.bin<br>

Netzherpes modifications:<br>
https://github.com/netzherpes/KIM1541

### Building

By default, the makefile places the code at 0xF000, the zero page variables starting at 0xDD and the bss segment at 0x17DD.

Those values are to allow integration with Corsham's xKIM (which occupies the 4K space at 0xE000) and KB9, which uses all zero page addresses from 0X00 to 0XDC.

Modify the OFFSET, ZPINIT and BSSINIT variables if you need a different setup.

To build, just use make:

```
$ make
```
This generates the romable code (binary and Intel hex files) and an include file, iecproto.inc, to make use of the functions in your code.

[Here there is an example integration](https://github.com/eduardocasino/xKIM/tree/IEC_support) with Corsham's xKIM monitor. 

[Another example with Microsoft's KB9 Basic](https://github.com/eduardocasino/msbasic/tree/IEC_support).


