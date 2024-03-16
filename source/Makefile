all:  	mainmsx1.bin gameload.bin rommon.bin 4k0.bin games


rommon.bin: rommon.asm
	z80asm -DPLATFORM=1 -b -l -m rommon.asm  2> rommon.err

mainmsx1.bin: mainmsx1.asm main.asm systemva.asm hooks.asm util.asm video.asm chput.asm inlin.asm font.asm slot.asm ps2keyb.asm rommon.asm basic.asm hardware.asm
	z80asm -DPLATFORM=1 -b -l -m mainmsx1.asm  2> main.err

gameload.bin: gameload.asm
	z80asm -b -l -m gameload.asm  2> gameload.err

4k0.bin: 4k0.asm
	z80asm -b -l -m 4k0.asm  2> 4k0.err


games: mainmsx1.bin gameload.bin
	cat gameload.bin mainmsx1.bin > ../bin/GOMSX.COM
	cat gameload.bin mainmsx1.bin ../gameimg/pac.IMG  > ../bin/PAC.COM
	cat gameload.bin mainmsx1.bin ../gameimg/moon.IMG >  ../bin/MOON.COM
	cat gameload.bin mainmsx1.bin ../gameimg/galax.IMG > ../bin/GALAX.COM
	cat gameload.bin mainmsx1.bin 4k0.bin ../gameimg/SPACE.IMG > ../bin/SPACE.COM
	cat gameload.bin mainmsx1.bin 4k0.bin rommon.bin > ../bin/ROMMON.COM

pretty:
	cp *.asm ./backup
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/mainmsx1.asm mainmsx1.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/gameload.asm gameload.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/systemva.asm systemva.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/hooks.asm hooks.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/util.asm util.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/video.asm video.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/chput.asm chput.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/inlin.asm inlin.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/font.asm font.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/slot.asm slot.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/ps2keyb.asm ps2keyb.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/rommon.asm rommon.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/basic.asm basic.asm
	pretty6502 -p2 -a0 -du -mu -c50 -l ./backup/hardware.asm hardware.asm


clean:
	@rm -f *.err
	@rm -f *.lis
	@rm -f *.o
	@rm -f *.out
	@rm -f *.com
	@rm -f *.co6
	@rm -f *.map
	@rm -f *.lst
	@rm -f *.hex
	@rm -f *.s19
	@rm -f *.s
	@rm -f *.bin
	@rm -f *.sys


cleandust:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.map
	@rm -f *.lst