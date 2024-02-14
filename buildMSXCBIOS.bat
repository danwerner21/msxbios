set TASM=m:\development\vintage\tasm301
set TASMTABS=%TASM%

cd source
%TASM%\tasm -t80 -b -g0 MAINMSX1.asm MAINMSX1.BIN
%TASM%\tasm -t80 -b -g0 LOGOMSX1.asm LOGOMSX1.BIN
pause
%TASM%\tasm -t80 -b -g3 MAINMSX1.asm MAINMSX1.img
%TASM%\tasm -t80 -b -g3 LOGOMSX1.asm LOGOMSX1.img
pause
%TASM%\tasm -t80 -b -g3 GAMELOAD.asm GAMELOAD.img
pause
%TASM%\tasm -t80 -b -g3 4k0.asm 4k0.img
pause
cd ..
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG  bin\GOMSX.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\PAC.IMG  bin\PAC.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+source\4k0.IMG+gameimg\SPACE.IMG bin\SPACE.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\MOON.IMG bin\MOON.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\galax.IMG bin\galax.com

copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Shougi.IMG bin\Shougi.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\LodeRun1.IMG bin\LodeRun1.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Gfright.IMG bin\Gfright.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\GpWorld.IMG bin\GpWorld.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\DrCopy.IMG bin\DrCopy.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\PacMan.IMG bin\PacMan.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Pegasus.IMG bin\Pegasus.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\CityConn.IMG bin\CityConn.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\SnakeIt.IMG bin\SnakeIt.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Bomberma.IMG bin\Bomberma.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Payload.IMG bin\Payload.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\MidnBrot.IMG bin\MidnBrot.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ALFAROID.IMG bin\ALFAROID.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Zaxxon.IMG bin\Zaxxon.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\NightSha.IMG bin\NightSha.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ZexasLim.IMG bin\ZexasLim.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\JPWinkle.IMG bin\JPWinkle.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\CastleEx.IMG bin\CastleEx.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\HADES.IMG bin\HADES.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Guardic.IMG bin\Guardic.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\TearofNi.IMG bin\TearofNi.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\MSTUDIO.IMG bin\MSTUDIO.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Laptick2.IMG bin\Laptick2.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\StarSold.IMG bin\StarSold.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Traffic.IMG bin\Traffic.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\SkyGaldo.IMG bin\SkyGaldo.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ChampIce.IMG bin\ChampIce.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ARAMO.IMG bin\ARAMO.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\StarBlaz.IMG bin\StarBlaz.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Rambo.IMG bin\Rambo.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Indianno.IMG bin\Indianno.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Scion.IMG bin\Scion.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\DokiDoki.IMG bin\DokiDoki.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Broder.IMG bin\Broder.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Scarlet7.IMG bin\Scarlet7.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ChampBox.IMG bin\ChampBox.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\GunjinSh.IMG bin\GunjinSh.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\JMahjong.IMG bin\JMahjong.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Flicky.IMG bin\Flicky.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\HeliTank.IMG bin\HeliTank.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\LodeRun2.IMG bin\LodeRun2.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Golgo13.IMG bin\Golgo13.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Swing.IMG bin\Swing.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\inspectz.IMG bin\inspectz.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\CosmoExp.IMG bin\CosmoExp.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Crusader.IMG bin\Crusader.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ShoutMat.IMG bin\ShoutMat.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\SuperDri.IMG bin\SuperDri.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Monsters.IMG bin\Monsters.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Bosconia.IMG bin\Bosconia.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+source\4k0.IMG+gameimg\GODZILLA.IMG bin\GODZILLA.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ALBATROS.IMG bin\ALBATROS.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ChlDurby.IMG bin\ChlDurby.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Protecto.IMG bin\Protecto.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Tensidac.IMG bin\Tensidac.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Arubator.IMG bin\Arubator.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Playball.IMG bin\Playball.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\BoulderD.IMG bin\BoulderD.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\HangOn.IMG bin\HangOn.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\FMahjong.IMG bin\FMahjong.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\KMahjong.IMG bin\KMahjong.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ChSoccer.IMG bin\ChSoccer.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\MobilePl.IMG bin\MobilePl.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+source\4k0.IMG+gameimg\MSXAudio.IMG bin\MSXAudio.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\MoleMole.IMG bin\MoleMole.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Erika.IMG bin\Erika.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\RallyEX.IMG bin\RallyEX.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\BoukenRo.IMG bin\BoukenRo.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Hydlide1.IMG bin\Hydlide1.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\PuzzlePa.IMG bin\PuzzlePa.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\OilsWell.IMG bin\OilsWell.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\BOUKEN.IMG bin\BOUKEN.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\NinjaJaj.IMG bin\NinjaJaj.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\SkyJagua.IMG bin\SkyJagua.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\NinjaKag.IMG bin\NinjaKag.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Anaza.IMG bin\Anaza.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\SuperSyn.IMG bin\SuperSyn.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\TheHeist.IMG bin\TheHeist.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ChampKen.IMG bin\ChampKen.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Panther.IMG bin\Panther.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Senjyo.IMG bin\Senjyo.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ZANAC87.IMG bin\ZANAC87.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\KagenoDe.IMG bin\KagenoDe.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\TheCastl.IMG bin\TheCastl.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Banana.IMG bin\Banana.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\MoonPatr.IMG bin\MoonPatr.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Zanac.IMG bin\Zanac.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\KonGameM.IMG bin\KonGameM.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\VenusFir.IMG bin\VenusFir.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Bosconi1.IMG bin\Bosconi1.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\DamBuste.IMG bin\DamBuste.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\DunkShot.IMG bin\DunkShot.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Volleyba.IMG bin\Volleyba.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ShougiMe.IMG bin\ShougiMe.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Rambo1.IMG bin\Rambo1.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\gprider.IMG bin\gprider.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Loderun3.IMG bin\Loderun3.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\StarTrap.IMG bin\StarTrap.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\IgaNinpo.IMG bin\IgaNinpo.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Explorer.IMG bin\Explorer.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ChampWre.IMG bin\ bin\ChampWre.IMG bcom
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\CHOPLIFT.IMG bin\CHOPLIFT.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\MSXSocce.IMG bin\MSXSocce.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\MKIDWIZ.IMG bin\MKIDWIZ.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\StonofWi.IMG bin\StonofWi.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\StrangeL.IMG bin\StrangeL.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\FireResc.IMG bin\FireResc.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Volguard.IMG bin\Volguard.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Macross.IMG bin\Macross.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\DRUAGA.IMG bin\DRUAGA.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ExArea5.IMG bin\ExArea5.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\LodeRun.IMG bin\LodeRun.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\FormatZ.IMG bin\FormatZ.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Zaider.IMG bin\Zaider.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Othello.IMG bin\Othello.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\PENGLAND.IMG bin\PENGLAND.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\247.img bin\247.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Becky.IMG bin\Becky.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\BattenTa.IMG bin\BattenTa.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\honkball.IMG bin\honkball.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\CasioWor.IMG bin\CasioWor.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Elevator.IMG bin\Elevator.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\BlackOny.IMG bin\BlackOny.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Dorodon.IMG bin\Dorodon.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\HoleIn1.IMG bin\HoleIn1.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Spelunke.IMG bin\Spelunke.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\vIssunhou.IMG bin\vIssunhou.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\SuperSoc.IMG bin\SuperSoc.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\BuruToMa.IMG bin\BuruToMa.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Mappy.IMG bin\Mappy.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\R_LEMAN2.IMG bin\R_LEMAN2.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Kinnikum.IMG bin\Kinnikum.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\catboy.IMG bin\catboy.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ATRUCK.IMG bin\ATRUCK.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Triton.IMG bin\Triton.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ALTLER-W.IMG bin\ALTLER-W.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\StarForc.IMG bin\StarForc.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\MoaiNoHi.IMG bin\MoaiNoHi.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\PoliceSt.IMG bin\PoliceSt.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\DeHeaven.IMG bin\DeHeaven.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ShougiSi.IMG bin\ShougiSi.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Zoom909.IMG bin\Zoom909.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\KungFuTa.IMG bin\KungFuTa.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\HyperRal.IMG bin\HyperRal.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\HadesuNo.IMG bin\HadesuNo.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\GunFrigh.IMG bin\GunFrigh.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\LunarBal.IMG bin\LunarBal.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\payload1.IMG bin\payload1.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\SpaceCam.IMG bin\SpaceCam.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\BOGGLE.IMG bin\BOGGLE.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\TensaiRa.IMG bin\TensaiRa.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Galaga.IMG bin\Galaga.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\HAUNTED.IMG bin\HAUNTED.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Blagger.IMG bin\Blagger.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\FlappyLi.IMG bin\FlappyLi.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\ToppleZi.IMG bin\ToppleZi.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\AMETRUCK.IMG bin\AMETRUCK.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\hole1pro.IMG bin\hole1pro.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Pipi.IMG bin\Pipi.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\DungeonM.IMG bin\DungeonM.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\52.img bin\52.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Jyanyuu.IMG bin\Jyanyuu.com
copy /B source\GAMELOAD.IMG+source\MAINMSX1.IMG+gameimg\Warroid.IMG bin\Warroid.com



PAUSE 
