# FLP 2024

## Bc. Juraj Mariani, xmaria03

## Akademicky rok 2023/2024

## Zadanie: Turingův stroj

### Použité metódy

Program si uchováva prechodovú funkciu pomocou dynamického predikátu `nstate/4`. Tento predikát, podobne ako postupnosť na vstupe obsahuje štyri časti.

```pl
nstate(CurrState, CurrSymb, NextState, Action)
```

Pre nájdenie riešenia bol použitý algoritmus DFS. Pri prehľadávaní nebol implementovaný mechanizmus limitu zanorenia ani detekcie cyklenia. Program tým pádom sekvenčne prechádza možné stavy. Program teda vypisuje a končí vykonávanie nájdením prvého správneho riešenia. Prípadne výpisom `The Turing Machine has abnormally stopped.` a skončí s kódom 1.

Ako implementáciu vstupno-výstupných operácii bol použitý poskytnutý súbor `input2.pl`, z ktorého boly využité najmä predikáty `read_lines/1` a `writeln/1`.

### Preklad

Preklad programu zastrešuje poskytnutý súbor `Makefile`, pričom preklad programu je hlavný cieľ.

```bash
$ make
swipl -o flp23-log -g main -c input2.pl proj2.pl
% Disabled autoloading (loaded 32 files)
% Disabled autoloading (loaded 4 files)
% Disabled autoloading (loaded 0 files)
```

Ako je možné vidieť, samotný preklad zaisťuje prekladač prologu `swipl`. Ako vstupný bod je špecifikovaná funkcia `main/0` zo súboru `proj2.pl`.

Pri prípadnom nedostatku zásobníka je možné preložiť program nasledovne.

```bash
$ make big_stack
swipl --stack_limit=16g -o flp23-log -g main -c input2.pl proj2.pl
% Disabled autoloading (loaded 32 files)
% Disabled autoloading (loaded 4 files)
% Disabled autoloading (loaded 0 files)
```

Preklad vyústi vo vytvorenie spustiteľného súboru `flp23-log`, ktorý je možné (podľa špecifikácie zadania) spustiť nasledovne.

```bash
./flp23-log < /cesta_k_testum/vstup1.txt > /cesta_k_vysledkum/vystup1.txt
```

V archíve je pribalený testovací súbor `inptSimp.txt`, ktorý bol použitý ako príklad pre rozšírenie.
Doba výpočtu uvedená nástrojom `time`

```bash
real    0m0.030s
user    0m0.008s
sys     0m0.008s
```

### ROZŠÍRENIE / ROZSIRENI

Bolo implementované pseudorozšírenie výberu nasledujúceho stavu v rámci prehľadávania priestoru. Na rozdiel od klasického prehľadávania sa v rozšírení vyberá nasledovník náhodne.

Aktivácia rozšírenia spočíva v zmene predikátu na začiatku súboru.

```pl
/** To use random NextState selection uncomment line 8 and comment line 7 */
operace(turingMachineforAll).
%operace(turingMachineforAllRandomized).
```

Použitie tohto režimu môže mať za následok nájdenie riešenia, ktoré by v prípade základnej implementácie nájdené nebolo (program by mohol cyklil). Na druhú stranu toto riešenie môže pri dlhých páskach spôsobovať vyššiu časovú náročnosť.

Porovnanie riešení klasickej

```bash
$ ./flp23-log < inptSimp.txt
Saaaaaaa
aBaaaaaa
aaFaaaaa
```

a náhodnej varianty

```bash
$ ./flp23-log < inptSimp.txt
Saaaaaaa
aSaaaaaa
aaSaaaaa
aaaBaaaa
aaaaFaaa

$ ./flp23-log < inptSimp.txt
Saaaaaaa
aBaaaaaa
aaFaaaaa

$ ./flp23-log < inptSimp.txt
Saaaaaaa
aSaaaaaa
aaBaaaaa
aaaFaaaa
```

### Obmedzenia

Program bol implementovaný na verzii `swipl 8.4.2`, Avšak na serveri merlin je verzia `6.6.1`. To malo za následok potrebu značnej úpravy programu. Vyššie porovnávacie behy boli vykonávané na vyššej verzii a na nižšej z časových dôvodov neexistujú. Pre použitie náhodného výberu nasledujúceho stavu je nutné mať verziu `swipl 8.4.2` (kompatibilita s nižšími verziami skúmaná nebola). Z toho dôvodu je poskytnutý súbor proj2rand.pl, ktorý obsahuje originálnu verziu projektu pre `swipl 8.4.2`. Preklad je možné uskutočniť pomocou príkazu

```bash
make ver8random
```

v prípade potreby väčšieho zásobníka

```bash
make ver8random_big_stack
```

Ospravedlňujem sa za nepríjemnosti, neočakával som až takéto markantné rozdiely medzi verziami.
