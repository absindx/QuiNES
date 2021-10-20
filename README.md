# QuiNES - Quine of NES  

![Execution screen](Images/QuiNES.gif)  

[Quine](https://en.wikipedia.org/wiki/Quine_(computing)) is a program that displays own source code.  

## Assemble  

Assemble using nesasm.  

```shell
nesasm QuiNES.asm
```

[build.bat](build.bat) is available for windows.  

## Cartridge  

Burn to 16KiB NROM cartridge.  
It can also be executed with a flash cartridge like EverDrive.  

## How to  

| Key                   | Description                                           |
|:----------------------|:------------------------------------------------------|
| D-Pad Up, Down        | Scroll the screen                                     |

## Idea  

`SrcData` defines the source code string for each row,
and `SrcIndex` defines the display line number corresponding to the source code.  
First, the source code is displayed it from `SrcData`, then `SrcIndex` and `SrcData`, and finally CHR ROM from PPU.  
The code is ... a verbose screen rendering process.  

## Policy  

* All written in assembly
* Include CHR in source code
* Do not include files
* Do not macroize routines
* Do not disassemble code area and display it

## ToDo  

* None so far  

## License  

[MIT License](LICENSE).  
