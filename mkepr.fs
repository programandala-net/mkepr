#! /usr/bin/env gforth

\ mkepr.fs

s" 0.0.1-201512101729" 2constant version

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Description

\ mkepr is a command line tool which creates EPROM image files
\ containing files from the host system, ready to be used by the
\ Cambridge Z88 emulators ZEsarUX and OZvm.

\ More details on the mkepr webpage:
\ http://programandala.net/en.program.mkepr.html

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Author and license

\ Copyright (C) 2015 Marcos Cruz (programandala.net)
\
\ You may do whatever you want with this work, so long as you
\ retain the copyright notice(s) and this license in all
\ redistributed copies and derived works. There is no warranty.

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Acknowledgments

\ mkepr is written in Forth for Gforth:
\
\ http://gnu.org/software/gforth

\ The information on the File Card format was retrieved from
\ the website of the Z88 Development Team:
\
\ https://cambridgez88.jira.com/wiki/display/DN/Miscellaneous+useful+information#Miscellaneoususefulinformation-FileCardformat

\ The testings were done on the ZEsarUX emulator (snapshot
\ version 3.2-SN):
\
\ http://sourceforge.net/projects/zesarux/

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ History

\ See at the end of the file.

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ To-do

\ Choose better names for some words.
\ Check input file names are 10+3 format.
\ Check the chars of filenames are legal in OZ.
\ Confirm that directories are stored as part of the filename,
\ and check their names as well.

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Installation

\ Make sure the <mkepr.fs> is executable. Example:

\     chmod u+x mkepr.fs

\ Copy, move or link it to a directory on your path. Example:

\     ln mkepr.fs ~/usr/local/bin/mkepr

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Documentation on the File Card format

\ $0000       File entry
\ ...         File entry
\ ... 
\ ...         Latest file entry
\ ...         $FF's until
\ $3FC0       $00's until
\ $3FF7       $01
\ $3FF8       4 byte random id
\ $3FFC       size of card in banks (2=32K, 8=128K, 16=256K)
\ $3FFD       sub-type:
\               $7E for 32K cards
\               $7C for 128K (or larger) cards
\ $3FFE       'o'
\ $3FFF       'z' (file eprom identifier, lower case 'oz')

\ A file entry has the form:

\ 1 byte      n           length of filename
\ 1 byte      x           '/' for latest version
\                         $00 for old version (deleted)
\ n-1 bytes   'xxxx'      filename
\ 4 bytes     m           length of file
\                         (least significant byte first)
\ m bytes                 body of file

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Requirements

only forth definitions
warnings off

\ From Gforth:

require string.fs  \ dynamic strings
require random.fs  \ random number generator

\ From the Forth Foundation Library
\ (http://irdvo.github.io/ffl/):

require ffl/arg.fs  \ argument parser

\ From the Galope library
\ (http://programandala.net/en.program.galope.html):

: unslurp-file  ( ca1 len1 ca2 len2 -- )
  w/o create-file throw >r
  r@ write-file throw
  r> close-file throw  ;
  \ Save memory region _ca1 len1_ to file _ca2 len2_.

: $variable  ( "name" -- )
  variable  0 latestxt execute $!len  ;
  \ Create and initialize a dynamic string variable "name".

: d>str  ( ud -- ca len )
  tuck dabs <# #s rot sign #>  ;

: n>str  ( n -- ca len )
  s>d d>str  ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ EPROM image

$variable eprom-filename

: eprom-filename!  ( ca len -- )
  eprom-filename $!  ;

: eprom-filename@  ( -- ca len )
  eprom-filename $@  ;

s" output.epr" 2dup 2constant default-eprom-filename
                    eprom-filename!

16 constant /filename
  \ Max length of OZ filenames.

: KiB  ( n1 -- n2 )  1024 *  ;
: KiB/  ( n1 -- n2 )  1024 /  ;

 16 KiB constant  16KiB
128 KiB constant 128KiB

0 value eprom
  \ Address of the EPROM space (0 if not allocated).

variable >eprom  0 >eprom !
  \ Pointer to the current free address in the EPROM.

: string>eprom  ( ca len  -- )
  dup >r  >eprom @ swap move  r> >eprom +!  ;
  \ Add the string _ca len_ to the EPROM;
  \ the length is not saved, only the contents.

: byte>eprom  ( b -- )
  >eprom @ c! 1 >eprom +!  ;
  \ Add _b_ to the EPROM.

32 KiB value /eprom
  \ Length of the EPROM image in bytes, 32 KiB by default.

0 value eprom-header
  \ Address of the EPROM header
  \ (the header is at the end of the file).

64 constant /eprom-header
  \ Length of the EPROM header in bytes
  \ (the header is at the end of the file).

: eprom-header-1          ( -- ca )  eprom-header 55 +  ;
: eprom-header-random-id  ( -- ca )  eprom-header 56 +  ;
: eprom-header-size       ( -- ca )  eprom-header 60 +  ;
: eprom-header-subtype    ( -- ca )  eprom-header 61 +  ;
: eprom-header-oz-id      ( -- ca )  eprom-header 62 +  ;

: allocate-eprom  ( -- a )
  /eprom allocate throw  ;

: erase-eprom  ( -- )
  eprom /eprom $FF fill  ;

: erase-eprom-header  ( -- )
  eprom-header /eprom-header erase  ;

: calculate-eprom-header  ( -- )
  eprom /eprom + /eprom-header - to eprom-header  ;
  \ Calculate the address of the EPROM header.

: random-id  ( -- b1 b2 b3 b4 )
  4 0 do  256 random  loop  ;
  \ Return an EPROM random id.

: store-eprom-random-id  ( -- )
  random-id eprom-header-random-id >r
  r@     c!
  r@ 1+  c!
  r@ 2 + c!
  r> 3 + c!  ;
  \ Store the EPROM random id.

: store-eprom-banks  ( -- )
  /eprom 16KiB / eprom-header-size c!  ;
  \ Store the number of 16 KiB banks of the EPROM.

: large-eprom?  ( -- f )
  /eprom [ 128KiB 1- ] literal >  ;
  \ Is the EPROM 128 KiB or larger?

: store-eprom-subtype  ( -- )
  large-eprom?
  if  $7C  else  $7E  then  eprom-header-subtype c!  ;
  \ Store the EPROM subtype, depending on its size.

: store-eprom-oz-id  ( -- )
  'O' eprom-header-oz-id    c!
  'Z' eprom-header-oz-id 1+ c!  ;
  \ Store the "OZ" identifier of the EPROM.

: init-eprom-header  ( -- )
  erase-eprom-header  1 eprom-header-1 c!
  store-eprom-random-id  store-eprom-banks
  store-eprom-subtype  store-eprom-oz-id  ;

: format-eprom  ( -- )
  erase-eprom init-eprom-header  ;

: create-eprom  ( -- )
  allocate-eprom  dup to eprom  >eprom !
  calculate-eprom-header  format-eprom  ;

: eprom-needed  ( -- )
  eprom ?exit create-eprom  ;
  \ Create an EPROM, if not done yet.

: unused-eprom  ( -- n )
  eprom-header >eprom @ -  ;
  \ Unused space in the EPROM, in bytes.

: save-eprom  ( -- )
  eprom /eprom eprom-filename@ unslurp-file  ;
  \ Save the EPROM to a file.

: ?eprom-size-number  ( ca len -- )
  nip 0<> abort" Wrong size: not a valid number."  ;
  \ Error if the EPROM size number is wrong.
  \ _ca len_ is the string left by `>number`, and its
  \ length must be zero if the conversion was successful.

: ?eprom-size-pages  ( n -- )
  16 mod abort" Wrong size: it must be a multiple of 16"  ;
  \ Error if the EPROM size _n_ (in KiB) is not a multiple of 16.

: check-eprom-size  ( d ca len -- n )
  ?eprom-size-number d>s dup ?eprom-size-pages  ;
  \ Check if the EPROM size, specified by the `--size` option,
  \ is right.  _d ca len_ are the result of `>number`.
  \ If no error is found, return the size _n_ (in KiB).

: ?no-eprom-yet  ( -- )
  eprom abort" Invalid size option: the EPROM is already created."  ;
  \ Error if the EPROM is already created.

: set-eprom-size  ( ca len -- )
  ?no-eprom-yet
  0. 2swap >number check-eprom-size KiB to /eprom  ;
  \ Set the EPROM size in KiB, as found in the string _ca len_.

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Input files

2variable filename
  \ Address and length of the current input file name.

2variable file-contents
  \ Address and length of the contents of the current input
  \ file.

: get-file  ( ca len -- )
  slurp-file file-contents 2!  ;
  \ Get the contents of input file _ca len_ and store it.

: file-length  ( -- len )
  file-contents @  ;
  \ Length of the current input file.

: free-file  ( -- )
  file-contents 2@ drop free throw  ;
  \ Free the allocated space of the the current input file.

variable files  \ counter

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Copying a file

: full-eprom?  ( -- f )
  unused-eprom file-length <  ;
  \ Is the EPROM full?

: ?space-left  ( -- )
  full-eprom? abort" No space left in the EPROM."  ;
  \ Error if there's no space left in the EPROM.

: file-too-big?  ( -- f )
  file-length  16KiB >  ;
  \ Is the input file is too big?

: ?file-length  ( -- )
  file-too-big? abort" File bigger than 16 KiB."  ;
  \ Error if the input file is too big.

: ?filename  ( ca len -- )
  2drop exit  \ XXX TMP
  nip /filename > abort" Filename too long."  ;
  \ Error if the input file name is too long.
  \ XXX TODO -- check the 12+3 format
  \ XXX TODO -- check all characters are legal
  \ XXX TODO -- check directory names

: check-file  ( ca len -- )
  ?filename ?file-length ?space-left  ;

: /filename  ( -- len )
  filename @  ;
  \ Length of the current input file name.

: filename>eprom  ( -- )
  /filename 1+ byte>eprom  '/' byte>eprom
  filename 2@ string>eprom  ;
  \ Copy the input file name.

: file-length>eprom  ( -- )
  file-length dup dup dup
                        256 mod byte>eprom
                          256 / byte>eprom
        [ 256 256 * ] literal / byte>eprom
  [ 256 265 256 * * ] literal / byte>eprom  ;
  \ Store the length of the current input file in the EPROM;
  \ least significant byte first.

: file-contents>eprom  ( -- )
  file-contents 2@ string>eprom  ;
  \ Store the contents of the current input file in the EPROM.

: current-file>eprom  ( -- )
  filename>eprom file-length>eprom  file-contents>eprom  ;
  \ Store the current input file in the EPROM.

true value verbose
  \ List the processed input files?

: show-filename  ( ca len -- )
  verbose if  type cr  else  2drop  then  ;

: file>eprom  ( ca len -- )
  1 files +!  eprom-needed
  2dup filename 2!  2dup get-file  2dup show-filename
  check-file current-file>eprom free-file  ;
  \ Copy file _ca len_ to the EPROM, if possible.

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Argument parser

s" mkepr"                               \ program name
s" [options] [inputfiles]"              \ program usage
version                                 \ program version
s" Report bugs to programandala.net"    \ program extra info
arg-new value mkepr-arguments

\ Add the default help and version options:

mkepr-arguments arg-add-help-option
mkepr-arguments arg-add-version-option

\ Add the -q/--quiet option switch:

char q                                \ short option name
s" quiet"                             \ long option name
s" quiet mode: input files will not be listed"  \ description
true                                  \ a switch
4 dup constant arg.quiet-option       \ option id
mkepr-arguments arg-add-option

\ Add the -o/--output option parameter:

char o                                \ short option name
s" output=FILE"                       \ long option name
s" set output file; default: <"
  default-eprom-filename s+ s" >" s+  \ description
false                                 \ a parameter
5 dup constant arg.output-option      \ option id
mkepr-arguments arg-add-option

char s                                \ short option name
s" size=KiB"                          \ long option name
s" set the EPROM size in KiB; default: "
  /eprom KiB/ n>str s+                \ description
false                                 \ a parameter
6 dup constant arg.size-option        \ option id
mkepr-arguments arg-add-option

: show-help  ( -- )
  mkepr-arguments arg-print-help bye  ;

: show-version  ( -- )
  mkepr-arguments arg-print-version  ;

: be-quiet  ( -- )
  false to verbose  ;

: option? ( -- ca len n f | n f )
  mkepr-arguments arg-parse  ( ca len n | n )
  dup arg.done <> over arg.error <> and  ;
  \ Parse and return the next option: its value _ca len_,
  \ if any; and its number _n_; _f_ is true when
  \ it's a valid option.

: option  ( ca len n | n -- )
  case
    arg.help-option     of  show-help           endof
    arg.version-option  of  show-version        endof
    arg.quiet-option    of  be-quiet            endof
    arg.output-option   of  eprom-filename!     endof
    arg.size-option     of  set-eprom-size      endof
    arg.non-option      of  file>eprom          endof
  endcase  ;
  \ Manage parsed option _n_ and its possible value _ca len_.

: parse-options  ( -- )
  begin  option?  while  option  repeat  ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Boot

: run  ( -- )
  files off  parse-options
  files @ if  save-eprom  else  show-help  then  ;

run bye

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ History

\ 2015-12-05: Start. Documentation on the File Card format.
\ 2015-12-08: Version 0.0.0. Main structure.
\ 2015-12-09: Version 0.0.1. First working version.

\ vim: tw=64
