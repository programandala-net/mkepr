#! /usr/bin/env gforth

\ mkepr.fs

: version  s" 0.4.1+20151217"  ;  \ after http://semver.org

\ ==============================================================
\ Description

\ mkepr is a command line tool that creates EPROM and Intel
\ Flash image files containing files from the host system, ready
\ to be used by the Cambridge Z88 emulators ZEsarUX and OZvm.

\ mkepr webpage:
\ http://programandala.net/en.program.mkepr.html

\ mkepr is written in Forth for Gforth (tested on version
\ 0.7.3):
\ http://gnu.org/software/gforth

\ ==============================================================
\ Author and license

\ Copyright (C) 2015 Marcos Cruz (programandala.net)
\
\ You may do whatever you want with this work, so long as you
\ retain the copyright notice(s) and this license in all
\ redistributed copies and derived works. There is no warranty.

\ ==============================================================
\ Acknowledgments

\ The testings were done on the following Z88 emulators:
\
\ ZEsarUX (snapshot version 3.2-SN), by César Hernández:
\ http://sourceforge.net/projects/zesarux/
\
\ OZvm (version 1.1.5), by Gunter Strube:
\ https://cambridgez88.jira.com/wiki/display/OZVM

\ Thanks to César Hernández for the location of the information
\ about the File Card format, on the website of the Z88
\ Development Team.
\
\ https://cambridgez88.jira.com/wiki/display/DN/Miscellaneous+useful+information#Miscellaneoususefulinformation-FileCardformat

\ Thanks to Garry Lancaster for confirming the format of
\ Intel Flash cards:
\
\ https://www.mail-archive.com/forth-sinclair@yahoogroups.com/msg00038.html

\ Thanks to Anton Erlt for the example how to do `bye` returning an
\ error code to the OS shell:
\
\ http://lists.gnu.org/archive/html/gforth/2015-12/msg00004.html
\ http://www.mail-archive.com/gforth@gnu.org/msg00540.html

\ ==============================================================
\ History

\ See at the end of the file.

\ ==============================================================
\ Installation

\ Make sure the <mkepr.fs> is executable.  Then copy, move or
\ link it to a directory on your path.

\ Example for single-user installation:

\     chmod u+x mkepr.fs
\     ln mkepr.fs ~/bin/mkepr

\ Example for system-wide installation:

\     chmod ugo+x mkepr.fs
\     ln mkepr.fs /usr/local/bin/mkepr

\ ==============================================================
\ Documentation on the File Card format

\ From
\ https://cambridgez88.jira.com/wiki/display/DN/Miscellaneous+useful+information#Miscellaneoususefulinformation-FileCardformat

\ $0000       File entry
\ ...         File entry
\ ... 
\ ...         Latest file entry
\ ...         $FF's until
\ $3FC0       $00's until
\ $3FF7       $01
\ $3FF8       4 byte random id
\ $3FFC       size of card in banks (in units of 16 KiB)
\ $3FFD       sub-type:
\               $7E for 32 KiB cards
\               $7C for 128 KiB (or larger) cards
\ $3FFE       'o'
\ $3FFF       'z' (file eprom identifier, lower case 'oz')

\ A file entry has the form:

\ 1 byte      n           length of filename
\ 1 byte      x           '/' for latest version
\                         $00 for old version (deleted)
\ n-1 bytes   '...'       filename
\ 4 bytes     m           length of file
\                         (least significant byte first)
\ m bytes                 body of file


\ From the source of FlashStore
\ (https://cambridgez88.jira.com/wiki/display/ZFS/FlashStore+%28Z88+popdown%29+Home):
\
\ ____
\ Due to a strange side effect with Intel Flash Chips, a
\ special "NULL" file is saved as the first file to the Card.
\ These bytes occupies the first bytes that otherwise could be
\ interpreted as a random boot command for the Intel chip -
\ the behaviour is an Intel chip suddenly gone into command
\ mode for no particular reason. The NULL file prevents this
\ behaviour by saving a file that avoids any kind of boot
\ commands which sends the chip into command mode when the
\ card has been inserted into a Z88 slot.
\ ____

\ ==============================================================
\ Requirements

only forth definitions
warnings off

\ ----------------------------------------------
\ From Gforth

require string.fs  \ dynamic strings
require random.fs  \ random number generator

\ ----------------------------------------------
\ From the Forth Foundation Library
\ (http://irdvo.github.io/ffl/)

require ffl/arg.fs  \ arguments parser
require ffl/chr.fs  \ char data type

\ ----------------------------------------------
\ From the Galope library
\ (http://programandala.net/en.program.galope.html)

: unslurp-file  ( ca1 len1 ca2 len2 -- )
  w/o create-file throw >r
  r@ write-file throw
  r> close-file throw  ;
  \ Save memory region _ca1 len1_ to file _ca2 len2_.

: $variable  ( "name" -- )
  variable  0 latestxt execute $!len  ;
  \ Create and initialize a dynamic string variable "name".

: d>str  ( d -- ca len )
  tuck dabs <# #s rot sign #>  ;
  \ Convert a double number _d_ to a string _ca len_.

: n>str  ( n -- ca len )
  s>d d>str  ;
  \ Convert number _n_ to a string _ca len_.

: c>str  ( c -- ca len )
  1 allocate throw  tuck c! 1  ;
  \ Convert an ASCII char to a string.

variable (any?)

: any?  ( x0 x1..xn n -- f )
  dup 1+ roll (any?) !  0 swap 0 do  swap (any?) @ = or  loop  ;
  \ Is any _x1..xn_ equal to _x0_?

: sides  { ca1' len1' ca1 len1 len2 -- ca3 len3 ca4 len4 }
  ca1  len1 len1' -             \ left side
  ca1' len2 +  len1' len2 -  ;  \ right side
  \ Convert the result returned by 'search': divide the searched
  \ string at the substring that was searched to.
  \ ca1' len1' = string searched, starting with the first (ca2 len2)
  \ ca1 len1 = original string, before the search
  \ len2 = length of the substring searched for
  \ ca1' len2 = substring found in ca1 len1
  \ ca3 len3 = left side of ca1 len1, until and excluding ca1' len2
  \ ca4 len4 = right side of ca1 len1, after ca1' len2

: /sides  { ca1 len1 ca2 len2 -- ca1 len1' ca3 len3 f }
  ca1 len1 ca2 len2 search dup >r
  if    ca1 len1 len2 sides
  else  over 0  \ fake right side
  then  r>  ;
  \ Search a string _ca1 len1_
  \ for the first occurence of a substring _ca2 len2_.
  \ Divide the string _ca1 len1_ in two parts: return both sides
  \ of the substring _ca2 len2_ (first occurence), excluding the
  \ substring _ca2 len2_ itself.
  \
  \ ca1 len1  = string
  \ ca2 len2  = substring
  \ ca1 len1' = left side (or whole string if not found)
  \ ca3 len3  = right side (or empty string if not found)
  \ f = found?
  \
  \ Note: _ca3 len3_ can be empty also when _f_ is true.

: /slash  ( ca len -- ca#1 len#1 ... ca#n len#n n )
  depth >r
  begin  s" /" /sides 0=  until  2drop
  depth r> 2 - - 2/  ;
  \ Divide a slash separated values string.

: -prefix  ( ca1 len1 ca2 len2 -- ca1' len1' )
  dup >r 2over 2swap string-prefix? r> and /string  ;
  \ Remove a prefix _ca2 len2_ from a string _ca1 len1_.

: typecr ( ca len -- )  type cr  ;

: ?error-bye ( f ca len -- )
  rot if
    ['] typecr stderr outfile-execute 1 (bye)
  then  2drop  ;

\ ==============================================================
\ Card

false value flash-card?
  \ Flag: Create a Flash card instead of an EPROM card?
  \ Default is false.

$variable card-filename

: card-filename!  ( ca len -- )
  card-filename $!  ;

: card-base-filename    ( -- ca len )  s" output"  ;
: flash-card-extension  ( -- ca len )  s" .flash"  ;
: eprom-card-extension  ( -- ca len )  s" .epr"    ;

: default-eprom-card-filename  ( -- ca len )
  card-base-filename eprom-card-extension s+  ;
  \ Default filename for EPROM cards.

: default-flash-card-filename  ( -- ca len )
  card-base-filename flash-card-extension s+  ;
  \ Default filename for Flash cards.

: card-extension  ( -- ca len )
  flash-card? if    flash-card-extension
              else  eprom-card-extension  then  ;
  \ Default card filename extension for the current
  \ type of card.

: default-card-filename  ( -- ca len )
  card-base-filename card-extension s+  ;
  \ Default card filename for the current type of card.

: card-filename@  ( -- ca len )
  card-filename $@len if    card-filename $@
                      else  default-card-filename  then  ;
  \ If the card filename has been set by the command line
  \ option, return it; else return the default filename.

16 constant /filename
  \ Max length of OZ filenames.

: KiB  ( n1 -- n2 )  1024 *  ;
: KiB/  ( n1 -- n2 )  1024 /  ;

 16 KiB constant  16KiB
128 KiB constant 128KiB
512 KiB constant 512KiB

0 value card
  \ Address of the card space (0 if not allocated).

variable >card  0 >card !
  \ Pointer to the current free address in the card.

: string>card  ( ca len  -- )
  dup >r  >card @ swap move  r> >card +!  ;
  \ Add the string _ca len_ to the card;
  \ the length is not saved, only the contents.

: byte>card  ( b -- )
  >card @ c! 1 >card +!  ;
  \ Add _b_ to the card.

 32 KiB constant eprom-card-default-size
512 KiB constant flash-card-default-size

eprom-card-default-size value default-card-size
  \ Default size of the card in bytes.

0 value /card
  \ Size of the card in bytes.

0 value card-header
  \ Address of the card header
  \ (the header is at the end of the file).

64 constant /card-header
  \ Length of the card header in bytes
  \ (the header is at the end of the file).

\ Fields of the card header:
: card-header-1          ( -- ca )  card-header 55 +  ;
: card-header-random-id  ( -- ca )  card-header 56 +  ;
: card-header-size       ( -- ca )  card-header 60 +  ;
: card-header-subtype    ( -- ca )  card-header 61 +  ;
: card-header-oz-id      ( -- ca )  card-header 62 +  ;

: allocate-card  ( -- a )
  /card ?dup 0= if  default-card-size dup to /card  then
  allocate throw  ;
  \ Allocate memory for the card and return its address.

: erase-card  ( -- )
  card /card $FF fill  ;
  \ Erase the card, filling it with $FF.

: erase-card-header  ( -- )
  card-header /card-header erase  ;
  \ Erase the card header, filling it with zeroes.

: calculate-card-header  ( -- )
  card /card + /card-header - to card-header  ;
  \ Calculate the address of the card header.

: random-id  ( -- b1 b2 b3 b4 )
  4 0 do  256 random  loop  ;
  \ Return a card random id: four random bytes.

: store-card-random-id  ( -- )
  random-id card-header-random-id >r
  r@     c!
  r@ 1+  c!
  r@ 2 + c!
  r> 3 + c!  ;
  \ Store the card random id.

: store-card-banks  ( -- )
  /card 16KiB / card-header-size c!  ;
  \ Store the number of 16 KiB banks of the card.

: large-card?  ( -- f )
  /card [ 128KiB 1- ] literal >  ;
  \ Is the card 128 KiB or larger?

: store-card-subtype  ( -- )
  large-card?
  if  $7C  else  $7E  then  card-header-subtype c!  ;
  \ Store the card subtype, depending on its size.

: store-card-oz-id  ( -- )
  'o' card-header-oz-id    c!
  'z' card-header-oz-id 1+ c!  ;
  \ Store the OZ card identifier.

: init-card-header  ( -- )
  erase-card-header  1 card-header-1 c!
  store-card-random-id  store-card-banks
  store-card-subtype  store-card-oz-id  ;
  \ Init the card header, located at the end of the card.

: null-file  ( -- )
  1 byte>card 0 byte>card 0 byte>card
  0 byte>card 0 byte>card 0 byte>card  ;
  \ Create a "null" file, a file entry with a null filename
  \ and zero length.

: format-card  ( -- )
  erase-card init-card-header
  flash-card? if  null-file  then  ;

: create-card  ( -- )
  allocate-card  dup to card  >card !
  calculate-card-header  format-card  ;

: card-needed  ( -- )
  card ?exit create-card  ;
  \ Create a card, if not done yet.

: unused-card  ( -- n )
  card-header >card @ -  ;
  \ Unused space in the card, in bytes.

: save-card  ( -- )
  card /card card-filename@ unslurp-file  ;
  \ Save the card to a file.

: ?card-size-number  ( ca len -- )
  nip 0<> s" Wrong size: not a valid number." ?error-bye  ;
  \ Error if the card size number is wrong.
  \ _ca len_ is the string left by `>number`, and its
  \ length must be zero if the conversion was successful.

: ?card-size-pages  ( n -- )
  16 mod s" Wrong size: it must be a multiple of 16" ?error-bye  ;
  \ Error if the card size _n_ (in KiB) is not a multiple of 16.

: eprom-card-sizes  ( -- x0..xn n )
  32 128 256 3  ;
  \ Return the proper sizes (in Kib) for an EPROM card,
  \ and their count.

: flash-card-sizes  ( -- x0..xn n )
  512 1024  2  ;
  \ Return the proper sizes (in Kib) for a Intel Flash card,
  \ and their count.

: ?card-size-range  ( n -- )
  flash-card? if    flash-card-sizes
              else  eprom-card-sizes  then
  any? 0= s" Wrong size for the card type." ?error-bye  ;
  \ Error if the card size _n_ (in KiB) is not in the range
  \ allowed by the card type.

: check-card-size  ( d ca len -- n )
  ?card-size-number d>s dup ?card-size-pages  dup ?card-size-range  ;
  \ Check if the card size, specified by the `--size` option,
  \ is right.  _d ca len_ are the result of `>number`.
  \ If no error is found, return the size _n_ (in KiB).

: ?no-card-yet  ( -- )
  card s" Invalid option: the card is already created." ?error-bye  ;
  \ Error if the card is already created.

: set-card-size  ( ca len -- )
  ?no-card-yet
  0. 2swap >number check-card-size KiB to /card  ;
  \ Set the card size in KiB, as found in the string _ca len_.

: flash-card  ( -- )
  ?no-card-yet
  true to flash-card?
  flash-card-default-size to default-card-size  ;
  \ Create a Flash card instead of an EPROM.

\ ==============================================================
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

\ ==============================================================
\ Copying a file

: full-card?  ( -- f )
  unused-card file-length <  ;
  \ Is the card full?

: ?space-left  ( -- )
  full-card? s" No space left in the card." ?error-bye  ;
  \ Error if there's no space left in the card.

: valid-char?  ( c -- )
  >r  r@ chr-alnum?
      r@ '-' = or
      r@ '.' = or
      r> chr-ascii? and  ;
  \ Is _c_ a valid filename char?

: ?valid-char  ( c -- )
  dup >r valid-char? 0=
  s" Invalid filename character '" r> c>str s+ s" '." s+ ?error-bye  ;
  \ Error if _c_ is not a valid char.

: ?segment-chars  ( ca len -- )
  bounds ?do  i c@ ?valid-char  loop  ;
  \ Error if any filename char in invalid.

: ?segment-length  ( ca len -- )
  nip /filename > s" Segment too long." ?error-bye  ;
  \ Error if filename length is too long.

: ?segment-dot  ( ca len -- +n | -1 )
  -1 -rot  over swap  ( -1 ca ca len )
  bounds ?do
    i c@ '.' =
    if  over -1 > s" More than one dot in the filename." ?error-bye
        nip i swap - dup  ( +n +n )  then
  loop  drop  ;
  \ Error if filename contains two or more dots.
  \ Return the position _+n_ of the dot (0..len-1), or -1 if
  \ there's no dot.

: ?segment-extension  ( len n )
  - 1- dup 3 > s" Segment extension is too long." ?error-bye
            0= s" Segment extension is empty." ?error-bye  ;
  \ Error if the filename extension is too long or empty; _len_
  \ is the length of the filename and _n_ is the position of the
  \ dot (0...len-1).

: ?segment-without-extension  ( len n -- )
  nip dup 12 > s" Segment without extension is too long." ?error-bye
            0= s" Segment without extension is empty." ?error-bye  ;
  \ Error if the filename without the extension is too long or
  \ empty.  _len_ is the length of the filename and _n_ is the
  \ position of the dot (0...len-1).

: ?segment-format  ( ca len n -- )
  rot drop  dup -1 = if  2drop exit  then  \ exit if no extension
  ( len n ) 2dup ?segment-extension
  ?segment-without-extension  ;
  \ Error if the filename format is not "12.3".
  \ The filename length is supposed to be under 17.
  \ _n_ is the position of the dot (0...len-1); or -1
  \ if there's no dot.

: ?segment  ( ca len -- )
  2dup ?segment-length
  2dup ?segment-chars
  2dup ?segment-dot
       ?segment-format  ;
  \ Error if the segment _ca len_ is invalid.
  \ A segment is each part of a explicit filename divided
  \ by slashes.

: ?filename  ( ca len -- )
  /slash 0 ?do  ?segment  loop  ;
  \ Error if the filename _ca len_ is invalid.

: check-file  ( ca len -- )
  ?filename ?space-left  ;

: /filename  ( -- len )
  filename @  ;
  \ Length of the current input file name.

: filename>card  ( -- )
  /filename 1+ byte>card  '/' byte>card
  filename 2@ string>card  ;
  \ Copy the input file name.

: file-length>card  ( -- )
  file-length dup dup dup
                        256 mod byte>card
                          256 / byte>card
        [ 256 256 * ] literal / byte>card
  [ 256 256 256 * * ] literal / byte>card  ;
  \ Store the length of the current input file on the card;
  \ least significant byte first.

: file-contents>card  ( -- )
  file-contents 2@ string>card  ;
  \ Store the contents of the current input file on the card.

: current-file>card  ( -- )
  filename>card file-length>card  file-contents>card  ;
  \ Store the current input file on the card.

true value verbose?
  \ List the processed input files?

: echo  ( ca len -- )
  verbose? if  type cr  else  2drop  then  ;
  \ Print the string _ca len_, if verbose mode is on,
  \ else discard it.

: adjust-path  ( ca len -- ca' len' )
  s" ./" -prefix  ;
  \ Remove the "./" prefix from filename _ca len_, if present.

: ?file-exists  ( ca len -- )
  2dup file-status s" No such file: " 2rot s+ ?error-bye drop  ;
  \ Error if file _ca len_ does not exist.

: file>card  ( ca len -- )
  2dup ?file-exists
  1 files +!  card-needed
  adjust-path  2dup filename 2!
  2dup get-file  2dup echo
  check-file current-file>card free-file  ;
  \ Copy file _ca len_ to the card, if possible.

\ ==============================================================
\ Argument parser

: KiBs+  ( ca len n -- ca' len' )
  KiB/ n>str s+ s"  KiB" s+  ;
  \ Concatenate the value of _n_ bytes in KiB, including the
  \ unit, to the string _ca len_, giving the result string _ca'
  \ len'_.

\ Program data:
s" mkepr"                              \ name
s" [options] [files]"                  \ usage
version                                \ version
s" Report bugs via programandala.net"  \ extra info
arg-new value mkepr-arguments

\ Add the default help and version options:
mkepr-arguments arg-add-help-option
mkepr-arguments arg-add-version-option

\ Add the -f/--flash option switch:
'f'                                   \ short option
s" flash"                             \ long option
s" create an Intel Flash card instead of an EPROM card"
                                      \ description
true                                  \ a switch
4 dup constant arg.flash-option       \ option id
mkepr-arguments arg-add-option

\ Add the -o/--output option parameter:
'o'                                   \ short option
s" output=FILE"                       \ long option
s" set output file; default: "
  default-eprom-card-filename s+ s"  (or " s+
  default-flash-card-filename s+ s" )" s+

                                      \ description
false                                 \ a parameter
5 dup constant arg.output-option      \ option id
mkepr-arguments arg-add-option

\ Add the -s/--size option parameter:
's'                                   \ short option
s" size=KiB"                          \ long option
s" set the card size; defaults: "
  eprom-card-default-size KiBs+ s"  (EPROM), " s+
  flash-card-default-size KiBs+ s"  (Flash)" s+
                                      \ description
false                                 \ a parameter
6 dup constant arg.size-option        \ option id
mkepr-arguments arg-add-option

\ Add the -q/--quiet option switch:
'q'                                   \ short option
s" quiet"                             \ long option
s" quiet mode: input files will not be listed"  \ description
true                                  \ a switch
7 dup constant arg.quiet-option       \ option id
mkepr-arguments arg-add-option

: show-help  ( -- )
  mkepr-arguments arg-print-help bye  ;

: show-version  ( -- )
  mkepr-arguments arg-print-version  ;

: be-quiet  ( -- )
  false to verbose?  ;
  \ Deactivate the verbose mode.

: option? ( -- ca len n f | n f )
  mkepr-arguments arg-parse  ( ca len n | n )
  dup arg.done <> over arg.error <> and  ;
  \ Parse the next command line option. Return its value _ca
  \ len_, if any; and its number _n_; _f_ is true when the
  \ option is valid.

: option  ( ca len n | n -- )
  case
    arg.non-option      of  file>card       endof
    arg.output-option   of  card-filename!  endof
    arg.size-option     of  set-card-size   endof
    arg.flash-option    of  flash-card      endof
    arg.quiet-option    of  be-quiet        endof
    arg.help-option     of  show-help       endof
    arg.version-option  of  show-version    endof
  endcase  ;
  \ Manage command line option _n_ and its value _ca len_.

: parse-options  ( -- )
  begin  option?  while  option  repeat  ;
  \ Parse and manage all command line options.

\ ==============================================================
\ Boot

' noop is dobacktrace
  \ Don't show the return stack backtrace after an error.

: mkepr  ( -- )
  files off  parse-options
  files @ if  save-card  else  show-help  then  ;

mkepr bye

\ ==============================================================
\ History

\ 2015-12-05: Start. Documentation on the File Card format.
\
\ 2015-12-08: Version 0.0.0. Main structure.
\
\ 2015-12-09: Version 0.0.1. First working version.
\
\ 2015-12-10: Version 0.1.0. New option `--flash` to create
\ Intel Flash cards instead of EPROM cards.
\
\ 2015-12-11: Version 0.2.0. Filenames are checked.
\
\ 2015-12-11: Version 0.3.1.  - Added card size check depending
\ on the card type.  - Fixed the format of file length (only
\ files larger than 16 MiB were affected).  - Fixed the default
\ size of Flash cards.  - Removed the check of maximum file
\ length.  - Improved the help.
\
\ 2015-12-15: Version 0.3.2. Added the explanation about the
\ "null" file required by Intel Flash cards and renamed one word
\ accordingly.
\
\ 2015-12-16: Version 0.4.0. Added support for directories.  No
\ backtrace shown on errors.
\
\ 2015-12-17: Version 0.4.1. No internal information shown on
\ errors. Missing files are detected, right at the start.
\ Improved error on invalid characters. Fixed and improved
\ installation instructions.

\ vim: tw=64
