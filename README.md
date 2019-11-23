# HamP-Free

HamP-Free is a free remote control MP3 jukebox based on mpg321. It reads your music library and communicates over TCP. Discovery is based on a UDP protocol. It is written in Haskell, thus the "Ham".

## Disclaimer

This was a fun project to discover Haskell's boundaries and advantages in highly IO-/sideeffect-bound scenarios.
It is poorly maintained.

## Installation

Given there is an instance of mpg321 or mpg123 installed on your system, setup is straight forward.

Run:

~~~bash
$ cabal v2-configure
$ cabal v2-build
~~~

To then start HamP-Free just run:

~~~bash
$ cabal v2-run HamP-Free -- [args]
~~~

The only argument taken is the filepath to the music library: \
```-c``` takes the current path \
```Folder``` accesses current path + Folder \
```/total-path``` accesses total path

Music library should be organised in a structure like:
Artists / Albums / Songs.mp3

<!-- 
### OS X (macOS)

~~~bash
Give an example
~~~ -->

## Remote control

The accompanying Android app will be released in the near future,
together with its Java library to handle the communication with HamP-Free.
