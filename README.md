# HamP-Free

HamP-Free is a free remote control MP3 jukebox based on mpg321. It reads your music library and communicates over TCP. Discovery is based on a UDP protocol. It is written in Haskell, thus the "Ham".

## Disclaimer

This was a fun project to discover Haskell's boundaries and advantages in highly IO-/sideeffect-bound scenarios.
It is poorly maintained.

## Installation

Given there is an instance of mpg321 or mpg123 installed on your system, setup is even more straight forward now with Stack!

Run:

~~~bash
$ stack build
~~~

To then start HamP-Free just run:

~~~bash
$ stack run -- [args]
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

The Java-Library containing everything to communicate with this app is [sharedham](https://github.com/pinselimo/sharedham).

