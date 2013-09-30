nono.erl
========

Nonograms solver in Erlang + C. Not a dumb bruteforcer!

Requires **Erlang R16B**+ ang **GCC 4.x** for compilation. To build just run ``make``. If you've got an error from GCC, update Makefile with correct Erlang libraries directory path in your system.

Beware, some input sequences may use a lot of RAM to get solved!


Usage
-----

Is simple as possible.

[Apple](https://raw.github.com/Mendor/nonographics/master/img/apple.png):


```
1> H2 = [[6], [8], [10], [1, 11], [2, 11], [3, 11], [2, 11], 
        [12], [14], [3, 11], [2, 11], [3, 6], [2, 4], [2, 4], 
        [6]].
[[6],
 "\b","\n",
 [1,11],
 ...
 [6]]
2> V2 = [[3, 2], [3, 3], [5], [2], [11], [13], [12, 2],
        [11, 1], [11, 1], [12, 2], [15], [15], [13], [11],
        [4, 4]].
[[3,2],
 [3,3],
 [5],
 ...
 [4,4]]
3> nono:pp(nono:solve(H2, V2)).
...XXX...XX....
....XXX.XXX....
.....XXXXX.....
.......XX......
..XXXXXXXXXXX..
.XXXXXXXXXXXXX.
XXXXXXXXXXXX.XX
XXXXXXXXXXX...X
XXXXXXXXXXX...X
XXXXXXXXXXXX.XX
XXXXXXXXXXXXXXX
XXXXXXXXXXXXXXX
.XXXXXXXXXXXXX.
..XXXXXXXXXXX..
...XXXX.XXXX...
[ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok]

```


[Eagle](https://raw.github.com/Mendor/nonographics/master/img/eagle.png):

```
1> H8 = [[6, 6, 7], [6, 4, 5, 1], [4, 1, 3, 5, 3],
        [3, 2, 4, 4], [2, 1, 3], [1, 2, 4], [2, 2, 1, 6],
        [2, 6, 3], [2, 12], [2, 12], [2, 7], [2, 2, 8],
        [2, 1, 4, 5], [2, 1, 3, 3], [2, 3, 1, 2], [2, 9],
        [1, 1, 1, 2, 2], [2, 2, 1, 2], [2, 1, 1, 1],
        [2, 2, 3], [3, 3], [2, 2, 2], [2, 1, 4], [2, 2],
        [6]].
[[6,6,7],
 [6,4,5,1],
 [4,1,3,5,3],
 ...
 [6]]
2> V8 = [[7], [11], [3, 3], [3, 3], [4, 2], [3, 2],
        [2, 1, 2], [3, 6, 1], [2, 2, 1, 2], [1, 1, 6, 1],
        [2, 2, 1, 2], [3, 2, 2, 2], [3, 2, 4, 1, 1],
        [2, 4, 1, 1, 2, 2, 1], [2, 12, 2, 1], [1, 1, 15, 1],
        [1, 2, 2, 6, 2, 1], [5, 5, 1, 1], [4, 5, 3],
        [3, 6, 2], [3, 1, 5, 1], [2, 1, 2, 2, 2],
        [1, 5, 1, 2], [1, 5, 1, 2], [7, 1, 2]].
[[7],
 "\v",
 [3,3],
 ...
 [7,1,2]]
3> nono:pp(nono:solve(H8, V8)).
......XXXXXXX............
....XXXXXXXXXXX..........
..XXX........XXX.........
.XXX...........XXX.......
XXXX.............XX......
XXX...............XX.....
XX.........X.......XX....
XXX........XXXXXX...X....
XX............XX.X.XX....
X..X..........XXXXXX.X...
..XX...XX......X.....XX..
XXX....XX......XX.....XX.
XXX....XX.....XXXX...X.X.
XX....XXXX..X..X..XX.XX.X
XX....XXXXXXXXXXXX.XX...X
X..X...XXXXXXXXXXXXXXX..X
X.XX.XX.XXXXXX......XX..X
.XXXXX.XXXXX..........X.X
XXXX...XXXXX..........XXX
XXX...XXXXXX..........XX.
XXX...X.XXXXX.........X..
XX.X.XX.XX.XX............
X.XXXXX..X..XX...........
X.XXXXX..X..XX...........
XXXXXXX..X..XX...........
[ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,
 ok,ok,ok,ok,ok,ok]
```

Follow [tests.txt](https://github.com/Mendor/nonographics/blob/master/tests.txt) file for more test sequences or write your own.


License
-------

[WTFPL](http://sam.zoy.org/wtfpl/)
