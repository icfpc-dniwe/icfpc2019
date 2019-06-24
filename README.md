```
    ___    _  _     ___  __      __ ___               _       _
   |   \  | \| |   |_ _| \ \    / /| __|     o O O   (_)     (_)      o      __ _
   | |) | | .` |    | |   \ \/\/ / | _|     o         _       _      o      / o  |
   |___/  |_|\_|   |___|   \_/\_/  |___|   TS__[O]  _(_)_   _(_)_   TS__[O] \__,_|
 _|"""""|_|"""""|_|"""""|_|"""""|_|"""""| {======|_|"""""|_|"""""| {======|_|"""""|
 "`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'./o--000'"`-0-0-'"`-0-0-'./o--000'"`-0-0-'

                                 Chasing bottoms!
```

## Quick start

To run the solver you need Stack (Haskell build tool).

> stack run -- problems/prob-001.desc solutions/prob-001.sol

This produces a file `solutions/prob-001.sol` with worker-wrapper commands.

To run the miner you need Python with NumPy.

> python3 python/miner/createTask.py "puzzles/puzzle1.cond" "puzzles/puzzle1.desc"

This produces a file `puzzles/puzle1.desc` with generated task.

## Description

The end result is an A-star based solver with limited depth and a bunch of
heuristics - for example wall and booster proximities, moving out of dead ends,
wing-like manipulators etc.

Mining task generation was based on connecting pre-assigned points into one
connected structure. Task generated this way was always too simple (number of
vertices in polygon < vMin) thus additional obstacle points were chosen on the
image plane. The whole process was iterated until needed difficulty was
achieved.

## Fun parts

+ fast-downward based solver (slow!);
+ Medial axis based solver, with calls into Python from Haskell;
+ domjs-based wrapper of your website-based checker to automatically verify solutions.
  Check out horrible things;
+ Bugs in manhattan metric aren't fun at all;
+ Really uses raytracing to determine one point's center visibily from another one's.

## Feedback

+ The task was nice and allowed for many different approaches. We had great fun;
+ UI-based checker without any automation capabilities isn't cool. We had a
  workaround but a proper batch testing tool would have been much better;
* Better logging in checker and visualizer would have been much appreciated. It
  was difficult to determine exact step at which error occured and what exactly
  was violated;
+ Lambda-chain is somewhat against the spirit of ICFPC as we know it - a
  contest to the very end. Because the mining process starts early teams with
  good solvers get only better and slow teams (like us :D) get worse. Just like
  in a capitalistic world!
