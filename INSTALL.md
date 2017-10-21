Assuming you have Haskell installed on your system, you can try this program by running the following commands in your bash terminal:

``` bash
$ git clone https://github.com/rachel-barrett/nim-haskell
$ cd nim-haskell
$ cabal sandbox init #  to create a sandbox
$ cabal install # to install into the sandbox
$ .cabal-sandbox/bin/nim # to run the executable
```

(The install step takes about 20 seconds.)

Once you have finished and want to remove the program from your system, you can simply remove the directory nim-haskell. 



