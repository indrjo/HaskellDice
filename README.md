# HaskellDice

Command-line dices written in Haskell.

## ```mnemosyne.hs```

### Intro

This program is a dice, and basically does what follows:
* read a plain-text file, which is intended to be a list
* perform a random extraction and record how many times every element is extracted so far
* tell to the world outside what's going on.
  
  
### Before you continue...
  
This unique file contains all the lines code you need for a working program: compile and use it or simply interpret it. Yet, you may need to intall some Haskell modules before: please run

```
$ cabal install extra
$ cabal install FindBin
```
    
### Necessary files
    
The unique file needed is the one that contains the list from which you want to randomly extract. By default, the program looks for a file called `list.dice` in the same directory where lies the executable. You can change that behaviour, from command-line:

```
$ ./mnemosyne --use /PATH/TO/THIS/FILE
```

The file to use has a simple stucture: all you need to do is to write one name per line. When you run the program, something occurs: if you open the list-file, you will notice the name of the extracted element is followed by 1, while other names by 0. Running the program again, will update the counters: if an element is extracted, then its counter is increased by 1, otherwise it remains as it was before the last run.

## ```lethe.hs```

*Documentation yet to come...*

## License

You are free to modify and share your changes, under whatever name, authorship and condition.

