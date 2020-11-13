# HackMOC

This repository contains revision notes and some useful resources that might be helpful when doing COMP30026 questions.

## Local Grok

#### Prerequisite

**Haskell** https://www.haskell.org/downloads/

#### Get Started

- Clone this repository

- Go to the directory that contains this repository

- run 

  ```
  cd Grok && ghci Main
  ```

#### Some useful functions

- complementDFA
- productDFA
- reverseDFA
- minimiseDFA

#### Notations

```haskell
dfa1 :: DFA
dfa1 = ([1,2,3,4], "ab", t, 1, [1,2])
    where
        t = [ ((1,'a'),2)
            , ((1,'b'),4)
            , ((2,'a'),2)
            , ((2,'b'),3)
            , ((3,'a'),4)
            , ((3,'b'),2)
            , ((4,'a'),4)
            , ((4,'b'),4)
            ]
```

------

## Useful Tools

#### [Logic Calculator](https://www.erpelstolz.at/gateway/formular-uk-zentral.html)

- **Truth Table**
- **CNF/DNF Conversion**

### In the following three tools, 'or' is represented by '+', not '|'

#### [Simpilify Regex](http://ivanzuzak.info/noam/webapps/regex_simplifier/)

- Produce **minimal regex** of a given regex **with steps**

#### [FSM Simulator](http://ivanzuzak.info/noam/webapps/fsm_simulator/)

- **Draw minDFA/NFA** (including dead state)

#### [FSM2Regex](http://ivanzuzak.info/noam/webapps/fsm2regex/)

- **Convert a given DFA/NFA to corresponding regex**
- The output regex is **not minimal**, please use the simpilify tool to simpify further

#### [CFG Developer](https://web.stanford.edu/class/archive/cs/cs103/cs103.1156/tools/cfg/)

- Verity if a string is in a CFG
- **CFG ambiguity check**

------

## Addtional Resources

> **Add more resources here that help your revision**