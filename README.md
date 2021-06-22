# Awesome MOC

This repository contains notes and tools that might be helpful when you are studying MoC (COMP30026 Models of Computation).:100:

## Prerequisites

### Haskell

[Haskell](https://www.haskell.org/) is used thoughout this subject. Haskell is a general-purpose, statically typed, **purely functional programming language** with type inference and lazy evaluation. 

#### Where do I learn Haskell?

- School

  - [COMP90020](https://handbook.unimelb.edu.au/2021/subjects/comp30020) or [COMP90048 (master equivalent)](https://handbook.unimelb.edu.au/2021/subjects/comp90048) Declarative Programming 
  
  **Declarative Programming** is not the official prerequisite for taking this subject but it is **highly recommended** to take it before taking the Models of Computation. All the coding material in this subject will be in Haskell and there is no specific time in the lectures to teach you how to write Haskell code.

- Online

  - [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters) (The best Haskell tutorial exist!)
  
## Useful Tools

### Local Grok

#### Get Started

- Clone this repository

- run 

  ```
  cd awesome-moc/Grok && ghci Main
  ```

#### Some useful functions implemented with Haskell

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


### [Logic Calculator](https://www.erpelstolz.at/gateway/formular-uk-zentral.html)

- **Truth Table**
- **CNF/DNF Conversion**

### In the following three tools, 'or' is represented by '+', not '|'

### [Simpilify Regex](http://ivanzuzak.info/noam/webapps/regex_simplifier/)

- Produce **minimal regex** of a given regex **with steps**

### [FSM Simulator](http://ivanzuzak.info/noam/webapps/fsm_simulator/)

- **Draw minDFA/NFA** (including dead state)

### [FSM2Regex](http://ivanzuzak.info/noam/webapps/fsm2regex/)

- **Convert a given DFA/NFA to corresponding regex**
- The output regex is **not minimal**, please use the simpilify tool to simpify further

### [CFG Developer](https://web.stanford.edu/class/archive/cs/cs103/cs103.1156/tools/cfg/)

- Verity if a string is in a CFG
- **CFG ambiguity check**

------

## Contribution

> **All contributions are welcome! Add more resources here that helps your study MOC!**
