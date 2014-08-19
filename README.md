dvorak practice
======

I've learning the alphanumeric Dvorak key mappings using learn.dvorak.nl, which offers problem sets made up of english words. I greatly prefer this to the standard method of typing random sequences of characters, but as a programmer I can't switch over to Dvorak before I'm somewhat productive using it, which means memorizing the new locations of -]=',/;{_}+\"<>?:[. I initially considered mining github repos for practice text, but I don't *want* plentiful comments and descriptive function names. In fact, it doesn't actually need to compile. What would be ideal is a sort of pidgin Scala, chock full of operators, brackets and with no type or val name longer than, say, two characters.

Something like this:

```scala
│   nd[Ur].Ea.Ig[Be]({                                                                                                                                                                                   │
│     case Ee.sh('b', "on") => Nt.ut[Pe].To[Lo]                                                                                                                                                          │
│     case "si" => ti[Se].sh[Ed].ma                                                                                                                                                                      │
│   } >>= Ca[Ro].Ig[Om])                                                                                                                                                                                 │
│   ut[Pe]                                                                                                                                                                                               │
│   it                                                                                                                                                                                                   │
│   li.si[De].ca                                                                                                                                                                                         │
│   It[La].us[Me].at({                                                                                                                                                                                   │
│     case "it" => on[So](di.ge[Ow].Di(Ha[Ai].ca[Ma]))                                                                                                                                                   │
│     case 'b' => Ng.Sa                                                                                                                                                                                  │
│   })
```

This project generates scala dvorak problem sets using QuickCheck's Arbitrary typeclass, displays them in the terminal using curses, and wraps the whole thing together with a tiny DSL built using the Control.Monad.Free which lets me keep a healthy distance between the core program and NCurses, the GUI library. Check it out!
