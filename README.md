
GitHub - https://github.com/armanjindal/haskell-reg-exp-derivatives

This project is motivated by Janus A. Brzozowski's paper on [Derivatives of Regular Expressions](https://dl.acm.org/doi/10.1145/321239.321249). In it, he presents the idea of a derivative of a regular expression which provides a faster and more elegant way to construct recognizers (DFAs/NFAs) for regular expressions than the more widely used algorithms which goes from a Regex -> GNFA -> NFA -> DFA, which can cause an exponential blow-up in states, using Brzozowski derivatives and the algorithm he presents, one can go from Regex -> DFA.  This, however, can also include an exponential blow of states which I will discuss later. 

The purpose of this report and class project for Types Languages and Compilers is educational. I am new to both Haskell and the world of Types, Languages, and Compilers. As such took it upon myself to implement the first part of the paper, which explains how derivatives can be used to match regex against specific strings.  Combined with a parser, this test displays the fully functional power of what I have built. 

```
let regex = Star (Plus (Ch 'a') (Ch 'b'))

Prelude.map (regex ~~) ["a", "aa", "ab", "bbb", "aaaaaaaaaaaaaaab", "c", "caaaababababbabad"]

`shouldBe` [True, True, True, True, True, False, False]
```

The function ~~ :: Regex -> String -> Bool, follows the algorithm in 3.2, which takes the first letter of the string (s:ss) and takes the derivative of regex with respect to s. This process is repeated until all the letters of the string are exhausted. If the remaining RegExp expresses a language that contains the empty string (Epsilon), which is to say it is nullable, then the string is part of the language of the regex.

In essence we are using these two functions (**deriv** and **nullable**) to check if a regex matches a string: 

Where r :: Regex, a :: String 

```
r ~ ε ⇔ nullable(r) = True
r ~ a · w ⇔ (deriv a r) ~ w
```

Example: 
```
a⋅b∗​∼abb⟺
∂a​(a⋅b∗)∼bb⟺
b∗∼bb⟺
∂b​(b∗)∼b⟺
b∗∼b⟺
b∗∼b⟺
∂b​(b∗)∼ε⟺
b∗∼ε⟺
nullabe (b∗)=ε True
```


Since this is my first time working with Haskell on a functional programming project of this size, I focused on understandability over complexity. I included in this read-me explanation of what I did and plenty of unit tests that help illustrate how the program works, at each step. This work draws heavily and follows both notation and naming from a paper by S. Owens, J. Reppy, A. Turon called [*Regular-expressions derivatives reexamined*](https://www.ccs.neu.edu/home/turon/re-deriv.pdf) and on the implementation strategy of [Professor Michael Greenberg](https://cs.pomona.edu/~michael/)of Pamano College, who graciously live streamed on Twitch the development and process of implementing some of the formalism in the paper, which proved invaluable for a novice Haskell developer. I based much of the architecture and code of this project on his presentation. 

This was built using the Haskeleton framework and can be built using `stack build`.

`stack ghci` for interactivity

`stack test` to run the unit test, located in /test-suit/Main.hs

# CLI Usage: 
```
parseDef :: String -> Regex (parses a String into a regex)
(~~) :: Regex -> String 

ex:
stack ghci
ghci> let test = parseDef "((ab)*)~"
ghci> :t test
test :: RegExp
ghci> test ~~ "ccdd"
True
ghci> map (test ~~) ["cc", "ab", "abab", "def"]
[True,False,False,True]
```

## Below is a breakdown of the key steps of the project:

1. Representing the Regular Expression as a recursive Abstract Data Type

```
data RegExp = EmptySet

| Epsilon

| Ch Char

| Plus RegExp RegExp

| Concat RegExp RegExp

| Not RegExp

| Star RegExp

| And RegExp RegExp

deriving (Eq, Ord)
```

2. Parser and Show for Regular Expression

The next major challenge was to parse the command line input into this object format. Using the standard parser Text.Megaparsec, based on examples and from the third homework. Understanding and getting this to work took up a lot of the focus of the project, as it also forced me to try to understand how monads work.  I didn't fully understand them or the library, which is why I couldn't get the not operator to be parsed on the left side, so I chose to parse where it binds to the right "b~" = Not ( Ch ('b'))

3. Derivative and Nullable

Implementing these was quite straightforward and followed very logically from the paper. This is the benefit of coding in Haskell and with the ADT.  I worked through a few examples to understand why each rule was what it was. The most confusing of the rules, mathematically is for concatenation. If the first part of the concat (s.a) is nullable it is possible that an epsilon in a string captures it, and in this case, we need to also take the derivative with respect to a. This condition:

```
deriv a (Concat b c) = Plus (Concat (deriv a b) c) (if nullable b then (deriv a c) else EmptySet)
```


4. The ~~ Function 
Looking at the slides on RegExp from class, the method there, which was to brute force the string matching proof on the basis of the judgment rules of regex was clearly less efficient than taking the partial derivative successively on the characters of the string and checking if the resulting regex is nullable (which is to say if it recognizes the epsilon). Haskell has a wonderfully concise way of implementing this:

```
(~~) :: RegExp -> String -> Bool
(~~) regex [] = nullable regex
(~~) regex (s:ss) = (deriv s regex) ~~ ss
```

It applies the derivative of the first character onto the regex recursivley untill the string is empty, and then checks if the resulting regex is nullable!

5. Testing using Tasty and Hspec Framework

In addition to testing, the simple unit helps document how the code is used and the objects internally represented. So far, the tests focus on:
- Pretty printing regex's (without parenthesis)
- Parsing the regular expressions
- Taking derivatives of regular expressions
- Matching regular expressions against lists of string


6. Building the general DFA for a Regex (DFA -> Regex)
This was the stated aim of the proposal. The algorithm's logic, which is to take the derivative of the regex with respect to every symbol in the alphabet and have the resulting regex be a transition (current, c, deriv c current) is very logical. However, I have faced challenges in coding and am still working to get this working. 

6A. A future step is using the weaker notion of equivalence between regexs. This is something that Professor Greenberg calls "smart constructors" which essentially reduce simple regexs we know to be equivalent. I implemented this by overloading the value constructors for regex. Below is an example for `Concat`:

```
concat :: RegExp -> RegExp -> RegExp
concat Epsilon b = b
concat a Epsilon = a
concat EmptySet b = EmptySet
concat a EmptySet = EmptySet
concat a b = Concat a b
```

6B. Visualize DFA
Recognizing that this is not production-grade, and educational, the next steps for this are to use the tool [GraphViz](https://graphviz.org/) to create a .gv file, which can then be viewed. Visualizing the RegEx -> DFA conversion and seeing the code, will help future CS students appreciate and understand elegance of Brzozowski derivatives. 

6C. U
The next step would then to be to use the [automata](https://github.com/andrewthad/automata/tree/master) library to implement the DFA, so it could be a valid recognizer. The combination of visualization and implementation may prove useful for people actually using these techniques for scanner generators and for building compilers.
