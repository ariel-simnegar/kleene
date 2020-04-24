# kleene.js: A Proof-of-Concept of Kleene's Theorem

**All code written by [Ariel Simnegar](https://www.linkedin.com/in/ariel-simnegar/). Mathematical concepts and algorithms due to [Professor David Mix Barrington](https://people.cs.umass.edu/~barring/)'s "A Mathematical Foundation for Computer Science" (MFCS).**

Kleene's Theorem is a result in formal language theory which states that a language is denoted by a regular expression if and only if it is decided by a DFA. kleene.js is a mathematical proof-of-concept of Kleene's Theorem. The script emulates regular expressions and finite state automatons (FSAs) from *λ*-NFAs to DFAs, providing a suite of methods for conversion between the mathematical concepts and deciding whether or not a string is in a regular language.

The mathematical model for regular expressions in kleene.js is defined by the following rules:
1. The string `0` is a regular expression denoting the empty language.
2. The string `λ` is a regular expression denoting the language {*λ*} where *λ* is the empty string "".
3. Any character *`a`* that is NOT `\`, `(`, `)`, `0`, `+`, `*`, or `λ` is a regular expression denoting the language {"*a*"}. The strings `\\`, `\(`, `\)`, `\0`, `\+`, `\*`, and `\λ` are the regular expressions denoting the languages {"\"}, {"("}, {")"}, {"0"}, {"+"}, {"\*"}, and {"λ"} respectively.
4. If *`S`* and *`T`* are regular expressions denoting languages *L(S)* and *L(T)*, then *`S+T`* is a regular expression denoting the union of *L(S)* and *L(T)*. Note that this sense of `+` in formal language theory is different from the usual sense indicating one or more occurences of the preceding regular expression.
5. If *`S`* and *`T`* are regular expressions denoting languages *L(S)* and *L(T)*, then *`ST`* is a regular expression denoting the concatenation product of *L(S)* and *L(T)*. The concatenation product of languages *A* and *B* is the language of all strings *uv* such that *u* is in *A* and *v* is in *B*.
6. If *`S`* is the regular expression denoting the language *L(S)*, then *`S*`* is a regular expression denoting the Kleene star of *L(S)*. The Kleene star *A\** of a language *A* is defined as follows:
   1. *λ* is in *A\**.
   2. If *u* is in *A\** and *v* is in *A*, then the concatenation *uv* is in *A\**.
   3. The only strings in *A\** are those given by the above rules.
7. The only regular expressions are those defined by the above rules.

The above rules are the rules for regular expressions in formal language theory (MFCS 5.1.1) with a slight extension:
1. The special characters `(`, `)`, `0`, `+`, `*`, and `λ` can now be escaped to refer to their respective strings.
2. `λ` now formally denotes the language containing only the empty string instead of being an informal substitute for the explicit formula `0*`.

kleene.js supports three ways to build regular expressions:
1. A regular expression can be built by parsing any string conforming to the above syntactic rules.
```javascript
const re = RegularExpression.fromString('(ab)*');
```
2. For any FSA, a regular expression can be built that denotes the language that is decided by the FSA. The state elimination algorithm used by kleene.js for this purpose does not guarantee that the built regular expression will be minimal.
```javascript
const re = fsa.toRegularExpression();
```
3. A regular expression can be built by explicitly constructing its abstract syntax tree.
```javascript
const re = new RegularExpression('star',
    new RegularExpression('cat',
        new RegularExpression('char', 'a'),
        new RegularExpression('char', 'b')
    )
);
```

kleene.js supports several regular expression operations:
1. `re.toString()` returns a string `s` such that `RegularExpression.fromString(s)` produces an equivalent regular expression.
2. `re.toLambdaNFA()`, `re.toNFA()`, and `re.toDFA()` return the respective FSAs which decide the language denoted by the regular expression.
3. `re.decide(s)` returns a boolean indicating whether or not `s` is in the regular expression's language. Since this method works by building a DFA that decides the regular expression's language and processing `s` through the DFA, it is often faster to cache `re.toDFA()` and call that DFA's `decide(s)` method instead.

In kleene.js, the mathematical model for an FSA consists of an input alphabet *Σ*, a finite set *S* of states, an initial state *ι* in *S*, a set of final states *F* where *F* is a subset of *S*, and a set of transitions *(p, c, q)* where *p* and *q* are in *S* and *c* is a string in *Σ\** (MFCS 14.1). kleene.js includes three variants of FSAs: *λ*-NFAs, NFAs, and DFAs.

kleene.js supports three ways to build FSAs:
1. An FSA can be built by building the corresponding regular expression and converting it to an FSA. The FSA's alphabet will include every base case character in the regular expression and every character in the method's optional set argument.
```javascript
const lnfa = re.toLambdaNFA();
const nfa = re.toNFA();
const dfa = re.toDFA(new Set(['a', 'b'])); // DFA's alphabet will include "a" and "b" even if not present in re
```
2. NFAs can be built from *λ*-NFAs and DFAs can be built from *λ*-NFAs and NFAs.
```javascript
const nfa = lnfa.toNFA();
const dfa = nfa.toDFA();
```
3. An FSA can be built by explicitly giving its mathematical model.
```javascript
const dfa = new DFA(
    new Set(['a', 'b']),             // Input alphabet
    [s1, s2, s3],                    // Set of states
    s1,                              // Initial state
    [                                // Transitions
        new Transition(s1, 'a', s2),
        new Transition(s1, 'b', s3),
        new Transition(s2, 'a', s3),
        new Transition(s2, 'b', s1),
        new Transition(s3, 'a', s3),
        new Transition(s3, 'b', s3)
    ],
    [s1]                             // Set of final states
);
```

kleene.js supports several FSA operations:
1. `fsa.toString()` returns a string representation of the FSA's mathematical model. State numbers are assigned based on a depth-first search of the FSA's graph with transitions selected in lexicographical order.
2. `fsa.toRegularExpression()` returns a regular expression that denotes the language decided by the FSA. The state elimination algorithm used by kleene.js for this purpose does not guarantee that the built regular expression will be minimal. For example, `RegularExpression.fromString('(ab)*').toDFA().minimize().toRegularExpression()` gives the regular expression `λ+a(ba)*b`, where it took me some time to realize the answer was correct if humourously suboptimal.
3. `dfa.minimize()` returns a minimal DFA that decides the DFA's language.
4. `fsa.decide(s)` returns a boolean indicating whether or not `s` is in the FSA's language. If the FSA is not already a DFA, this method works by building a DFA that decides the FSA's language and processing `s` through the DFA, so it is often faster to cache `re.toDFA()` and call that DFA's `decide(s)` method instead.
