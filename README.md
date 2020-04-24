Regular expressions are defined by the following rules:
1. The string "0" is a regular expression denoting the empty language.
2. Any character a that is NOT "\", "(", ")", "0", "+", "*", or "λ" is a regular expression denoting the language {a}. The strings "\\", "\(", "\)", "\0", "\+", "\*", and "\λ" are the regular expressions denoting the languages {"\"}, {"("}, {")"}, {"0"}, {"+"}, {"*"}, and {"λ"} respectively.
3. If "S" and "T" are regular expressions denoting languages L(S) and L(T), then "S+T" is a regular expression denoting the union of L(S) and L(T). Note that this sense of "+" in formal language theory is different regular expression.
4. If "S" and "T" are regular expressions denoting languages L(S) and L(T), then "ST" is a regular expression denoting the concatenation product of L(S) and L(T). The concatenation product of languages A and B is the language of all strings uv such that u is in A and v is in B.
5. If "S" is the regular expression denoting the language L(S), "S*" is a regular expression denoting the Kleene star of L(S). The Kleene star A* of a language A is defined as follows:
  1. LAMBDA is in A*.
  2. If u is in A* and v is in A, then the concatenation uv is in A*.
  3. The only strings in A* are those given by the above rules.
6. The only regular expressions are those defined by the above rules.
