
The front-end part of the compiler must perform the following checks:
- Verify if the input program belongs to the language
- Verify if the input program is semantically correct
- Build an IR version of the code that is semantically equivalent.

The front-end is divided into the scanner and the parser:
- The scanner reads every word of the input program and return tokens, that can be seen as the elementary components, or keywords, of the language.
- The parser applies grammatical derivations to check if the stream of token is a valid program.
This division is also due to the hardness and slowness of parsing.

Scanning deals with regular expressions, implemented as DFA, while parsing deals with context-free grammars, implemented through pushdown-automata. Both performs transitions in the corresponding automata.

Considering lexical analysis, a lexeme is a sequence of characters included in the source program that match a specific token.

Scanners can be built by hand, identifying pairs lexeme-token, or automatically, compiling patterns corresponding to a lexeme, expressed as regular expressions.

Given the regular expression for a specific token, a corresponding DFA can be built, so that the token can be recognized by reading the input and verifying if a final state is reached. This transition can be encoded through a table indexed by pairs state-character, giving a constant time cost for each transition. Transactions can also be associated with action (i.e. capture the current lexeme)

Tighter specification implies more complex regex, leading to greater tables.

To implement a scanner, the precise steps are:
- Define the regex for the input language
- Define the corresponding $\epsilon$-NFA
- Turn the $\epsilon$-NFA into an equivalent NFA
- Compute the equivalent (minimal) DFA
- Turn it into code by encoding it as a table
Alternatively, the DFA can be simulated with direct-code scanners and hand-coded scanners, as they only differs in how they implement the transition table and how they simulate the DFA operations.

Generally, they continuously read the next character simulating the corresponding DFA transitions, until no outgoing transitions are possible with the current input character. If the current state is final, then the scanner recognise the lexeme and returns the corresponding token, otherwise it must check if previously a final state was reached: if yes, it must rollback to that state, restoring the input stream, otherwise it should return failure.

Considering table-driven scanners, the size of the table can be shrunk by combining characters into equivalent categories, meaning that they cause equivalent transitions. To perform the rollback, the lexeme must be saved, but also the past states, that they are pushed onto a stack whenever a transition is performed.

Since we want a single DFA, all regexes must be combined into one, but this allows strings to fit into different syntactic categories, so the scanner must disambiguate. Typically regexes are ordered, and a string is assigned to the first matching regex. 