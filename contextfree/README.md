# Context-free grammars

From your fork of the repository, go to the `contextfree` directory and create a new project with the following command:
```
dune init project contextfree
```

Executing the command will preserve predefined files in [bin](bin/), [lib/](lib) and [test/](test/) folders. 
Like in previous exercises, you need to open [test/dune](test/dune) and ensure that its content is exactly as follows:
```ocaml
(library
 (name contextfreetest)
 (inline_tests)
 (preprocess
  (pps ppx_inline_test))
 (libraries contextfree))
```

Running `dune build` after performing this check should return silently.

## Formalizing a grammar in OCaml

Recall the informal definition of a grammar, which is the product of the following ingredients:

- a set of non-terminal symbols `V`
- a set of terminal symbols `A`
- a set of productions `P`
- a start symbol `S`

Context-free grammars can easily be formalized in OCaml with types only. We have defined these types in [lib/types.ml](lib/types.ml). You must not edit this file.

Now let's go over each type that makes up a grammar.

First, we need a few constructors to represent non-terminal symbols:
```ocaml
type symbol = A | B | S
```

To represent terminal symbols we use the type `char`:
```ocaml
type terminal = char
```

A _sentential form_ is a word made up of a mixture of terminals and non-terminals. In OCaml, this sort of datatype can be achieved by composing a sum type and a list type.

```ocaml
type symbol_or_terminal = Symbol of symbol | Terminal of terminal

type sentential_form = symbol_or_terminal list
```

In a context-free grammar, a production maps a single non-terminal symbol to a sentential form. We encode this relation with a product type where the first component is `symbol` and the second component is a `sentential_form`:
```ocaml
type production = symbol * sentential_form
```

We now combine these types to describe a context free grammar to OCaml. We model this collection with a record type, using lists in place of sets:
```ocaml
type grammar = {
  symbols : symbol list;
  terminals : terminal list;
  productions : production list;
  start : symbol;
}
```

An example of a value of `grammar` is:

```ocaml
let todo : grammar = {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [
        S --> "0S0";
        S --> "1S1";
        S --> "";
      ];
    start = S;
  }
```

This grammar is already defined in [lib/exercises.ml](lib/exercises.ml), and it represents the language of words over {0,1} where the second half equals the first half reversed. A few words generated by this grammar are the empty word, 00, 0110 and 1101001011.

## Deriving words

A grammar can generate words of a certain language by iteratively applying its
productions to the start symbol. The grammar is said to _derive_ the words of the language.

The derivation logic is implemented in the file [lib/grammar.ml](lib/grammar.ml). You don't need to understand the code inside it, but it defines a function that you'll need to use to complete the exercises, called `derive`:
```ocaml
derive : grammar -> int list -> sentential_form
```

The type tells us that `derive` takes a grammar and a list of integers and returns a list of symbols and terminals. The `int list` argument represents the sequence of productions that are to be applied in order to generate the word, where each number represents the index of the desired production in the list field `productions` of the grammar.

Beginning from the start symbol of the grammar, `derive` applies every production in the list on top of the result of the previous one until the list is exhausted.

For example, here's how the word 1101001011 can be derived for the grammar above using `derive`:
```ocaml
derive todo [1; 1; 0; 1; 0; 2];;
```

Running this code in `dune utop` returns a list of terminals:

```ocaml
[Terminal '1'; Terminal '1'; Terminal '0'; Terminal '1'; Terminal '0';
 Terminal '0'; Terminal '1'; Terminal '0'; Terminal '1'; Terminal '1']
```

This can be quite hard to read, so we have provided an auxiliary function `string_of_sentform` to convert it to a string. You can apply it to the output of `derive` with the pipe `|>` operator:
```ocaml
derive todo [1; 1; 0; 1; 0; 2] |> string_of_sentform;;
```

Running it in utop yields the string we're after:

```ocaml
"1101001011"
```

## Front end

To simplify the process of testing your grammars and get a better idea of what words they generate, the project also provides a command that draws random words from the grammar. Its behavior is defined in [bin/main.ml](bin/main.ml). You can run it with:

```ocaml
dune exec contextfree
```

It will print a random list of productions and the word obtained by applying them in a leftmost derivation every half of a second. This is what it looks like:

```
Exit with Ctrl+C
Generating words...

(S->0S0) (S->1S1) (S->0S0) (S->1S1) (S->0S0) (S->0S0) (S->0S0) (S->) 
01010000001010

...
```

To change the grammar that is processed, edit the `input_grammar` variable with the name of a grammar from the file [lib/exercises.ml](lib/exercises.ml) that you would like to test out. Since [bin/main.ml](bin/main.ml) imports the `Exercise` modules, VS Code should show a pop-up suggestion as you type the name of the grammar.


## Exercises

Your job in this project is to define a few grammars and test their behavior. The only files you need to edit are [lib/exercises.ml](lib/exercises.ml) and [test/test_contextfree.ml](test/test_contextfree.ml).

In the file [lib/exercises.ml](lib/exercises.ml) there are four grammars that are missing a definition. Provide an appropriate definition for the required language of the task.

Then, after defining each grammar, complete the tests in [lib/exercises.ml](lib/exercises.ml) and make sure they pass with:

```
dune test
```

For each test, you need to write a sequence of productions that generates the string on the right side of the `=` sign.

> [!IMPORTANT]
> You can only apply to the productions by using their index in the list you provided for the `productions` field of the grammar.
>
>For example, write `[0; 1; 2]` to apply the productions `S --> "0S0"`, `S --> "1S1"`, `S --> ""`, in that order.

### Exercise 1 (zero_n_one_n)
Define a grammar for the language `0^n1^n` (that is, `n` repetitions of the letter 0 followed by `n` repetitions of 1, for any `n`). Derive the following words: the empty word, `01`, `0^(10)1^(10)`.

### Exercise 2 (palindromes)
Define a grammar for the language over {0,1} where every word is equal to its reverse, or, equivalently, any word can read the same backwards as forwards. Derive the following words: `11011`, `0110`, `011101110`.

### Exercise 3 (balanced_parentheses)
Define a grammar for the language of balanced parentheses. The available parentheses are "()", "[]", "{}". Each open parenthesis must be matched by a closed one, and different pairs must not intersect.

Derive the following words: `()[]{}`, `({})[]`, `({[][{}()]})`.

### Exercise 4 (same_amount)
Define a grammar for the language whose words have as many 0's as 1's. Derive the following words: empty, `1001`, `00110101`, `10001110`.

> [!TIP]
> `dune build` has a handy "watch mode" that builds your project in real time. In the long run, this saves you a lot of typing. Open a new terminal and run it with the `-w` option:
>
>```
>dune build -w
>```
>Keep it running in the background while you work on [lib/grammar.ml](lib/grammar.ml) and look at its output when your code doesn't compile.