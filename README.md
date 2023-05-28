# gobstones-parser
Haskell-based parser for Gobstones, an educative imperative language.

Gobstones allows the user to manipulate a board with several cells via the following sentences:
- Mover (Norte|Sur|Este|Oeste): moves the currently selected cell in a cardinal direction (fails if it has gone off the board's border).
- IrAlBorde (Norte|Sur|Este|Oeste): moves the currently selected cell as far as it's possible in a cardinal direction.
- Poner (Rojo|Azul|Negro|Verde): adds a ball of a certain colour in the currently selected cell.
- Sacar (Rojo|Azul|Negro|Verde): removes a ball of a certain colour in the currently selected cell (fails if it has no balls of this colour to remove).

Gobstones also provides the following expressions:
- nroBolitas (Rojo|Azul|Negro|Verde): checks how many balls of this colour the currently selected cell has. Reduces to an integer value.
- natural numeric literals. They are interpreted as integer values.
- hayBolitas (Rojo|Azul|Negro|Verde): checks whether the currently selected cell has at least one ball of this colour. Reduces to a boolean value.
- puedeMover (Norte|Sur|Este|Oeste): checks whether there's available space in this cardinal direction to move, i.e. whether Mover will succeed or fail if executed with this cardinal direction. Reduces to a boolean value.

Currently, this parser is only able to parse a subset of the original language:
- programs: they define the main context of the program, using the "program" keyword.
- conditional structures: if, if-else. The boolean expression must be enclosed in parentheses.
- exact iterative structure: repeat (a.k.a. C's "for"). The integer expression must be enclosed in parentheses.
- conditional iterative structure: while. The boolean expression must be enclosed in parentheses.
- sentences and expressions previously mentioned.

Compound sentences must be enclosed in curly brackets. Sentences may be separated with tabs, spaces or newlines.

The file path is passed as a command line argument (there's an example, "programa.gobstones", in the repo). 
