# PROLOG PROJECTS

## Racing Kings

In this game, checks are entirely forbidden: not only is it forbidden to move one's king into check, but it is also forbidden to check the opponent's king.

The purpose of the game is to be the first player to move their king to the eighth row. When White moves their king to the eighth row, and Black, immediately on the next move, moves their king to the eighth rank, the game is declared a draw (this rule is to compensate for White's first-move advantage).

Apart from the above, pieces move and capture exactly as in normal chess.

![board](https://github.com/joao-conde/feup-plog/blob/master/racing-kings/report/Relat%C3%B3rio%20Final/initialBoard.PNG)

## Zurero

This game is an interesting variation of Go-Moku, in which players "slide" stones along the board lines from the edge of the board till the "sliding" stone is stopped by another stone on the board.

The intriguing feature of the game is that the "sliding" stone pushes the "stopping" stone if there is an empty space behind it.

![board](https://github.com/joao-conde/feup-plog/blob/master/zurero/docs/Intermediate%20Report/end_game.png)

# PROLOG CONVENTIONS FOR BETTER CODE

## Predicate names

Lower case letters with words delimited by underscore.

**Example** ast_node (not ASTnode or astNode)  
**Reason** consistency with standard Prolog notation and in-built predicates

## Variables

Java-style but start with capital letter.

**Example** AVeryLongVariableName (not a_very_long_var_name)   
**Reason** Shorter and easier to write than the underscore variant. The leading capital letter is required by PROLOG to identify variables.

## Comments

Commenting style from official SWI-Prolog code, public at their repository.
* '+' can be used to describe an argument that should be instantiated
* '-' can be used to describe an argument that the predicate will instantiate
* '?' can be used to describe an argument which can be either instantiated or not

**Example** 

```c
/*  max_member(-Max, +List)
    
    True when Max is the largest  member in the standard order of terms.  
    
    Fails if List is empty.
*/
  max_member(Max, List).
```

**Reason** Better documentation and quick understanding of the predicates


## Line breaking

One predicate per line.   

If argument list is too long, put one argument in every line with matching brackets in same column.  

**Example** 

```c

mortal(X):- 
  alive(X).

example_with_long_parameter_list(X, Y, Z):-
  findall(X,
         (long(Z,X), conjunction(X), of(X,Y), predicates(X)),
         Result),
  next_predicate(Result).

```

**Reason** Better code readability.
