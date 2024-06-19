# Projects

Use a SICStus Prolog installation and consult the file within the respective `src` folder of each project with the name of that project (racingkings.pl, houses.pl or zurero.pl). The following projects can be found in the [projects](projects/) folder.

## Racing Kings

Like a regular game of chess but no checks are allowed. The first player to move the king to the 8th rank wins. If both arrive in the same turn, it is a draw.

## Zurero

This game is an interesting variation of Go-Moku, in which players "slide" stones along the board lines from the edge of the board till the "sliding" stone is stopped by another stone on the board.

The intriguing feature of the game is that the "sliding" stone pushes the "stopping" stone if there is an empty space behind it.

## Houses

This project is based on a CLPFD (Constraint Logic Programming with Finite Domains) problem.
The Houses Puzzle’s is a puzzle created by Erich Friedman, a puzzles enthusiast, with the objective of connecting pairs of houses so that there are at max two different distances. These pairs of houses, with only one connection each, are created with the elements of a list with even number of house coordinates, which is the puzzle’s input.

# Prolog Coding Conventions

Variables in `PascalCase`.

Predicates in `snake_case`.

Commenting style from official SWI-Prolog code, public at their repository.
* `/* */` multiline comment blocks
* `+` can be used to describe an argument that should be instantiated
* `-` can be used to describe an argument that the predicate will instantiate
* `?` can be used to describe an argument which can be either instantiated or not

**Example** 

```prolog
/*  max_member(-Max, +List)
    
    True when Max is the largest  member in the standard order of terms.  
    
    Fails if List is empty.
*/
  max_member(Max, List).
```

