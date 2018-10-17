# PROLOG CONVENTIONS FOR BETTER CODE

## Predicate names

Lower case letters with words delimited by underscore.

**Example** ast_node (not ASTnode or astNode)  
**Reason** consistency with standard Prolog notation and in-built predicates

## Variables

Java-style but start with capital letter.

**Example** AVeryLongVariableName (not a_very_long_var_name)   
**Reason** Shorter and easier to write than the underscore variant. The leading capital letter is required by Prolog to identify variables.

## Comments

Commenting style from official SWI-Prolog code, public at their repository.
* '+' can be used to describe an argument that should be instantiated
* '-' can be used to describe an argument that the predicate will instantiate
* '?' can be used to describe an argument which can be either instantiated or not

**Example** 

```c
%!  max_member(-Max, +List)
%
%   True when Max is the largest  member in the standard order of
%   terms.  Fails if List is empty.
 max_member(Max, List).
```

**Reason** Better documentation and later easily slightly changed for Prolog documentation tools.


## Line breaking

If the predicate makes only one subsequent predicate call, place both on same line. If not, one predicate per line.   

If argument list is too long, put one argument in every line with matching brackets in same column.  

**Example** 

```c

mortal(X):- alive(X).

example_with_long_parameter_list(X, Y, Z):-
  findall(X,
         (long(Z,X), conjunction(X), of(X,Y), predicates(X)),
         Result),
  next_predicate(Result).

```

**Reason** Better code readability.
