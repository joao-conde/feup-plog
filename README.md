# feup-plog

## PROLOG CONVENTIONS FOR BETTER CODE

### Predicate names

Lower case letters with words delimited by underscore.

**Example** ast_node (not ASTnode or astNode)  
**Reason** consistency with standard Prolog notation and in-built predicates

### Variables

Java-style but start with capital letter.

**Example** AVeryLongVariableName (not a_very_long_var_name)   
**Reason** Shorter and easier to write than the underscore variant. The leading capital letter is required by Prolog to identify variables.

### Comments

Use javadoc comments for things to include in Prologdoc (between /** ...... */)

**Example** 

```c
/**
  @descript A short description of the predicate
  
  @param VarOne This variable meaning
  @param VarTwo This variable meaning
*/
my_predicate(VarOne, VarTwo).

```
**Reason** Better documentation and later easily slightly changed for Prolog documentation tools.


### Line breaking

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

**Reason** Better code legibility.
