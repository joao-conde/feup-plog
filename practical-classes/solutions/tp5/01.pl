/*

    A :- B, C, !, D, E.
    A :- F, G.

    If B and C is proved, PROLOG tries to prove D and E.
    If D or E fails PROLOG would backtrack but stops at the cut.
    Cut always succeeds when passing normally in the search but fails in backtrack.
    This way we got "if B and C, prove D and E; else do F and G"

*/