ttm-puzzle-solver
=================

This is a naïve prolog implementation of a solver for the puzzles of the videogame "To the moon" (which is great by the way).
So if you are a little lazy (like me) and don't want to waste your time with the puzzles, which is sometimes a little annoying if you want to see the end of the story, you are free to use it.
The only cons is that you need prolog installed*.

To execute it, open the prolog shell and load 'trabajo.pl' and execute:

anchura_2(initial_state,S),imprime_lista(S).

where initial_state is a constant indicating the initial state of the puzzle as list of list, for example:

anchura_2([[1,1,0,1,0],[0,0,1,0,1],[1,1,0,1,0],[1,1,0,1,0],[1,1,0,1,0]],S),imprime_lista(S).

*Well maybe another problem is it's in my mother tongue (Spanish)
