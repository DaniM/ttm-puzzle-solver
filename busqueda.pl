:- module(busqueda,[profundidad_sin_ciclos/1,profundidad_con_ciclos/1, anchura/1]).


% Búsqueda en profundidad sin ciclos



profundidad_sin_ciclos(S) :-
                  estado_inicial(E),
                  profundidad_sin_ciclos(E,S).

profundidad_sin_ciclos(E,[E]) :- estado_final(E).

profundidad_sin_ciclos(E,[E|S1]) :-
                         sucesor(E,E1),
                         profundidad_sin_ciclos(E1,S1).



% Búsqueda en profundidad con ciclos


profundidad_con_ciclos(S) :-
      estado_inicial(E),
      profundidad_con_ciclos([E],S).

profundidad_con_ciclos([E|C],S) :-
       estado_final(E),
       reverse([E|C],S).

profundidad_con_ciclos([E|C],S) :-
               sucesor(E,E1),
               not(memberchk(E1,C)),
               profundidad_con_ciclos([E1,E|C],S).


% Búsqueda en anchura


anchura(S) :-
       estado_inicial(E),
       anchura([[E]],S).


anchura([[E|C]|_],S) :-
          estado_final(E),
		  reverse([E|C],S).

anchura([N|R],S) :-
         expande([N|R],Sucesores),
         append(R,Sucesores,NAbiertos),
         anchura(NAbiertos,S).


expande([[E|C]|R],Sucesores) :-
             findall([E1,E|C],
             (sucesor(E,E1),
              not(memberchk(E1,C)),
              not(memberchk([E1|_],[[E|C]|R]))),
              Sucesores).












