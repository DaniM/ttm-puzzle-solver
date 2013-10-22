% Ejercicio propio
% Se trata de resolver el siguiente puzzle:
% Tenemos una tablero cuadrado de 1s y 0s
% [ 1,0,0
%   1,0,0
%   1,0,0]
% En el cual podemos operar por filas, por columnas o por la diagonal
% (abajo-izquierda, arriba-derecha).
% Si por ejemplo aplicamos "operamos" sobre la fila 1 el estado resultante sería
% el siguiente
%[
% 0,1,1
% 1,0,0
% 1,0,0
%]
% Si sobre este estado aplicamos la diagonal obetenemos
% [
% 0,1,0
% 1,1,0
% 0,0,0
% ]
%
% El objetivo es conseguir todo a 1
%
%[
% 1,1,1
% 1,1,1
% 1,1,1
%]
% Usando el módulo busqueda.pl vamos a diseñar un programa Prolog que
% resuelva el dicho problema.
% Las búsquedas que extenderemos serán:
%  - profundidad con ciclos,
%  - anchura
%
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(busqueda).

:- dynamic estado_inicial/1.
% el estado inicial será dinámico ya que la búsqueda como entrada recibirá
% una matriz M que nosotros estableceremos como el estado_inicial(M).
	
:- dynamic estado_final/1.
% pondremos el estado final dinámico ya que dependerá del estado inicial


%% ======================
%% BÚSQUEDA
%% ======================
%% Extendemos las diversas búsquedas para construir el estado inicial y final
%% a partir de una matriz
profundidad_con_ciclos_2([H|T],S) :-
                  retractall(estado_inicial(_)),
		  retractall(estado_final(_)), assert( estado_inicial([H|T]) ),
		  length([H|T],F), length(H,C), matriz1(F,C,EF),
		  assert(estado_final(EF)),
		  profundidad_con_ciclos(S).

anchura_2([H|T],S) :- retractall(estado_inicial(_)),
	retractall(estado_final(_)), assert( estado_inicial([H|T]) ),
	length([H|T],F), length(H,C), matriz1(F,C,EF),
	assert(estado_final(EF)),
	anchura(S).


%% Reglas auxiliares

%% matriz1(+N,-M) dado un número crea una matriz cuadrada M de 1s con esa
%% dimensión
matriz1(N,M) :- matriz1_aux(N,N,M).
matriz1(1,[[1]]).
matriz1(0,[]).

matriz1_aux(F,C,M) :- F > 1, !, F2 is F - 1, matriz1_aux(F2,C,M2),
	inicializa_lista(C,1,L), append(M2,[L],M).
matriz1_aux(1,C,[M]) :- inicializa_lista(C,1,M).
matriz1_aux(0,_,[]).

%% inicializa_lista(+N,+V,-L) devuelve/crea una lista L de tamaño N con el valor
%% V dado

inicializa_lista(N,V,[V|L]) :- N > 1,!, N2 is N - 1,
	inicializa_lista(N2,V,L). 
inicializa_lista(1,V,[V]).
inicializa_lista(0,_,[]).

%% posicion(?L1,X?,?L2) encuentra la posición L2 de un elemento en la lista L1
%% que puede contener a su vez otra lista
posicion([X|_],X,[1]).

posicion([H|_],X,[1|Pt]) :-
	is_list(H),
	posicion(H,X,Pt).

posicion([_|T],X,[Ph1|Pt]) :-
	posicion(T,X,[Ph2|Pt]),
	Ph1 is Ph2 + 1.

%% intercambia(+L1,+V1,+V2,-L2) sustituye las ocurrencias V1 por V2 y viceversa
%% en la lista L1, obteniendo como resultado L2

intercambia([],_,_,[]).

intercambia([V1|T1],V1,V2,[V2|T2]) :- intercambia(T1,V1,V2,T2).

intercambia([V2|T1],V1,V2,[V1|T2]) :- intercambia(T1,V1,V2,T2).

intercambia([H|T],V1,V2,[H|L]) :-
	not(is_list(H)),
	H \== V1,
	H \== V2,
	intercambia(T,V1,V2,L).

intercambia([H1|T1],V1,V2,[H2|T2]) :-
	is_list(H1),
	intercambia(H1,V1,V2,H2),
	intercambia(T1,V1,V2,T2).

%% reemplaza(+P,+L1,+V,-L2) dada una posición P de una lista L1 y un valor V,
%% reemplaza el valor de la posición P de L1 por V dando como resultado L2
%% NOTA: realmente la interfaz es reemplaza(+P,?L1,?V,?L2)
reemplaza(N,[H|T],V,[H|T2]) :- N > 0, N2 is N-1, reemplaza(N2,T,V,T2).
reemplaza(0,[_|T],V,[V|T]).

%% niega(+P,+L1,-L2) dada una posición P de una lista L1 de 1s y 0s (binaria)
%% hace un "not" del valor de la posición P dando como resultado L2
%% NOTA: realmente la interfaz es niega(?P,?L1,?L2)
niega(N,[H|T],[H|T2]) :- N > 0, N2 is N-1, niega(N2,T,T2).
niega(0,[1|T],[0|T]) :- !.
niega(0,[0|T],[1|T]) :- !.


%% guarda_lista(+L,+NombreFichero) imprime la lista, en el fichero con el
%% nombre indicado
guarda_lista(L,N) :-
	 open(N,write,Stream), 
         imprime_lista(L,Stream), 
         close(Stream).

imprime_lista([],_).
imprime_lista([X],Stream) :-
	not(is_list(X)),
	write(Stream,X),
	nl(Stream).
imprime_lista([H|T],Stream) :-
	not(is_list(H)),
	write(Stream,H),
	write(Stream,' '),
	imprime_lista(T,Stream).
imprime_lista([H|T],Stream) :-
	is_list(H),
	imprime_lista(H,Stream),
	imprime_lista(T,Stream).


imprime_lista([]):-nl,!.
imprime_lista([X]) :-
	not(is_list(X)),!,
	write(X),
	nl.
imprime_lista([H|T]) :-
	not(is_list(H)),!,
	write(H),
	write(' '),
	imprime_lista(T).
imprime_lista([H|T]) :-
	is_list(H),!,
	imprime_lista(H),
	imprime_lista(T).


%% guarda_solucion(+S,+Nombre) la usaremos para dar un poco más de formato a
%% nuestro fichero de salida. Básicamente es añadir una línea en blanco entre
%% estados
guarda_solucion(S,N) :-
	 open(N,write,Stream), 
         imprime_solucion(S,Stream), 
         close(Stream).

imprime_solucion([],_).

imprime_solucion([H|T],Stream) :-
	imprime_lista(H,Stream),
	nl(Stream),
	imprime_solucion(T,Stream). 

%%=========
%% Estados
%%=========

%% Los estados serán una de lista de listas cuyo tamaño dependerá del problema
%% (estado inicial)

%% [[1,0,0]
%%  [1,0,0]
%%  [1,0,0]]

%% Estado final (para 3x3)
%% [[1,1,1]
%%  [1,1,1]
%%  [1,1,1]]

%%======================
%% CÁLCULO DE SUCESORES
%%======================

sucesor(E,S) :- length(E,N),
	(sucesores_filas(E,N,S);sucesores_columnas(E,N,S);opera_diagonal(E,S)).

% Nótese como el operador diagonal no hace falta hacer un "for"

% sucesores operando sobre filas
% es necesaria esta regla para generar todos los candidatos posibles
% (operar sobre fila 1, operar sobre fila 2, ..., operar sobre fila N)
% sería el equivalente a un "for"
sucesores_filas(E,N,S) :- N > 0, N2 is N - 1,
	(opera_fila(E,N2,S);sucesores_filas(E,N2,S)).

% sucesores operando sobre columnas
% de la misma forma que con las filas debemos hacer las columnas
sucesores_columnas(E,N,S) :- N > 0, N2 is N - 1,
	(opera_columna(E,N2,S);sucesores_columnas(E,N2,S)).

%%============
%% OPERADORES
%%============
%% Las reglas de los operadores usados para el cálculo de sucesores, tendrán el
%% siguiente formato (+Estado,+N,-Sucesor) donde N será la fila o columna para
%% los operadores verticales y horizontales.
%% Para el operador de diagonal no hará falta especificar ni fila ni columna
%% (+Estado,-Sucesor)

opera_fila(E,N,S) :- op_filas_aux(E,N,S).
op_filas_aux([H1|T],N,[H1|T2]) :- N > 0, !, N2 is N-1, op_filas_aux(T,N2,T2).
op_filas_aux([H1|T],0,[H2|T]) :- intercambia(H1,1,0,H2).


opera_columna(E,N,S) :- op_columnas_aux(E,N,S).
op_columnas_aux([H1|T1],C,[H2|T2]) :- niega(C,H1,H2),
	op_columnas_aux(T1,C,T2).  
op_columnas_aux([],_,[]).

% si sólo admitimos matrices cuadradas
%opera_diagonal(E,F) :- length(E,N), op_diagonal_aux(E,N,S).

% NOTA: para soportar matrices no cuadradas hay que hacer unas comprobaciones
% extra
opera_diagonal([H|T],S) :- length(H,N), length([H|T],F), F == N
	,op_diagonal_aux([H|T],N,S).

opera_diagonal([H|T],[H|S]) :- length(H,N), length([H|T],F), F \== N
	,opera_diagonal(T,S).

op_diagonal_aux([H1|T1],N,[H2|T2]) :- N > 0, N2 is N - 1, niega(N2,H1,H2),
	op_diagonal_aux(T1,N2,T2).
op_diagonal_aux([],_,[]).



%%==========================
%% RESULTADOS
%%==========================

% Aunque también se le ha dado soporte a la búsqueda en profundidad,
% la idea es resolver el puzzle en el menor número de movimientos posible
% es por eso que usamos la búsqueda en anchura.

% probamos primero con ejemplos sencillos
% anchura_2([[0,0,0],[1,1,1],[1,1,1]],S),imprime_lista(S).

% algo un poco más complejo
% anchura_2([[1,0,0,1],[0,1,1,0],[0,1,1,0],[1,0,0,1]],S),imprime_lista(S).

% anchura_2([[1,0,0],[0,0,0],[0,0,1]],S),imprime_lista(S).

% anchura_2([[0,0,0,1],[1,1,0,1],[1,0,1,1],[1,0,0,0]],S),imprime_lista(S).

% anchura_2([[1,0,0,1],[0,1,1,0],[1,0,0,1],[0,1,1,0]],S),imprime_lista(S).

% anchura_2([[1,0,0,0],[0,1,0,0],[1,1,0,1],[1,1,1,0]],S),imprime_lista(S).

% 5x5
% anchura_2([[1,1,0,1,0],[0,0,1,0,1],[1,1,0,1,0],[1,1,0,1,0],[1,1,0,1,0]],S),imprime_lista(S).

%%=============================
%% EXTENSIÓN
%%=============================

%% Vamos a extender el ejercicio para que soporte matrices no cuadradas.
%% es decir, no habrá operador de diagonal

matriz1(F,C,M) :- matriz1_aux(F,C,M).

% anchura_2([[0,1,0],[0,1,0],[1,0,1],[1,0,1]],S),imprime_lista(S).

% anchura_2([[1,0,0],[0,1,0],[1,1,0],[1,1,1]],S),imprime_lista(S).

% anchura_2([[1,1,1,1],[1,1,1,0],[0,0,1,0],[0,1,0,0],[0,1,1,1]],S),imprime_lista(S).

% anchura_2([[1,0,0,0],[0,1,1,1],[1,0,0,0],[0,1,1,1],[1,0,0,0]],S),imprime_lista(S).

%%============
%% NOTA FINAL
%%============

%% Para probar más puzzles, se pueden encontrar aquí
%% http://www.gameboomers.com/wtcheats/pcTt/ToTheMoon/tothemoon.htm 


%% Pasos a seguir a partir de aquí:
%%
%% - Elaborar una heurística
%% - Implementar un A*