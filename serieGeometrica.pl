% N = 3
%  _______________________
% | (0,2) | (1,2) | (2,2) |
% |_______|_______|_______|
% | (0,1) | (1,1) | (2,1) |
% |______ |_______|_______|
% | (0,0) | (1,0) | (2,0) |
% |_______|_______|_______|

siguiente(_, quieto, F, F).
siguiente(_, invertir, [Figura, [X, Y]], [Figura, [Y, X]]).
siguiente(N, invertirHorizontal, [Figura, [N, Y]], [Figura, [0, Y]]). 
siguiente(N, invertirHorizontal, [Figura, [0, Y]], [Figura, [N, Y]]). 
siguiente(N, invertirHorizontal, [Figura, [X, Y]], [Figura, [X2, Y]]) :-
	X > 0, X < N, X2 is N - X.
siguiente(N, invertirVertical, [Figura, [X, N]], [Figura, [X, 0]]).
siguiente(N, invertirVertical, [Figura, [X, 0]], [Figura, [X, N]]).
siguiente(N, invertirVertical, [Figura, [X, Y]], [Figura, [X, Y2]]) :-
	Y > 0, Y < N, Y2 is N - Y.
siguiente(N, arriba, [Figura, [X, Y]], [Figura, [X, Y2]]) :- 
	Y < N, Y2 is Y+1.
siguiente(_, abajo, [Figura, [X, Y]], [Figura, [X, Y2]]) :-
	Y > 0, Y2 is Y-1.
siguiente(N, derecha, [Figura, [X, Y]], [Figura, [X2, Y]]) :-
	X < N, X2 is X+1.
siguiente(_, izquierda, [Figura, [X, Y]], [Figura, [X2, Y]]) :-
	X > 0, X2 is X-1.
siguiente(N, todoArriba, [Figura, [X, _]], [Figura, [X, N]]).
siguiente(_, todoAbajo, [Figura, [X, _]], [Figura, [X, 0]]).
siguiente(N, todoDerecha, [Figura, [_, Y]], [Figura, [N, Y]]).
siguiente(_, todoIzquierda, [Figura, [_, Y]], [Figura, [0, Y]]).
siguiente(N, horario, [Figura, [X, Y]], [Figura, [X2, Y2]]) :-
	( N = 0 -> X2 is X, Y2 is Y;
	  X = 0, Y < N -> X2 is X, Y2 is Y+1;
	  Y = N, X < N -> X2 is X+1, Y2 is Y;
	  X = N, Y > 0 -> X2 is X, Y2 is Y-1;
	  Y = 0, X > 0 -> X2 is X-1, Y2 is Y;
	  X > 0, Y > 0 -> reducir(N, X, Y, Xt, Yt, D), Dif is (N - D) / 2,
	  				  siguiente(D, horario, [Figura, [Xt, Yt]], [Figura, [Xt2, Yt2]]),
	  				  X2 is Xt2 + Dif, Y2 is Yt2 + Dif).
siguiente(N, antihorario, [Figura, [X, Y]], [Figura, [X2, Y2]]) :-
	( N = 0 -> X2 is X, Y2 is Y;
	  X = 0, Y > 0 -> X2 is X, Y2 is Y-1;
	  Y = N, X > 0 -> X2 is X-1, Y2 is Y;
	  X = N, Y < N -> X2 is X, Y2 is Y+1;
	  Y = 0, X < N -> X2 is X+1, Y2 is Y;
	  X > 0, Y > 0 -> reducir(N, X, Y, Xt, Yt, D), Dif is (N - D) / 2,
	  				  siguiente(D, antihorario, [Figura, [Xt, Yt]], [Figura, [Xt2, Yt2]]),
	  				  X2 is Xt2 + Dif, Y2 is Yt2 + Dif).

reducir(N, X, Y, X2, Y2, D) :-
	((X =:= 0; Y =:= 0; X =:= N; Y =:= N) -> X2 is X, Y2 is Y, D is N;
	 Nt is N - 2, Xt is X - 1, Yt is Y - 1,
	 reducir(Nt, Xt, Yt, X2, Y2, D)).

resolverSerie(_, X, _, _) :-
	length(X, L), L < 2, write('Input insuficiente'), !.

resolverSerie(N, [X|Y], Respuesta, Patron) :-
	length(Y, L), L > 0,
	N2 is N-1,
	hallarPatron(N2, X, Y, Patron),
	last(Y, Ultimo),
	siguienteImagen(N2, Patron, Ultimo, Respuesta).

hallarPatron(N, X, [Y|[]], Patron) :-
	siguienteImagen(N, Patron, X, Y).

hallarPatron(N, X, [Y|Z], Patron) :-
	length(Z, L), L > 0,
	hallarPatron(N, X, [Y], Patron1),
	siguienteImagen(N, Patron1, Y, Siguiente),
	hallarPrimero(Z, Primero, _),
	Primero = Siguiente,
	hallarPatron(N, Y, Z, Patron),
	Patron1 = Patron.

hallarPrimero([Primero|Resto], Primero, Resto).

siguienteImagen(N, [Patron|[]], [Imagen1|[]], [Imagen2|[]]) :-
	siguiente(N, P, Imagen1, Imagen2),
	append([P], [], Patron).

siguienteImagen(N, [Patron|[]], [Imagen1|[]], [Imagen2|[]]) :-
	siguiente(N, P1, Imagen1, ImagenT),
	siguiente(N, P2, ImagenT, Imagen2),
	append([P1, P2], [], Patron).

%siguienteImagen(N, [Patron|[]], [Imagen1|[]], [Imagen2|[]]) :-
%	siguiente(N, P1, Imagen1, ImagenT1),
%	siguiente(N, P2, ImagenT1, ImagenT2),
%	siguiente(N, P3, ImagenT2, Imagen2),
%	append([P1, P2, P3], [], Patron).

siguienteImagen(N, [Patron|X], [Imagen1|Y], [Imagen2|Z]) :-
	length(Y, LY), LY > 0,
	siguienteImagen(N, [Patron], [Imagen1], [Imagen2]),
	siguienteImagen(N, X, Y, Z).

testSerie(Nombre, X, Y) :-
	figuras(Nombre, N, Figuras),
	resolverSerie(N, Figuras, X, Y).

figuras(test1, 3,
		[ [ [cuadrado, [2, 0] ], [circulo, [2, 2] ] ],
		  [ [cuadrado, [2, 1] ], [circulo, [2, 0] ] ],
		  [ [cuadrado, [2, 2] ], [circulo, [0, 0] ] ],
		  [ [cuadrado, [1, 2] ], [circulo, [0, 2] ] ] ]).

figuras(test2, 5,
		[ [ [cuadrado, [1, 3] ], [circulo, [3, 1] ], [triangulo, [2, 3]] ],
		  [ [cuadrado, [3, 2] ], [circulo, [1, 2] ], [triangulo, [2, 2]] ],
		  [ [cuadrado, [1, 1] ], [circulo, [2, 0] ], [triangulo, [3, 1]] ] ]).

figuras(test3, 4,
		[ [ [cuadrado, [0, 1] ], [circulo, [3, 0] ], [triangulo, [1, 3]], [equis, [0, 0]] ],
		  [ [cuadrado, [0, 0] ], [circulo, [2, 1] ], [triangulo, [3, 3]], [equis, [1, 1]] ],
		  [ [cuadrado, [1, 0] ], [circulo, [1, 2] ], [triangulo, [3, 1]], [equis, [2, 2]] ] ]).