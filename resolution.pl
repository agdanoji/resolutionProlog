:- import length/2, append/3, select/3 from basics.
:- import maplist/2 from swi.

present(_,[],_):-!.
present(X,[(Y1,X)|_],Y1):- !.
present(X,[_|T],Y1):- present(X,T,Y1). 

member(X, [Y|T]) :- X = Y; member(X, T).

flatten_or(or(A,B), F) :-
   !,
   flatten_or(A,A1),
   flatten_or(B,B1),
   append(A1,B1,F).
flatten_or(X,[X]).

dif(X,Y):- X\=Y.
	
modifyList([],[]):-!.
modifyList([(X,Y)|T],[(X,Y1)|T1]):- flatten_or(Y,Y1),modifyList(T,T1).

listsort([],[]):-!.
listsort([(X,Y)|T],[(X,Y1)|T2]):- sort(Y,Y1),listsort(T,T2).
	
makelist(L):- findall((X,Y),myClause(X,Y),L).

makeclause([],[]):-!.
makeclause([(_,Y)|T],[Y|T1]):-makeclause(T,T1). 

%resolve([(_,L)|T]):- 
resolve([], Clauses,_,_) :-
        member([], Clauses).
resolve([H|T], Clauses,Z,N) :-
		resolveHelper(H, Clauses, R,Z,N,N1),
		resolve(T, [R|Clauses],[(N1,R)|Z],N1).				
resolveHelper((A,B,R),Clauses,R,Z,N,N1) :-
        member(A, Clauses),
		present(A,Z,P1),
		select(Q, A, X),
        member(B, Clauses),
		present(B,Z,P2),
		select(neg(Q), B, Y),
        append(X,Y,R0),
		sort(R0,R),
		maplist(dif(R),Clauses),
		N1 is N+1,
		write(resolution(P1,P2,R,N1)),nl.
		

resolution(I):- load_dyn(I),myQuery(X,Y),assert(myClause(X,neg(Y))),
				makelist(L),modifyList(L,L1),listsort(L1,Z),
				makeclause(Z,L2),length(Z,N),resolve(R,L2,Z,N)->
				write(resolution(success)),nl;
				write(resolution(failure)).