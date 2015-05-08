member(_, []) :- false.
member(E, [H|T]) :- H =:= E -> true, !; member(E,T) .
searchList(I,H,[],X) :- [_|R] = H, member(I,R) -> X = false; X = true.
searchList(I,H,T,X) :- [_|R] = H, print(R), print(I), member(I,R) -> X = false; [F|Q] = T, searchList(I,F,Q,X).
searchFZI(I,[],X) :- X = I.
searchFZI(I,[H|T],X) :- searchList(I,H,T,X).
searchFZI([],X) :- X = [].
searchFZI(W,X) :- [H|T] = W, [I|_] = H, searchFZI(I,T,X) .
