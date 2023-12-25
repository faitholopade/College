%Problem 1 Write a DCG that accepts strings of the form 
%u0v where u and v are strings over the alphabet {1, 2, 3} such that u is v in
%reverse.
s --> u(U), [0], v(U).

u([]) --> [].
u([X|Xs]) --> [X], { member(X, [1, 2, 3]) }, u(Xs).

v([]) --> [].
v([X|Xs]) --> v(Xs), [X].

%Problem 2 write a DCG that outputs strings
% [h(Col1,Nat1,Pet1), h(Col2,Nat2,Pet2), h(Col3,Nat3,Pet3)]satisfying (∗), 
% where the nationalities are english, spanish, japanese and the pets are jaguar
% , snail, zebra. To avoid confusion with the first problem, use different 
% binary predicates for the difference lists, and, in particular, nbd/2 for 
% the 3 houses. 

nbd --> house(Col1, Nat1, Pet1), house(Col2, Nat2, Pet2), house(Col3, Nat3, Pet3), 
        { Col1 \= Col2, Col1 \= Col3, Col2 \= Col3,
          Nat1 \= Nat2, Nat1 \= Nat3, Nat2 \= Nat3,
          Pet1 \= Pet2, Pet1 \= Pet3, Pet2 \= Pet3,
          member(h(red,_,_), [h(Col1, Nat1, Pet1), h(Col2, Nat2, Pet2), h(Col3, Nat3, Pet3)]),
          member(h(_,english,_), [h(Col1, Nat1, Pet1), h(Col2, Nat2, Pet2), h(Col3, Nat3, Pet3)]),
          member(h(_,_,snail), [h(Col1, Nat1, Pet1), h(Col2, Nat2, Pet2), h(Col3, Nat3, Pet3)]) }.

house(Col, Nat, Pet) --> [h(Col, Nat, Pet)], 
    { member(Col, [red, blue, green]),
      member(Nat, [english, spanish, japanese]),
      member(Pet, [jaguar, snail, zebra]) }.

%Problem 3 Define a DCG that generates for every n ≥ 1, lists [F0, F1, . . . , Fn] 

fib --> [0,1], calc_fib(0,1).
calc_fib(_,_) --> [].
calc_fib(F1,F2) --> {F is F1+F2}, [F], calc_fib(F2,F).


%Problem 4 Define the predicates tran and final to accept precisely the strings 
%in L3. 
accept(L) :- steps(q0,L,F), final(F).
steps(Q,[],Q).
steps(Q,[H|T],Q2) :- tran(Q,H,Qn), steps(Qn,T,Q2).

tran(q0, 0, q0).
tran(q0, 1, q0).
tran(q0, 0, q1).
tran(q0, 1, q1).

tran(q1, 1, q2).
tran(q2, 0, q3).
tran(q2, 1, q3).
tran(q3, 0, q4).
tran(q3, 1, q4).
final(q4).

%Turn transitions into a DCG for L3. 

q0 --> [0], q0.
q0 --> [1], q0.
q0 --> [0], q1.
q0 --> [1], q1.
    
  
q1 --> [1], q2.
q2 --> [0], q3.
q2 --> [1], q3.
q3 --> [0], q4.
q3 --> [1], q4.
q4 --> [].

%Define a predicate l3(String,Numeral) that holds if String belongs to L3 and 
%has length Numeral and numeral(Numeral)

numeral(0).
numeral(succ(X)) :- numeral(X).

string_length([], 0).
string_length([_|T], succ(X)) :- string_length(T, X), numeral(X).

l3([1,0,0], succ(succ(succ(0)))).
l3([1,0,1], succ(succ(succ(0)))).
l3([1,1,0], succ(succ(succ(0)))).
l3([1,1,1], succ(succ(succ(0)))).


l3([0|T], succ(X)) :- string_length(T, X), X\=0, X\=succ(0), l3(T,X).
l3([1|T], succ(X)) :- string_length(T, X), X\=0, X\=succ(0), l3(T,X).