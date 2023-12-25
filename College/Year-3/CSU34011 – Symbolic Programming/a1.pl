% Simplifying numerals
refactorNum(-0, 0).
refactorNum(-p(X), Result) :- minus(p(X), Result).
refactorNum(-s(X), Result) :- minus(s(X), Result).
refactorNum(X-Y, Result) :- subtract(X, Y, Temp), refactorNum(Temp, Result).
refactorNum(-(X-Y), Result) :- subtract(X, Y, Temp), refactorNum(-Temp, Result).
refactorNum(0, 0).
refactorNum(X+0, Result) :- refactorNum(X, Result).
refactorNum(0+X, Result) :- refactorNum(X, Result).
refactorNum(X+s(Y), Result) :- refactorNum(s(X)+Y, Result).
refactorNum(X+p(Y), Result) :- refactorNum(p(X)+Y, Result).
refactorNum(p(s(X)), Result) :- refactorNum(X, Result).
refactorNum(s(p(X)), Result) :- refactorNum(X, Result).
refactorNum(p(X), p(Result)) :- refactorNum(X, Result).
refactorNum(s(X), s(Result)) :- refactorNum(X, Result).

% Negating a numeral
inverseNum(0, 0).
inverseNum(s(X), p(Result)) :- inverseNum(X, Result).
inverseNum(p(X), s(Result)) :- inverseNum(X, Result).

% Getting the negative of a numeral
minus(X, Result) :- inverseNum(X, Inverted), refactorNum(Inverted, Result).

% Adding two numerals
add2(X, Y, Result) :-
    refactorNum(X, RefactoredX),
    refactorNum(Y, RefactoredY),
    refactorNum(RefactoredX+RefactoredY, Result).

% Subtracting two numerals
subtract(X, Y, Result) :-
    refactorNum(X, RefinedX),
    refactorNum(Y, RefinedY),
    inverseNum(RefinedY, InvertedY),
    add2(RefinedX, InvertedY, Result).

% Revised subtraction to handle more numerals
subtract2(X, Y, Result) :- subtract(X, Y, Result).

% Revised addition to handle more numerals
add3(X, Y, Result) :- add2(X, Y, Result).
add4(X, Y, Result) :- add2(X, Y, Result).
add5(X, Y, Result) :- add2(X, Y, Result).
