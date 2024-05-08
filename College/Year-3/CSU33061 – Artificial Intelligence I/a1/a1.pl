% Establishes connections between nodes and calculates cost.
arc([H|T],Node,Cost,KB) :- member([H|B],KB), append(B,T,Node),
                           length(B,L), Cost is 1+L/(L+1).

% Estimates cost to reach the goal from a given node.
heuristic(Node,H) :- length(Node,H).


% Identifies the goal state in the search process.
goal([]).

% Integrates new paths into the search frontier.
add2frontier(NewChildren, Frontier, UpdatedFrontier) :-
    append(Frontier, NewChildren, UpdatedFrontier).

% Compares two nodes based on the sum of their cost and heuristic value.
less-than([Node1,_,Cost1|_],[Node2,_,Cost2|_]) :- heuristic(Node1,Hvalue1), 
                                                  heuristic(Node2,Hvalue2),
                                                  F1 is Cost1+Hvalue1, 
                                                  F2 is Cost2+Hvalue2,
                                                  F1 =< F2.


% Initiates the A* search algorithm.
astar(InitialNode, OptimalPath, TotalCost, KnowledgeBase) :-
    search([[InitialNode, [], 0]], OptimalPath, TotalCost, KnowledgeBase).

% Concludes the search when the goal state is reached.
search([[Node, Path, Cost]|_], CompletePath, Cost, _) :-
    goal(Node),
    reverse([Node|Path], CompletePath).

% Expands the current node and proceeds with the search.
search([[Node, PathSoFar, CostSoFar]|More], Path, Cost, KB) :-
    findall([Next, [Node|PathSoFar], NewCost],
            (arc(Node, Next, StepCost, KB), NewCost is CostSoFar + StepCost),
            Children),
    add2frontier(Children, More, NewFrontier),
    sort_frontier(NewFrontier, SortedFrontier),
    search(SortedFrontier, Path, Cost, KB).

% Sorts the frontier by the combined cost and heuristic value.
sort_frontier(Frontier, Sorted) :-
    maplist(assign_f_value, Frontier, ValuedFrontier),
    sort(ValuedFrontier, SortedValued),
    maplist(remove_f_value, SortedValued, Sorted).

% Assigns an F-value to each node-path pair for sorting.
assign_f_value([Node, Path, Cost], [FValue, Node, Path, Cost]) :-
    heuristic(Node, HValue),
    FValue is Cost + HValue.

% Removes the F-value after sorting to return to the original format.
remove_f_value([_, Node, Path, Cost], [Node, Path, Cost]).
