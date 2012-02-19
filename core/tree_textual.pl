%%Display Routines:
% Give out structure, Incl. tags and chunks. tree -> tree-struc; fstruc -> functional structure

print_strucs2(Count,FinalScore,MC,1-_,Strucs) :-
  write(Count), write(' +> '), write(FinalScore), write(' :: '), write(MC),nl,
  print_strucs(Strucs),
  write('========================================================================'),nl.

print_strucs([]).
print_strucs([F|R]) :- 
  write_tree(F),
  %% print(F), %% DEBUG CODE; REMOVE.
  print_strucs(R).

%%%%%    write_tree( +Struct)
%%%%%         displays prolog structure Struct as a tree
%%%%%         ex: write_tree(s(np(d(le), n(chien)), vp(v(dort)))).
%%%%%                   s
%%%%%               ____|___
%%%%%              /        \
%%%%%             np        vp
%%%%%            _|_        |
%%%%%           /   \       |
%%%%%           d    n      v
%%%%%           |    |      |
%%%%%           |    |      |
%%%%%          le  chien  dort
%%% write_tree( +Struct)
%%%   Tree output of prolog term Struct
%

%atom_length(A,L) :- name(A,NA), length(NA,L).

write_tree( Struct) :-
     analyse( Struct, List, 0, _R),
     print_tree( List).

     analyse( Atom, [n( Atom, Pos)], L, R) :-  %% atomic case
          atomic( Atom),
          !,
          atom_length( Atom, Len),
          R is L + Len -2, %% R is L + Len + 2,
          Pos is (R + L) // 2 .

     analyse( Tree, [n( Func, Pos), [n( Succ, Pos)]], L, R) :- %% atomic successor
          Tree =.. [Func, Succ],
          atomic( Succ),
          !,
          atom_length( Succ, Len1),
          atom_length( Func, Len2),
          max( Len1, Len2, Len),
          R is L + Len -2, %% R is L + Len + 2
          Pos is (R + L) // 2.



     analyse( Tree, [n( Func, Pos), List_Succ], L, R) :-
          is_list(Tree),
	  length(Tree,Length),
	  Length > 1,
	  !,
	  Func = '.',
	  (delete( Tree, 'passive', TempSucc2);true),
	  (delete( TempSucc2, 'mainclause', [FirstElem|_]);true),
	  R_Node is L -1,
          analyse_successor( [FirstElem], L, R_Sub_Tree, List_Succ),
          calculate_pos( L, Pos, List_Succ, R_Sub_Tree, R_Node, R).


     analyse( Tree, [n( Func, Pos), List_Succ], L, R) :-
          Tree =.. [FuncTemp, Succ],
          is_list(Succ),
	  (	(lexic(FuncTemp,Lemma,SenPos), chart(SenPos,SenPos,SenPos,_,_,Tag,_,_,_,[Func|_])
		)
	  	; 
		Func = FuncTemp
	  ),
          atom_length( Func, Len),
	  !,
	  jointag(Lemma,Tag,NewSucc),
	  R_Node is L +Len-2,
          analyse_successor( [NewSucc], L, R_Sub_Tree, List_Succ),
          calculate_pos( L, Pos, List_Succ, R_Sub_Tree, R_Node, R).



     analyse( Tree, [n( Func, Pos), List_Succ], L, R) :-
          Tree =.. [Func|Succ],
          member( [], Succ),
          delete( Succ, [], NewSucc),
          !,
          atom_length( Func, Len),
          R_Node is L + Len -2,  %% +2
          analyse_successor( NewSucc, L, R_Sub_Tree, List_Succ),
          calculate_pos( L, Pos, List_Succ, R_Sub_Tree, R_Node, R).


     analyse( Tree, [n( Func, Pos), List_Succ], L, R) :-
          Tree =.. [FuncTemp|Succ],
	  (	(lexic(FuncTemp,_,SenPos), chart(SenPos,SenPos,SenPos,_,_,_,_,_,_,[Func|_])
		)
	  	; 
		Func = FuncTemp
	  ),
          atom_length( Func, Len),
          R_Node is L + Len -2,  %% +2
          analyse_successor( Succ, L, R_Sub_Tree, List_Succ),
          calculate_pos( L, Pos, List_Succ, R_Sub_Tree, R_Node, R).

          analyse_successor( [H|T], L, R, List) :- %% do not display diff-lists in tree
               H =.. [F|_],
               F = '-',
               \+ (T = []),
               !,
               analyse_successor( T, L, R, List).
          analyse_successor( [H|T], L, R, List) :-
               \+ (T = []),
               analyse( H, List1, L, Mid),
               analyse_successor( T, Mid, R, List2),
               append( List1, List2, List).
          analyse_successor( [E], L, R, List) :-
               analyse( E, List, L, R).

          calculate_pos( _L, Pos, List_Succ, R, R_Node, R) :-
               R_Node =< R,
               !,
               first_node( List_Succ, Pos1),
               last_node(  List_Succ, Pos2),
               Pos is (Pos1 + Pos2) // 2.
          calculate_pos( L, Pos, _List_Succ, R_Sub_Tree, R, R) :-
               R > R_Sub_Tree,
               Pos is (R + L) // 2.

     print_tree( List) :-
          List = [_|_],
          write_node( List, 0), nl,
          write_branches( List, 0), nl,
          write_twigs( List, List1, 0), nl,
          print_tree( List1).
     print_tree( []).

         write_node( [H|T], Pos) :-
               print_node( H, Pos, Pos1),
               !,
               write_node( T, Pos1).
          write_node( [_|T], Pos) :-
               !,
               write_node( T, Pos).
          write_node( [], _Pos).

               print_node( n( E, Pos), S1, S2) :-
                    atom_length( E, Len),
                    B is Pos - Len // 2 - S1, write_n_times( ' ', B),
                    S2 is Pos + Len // 2 + Len mod 2,
                    write( E).

            % leaves
          write_branches( [_, H|T], Pos) :-
               \+ (H = [_|_]),
               !,
               write_branches( [H|T], Pos).
          write_branches( [_E], _Pos) :- !.
          write_branches( [n( _E, Pos), H2|T], Pos1) :-
               node_number( H2, 1),
               !,
               B is Pos - Pos1, write_n_times( ' ', B),
               write( '|'),
               Pos2 is Pos + 1,
               write_branches( T, Pos2).
            % normal categories
          write_branches( [_H, H2|T], Pos1) :-
               \+ node_number( H2, 1),
               !,
               print_branches( H2, Pos1, Pos2),
               write_branches( T, Pos2).
          write_branches( [], _).

               print_branches( List, Pos1, Pos2) :-
                    first_node( List, PosFirst),
                    last_node(  List, PosLast),
                    Mid is (PosFirst + PosLast) // 2,
                    B is PosFirst - Pos1 + 1, write_n_times( ' ', B),
                    U is Mid - PosFirst - 1,  write_n_times( '_', U),
                    write( '|'),
                    V is PosLast - Mid - 2,   write_n_times( '_', V),
                    Pos2 is PosLast - 1.

                    first_node( [n( _, Pos)|_], Pos).

                    last_node( [_|T], Pos)        :-  last_node( T, Pos).
                    last_node( [n( _, Pos)], Pos) :-  !.    % 1. case: leaf
                    last_node( [n( _, Pos), [_|_]], Pos).   % 2. case: dominant node

            % leaves
          write_twigs( [_H, H2|T], T1, Pos) :-
               \+ (H2 = [_|_]),
               !,
               write_twigs( [H2|T], T1, Pos).
          write_twigs( [E], [], _Pos) :-
               \+ (E = [_|_]),
               !.
            % non branching, i.e. lexical categories
          write_twigs( [n( _, Pos), H|T], List, Pos1) :-
               node_number( H, 1),
               !,
               B is Pos - Pos1, write_n_times( ' ', B),
               write( '|'),
               Pos2 is Pos + 1,
               write_twigs( T, List1, Pos2),
               append( H, List1, List).

            % normal categories
          write_twigs( [H, [H2|T2]|T], List, Pos1) :-
               !,
               print_twigs( H, [H2|T2], Pos1, Pos2),
               write_twigs( T, List1, Pos2),
               append( [H2|T2], List1, List).
          write_twigs( [], [], _).

               print_twigs( n( _H, _Pos), List, Pos1, Pos4) :-
                    first_node( List, PosFirst),
                    B is PosFirst - Pos1, write_n_times( ' ', B),
                    write( /),
                    Pos2 is PosFirst + 1,
                    intermediate_node( List, Pos2, Pos3),
                    last_node( List, PosLast),
                    B2 is PosLast - Pos3 - 1, write_n_times( ' ', B2),
                    write( \),
                    Pos4 is PosLast.

                    intermediate_node( [n( _, _)|T], Pos1, Pos2) :-
                         print_twigs_for_node( T, Pos1, Pos2).

                         print_twigs_for_node( [n( _, _)], Pos, Pos)                :-  !.
                         print_twigs_for_node( [n( _, _), [_Last|_Succ]], Pos, Pos) :-  !.
                         print_twigs_for_node( [n( _, Pos)|T], Pos1, Pos2) :-
                              !,
                              B is Pos - Pos1, write_n_times( ' ', B),
                              write( '|'),
                              Pos3 is Pos + 1,
                              print_twigs_for_node( T, Pos3, Pos2).
                         print_twigs_for_node( [_|T], Pos1, Pos2) :-
                              print_twigs_for_node( T, Pos1, Pos2).

               node_number( [n( _, _)|T], 1) :-
                    \+ member_check( n( _, _), T).

%%% write_n_times( +Term, +N)
%%%   writes Term N times
%%%   ex:  hello, 2 --> hellohello
%
write_n_times( Term, N) :-
     N > 0,
     for( 1, N, _),
     write( Term),
     fail.
write_n_times( _, _).

%%% for( +From, +To, -Temp)
%%%   for loop
%
for( I, I, I) :- !.
for( I, _, I).
for( I, J, K) :-
  I1 is I + 1,
  for( I1, J, K).

%%% member_check( ?Element, ?List)
%%%   true if Element is a member of List (no backtracking)
%
member_check( E, [E|_]) :- !.
member_check( E, [_|T]) :-
     member_check( E, T).

%%% max( +X, +Y, -Max)
%%%   gets the max of X and Y
%
max( X, Y, X) :- X >= Y, !.
max( X, Y, Y) :- X <  Y.

%%% min( +X, +Y, -Min)
%%%   gets the min of X and Y
%
min( X, Y, X) :- X =< Y, !.
min( X, Y, Y) :- X >  Y.





jointag(Word,Tag,Out) :-
     name(Word,CW),
     name(Tag,CT),
     append(CW,[95],First),
     append(First,CT,CO),
     name(Out,CO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%end of tree_write%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% prettyprint(+List,-Indentation).
% prints out a complex structure nicely indented.
% This version leaves the last non-complex bracketting intact.    

% prettyprint(+List,-Indentation).
% prints out a complex structure nicely indented.
% This version leaves the last non-complex bracketting intact.    

% prettyprint(+List,-Indentation).
% prints out a complex structure nicely indented.
% This version leaves the last non-complex bracketting intact.    
