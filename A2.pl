:- prolog_load_context(directory,Dir), atom_concat(Dir, "C:/Program Files/WordNet/3.0", WNDB_), absolute_file_name(WNDB_, WNDB), asserta(user:file_search_path(wndb, WNDB)).
:- use_module("C:/Users/nickolas/Downloads/wnload/prolog/wn").
:- use_module(library(clpfd)).

% related_words( Start/Spos/Ssen/Ssyn, Goal/Gpos/Gsen/Gsyn, MaxDist, Solution) :-
%     wn_s(S, _, Start, Spos, Ssen, _),
%     wn_s(G, _, Goal, Gpos, Gsen, _),
%     breadthfirst( [ [Goal/Gpos/Gsen/G] ], Start/Spos/Ssen/S, Solution,  MaxDist).

% breadthfirst( [ [Node | Path] |_], Goal, [Node | Path], _) :-
%     Node==Goal.

% breadthfirst( [ [Nn/Nnpos/Nnsen/Nnsyn | Path] | Paths], Goal/Gpos/Gsen/Gsyn, Solution, MaxDist) :-
%     length(Path, L),
%     L #=<MaxDist - 1,
%     bagof([M/Mpos/Msen/Msyn, N/Npos/Nsen/Nsyn | Path],
%     (wn_s(Nsyn, _, N, Npos, Nsen, _),
%     wn_s(Msyn, _, M, Mpos, Msen, _),
%     ((wn_hyp(Msyn, Nsyn), \+ member(M/Mpos/Msen/Msyn, [N/Npos/Nsen/Nsyn | Path]));
%     (wn_mm(Msyn, Nsyn), \+ member(M/Mpos/Msen/Msyn, [N/Npos/Nsen/Nsyn | Path]));
%     (wn_mp(Msyn, Nsyn), \+ member(M/Mpos/Msen/Msyn, [N/Npos/Nsen/Nsyn | Path]));
%     (wn_hyp(Nsyn, Msyn), \+ member(M/Mpos/Msen/Msyn, [N/Npos/Nsen/Nsyn | Path]));
%     (wn_mm(Nsyn, Msyn), \+ member(M/Mpos/Msen/Msyn, [N/Npos/Nsen/Nsyn | Path]));
%     (wn_mp(Nsyn, Msyn), \+ member(M/Mpos/Msen/Msyn, [N/Npos/Nsen/Nsyn | Path])))),
%     NewPaths),
%     append(Paths, NewPaths, Pathsl), !,
%     breadthfirst( Pathsl, Goal/Gpos/Gsen/Gsyn, Solution, MaxDist);
%     breadthfirst( Paths, Goal/Gpos/Gsen/Gsyn, Solution, MaxDist).

solve( Start, Goal, MaxDist, Solution) :-
    wn_s(S, _, Start, _, _, _),
    wn_s(G, _, Goal, _, _, _),
    breadthfirst( [ [G] ], S, Solution,  MaxDist).

breadthfirst( [ [Node | Path] |_], Goal, [Node | Path], _) :-
    Node#=Goal.

breadthfirst( [ [N | Path] | Paths], Goal, Solution, MaxDist) :-
    length(Path, L),
    L #=<MaxDist - 1,
    bagof([M, N|Path],
    ((wn_hyp(M, N), \+ member(N, [M | Path]));
    (wn_mm(M, N), \+ member(N, [M | Path]));
    (wn_mp(M, N), \+ member(N, [M | Path]));
    (wn_hyp(N, M), \+ member(M, [N | Path]));
    (wn_mm(N, M), \+ member(M, [N | Path]));
    (wn_mp(N, M), \+ member(M, [N | Path]))),
    NewPaths),
    append(Paths, NewPaths, Pathsl), !,
    breadthfirst( Pathsl, Goal, Solution, MaxDist);
    breadthfirst( Paths, Goal, Solution, MaxDist).