:-use_module(library(clpfd)).
unique([]):-!.
unique([Head|Tail]):-
   member(Head, Tail), !, fail;
   unique(Tail).
book(away).
book(call).
book(dessert).
book(wendallisa).

author(olson).
author(hansen).
author(jones).
author(stout).

count(12).
count(19).
count(26).
count(33).

sol(Solution) :- %Solution - это список троек вида КоличествоКопий-НазваниеКниги-Автор.
    Solution = [ACount-ABook-AAuthor,
                BCount-BBook-BAuthor,
                CCount-CBook-CAuthor,
                DCount-DBook-DAuthor],
    book(ABook), book(BBook), book(CBook), book(DBook),
    unique([ABook, BBook, CBook, DBook]),
    author(AAuthor), author(BAuthor), author(CAuthor), author(DAuthor),
    unique([AAuthor, BAuthor, CAuthor, DAuthor]),
    count(ACount), count(BCount), count(CCount), count(DCount),
    unique([ACount, BCount, CCount, DCount]),
    ((member(19-dessert-_, Solution),
      %first condition down there
      member(_-wendallisa-olson, Solution));
    (member(19-wendallisa-_, Solution),
     member(_-dessert-olson, Solution))),
      %second condition down there
      ((member(19-_-jones, Solution),
        member(33-away-_, Solution));
      (member(33-_-jones, Solution),
       member(19-away-_, Solution))),
      %third condition down there
      member(R1-wendallisa-_, Solution),
      member(R2-_-stout, Solution),
      R1-R2#=7.
