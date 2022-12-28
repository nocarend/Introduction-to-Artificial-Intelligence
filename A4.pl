:- use_module(small_data).
% :- use_module(large_data).
:- use_module(print).
:- use_module(utils).
:- use_module(cost).

% Show the schedule of exams with least amount of penalties.
main :-
    findall(Exam, class_has_exam(Exam), Exams), % class_has_exam from *data, all exams.
    prepare_env(Exams), % from utils, who knows what it does.
    length(Exams, Length), % exams count.
    ucs_traverse([node([], 0, 0)], Length).

% State - current list of events.
% Goal - number of all exams.
% Permorm Dijkstra search algo.
ucs_traverse([node(State, Goal, _)| _], Goal) :-
    cost(schedule(State), Cost), % from cost, calculates schedule cost.
    format("\nTotal penalty: ~w \n\n\nSCHEDULE\n", [Cost]), % Write beginning of print feature.
    pretty_print(schedule(State)), % from print; print like in example.
    !. % if we found result, stop searching.

% Node - node(_, _, _) with minimal penalty.
% Rest - other nodes.
% Goal - needed length of schedule.
ucs_traverse([Node | Rest], Goal) :-
    findall(Neighbor, (neighbor(Node, Neighbor), estimate(Neighbor)), Neighbors), % find all derivative (from current node) nodes with additional event (exam), satisfying the conditions. 
    update(Neighbors, Rest, Frontier), % add all neighbors to search via ucs_traverse (dijkstra algo).
    ucs_traverse(Frontier, Goal).

% State - list of events.
% Length - length of list of events.
% NewState - new list of events with added exam.
% Cost - new schedule cost.
neighbor(node(State, Length, _), node(NewState, NewLength, Cost)) :-
    exam(Exam, _), % find any exam, satisfying conditions.
    \+member(event(Exam, _, _, _), State), % exam is not in schedule.
    classroom_available(Room, Day, Begin, End), % from *data; any classrom, satisfying conditions.
    exam_duration(Exam, Duration), % from *data; return exam duration.
    To is End - Duration, % we can choose only time, when classrom is free during all exam.
    between(Begin, To, Start), % random integer number between Begin and To.
    NewState = [event(Exam, Room, Day, Start) | State], % adding event to new schedule.
    NewLength is Length + 1,
    cost(schedule(NewState), Cost). % from cost; calculates new schedule cost.

% Given node, we want to chech that schedule with new event is satisfied all strict conditions.
estimate(node([event(Exam, Room, Day, From) | Schedule], _, _)) :-
    \+exam_loop(event(Exam, Room, Day, From), Schedule). % Check that all conditions are satisfied.

% ExamAdd - exam which wants to be added to schedule.
% We are going through all list and check that any of strict conditions is not satisfied.
exam_loop(ExamAdd, [Exam| _]) :-
    check_capacity(ExamAdd);
    conditions(ExamAdd, Exam).
% Check next exam.
exam_loop(ExamAdd, [_ | OtherExams]) :-
    exam_loop(ExamAdd, OtherExams).

% Check if any student's group is bigger than capacity of given classrom.
% Strict condition.
check_capacity(event(Exam, Room, _, _)) :-
    classroom_capacity(Room, Capacity), % from *data; calulates classroom capacity.
    st_group(Exam, Students), % from *data; calculates student's group list.
    length(Students, StudentsNumber), % calculates student's number.
    StudentsNumber > Capacity. % if capacity of classroom is lesser than StudentsNumber that is bad.

% ExamFirst - first exam.
% BeginFirst - beginning time of first exam.
% ExamSecond - second exam.
% BeginSecond - beginning time of second exam.
% RoomFirst - first classroom.
% RoomSecond - second classroom.
% Check strict conditions (classrom is free during all exam, two exams cannot intersect if student or teacher follows both).
conditions(event(ExamFirst, RoomFirst, Day, BeginFirst), event(ExamSecond, RoomSecond, Day, BeginSecond)) :-
    (student_follows_both_classes(ExamFirst, ExamSecond); % from utils; if student follows both exams in the same day and in the same time, that is bad.
    teacher_teaches_both_classes(ExamFirst, ExamSecond); % from utils; if teacher follows both exams in the same day and in the same time, that is bad.
    RoomFirst is RoomSecond), % One classroom cannot be free for two different exams.
    (
    exam_duration(ExamFirst, DurationFirst), % from *data; calculates exam duration.
    exam_duration(ExamSecond, DurationSecond), % same.
    EndFirst is BeginFirst + DurationFirst, % calcualtes end of first exam.
    EndSecond is BeginSecond + DurationSecond, % same, but for second.
    ((BeginSecond >= BeginFirst, BeginSecond =< EndFirst); % if two exams intersect each other.
    (BeginFirst >= BeginSecond, BeginFirst =< EndSecond))
    ).    

% Add to frontier new nodes.
update([], F, F). 

% NewNodes - list of new nodes.
% Frontier - old Frontier.
% NewFrontier - sorted frontier with new nodes, like in dijkstra algorithm(sort by minimal weight).
update(NewNodes, Frontier, NewFrontier) :-
    append(NewNodes, Frontier, Nodes),
    predsort(mycompare, Nodes, NewFrontier). % default sort doesn't work, idk why.

% X - relation between A1 and A2. Examples: <, >, =.
% A1 - penalty (weight) of first node.
% A2 - penalty (weight) of second node.
% Comparator for predsort.
mycompare(X, node(_, _, A1), node(_, _, A2)) :- 
    compare(X, A1, A2).
