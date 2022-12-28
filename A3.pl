/*
 * Let us now try to develop an interactive expert system utilizing hybrid chaining approach.
 * It means we start with no information at all about what user wants to know, and should
 * somehow pose hypothesis and get an information to work with.
 *
 * We also want our rules work not only with binary answers like 'yes' or 'no', but take
 * some values from user. The knowledge base we are about to implement consists of the
 * following IF-THEN rules:
 *
 * 1. If opponents have hero with disable skill then disablers is true.
 * 2. If opponents have hero with invisible skill then invis is true.
 * 3. if opponents have hero with high magic damage then magic is true.
 * 4. If opponents have hero with very high hit point value then fat is true.
 * 5. If opponents have hero with high physical damage and hero is melee attack type then melee_phys is true.
 * 6. If opponents have hero with high physical damage and hero is range attack type then range_phys is true.
 * 7. If you have matchmaking rating between 0 and 2500 then rating is low.
 * 8. If you have matchmaking rating between 2500 and 5000 then rating is middle.
 * 9. If you have matchmaking rating more than 5000 then rating is high.
 *10. If rating is low and opponents have heroes with every hero type (disabler, invisible, magic, fat, melee physical, range physical) then hard_heroes is true.
 *11. If hard_heroes is true and rating is low then pick result is anti mage.
 *12. If hard_heroes is true and rating is middle then pick result is ursa.
 *13. If hard_heroes is true and rating is high then pick result is spectre.
 *14. If melee physical is true and disablers is true then pick result is techies.
 *15. If range physical it true and melee physical is true and magic is true then pick result is troll warlord.
 *16. If magic is true and rating is low then pick result is lifestealer.
 *17. If magic is true and rating is middle then pick result is medusa.
 *18. If magic is true and rating is high then pick result is nyx assassin.
 *19. If invisible is true then pick result is bounty hunter.
 *20. If fat is true and rating is low then pick result is faceless void.
 *21. If fat is true and rating is not low then pick result is lifestealer.
 *22. If melee physical is true then pick result is omniknight.
 *23. If range physical is true then pick result is ursa.
 *24. If disablers is true and rating is low then pick result is bristleback.
 *25. If disablers is true and rating is middle then pick result is mars.
 *26. If disablers is true and rating is high then pick result is ember spirit.
 *
 * As we see, some properties have binary values ('disablers' and others are expected to be external true or not),
 * and some aren't of binary domain. For example, mmr coulde be low, middle or high.
 * When asking user to provide a property's value we should at the same time show the corresponding domain.
 * For example if asking about mmr:
 *
 * Of what type bill is ? Please choose one of the below:
 *  1. mmr_0_2500
 *  2. mmr_2500_5000
 *  3. mmr_5000_more
 *
 * We expect user to enter an integer from 1 to 3.
 *
 * Example:
 *
 * ?- start.
 * Hi!
 * 
 * What type opponent_team_hero_3 is? Type an integer or 'why'.
 * 1: pudge
 * 2: shadow_fiend
 * 3: sniper
 * 4: witch_doctor
 * 5: lion
 * 6: juggernaut
 * 7: phantom_assassin
 * 8: drow_ranger
 * 9: crystal_maiden
 *10: silencer
 *11: invoker
 *12: ogre_magi
 *13: legion_commander
 *14: axe
 *15: rubick
 *16: zeus
 *17: slark
 *18: marci
 *19: undying
 *20: primal_beast
 *21: morphling
 *22: tiny
 *23: leshrac
 *24: lina
 *25: tusk
 *26: arc_warden
 * |: 1
 * 
 * What type opponent_team_hero_2 is? Type an integer or 'why'.
 * 1: pudge
 * 2: shadow_fiend
 * 3: sniper
 * 4: witch_doctor
 * 5: lion
 * 6: juggernaut
 * 7: phantom_assassin
 * 8: drow_ranger
 * 9: crystal_maiden
 *10: silencer
 *11: invoker
 *12: ogre_magi
 *13: legion_commander
 *14: axe
 *15: rubick
 *16: zeus
 *17: slark
 *18: marci
 *19: undying
 *20: primal_beast
 *21: morphling
 *22: tiny
 *23: leshrac
 *24: lina
 *25: tusk
 *26: arc_warden
 * 11
 * 
 * What type opponent_team_hero_3 is? Type an integer or 'why'.
 * 1: pudge
 * 2: shadow_fiend
 * 3: sniper
 * 4: witch_doctor
 * 5: lion
 * 6: juggernaut
 * 7: phantom_assassin
 * 8: drow_ranger
 * 9: crystal_maiden
 *10: silencer
 *11: invoker
 *12: ogre_magi
 *13: legion_commander
 *14: axe
 *15: rubick
 *16: zeus
 *17: slark
 *18: marci
 *19: undying
 *20: primal_beast
 *21: morphling
 *22: tiny
 *23: leshrac
 *24: lina
 *25: tusk
 *26: arc_warden
 * 14
 * 
 * What type opponent_team_hero_4 is? Type an integer or 'why'.
 * 1: pudge
 * 2: shadow_fiend
 * 3: sniper
 * 4: witch_doctor
 * 5: lion
 * 6: juggernaut
 * 7: phantom_assassin
 * 8: drow_ranger
 * 9: crystal_maiden
 *10: silencer
 *11: invoker
 *12: ogre_magi
 *13: legion_commander
 *14: axe
 *15: rubick
 *16: zeus
 *17: slark
 *18: marci
 *19: undying
 *20: primal_beast
 *21: morphling
 *22: tiny
 *23: leshrac
 *24: lina
 *25: tusk
 *26: arc_warden
 * 23
 * 
 * What type opponent_team_hero_5 is? Type an integer or 'why'.
 * 1: pudge
 * 2: shadow_fiend
 * 3: sniper
 * 4: witch_doctor
 * 5: lion
 * 6: juggernaut
 * 7: phantom_assassin
 * 8: drow_ranger
 * 9: crystal_maiden
 *10: silencer
 *11: invoker
 *12: ogre_magi
 *13: legion_commander
 *14: axe
 *15: rubick
 *16: zeus
 *17: slark
 *18: marci
 *19: undying
 *20: primal_beast
 *21: morphling
 *22: tiny
 *23: leshrac
 *24: lina
 *25: tusk
 *26: arc_warden
 * why
 * 
 * To infer melee_phys be true, using rule
 *  (if opponent_team_hero_1 be juggernaut or opponent_team_hero_1 be phantom_assassin or opponent_team_hero_1
 *  be legion_commander or opponent_team_hero_1 be slark or opponent_team_hero_1 be marci or opponent_team_hero_1
 *  be tiny or opponent_team_hero_2 be juggernaut or opponent_team_hero_2 be phantom_assassin or opponent_team_hero_2 
 *  be legion_commander or opponent_team_hero_2 be slark or opponent_team_hero_...
 * 
 * What type opponent_team_hero_5 is? Type an integer or 'why'.
 * 1: pudge
 * 2: shadow_fiend
 * 3: sniper
 * 4: witch_doctor
 * 5: lion
 * 6: juggernaut
 * 7: phantom_assassin
 * 8: drow_ranger
 * 9: crystal_maiden
 *10: silencer
 *11: invoker
 *12: ogre_magi
 *13: legion_commander
 *14: axe
 *15: rubick
 *16: zeus
 *17: slark
 *18: marci
 *19: undying
 *20: primal_beast
 *21: morphling
 *22: tiny
 *23: leshrac
 *24: lina
 *25: tusk
 *26: arc_warden
 * 24
 * What type mmr is? Type an integer or 'why'.
 * 1: mmr_0_2500
 * 2: mmr_2500_5000
 * 3: mmr_5000_more
 * 2
 * Conclusion: result be lifestealer
 * Explanation: result be lifestealer<==(result be lifestealer<==(fat be true<==opponent_team_hero_1 be pudge))
 * true.
 */

:- use_module(library(clpfd)).

% As before we define dynamic procedures for data already derived, for questions already asked
% and for hypothesis already rejected.
:- dynamic derived/1.
:- dynamic asked/1.
:- dynamic rejected/1.

% Syntax of rules and proof trees.
:- op(800, xfx, <==).
:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).

% Clear dynamic database on program start up.
:- 
    retractall(derived(_)),
    retractall(rejected(_)),
    retractall(asked(_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% It would be handy to use binary predicate 'is' to denote properties' values, but,
% alas, 'is'/2 is a built-in ISO predicate, and it cannot be redefined. So we are forced
% to find an alternative. For example we can define a predicate 'be'/2. Using 'be' in rules
% makes them sound like being pronounced by some illiterate barbarians, but nothing better
% comes in mind.
:- op(100, xfx, be).

% Properties we can ask about. Each provided with its' domain. If domain consists of
% a single value then property is a binary one, otherwise we should show a menu.
askable(mmr, [mmr_0_2500, mmr_2500_5000, mmr_5000_more]).  % mmr value       
askable(opponent_team_hero_1, [pudge, shadow_fiend, sniper, witch_doctor, lion, juggernaut, phantom_assassin, drow_ranger, crystal_maiden,
 silencer, invoker, ogre_magi, legion_commander, axe, rubick, zeus, slark, marci, undying, primal_beast, morphling, tiny, leshrac, lina, tusk, arc_warden]). % first opponent hero         
askable(opponent_team_hero_2, [pudge, shadow_fiend, sniper, witch_doctor, lion, juggernaut, phantom_assassin, drow_ranger, crystal_maiden,
 silencer, invoker, ogre_magi, legion_commander, axe, rubick, zeus, slark, marci, undying, primal_beast, morphling, tiny, leshrac, lina, tusk, arc_warden]). % second opponent hero                                   
askable(opponent_team_hero_3, [pudge, shadow_fiend, sniper, witch_doctor, lion, juggernaut, phantom_assassin, drow_ranger, crystal_maiden,
 silencer, invoker, ogre_magi, legion_commander, axe, rubick, zeus, slark, marci, undying, primal_beast, morphling, tiny, leshrac, lina, tusk, arc_warden]). % third opponent hero                
askable(opponent_team_hero_4, [pudge, shadow_fiend, sniper, witch_doctor, lion, juggernaut, phantom_assassin, drow_ranger, crystal_maiden,
 silencer, invoker, ogre_magi, legion_commander, axe, rubick, zeus, slark, marci, undying, primal_beast, morphling, tiny, leshrac, lina, tusk, arc_warden]). % fourth opponent hero    
askable(opponent_team_hero_5, [pudge, shadow_fiend, sniper, witch_doctor, lion, juggernaut, phantom_assassin, drow_ranger, crystal_maiden,
 silencer, invoker, ogre_magi, legion_commander, axe, rubick, zeus, slark, marci, undying, primal_beast, morphling, tiny, leshrac, lina, tusk, arc_warden]). % fifth opponent hero                

% Production rules

if
    hard_heroes be true and rating be low
then
    result be anti_mage.

if 
    hard_heroes be true and rating be middle
then
    result be ursa.

if 
    hard_heroes be true and rating be high
then 
    result be spectre.

if
    disablers be true and invis be true and magic be true and melee_phys be true and range_phys be true and fat be true
then
    hard_heroes be true.

if
    melee_phys be true and disablers be true
then
    result be techies.

if 
    range_phys be true and melee_phys be true and magic be true
then
    result be troll_warlord.

if 
    magic be true and rating be low
then
    result be lifestealer.

if 
    magic be true and rating be middle
then
    result be medusa.

if 
    magic be true and rating be high
then
    result be nyx_assassin.

if 
    invis be true 
then
    result be bounty_hunter.

if
    fat be true and rating be low
then 
    result be faceless_void.
if 
    fat be true
then
    result be lifestealer.

if 
    melee_phys be true
then
    result be omniknight.

if 
    range_phys be true
then
    result be ursa.

if
    disablers be true and rating be low
then
    result be bristleback.

if 
    disablers be true and rating be middle
then
    result be mars.

if
    disablers be true and rating be high
then
    result be ember_spirit.

if 
    opponent_team_hero_1 be slark or opponent_team_hero_2 be slark or opponent_team_hero_3 be slark or opponent_team_hero_4 be slark or opponent_team_hero_5 be slark
    or opponent_team_hero_1 be invoker or opponent_team_hero_2 be invoker or opponent_team_hero_3 be invoker or opponent_team_hero_4 be invoker or opponent_team_hero_5 be invoker
then
    invis be true.

if 
    opponent_team_hero_1 be pudge or opponent_team_hero_1 be legion_commander or opponent_team_hero_1 be axe or opponent_team_hero_1 be undying or opponent_team_hero_1 be primal_beast or 
    opponent_team_hero_2 be pudge or opponent_team_hero_2 be legion_commander or opponent_team_hero_2 be axe or opponent_team_hero_2 be undying or opponent_team_hero_2 be primal_beast or 
    opponent_team_hero_3 be pudge or opponent_team_hero_3 be legion_commander or opponent_team_hero_3 be axe or opponent_team_hero_3 be undying or opponent_team_hero_3 be primal_beast or
    opponent_team_hero_4 be pudge or opponent_team_hero_4 be legion_commander or opponent_team_hero_4 be axe or opponent_team_hero_4 be undying or opponent_team_hero_4 be primal_beast or
    opponent_team_hero_5 be pudge or opponent_team_hero_5 be legion_commander or opponent_team_hero_5 be axe or opponent_team_hero_5 be undying or opponent_team_hero_5 be primal_beast
then
    fat be true.

if 
    opponent_team_hero_1 be shadow_fiend or opponent_team_hero_1 be sniper or opponent_team_hero_1 be drow_ranger or opponent_team_hero_1 be silencer or opponent_team_hero_1 be morphling or opponent_team_hero_1 be arc_warden or 
    opponent_team_hero_2 be shadow_fiend or opponent_team_hero_2 be sniper or opponent_team_hero_2 be drow_ranger or opponent_team_hero_2 be silencer or opponent_team_hero_2 be morphling or opponent_team_hero_2 be arc_warden or
    opponent_team_hero_3 be shadow_fiend or opponent_team_hero_3 be sniper or opponent_team_hero_3 be drow_ranger or opponent_team_hero_3 be silencer or opponent_team_hero_3 be morphling or opponent_team_hero_3 be arc_warden or 
    opponent_team_hero_4 be shadow_fiend or opponent_team_hero_4 be sniper or opponent_team_hero_4 be drow_ranger or opponent_team_hero_4 be silencer or opponent_team_hero_4 be morphling or opponent_team_hero_4 be arc_warden or
    opponent_team_hero_5 be shadow_fiend or opponent_team_hero_5 be sniper or opponent_team_hero_5 be drow_ranger or opponent_team_hero_5 be silencer or opponent_team_hero_5 be morphling or opponent_team_hero_5 be arc_warden
then
    range_phys be true.

if
    opponent_team_hero_1 be juggernaut or opponent_team_hero_1 be phantom_assassin or opponent_team_hero_1 be legion_commander or opponent_team_hero_1 be slark or opponent_team_hero_1 be marci or opponent_team_hero_1 be tiny or
    opponent_team_hero_2 be juggernaut or opponent_team_hero_2 be phantom_assassin or opponent_team_hero_2 be legion_commander or opponent_team_hero_2 be slark or opponent_team_hero_2 be marci or opponent_team_hero_2 be tiny or
    opponent_team_hero_3 be juggernaut or opponent_team_hero_3 be phantom_assassin or opponent_team_hero_3 be legion_commander or opponent_team_hero_3 be slark or opponent_team_hero_3 be marci or opponent_team_hero_3 be tiny or
    opponent_team_hero_4 be juggernaut or opponent_team_hero_4 be phantom_assassin or opponent_team_hero_4 be legion_commander or opponent_team_hero_4 be slark or opponent_team_hero_4 be marci or opponent_team_hero_4 be tiny or
    opponent_team_hero_5 be juggernaut or opponent_team_hero_5 be phantom_assassin or opponent_team_hero_5 be legion_commander or opponent_team_hero_5 be slark or opponent_team_hero_5 be marci or opponent_team_hero_5 be tiny
then
    melee_phys be true.

if 
    opponent_team_hero_1 be witch_doctor or opponent_team_hero_1 be shadow_fiend or opponent_team_hero_1 be lion or opponent_team_hero_1 be crystal_maiden or opponent_team_hero_1 be invoker or opponent_team_hero_1 be rubick or opponent_team_hero_1 be zeus or opponent_team_hero_1 be leshrac or opponent_team_hero_1 be lina or opponent_team_hero_1 be arc_warden or
    opponent_team_hero_2 be witch_doctor or opponent_team_hero_2 be shadow_fiend or opponent_team_hero_2 be lion or opponent_team_hero_2 be crystal_maiden or opponent_team_hero_2 be invoker or opponent_team_hero_2 be rubick or opponent_team_hero_2 be zeus or opponent_team_hero_2 be leshrac or opponent_team_hero_2 be lina or opponent_team_hero_2 be arc_warden or
    opponent_team_hero_3 be witch_doctor or opponent_team_hero_3 be shadow_fiend or opponent_team_hero_3 be lion or opponent_team_hero_3 be crystal_maiden or opponent_team_hero_3 be invoker or opponent_team_hero_3 be rubick or opponent_team_hero_3 be zeus or opponent_team_hero_3 be leshrac or opponent_team_hero_3 be lina or opponent_team_hero_3 be arc_warden or
    opponent_team_hero_4 be witch_doctor or opponent_team_hero_4 be shadow_fiend or opponent_team_hero_4 be lion or opponent_team_hero_4 be crystal_maiden or opponent_team_hero_4 be invoker or opponent_team_hero_4 be rubick or opponent_team_hero_4 be zeus or opponent_team_hero_4 be leshrac or opponent_team_hero_4 be lina or opponent_team_hero_4 be arc_warden or
    opponent_team_hero_5 be witch_doctor or opponent_team_hero_5 be shadow_fiend or opponent_team_hero_5 be lion or opponent_team_hero_5 be crystal_maiden or opponent_team_hero_5 be invoker or opponent_team_hero_5 be rubick or opponent_team_hero_5 be zeus or opponent_team_hero_5 be leshrac or opponent_team_hero_5 be lina or opponent_team_hero_5 be arc_warden
then
    magic be true.

if
    opponent_team_hero_1 be pudge or opponent_team_hero_1 be witch_doctor or opponent_team_hero_1 be lion or opponent_team_hero_1 be ogre_magi or opponent_team_hero_1 be axe or opponent_team_hero_1 be rubick or opponent_team_hero_1 be leshrac or opponent_team_hero_1 be tusk or
    opponent_team_hero_2 be pudge or opponent_team_hero_2 be witch_doctor or opponent_team_hero_2 be lion or opponent_team_hero_2 be ogre_magi or opponent_team_hero_2 be axe or opponent_team_hero_2 be rubick or opponent_team_hero_2 be leshrac or opponent_team_hero_2 be tusk or
    opponent_team_hero_3 be pudge or opponent_team_hero_3 be witch_doctor or opponent_team_hero_3 be lion or opponent_team_hero_3 be ogre_magi or opponent_team_hero_3 be axe or opponent_team_hero_3 be rubick or opponent_team_hero_3 be leshrac or opponent_team_hero_3 be tusk or
    opponent_team_hero_4 be pudge or opponent_team_hero_4 be witch_doctor or opponent_team_hero_4 be lion or opponent_team_hero_4 be ogre_magi or opponent_team_hero_4 be axe or opponent_team_hero_4 be rubick or opponent_team_hero_4 be leshrac or opponent_team_hero_4 be tusk or
    opponent_team_hero_5 be pudge or opponent_team_hero_5 be witch_doctor or opponent_team_hero_5 be lion or opponent_team_hero_5 be ogre_magi or opponent_team_hero_5 be axe or opponent_team_hero_5 be rubick or opponent_team_hero_5 be leshrac or opponent_team_hero_5 be tusk
then
    disablers be true.

if
    mmr be mmr_be_0_2500
then
    rating be low.

if 
    mmr be mmr_2500_5000
then
    rating be middle.

if
    mmr be mmr_5000_more
then
    rating be high.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Backward chain inference engine. Looks familiar with what we already did. Note that here we
% work non only with binary rules, so we need to develop two ways of asking questions and processing
% answers.
true(Statement, Proof) :- 
                            retractall(derived(_)),
                            retractall(asked(_)),
                            retractall(rejected(_)),
                            true(Statement, Proof, []).

true(Statement, Statement, _) :- derived(Statement).
true(S1 and S2, P1 and P2, Trace) :-
                                    true(S1, P1, Trace),
                                    true(S2, P2, Trace).
true(S1 or S2, P, Trace) :-
                                    true(S1, P, Trace) ;
                                    true(S2, P, Trace).
true(Conclusion, Conclusion <== ConditionProof, Trace) :-
                                                        if Condition then Conclusion,
                                                        true(Condition, ConditionProof, [if Condition then Conclusion | Trace]).
true(Statement, Proof, Trace) :-
                                    Statement = Subject be _,
                                    askable(Subject, Menu),
                                    \+ derived(Statement),
                                    \+ asked(Subject),
                                    ask(Statement, Subject, Proof, Trace, Menu).

% Ask question in case of non-binary property. A property is detected to be non0binary if its' domain has more than one value.
%
% ask(+Statement, +Subject, -Proof, -Trace, +Menu)
% Statement: current proposition we are trying to derive, asking a question. For example: whether nostrils are external_tubular ?.
% Subject  : subject of the proposition we ask about. For example if we are asking whether nostrils are external tubular, the subject
%            would be 'nostrils'. When asking about a bill, bill is the subject.
% Proof    : Current proof tree.
% Trace    : Trace of rules we use to answer the WHY-question.
% Menu     : Subject's domain as a list. Domains are defined in predicate askable/2.

ask(Statement, Subject, Proof, Trace, [V,V1|Vs]) :-
                                Menu = [V,V1|Vs],
                                format('\n What type ~w is? Type an integer or \'why\'.\n', [Subject]),
                                show_menu(1, Menu),
                                read_string(user_input, "\n", "\r\t", _, Answer),
                                process(Answer, Statement, Subject, Proof, Trace, Menu).

% Shows menu as a enumerated list of values.
% For example:
%
% 1. Val1
% 2. Val2
% 3. Val3
show_menu(_, []).
show_menu(Counter, [V|Vs]) :-
                            format('~d: ~w\n', [Counter, V]),
                            Next #= Counter + 1,
                            show_menu(Next, Vs).

% Process non-binary answer.
%
% process(+Answer, +Statement, +Subject, -Proof, -Trace, +Menu)
% Answer   : an answer got from a user. It could be 'why' or some integer
% Statement: statement we are trying to derive
% Subject  : property we are asking about (nostrils, feet, wings etc).
% Proof    : Statement's proof tree
% Trace    : list of rules we are about to use to derive the Statement
% Menu     : Domain
process("why", St, S, Proof, Trace, Menu) :- !,
                                        show_reasoning_chain(Trace, 0), nl,
                                        ask(St, S, Proof, Trace, Menu).

% Note the last expression in the predicate. We should check if we derived what we are trying to derive.
% Let us suppose we want to confirm a hypothesis that order is tubenose and to do so we ask of what type
% bill is. In order to derive that order is tubenose we expect bill to be hooked, meaning that the
% Statement here is (bill be hooked). But user can choose any value from the menu. Let it be, for example,
% (bill be short). So, the Proposition we have just derived is (bill is short), and Statement we wanted to
% derive is (bill is hooked).
process(StrInd, St, S, Proof <== is_stated, _, Menu) :-
                                            atom_number(StrInd, Index),     % convert string to integer I
                                            nth1(Index, Menu, Answer),      % get I-th element from the domain if it exists
                                            !,
                                            Proposition = S be Answer,      % Make a proposition from user's answer
                                            Proof = Proposition,            % Proof of the proposition is proposition itself
                                            asserta(derived(Proposition)),  % Save the information that the proposition is derived
                                            asserta(asked(S)),              % Save the information that the question was asked
                                            St == Proposition.              % Check if we derived what we want to derive.

% Process incorrect answers.
process(_, Statement, Subject, Proof, Trace, Menu) :-
                                            write('Incorrect answer! Try again, please\n'),
                                            ask(Statement, Subject, Proof, Trace, Menu).

show_reasoning_chain([], _).
show_reasoning_chain([if Cond then Concl | Rules], _) :-
                                                        format('\n   To infer ~w, using rule\n   (if ~w then ~w)', [Concl, Cond, Concl]),
                                                        show_reasoning_chain(Rules, _).


% User interaction section.

% Check if some Conclusion is derivable.
%
% derivable(?Condition, -Conclusion, -ConclusionProofTree)
% Condition          : A piece of currently known information (could be unbound)
% Conclusion         : A hypothesis we want to confirm or reject, based on currently known information
% ConclusionProofTree: If hypothesis is confirmed ConclusionProofTree is a assigned with the corresponding proof tree
derivable(CondPart, Concl, Concl <== How) :-
                                if Cond then Concl,             % looking for a rule to apply
                                contains_term(CondPart, Cond),  % check if Condition occurs in the IF-path of the rule
                                \+ derived(Concl),
                                \+ rejected(Concl),
                                % If Concl is true then the hypothesis is confirmed, otherwise it is rejected
                                (
                                    true(Concl, How, []) -> !, asserta(derived(Concl)); asserta(rejected(Concl)), fail
                                ).

% infer(+CurrentCondition, -Hypothesis, +CurrentExplanation, -NextExplanation)
% CurrentCondition    : A piece of information we use to make a hypothesis
% Hypothesis          : A hypothesis we make based on the CurrentCondition
% CurrentExplanation  : Current proof tree
% NextExplanation     : A proof tree we will make in case the hypothesis is confirmed
infer(Cond, Concl, Prev, Expl) :-
                            derivable(Cond, Concl1, Trace),     % check if hypothesis is derivable
                            !,
                            infer(Concl1, Concl, Trace, Expl) ; % if so, use it as a new condition
                            Expl = Prev,                        % otherwise the final proof tree is equal to the current one
                            Concl = Cond.                       % and return the current Condition as the last information we were able to derive.


infer(Conclusion, Proof) :-
                            infer(_, Conclusion, 'Can\'t infer something from the information provided.' , Proof), !.



start :-
            retractall(derived(_)),
            retractall(rejected(_)),
            retractall(asked(_)),
            write('Hi!\n'),
            infer(Conclusion, How),
            format('Conclusion: ~w\nExplanation: ~w', [Conclusion, How]).


clear :- retractall(derived(_)), retractall(rejected(_)), retractall(asked(_)).
