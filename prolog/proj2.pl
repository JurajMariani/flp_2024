/** FLP 2024 */
/** Assignment: Nondeterministic Turing Machine */
/** Author: Bc. Juraj Mariani, xmaria03 <xmaria03@stud.fit.vutbr.cz> */
/** Extensions: Random next state selection */

:- use_module(library(lists)).

/** To use random NextState selection, compile project using 'make ver8random' */
/** Please note that random has been tested on swipl ver 8.4.2 and may not work on other versions */
operace(turingMachineforAll).

:- dynamic nstate/4.
/** nstate(state, tapechar, nextstate, action) */


/** Reads input, asserts nstate predicate and returns the Tape */
handleInput(Tape) :-
    read_lines(Lines),
    length(Lines, NLines),
    TapeIdx is NLines - 1,
    nth0(TapeIdx, Lines, Tape),
    take(TapeIdx, Lines, Ruleset),
    assertRuleset(Ruleset).
    
/** Riven a rule in specified format, create a dynamic predicate */
assertRule(Rule) :-
    nth0(0, Rule, CurrState),
    nth0(2, Rule, CurrSymb),
    nth0(4, Rule, NextState),
    nth0(6, Rule, Action),
    assertz(nstate(CurrState, CurrSymb, NextState, Action)).

/** Given a list of rules, assert each one */
assertRuleset([]).
assertRuleset([Rule|T]) :-
    assertRule(Rule),
    assertRuleset(T).

/** Check if input is an even unsigned integer */
isEvenUint(Num) :-
    integer(Num),
    Num >= 0,
    Num mod 2 =:= 0.

/** Setters and checkers for start and end states */
startState('S').
endState('F').

/** If no next states are available, the machine should stop abnormally */
abnormalStop :-
    writeln("The Turing Machine has abnormally stopped."),
    halt(1).

/** Create a list containing first N elements */
take(0, _, []):- !.
take(N, [X|Xs], [X|Ys]) :-
    N1 is N - 1,
    take(N1, Xs, Ys).

/** Create a list without the first N elements */
drop(N, List, Res) :-
    length(Pref, N),
    append(Pref, Res, List).

/** Predicate to count leading blanks */
countLeadingBlanks([], 0).
countLeadingBlanks([H|T], Count) :-
    H == ' ', !,
    countLeadingBlanks(T, SubCount),
    Count is SubCount + 1.
countLeadingBlanks(_, 0).

/** Predicate to count trailing blanks */
countWOutTrailingBlanks(List, Count) :-
    reverse(List, Reversed),
    countLeadingBlanks(Reversed, CountBlank),
    length(List, ListLen),
    Count is ListLen - CountBlank.

/** Removes Blanks in front of the first and following the last symbol of the Tape */
removeExcessBlanks(Arr, NoBlankArr) :-
    countLeadingBlanks(Arr, LeadingCnt),
    drop(LeadingCnt, Arr, TmpArr),
    countWOutTrailingBlanks(TmpArr, NoTrailingCnt),
    take(NoTrailingCnt, TmpArr, NoBlankArr).


/** Construct "String" of current state */
/** To adhere to the assignment, leading and trailing blanks are not printed */
constructOutputString(State, TpeIdx, Tape, Otpt) :-
    take(TpeIdx, Tape, OtptList),
    /** on version 8. 4. 2. (version implemented on) used string_chars/2 */
    append(OtptList, [State], OtptList1),
    drop(TpeIdx, Tape, OtptList2),
    append(OtptList1, OtptList2, OtptBlanks),
    removeExcessBlanks(OtptBlanks, Otpt).

/** Print state as per assignment */
printCurrState(S, TI, T) :-
    constructOutputString(S, TI, T, Otpt),
    /** Transform list of atoms to a sequence of atoms */
    atom_chars(Output, Otpt),
    writeln(Output).

/** Check "capitalness" of a letter */
isUppercase(Char) :-
    /** Get Lower and Upper interval bound */
    char_code('A', L),
    char_code('Z', U),
    char_code(Char, C),
    C >= L,
    C =< U.

/** Add a Blank character to the fron of the tape */
/** Bumper */
addLeadingBlank(Tape, [' '|Tape]).

/** Add a Blank character to the end of the tape */
addTrailingBlank(Tape, NTape) :-
    append(Tape, [' '], NTape).

/** Move TapeIndex depending on the Action */
/** R - Right, L - Left */
executeTapeShift(Action, TapeIdx, Tape, NTape, NIdx) :-
    (Action == 'R' -> 
        NIdx is TapeIdx + 1,
        /** If the index should move past the last letter of the tape, add a Blank the end */
        length(Tape, TapeLen),
        (NIdx >= TapeLen -> addTrailingBlank(Tape, NTape) ; NTape = Tape)
    ; (
        Action == 'L' -> 
            NIdx is TapeIdx - 1,
            NTape = Tape,
            (TapeIdx < 0 -> writeln("ERROR: Attempt to move before the start of the tape."), halt(1) ; true)
        ;
            writeln("Unknown action detected"),
            halt(1)
      )
    ).

/** Replace a letter on the tape */
executeTapeReplacement(Action, TapeIdx, Tape, NTape) :-
    take(TapeIdx, Tape, Prefix),
    append(Prefix, [Action], PrefWAction),
    NextIdx is TapeIdx + 1,
    drop(NextIdx, Tape, Suffix),
    append(PrefWAction, Suffix, NTape).

/** Execute supplied action: either tape modification or index shift depending on the "capitalness" of the letter */
executeAction(Act, TpeIdx, Tape, NTape, Nidx) :-
    (isUppercase(Act) -> executeTapeShift(Act, TpeIdx, Tape, NTape, Nidx) ; executeTapeReplacement(Act, TpeIdx, Tape, NTape), Nidx is TpeIdx).

/** "ROZSIRENIE"/Uprava */
/** Instead of sequential next state selection we introduce a random selection */
/** Otherwise this is the same predicate as turingMachinforAll/5 */
/** REMOVED!!! Usable only when compiles 'make ver8random'/'make ver8random_big_stack' */

    
/** Go through alternative next steps to find a solution */
turingMachineforAll(_,_,_,[],[]).
turingMachineforAll(State, TapeIndex, Tape, [NextStep|Steps], Order) :-
    /** Try to continue using Rule NextStep */
    turingMachine(State, NextStep, TapeIndex, Tape, TmpOrder),
    /** The order received - TmpOrder is a valid order if has nonzero elements and ends with ['F'] */
    length(TmpOrder, TmpLen),
    operace(Op),
    (TmpLen == 0 -> 
        /** If NextStep returned an empty list, try other steps */
        call(Op, State, TapeIndex, Tape, Steps, Order)
    ; 
        /** If Simulation using NextStep returned a sequence of Transitions, look at the last one */
        /** Explanation from turingMachine/5: */
        /** If Next state with no possible continuations appears, it is put at the end of Order list */
        /** This is also true for the Final state which is useful in the following check */
        nth1(TmpLen, TmpOrder, LastTransition),
        nth0(0, LastTransition, LastState),
        (endState(LastState) ->
            /** If last state is F (final state), return TmpOrder with prefx NextStep */
            Order = TmpOrder
        ;
            /** If last state is not final step, prune this branch and try different NextState */
            call(Op, State, TapeIndex, Tape, Steps, Order)
        )
    ).


/** Execute Transition to next state and determine possible next states */
turingMachine(State, Transition, TapeIndex, Tape, Succession) :-
    /** In case of not reaching Final State, consult the Transition array length */
    length(Transition, TrLen),
    (TrLen == 2 ->
        /** Transitions with length 2 are normal reansitions */
        nth0(0, Transition, NextState),
        nth0(1, Transition, Action),
        executeAction(Action, TapeIndex, Tape, NTape, NewIdx)
    ;
        /** Transitions with other lengths (0 in particular) do not execute actions */
        /** This option is here for the start of computation, where ther eis no transaction */
        /** In this case the program merely finds possible future transitions */
        NewIdx = TapeIndex,
        NTape = Tape,
        NextState = State
    ),
    nth0(NewIdx, NTape, Symbol),
    /** Find unique pairs of [NextState, Action] for current state State and Symbol under the reading head */
    (setof([NS, NA], nstate(NextState, Symbol, NS, NA), NextArr) ->
        /** At least one pair has been found */
        operace(Op),
        call(Op, NextState, NewIdx, NTape, NextArr, Order),
        Succession = [Transition|Order]
    ;
        /** There are no possible steps, set the end of Succession list to current transaction */
        /** The significance of this is described in predicate turingMachineforAll/5 */
        Succession = [Transition]
    ).

/** Upon determining a series of steps to reach the Final step */
/** the steps are executed and each step is printed as per assignment */
simulateNondeterministicTraverse(State, [], TapeIndex, Tape) :-
    /** In case of last state, State should be F */
    printCurrState(State, TapeIndex, Tape).
simulateNondeterministicTraverse(State, [Step|Steps], TapeIndex, Tape) :-
    /** First print state */
    printCurrState(State, TapeIndex, Tape),
    /** Extract Next state and action to perform step */
    nth0(0, Step, NextState),
    nth0(1, Step, Action),
    executeAction(Action, TapeIndex, Tape, NTape, NewIdx),
    /** Continue with the simulation */
    simulateNondeterministicTraverse(NextState, Steps, NewIdx, NTape).

main :-
    prompt(_, ''),
    /** handle input and fill dynamic predicates, receive Tape */
    handleInput(TapeRaw),
    startState(S),
    addLeadingBlank(TapeRaw, TapeTmp),
    /** Trailing blank is added to combat possible empty tape. */
    addTrailingBlank(TapeTmp, Tape),
    turingMachine(S,[],1,Tape,OrderRaw),
    length(OrderRaw, SolutionLen),
    /** In case of failure to locate, [[]] is returned */
    (SolutionLen == 1 ->
        abnormalStop
    ;
        /** A valid solution has been found, simulate traverse based on succession */
        /** The first/zero-th element is an empty list because starting transition is an empty list */
        drop(1, OrderRaw, Order),
        simulateNondeterministicTraverse(S, Order, 1, Tape)
    ).
