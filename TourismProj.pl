% offerMean(X, Y) -> Transportation mean Y is used with offer X

offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), bus).

% offerAccommodation(X, Y) -> Accommodation Y is part of offer X

offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), hotel).

% customerPreferredActivity(X, Y, R) -> Y is the preferred activity kind wrt customer X with relevance R

customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50).

% customerPreferredMean(X, Y, R) -> Y is the preferred transportaion mean wrt customer X with relevance R

customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).

% customerPreferredAccommodation(X, Y, R) -> Y is the preferred accommodation to customer X with relevance R

customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel, 100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).


subset([], []).
subset([E|Tail], [E|NTail]):-
  subset(Tail, NTail).
subset([_|Tail], NTail):-
  subset(Tail, NTail).
perm([H|T],L) :- perm(T,P), insert(H,P,L). perm([],[]).
insert(X,L,[X|L]).
possibleSubset([],[]).
possibleSubset([H|T],R):-
	subset([H|T],R1),
	perm(R1,R).
choosePreferences([H|T],R):-
	possibleSubset([H|T],R).

remove(X1,[],[]). 
remove(X1,[X1|R],R2):- 
remove(X1,R,R2). 
remove(X1,[F|R],[F|S]) :-
X1\==F, remove(X1,R,S).

helperMean(Offer,Customer,ChosenPrefs,S):-

    offerMean(Offer,M),
    customerPreferredMean(Customer,M,R),
    member(means(M),ChosenPrefs),
    deleteElement(means(M),ChosenPrefs,NewPrefs),
    preferenceSatisfaction(Offer,Customer,NewPrefs,S1),
    S is R + S1.
	
helperAcc(Offer,Customer,ChosenPrefs,S1):-
	offerAccommodation(Offer,A),
    customerPreferredAccommodation(Customer,A,R),
    member(accommodation(A),ChosenPrefs),
    deleteElement(accommodation(A),ChosenPrefs,NewPrefs),
    preferenceSatisfaction(Offer,Customer,NewPrefs,S1),
    S is R + S1.
helperAct(Offer,Customer,ChosenPrefs,S2):-
    Offer=offer(_,ListOfActivities,_,_,_,_,_,_),
     member(activity(List),ChosenPrefs),
     List = [X|_],
     member(X,ListOfActivities),
     size(List,Size),
     Size = 1 ,
     customerPreferredActivity(Customer,X,R),
     deleteElement(activity(List),ChosenPrefs,NewPrefs),
     preferenceSatisfaction(Offer,Customer,NewPrefs,S1),
     S is R + S1.
helperAct(Offer,Customer,ChosenPrefs,S2):-
    Offer=offer(_,ListOfActivities,_,_,_,_,_,_),
     member(activity(List),ChosenPrefs),
     List = [X|_],
     member(X,ListOfActivities),
     size(List,Size),
     Size>1,
     deleteElement(X,List,NewList),
     customerPreferredActivity(Customer,X,R),
     %deleteElement(activity([X|_]),ChosenPrefs,NewPrefs),
     replaceWith(activity(List),activity(NewList),ChosenPrefs,NewPrefs),
     preferenceSatisfaction(Offer,Customer,NewPrefs,S1),
     S is R + S1.
preferenceSatisfaction(_,_,ChosenPrefs,0):-

   \+ member(accommodation(_),ChosenPrefs),
   \+ member(means(_),ChosenPrefs),
   \+ member(activity(_),ChosenPrefs).
preferenceSatisfaction(Offer,Customer,ChosenPrefs,SFinal):-
helperMean(Offer,Customer,ChosenPrefs,S),
helperAcc(Offer,Customer,ChosenPrefs,S1),
helperAct(Offer,Customer,ChosenPrefs,S2),
SFinal is S+S1+S2.



size([],0).
size([_|T],R):-
    size(T,R1),
    R is 1 + R1 .

replace_all([],_,_,[]). 
replace_all([X|T],X,Y,[Y|T2]) :- 
replace_all(T,X,Y,T2). 
replace_all([H|T],X,Y,[H|T2]) :- 
H \= X, replace_all(T,X,Y,T2).




