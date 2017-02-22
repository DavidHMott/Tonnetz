/*  File:    tzgeometry.pl
    Author:  david
    Created: Dec 26 2016
    Purpose: Tonnetz geometry
*/

% the Tonnetz geometry
%
% We define the triangle with index numbers, and we create the geometric
% relations between them, independently of the note values. Thus we
% model the tonnetz as a geometric entity and we use its geometrc
% properties to do inferencing
%
% We define the set of indexes as being 2,1,10,9,18,17, ... along the
% top row

% We define a set of higher level relations to allow access to the basic
% information above geometric relations between the triangles and about
% the triangles

% The set of triangles
% !!! needs generalising
tz_istriangle(T):-
	member(T,
	       [1,2,3,4,5,6,7,8,9,10,
		11,12,13,14,15,16,17,18,19,20,
		21,22,23,24,25,26,27,28,29,30,
		31,32,33,34,35,36,37,38,39,40
	       ]).

% A triangle is an uptriangle (generator)
tz_isUp(I):-
	tz_istriangle(I),
	I1 is I mod 2,
	I1=0.

% A triangle is a down triangle (generator)
tz_isDown(I):-
	tz_istriangle(I),
	not(tz_isUp(I)).

% A triangle is in the core 24
tz_isCore(T):-
	member(T,[2,1,10,9,18,17,26,25,
		  4,3,12,11,20,19,28,27,
		  6,5,14,13,22,21,30,29]).

% otherwise a trinagle is wrapped (not a generator)
tz_isWrapped(T):-
	not(tz_isCore(T)).

% Some of the triangles are on an edge

tz_topEdge(T):-
	member(T,[1,2,9,10,17,18,25,26,33,34]).
tz_rightEdge(T):-
	member(T,[33,35,37,39]).
tz_bottomEdge(T):-
	member(T,[7,8,15,16,23,24,31,32,39,40]).
tz_leftEdge(T):-
	member(T,[2,4,6,8]).

% Each triangle has three other bordering triangles in three different
% directions. For a down triangle these are top, right, left;  for an up
% triangle these are right bottom left
%
% All of the arguments of these relations are
% triangle indexes (currently integers)
%
% These direction relations actually depend upon whether we are
% considering the layout on the page, or the theoretic torus which wraps
% around the triangles in 3d space. The following ...X relations define
% the physial layout not the torus
%
% Note that the wrap does not necessarily occur at the physical 2d edge!
%
% For an up triangle Arg1 has Arg2 at its bottom
%

% For an up triangle Arg1 has Arg2 as its left
tz_upLeftX(From,To):-
	tz_isUp(From),
	not(tz_leftEdge(From)),
	tz_dimensions(_,NRows),
	To is From-NRows*2-1.

% For an up triangle Arg1 has Arg2 at its right
tz_upRightX(From,To):-
	tz_isUp(From),
	To is From-1.

tz_upBottomX(From,To):-
	tz_isUp(From),
	not(tz_bottomEdge(From)),
	To is From+1.

% For a down triangle Arg1 has Arg2 as its top
tz_downTopX(From,To):-
	tz_isDown(From),
	not(tz_topEdge(From)),
	To is From-1.

% For a down triangle Arg1 has Arg2 as its right
tz_downRightX(From,To):-
	tz_isDown(From),
	not(tz_rightEdge(From)),
	tz_dimensions(_,NRows),
	To is From+NRows*2+1.

% For a down triangle Arg1 has Arg2 as its left
tz_downLeftX(From,To):-
	tz_isDown(From),
	To is From+1.

% Calculating the wrap over
% -------------------------
%

% one triangle is a wrap of another, given a core of 3 rows and 4 strips
% located at the top left
% Top edge wraps to bottom edge
% Left edge wraps to right edge but 1 row down
% !!!! no this is only true for certain dimensions
tz_wrap(T1,T2):-
	tz_topEdge(T1),
	T2 is T1+6.
tz_wrap(T1,T2):-
	tz_bottomEdge(T1),
	T2 is T1-6.
tz_wrap(T1,T2):-
	tz_leftEdge(T1),
	T2 is T1+34. % !!!! need to generalise!!!!
tz_wrap(T1,T2):-
	tz_rightEdge(T1),
	T2 is T1-34.

% now the wrapped versions of the triangle relations

tz_upLeft(From,To):-
	tz_upLeftX(From,To);
	(   tz_wrap(From,FromW),tz_upLeftX(FromW,To)).

tz_upRight(From,To):-
	tz_upRightX(From,To);
	(   tz_wrap(From,FromW),tz_upRightX(FromW,To)).

tz_upBottom(From,To):-
	tz_upBottomX(From,To);
	(   tz_wrap(From,FromW),tz_upBottomX(FromW,To)).


tz_downTop(From,To):-
	tz_downTopX(From,To);
	(   tz_wrap(From,FromW),tz_downTopX(FromW,To)).

tz_downRight(From,To):-
	tz_downRightX(From,To);
	(   tz_wrap(From,FromW),tz_downRightX(FromW,To)).

tz_downLeft(From,To):-
	tz_downLeftX(From,To);
	(   tz_wrap(From,FromW),tz_downLeftX(FromW,To)).


% Two triangles share a common root, ie the same root-fifth border
%
tz_commonRoot(T1,T2):-
	tz_isUp(T1),
	tz_upBottomX(T1,T2).
tz_commonRoot(T1,T2):-
	tz_isDown(T1),
	tz_downTopX(T1,T2).

% Define parts of a triad, (args are note names)
% ==============================================
% Strictly speaking these are not aspects of the tonnetz geometry
%
% The notes in each possible triad:
%   Rootnote, maj third, min third, fifth
tz_triad('C','E','Ef','G').
tz_triad('Cs','Es','E','Gs').

tz_triad('Df','F','E','Af').
tz_triad('D','Fs','F','A').
tz_triad('Ds','Fss','Fs','As').

tz_triad('Ef','G','Gf','Bf').
tz_triad('E','Gs','G','B').
tz_triad('F','A','Af','C').

tz_triad('Fs','As','A','Cs').
tz_triad('Gf','Bf','A','Df').
tz_triad('G','B','Bf','D').

tz_triad('Gs','C','B','Ds').
tz_triad('Af','C','Cf','Ef').

tz_triad('A','Cs','C','E').
tz_triad('Bf','D','Df','F').

tz_triad('B','Ds','D','Fs').

% simple enharmonic relationships (needs enhancing)
tz_enharmonic(V1,V2):-
	(   tz_enharmonic1(V1,V2);tz_enharmonic1(V2,V1)),
	!.

tz_enharmonic1('Ff','E').
tz_enharmonic1('Df','Cs').
tz_enharmonic1('Gf','Fs').
tz_enharmonic1('Cf','B').
tz_enharmonic1('As','Bf').


% get the various partof a triad
%
tz_fifthOf(RV,FV):-
	% either mode has same fifth
	tz_triad(RV,_,_,FV),
	!.
tz_fifthOf(RV,FV):-
	tz_enharmonic(RV,RV1),
	tz_triad(RV1,_,_,FV),
	!.
tz_fifthOf(RV,''):-
	format('No fifthof ~q~n',[RV]).

tz_majThirdOf(RV,TV):-
	tz_triad(RV,TV,_,_),
	!.
tz_majThirdOf(RV,TV):-
	tz_enharmonic(RV,RV1),
	tz_triad(RV1,TV,_,_),
	!.
tz_majThirdOf(RV,''):-
	format('No majthirdof ~q~n',[RV]).

tz_minThirdOf(RV,TV):-
	tz_triad(RV,_,TV,_),
	!.
tz_minThirdOf(RV,TV):-
	tz_enharmonic(RV,RV1),
	tz_triad(RV1,_,TV,_),
	!.
tz_minThirdOf(RV,''):-
	format('No minthirdof ~q~n',[RV]).

% relationships between notes
tz_majThirdDown(V1,V2):-
	tz_majThirdDown1(V1,V2),
	!.
tz_majThirdDown(V1,V2):-
	tz_enharmonic(V1,V11),
	tz_majThirdDown1(V11,V2),
	!.
tz_majThirdDown(V1,''):-
	format('no majthirddown from ~q~n',[V1]).

tz_majThirdDown1('C','Af').
tz_majThirdDown1('Af','Ff').
tz_majThirdDown1('Gs','E').
tz_majThirdDown1('E','C').

tz_majThirdDown1('Cs','A').
tz_majThirdDown1('A','F').
tz_majThirdDown1('F','Df').
tz_majThirdDown1('D','Bf').
tz_majThirdDown1('Bf','Gf').
tz_majThirdDown1('Fs','D').

tz_majThirdDown1('Ef','Cf').
tz_majThirdDown1('Ds','B').
tz_majThirdDown1('B','G').
tz_majThirdDown1('G','Ef').


% % Given a root and index of a triangle, return the other notes of the
% triangle.
% !!! how do we handle enharmonics?
tz_getOtherNotes(I,RV,TV,FV):-
	tz_fifthOf(RV,FV),
	(   tz_isUp(I) ->
	    tz_majThirdOf(RV,TV)
	|   otherwise ->
	    tz_minThirdOf(RV,TV)
	),
	!.
%tz_getOtherNotes(I,RV,TV,FV).


