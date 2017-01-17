

:- use_module(library(pce)).
:- use_module(library(find_file)).

% global flags
%
:-dynamic(tz_playmidi/1).
tz_playmidi(false).

% last triangle played, and the specific notes used
:-dynamic(tz_lastTrianglePlayed/2).

% parameters for sizes
% screen size
tz_param_screensize(1650,700).
tz_param_left_top(500,100).
% Triangle size
tz_param_width(230).
tz_param_height(250).
% Note font size
tz_param_note_size(24).
% triangle label size for chord
tz_param_chord_size(14).
% transformation size
tz_param_trans_size(20).

tz_param_note_col(blue).

% tonnetz dimensions(NStrips,NRows)
% Note there are two triangles per row in a strip
tz_dimensions(5,4).
% triad name of top left
tz_topleft_triad('Fs').



:-[tzgeometry].

:-['tz_arrow_class','tz_weitzmann_class','tz_strip_class',
   'tz_triangle_class','tz_tonnetz_class','tz_diatonic_class'].

tz_mainScreen:-
	tz_param_screensize(X,Y),

	new(F, frame('Tonnetz')),
	%send(F,height,1000),
	%send(F,width,2000),

	new(TZ,tonnetz('tonnetz',X,Y)),
	send(F,append(TZ)),

	new(D,dialog),
	new(MUS_TI,text_item('')),
	send(MUS_TI,length,80),
	Font=font(calibri,bold,14),
	send(MUS_TI,value_font,Font),

	send(D,append(MUS_TI,right)),

	new(PLAY,button(noplay)),
	send(PLAY,message,message(@prolog,tz_play,PLAY)),
	send(D,append(button(reset,message(TZ,reset)),right)),
	send(D,append,PLAY,right),
%	send(D,append(button(index,message(@prolog,tz_indexes,@arg1)),right)),
%
	send(D,append(button(playroute,message(TZ,playroute)),right)),
	send(D,append(button(writeroute,message(TZ,writeroute)),right)),
	send(D,append(button(save,message(TZ,save)),right)),
	send(D,append(button(load,message(TZ,load)),right)),

	send(D,below,TZ),

	send(TZ,put_music_ti,MUS_TI),

	send(TZ,open),

	tz_param_left_top(L,T),
	tz_dimensions(NStrips,NRows),
	send(TZ,draw,NStrips,NRows,L,T),

	tz_topleft_triad(TLT),
	send(TZ,setupNoteOrigin,TLT),

	tz_param_width(TW),
	tz_param_height(TH),

	% setup the diatonic area, !!! this needs to be generalised
	new(DT,tz_diatonic(TZ)),
	send(DT,draw,L,T,TW,TH,20),
	send(TZ,adddiatonic,DT),


	retractall(tz_lastTrianglePlayed(_,_)),

	retractall(tz_playmidi(__)),
	assert(tz_playmidi(false)).

tz_play(Button):-
%	trace,
	% flip
	tz_playmidi(V),
	retractall(tz_playmidi(_)),
	(   V=true ->
	    assert(tz_playmidi(false)),
	    send(Button,label,noplay)
	|   otherwise ->
	    assert(tz_playmidi(true)),
	    send(Button,label,play)
	),
	!.
tz_play(Button):-
	assert(tz_playmidi(false)),
	send(Button,label,noplay).



% ENTRY POINT TO CALCULATING TRANSFORMS
% Get all of the close transformations to a triangle
%
tz_closeTransformations(I,Trans):-
	findall(T,(tz_isTransform(Type,Dir,I,To),
		   tz_transform(Type,Dir,_,VDist),
		   T=(Type,Dir,I,To,VDist)
		  ),
		Trans0),
	% remove multiple transforms to wrapped triangles
	tz_removeDuplicateTransforms(Trans0,Trans).

% PRIMARY TRANSFORMS
% L transform along the maj third, 1 voice dist
tz_isTransform('L',-,From, To):-
	tz_istriangle(From),
	tz_isUp(From),
	tz_upRight(From,To).
%	To is From-1.
tz_isTransform('L',+,From, To):-
	tz_istriangle(From),
	tz_isDown(From),
	tz_downLeft(From,To).
%	To is From+1.


% P transform is from one mode to another, 1 voice dist
tz_isTransform('P',-,From, To):-
	tz_istriangle(From),
	tz_isUp(From),
%	To is From+1.
        tz_upBottom(From,To).
% P transform is from one mode to another, 1 voice dist
tz_isTransform('P',+,From, To):-
	tz_istriangle(From),
	tz_isDown(From),
	tz_downTop(From,To).
%	To is From-1.

% R transform is from relative major to minor, 2 voice dist
tz_isTransform('R',+,From, To):-
	tz_istriangle(From),
	tz_isUp(From),
	tz_upLeft(From,To).
%	To is From-7.
tz_isTransform('R',-,From, To):-
	tz_istriangle(From),
	tz_isDown(From),
	tz_downRight(From,To).
%	To is From +7.

% SECONDARY TRANSFORMS
% N transform
% this involves several relations
tz_isTransform('N',+,From, To):-
	tz_istriangle(From),
	tz_isUp(From),
	tz_upLeft(From,T0),
	tz_downLeft(T0,T1),
	tz_upBottom(T1,To).
%	To is From-5.
tz_isTransform('N',-,From, To):-
	tz_istriangle(From),
	tz_isDown(From),
	tz_downTop(From,T1),
	tz_upRight(T1,T0),
	tz_downRight(T0,To).
%	To is From+5.
% S transform
tz_isTransform('S',-,From, To):-
	tz_istriangle(From),
	tz_isUp(From),
	tz_upLeft(From,T0),
	tz_downTop(T0,T1),
	tz_upRight(T1,To).
tz_isTransform('S',+,From, To):-
	tz_istriangle(From),
	tz_isDown(From),
	tz_downLeft(From,T0),
	tz_upBottom(T0,T1),
	tz_downRight(T1,To).

% COMPOUND TRANSFORMS
% PL transform
tz_isTransform('PL',-,From, To):-
	tz_istriangle(From),
	tz_isUp(From),
	tz_isTransform('P',-,From, To0),
	tz_isTransform('L',+,To0, To).
tz_isTransform('PL',+,From, To):-
	tz_istriangle(From),
	tz_isDown(From),
	tz_isTransform('P',+,From, To0),
	tz_isTransform('L',-,To0, To).

tz_isTransform('LP',-,From, To):-
	tz_istriangle(From),
	tz_isDown(From),
	tz_isTransform('L',+,From, To0),
	tz_isTransform('P',-,To0, To).
tz_isTransform('LP',+,From, To):-
	tz_istriangle(From),
	tz_isUp(From),
	tz_isTransform('L',-,From, To0),
	tz_isTransform('P',+,To0, To).

/*% SP transform
tz_isTransform('SP',-,From, To):-
	tz_isTransform('S',-,From, To0),
	tz_isTransform('P',+,To0, To).
tz_isTransform('SP',+,From, To):-
	tz_isTransform('S',+,From, To0),
	tz_isTransform('P',-,To0, To).
*/

% null transform
tz_isTransform('','',From, From).

% search for extra possible transforms between T1 and T2
% based on the primary and secondary transforms above

% two step
tz_findtransform(2,From,To,[T1,D1,T2,D2]):-
	tz_isTransform(T1,D1,From,Int),
	not(T1=''),
	tz_isTransform(T2,D2,Int,To),
	not(T2='').

tz_findtransforms(From,To,Trans):-
	findall(TS,
		(   tz_findtransform(2,From,To,[T1,_D1,T2,_D2]),
		    concat_atom([T1,T2],TS)
		),
		Trans0),
	% check for extra diatonics
	findall(DT,(member(T,Trans0),
		    tz_diatonictransform(_Mode,T,DT)
		   ),
		DTs0),
	sort(DTs0,DTs),
	flatten([Trans0,DTs,[?]],Trans),
%	append(Trans0,[?],Trans),
	!.
tz_findtransforms(_,_,[?]).

% extra diatonic transformations, in the context of a major or minor
% mode, and depending on direction
tz_diatonictransform(major,'R','SubM').
tz_diatonictransform(major,'L','Med').
tz_diatonictransform(major,'LR','Dom').
tz_diatonictransform(major,'RL','Sub').
tz_diatonictransform(minor,'N','Dom').
tz_diatonictransform(minor,'R','Med').
tz_diatonictransform(minor,'L','SubM').


%	Trans=['N','?'].

% Remove multiple transforms to wrapped triangles
% ie if there is a transform to a non wrapped triangle prefer it to the
% same transform type to a wrapped triangle
%

tz_removeDuplicateTransforms(Trans0,Trans):-
	% get the non-wrapped transforms
	tz_nonWrappedTransforms(Trans0,NWTrans),
	% get those other transforms that are duplicates of a non-wrapped one
	tz_dupWrappedTransforms(Trans0,NWTrans,Dups),
	% remove the dups
	subtract(Trans0,Dups,Trans).

tz_nonWrappedTransforms(Trans0,NWTrans):-
	findall(T,(member(T,Trans0),
		   T=(_,_,_,To,_),
		   tz_isCore(To)
		  ),
		NWTrans).

tz_dupWrappedTransforms(Trans0,NWTrans,Dups):-
	findall(T,(member(T,Trans0),
		   T=(_,_,_,To,_),
		   tz_isWrapped(To),
		   tz_dupedTransform(T,NWTrans)
		  ),
		Dups).

% is this T a duped transform, ie has the same type and dir?
tz_dupedTransform(T,NWTrans):-
	T=(Type,Dir,_,To,_),
	% there is another transform
	member(NWT,NWTrans),
	% of the same type and dir
	NWT=(Type,Dir,_,To1,_),
	% but a different destination
	not(To=To1).

% Working out the voice movements
% ===============================
%
tz_transform(Type,Dir,Info,VD):-
	tz_transform1(Type,Deltas,VD),
	Deltas=[A1:AD0:A2,B1:BD0:B2,C1:CD0:C2],
	(   Dir= + ->
	    OldA=A1,
	    NewA=A2,
	    AD=AD0,
	    OldB=B1,
	    NewB=B2,
	    BD=BD0,
	    OldC=C1,
	    NewC=C2,
	    CD=CD0
	|   otherwise ->
	    OldA=A2,
	    NewA=A1,
	    AD is 0 - AD0,
	    OldB=B2,
	    NewB=B1,
	    BD is 0 - BD0,
	    OldC=C2,
	    NewC=C1,
	    CD is 0 - CD0
	),
	Info=[OldA:AD:NewA,OldB:BD:NewB,OldC:CD:NewC].


% the movement of each note i the + direction, in form
% OldPlace:Delta:NewPlace
% Also holds the absolute total distance
%
tz_transform1('L',[1:0:3,3:0:5,5:1:1],1).
tz_transform1('N',[1:0:5,3:1:1,5:1:3],2).
tz_transform1('P',[1:0:1,3:1:3,5:0:5],1).
tz_transform1('R',[1:0:3,3:0:5,5:2:1],2).
tz_transform1('S',[1:(-1):1,3:0:3,5:(-1):5],2).
tz_transform1('PL',[1:4:1,3:4:3,5:4:5],2).
tz_transform1('LP',[1:4:1,3:4:3,5:4:5],2).
%tz_transform1('SP',[1:1:1,3:1:3,5:1:5],3).

% for completeness, the null transform
tz_transform1('',[1:0:1,3:0:3,5:0:5],0).

% apply a transform to a list of notes in form [N:135,...], where they
% may be in a inverted form
%
tz_applyTransformToNumber(Type, Dir,Notes, NewNotes):-
	Notes = [A:AF,B:BF,C:CF],
	format('Old notes ~q~n',[Notes]),

	tz_transform(Type,Dir,Deltas,_),
	format('Deltas=~q~n',[Deltas]),
%	Deltas=[A1:AD:A2,B1:BD:B2,C1:CD:C2],

	% add the deltas to each note acording to its function
	% ANew etc is the new function of this note
	member(AF:AD:ANew,Deltas),
	A1 is A + AD,
	tz_inRangeNoteNo(A1,A2),

	member(BF:BD:BNew,Deltas),
	B1 is B + BD,
	tz_inRangeNoteNo(B1,B2),
	member(CF:CD:CNew,Deltas),
	C1 is C + CD,
	tz_inRangeNoteNo(C1,C2),

	format('First note now ~q:~q~n',[A2,ANew]),
	format('Second note now ~q:~q~n',[B2,BNew]),
	format('Third note now ~q:~q~n',[C2,CNew]),

	NewNotes=[A2:ANew,B2:BNew,C2:CNew],
	format('New notes ~q~n',[NewNotes]),
	!.


tz_inRangeNoteNo(V,V1):-
	(   V =<0 -> V1 is V + 12
	|   V >12 -> V1 is V -12
	| true -> V1 = V).

% given a set of named notes in form Note:Function (1/3/5), apply the
% transform
tz_applyTransformToNotes(Type, Dir, NamedNotes, NewNamedNotes):-
	findall(X,
		(   member(N:F,NamedNotes),
		    tz_noteNumber(N,NN),
		    X=NN:F
		),
		NumberedNotes),

	tz_applyTransformToNumber(Type, Dir,NumberedNotes, NewNumberedNotes),
	findall(X,
		(   member(NN:F,NewNumberedNotes),
		    tz_noteNumber(N,NN),
		    X=N:F
		),
		NewNamedNotes),
	!.

% Calculating minimum voice leading between two triads

tz_minVoiceLeading(Notes1,Notes2,_Min):-
	tz_vl_options(Notes1,Notes2,Options),
	format('~q~n',[Options]).

tz_vl_options(Notes1,Notes2,Options):-
	findall(X,
		(   member(N1,Notes1),
		    member(N2,Notes2),
		    X=N1-N2
		),
		Options).

/*
% add deltas to the notes taking account of the placing of
% the notes in the chord ie N:1/3/5
tz_addvoicedeltas(Notes,[RDelta,TDelta,FDelta],Notes1):-
	Notes=(A:AN,B:BN,C:CN),
	Notes0 = [A:AN,B:BN,C:CN],
	member(RV:1,Notes0),
	member(TV:3,Notes0),
	member(FV:5,Notes0),
	tz_addvoicedelta(RV,RDelta,RV1),
	tz_addvoicedelta(TV,TDelta,TV1),
	tz_addvoicedelta(FV,FDelta,FV1),
	Notes1 = [_:AN,_:BN,_:CN],
	member(RV1:1,Notes1),
	member(TV1:3,Notes1),
	member(FV1:5,Notes1).
*/
% playing and writing midi
%
% For a triangle or weissman area

tz_doplaymidi(Item):-
	get(Item,class_name,CN),
	(   (CN=ttriangledown;CN=ttriangleup) ->
	    tz_doplaymidiTriangle(Item)
	|   otherwise ->
	    tz_doplaymidiWZ(Item)
	),
	!.

tz_doplaymidiTriangle(Tri):-
%	trace,

	% calculate which notes to change
	% take account of context
	(   tz_lastTrianglePlayed(LastT,LastNotes) ->
	    retractall(tz_lastTrianglePlayed(_,_)),
	    get(Tri,index,I),
	    get(LastT,index,LastI),
	    % find what the transform was
	    (	tz_isTransform(Type,Dir,LastI,I) ->
	        % apply transform to previous triangle
	        format('Last Notes=~q~n',[LastNotes]),
		LastNotes1=LastNotes
	    |   otherwise ->
	        % there is a previous triangle but no "known" transform
		% so go with these notes
	        get(Tri,allNotes,Notes),
	        format('Notes=~q~n',[Notes]),
	        Notes=(A,B,C),
	        LastNotes1=[A,B,C],

	        % the null transform
	        Type='',
	        Dir= +
	    )
	| otherwise ->
	    % no last so go with these notes
	    get(Tri,allNotes,Notes),
	    format('Notes=~q~n',[Notes]),
	    Notes=(A,B,C),
	    LastNotes1=[A,B,C],

	    % the null transform
	    Type='',
	    Dir= +
	),

	tz_applyTransformToNotes(Type,Dir,LastNotes1,Notes1),
	format('Voice led transform = ~q~n',[Notes1]),

	assert(tz_lastTrianglePlayed(Tri,Notes1)),

%	(   SKIP=false ->
	    Notes1=[A1:_,B1:_,C1:_],
	    tz_playmidifile([A1,B1,C1]).
%	|   true).
%
%

tz_doplaymidiWZ(WZ):-
	    retractall(tz_lastTrianglePlayed(_,_)),
	%	trace,
	get(WZ,getAugChord,Notes),
	format('~q~n',[Notes]),
	tz_playmidifile(Notes).

tz_dowriteLilyPondItem(Item):-
	retractall(tz_lastTrianglePlayed(_,_)),
	get(Item,class_name,CN),
	(   (CN=ttriangledown;CN=ttriangleup) ->
	    tz_dowriteLilyPondItemTriangle(Item)
	|   otherwise ->
	    tz_dowriteLilyPondItemWZ(Item)
	),
	!.

% NOte this is mostly a repeat of the playroute, so htere is some work
% to be done to generalise this
tz_dowriteLilyPondItemTriangle(Tri):-
%	trace,

	format('~n%{~n',[]),
	% calculate which notes to change
	% take account of context
	(   tz_lastTrianglePlayed(LastT,LastNotes) ->
	    retractall(tz_lastTrianglePlayed(_,_)),
	    get(Tri,index,I),
	    get(LastT,index,LastI),
	    % find what the transform was
	    (	tz_isTransform(Type,Dir,LastI,I) ->
	        % apply transform to previous triangle
	        format('Last Notes=~q~n',[LastNotes]),
		LastNotes1=LastNotes
	    |   otherwise ->
	        % there is a previous triangle but no "known" transform
		% so go with these notes
	        get(Tri,allNotes,Notes),
	        format('Notes=~q~n',[Notes]),
	        Notes=(A,B,C),
	        LastNotes1=[A,B,C],

	        % the null transform
	        Type='',
	        Dir= +
	    )
	| otherwise ->
	    % no last so go with these notes
	    get(Tri,allNotes,Notes),
	    format('Notes=~q~n',[Notes]),
	    Notes=(A,B,C),
	    LastNotes1=[A,B,C],

	    % the null transform
	    Type='',
	    Dir= +
	),

	tz_applyTransformToNotes(Type,Dir,LastNotes1,Notes1),
	format('Voice led transform = ~q~n',[Notes1]),
	format('~n%}~n',[]),

	assert(tz_lastTrianglePlayed(Tri,Notes1)),


	Notes1=[A1:_,B1:_,C1:_],
	tz_writeLilyPondTriad([A1,B1,C1]).

tz_dowriteLilyPondItemWZ(Item):-
	trace,
	format('~n%{~n',[]),

	retractall(tz_lastTrianglePlayed(_,_)),
	%	trace,
	get(Item,getAugChord,Notes),
	format('~n%}~n',[]),

	tz_writeLilyPondTriad(Notes).


tz_addvoicedelta(V,Delta,V1):-
	tz_noteNumber(V,VN),
	VN1 is VN + Delta,
	tz_noteNumber(V1,VN1),!.

% note names just in the tonnetz
tz_noteNumber(X,Y):-
	nonvar(Y),
	Y1 is Y mod 12,
	tz_noteNumber1(X,Y1),
	!.
tz_noteNumber(X,Y):-
	tz_noteNumber1(X,Y),
	!.


tz_noteNumber1('A',1).
tz_noteNumber1('As',2).
tz_noteNumber1('Bf',2).
tz_noteNumber1('B',3).
tz_noteNumber1('Cf',3).
tz_noteNumber1('C',4).
tz_noteNumber1('Bs',4).
tz_noteNumber1('Cs',5).
tz_noteNumber1('Df',5).
tz_noteNumber1('D',6).
tz_noteNumber1('Ds',7).
tz_noteNumber1('Ef',7).
tz_noteNumber1('E',8).
tz_noteNumber1('Ff',8).
tz_noteNumber1('F',9).
tz_noteNumber1('Es',9).
tz_noteNumber1('Fs',10).
tz_noteNumber1('Gf',10).
tz_noteNumber1('G',11).
tz_noteNumber1('Gs',12).
tz_noteNumber1('Af',12).



tz_playmidifile(Notes):-
	concat_atom(Notes,' ',LP),
	downcase_atom(LP,LP1),
	format('playing ~q~n',[LP1]),
	tz_writeLilyPond(LP1),
	Cmd1='runlilypond.bat',
	shell(Cmd1,X),
	Cmd2='midi.bat temp',
	shell(Cmd2,Y),
	format('~q ~q~n',[X,Y]).

% write a single chord to a lilypond file
tz_writeLilyPond(String):-
	tell('temp.ly'),
	format('\\version "2.16.0"~n~n\\language "english"~n~n\\score {~n~n',[]),

	format('  << ~w >>~n~n',[String]),
	format('  \\midi { }~n~n}~n',[]),
	told.

% write a lilypond file in stages
tz_writeLilyPondHdr:-
	tell('temp.ly'),
	format('\\version "2.16.0"~n~n\\language "english"~n~n\\score {~n~n',[]),
	format(' \\relative c\' {~n~n',[]).

tz_writeLilyPondTriad(Notes):-
	concat_atom(Notes,' ',LP),
	downcase_atom(LP,LP1),
	format('  < ~w >1~n~n',[LP1]).

tz_writeLilyPondFtr:-
	format('~n }~n~n',[]),
	format('  \\layout { }~n',[]),
	format('  \\midi { }~n~n}~n',[]),
	told.

% if ONOFF=@on it means open a file to load
% if  = @off it means open a file for writing
getSavedFileName(File,ONOFF):-
	getFileName(File,'.saved',ONOFF).

% pattern should be something like '.saved'
getFileName(File,Pattern,ONOFF):-
	new(FI,finder),
	get(FI, file, ONOFF, Pattern, File).

% pattern should be something like '.saved'
getFileName(File,DefaultName,Ext,ONOFF):-
	new(FI,finder),
	get(FI, file, ONOFF, Ext, @default,DefaultName,File).

