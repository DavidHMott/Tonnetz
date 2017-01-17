:-dynamic(tz_diatonic/1).
:-dynamic(title/1).
:-dynamic(tz_arrow/4).

:-pce_begin_class(tonnetz,picture,"The tonnetz").

variable(triangles,chain,both,"all the triangles").
variable(weitzmanns,chain,both,"all the weitzmann regions").
variable(mti,object,both,"the text tem for showing the music").
variable(arrows,chain,both,"all the arrows currently shown").
variable(chosenarrows,chain,both,"the arrows chosen as a route").
variable(selecteditem,object,both,"the currently selected triangle or wz area or ''").
variable(titleti,text_item,both,"the item holding the title").
variable(nstrips,int,both,"No of strips to draw").
variable(nrows,int,both,"No of rows to draw").
variable(diatonic,object,both,"The diatonic mask").



initialise(O,Label,Width,Height) :->
	send_super(O,initialise(Label)),
	send_super(O,width(Width)),
	send_super(O,height(Height)),
	send(O,triangles,new(chain)),
	send(O,weitzmanns,new(chain)),
	send(O,arrows,new(chain)),
	send(O,chosenarrows,new(chain)),
	send(O,selecteditem,''),

	% left shift click is closing of the diatonic area
	send(O,recogniser,
	     click_gesture(left,'s',single,
			   message(@prolog,leftshiftclicktz,O))).



draw(O,NStrips,NRows,TZLeft,TZTop) :->
	send(O,nstrips,NStrips),
	send(O,nrows,NRows),
	new(L,text_item('','Title')),
	send(L,value_font,font(screen,helvetica,30)),
	send(L,value_width,1200),
	send(O,display,L,point(100,TZTop-100)),
	send(O,titleti,L),

	tz_param_width(TWidth),
	tz_param_height(THeight),
/*	SI=1,
	get(O,drawstrip,NRows,TZLeft,TZTop,TWidth,THeight,SI,Triangles1),
	get(O,drawstrip,NRows,TZLeft+TWidth,TZTop,TWidth,THeight,SI+6,Triangles2),
	get(O,drawstrip,NRows,TZLeft+TWidth*2,TZTop,TWidth,THeight,SI+12,Triangles3),
	get(O,drawstrip,NRows,TZLeft+TWidth*3,TZTop,TWidth,THeight,SI+18,Triangles4),
	get(O,drawstrip,NRows,TZLeft+TWidth*4,TZTop,TWidth,THeight,SI+52,Triangles5),
	*/
	% no of triangles per row
	NTri is NRows*2,
	get(O,drawstrip,NRows,TZLeft,TZTop,TWidth,THeight,
	    NTri*0+1,Triangles1),
	get(O,drawstrip,NRows,TZLeft+TWidth,TZTop,TWidth,THeight,
	    NTri*1+1,Triangles2),
	get(O,drawstrip,NRows,TZLeft+TWidth*2,TZTop,TWidth,THeight,
	    NTri*2+1,Triangles3),
	get(O,drawstrip,NRows,TZLeft+TWidth*3,TZTop,TWidth,THeight,
	    NTri*3+1,Triangles4),
	get(O,drawstrip,NRows,TZLeft+TWidth*4,TZTop,TWidth,THeight,
	    NTri*4+1,Triangles5),

	% do weitzmann regions

%	trace,
	tz_makeWeitzmannRegion(O,TZLeft,TZTop,TWidth,THeight,[],Triangles1),
	tz_makeWeitzmannRegion(O,TZLeft+TWidth,TZTop,TWidth,THeight,Triangles1,Triangles2),
	tz_makeWeitzmannRegion(O,TZLeft+TWidth*2,TZTop,TWidth,THeight,Triangles2,Triangles3),
	tz_makeWeitzmannRegion(O,TZLeft+TWidth*3,TZTop,TWidth,THeight,Triangles3,Triangles4),
	tz_makeWeitzmannRegion(O,TZLeft+TWidth*4,TZTop,TWidth,THeight,Triangles4,Triangles5),
	tz_makeWeitzmannRegion(O,TZLeft+TWidth*5,TZTop,TWidth,THeight,Triangles5,[]).


put_music_ti(O,MTI) :->
	send(O,mti,MTI).

title(O,Title) :<-
	get(O?titleti,selection,Title).
title(O,Title) :->
	send(O?titleti,selection,Title).

clearHighlighting(O) :->
	get(O,triangles,Tris),
	chain_list(Tris,TriList),
	forall(member(T,TriList),send(T,clearHighlighting)),
	get(O,weitzmanns,Ws),
	chain_list(Ws,WsL),
	forall(member(W,WsL),send(W,deselect)).

clearRoute(O) :->
	get(O,triangles,Tris),
	chain_list(Tris,TriList),
	forall(member(T,TriList),send(T,clearinroute)).

reset(O) :->
	get(O,weitzmanns,Ws),
	chain_list(Ws,WsL),
	forall(member(W,WsL),send(W,clearinroute)),

	send(O,clearHighlighting),
	send(O,clearRoute),
	retractall(tz_lastTrianglePlayed(_,_)),
	send(O,clearArrows),
	send(O,selecteditem,''),
	send(O,title,''),

	send(O,hideDiatonic).

% Arrows
addArrow(O,Arrow) :->
	get(O,arrows,Arrows),
	send(Arrows,append,Arrow).

% clear all arrows
clearArrows(O) :->
%trace,
	get(O,arrows,Arrows),
	chain_list(Arrows,ArrowsL),
	forall(member(Arrow,ArrowsL),
	       send(Arrow,delete)
	      ),
	send(Arrows,free),
	send(O,arrows,new(chain)),

	get(O,chosenarrows,ChosenArrows),
	chain_list(ChosenArrows,ChosenArrowsL),
	forall(member(Arrow,ChosenArrowsL),
	       send(Arrow,delete)
	      ),
	send(ChosenArrows,free),
	send(O,chosenarrows,new(chain)).

% clear the list of temporary arrows
clearTempArrows(O) :->
	get(O,arrows,Arrows),
	chain_list(Arrows,ArrowsL),
	forall(member(Arrow,ArrowsL),
	       send(Arrow,delete)
	      ),
	send(Arrows,free),
	send(O,arrows,new(chain)).

% chosen this arrow so move from temp arrows to chosen arrows
% % highlight destination as chosen
% Make sure its visible
chosenarrow(O,ChosenArrow) :->
	send(ChosenArrow,colour,colour(red)),
	send(ChosenArrow,expose),
	get(O,chosenarrows,Chosen),
	send(Chosen,append,ChosenArrow),
	get(O,arrows,Arrows),
	send(Arrows,delete,ChosenArrow),
	send(O,clearTempArrows).

/*	get(ChosenArrow,toitem,To),
	send(To,inroute).
*/

% Diatonic area
% initial addition of the DT
adddiatonic(O,DT) :->
	send(O,diatonic,DT).

% place the area at triangle index I, and update the arrows, both chosen
% and not chosen
placeDiatonic(O,I) :->
	get(O,diatonic,DT),
	send(DT,centreAt,I),

	get(O,arrows,Arrows),
	chain_list(Arrows,ArrowsL),
	get(O,chosenarrows,Arrows1),
	chain_list(Arrows1,ArrowsL1),
	append(ArrowsL,ArrowsL1,ArrowsL2), % really we should make the tonnetz give all arrows!

	forall(member(A,ArrowsL2),
	       send(A,recontextualise)
	      ).


% hide the diatonic area and update the arrows
hideDiatonic(O) :->
	send(O?diatonic,hide),

	get(O,arrows,Arrows),
	chain_list(Arrows,ArrowsL),
	forall(member(A,ArrowsL),
	       send(A,recontextualise)
	      ).


% is a pair of triangles both in the diatonic area?
% (use send to test)
inDiatonicArea(O,From,To) :->
	get(From,index,FromI),
	get(To,index,ToI),

	get(O,diatonic,DT),
	send(DT,inDiatonicArea,FromI),
        send(DT,inDiatonicArea,ToI).

% Strips
%
% Draw strip and return the set of triangles
% I is the starting index
% !!! should take note of NRows
drawstrip(O,_NRows,TZLeft,TZTop,TWidth,THeight,I,Triangles) :<-
	TW2 is TWidth /2,
	TH2 is THeight/2,

	get(O,triangles,Ts),

	new(TT,ttriangledown(I)),
	send(Ts,append,TT),
	send(TT,draw,O,TZLeft,TZTop,TWidth,THeight),

	new(TT0,ttriangleup(I+1)),
	send(Ts,append,TT0),
	send(TT0,draw,O,TZLeft-TW2,TZTop,TWidth,THeight),

	new(TT1,ttriangledown(I+2)),
	send(Ts,append,TT1),
	send(TT1,draw,O,TZLeft-TW2,TZTop+TH2,TWidth,THeight),

	new(TT2,ttriangleup(I+3)),
	send(Ts,append,TT2),
	send(TT2,draw,O,TZLeft-TW2-TW2,TZTop + TH2,TWidth,THeight),

	new(TT3,ttriangledown(I+4)),
	send(Ts,append,TT3),
	send(TT3,draw,O,TZLeft-TW2-TW2,TZTop + TH2+TH2,TWidth,THeight),

	new(TT4,ttriangleup(I+5)),
	send(Ts,append,TT4),
	send(TT4,draw,O,TZLeft-TW2-TW2-TW2,TZTop + TH2+TH2,TWidth,THeight),

%	new(TT5,ttriangledown(24+I)),
	new(TT5,ttriangledown(I+6)),
	send(Ts,append,TT5),
	send(TT5,draw,O,TZLeft-TW2-TW2-TW2,TZTop + TH2+TH2+TH2,TWidth,THeight),

%	new(TT6,ttriangleup(25+I)),
	new(TT6,ttriangleup(I+7)),
	send(Ts,append,TT6),
	send(TT6,draw,O,TZLeft-TW2-TW2-TW2-TW2,TZTop+TH2+TH2+TH2,TWidth,THeight),

	Triangles=[TT,TT0,TT1,TT2,TT3,TT4,TT5,TT6].

% find triangle with given index

triangleAtI(O,I,T) :<-
	get(O,triangles,Ts),
	chain_list(Ts,TList),
	findall(X,(member(X,TList),get(X,index,II),II=I),Xs),
	% should only be one
	(   Xs=[T] -> true
	| otherwise ->
	    format('Cant find triangle ~q~n',[I])
	).
setupNoteOrigin(O,Root) :->
	%do each strip
	St=1,
	send(O,setupStripNoteOrigin,Root,St),
	tz_fifthOf(Root,Root1),
	send(O,setupStripNoteOrigin,Root1,St+1),
	tz_fifthOf(Root1,Root2),
	send(O,setupStripNoteOrigin,Root2,St+2),
	tz_fifthOf(Root2,Root3),
	send(O,setupStripNoteOrigin,Root3,St+3),
	tz_fifthOf(Root3,Root4),
	send(O,setupStripNoteOrigin,Root4,St+4).

% set the note names in strip N.
setupStripNoteOrigin(O,Root,Strip) :->
	tz_stripOrigin(Strip,TriOr), % find the origin triangle for the strip
	% down
	get(O,triangleAtI,TriOr,T1),
	send(T1,setupNoteOrigin,Root),
	send(T1,updateNotes),
	%up
	tz_majThirdDown(Root,Root1),
	get(O,triangleAtI,TriOr+1,T2),
	send(T2,setupNoteOrigin,Root1),
	send(T2,updateNotes),
	%down
	get(O,triangleAtI,TriOr+2,T3),
	send(T3,setupNoteOrigin,Root1),
	send(T3,updateNotes),
	%up
	tz_majThirdDown(Root1,Root2),
	get(O,triangleAtI,TriOr+3,T4),
	send(T4,setupNoteOrigin,Root2),
	send(T4,updateNotes),
	%down
	get(O,triangleAtI,TriOr+4,T5),
	send(T5,setupNoteOrigin,Root2),
	send(T5,updateNotes),
	%up
	tz_majThirdDown(Root2,Root3),
	get(O,triangleAtI,TriOr+5,T6),
	send(T6,setupNoteOrigin,Root3),
	send(T6,updateNotes),

	% these are enharmonic cycled
	%down
	get(O,triangleAtI,TriOr+6,T25),
	send(T25,setupNoteOrigin,Root3),
	send(T25,updateNotes),
	%up
	tz_majThirdDown(Root3,Root4),
	get(O,triangleAtI,TriOr+7,T26),
	send(T26,setupNoteOrigin,Root4),
	send(T26,updateNotes),
	!.

% play the route through the arrows. This is done via:
% 1) play the first item of the first arrow
% 2) for all subsequent arrows, play the last item
% However, for a skip we do not play the last item, but we make sure
% we play the last item of all
playroute(O) :->
%	trace,
	get(O,chosenarrows,Arrows),
	chain_list(Arrows,ArrowsL),
	(   ArrowsL=[] ->
	    % empty route
	    true
	|   otherwise ->

	    % play 1st item in first arrow
	    ArrowsL=[A1|_],
	    get(A1,fromitem,T1),
	    send(T1,highlight,colour(salmon)),
	    tz_doplaymidi(T1),
	    send(T1,clearHighlighting),

	    append(NotLast, [AZ], ArrowsL),

	    % play second item in all middle arrows
	    forall(member(Arrow,NotLast),
		   (   get(Arrow,toitem,T),
		       send(T,highlight,colour(salmon)),
		       tz_doplaymidi(T),
		       send(T,clearHighlighting)
		   )
		  ),

	    % play 2nd item in last arrow
	    get(AZ,toitem,T2),
	    send(T2,highlight,colour(salmon)),
	    tz_doplaymidi(T2),
	    send(T2,clearHighlighting)

	).

% Make a LP file with the route
writeroute(O) :->
%	trace,
	tz_writeLilyPondHdr,
	get(O,chosenarrows,Arrows),
	chain_list(Arrows,ArrowsL),
	(   ArrowsL=[] ->
	    % empty route
	    true
	|   otherwise ->

	    % play 1st item in first arrow
	    ArrowsL=[A1|_],
	    get(A1,fromitem,T1),
	    tz_dowriteLilyPondItem(T1),

	    append(NotLast, [AZ], ArrowsL),

	    % play second item in all middle arrows
	    forall(member(Arrow,NotLast),
		   (   get(Arrow,toitem,T),
		       tz_dowriteLilyPondItem(T)
		   )
		  ),

	    % play 2nd item in last arrow
	    get(AZ,toitem,TZ),
	    tz_dowriteLilyPondItem(TZ)

	),

	tz_writeLilyPondFtr,

	Cmd1='runlilypond.bat',
	shell(Cmd1,_).

%:-dynamic(title/1).

save(O) :->
	getSavedFileName(F,@off),
	tell(F),

	get(O,title,Title),
	format('title(~q).~n',[Title]),

	% save the diatonic area
	get(O,diatonic,DT),
	send(DT,serialise),

	% save the route
	get(O,chosenarrows,Arrows),
	chain_list(Arrows,ArrowsL),
	forall(member(A,ArrowsL),
	       send(A,serialise)
	      ),

	told,
	format('Saved in ~q~n',[F]),
	!.
save(_) :-> true.

load(O) :->
	retractall(tz_diatonic(_)),
	retractall(title(_)),
	retractall(tz_arrow(_,_,_,_)),

	send(O,reset),
	getSavedFileName(F,@on),
	consult(F),

	listing(title),
	listing(tz_diatonic),
	listing(tz_arrow),

	title(Title),
	send(O,title,Title),

	forall(tz_arrow(A,B,C,D),
	       tz_arrow_deserialise(O,A,B,C,D)
	      ),

	% the diatonic area is already setup, we just need to place it
	% note we have saved the root, i.e. the users click, not the centre
	send(O,hideDiatonic),
	(   tz_diatonic(A) ->
	    send(O,placeDiatonic,A)
	|   true
	),

	!.
load(_) :-> true.


% support
%
% find an arrow in the temp list that is between these two items, or
% ''
tz_findarrow(TZ,LastI,ThisI,ThisArrow):-
	get(TZ,arrows,Arrows),
	chain_list(Arrows,ArrowsL),
	member(ThisArrow,ArrowsL),
	get(ThisArrow,fromitem,FT),
	get(ThisArrow,toitem,TT),
	FT=LastI,
	TT=ThisI,
	!.
tz_findarrow(_TZ,_,_,''):-!.

% is there an arrow in the chosen list that is between these two
% items? may be either direction
tz_ischosenarrow(TZ,LastI,ThisI):-
	get(TZ,chosenarrows,Arrows),
	chain_list(Arrows,ArrowsL),
	member(Arrow,ArrowsL),
	get(Arrow,fromitem,FT),
	get(Arrow,toitem,TT),
	(   (FT=LastI,TT=ThisI);
	    (FT=ThisI,TT=LastI)
	),
	!.

% is there an arrow in the chosen list that is between these two
% items? matching the direction
tz_ischosenarrowexact(TZ,LastI,ThisI):-
	get(TZ,chosenarrows,Arrows),
	chain_list(Arrows,ArrowsL),
	member(Arrow,ArrowsL),
	get(Arrow,fromitem,FT),
	get(Arrow,toitem,TT),
	FT=LastI,
	TT=ThisI,
	!.


:-pce_end_class.

% user clicked on the tonnetz
leftshiftclicktz(TZ):-
	send(TZ,hideDiatonic).

% add a single transform arrow to the tonnetz, given voice distance,
% from tri, totri, name of transform
% We will use the context of the diatonic area to augment the transform
% name - is this the best place to do this?
tz_addtransformarrow(TZ,VD,TRI,To,Name):-
%	trace,
	tz_vdColour(VD,Col),
	get(TZ,triangleAtI,To,TTri),

	send(TTri,highlight,Col),

	% contexualise the name
%	tz_contextualiseTName(TZ,Name,TRI,TTri,NewName),

	tz_makeArrow(TZ,TRI,TTri,Name,A),

        % contexualise the name
	send(A,recontextualise).


/*% contextualise the name of a transform given its location in the
% diatonic area.
% If both ends of the transform are in the DT and the transform has a
% diatonic spelling, then use this spelling

tz_contextualiseTName(TZ,OldName,From,To,NewName):-
	(   send(TZ,inDiatonicArea,From,To),
	    tz_diatonictransform(OldName,N1) ->
	    NewName = N1
	|   otherwise ->
	    NewName=OldName
	).
*/

tz_vdColour(0,colour(lightgrey)).
tz_vdColour(1,colour(lightblue)).
tz_vdColour(-1,colour(lightblue)).
tz_vdColour(2,colour(green)).
tz_vdColour(-2,colour(green)).
tz_vdColour(3,colour(orange)).
tz_vdColour(-3,colour(orange)).
tz_vdColour(4,colour(red)).
tz_vdColour(-4,colour(red)).
