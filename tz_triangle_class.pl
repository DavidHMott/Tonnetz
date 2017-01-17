% ------------------ TTRIANGLE ----------------------------
%
%
:-dynamic(tz_showindex/0).
% uncomment this to show triangle indexes rather than triadnames
%tz_showindex.
%
:-pce_begin_class(ttriangle,object,"A triadtriangle").

variable(index,int,both,"the index").

variable(rootlab,label,both,"the label for the root").
variable(thirdlab,label,both,"the label for the third").
variable(fifthlab,label,both,"the label for the fifth").
variable(indexlab,text,both,"label for the index").
variable(poly,path,both,"the background polygon").
variable(weitz,tz_weitzmann,both,"the weitzmann region it borders one").
variable(tz,tonnetz,both,"").

draw(O,TZ,_Left,_Top,_TW,_TH) :->
	new(P,path),
	send(O,poly,P),
	send(O,tz,TZ).

drawIndex(O,TZ,Left,Top,TW,TH) :->
	TW2 is TW/2,
	TH4 is TH/4,
	get(O,index,I),
	tz_param_chord_size(CS),
	new(LB,text(I,left,font(screen,roman,CS))),
	send(TZ,display,LB,point(Left+TW2-5,Top+TH4)),
	send(O,indexlab,LB),

/*	new(LB,label(li,I,font(screen,roman,10))),
	send(LB,width,5),
	send(TZ,display,LB,point(Left+TW2-5,Top+TH4)),
	send(O,indexlab,LB),
*/
	% attach click to index, cos poly doesnt seem to get it right
	% left click is a selection
	send(LB,recogniser,
	     click_gesture(left,'',single,
			   message(@prolog,leftclicktri,TZ,O,false))),
	% left ctrl click is a end of route
	send(LB,recogniser,
	     click_gesture(left,'c',single,
			   message(@prolog,leftclicktri,TZ,O,true))),
	% left shift click is a movement of the diatonic area
	send(LB,recogniser,
	     click_gesture(left,'s',single,
			   message(@prolog,leftshiftclicktri,TZ,O))),

	!.

hideRight(O) :->
	send(O?fifthlab,displayed,@off),
	send(O?thirdlab,displayed,@off).
%	get(O,index,I),
%	get(O,fifthlab,L5),
%	get(O,thirdlab,L3),
%	format('off ~q ~q ~q~n',[I,L5,L3]).

% refresh the notes as already defined and update the index label
updateNotes(O,RV,TV,FV) :->
	get(O,rootlab,RL),
	get(O,thirdlab,TL),
	get(O,fifthlab,FL),

	send(RL,selection,RV),
	send(TL,selection,TV),
	send(FL,selection,FV),

	% overwrite index numbers with triadnames
	(   not(tz_showindex) ->
	    get(O,triadName,TN),
	    get(O,indexlab,IL),
	    send(IL,value,TN)
	),

	send(O,hideRight),

	send(O,addenharmonics).

%	get(O,index,I),
%	format('updnotes ~q ~q: ~q ~q ~q ~q ~q
%	~q~n',[I,TN,RL,RV,TL,TV,FL,FV]).

% add a enharmonic popup menu to root
addenharmonics(O) :->
	get(O,tz,TZ),
	get(O,rootlab,RL),
	get(RL,selection,RV),

	findall(EV,tz_enharmonic(RV,EV),EVs0),
	% assume only one enharmonic
	% include the label with both

	(   EVs0=[EV1|_] ->
	    concat_atom([RV,'/',EV1],Both),
	    EVs = [RV,EV1,Both]
	|   otherwise ->
	    EVs=[RV]
	),

	new(Pop,popup),
	forall(member(V,EVs),
	       send(Pop,append,
		    menu_item(V,
		       message(@prolog, popupenharmonicchosen,TZ,O,V))
		   )
	    ),
	send(RL,popup,Pop).


closeTransformations(O,Trans) :<-
	get(O,index,I),
	tz_closeTransformations(I,Trans).

clearHighlighting(O) :->
	send(O,highlight(colour(white))),
	send(O?rootlab,expose).

highlight(O,Col) :->
	get(O,poly,P),
	send(P,fill_pattern,Col),
	send(P,flush).

allNotes(O,Notes) :<-
	get(O,rootlab,RV),
	get(RV,selection,RV1),
	get(O,thirdlab,TV),
	get(TV,selection,TV1),
	get(O,fifthlab,FV),
	get(FV,selection,FV1),
	Notes = (RV1:1,TV1:3,FV1:5).

allNotes1(O,Notes) :<-
	get(O,rootlab,RV),
	get(RV,selection,RV1),
	get(O,thirdlab,TV),
	get(TV,selection,TV1),
	get(O,fifthlab,FV),
	get(FV,selection,FV1),
	Notes = (RV1,TV1,FV1).

triadName(O,Name) :<-
	get(O,rootlab,RV),
	get(RV,selection,RV1),
	get(RV1,value,RV2),
	get(O,index,I),
	(   tz_isUp(I) ->
	    Mode = ''
	|   otherwise ->
	    Mode = -
	),
	concat_atom([RV2,Mode],Name).

% triangle is on the route, so show it in red
inroute(O) :->
	get(O,indexlab,Lab),
	send(Lab,background,colour(red)).

clearinroute(O) :->
	get(O,indexlab,Lab),
	send(Lab,background,@default).

% get the coords of the triangle
top(O,Top) :<-
	get(O,poly,P),
	get(P,position,Pos),
	get(Pos,y,Top).

left(O,Left) :<-
	get(O,poly,P),
	get(P,position,Pos),
	get(Pos,x,Left).

:-pce_end_class.

:-pce_begin_class(ttriangledown,ttriangle,"A downfacing triadtriangle").

variable(noteTL,name,both,"top left - the root").
variable(noteB,name,both,"bottom - the third").
variable(noteTR,name,both,"top right - the fifth").

initialise(O,I) :->
	send(O,noteTL,''),
	send(O,noteB,''),
	send(O,noteTR,''),
	send(O,index,I).

% draw the basic triangle, but dont put the notes up
draw(O,TZ,Left,Top,TW,TH) :->

	send_super(O,draw,TZ,Left,Top,TW,TH),
	get(O,poly,T),

	TW2 is TW/2,
	TH2 is TH/2,

	new(TLP,point(Left,Top)),
	new(TRP,point(Left+TW,Top)),
	new(BP,point(Left+TW2,Top+TH2)),

	send(T,append,TLP),
	send(T,append,TRP),
	send(T,append,BP),
	send(T,append,TLP),
	send(T,closed,@on), % close it

	new(L,line(Left+TW,Top,Left+TW2,Top+TH2)),
	send(L,pen,3),

	send(TZ,display,T),
	send(TZ,display,L),

	tz_param_note_size(NS),
	tz_param_note_col(NCol),

	new(LB1,label(la,'',font(screen,helvetica,NS))),
	send(LB1,colour,colour(NCol)),
	send(TZ,display,LB1,point(Left-9,Top-15)),

	send(O,rootlab,LB1),

	new(LB2,label(lb,'',font(screen,helvetica,NS))),
	send(LB2,colour,colour(NCol)),
	send(TZ,display,LB2,point(Left+TW-9,Top-15)),
	send(O,fifthlab,LB2),

	new(LB3,label(lc,'',font(screen,helvetica,NS))),
	send(LB3,colour,colour(NCol)),
	send(TZ,display,LB3,point(Left+TW2-9,Top+TH2-15)),
	send(O,thirdlab,LB3),

	send_super(O,drawIndex,TZ,Left,Top,TW,TH).

%	get(O,index,I),
%	format('~q ~q ~q ~q~n',[I,LB1,LB2,LB3]).



% set the note name of the root of this tri to be Name
% and corresponding other notes
setupNoteOrigin(O,RV) :->
	get(O,index,I),
	tz_getOtherNotes(I,RV,TV,FV),
	send(O,noteTL,RV),
	send(O,noteB,TV),
	send(O,noteTR,FV).


updateNotes(O):->
	get(O,noteTL,RV),
	get(O,noteB,TV),
	get(O,noteTR,FV),
	send_super(O,updateNotes,RV,TV,FV).

:-pce_end_class.

:-pce_begin_class(ttriangleup,ttriangle,"An upfacing triadtriangle").

variable(noteBL,name,both,"bottom left - the root").
variable(noteT,name,both,"top - the third").
variable(noteBR,name,both,"bottom right - the fifth").


initialise(O,I) :->
	send(O,index,I),
	send(O,noteBL,''),
	send(O,noteT,''),
	send(O,noteBR,'').


draw(O,TZ,Left,Top,TW,TH) :->

	send_super(O,draw,TZ,Left,Top,TW,TH),
	get(O,poly,T),

	TW2 is TW/2,
	TH2 is TH/2,

	new(TPP,point(Left+TW2,Top)),
	new(BRP,point(Left+TW,Top+TH2)),
	new(BLP,point(Left,Top+TH2)),

	send(T,append,TPP),
	send(T,append,BRP),
	send(T,append,BLP),
	send(T,append,TPP),
	send(T,closed,@on),

%	send(T,fill_pattern,colour(red)),

	send(TZ,display,T),

	new(L3,line(Left,Top+TH2,Left+TW2,Top)),
	send(L3,pen,3),
	send(TZ,display,L3),

	tz_param_note_size(NS),
	tz_param_note_col(NCol),

	new(LB1,label(la,'',font(screen,helvetica,NS))),
	send(LB1,colour,colour(NCol)),
	send(TZ,display,LB1,point(Left+TW2-9,Top-15)),
	send(O,thirdlab,LB1),

	new(LB2,label(lb,'',font(screen,helvetica,NS))),
	send(LB2,colour,colour(NCol)),
	send(TZ,display,LB2,point(Left-9,Top+TH2-15)),
	send(O,rootlab,LB2),

	new(LB3,label(lc,'',font(screen,helvetica,NS))),
	send(LB3,colour,colour(NCol)),
	send(TZ,display,LB3,point(Left+TW-9,Top+TH2-15)),
	send(O,fifthlab,LB3),

	send_super(O,drawIndex,TZ,Left,Top,TW,TH).

%		get(O,index,I),
%	format('~q ~q ~q ~q~n',[I,LB1,LB2,LB3]).



% set the note name of the root of this tri to be Name
setupNoteOrigin(O,RV) :->
	get(O,index,I),
	tz_getOtherNotes(I,RV,TV,FV),
	send(O,noteBL,RV),
	send(O,noteT,TV),
	send(O,noteBR,FV).


updateNotes(O):->
	get(O,noteBL,RV),
	get(O,noteT,TV),
	get(O,noteBR,FV),
	send_super(O,updateNotes,RV,TV,FV).

:-pce_end_class.

% left click on a triangle. so accept the route to here
% show all close transformations
% also delete all arrows apart from the one to here
leftclicktri(TZ,TRI,ISCTRL):-
%	trace,

	% go find the last selected item (and reset this)
	get(TZ,selecteditem,LastI),
	send(TZ,selecteditem,TRI),

	(   ISCTRL ->
	    % control clicked, so stop the route
	    send(TZ,clearHighlighting),
	    send(TZ,clearTempArrows)
	|   otherwise ->
	    % continue the route
	    leftclicktri1(TZ,TRI,LastI)
	).

leftclicktri1(TZ,TRI,LastI):-
%	trace,
	% show the triangle as on the route
	send(TRI,inroute),

	% is this the first?
	(   LastI='' ->
	    % yes
	    true
	|   otherwise ->
	    % see if there was an arrow to here
	    tz_findarrow(TZ,LastI,TRI,ChosenArrow),

	    (   ChosenArrow='' ->
	        % no there wasnt, so we make this a skip and
	        % create a new skipped arrow from TRI to LastI
	        tz_makeArrow(TZ,LastI,TRI,'',A),
		send(A,makeskipped),
		send(TZ,chosenarrow,A)

	    |   otherwise ->
	        % user has chosen an arrow from last to here
		send(TZ,chosenarrow,ChosenArrow)
	    )
	),

	send(TZ,clearHighlighting),

	% get and display all possible transforms as arrows
	% use context of diatonic area
	get(TRI,closeTransformations,Trans),
	format('Transformations ~q~n',[Trans]),
	forall(member((Name,_,_,To,VD),Trans),
	       tz_addtransformarrow(TZ,VD,TRI,To,Name)
	      ),

	% find the weitzmann region and select it
	% also add a transform arrow from the last triangle to the region
	get(TRI,weitz,WZ),
	send(WZ,highlight),
	tz_makeArrow(TZ,TRI,WZ,'',_),


	(   tz_playmidi(true) ->
	    tz_doplaymidi(TRI)
	|   true
	).

% user chosen an enharmonic spelling to a root for a triangle
% so we change the label, but we must also change the label of
% the triangle with the shared root (since this is also displayed)
popupenharmonicchosen(TZ,T,V):-
	get(T,rootlab,RL),
	send(RL,selection,V),
	get(T,index,TI),
	tz_commonRoot(TI,TI1),
	get(TZ,triangleAtI,TI1,T1),
	get(T1,rootlab,RL1),
	send(RL1,selection,V).
%	format('update ~q ~q ~q~n',[TI,RL,V]).

% user wants to put the diatonic centred at this triangle
leftshiftclicktri(TZ,CentreTri):-
	get(CentreTri,index,I),
	send(TZ,placeDiatonic,I).
