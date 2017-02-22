:-pce_begin_class(tz_weitzmann,line,"A line showing the weitzmann augmented region on the tonnetz").

variable(tz,tonnetz,both,"").
variable(triangles,chain,both,"the set of triangles whose leftside is on the strip").
variable(isselectable,object,both,"is the strip ready for selection?").
variable(isonroute,object,both,"is the strip on the route?").
variable(indexlab,text,both,"to be the same as a triangle, the label for the index").

initialise(O,TZ,SX,SY,EX,EY) :->
	send_super(O,initialise,SX,SY,EX,EY),
	send(O,tz,TZ),
	send(O,triangles,new(chain)),
	send(O,isselectable,@off),
	send(O,isonroute,@off),
	send(O,recogniser,
	     click_gesture(left,'',single,
			   message(@prolog,leftclickwtz,O))),

	MX is integer((SX+EX)/2),
	MY is integer((SY+EY)/2),
	new(LB,text(' ',left,font(screen,roman,10))),

/*	new(LB,label(li,'',font(screen,roman,10))),
	send(LB,width,5),
	*/

	send(TZ,display,LB,point(MX,MY)),
	send(O,indexlab,LB).


addtriangle(O,TRI:ttriangle) :->
	send(O?triangles,append,TRI),
	send(TRI,weitz,O).

draw(O) :->
%	trace,
	send(O,pen,2),

	get(O,tz,TZ),
	% show the line
	send(TZ,display,O).

% indicate that the line is selectable
highlight(O) :->
	send(O,colour,colour(purple)),
	send(O,pen,4),
	send(O,isselectable,@on).

% actually select the wz line, so:
%  hide old transform arrows and clear colouring of wz area
%  add arrow from previous triangle to wz line,
%  show triangles, and temp arrows to them
select(O) :->
%	trace,
	get(O,tz,TZ),

	% show route from previous item
	get(TZ,selecteditem,LastItem),
	% we should already have an arrow for this
	tz_findarrow(TZ,LastItem,O,ChosenArrow),
	(   ChosenArrow='' -> format('!!! no chosen arrow to WZ~n',[]) | true),

/*	new(A,tz_arrow(TZ,LastItem,O)),
	send(TZ,addArrow,A),
	send(A,draw,''),
	*/

	send(TZ,chosenarrow,ChosenArrow),
	send(TZ,clearHighlighting),
	send(TZ,clearTempArrows),

	% update current selection to be the wz area
	send(TZ,selecteditem,O),

	% show all the new triangles with their arrows
	% but dont show route back to previous triangle
	get(O,triangles,Ts),
	chain_list(Ts,TsL),
	forall((member(T,TsL),not(T=LastItem)),
	       (   send(T,highlight,colour(yellow)),
		   tz_makeArrow(TZ,O,T,'',A1),
		   send(A1,hide)
%		   new(A1,tz_arrow(TZ,O,T)),
%		   send(A1,colour,@default)
%		   send(A1,draw,''),
%		   send(TZ,addArrow,A1)
	       )
	      ),

	% and show as being in the route
	send(O,inroute),

	% play the chord
	(   tz_playmidi(true) ->
	    tz_doplaymidi(O)
	|   true
	).


% the area is deselected, but we need to retain the red if its on the
% route
deselect(O) :->
	get(O,isonroute,ISR),
	(   ISR = @on -> Col = red | otherwise -> Col = black),
	send(O,colour,colour(Col)),
	send(O,pen,2),
	send(O,isselectable,@off),
	% clear all the triangles
	get(O,triangles,Ts),
	chain_list(Ts,TsL),
	forall(member(T,TsL),
	       send(T,clearHighlighting)
	      ).

% wz is on the route, so show it in red
inroute(O) :->
	send(O,pen,2),
	send(O,colour,colour(red)),
	send(O,isonroute,@on).

clearinroute(O) :->
	send(O,colour,@default),
	send(O,isonroute,@off).

% calc the aug chord from the up triangles,
getAugChord(O,Notes) :<-
	get(O,triangles,Ts),
	chain_list(Ts,TsL),

        findall(N,(member(T,TsL),
		   get(T,class_name,ttriangleup),
		   get(T,rootlab,L),
		   get(L,selection,S),
		   get(S,value,N)
		  ),
		Notes0
	      ),
	append(Notes,[_],Notes0).


% SUPPORT
%
% user clicked on this area when it was selected, so we should
% clear the highlighting and show this one
leftclickwtz(O) :-
	get(O,isselectable,X),
	(   X = @on ->
	   send(O,select),
	   send(O,isselectable,@off)

	|  otherwise ->
	   true
	).


% draw weitzmann regions given the triangles.
% Note that one set may be empty, these are strips on the edges
% we look for all the downs on the left and all the ups on the right
% The right triangles give the notes of the augmented chord
%
tz_makeWeitzmannRegion(TZ,TZLeft,TZTop,TWidth,THeight,LeftTs,RightTs):-

	get(TZ,weitzmanns,Ws),

	new(WR,tz_weitzmann(TZ,TZLeft,TZTop,TZLeft-TWidth-TWidth,TZTop+THeight+THeight)),
	send(WR,draw),
	send(Ws,append,WR),

	% link to all the triangles on the edge

        forall((member(LT,LeftTs),get(LT,class_name,ttriangledown)),
	       send(WR,addtriangle,LT)
	      ),

        forall((member(RT,RightTs),get(RT,class_name,ttriangleup)),
	       send(WR,addtriangle,RT)
	      ),

	send(TZ,weitzmanns,Ws).





:-pce_end_class.



