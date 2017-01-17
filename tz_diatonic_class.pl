/*  File:    tz_diatonic_class.pl
    Author:  david
    Created: Dec 27 2016
    Purpose: object to show the diatonic area
*/

:-pce_begin_class(tz_diatonic,path,"A line showing transforms").

variable(tz,tonnetz,both,"").
variable(poly,path,both,"the background polygon").
variable(centred,int,both,"the triangle index on which the diatonic area is physically centred").
variable(root,int,both,"the root of the diatonic area - not necessarily the same as the centred").
variable(mode,name,both,"major or minor").

initialise(O,TZ) :->
	send_super(O,initialise),
	new(P,path),
	send(O,poly,P),
	send(O,tz,TZ).

draw(O,Left,Top,TW,TH,CentredTriI) :->
	send(O,centred,CentredTriI),
	get(O,poly,P),
	get(O,tz,TZ),
	send(P,texture,dotted),

	send(P,pen,6),

	TW2 is TW/2,
	TH2 is TH/2,

	new(P1,point(Left,Top)),
	new(P2,point(Left+TW*2,Top)),
	new(P3,point(Left+TW*2+TW2,Top+TH2)),
	new(P4,point(Left+TW*2,Top+TH)),
	new(P5,point(Left,Top+TH)),
	new(P6,point(Left-TW2,Top+TH2)),

	send(P,append,P1),
	send(P,append,P2),
	send(P,append,P3),
	send(P,append,P4),
	send(P,append,P5),
	send(P,append,P6),
	send(P,append,P1),
	send(P,closed,@on), % close it (probably not necessary)

	send(TZ,display,P),

	send(O,centreAt,CentredTriI).

% centre the area with the triangle (indexed by I) as the root
% if its an uptriangle then mode is major, else its minor
centreAt(O,I) :->
	get(O,poly,P),
	send(P,displayed,@on),

	% get the new position (X,Y) which will be the top of this triangle
	% or the one above according to being up/down
	% and the left of this triangle - TW
	get(O,tz,TZ),
	(   (tz_isUp(I),not(tz_bottomEdge(I))) ->
	    get(TZ,triangleAtI,I,CTri),
	    send(O,mode,major),
	    get(CTri,left,X),
	    get(CTri,top,Y),
	    tz_param_width(TW),
	    send(P,geometry,X-TW,Y),
	    send(O,centred,I),
	    send(O,root,I), % here the root is same as the centre
	    send(P,colour,colour(brown)),
	    format('major DT at ~w root ~w~n',[I,I])

	|   (tz_isDown(I),not(tz_topEdge(I))) ->
	    % get the up triangle above
	    tz_downTopX(I,IUp),
	    get(TZ,triangleAtI,IUp,CTri),
	    send(O,mode,minor),
	    get(CTri,left,X),
	    get(CTri,top,Y),
	    tz_param_width(TW),
	    send(P,geometry,X-TW,Y),
	    % centre on up triangle
	    send(O,centred,IUp),
	    % but the root is the down triangle (the minor)
	    send(O,root,I),
	    send(P,colour,colour(blue)),
	    format('minor DT at ~w root ~w~n',[IUp,I])

	|   otherwise ->

	    format('Triangle on edge~n',[])

	).

% hide the diatonic area
hide(O) :->
	get(O,poly,P),
	send(P,displayed,@off).

% a triangle index is inside the diatonic area
% Use the centre position, not the root

inDiatonicArea(O,I) :->
	get(O,poly,P),
	get(P,displayed,D),
	D = @on,
	get(O,centred,Cen),
	tz_inDiatonicArea(Cen,I).

tz_inDiatonicArea(Cen,I):-
	tz_upBottomX(Cen,CenBot),
	(   I=Cen;
	    tz_upLeftX(Cen,I);
	    tz_upRightX(Cen,I);
	    (	tz_upLeftX(Cen,I1),tz_downLeftX(I1,I));
	    (	tz_upRightX(Cen,I1),tz_downRightX(I1,I));

	    I=CenBot;
	    tz_downLeftX(CenBot,I);
	    tz_downRightX(CenBot,I);
	    (	tz_downLeftX(CenBot,I1),tz_upLeftX(I1,I));
	    (	tz_downRightX(CenBot,I1),tz_upRightX(I1,I))
	).

serialise(O) :->
	get(O,root,R),
	get(O,poly,P),
	get(P,displayed,D),
	(   D = @on ->
	    format('tz_diatonic(~q).~n',[R])
	|   true
	).

%tz_diatonic_deserialise(TZ,A):-
%	new(DT,tz_diatonic(TZ,A)),
%	send(TZ,adddiatonic,DT).

:-pce_end_class.
