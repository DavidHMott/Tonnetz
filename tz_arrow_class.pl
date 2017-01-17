:-pce_begin_class(tz_arrow,line,"A line showing transforms").

variable(fromitem,object,both,"the source, may be a triangle or weitzmann area").
variable(toitem,object,both,"the destination, may be a triangle or weitzmann area").
variable(textbox,label,both,"The textbox with the current label").
variable(tz,tonnetz,both,"").
variable(isskip,name,both,"this arrow is a skip").
variable(rawname,name,both,"The original name for the transform before contextualisation").


% somewhat arbitrary split betwen init and draw
initialise(O,TZ,From,To) :->
	send_super(O,initialise),
	send(O,fromitem,From),
	send(O,toitem,To),
	send(O,tz,TZ),
	tz_param_trans_size(TS),
	new(Lab,label(l,'',font(screen,courier,TS))),
	send(Lab,colour,colour(red)),
	send(O,textbox,Lab),
	send(O,isskip,false).

% draw an arrow with a given label
% save this as the rawname and also the label
draw(O,Label) :->
%	trace,
	send(O,rawname,Label),
	send(O,pen,2),
	get(O,textbox,TBox),
	send(TBox,selection,Label),
	send(TBox,width,5),
	% get the xy points of source and destination
	get(O?fromitem,indexlab,FLab),
	get(O?toitem,indexlab,TLab),
	get(FLab,x,FX),
	get(FLab,y,FY),
	get(TLab,x,TX),
	get(TLab,y,TY),
	send(O,start,point(FX,FY)),
	send(O,end,point(TX,TY)),

	get(O,tz,TZ),
	% show the line
	send(TZ,display,O),
	% and its label
/*	% in the middle
	FX1 is ((FX + TX) /2) - 10,
	FY1 is ((FY + TY)/2) -15,
	*/
	% towards the destination

	%(((1-t)x0+tx1),((1-t)y0+ty1))
	FX1 is (((1-0.75)*FX + 0.75*TX))-10,
	FY1 is (((1-0.75)*FY + 0.75*TY))-15,

	send(TZ,display,TBox,point(FX1,FY1)).

getLabel(O,Lab) :<-
	get(O,textbox,TB),
	get(TB,selection,Lab).

updateLabel(O,Lab) :->
	get(O,textbox,TB),
	send(TB,selection,Lab).

% set this transform to be a skipped one
% make the text box a ? which is selectable
makeskipped(O) :->
	send(O,isskip,true),
	send(O,texture,dotted),
	send(O,draw,'?'),
	get(O,textbox,TB),
%	get(O,tz,TZ),
	get(O,fromitem,From0),
	get(O,toitem,To0),
	get(From0,index,From),
	get(To0,index,To),

	tz_findtransforms(From,To,Trans),
	format('!!!~w ~w ~w ~n',[From,To,Trans]),
	new(Pop,popup),
	forall(member(T,Trans),
	       send(Pop,append,
		    menu_item(T,
		       message(@prolog, popuparrowchosen,O,T)))
	      ),
	send(TB,popup,Pop).
/*	send(TB,recogniser,
	     click_gesture(left,'',single,
			   message(@prolog,leftclickskipped,TZ,O))).
*/

delete(O) :->
	get(O,textbox,TB),
	send(TB,free),
	send(O,free).

colour(O,Colour) :->
	send_super(O,colour,Colour),
	send(O?textbox,colour,Colour).

serialise(O) :->
	get(O,fromitem,From0),
	get(From0,index,From),
	get(O,toitem,To0),
	get(To0,index,To),
%	get(O,textbox,TB0),
%	get(TB0,selection,TB),
	get(O,rawname,TB),
	get(O,isskip,ISSK),
	format('tz_arrow(~q,~q,~q,~q).~n',[From,To,TB,ISSK]).

% update the info on the arrow in the current context
% if in a diatonic area and either from/to is the root, then use the
% contextualised rawname,
% else use the rawname
recontextualise(O) :->
	get(O,rawname,RawName),
	get(O,fromitem,From),
	get(O,toitem,To),

	get(From,index,FromI),
	get(To,index,ToI),

	get(O?tz,diatonic,DT),
	get(DT,mode,DTMode),
	(   (send(DT,inDiatonicArea,FromI),
	     send(DT,inDiatonicArea,ToI),
	     (	 get(DT,root,FromI);get(DT,root,ToI)), % not the centred!
	     tz_diatonictransform(DTMode,RawName,N1))
	     ->
	     NewName = N1

%	(   send(O?tz,inDiatonicArea,From,To),
%	    tz_diatonictransform(RawName,N1) ->
%	    NewName = N1

	|   otherwise ->
	    NewName = RawName
	),
	send(O,updateLabel,NewName).


tz_arrow_deserialise(TZ,FromI,ToI,Name,ISSK):-
	get(TZ,triangleAtI,FromI,From),
	get(TZ,triangleAtI,ToI,To),

	tz_makeArrow(TZ,From,To,Name,A),
	send(TZ,chosenarrow,A),
	(   ISSK=true ->
	    send(A,makeskipped),
	    send(A,updateLabel,Name)
	|   true
	),
	send(A,recontextualise),
	send(From,inroute),
	send(To,inroute).

% utility to add an arrow from X to Y with given name
tz_makeArrow(TZ,From,To,Name,A):-
	new(A,tz_arrow(TZ,From,To)),
	send(A,draw,Name),
	send(TZ,addArrow,A).

/*leftclickskipped(TZ,A):-
	format('skipped ~q~n',[A]).
*/
popuparrowchosen(A,T):-
	send(A,updateLabel,T).
%	send(TB,selection,X).



:-pce_end_class.

