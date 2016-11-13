:- use_module(library(random)).
:- use_module(library(samsort)).
menu(X):-
repeat,
write('---------------------------------------------------'),nl,
write('|Choose type of game you wish to play:            |'), nl,
write('|1- 1 v 1                                         |'), nl,
write('|2- 1 vs CPU                                      |'), nl,
write('|3- CPU vs CPU                                    |'), nl,
write('|4- Exit                                          |'), nl,
write('---------------------------------------------------'),nl, prcss_ans(1,4,X).

initiateGame(?):-
write('---------------------------------------------------'),nl,
write('|Player1/CPU1 pawns are represented by X.         |'),nl,
write('---------------------------------------------------'),nl.

chooseDificulty(X):-
repeat,
write('---------------------------------------------------'),nl,
write('|Choose dificult of cpu:                           |'), nl,
write('|1- Random                                         |'), nl,
write('|2- Indeed very hard                               |'), nl,
write('---------------------------------------------------'),nl, prcss_ans(1,2,X).

prcss_ans(Min,Max,Ans):-read(Ans),integer(Ans),Ans=<Max, Ans>=Min,nl.
prcss_str(Ans):-read(Str), atom_codes(Ans,Str), (Str==[79,79];Str==[79];Str==[69];Str==[69,69];Str==[83,83];Str==[83];Str==[78,78];Str==[78];Str==[78,69];Str==[78,79];Str==[83,69];Str==[83,79]).

chooseWall(Vert, Hor, Choice,[X,Y]):-
repeat,
write('---------------------------------------------------'),nl,
write('|Choose type of wall you want to place:           |'),nl,
write('|1- Vertical ('),write(Vert),write(')                                  |'),nl,
write('|2- Horizontal ('),write(Hor),write(')                                |'),nl,
write('---------------------------------------------------'),nl,
prcss_ans(1,2,Choice),chooseWallPosition(X,Y).

chooseWallPosition(X,Y):-chooseWallLine(X),chooseWallColumn(Y).

chooseWallLine(X):-
write('---------------------------------------------------'),nl,
write('|Choose line to place wall (1-14):                |'),nl,
write('---------------------------------------------------'),nl,prcss_ans(1,14,X).
chooseWallColumn(Y):-
write('---------------------------------------------------'),nl,
write('Choose column to place wall (1-11):               |'),nl,
write('---------------------------------------------------'),nl,prcss_ans(1,11,Y).

cpuTurn(Turn):-
write('--------------------------------------------'),nl,
write('|Its Cpu '),write(Turn), write(' turn.                           |'),nl,
write('--------------------------------------------'),nl.

placeWall(Vert,Hor,Board,NewBoard,NewVert,NewHor):-chooseWall(Vert, Hor, Choice,[X,Y]),
((Choice==1,Vert>0,checkColisionVertical(Board,[X,Y]),P1 is 2*X-1,P2 is 2*Y,replaceMatrix(Board,P1,P2,1,q,StackBoard),P3 is 2*X+1,replaceMatrix(StackBoard,P3,P2,1,q,NewBoard),checkPawnsPath(NewBoard),NewVert is Vert-1,NewHor is Hor;
Choice==2,Hor>0,checkColisionHorizontal(Board,[X,Y]),P1 is 2*X,replaceMatrix(Board,P1,Y,1,w,StackBoard),P2 is Y+1,replaceMatrix(StackBoard,P1,P2,1,w,NewBoard),checkPawnsPath(NewBoard),NewVert is Vert,NewHor is Hor-1);placeWall(Vert,Hor,Board,NewBoard,NewVert,NewHor)).

placeRandomWall(Vert,Hor,Board,NewBoard,NewVert,NewHor):-random(1,3,Choice),((Choice==1,Hor \= 0;Choice==2,Vert==0),availableWallHorizontal(Board,1,1),availableWallH(List),sizeOfList(List,Size),RealSize is Size+1,random(1,RealSize,RandomPos),positionInList(RandomPos,List,1,[X,Y]),
P1 is 2*X,replaceMatrix(Board,P1,Y,1,w,StackBoard),P2 is Y+1,replaceMatrix(StackBoard,P1,P2,1,w,NewBoard),NewVert is Vert,NewHor is Hor-1,retract(availableWallH(_));
(Choice==1,Hor == 0;Choice==2,Vert\=0),availableWallVertical(Board,1,1),availableWallV(List),sizeOfList(List,Size),RealSize is Size+1,random(1,RealSize,RandomPos),positionInList(RandomPos,List,1,[X,Y]),
P1 is 2*X-1,P2 is 2*Y,replaceMatrix(Board,P1,P2,1,q,StackBoard),P3 is 2*X+1,replaceMatrix(StackBoard,P3,P2,1,q,NewBoard),NewVert is Vert-1,NewHor is Hor,retract(availableWallV(_))).

availableWallHorizontal(Board,X,Y):-(checkColisionHorizontal(Board,[X,Y]),P1 is 2*X,replaceMatrix(Board,P1,Y,1,w,StackBoard),P2 is Y+1,replaceMatrix(StackBoard,P1,P2,1,w,NewBoard),checkPawnsPath(NewBoard), (\+ availableWallH(List), asserta(availableWallH([[X,Y]]));availableWallH(List),retract(availableWallH(_))
,append([[X,Y]],List,NewList), asserta(availableWallH(NewList)));true),(Y<10,Y1 is Y+1, X1 is X;Y1 is 1,X1 is X+1),X<15,!,garbage_collect,availableWallHorizontal(Board,X1,Y1);true.

availableWallVertical(Board,X,Y):-(checkColisionVertical(Board,[X,Y]),P1 is 2*X-1,P2 is 2*Y,replaceMatrix(Board,P1,P2,1,q,StackBoard),P3 is 2*X+1,replaceMatrix(StackBoard,P3,P2,1,q,NewBoard),checkPawnsPath(NewBoard),(\+ availableWallV(List), asserta(availableWallV([[X,Y]]));availableWallV(List),retract(availableWallV(_))
 ,append([[X,Y]],List,NewList), asserta(availableWallV(NewList)));true),(Y<11,Y1 is Y+1, X1 is X;Y1 is 1,X1 is X+1),X<14,!,garbage_collect,availableWallVertical(Board,X1,Y1);true.

checkPawnsPath(Board):- pawn1(Pawn1),pawn2(Pawn2),pawn3(Pawn3),pawn4(Pawn4),checkPawnPath(Board,Pawn1,1),checkPawnPath(Board,Pawn2,1),checkPawnPath(Board,Pawn3,2),checkPawnPath(Board,Pawn4,2).

checkPawnPath(Board,[X,Y],Player):-(Player==2,X1 is 2*X-1,Y1 is 2*Y-1,assert(visited([[X1,Y1]])),checkPath(Board,X1,Y1,[7,7],[7,15]),
retract(visited(_));%,X1 is 2*X-1,Y1 is 2*Y-1,assert(visited([[X1,Y1]])),checkPath(Board,X1,Y1,[7,15]),retract(visited(_));
Player==1,X1 is 2*X-1,Y1 is 2*Y-1,assert(visited([[X1,Y1]])),checkPath(Board,X1,Y1,[21,7],[21,15]),
retract(visited(_)))%,X1 is 2*X-1,Y1 is 2*Y-1,assert(visited([[X1,Y1]])),checkPath(Board,X1,Y1,[21,15]),retract(visited(_)))
;retract(visited(_)),fail.

calcboard([
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1],
  [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
  ]).

board([[c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c],
	[b,b,b,b,b,b,b,b,b,b,b],
	[c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c],
	[b,b,b,b,b,b,b,b,b,b,b],
	[c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c],
	[b,b,b,b,b,b,b,b,b,b,b],
	[c,a,c,a,c,a,p,a,c,a,c,a,c,a,p,a,c,a,c,a,c],
	[b,b,b,b,b,b,b,b,b,b,b],
	[c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c],
	[b,b,b,b,b,b,b,b,b,b,b],
	[c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c],
	[b,b,b,b,b,b,b,b,b,b,b],
	[c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c],
	[b,b,b,b,b,b,b,b,b,b,b],
	[c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c],
	[b,b,b,b,b,b,b,b,b,b,b],
	[c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c],
	[b,b,b,b,b,b,b,b,b,b,b],
	[c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c],
	[b,b,b,b,b,b,b,b,b,b,b],
	[c,a,c,a,c,a,j,a,c,a,c,a,c,a,j,a,c,a,c,a,c],
	[b,b,b,b,b,b,b,b,b,b,b],
	[c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c],
	[b,b,b,b,b,b,b,b,b,b,b],
	[c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c],
	[b,b,b,b,b,b,b,b,b,b,b],
	[c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c]]).

:-dynamic visited/1.
:-dynamic availableList/1.
:-dynamic pawn1/1.
:-dynamic pawn2/1.
:-dynamic pawn3/1.
:-dynamic pawn4/1.
:-dynamic availableWallH/1.
:-dynamic availableWallV/1.
:-dynamic calcBoard/1.

floodFill(Board,X,Y):-(XC1 is round((X+1)/2),XC2 is round((Y+1)/2), calcBoard(CalcBoard),getElementFromMatrix(CalcBoard,XC1,XC2,1,1,Value),
X1 is X+2,checkPawnColision(X1,Y),XWall is X+1,YWall is round((Y+1)/2),X1<28,getElementFromMatrix(Board,XWall,YWall,1,1,WallH),WallH\=w,
XC3 is round((X1+1)/2),getElementFromMatrix(CalcBoard,XC3,XC2,1,1,DestValue),(Value+1<DestValue;DestValue==(-1)),NewValue is Value+1,replaceMatrix(CalcBoard,XC3,XC2,1,NewValue,NewCalcBoard),
retract(calcBoard(_)),asserta(calcBoard(NewCalcBoard)),floodFill(Board,X1,Y),false;

XC1 is round((X+1)/2),XC2 is round((Y+1)/2), calcBoard(CalcBoard),getElementFromMatrix(CalcBoard,XC1,XC2,1,1,Value),
X1 is X-2,X1>0,checkPawnColision(X1,Y), XWall is X-1,YWall is round((Y+1)/2),getElementFromMatrix(Board,XWall,YWall,1,1,WallH),WallH\=w,
XC3 is round((X1+1)/2),getElementFromMatrix(CalcBoard,XC3,XC2,1,1,DestValue),(Value+1<DestValue;DestValue==(-1)),NewValue is Value+1,replaceMatrix(CalcBoard,XC3,XC2,1,NewValue,NewCalcBoard),
retract(calcBoard(_)),asserta(calcBoard(NewCalcBoard)),floodFill(Board,X1,Y),false;

XC1 is round((X+1)/2),XC2 is round((Y+1)/2), calcBoard(CalcBoard),getElementFromMatrix(CalcBoard,XC1,XC2,1,1,Value),
Y1 is Y+2,Y1<22,checkPawnColision(X,Y1), YWall is Y+1,getElementFromMatrix(Board,X,YWall,1,1,WallV),WallV\=q,
XC3 is round((Y1+1)/2),getElementFromMatrix(CalcBoard,XC1,XC3,1,1,DestValue),(Value+1<DestValue;DestValue==(-1)),NewValue is Value+1,replaceMatrix(CalcBoard,XC1,XC3,1,NewValue,NewCalcBoard),
retract(calcBoard(_)),asserta(calcBoard(NewCalcBoard)),floodFill(Board,X,Y1),false;

XC1 is round((X+1)/2),XC2 is round((Y+1)/2), calcBoard(CalcBoard),getElementFromMatrix(CalcBoard,XC1,XC2,1,1,Value),
Y1 is Y-2,Y1>0,checkPawnColision(X,Y1), YWall is Y-1,getElementFromMatrix(Board,X,YWall,1,1,WallV),WallV\=q,
XC3 is round((Y1+1)/2),getElementFromMatrix(CalcBoard,XC1,XC3,1,1,DestValue),(Value+1<DestValue;DestValue==(-1)),NewValue is Value+1,replaceMatrix(CalcBoard,XC1,XC3,1,NewValue,NewCalcBoard),
retract(calcBoard(_)),asserta(calcBoard(NewCalcBoard)),floodFill(Board,X,Y1),false

);true.

compareAvg( [P1x,P1y],[P2x,P2y], [X1,Y1],[X2,Y2]) :-Value1 is (abs(P1x-X1)+abs(P1y-Y1)),Value2 is(abs(P1x-X2)+abs(P1y-Y2)), Value3 is(abs(P2x-X1)+abs(P2y-Y1)),Value4 is (abs(P2x-X2)+abs(P2y-Y2)),
min(Value1,Value3)<min(Value2,Value4).

smartWallCPU(Board,Turn,NewBoard,Vert,Hor,NewVert,NewHor):-(Turn==2,pawn1(Pawn1),pawn2(Pawn2);Turn==1,pawn3(Pawn1),pawn4(Pawn2)),
random(1,3,RandomWall),((RandomWall==1;Vert==0),availableWallHorizontal(Board,1,1),availableWallH(List),samsort(compareAvg(Pawn1,Pawn2),List,OrderedList),positionInList(1,OrderedList,1,[X,Y]),
P1 is 2*X,replaceMatrix(Board,P1,Y,1,w,StackBoard),P2 is Y+1,replaceMatrix(StackBoard,P1,P2,1,w,NewBoard),NewVert is Vert,NewHor is Hor-1,retract(availableWallH(_));
(RandomWall==2;Hor==0),availableWallVertical(Board,1,1),availableWallV(List),samsort(compareAvg(Pawn1,Pawn2),List,OrderedList),positionInList(1,OrderedList,1,[X,Y]),
P1 is 2*X-1,P2 is 2*Y,replaceMatrix(Board,P1,P2,1,q,StackBoard),P3 is 2*(X+1)-1,replaceMatrix(StackBoard,P3,P2,1,q,NewBoard),NewVert is Vert-1,NewHor is Hor,retract(availableWallV(_))).


smartMovementCPU(Board,Turn,NewBoard):-(Turn==1,pawn1([Pawn1V,Pawn1H]),pawn2([Pawn2V,Pawn2H]);Turn ==2,pawn3([Pawn1V,Pawn1H]),pawn4([Pawn2V,Pawn2H])),
calcboard(CalcBoard),replaceMatrix(CalcBoard,Pawn1V,Pawn1H,1,0,NewCalcBoard),Pawn1Vff is 2*Pawn1V-1,Pawn1Hff is 2* Pawn1H-1,asserta(calcBoard(NewCalcBoard)),floodFill(Board,Pawn1Vff,Pawn1Hff),
calcBoard(CalcPawn1),retract(calcBoard(_)),
replaceMatrix(CalcBoard,Pawn2V,Pawn2H,1,0,NewCalcBoard2),asserta(calcBoard(NewCalcBoard2)),Pawn2Vff is 2*Pawn2V-1,Pawn2Hff is 2* Pawn2H-1,floodFill(Board,Pawn2Vff,Pawn2Hff),calcBoard(CalcPawn2),retract(calcBoard(_)),
(Turn==1,(getElementFromMatrix(CalcPawn1,11,4,1,1,Value1),Value1\=(-1),Pos1x=11,Pos1y=4;getElementFromMatrix(CalcPawn1,11,5,1,1,Value1),Value1\=(-1),Pos1x=11,Pos1y=5;getElementFromMatrix(CalcPawn1,11,3,1,1,Value1),Value1\=(-1),Pos1x=11,Pos1y=3;getElementFromMatrix(CalcPawn1,12,4,1,1,Value1),Value1\=(-1),Pos1x=12,Pos1y=4;getElementFromMatrix(CalcPawn1,10,4,1,1,Value1),Value1\=(-1),Pos1x=10,Pos1y=4),
(getElementFromMatrix(CalcPawn1,11,8,1,1,Value2),Value2\=(-1),Pos2x=11,Pos2y=8;getElementFromMatrix(CalcPawn1,11,7,1,1,Value2),Value2\=(-1),Pos2x=11,Pos2y=7;getElementFromMatrix(CalcPawn1,11,9,1,1,Value2),Value2\=(-1),Pos2x=11,Pos2y=9;getElementFromMatrix(CalcPawn1,10,8,1,1,Value2),Value2\=(-1),Pos2x=10,Pos2y=8;getElementFromMatrix(CalcPawn1,12,8,1,1,Value2),Value2\=(-1),Pos2x=12,Pos2y=8),
(getElementFromMatrix(CalcPawn2,11,4,1,1,Value3),Value3\=(-1),Pos3x=11,Pos3y=4;getElementFromMatrix(CalcPawn2,11,5,1,1,Value3),Value3\=(-1),Pos3x=11,Pos3y=5;getElementFromMatrix(CalcPawn2,11,3,1,1,Value3),Value3\=(-1),Pos3x=11,Pos3y=3;getElementFromMatrix(CalcPawn2,10,4,1,1,Value3),Value3\=(-1),Pos3x=10,Pos3y=4;getElementFromMatrix(CalcPawn2,12,4,1,1,Value3),Value3\=(-1),Pos3x=12,Pos3y=4),
(getElementFromMatrix(CalcPawn2,11,8,1,1,Value4),Value4\=(-1),Pos4x=11,Pos4y=8;getElementFromMatrix(CalcPawn2,11,7,1,1,Value4),Value4\=(-1),Pos4x=11,Pos4y=7;getElementFromMatrix(CalcPawn2,11,9,1,1,Value4),Value4\=(-1),Pos4x=11,Pos4y=9;getElementFromMatrix(CalcPawn2,10,8,1,1,Value4),Value4\=(-1),Pos4x=10,Pos4y=8;getElementFromMatrix(CalcPawn2,12,8,1,1,Value4),Value4\=(-1),Pos4x=12,Pos4y=8),
P1 is min(Value1,Value2),P2 is min(Value3,Value4),(PFinal is min(P1,P2),PFinal\=0;Value1==0,PFinal is Value4;Value2==0,PFinal is Value3;Value3==0,PFinal is Value2;Value4==0,PFinal is Value1),
(PFinal=:=Value1,Mov is Value1-2,getPath(CalcPawn1,[Pos1x,Pos1y],Value1,[FinalX,FinalY],Mov),posToMov([Pawn1V,Pawn1H],[FinalX,FinalY],Result),move(Board,[Pawn1V,Pawn1H],Result,NewPawn,NewBoard,r),retract(pawn1(_)),assert(pawn1(NewPawn));
PFinal=:=Value2,Mov is Value2-2,getPath(CalcPawn1,[Pos2x,Pos2y],Value2,[FinalX,FinalY],Mov),posToMov([Pawn1V,Pawn1H],[FinalX,FinalY],Result),move(Board,[Pawn1V,Pawn1H],Result,NewPawn,NewBoard,r),retract(pawn1(_)),assert(pawn1(NewPawn));
PFinal=:=Value3,Mov is Value3-2,getPath(CalcPawn2,[Pos3x,Pos3y],Value3,[FinalX,FinalY],Mov),posToMov([Pawn2V,Pawn2H],[FinalX,FinalY],Result),move(Board,[Pawn2V,Pawn2H],Result,NewPawn,NewBoard,r),retract(pawn2(_)),assert(pawn2(NewPawn));
PFinal=:=Value4,Mov is Value4-2,getPath(CalcPawn2,[Pos4x,Pos4y],Value4,[FinalX,FinalY],Mov),posToMov([Pawn2V,Pawn2H],[FinalX,FinalY],Result),move(Board,[Pawn2V,Pawn2H],Result,NewPawn,NewBoard,r),retract(pawn2(_)),assert(pawn2(NewPawn)))
;
Turn==2,(getElementFromMatrix(CalcPawn1,4,4,1,1,Value1),Value1\=(-1),Pos1x=4,Pos1y=4;getElementFromMatrix(CalcPawn1,4,5,1,1,Value1),Value1\=(-1),Pos1x=4,Pos1y=5;getElementFromMatrix(CalcPawn1,4,3,1,1,Value1),Value1\=(-1),Pos1x=4,Pos1y=3;getElementFromMatrix(CalcPawn1,3,4,1,1,Value1),Value1\=(-1),Pos1x=3,Pos1y=4;getElementFromMatrix(CalcPawn1,5,4,1,1,Value1),Value1\=(-1),Pos1x=5,Pos1y=4),
(getElementFromMatrix(CalcPawn1,4,8,1,1,Value2),Value2\=(-1),Pos2x=4,Pos2y=8;getElementFromMatrix(CalcPawn1,4,7,1,1,Value2),Value2\=(-1),Pos2x=4,Pos2y=7;getElementFromMatrix(CalcPawn1,4,9,1,1,Value2),Value2\=(-1),Pos2x=4,Pos2y=9;getElementFromMatrix(CalcPawn1,3,8,1,1,Value2),Value2\=(-1),Pos2x=3,Pos2y=8;getElementFromMatrix(CalcPawn1,5,8,1,1,Value2),Value2\=(-1),Pos2x=5,Pos2y=8),
(getElementFromMatrix(CalcPawn2,4,4,1,1,Value3),Value3\=(-1),Pos3x=4,Pos3y=4;getElementFromMatrix(CalcPawn2,4,3,1,1,Value3),Value3\=(-1),Pos3x=4,Pos3y=3;getElementFromMatrix(CalcPawn2,4,5,1,1,Value3),Value3\=(-1),Pos3x=4,Pos3y=5;getElementFromMatrix(CalcPawn2,3,4,1,1,Value3),Value3\=(-1),Pos3x=3,Pos3y=4;getElementFromMatrix(CalcPawn2,5,4,1,1,Value3),Value3\=(-1),Pos3x=5,Pos3y=4),
(getElementFromMatrix(CalcPawn2,4,8,1,1,Value4),Value4\=(-1),Pos4x=4,Pos4y=8;getElementFromMatrix(CalcPawn2,4,7,1,1,Value4),Value4\=(-1),Pos4x=4,Pos4y=7;getElementFromMatrix(CalcPawn2,4,9,1,1,Value4),Value4\=(-1),Pos4x=4,Pos4y=9;getElementFromMatrix(CalcPawn2,3,8,1,1,Value4),Value4\=(-1),Pos4x=3,Pos4y=8;getElementFromMatrix(CalcPawn2,5,8,1,1,Value4),Value4\=(-1),Pos4x=5,Pos4y=8),
P1 is min(Value1,Value2),P2 is min(Value3,Value4),(PFinal is min(P1,P2),PFinal\=0;Value1==0,PFinal is Value4;Value2==0,PFinal is Value3;Value3==0,PFinal is Value2;Value4==0,PFinal is Value1),
(PFinal=:=Value1,Mov is Value1-2,getPath(CalcPawn1,[Pos1x,Pos1y],Value1,[FinalX,FinalY],Mov),posToMov([Pawn1V,Pawn1H],[FinalX,FinalY],Result),move(Board,[Pawn1V,Pawn1H],Result,NewPawn,NewBoard,e),retract(pawn3(_)),assert(pawn3(NewPawn));
PFinal=:=Value2,Mov is Value2-2,getPath(CalcPawn1,[Pos2x,Pos2y],Value2,[FinalX,FinalY],Mov),posToMov([Pawn1V,Pawn1H],[FinalX,FinalY],Result),move(Board,[Pawn1V,Pawn1H],Result,NewPawn,NewBoard,e),retract(pawn3(_)),assert(pawn3(NewPawn));
PFinal=:=Value3,Mov is Value3-2,getPath(CalcPawn2,[Pos3x,Pos3y],Value3,[FinalX,FinalY],Mov),posToMov([Pawn2V,Pawn2H],[FinalX,FinalY],Result),move(Board,[Pawn2V,Pawn2H],Result,NewPawn,NewBoard,e),retract(pawn4(_)),assert(pawn4(NewPawn));
PFinal=:=Value4,Mov is Value4-2,getPath(CalcPawn2,[Pos4x,Pos4y],Value4,[FinalX,FinalY],Mov),posToMov([Pawn2V,Pawn2H],[FinalX,FinalY],Result),move(Board,[Pawn2V,Pawn2H],Result,NewPawn,NewBoard,e),retract(pawn4(_)),assert(pawn4(NewPawn)))
).

getPath(Board,[X,Y],Number,[Posx,Posy],Final):-Final=<0,Posx=X,Posy=Y;(X1 is X+1,getElementFromMatrix(Board,X1,Y,1,1,Value),Value =:= Number-1,Final1 is Final-1,(Value\=0,Final1\=0,getPath(Board,[X1,Y],Value,[Posx,Posy],Final1);Value\=0,Posx is X1,Posy is Y;Posx is X,Posy is Y);
X1 is X-1,getElementFromMatrix(Board,X1,Y,1,1,Value),Value =:= Number-1,Final1 is Final-1,(Value\=0,Final1\=0,getPath(Board,[X1,Y],Value,[Posx,Posy],Final1);Value\=0,Posx is X1,Posy is Y;Posx is X,Posy is Y);
Y1 is Y+1,getElementFromMatrix(Board,X,Y1,1,1,Value),Value =:= Number-1,Final1 is Final-1,(Value\=0,Final1\=0,getPath(Board,[X,Y1],Value,[Posx,Posy],Final1);Value\=0,Posx is X,Posy is Y1;Posx is X,Posy is Y);
Y1 is Y-1,getElementFromMatrix(Board,X,Y1,1,1,Value),Value =:= Number-1,Final1 is Final-1,(Value\=0,Final1\=0,getPath(Board,[X,Y1],Value,[Posx,Posy],Final1);Value\=0,Posx is X,Posy is Y1;Posx is X,Posy is Y)).


start(?):-menu(X),((X==2;X==3),chooseDificulty(Y);true),(X<4,board(L1),asserta(pawn1([4,4])),asserta(pawn2([4,8])),asserta(pawn3([11,4])),asserta(pawn4([11,8])),J1=player(8,8),J2=player(8,8),initiateGame(?),play(L1,J1,J2,1,0,X,Y);true,!).


play(_,_,_,_,1,_,_):-retract(pawn2(_)),retract(pawn1(_)),retract(pawn3(_)),retract(pawn4(_)),start(?).
play(L1,J1,J2,Turn,0,1,_):-J1=player(V1,H1),J2=player(V2,H2),startDisplay(L1,1,1),pawn1(Pawn1),pawn2(Pawn2),pawn3(Pawn3),pawn4(Pawn4),
((Turn==1,
	chooseStaringPiece(Turn,Pawn),
	choosePositionToMove(Place),
  (Pawn==1,availablePositions(L1,Pawn1),availableList(List),member(Place,List),retract(availableList(_)),move(L1,Pawn1,Place,NewPawn,StackBoard,r),retract(pawn1(_)),asserta(pawn1(NewPawn)),((V1>0;H1>0),placeWall(V1,H1,StackBoard,NewBoard,NewVert,NewHor);NewBoard=StackBoard,NewVert is 0,NewHor is 0),checkWinners(End),play(NewBoard,player(NewVert,NewHor),player(V2,H2),2,End,1,_);
	Pawn==2,availablePositions(L1,Pawn2),availableList(List),member(Place,List),retract(availableList(_)),move(L1,Pawn2,Place,NewPawn,StackBoard,r),retract(pawn2(_)),asserta(pawn2(NewPawn)),((V1>0;H1>0),placeWall(V1,H1,StackBoard,NewBoard,NewVert,NewHor);NewBoard=StackBoard,NewVert is 0,NewHor is 0),checkWinners(End),play(NewBoard,player(NewVert,NewHor),player(V2,H2),2,End,1,_)));
(Turn==2,
	chooseStaringPiece(Turn,Pawn),
  choosePositionToMove(Place),
  (Pawn==1,availablePositions(L1,Pawn3),availableList(List),member(Place,List),retract(availableList(_)),move(L1,Pawn3,Place,NewPawn,StackBoard,e),retract(pawn3(_)),asserta(pawn3(NewPawn)),((V2>0;H2>0),placeWall(V2,H2,StackBoard,NewBoard,NewVert,NewHor);NewBoard=StackBoard,NewVert is 0,NewHor is 0),checkWinners(End),play(NewBoard,player(V1,H1),player(NewVert,NewHor),1,End,1,_);
	Pawn==2,availablePositions(L1,Pawn4),availableList(List),member(Place,List),retract(availableList(_)),move(L1,Pawn4,Place,NewPawn,StackBoard,e),retract(pawn4(_)),asserta(pawn4(NewPawn)),((V2>0;H2>0),placeWall(V2,H2,StackBoard,NewBoard,NewVert,NewHor);NewBoard=StackBoard,NewVert is 0,NewHor is 0),checkWinners(End),play(NewBoard,player(V1,H1),player(NewVert,NewHor),1,End,1,_))));fail.

play(Board,J1,J2,Turn,0,3,Dif):-J1=player(V1,H1),J2=player(V2,H2),startDisplay(Board,1,1),cpuTurn(Turn),(Dif==1,movePawnRandomly(Board,Turn,StackBoard);Dif==2,smartMovementCPU(Board,Turn,StackBoard)),read(_),
(Turn==1,(Dif==1,((V1>0;H1>0),placeRandomWall(V1,H1,StackBoard,NewBoard,NewVert,NewHor);NewBoard=StackBoard,NewVert is 0,NewHor is 0);Dif==2,((V1>0;H1>0),smartWallCPU(StackBoard,Turn,NewBoard,V1,H1,NewVert,NewHor);NewBoard=StackBoard,NewVert is 0,NewHor is 0)),checkWinners(End),!,garbage_collect,play(NewBoard,player(NewVert,NewHor),J2,2,End,3,Dif) ;
Turn==2,(Dif==1,((V1>0;H1>0),placeRandomWall(V2,H2,StackBoard,NewBoard,NewVert,NewHor);NewBoard=StackBoard,NewVert is 0,NewHor is 0);Dif==2,((V2>0;H2>0),smartWallCPU(StackBoard,Turn,NewBoard,V2,H2,NewVert,NewHor);NewBoard=StackBoard,NewVert is 0,NewHor is 0)),checkWinners(End),!,garbage_collect,play(NewBoard,J1,player(NewVert,NewHor),1,End,3,Dif)).

play(Board,J1,J2,Turn,0,2,Dif):-J1=player(V1,H1),J2=player(V2,H2),startDisplay(Board,1,1),
  ((Turn==1,
  	chooseStaringPiece(Turn,Pawn),
  	choosePositionToMove(Place),
    (Pawn==1,availablePositions(Board,Pawn1),availableList(List),member(Place,List),retract(availableList(_)),move(Board,Pawn1,Place,NewPawn,StackBoard,r),retract(pawn1(_)),asserta(pawn1(NewPawn)),((V1>0;H1>0),placeWall(V1,H1,StackBoard,NewBoard,NewVert,NewHor);NewBoard=StackBoard,NewVert is 0,NewHor is 0);
  	Pawn==2,availablePositions(Board,Pawn2),availableList(List),member(Place,List),retract(availableList(_)),move(Board,Pawn2,Place,NewPawn,StackBoard,r),retract(pawn2(_)),asserta(pawn2(NewPawn)),((V1>0;H1>0),placeWall(V1,H1,StackBoard,NewBoard,NewVert,NewHor);NewBoard=StackBoard,NewVert is 0,NewHor is 0)),checkWinners(End),play(NewBoard,player(NewVert,NewHor),player(V2,H2),2,End,2,Dif));
    (Turn==2,cpuTurn(1),(Dif==1,movePawnRandomly(Board,Turn,StackBoard);Dif==2,smartMovementCPU(Board,Turn,StackBoard)),read(_),(Dif==1,((V2>0;H2>0),placeRandomWall(V2,H2,StackBoard,NewBoard,NewVert,NewHor);NewBoard=StackBoard,NewVert is 0,NewHor is 0);Dif==2,((H2>0;V2>0),smartWallCPU(StackBoard,Turn,NewBoard,V2,H2,NewVert,NewHor));NewBoard=StackBoard,NewVert is 0,NewHor is 0),checkWinners(End),!,play(NewBoard,player(V1,H1),player(NewVert,NewHor),1,End,2,Dif))).



checkWinners(End):-pawn1([P1v,P1h]),pawn2([P2v,P2h]),pawn3([P3v,P3h]),pawn4([P4v,P4h]),
(P1v==11,P2v==11,(P1h==4;P1h==8),(P2h==4;P2h==8),nl,write('Player 1 wins'),nl,End is 1;
P3v==4,P4v==4,(P3h==4;P3h==8),(P4h==4;P4h==8),nl,write('Player 2 wins'),nl,End is 1);End is 0,true.


posToMov([Xi,Yi],[Xf,Yf],Result):-Yi==Yf,Xi-Xf=:=1,Result='N';Xi-Xf=:=2,Result='NN';Yi==Yf,Xf-Xi=:=1,Result='S';Xf-Xi=:=2,Result='SS';
Xi==Xf,Yi-Yf=:=1,Result='O';Yi-Yf=:=2,Result='OO';Xi==Xf,Yf-Yi=:=1,Result='E';Yf-Yi=:=2,Result='EE';
Xf-Xi=:=1,Yf-Yi=:=1,Result='SE';Xi-Xf=:=1,Yf-Yi=:=1,Result='NE';Xi-Xf=:=1,Yi-Yf=:=1,Result='NO';Xf-Xi=:=1,Yi-Yf=:=1,Result='SO'.

move(Board,[P1,P2],Char,L1,NewBoard,PlayerChar):-
(Char=='NN',NewPosX is P1-2,NewX is 2*NewPosX-1,NewY is 2*P2-1,getElementFromMatrix(Board,NewX,NewY,1,1,Value),(Value\=p,Value\=j,replaceMatrix(Board,NewX,NewY,1,PlayerChar,StackBoard);StackBoard=Board),changePawn(NewPosX,P2,L1);
Char=='N',NewPosX is P1-1,NewX is 2*NewPosX-1,NewY is 2*P2-1,getElementFromMatrix(Board,NewX,NewY,1,1,Value),(Value\=p,Value\=j,replaceMatrix(Board,NewX,NewY,1,PlayerChar,StackBoard);StackBoard=Board),changePawn(NewPosX,P2,L1);
Char=='SS',NewPosX is P1+2,NewX is 2*NewPosX-1,NewY is 2*P2-1,getElementFromMatrix(Board,NewX,NewY,1,1,Value),(Value\=p,Value\=j,replaceMatrix(Board,NewX,NewY,1,PlayerChar,StackBoard);StackBoard=Board),changePawn(NewPosX,P2,L1);
Char=='S',NewPosX is P1+1,NewX is 2*NewPosX-1,NewY is 2*P2-1,getElementFromMatrix(Board,NewX,NewY,1,1,Value),(Value\=p,Value\=j,replaceMatrix(Board,NewX,NewY,1,PlayerChar,StackBoard);StackBoard=Board),changePawn(NewPosX,P2,L1);
Char=='OO',NewPosY is P2-2,NewX is 2*P1-1,NewY is 2*NewPosY-1,getElementFromMatrix(Board,NewX,NewY,1,1,Value),(Value\=p,Value\=j,replaceMatrix(Board,NewX,NewY,1,PlayerChar,StackBoard);StackBoard=Board),changePawn(P1,NewPosY,L1);
Char=='O',NewPosY is P2-1,NewX is 2*P1-1,NewY is 2*NewPosY-1,getElementFromMatrix(Board,NewX,NewY,1,1,Value),(Value\=p,Value\=j,replaceMatrix(Board,NewX,NewY,1,PlayerChar,StackBoard);StackBoard=Board),changePawn(P1,NewPosY,L1);
Char=='EE',NewPosY is P2+2,NewX is 2*P1-1,NewY is 2*NewPosY-1,getElementFromMatrix(Board,NewX,NewY,1,1,Value),(Value\=p,Value\=j,replaceMatrix(Board,NewX,NewY,1,PlayerChar,StackBoard);StackBoard=Board),changePawn(P1,NewPosY,L1);
Char=='E',NewPosY is P2+1,NewX is 2*P1-1,NewY is 2*NewPosY-1,getElementFromMatrix(Board,NewX,NewY,1,1,Value),(Value\=p,Value\=j,replaceMatrix(Board,NewX,NewY,1,PlayerChar,StackBoard);StackBoard=Board),changePawn(P1,NewPosY,L1);
Char=='SE',NewPosX is P1+1,NewPosY is P2+1,NewX is 2*NewPosX-1,NewY is 2*NewPosY-1,getElementFromMatrix(Board,NewX,NewY,1,1,Value),(Value\=p,Value\=j,replaceMatrix(Board,NewX,NewY,1,PlayerChar,StackBoard);StackBoard=Board),changePawn(NewPosX,NewPosY,L1);
Char=='NE',NewPosX is P1-1,NewPosY is P2+1,NewX is 2*NewPosX-1,NewY is 2*NewPosY-1,getElementFromMatrix(Board,NewX,NewY,1,1,Value),(Value\=p,Value\=j,replaceMatrix(Board,NewX,NewY,1,PlayerChar,StackBoard);StackBoard=Board),changePawn(NewPosX,NewPosY,L1);
Char=='NO',NewPosX is P1-1,NewPosY is P2-1,NewX is 2*NewPosX-1,NewY is 2*NewPosY-1,getElementFromMatrix(Board,NewX,NewY,1,1,Value),(Value\=p,Value\=j,replaceMatrix(Board,NewX,NewY,1,PlayerChar,StackBoard);StackBoard=Board),changePawn(NewPosX,NewPosY,L1);
Char=='SO',NewPosX is P1+1,NewPosY is P2-1,NewX is 2*NewPosX-1,NewY is 2*NewPosY-1,getElementFromMatrix(Board,NewX,NewY,1,1,Value),(Value\=p,Value\=j,replaceMatrix(Board,NewX,NewY,1,PlayerChar,StackBoard);StackBoard=Board),changePawn(NewPosX,NewPosY,L1)),
(LastX is 2*P1-1, LastY is 2*P2-1,getElementFromMatrix(Board,LastX,LastY,1,1,LastValue) ,LastValue\=p,LastValue\=j, replaceMatrix(StackBoard,LastX,LastY,1,c,NewBoard);NewBoard=StackBoard).

checkPawnColision(NewX,NewY):-pawn1([Pawn1x,Pawn1y]),pawn2([Pawn2x,Pawn2y]),pawn3([Pawn3x,Pawn3y]),pawn4([Pawn4x,Pawn4y]),
(NewX=\=2*Pawn3x-1;NewY=\=2*Pawn3y-1),(NewX=\=2*Pawn4x-1;NewY=\=2*Pawn4y-1),
(NewX=\=2*Pawn2x-1;NewY=\=2*Pawn2y-1),(NewX=\=(2*Pawn1x-1);NewY=\=(2*Pawn1y-1)),!.

canPassWalls(Board,[P1,P2],Char):-Char=='NN',P1-2>0,P3 is 2*P1-2,getElementFromMatrix(Board,P3,P2,1,1,Value1),Value1\=w,P4 is 2*P1-4,getElementFromMatrix(Board,P4,P2,1,1,Value2),Value2\=w;
Char=='N',P1-1>0,P3 is 2*P1-2,getElementFromMatrix(Board,P3,P2,1,1,Value1),Value1\=w;
Char=='SS',P1+2<15,P3 is 2*P1,getElementFromMatrix(Board,P3,P2,1,1,Value1),Value1\=w,P4 is 2*P1+2,getElementFromMatrix(Board,P4,P2,1,1,Value2),Value2\=w;
Char=='S',P1+1<15,P3 is 2*P1,getElementFromMatrix(Board,P3,P2,1,1,Value1),Value1\=w;
Char=='OO',P2-2>0,P3 is 2*P1-1,P4 is 2*P2-2,getElementFromMatrix(Board,P3,P4,1,1,Value1),Value1\=q,P5 is 2*P2-4,getElementFromMatrix(Board,P3,P5,1,1,Value2),Value2\=q;
Char=='O',P2-1>0,P3 is 2*P1-1, P4 is 2*P2,getElementFromMatrix(Board,P3,P4,1,1,Value1),Value1\=q;
Char=='EE',P2+2<12,P3 is 2*P1-1,P4 is 2*P2,getElementFromMatrix(Board,P3,P4,1,1,Value1),Value1\=q,P5 is 2*P2-2,getElementFromMatrix(Board,P3,P5,1,1,Value2),Value2\=q;
Char=='E',P2+1<12,P3 is 2*P1-1, P4 is 2*P2,getElementFromMatrix(Board,P3,P4,1,1,Value1),Value1\=q;
Char=='SE',P2+1<12,P1+1<15,(P3 is 2*P1-1,P4 is 2*P2,getElementFromMatrix(Board,P3,P4,1,1,Value1),Value1\=q,P5 is 2*P1 ,P6 is P2 +1,getElementFromMatrix(Board,P5,P6,1,1,Value2),Value2\=w;
                            P3 is 2*P1,getElementFromMatrix(Board,P3,P2,1,1,Value1),Value1\=w,P4 is 2*P1+1,P5 is 2*P2,getElementFromMatrix(Board,P4,P5,1,1,Value2),Value2\=w) ;
Char=='NE',P1-1>0,P2+1<12,(P3 is 2*P1-1,P4 is 2*P2,getElementFromMatrix(Board,P3,P4,1,1,Value1),Value1\=q,P5 is 2*P1-2 ,P6 is P2 +1,getElementFromMatrix(Board,P5,P6,1,1,Value2),Value2\=w;
                           P3 is 2*P1-2,getElementFromMatrix(Board,P3,P2,1,1,Value1),Value1\=w,P4 is 2*P1-3,P5 is 2*P2,getElementFromMatrix(Board,P4,P5,1,1,Value2),Value2\=w);
Char=='NO',P2-1>0,P1-1>0,(P3 is 2*P1-1,P4 is 2*(P2-1),getElementFromMatrix(Board,P3,P4,1,1,Value1),Value1\=q,P5 is 2*(P1-1) ,P6 is P2 -1,getElementFromMatrix(Board,P5,P6,1,1,Value2),Value2\=w;
                           P3 is 2*P1-2,getElementFromMatrix(Board,P3,P2,1,1,Value1),Value1\=w,P4 is 2*P1-3,P5 is 2*(P2-1),getElementFromMatrix(Board,P4,P5,1,1,Value2),Value2\=w);
Char=='SO',P2-1>0,P1+1<15,(P3 is 2*P1-1,P4 is 2*(P2-1),getElementFromMatrix(Board,P3,P4,1,1,Value1),Value1\=q,P5 is 2*P1 ,P6 is P2 -1,getElementFromMatrix(Board,P5,P6,1,1,Value2),Value2\=w;
                           P3 is 2*P1,getElementFromMatrix(Board,P3,P2,1,1,Value1),Value1\=w,P4 is 2*P1+1,P5 is 2*(P2-1),getElementFromMatrix(Board,P4,P5,1,1,Value2),Value2\=w).

changePawn(X,Y,[X,Y]).

multiply(A,B,Product):-Product is A*B.

movePawnRandomly(Board,Turn,NewBoard):-pawn1(Pawn1),pawn2(Pawn2),pawn3(Pawn3),pawn4(Pawn4),random(1,3,RandomPawn),
(RandomPawn==1, (Turn==1,availablePositions(Board,Pawn1);Turn==2,availablePositions(Board,Pawn3));
RandomPawn==2,(Turn==1,availablePositions(Board,Pawn2);Turn==2,availablePositions(Board,Pawn4))),
availableList(List),retract(availableList(_)),sizeOfList(List,Size), RealSize is Size+1,random(1,RealSize,RandomPos),positionInList(RandomPos,List,1,Choice),
(RandomPawn==1,(Turn==1,move(Board,Pawn1,Choice,NewPawn,NewBoard,r),retract(pawn1(_)),asserta(pawn1(NewPawn));Turn==2,move(Board,Pawn3,Choice,NewPawn,NewBoard,e),retract(pawn3(_)),asserta(pawn3(NewPawn)));
RandomPawn==2,(Turn==1,move(Board,Pawn2,Choice,NewPawn,NewBoard,r),retract(pawn2(_)),asserta(pawn2(NewPawn));Turn==2,move(Board,Pawn4,Choice,NewPawn,NewBoard,e),retract(pawn4(_)),asserta(pawn4(NewPawn)))).

sizeOfList([],0).
sizeOfList([_|L2],Size):-sizeOfList(L2,Size1),Size is Size1+1.

positionInList(Pos,[L1|L2],Iterador,Result):-Iterador==Pos,Result =L1;Iterador1 is Iterador+1,positionInList(Pos,L2,Iterador1,Result).

availablePositions(Board,[P1,P2]):-
canPassWalls(Board,[P1,P2],'NN'),NewPosX is P1-2,NewX is 2*NewPosX-1,NewY is 2*P2-1,checkPawnColision(NewX,NewY),(\+ availableList(List),asserta(availableList(['NN']));availableList(List),\+ member('NN',List),retract(availableList(_)),append(['NN'],List,NewList),asserta(availableList(NewList))),fail;
canPassWalls(Board,[P1,P2],'N'),NewPosX is P1-1,NewX is 2*NewPosX-1,NewY is 2*P2-1,checkPawnColision(NewX,NewY),(\+availableList(List),asserta(availableList(['N']));availableList(List),\+ member('N',List),retract(availableList(_)),append(['N'],List,NewList),asserta(availableList(NewList))),fail;
canPassWalls(Board,[P1,P2],'SS'),NewPosX is P1+2,NewX is 2*NewPosX-1,NewY is 2*P2-1,checkPawnColision(NewX,NewY),(\+availableList(List),asserta(availableList(['SS']));availableList(List),\+ member('SS',List),retract(availableList(_)),append(['SS'],List,NewList),asserta(availableList(NewList))),fail;
canPassWalls(Board,[P1,P2],'S'),NewPosX is P1+1,NewX is 2*NewPosX-1,NewY is 2*P2-1,checkPawnColision(NewX,NewY),(\+availableList(List),asserta(availableList(['S']));availableList(List),\+ member('S',List),retract(availableList(_)),append(['S'],List,NewList),asserta(availableList(NewList))),fail;
canPassWalls(Board,[P1,P2],'OO'),NewPosY is P2-2,NewX is 2*P1-1,NewY is 2*NewPosY-1,checkPawnColision(NewX,NewY),(\+availableList(List),asserta(availableList(['OO']));availableList(List),\+ member('OO',List),retract(availableList(_)),append(['OO'],List,NewList),asserta(availableList(NewList))),fail;
canPassWalls(Board,[P1,P2],'O'),NewPosY is P2-1,NewX is 2*P1-1,NewY is 2*NewPosY-1,checkPawnColision(NewX,NewY),(\+availableList(List),asserta(availableList(['O']));availableList(List),\+ member('O',List),retract(availableList(_)),append(['O'],List,NewList),asserta(availableList(NewList))),fail;
canPassWalls(Board,[P1,P2],'EE'),NewPosY is P2+2,NewX is 2*P1-1,NewY is 2*NewPosY-1,checkPawnColision(NewX,NewY),(\+availableList(List),asserta(availableList(['EE']));availableList(List),\+ member('EE',List),retract(availableList(_)),append(['EE'],List,NewList),asserta(availableList(NewList))),fail;
canPassWalls(Board,[P1,P2],'E'),NewPosY is P2+1,NewX is 2*P1-1,NewY is 2*NewPosY-1,checkPawnColision(NewX,NewY),(\+availableList(List),asserta(availableList(['E']));availableList(List),\+ member('E',List),retract(availableList(_)),append(['E'],List,NewList),asserta(availableList(NewList))),fail;
canPassWalls(Board,[P1,P2],'NO'),NewPosX is P1-1,NewX is 2*NewPosX-1,NewPosY is P2-1,NewY is 2*NewPosY-1,checkPawnColision(NewX,NewY),(\+availableList(List),asserta(availableList(['NO']));availableList(List),\+ member('NO',List),retract(availableList(_)),append(['NO'],List,NewList),asserta(availableList(NewList))),fail;
canPassWalls(Board,[P1,P2],'NE'),NewPosX is P1-1,NewX is 2*NewPosX-1,NewPosY is P2+1,NewY is 2*NewPosY-1,checkPawnColision(NewX,NewY),(\+availableList(List),asserta(availableList(['NE']));availableList(List),\+ member('NE',List),retract(availableList(_)),append(['NE'],List,NewList),asserta(availableList(NewList))),fail;
canPassWalls(Board,[P1,P2],'SE'),NewPosX is P1+1,NewX is 2*NewPosX-1,NewPosY is P2+1,NewY is 2*NewPosY-1,checkPawnColision(NewX,NewY),(\+availableList(List),asserta(availableList(['SE']));availableList(List),\+ member('SE',List),retract(availableList(_)),append(['SE'],List,NewList),asserta(availableList(NewList))),fail;
canPassWalls(Board,[P1,P2],'SO'),NewPosX is P1+1,NewX is 2*NewPosX-1,NewPosY is P2-1,NewY is 2*NewPosY-1,checkPawnColision(NewX,NewY),(\+availableList(List),asserta(availableList(['SO']));availableList(List),\+ member('SO',List),retract(availableList(_)),append(['SO'],List,NewList),asserta(availableList(NewList))),fail;
true.

choosePositionToMove(Place):-
  repeat,
  write('--------------------------------------------------------------------------------------------------'),nl,
  write('|In what position would you like to place the pawn?                                              |'),nl,
  write('|You can choose from "N","NN","NO","NE","EE","SE","S","SS","SO","O" or "OO" (with "").                     |'),nl,
	write('|O means one position to the left, OO means two positions to the left, etc.                      |'),nl,
	write('|REMEMBER: You can not go through "-", through "|", nor to a position where an enemy pawn stands.|'),nl,
  write('--------------------------------------------------------------------------------------------------'),nl,
	prcss_str(Place).

chooseStaringPiece(Turn,Ans):-
  repeat,
  write('---------------------------------------------------'),nl,
  write('|It´s player´s '),write(Turn),write(' turn.                          |'),nl,
	write('|Which pawn would you like to move?               |'),nl,
  pawn1(P1),pawn2(P2),pawn3(P3),pawn4(P4),
	write('|1- O peao que se encontra na posicao '),(Turn==1,write(P1);Turn==2,write(P3)),write('       |'),nl,
	write('|2- O peao que se encontra na posicao '),(Turn==1,write(P2);Turn==2,write(P4)),write('       |'),nl,
	write('---------------------------------------------------'),nl,prcss_ans(1,2,Ans).


startDisplay(Board,X,Y):-write('    1 2 3 4 5 6 7 8 9 1011'),nl,
write('    ---------------------'),nl,display(Board,X,Y),write('    ---------------------'),nl.
display([],_,_).
display([L1|L2],X,Y):-(X mod 2=\=0,X2 is round((X+1)/2),write(X2),(X2>9,write('-'),write('|');write(' -'),write('|'));write('    ') ),displayy(L1,X,Y),(X mod 2=\=0,write('|');true),nl,X1 is X+1,display(L2,X1,Y).
displayy([],_,_).
displayy([L1|L2],X,Y):-translate(L1,X,Y,Value),write(Value),Y1 is Y+1,displayy(L2,X,Y1).

translate(Char,X,Y,Result):-pawn1([L3,L7]),pawn2([L4,L8]),pawn3([L5,L9]),pawn4([L6,L10]),(Char==w,Result ='- ';Char ==q, Result ='|';Char ==a, Result =':';Char ==b, Y\=12, Result ='* ';
Char ==c, Result =' ';Char ==r, Result ='X';Char ==e, Result ='O';Char ==p, ((2*L3-1=:=X,2*L7-1=:=Y;2*L4-1=:=X,2*L8-1=:=Y),Result ='X';(2*L5-1=:=X,2*L9-1=:=Y;2*L6-1=:=X,2*L10-1=:=Y),Result ='O';Result ='+');
Char ==j, ((2*L3-1=:=X,2*L7-1=:=Y;2*L4-1=:=X,2*L8-1=:=Y),Result ='X';(2*L5-1=:=X,2*L9-1=:=Y;2*L6-1=:=X,2*L10-1=:=Y),Result ='O';Result ='o')).

getElementFromMatrix([_|L2],X,Y,Linha,Coluna,Value):-Linha<X,Linha1 is Linha +1,getElementFromMatrix(L2,X,Y,Linha1,Coluna,Value).
getElementFromMatrix([L1|_],X,Y,Linha,Coluna,Value):-Linha==X,getElementFromMatrixC(L1,X,Y,Linha,Coluna,Value).
getElementFromMatrixC([_|L2],X,Y,Linha,Coluna,Value):-Coluna<Y, Coluna1 is Coluna+1,getElementFromMatrixC(L2,X,Y,Linha,Coluna1,Value).
getElementFromMatrixC([L1|_],_,Y,_,Coluna,L1):-Coluna==Y.

replaceMatrix(Board,NewX,NewY,1,PlayerChar,NewBoard):-replaceElementFromMatrix(Board,NewX,NewY,1,PlayerChar,L3),NL is NewX-1,replace(Board,NL,L3,NewBoard).
replaceElementFromMatrix([_|L2],X,Y,Linha,Value,L3):-Linha<X,Linha1 is Linha +1,replaceElementFromMatrix(L2,X,Y,Linha1,Value,L3).
replaceElementFromMatrix([L1|_],X,Y,Linha,Value,L3):-Linha==X,Y1 is Y-1,replace(L1,Y1,Value,L3).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).

checkPath(Board,X,Y,[C1,C2],[C3,C4]):-(visited(Lista),(member([C1,C2],Lista),member([C3,C4],Lista); X1 is X+2,XWall is X+1,YWall is round((Y+1)/2),X1<28,getElementFromMatrix(Board,XWall,YWall,1,1,Value),Value\=w,\+ member([X1,Y],Lista),append(Lista,[[X1,Y]],NewVisited),retract(visited(_)),asserta(visited(NewVisited)),checkPath(Board,X1,Y,[C1,C2],[C3,C4]));
visited(Lista),(member([C1,C2],Lista),member([C3,C4],Lista);X1 is X-2,X1>0, XWall is X-1,YWall is round((Y+1)/2),getElementFromMatrix(Board,XWall,YWall,1,1,Value),Value\=w, \+ member([X1,Y],Lista),append(Lista,[[X1,Y]],NewVisited),retract(visited(_)),asserta(visited(NewVisited)),checkPath(Board,X1,Y,[C1,C2],[C3,C4]));
visited(Lista),(member([C1,C2],Lista),member([C3,C4],Lista);Y1 is Y+2,Y1<22, YWall is Y+1,getElementFromMatrix(Board,X,YWall,1,1,Value),Value\=q, \+ member([X,Y1],Lista),append(Lista,[[X,Y1]],NewVisited),retract(visited(_)),asserta(visited(NewVisited)),checkPath(Board,X,Y1,[C1,C2],[C3,C4]));
visited(Lista),(member([C1,C2],Lista),member([C3,C4],Lista);Y1 is Y-2,Y1>0, YWall is Y-1,getElementFromMatrix(Board,X,YWall,1,1,Value),Value\=q, \+ member([X,Y1],Lista),append(Lista,[[X,Y1]],NewVisited),retract(visited(_)),asserta(visited(NewVisited)),checkPath(Board,X,Y1,[C1,C2],[C3,C4]))).
checkPath(_,_,_,[_,_],[_,_]):-false.

checkColisionVertical(Board,[C1,C2]):-C3 is 2* C1-1,C4 is C2*2,C5 is 2* C1+1,getElementFromMatrix(Board,C3,C4,1,1,Value),Value ==a,getElementFromMatrix(Board,C5,C4,1,1,Value2),Value2 ==a ,P1 is C1*2,checkColisionVerticalAux(Board,[P1,C2],Return),!,Return mod 2 =:=0.

checkColisionVerticalAux(Board,[P1,P2],Return):-getElementFromMatrix(Board,P1,P2,1,1,Value),Value ==w,P3 is P2-1,P3>0,checkColisionVerticalAux(Board,[P1,P3],ReturnAux),Return is ReturnAux+1.
checkColisionVerticalAux(Board,[P1,P2],1):-getElementFromMatrix(Board,P1,P2,1,1,Value),Value ==w.
checkColisionVerticalAux(_,[_,_],0).

checkColisionHorizontal(Board,[C1,C2]):-C3 is 2* C1,C4 is C2+1,getElementFromMatrix(Board,C3,C2,1,1,Value),Value ==b,getElementFromMatrix(Board,C3,C4,1,1,Value2),Value2 ==b,P1 is C1*2-1,P2 is C2*2,checkColisionHorizontalAux(Board,[P1,P2],Return),!,Return mod 2 =:=0.

checkColisionHorizontalAux(Board,[P1,P2],Return):-getElementFromMatrix(Board,P1,P2,1,1,Value),Value ==q,P3 is P1-2,P3>0,checkColisionHorizontalAux(Board,[P3,P2],ReturnAux),Return is ReturnAux+1.
checkColisionHorizontalAux(Board,[P1,P2],1):-getElementFromMatrix(Board,P1,P2,1,1,Value),Value ==q.
checkColisionHorizontalAux(_,[_,_],0).

?-start(?).
