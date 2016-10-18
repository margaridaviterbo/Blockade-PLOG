menu(X):-
write('Choose type of game you wish to play:'), nl,
 write('1- 1 v 1'), nl,
			write('2- you vs cpu'), nl,
			write('3- cpu vs cpu'), nl, prcss_ans(1,3,Ans);
			
			write('You must choose an answer between 1 and 3, then insert "." and press ENTER!').

prcss_ans(Min,Max,Ans):-read(Ans),integer(Ans),Ans=<Max, Ans>=Min,nl.
prcss_str(Ans):-read(Str), atom_codes(Ans,Str), (Str==[79,79];Str==[79];Str==[69];Str==[69,69];Str==[83,83];Str==[83];Str==[78,78];Str==[78];Str==[78,69];Str==[78,79];Str==[83,69];Str==[83,79]).

board([[' = ','= ','= ','= ','= ','= ','= ','= ','= ','= ','= '],
['|',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ','|'],
['|','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','|'],
['|',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ','|'],
['|','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','|'],
['|',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ','|'],
['|','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','|'],
['|',' ',':',' ',':',' ',':','X',':',' ',':',' ',':',' ',':','X',':',' ',':',' ',':',' ','|'],
['|','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','|'],
['|',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ','|'],
['|','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','|'],
['|',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ','|'],
['|','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','|'],
['|',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ','|'],
['|','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','|'],
['|',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ','|'],
['|','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','|'],
['|',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ','|'],
['|','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','|'],
['|',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ','|'],
['|','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','|'],
['|',' ',':',' ',':',' ',':','y',':',' ',':',' ',':',' ',':','Y',':',' ',':',' ',':',' ','|'],
['|','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','|'],
['|',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ','|'],
['|','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','|'],
['|',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ','|'],
['|','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','- ','|'],
['|',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ',':',' ','|'],
[' = ','= ','= ','= ','= ','= ','= ','= ','= ','= ','= ']]).

tentativa([[z,w,w,w,w,w,w,w,w,w,w],
	[q,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,q],
	[q,b,b,b,b,b,b,b,b,b,b,b,q],
	[q,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,q],
	[q,b,b,b,b,b,b,b,b,b,b,b,q],
	[q,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,q],
	[q,b,b,b,b,b,b,b,b,b,b,b,q],
	[q,c,a,c,a,c,a,p,a,c,a,c,a,c,a,p,a,c,a,c,a,c,q],
	[q,b,b,b,b,b,b,b,b,b,b,b,q],
	[q,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,q],
	[q,b,b,b,b,b,b,b,b,b,b,b,q],
	[q,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,q],
	[q,b,b,b,b,b,b,b,b,b,b,b,q],
	[q,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,q],
	[q,b,b,b,b,b,b,b,b,b,b,b,q],
	[q,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,q],
	[q,b,b,b,b,b,b,b,b,b,b,b,q],
	[q,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,q],
	[q,b,b,b,b,b,b,b,b,b,b,b,q],
	[q,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,q],
	[q,b,b,b,b,b,b,b,b,b,b,b,q],
	[q,c,a,c,a,c,a,j,a,c,a,c,a,c,a,j,a,c,a,c,a,c,q],
	[q,b,b,b,b,b,b,b,b,b,b,b,q],
	[q,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,q],
	[q,b,b,b,b,b,b,b,b,b,b,b,q],
	[q,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,q],
	[q,b,b,b,b,b,b,b,b,b,b,b,q],
	[q,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,a,c,q],
	[z,w,w,w,w,w,w,w,w,w,w]]).

display_board([]).
display_board([L1|L2]):-display_line(L1),nl,display_board(L2).
display_line([]).
display_line([L1|L2]):-write(L1),display_line(L2).

start(X):-menu(X),tentativa(L1),J1=player([4,4],[4,8],8,8),J2=player([11,4],[11,8],8,8),play(L1,J1,J2,1).

play(L1,J1,J2,Turn):-nl,nl,J1=player(P1,P2,H1,V1),J2=player(P3,P4,H2,V2),display(L1,P1,P2,P3,P4,1,1),
	chooseStaringPiece(P1,P2,Turn,Pawn),
	choosePositionToMove(Place),(Pawn==1,move(L1,P1,Place,NewPawn,NewBoard),play(NewBoard,player(NewPawn,P2,8,8),player(P3,P4,8,8),1);
	Pawn==2,move(L1,P2,Place,NewPawn,NewBoard),play(NewBoard,player(P1,NewPawn,8,8),player(P3,P4,8,8),1)).
	
	
move(Board,[P1,P2],'NN',L1,NewBoard):-P1-2>0,NewPosX is P1-2,multiply(NewPosX,2,NewX),multiply(P2,2,NewY),getElementFromMatrix(Board,NewX,NewY,1,1,Value),Value\=r,Value\=e,replaceMatrix(Board,NewX,NewY,1,r,NewBoard),changePawn(NewPosX,P2,L1).
move(Board,[P1,P2],'N',L1,NewBoard):-P1-1>0,NewPosX is P1-1,multiply(NewPosX,2,NewX),multiply(P2,2,NewY),getElementFromMatrix(Board,NewX,NewY,1,1,Value),Value\=r,Value\=e,replaceMatrix(Board,NewX,NewY,1,r,NewBoard),changePawn(NewPosX,P2,L1).
move(Board,[P1,P2],'SS',L1,NewBoard):-P1+2>0,NewPosX is P1+2,multiply(NewPosX,2,NewX),multiply(P2,2,NewY),getElementFromMatrix(Board,NewX,NewY,1,1,Value),Value\=r,Value\=e,replaceMatrix(Board,NewX,NewY,1,r,NewBoard),changePawn(NewPosX,P2,L1).
move(Board,[P1,P2],'S',L1,NewBoard):-P1+1>0,NewPosX is P1+1,multiply(NewPosX,2,NewX),multiply(P2,2,NewY),getElementFromMatrix(Board,NewX,NewY,1,1,Value),Value\=r,Value\=e,replaceMatrix(Board,NewX,NewY,1,r,NewBoard),changePawn(NewPosX,P2,L1).
move(Board,[P1,P2],'OO',L1,NewBoard):-P2-2>0,NewPosY is P2-2,multiply(NewPosY,2,NewY),multiply(P1,2,NewX),getElementFromMatrix(Board,NewX,NewY,1,1,Value),Value\=r,Value\=e,replaceMatrix(Board,NewX,NewY,1,r,NewBoard),changePawn(P1,NewPosY,L1).
move(Board,[P1,P2],'O',L1,NewBoard):-P2-1>0,NewPosY is P2-1,multiply(NewPosY,2,NewY),multiply(P1,2,NewX),getElementFromMatrix(Board,NewX,NewY,1,1,Value),Value\=r,Value\=e,replaceMatrix(Board,NewX,NewY,1,r,NewBoard),changePawn(P1,NewPosY,L1).
move(Board,[P1,P2],'EE',L1,NewBoard):-P2+2>0,NewPosY is P2+2,multiply(NewPosY,2,NewY),multiply(P1,2,NewX),getElementFromMatrix(Board,NewX,NewY,1,1,Value),Value\=r,Value\=e,replaceMatrix(Board,NewX,NewY,1,r,NewBoard),changePawn(P1,NewPosY,L1).
move(Board,[P1,P2],'E',L1,NewBoard):-P2+1>0,NewPosY is P2+1,multiply(NewPosY,2,NewY),multiply(P1,2,NewX),getElementFromMatrix(Board,NewX,NewY,1,1,Value),Value\=r,Value\=e,replaceMatrix(Board,NewX,NewY,1,r,NewBoard),changePawn(P1,NewPosY,L1).
move(Board,[P1,P2],'SE',L1,NewBoard).
move(Board,[P1,P2],'NE',L1,NewBoard).
move(Board,[P1,P2],'NO',L1,NewBoard).
move(Board,[P1,P2],'SO',L1,NewBoard).

changePawn(X,Y,[X,Y]).

multiply(A,B,Product):-Product is A*B.

choosePositionToMove(Place):-write('In what position would you like to place the pawn? U can choose from "N","NN","NO","NE","EE","SE","S","SS","SO","O" or "OO"'),nl,
	write('O means one position to the left, OO means two positions to the left, etc.'),nl,
	write('REMEMBER!!! You can not go through "=", through "|", nor to a position where an enemy pawn stands.'),nl,
	prcss_str(Place);write('You can not move to that position!!'),nl,choosePositionToMove(Place).

chooseStaringPiece(P1,P2,Turn,Ans):-write('It´s player´s '),write(Turn),write(' turn.'),nl,
	write('Which pawn would you like to move?'),
	nl,write('1- O peao que se encontra na posicao '),write(P1),nl,
	write('2- O peao que se encontra na posicao '),write(P2),nl,
	prcss_ans(1,2,Ans);
	write('You must choose between 1 and 2!!!'),nl,chooseStaringPiece(P1,P2,Turn,Ans).


display([],L3,L4,L5,L6,X,Y).
display([L1|L2],L3,L4,L5,L6,X,Y):-displayy(L1,L3,L4,L5,L6,X,Y),nl,X1 is X+1,display(L2,L3,L4,L5,L6,X1,Y).
displayy([],L3,L4,L5,L6,X,Y).
displayy([L1|L2],L3,L4,L5,L6,X,Y):-translate(L1,L3,L4,L5,L6,X,Y,Value),write(Value),Y1 is Y+1,displayy(L2,L3,L4,L5,L6,X,Y1).

translate(z,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,' - ').
translate(w,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,'- ').
translate(q,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,'|').
translate(a,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,':').
translate(b,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,'*'):- Y==12.
translate(b,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,'* '):- Y\=12.
translate(c,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,' ').
translate(r,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,'X').
translate(e,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,'O').
translate(p,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,'X'):-2*L3=:=X,2*L7=:=Y;2*L4=:=X,2*L8=:=Y.
translate(p,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,'O'):-2*L5=:=X,2*L9=:=Y;2*L6=:=X,2*L10=:=Y.
translate(p,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,'+').
translate(j,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,'X'):-2*L3=:=X,2*L7=:=Y;2*L4=:=X,2*L8=:=Y.
translate(j,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,'O'):-2*L5=:=X,2*L9=:=Y;2*L6=:=X,2*L10=:=Y.
translate(j,[L3,L7],[L4,L8],[L5,L9],[L6,L10],X,Y,'o').

getElementFromMatrix([L1,L2],X,Y,Linha,Coluna,Value):-Linha<X,Linha1 is Linha +1,getElementFromMatrix(L2,X,Y,Linha1,Coluna,Value).
getElementFromMatrix([L1,L2],X,Y,Linha,Coluna,Value):-Linha==X,getElementFromMatrixC(L1,X,Y,Linha,Coluna,Value).
getElementFromMatrixC([L1,L2],X,Y,Linha,Coluna,Value):-Coluna<Y, Coluna1 is Coluna+1,getElementFromMatrixC(L2,X,Y,Linha,Coluna1,Value).
getElementFromMatrixC([L1,L2],X,Y,Linha,Coluna,L1):-Coluna==Y.

replaceMatrix(Board,NewX,NewY,1,e,NewBoard):-replaceElementFromMatrix(Board,NewX,NewY,1,e,L3),NL is NewX-1,replace(Board,NL,L3,NewBoard).
replaceElementFromMatrix([L1,L2],X,Y,Linha,Value,L3):-Linha<X,Linha1 is Linha +1,replaceElementFromMatrix(L2,X,Y,Linha1,Value,L3).
replaceElementFromMatrix([L1,L2],X,Y,Linha,Value,L3):-Linha==X,Y1 is Y-1,replace(L1,Y1,Value,L3).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).
?-trace.
?- start(X).