% ----------------------------------------
% --  Sistema experto
% ----------------------------------------

%importamos la base de conocimiento
:- include(base).

% valida si es un equipo valido
es_un_equipo(Nombre):- equipo(Nombre,_).

% despliega el marcador de un partido
marcador(Partido,Local,Visitante,GL,GV,Status):- 
    evento(_,Partido,_,IdLocal,GL,IdVisitante,GV,Status)
    ,equipo(Local,IdLocal)
    ,equipo(Visitante,IdVisitante)
    %output esperado
    % 1 liverpol vs chelsea 1 - 2
    %,write(Partido),tab(4),write(Local),write(' vs '),write(Visitante),tab(4),write(GL),write('-'),write(GV)
    .
%Despliega la tabla
drawtabla(Temporada):-
    calcTabla(Temporada,Tabla)
    % obtenemos la lista de IdEquipos
    ,writeln('╔══════════════════════════════════╗')
      ,write('║             ')
                ,write(Temporada)
                          ,writeln('             ║')
    ,writeln('╠═══════════════════╦══════════════╣')
    ,writeln('║       Equipo      ║    Puntos    ║')
    ,writeln('╠═══════════════════╬══════════════╣')
    % ,write(ListaParti)
    ,drawRow(Tabla)
    .

%dibuja de manera recursiva las filas de la tabla
drawRow([Nombre,Puntos|Tabla]):-
    string_length(Nombre,Size)
    ,Tab is 18-Size
    ,write('║ '),write(Nombre),tab(Tab),write('║   '),write(Puntos),writeln('         ║')
    ,drawRow(Tabla)
    .
drawRow([]):- writeln('╚═══════════════════╩══════════════╝').

%inicia el metodo recursivo para generar una tabla con formato [Nombre,Puntos]
calcTabla(Temporada,Tabla):-
    findall(X,participantes(Temporada,X),ListaParti)
    ,calcRow(Temporada,ListaParti,Tabla)
    .
% determina el fin recursivo de calcRow
calcRow(_,[],Tabla):- Tabla=[].
% obtiene el id de equipo, consigue el nombre y calcula los puntos acumulados hasta la fecha
calcRow(Temporada,[IdEquipo|ListaParti],Tabla):-
    equipo(Nombre,IdEquipo)
    ,sumatoria(Temporada,IdEquipo,Puntos)
    ,calcRow(Temporada,ListaParti,TablaTemp)
    ,Row=[Nombre,Puntos]
    ,append(TablaTemp,Row,Tabla)
    .

%sumatoria de puntos de la temporada
sumatoria(Temporada,IdEquipo,Puntos):-
    %listamos los partidos en local y visitante
    findall(X,evento(Temporada,X,_,IdEquipo,_,_,_,terminado),Lloc)
    ,findall(X,evento(Temporada,X,_,_,_,IdEquipo,_,terminado),Lvis)
    ,sumPuntos(Lloc,local,Ploc)
    ,sumPuntos(Lvis,visitante,Pvis)
    %,write(Ploc),tab(4),writeln(Pvis) % debug
    ,Puntos is Ploc + Pvis
    .

%realiza la suma recursiva de puntos de la temporada
sumPuntos([],_,Puntos):-
    Puntos is 0.
sumPuntos([Head|Tail],Tipo,Puntos):-
    sumPuntos(Tail,Tipo,P1)
    ,defPuntos(Head,Tipo,P2)
    %,write(Head),tab(4),write(P2),tab(4),writeln(P1) % debug
    ,Puntos is P1+P2
    .


%consulta de quien gano un partido especifico
defPuntos(Partido,local,P):-
    % adquirimos el registro como local
    evento(_,Partido,_,_,GF,_,GC,_),
    %se hace la comparativa
    (GF==GC ->
        P is 1;
        GF > GC ->
            P is 3;
            P is 0
    )
    .
%consulta de puntos en un partido especifico
defPuntos(Partido,visitante,P):-
    % adquirimos el registro como local
    evento(_,Partido,_,_,GC,_,GF,_),
    %se hace la comparativa
    (GF==GC ->
        P is 1;
        GF > GC ->
            P is 3;
            P is 0
    )
    .