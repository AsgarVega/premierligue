/*
 * Ejemplo sacado de https://www.monolune.com/articles/web-programming-in-swi-prolog/
 */


/* 
    :- use_module(library(http/thread_httpd)).
    :- use_module(library(http/http_dispatch)).
    :- use_module(library(http/http_json)).

    % URL handlers.
    :- http_handler('/', handle_request, []).

    % Calculates a + b.
    solve(_{a:X, b:Y}, _{answer:N}) :-
        number(X),
        number(Y),
        N is X + Y.

    handle_request(Request) :-
        http_read_json_dict(Request, Query),
        solve(Query, Solution),
        reply_json_dict(Solution).

    server(Port) :-
        http_server(http_dispatch, [port(Port)]).

    :- initialization(server(8000)). 
 */

/*
    library(http/thread_httpd)
        This library is responsible for accepting and
        managing connections.2
    library(http/http_dyn_workers)
        This library dynamically adds and removes 
        workers based on the workload of the server.
    library(http/http_wrapper)
        This library takes a connection, parses the 
        HTTP request header and runs a goal that pro-
        duces a CGI document based on the parsed request.
        It watches for exceptions and turns these into
        (error) status pages. The status page generation
        may be hooked to provide custom pages.
    library(http/http_dispatch)
        This library associates the path of the HTTP 
        request with a handler that services this 
        particular path. It also manages timeouts and 
        may pass the execution of a request to a dedi-
        cated thread with specified resource limits 
        using http_spawn/2. The module supports pluga-
        ble request rewrite handlers that may be used 
        to implement identification, authorization, 
        input argument processing, etc.
    library(http/http_parameters)
        This library parses HTTP request parameters, 
        both dealing with GET and POST style parameter
        passing.
    library(http/html_write)
        This library translates a Prolog term into an 
        HTML document using Prolog grammar rules (DCG).
        It provides a modular infrastructure to build 
        pages that are guaranteed to be valid HTML. The
        HTTP server libraries provide several alternatives 
        for generating HTML ranging from simple printing 
        to current_output to XML-based templates (PWP).
    library(http/http_json)
        This library parses a POSTed HTTP document into
        a Prolog dict and formulates an HTTP JSON reply
        from a Prolog dict and is typically used to 
        implement REST services.

    https://www.swi-prolog.org/pldoc/man?section=httpserver
*/
                                                  
% librerias
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
% base de conocimiento
%:- use_module(library(hechos)). asi no Xd
:- use_module(library(http/json_convert)).

:- json_object
      point(x:integer, y:integer).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- initialization(server(8000)). 

% URL handlers.
:- http_handler('/', handle_request, []). % escuchamos en /
:- http_handler('/partido', to_do, []).
:- http_handler('/tabla', to_do, []).
:- http_handler('/pronostico/partido', to_do, []).
:- http_handler('/pronostico/tabla', to_do, []).
:- http_handler('/equipos',response_es_un_equipo,[]).

% metodo que responde un to do para demstrar que no esta implementado aun
to_do(Request) :-
    atomics_to_string(Request,Text),
    string_concat('ToDo',Text,R),
    reply_json_dict(R).

handle_request(Request) :-
    reply_html_page(title('Titulo :3'),
        [ h1('Bienvenido'),
            p('Aquí se mostraran los posibles cosos de api rest')
        ]).
response_es_un_equipo(Request) :-
    es_un_equipo(R).
    reply_html_page(title('Lista de equipos'),
        [h1('lista de equipos'),
            p(R)
        ]).






% ----------------------------------------
% --  base de conocimiento
% ----------------------------------------

% equipo(Nombre,Id_Equipo).
equipo('arsenal',1).
equipo('aston villa',2).
equipo('bournemouth',3).
equipo('brighton',4).
equipo('burnley',5).
equipo('cardiff',6).
equipo('chelsea',7).
equipo('crystal palace',8).
equipo('everton',9).
equipo('fulham',10).
equipo('huddersfield town',11).
equipo('leicester city',12).
equipo('liverpool',13).
equipo('manchester city',14).
equipo('manchester united',15).
equipo('newcastle',16).
equipo('norwich city',17).
equipo('sheffield united',18).
equipo('southampton',19).
equipo('tottenham',20).
equipo('watford',21).
equipo('west ham',22).
equipo('wolves',23).

es_un_equipo(Nombre):- equipo(Nombre,_).

% evento deportivo
% evento('Temporada',Partido,'FECHA','Equipo Local',Goles a Favor Local,'Equipo Visitante',Goles a Favor Visitante,'status').
evento('20182019',1,'10/08/2018','Manchester United',2,'Leicester City',1,'terminado').
evento('20182019',2,'11/08/2018','Huddersfield Town',0,'Chelsea',3,'terminado').
evento('20182019',3,'11/08/2018','Watford',2,'Brighton',0,'terminado').
evento('20182019',4,'11/08/2018','Wolves',2,'Everton',2,'terminado').
evento('20182019',5,'12/08/2018','Liverpool',4,'West Ham',0,'terminado').
evento('20182019',6,'11/08/2018','Newcastle',1,'Tottenham',2,'terminado').
evento('20182019',7,'11/08/2018','Bournemouth',2,'Cardiff',0,'terminado').
evento('20182019',8,'11/08/2018','Fulham',0,'Crystal Palace',2,'terminado').
evento('20182019',9,'12/08/2018','Southampton',0,'Burnley',0,'terminado').
evento('20182019',10,'12/08/2018','Arsenal',0,'Manchester City',2,'terminado').
evento('20182019',11,'18/08/2018','Cardiff',0,'Newcastle',0,'terminado').
evento('20182019',12,'18/08/2018','West Ham',1,'Bournemouth',2,'terminado').
evento('20182019',13,'18/08/2018','Everton',2,'Southampton',1,'terminado').
evento('20182019',14,'19/08/2018','Manchester City',6,'Huddersfield Town',1,'terminado').
evento('20182019',15,'19/08/2018','Brighton',3,'Manchester United',2,'terminado').
evento('20182019',16,'18/08/2018','Tottenham',3,'Fulham',1,'terminado').
evento('20182019',17,'18/08/2018','Leicester City',2,'Wolves',0,'terminado').
evento('20182019',18,'18/08/2018','Chelsea',3,'Arsenal',2,'terminado').
evento('20182019',19,'19/08/2018','Burnley',1,'Watford',3,'terminado').
evento('20182019',20,'19/08/2018','Crystal Palace',0,'Liverpool',2,'terminado').
evento('20182019',21,'25/08/2018','Wolves',1,'Manchester City',1,'terminado').
evento('20182019',22,'25/08/2018','Arsenal',3,'West Ham',1,'terminado').
evento('20182019',23,'25/08/2018','Huddersfield Town',0,'Cardiff',0,'terminado').
evento('20182019',24,'26/08/2018','Watford',2,'Crystal Palace',1,'terminado').
evento('20182019',25,'26/08/2018','Newcastle',1,'Chelsea',2,'terminado').
evento('20182019',26,'25/08/2018','Southampton',1,'Leicester City',2,'terminado').
evento('20182019',27,'25/08/2018','Bournemouth',2,'Everton',2,'terminado').
evento('20182019',28,'25/08/2018','Liverpool',1,'Brighton',0,'terminado').
evento('20182019',29,'26/08/2018','Fulham',4,'Burnley',2,'terminado').
evento('20182019',30,'26/08/2018','Manchester United',0,'Tottenham',3,'terminado').
evento('20182019',31,'01/09/2018','Leicester City',1,'Liverpool',2,'terminado').
evento('20182019',32,'01/09/2018','Everton',1,'Huddersfield Town',1,'terminado').
evento('20182019',33,'01/09/2018','Chelsea',2,'Bournemouth',0,'terminado').
evento('20182019',34,'01/09/2018','Manchester City',2,'Newcastle',1,'terminado').
evento('20182019',35,'01/09/2018','Watford',2,'Tottenham',1,'terminado').
evento('20182019',36,'01/09/2018','Crystal Palace',0,'Southampton',2,'terminado').
evento('20182019',37,'01/09/2018','Brighton',2,'Fulham',2,'terminado').
evento('20182019',38,'01/09/2018','West Ham',0,'Wolves',1,'terminado').
evento('20182019',39,'01/09/2018','Cardiff',2,'Arsenal',3,'terminado').
evento('20182019',40,'01/09/2018','Burnley',0,'Manchester United',2,'terminado').
evento('20182019',41,'15/09/2018','Tottenham',1,'Liverpool',2,'terminado').
evento('20182019',42,'15/09/2018','Bournemouth',4,'Leicester City',2,'terminado').
evento('20182019',43,'15/09/2018','Huddersfield Town',0,'Crystal Palace',1,'terminado').
evento('20182019',44,'15/09/2018','Watford',1,'Manchester United',2,'terminado').
evento('20182019',45,'15/09/2018','Everton',1,'West Ham',3,'terminado').
evento('20182019',46,'15/09/2018','Manchester City',3,'Fulham',0,'terminado').
evento('20182019',47,'15/09/2018','Newcastle',1,'Arsenal',2,'terminado').
evento('20182019',48,'15/09/2018','Chelsea',4,'Cardiff',1,'terminado').
evento('20182019',49,'15/09/2018','Wolves',1,'Burnley',0,'terminado').
evento('20182019',50,'15/09/2018','Southampton',2,'Brighton',2,'terminado').
evento('20182019',51,'22/09/2018','Fulham',1,'Watford',1,'terminado').
evento('20182019',52,'22/09/2018','Crystal Palace',0,'Newcastle',0,'terminado').
evento('20182019',53,'22/09/2018','Cardiff',0,'Manchester City',5,'terminado').
evento('20182019',54,'22/09/2018','Burnley',4,'Bournemouth',0,'terminado').
evento('20182019',55,'22/09/2018','West Ham',0,'Chelsea',0,'terminado').
evento('20182019',56,'22/09/2018','Manchester United',1,'Wolves',1,'terminado').
evento('20182019',57,'22/09/2018','Leicester City',3,'Huddersfield Town',1,'terminado').
evento('20182019',58,'22/09/2018','Liverpool',3,'Southampton',0,'terminado').
evento('20182019',59,'22/09/2018','Brighton',1,'Tottenham',2,'terminado').
evento('20182019',60,'22/09/2018','Arsenal',2,'Everton',0,'terminado').
evento('20182019',61,'29/09/2018','West Ham',3,'Manchester United',1,'terminado').
evento('20182019',62,'29/09/2018','Arsenal',2,'Watford',0,'terminado').
evento('20182019',63,'29/09/2018','Newcastle',0,'Leicester City',2,'terminado').
evento('20182019',64,'29/09/2018','Everton',3,'Fulham',0,'terminado').
evento('20182019',65,'29/09/2018','Cardiff',1,'Burnley',2,'terminado').
evento('20182019',66,'29/09/2018','Huddersfield Town',0,'Tottenham',2,'terminado').
evento('20182019',67,'29/09/2018','Wolves',2,'Southampton',0,'terminado').
evento('20182019',68,'29/09/2018','Manchester City',2,'Brighton',0,'terminado').
evento('20182019',69,'29/09/2018','Chelsea',1,'Liverpool',1,'terminado').
evento('20182019',70,'29/09/2018','Bournemouth',2,'Crystal Palace',1,'terminado').
evento('20182019',71,'05/10/2018','Brighton',1,'West Ham',0,'terminado').
evento('20182019',72,'05/10/2018','Crystal Palace',0,'Wolves',1,'terminado').
evento('20182019',73,'05/10/2018','Leicester City',1,'Everton',2,'terminado').
evento('20182019',74,'05/10/2018','Manchester United',3,'Newcastle',2,'terminado').
evento('20182019',75,'05/10/2018','Southampton',0,'Chelsea',3,'terminado').
evento('20182019',76,'05/10/2018','Burnley',1,'Huddersfield Town',1,'terminado').
evento('20182019',77,'05/10/2018','Watford',0,'Bournemouth',4,'terminado').
evento('20182019',78,'05/10/2018','Tottenham',1,'Cardiff',0,'terminado').
evento('20182019',79,'05/10/2018','Fulham',1,'Arsenal',5,'terminado').
evento('20182019',80,'05/10/2018','Liverpool',0,'Manchester City',0,'terminado').
evento('20182019',81,'20/10/2018','Chelsea',2,'Manchester United',2,'terminado').
evento('20182019',82,'20/10/2018','Newcastle',0,'Brighton',1,'terminado').
evento('20182019',83,'20/10/2018','Cardiff',4,'Fulham',2,'terminado').
evento('20182019',84,'20/10/2018','Manchester City',5,'Burnley',0,'terminado').
evento('20182019',85,'20/10/2018','Everton',2,'Crystal Palace',0,'terminado').
evento('20182019',86,'20/10/2018','West Ham',0,'Tottenham',1,'terminado').
evento('20182019',87,'20/10/2018','Bournemouth',0,'Southampton',0,'terminado').
evento('20182019',88,'20/10/2018','Wolves',0,'Watford',2,'terminado').
evento('20182019',89,'20/10/2018','Huddersfield Town',0,'Liverpool',1,'terminado').
evento('20182019',90,'20/10/2018','Arsenal',3,'Leicester City',1,'terminado').
evento('20182019',91,'27/10/2018','Brighton',1,'Wolves',0,'terminado').
evento('20182019',92,'27/10/2018','Liverpool',4,'Cardiff',1,'terminado').
evento('20182019',93,'27/10/2018','Watford',3,'Huddersfield Town',0,'terminado').
evento('20182019',94,'27/10/2018','Burnley',0,'Chelsea',4,'terminado').
evento('20182019',95,'27/10/2018','Manchester United',2,'Everton',1,'terminado').
evento('20182019',96,'27/10/2018','Southampton',0,'Newcastle',0,'terminado').
evento('20182019',97,'27/10/2018','Fulham',0,'Bournemouth',3,'terminado').
evento('20182019',98,'27/10/2018','Leicester City',1,'West Ham',1,'terminado').
evento('20182019',99,'27/10/2018','Crystal Palace',2,'Arsenal',2,'terminado').
evento('20182019',100,'27/10/2018','Tottenham',0,'Manchester City',1,'terminado').
evento('20182019',101,'03/11/2018','Bournemouth',1,'Manchester United',2,'terminado').
evento('20182019',102,'03/11/2018','Everton',3,'Brighton',1,'terminado').
evento('20182019',103,'03/11/2018','Cardiff',0,'Leicester City',1,'terminado').
evento('20182019',104,'03/11/2018','Wolves',2,'Tottenham',3,'terminado').
evento('20182019',105,'03/11/2018','Chelsea',3,'Crystal Palace',1,'terminado').
evento('20182019',106,'03/11/2018','Newcastle',1,'Watford',0,'terminado').
evento('20182019',107,'03/11/2018','West Ham',4,'Burnley',2,'terminado').
evento('20182019',108,'03/11/2018','Arsenal',1,'Liverpool',1,'terminado').
evento('20182019',109,'03/11/2018','Manchester City',6,'Southampton',1,'terminado').
evento('20182019',110,'03/11/2018','Huddersfield Town',1,'Fulham',0,'terminado').
evento('20182019',111,'10/11/2018','Cardiff',2,'Brighton',1,'terminado').
evento('20182019',112,'10/11/2018','Leicester City',0,'Burnley',0,'terminado').
evento('20182019',113,'10/11/2018','Newcastle',2,'Bournemouth',1,'terminado').
evento('20182019',114,'10/11/2018','Liverpool',2,'Fulham',0,'terminado').
evento('20182019',115,'10/11/2018','Chelsea',0,'Everton',0,'terminado').
evento('20182019',116,'10/11/2018','Arsenal',1,'Wolves',1,'terminado').
evento('20182019',117,'10/11/2018','Southampton',1,'Watford',1,'terminado').
evento('20182019',118,'10/11/2018','Huddersfield Town',1,'West Ham',1,'terminado').
evento('20182019',119,'10/11/2018','Crystal Palace',0,'Tottenham',1,'terminado').
evento('20182019',120,'10/11/2018','Manchester City',3,'Manchester United',1,'terminado').
evento('20182019',121,'24/11/2018','West Ham',0,'Manchester City',4,'terminado').
evento('20182019',122,'24/11/2018','Brighton',1,'Leicester City',1,'terminado').
evento('20182019',123,'24/11/2018','Watford',0,'Liverpool',3,'terminado').
evento('20182019',124,'24/11/2018','Tottenham',3,'Chelsea',1,'terminado').
evento('20182019',125,'24/11/2018','Wolves',0,'Huddersfield Town',2,'terminado').
evento('20182019',126,'24/11/2018','Everton',1,'Cardiff',0,'terminado').
evento('20182019',127,'24/11/2018','Manchester United',0,'Crystal Palace',0,'terminado').
evento('20182019',128,'24/11/2018','Fulham',3,'Southampton',2,'terminado').
evento('20182019',129,'24/11/2018','Bournemouth',1,'Arsenal',2,'terminado').
evento('20182019',130,'24/11/2018','Burnley',1,'Newcastle',2,'terminado').
evento('20182019',131,'30/11/2018','Cardiff',2,'Wolves',1,'terminado').
evento('20182019',132,'30/11/2018','Leicester City',2,'Watford',0,'terminado').
evento('20182019',133,'30/11/2018','Newcastle',0,'West Ham',3,'terminado').
evento('20182019',134,'30/11/2018','Southampton',2,'Manchester United',2,'terminado').
evento('20182019',135,'30/11/2018','Arsenal',4,'Tottenham',2,'terminado').
evento('20182019',136,'30/11/2018','Manchester City',3,'Bournemouth',1,'terminado').
evento('20182019',137,'30/11/2018','Crystal Palace',2,'Burnley',0,'terminado').
evento('20182019',138,'30/11/2018','Huddersfield Town',1,'Brighton',2,'terminado').
evento('20182019',139,'30/11/2018','Chelsea',2,'Fulham',0,'terminado').
evento('20182019',140,'30/11/2018','Liverpool',1,'Everton',0,'terminado').
evento('20182019',141,'04/12/2018','Bournemouth',2,'Huddersfield Town',1,'terminado').
evento('20182019',142,'04/12/2018','Brighton',3,'Crystal Palace',1,'terminado').
evento('20182019',143,'04/12/2018','Fulham',1,'Leicester City',1,'terminado').
evento('20182019',144,'04/12/2018','Burnley',1,'Liverpool',3,'terminado').
evento('20182019',145,'04/12/2018','Manchester United',2,'Arsenal',2,'terminado').
evento('20182019',146,'04/12/2018','West Ham',3,'Cardiff',1,'terminado').
evento('20182019',147,'04/12/2018','Watford',1,'Manchester City',2,'terminado').
evento('20182019',148,'04/12/2018','Wolves',2,'Chelsea',1,'terminado').
evento('20182019',149,'04/12/2018','Everton',1,'Newcastle',1,'terminado').
evento('20182019',150,'04/12/2018','Tottenham',3,'Southampton',1,'terminado').
evento('20182019',151,'08/12/2018','Bournemouth',0,'Liverpool',4,'terminado').
evento('20182019',152,'08/12/2018','Manchester United',4,'Fulham',1,'terminado').
evento('20182019',153,'08/12/2018','Cardiff',1,'Southampton',0,'terminado').
evento('20182019',154,'08/12/2018','Chelsea',2,'Manchester City',0,'terminado').
evento('20182019',155,'08/12/2018','Newcastle',1,'Wolves',2,'terminado').
evento('20182019',156,'08/12/2018','Burnley',1,'Brighton',0,'terminado').
evento('20182019',157,'08/12/2018','Arsenal',1,'Huddersfield Town',0,'terminado').
evento('20182019',158,'08/12/2018','West Ham',3,'Crystal Palace',2,'terminado').
evento('20182019',159,'08/12/2018','Leicester City',0,'Tottenham',2,'terminado').
evento('20182019',160,'08/12/2018','Everton',2,'Watford',2,'terminado').
evento('20182019',161,'15/12/2018','Manchester City',3,'Everton',1,'terminado').
evento('20182019',162,'15/12/2018','Wolves',2,'Bournemouth',0,'terminado').
evento('20182019',163,'15/12/2018','Tottenham',1,'Burnley',0,'terminado').
evento('20182019',164,'15/12/2018','Fulham',0,'West Ham',2,'terminado').
evento('20182019',165,'15/12/2018','Brighton',1,'Chelsea',2,'terminado').
evento('20182019',166,'15/12/2018','Crystal Palace',1,'Leicester City',0,'terminado').
evento('20182019',167,'15/12/2018','Huddersfield Town',0,'Newcastle',1,'terminado').
evento('20182019',168,'15/12/2018','Watford',3,'Cardiff',2,'terminado').
evento('20182019',169,'15/12/2018','Southampton',3,'Arsenal',2,'terminado').
evento('20182019',170,'15/12/2018','Liverpool',3,'Manchester United',1,'terminado').
evento('20182019',171,'21/12/2018','Wolves',0,'Liverpool',2,'terminado').
evento('20182019',172,'21/12/2018','Huddersfield Town',1,'Southampton',3,'terminado').
evento('20182019',173,'21/12/2018','Manchester City',2,'Crystal Palace',3,'terminado').
evento('20182019',174,'21/12/2018','Chelsea',0,'Leicester City',1,'terminado').
evento('20182019',175,'21/12/2018','Cardiff',1,'Manchester United',5,'terminado').
evento('20182019',176,'21/12/2018','Arsenal',3,'Burnley',1,'terminado').
evento('20182019',177,'21/12/2018','Bournemouth',2,'Brighton',0,'terminado').
evento('20182019',178,'21/12/2018','Newcastle',0,'Fulham',0,'terminado').
evento('20182019',179,'21/12/2018','West Ham',0,'Watford',2,'terminado').
evento('20182019',180,'21/12/2018','Everton',2,'Tottenham',6,'terminado').
evento('20182019',181,'26/12/2018','Fulham',1,'Wolves',1,'terminado').
evento('20182019',182,'26/12/2018','Liverpool',4,'Newcastle',0,'terminado').
evento('20182019',183,'26/12/2018','Leicester City',2,'Manchester City',1,'terminado').
evento('20182019',184,'26/12/2018','Manchester United',3,'Huddersfield Town',1,'terminado').
evento('20182019',185,'26/12/2018','Watford',1,'Chelsea',2,'terminado').
evento('20182019',186,'26/12/2018','Burnley',1,'Everton',5,'terminado').
evento('20182019',187,'26/12/2018','Crystal Palace',0,'Cardiff',0,'terminado').
evento('20182019',188,'26/12/2018','Tottenham',5,'Bournemouth',0,'terminado').
evento('20182019',189,'26/12/2018','Brighton',1,'Arsenal',1,'terminado').
evento('20182019',190,'26/12/2018','Southampton',1,'West Ham',2,'terminado').
evento('20182019',191,'29/12/2018','Watford',1,'Newcastle',1,'terminado').
evento('20182019',192,'29/12/2018','Brighton',1,'Everton',0,'terminado').
evento('20182019',193,'29/12/2018','Fulham',1,'Huddersfield Town',0,'terminado').
evento('20182019',194,'29/12/2018','Liverpool',5,'Arsenal',1,'terminado').
evento('20182019',195,'29/12/2018','Southampton',1,'Manchester City',3,'terminado').
evento('20182019',196,'29/12/2018','Manchester United',4,'Bournemouth',1,'terminado').
evento('20182019',197,'29/12/2018','Tottenham',1,'Wolves',3,'terminado').
evento('20182019',198,'29/12/2018','Leicester City',0,'Cardiff',1,'terminado').
evento('20182019',199,'29/12/2018','Crystal Palace',0,'Chelsea',1,'terminado').
evento('20182019',200,'29/12/2018','Burnley',2,'West Ham',0,'terminado').
evento('20182019',201,'01/01/2019','Everton',0,'Leicester City',1,'terminado').
evento('20182019',202,'01/01/2019','Cardiff',0,'Tottenham',3,'terminado').
evento('20182019',203,'01/01/2019','Chelsea',0,'Southampton',0,'terminado').
evento('20182019',204,'01/01/2019','Huddersfield Town',1,'Burnley',2,'terminado').
evento('20182019',205,'01/01/2019','Bournemouth',3,'Watford',3,'terminado').
evento('20182019',206,'01/01/2019','Arsenal',4,'Fulham',1,'terminado').
evento('20182019',207,'01/01/2019','Wolves',0,'Crystal Palace',2,'terminado').
evento('20182019',208,'01/01/2019','West Ham',2,'Brighton',2,'terminado').
evento('20182019',209,'01/01/2019','Newcastle',0,'Manchester United',2,'terminado').
evento('20182019',210,'01/01/2019','Manchester City',2,'Liverpool',1,'terminado').
evento('20182019',211,'12/01/2019','West Ham',1,'Arsenal',0,'terminado').
evento('20182019',212,'12/01/2019','Crystal Palace',1,'Watford',2,'terminado').
evento('20182019',213,'12/01/2019','Brighton',0,'Liverpool',1,'terminado').
evento('20182019',214,'12/01/2019','Chelsea',2,'Newcastle',1,'terminado').
evento('20182019',215,'12/01/2019','Tottenham',0,'Manchester United',1,'terminado').
evento('20182019',216,'12/01/2019','Burnley',2,'Fulham',1,'terminado').
evento('20182019',217,'12/01/2019','Cardiff',0,'Huddersfield Town',0,'terminado').
evento('20182019',218,'12/01/2019','Leicester City',1,'Southampton',2,'terminado').
evento('20182019',219,'12/01/2019','Everton',2,'Bournemouth',0,'terminado').
evento('20182019',220,'12/01/2019','Manchester City',3,'Wolves',0,'terminado').
evento('20182019',221,'19/01/2019','Wolves',4,'Leicester City',3,'terminado').
evento('20182019',222,'19/01/2019','Watford',0,'Burnley',0,'terminado').
evento('20182019',223,'19/01/2019','Manchester United',2,'Brighton',1,'terminado').
evento('20182019',224,'19/01/2019','Southampton',2,'Everton',1,'terminado').
evento('20182019',225,'19/01/2019','Huddersfield Town',0,'Manchester City',3,'terminado').
evento('20182019',226,'19/01/2019','Liverpool',4,'Crystal Palace',3,'terminado').
evento('20182019',227,'19/01/2019','Newcastle',3,'Cardiff',0,'terminado').
evento('20182019',228,'19/01/2019','Bournemouth',2,'West Ham',0,'terminado').
evento('20182019',229,'19/01/2019','Arsenal',2,'Chelsea',0,'terminado').
evento('20182019',230,'19/01/2019','Fulham',1,'Tottenham',2,'terminado').
evento('20182019',231,'29/01/2019','Arsenal',2,'Cardiff',1,'terminado').
evento('20182019',232,'29/01/2019','Wolves',3,'West Ham',0,'terminado').
evento('20182019',233,'29/01/2019','Newcastle',2,'Manchester City',1,'terminado').
evento('20182019',234,'29/01/2019','Southampton',1,'Crystal Palace',1,'terminado').
evento('20182019',235,'29/01/2019','Liverpool',1,'Leicester City',1,'terminado').
evento('20182019',236,'29/01/2019','Fulham',4,'Brighton',2,'terminado').
evento('20182019',237,'29/01/2019','Huddersfield Town',0,'Everton',1,'terminado').
evento('20182019',238,'29/01/2019','Manchester United',2,'Burnley',2,'terminado').
evento('20182019',239,'29/01/2019','Bournemouth',4,'Chelsea',0,'terminado').
evento('20182019',240,'29/01/2019','Tottenham',2,'Watford',1,'terminado').
evento('20182019',241,'02/02/2019','Tottenham',1,'Newcastle',0,'terminado').
evento('20182019',242,'02/02/2019','Chelsea',5,'Huddersfield Town',0,'terminado').
evento('20182019',243,'02/02/2019','Burnley',1,'Southampton',1,'terminado').
evento('20182019',244,'02/02/2019','Cardiff',2,'Bournemouth',0,'terminado').
evento('20182019',245,'02/02/2019','Manchester City',3,'Arsenal',1,'terminado').
evento('20182019',246,'02/02/2019','Everton',1,'Wolves',3,'terminado').
evento('20182019',247,'02/02/2019','Brighton',0,'Watford',0,'terminado').
evento('20182019',248,'02/02/2019','Crystal Palace',2,'Fulham',0,'terminado').
evento('20182019',249,'02/02/2019','Leicester City',0,'manchester united',1,'terminado').
evento('20182019',250,'02/02/2019','West Ham',1,'Liverpool',1,'terminado').
evento('20182019',251,'09/02/2019','Fulham',0,'Manchester United',3,'terminado').
evento('20182019',252,'09/02/2019','Huddersfield Town',1,'Arsenal',2,'terminado').
evento('20182019',253,'09/02/2019','Crystal Palace',1,'West Ham',1,'terminado').
evento('20182019',254,'09/02/2019','Brighton',1,'Burnley',3,'terminado').
evento('20182019',255,'09/02/2019','Manchester City',6,'Chelsea',0,'terminado').
evento('20182019',256,'09/02/2019','Watford',1,'Everton',0,'terminado').
evento('20182019',257,'09/02/2019','Southampton',1,'Cardiff',2,'terminado').
evento('20182019',258,'09/02/2019','Liverpool',3,'Bournemouth',0,'terminado').
evento('20182019',259,'09/02/2019','Tottenham',3,'Leicester City',1,'terminado').
evento('20182019',260,'09/02/2019','Wolves',1,'Newcastle',1,'terminado').
evento('20182019',261,'06/02/2019','Everton',0,'Manchester City',2,'terminado').
evento('20182019',262,'22/02/2019','West Ham',3,'Fulham',1,'terminado').
evento('20182019',263,'22/02/2019','Burnley',2,'Tottenham',1,'terminado').
evento('20182019',264,'22/02/2019','Newcastle',2,'Huddersfield Town',0,'terminado').
evento('20182019',265,'22/02/2019','Arsenal',2,'Southampton',0,'terminado').
evento('20182019',266,'22/02/2019','Cardiff',1,'Watford',5,'terminado').
evento('20182019',267,'22/02/2019','Bournemouth',1,'Wolves',1,'terminado').
evento('20182019',268,'22/02/2019','Leicester City',1,'Crystal Palace',4,'terminado').
evento('20182019',269,'22/02/2019','manchester united',0,'Liverpool',0,'terminado').
evento('20182019',270,'26/02/2019','Huddersfield Town',1,'Wolves',0,'terminado').
evento('20182019',271,'26/02/2019','Leicester City',2,'Brighton',1,'terminado').
evento('20182019',272,'26/02/2019','Southampton',2,'Fulham',0,'terminado').
evento('20182019',273,'26/02/2019','Crystal Palace',1,'manchester united',3,'terminado').
evento('20182019',274,'26/02/2019','Manchester City',1,'West Ham',0,'terminado').
evento('20182019',275,'26/02/2019','Cardiff',0,'Everton',3,'terminado').
evento('20182019',276,'26/02/2019','Newcastle',2,'Burnley',0,'terminado').
evento('20182019',277,'26/02/2019','Arsenal',5,'Bournemouth',1,'terminado').
evento('20182019',278,'26/02/2019','Liverpool',5,'Watford',0,'terminado').
evento('20182019',279,'26/02/2019','Chelsea',2,'Tottenham',0,'terminado').
evento('20182019',280,'02/03/2019','Tottenham',1,'Arsenal',1,'terminado').
evento('20182019',281,'02/03/2019','Brighton',1,'Huddersfield Town',0,'terminado').
evento('20182019',282,'02/03/2019','manchester united',3,'Southampton',2,'terminado').
evento('20182019',283,'02/03/2019','West Ham',2,'Newcastle',0,'terminado').
evento('20182019',284,'02/03/2019','Fulham',1,'Chelsea',2,'terminado').
evento('20182019',285,'02/03/2019','Burnley',1,'Crystal Palace',3,'terminado').
evento('20182019',286,'02/03/2019','Bournemouth',0,'Manchester City',1,'terminado').
evento('20182019',287,'02/03/2019','Wolves',2,'Cardiff',0,'terminado').
evento('20182019',288,'02/03/2019','Watford',2,'Leicester City',1,'terminado').
evento('20182019',289,'02/03/2019','Everton',0,'Liverpool',0,'terminado').
evento('20182019',290,'09/03/2019','Crystal Palace',1,'Brighton',2,'terminado').
evento('20182019',291,'09/03/2019','Southampton',2,'Tottenham',1,'terminado').
evento('20182019',292,'09/03/2019','Cardiff',2,'West Ham',0,'terminado').
evento('20182019',293,'09/03/2019','Manchester City',3,'Watford',1,'terminado').
evento('20182019',294,'09/03/2019','Chelsea',1,'Wolves',1,'terminado').
evento('20182019',295,'09/03/2019','Huddersfield Town',0,'Bournemouth',2,'terminado').
evento('20182019',296,'09/03/2019','Leicester City',3,'Fulham',1,'terminado').
evento('20182019',297,'09/03/2019','Newcastle',3,'Everton',2,'terminado').
evento('20182019',298,'09/03/2019','Liverpool',4,'Burnley',2,'terminado').
evento('20182019',299,'09/03/2019','Arsenal',2,'manchester united',0,'terminado').
evento('20182019',300,'16/03/2019','West Ham',4,'Huddersfield Town',3,'terminado').
evento('20182019',301,'16/03/2019','Bournemouth',2,'Newcastle',2,'terminado').
evento('20182019',302,'16/03/2019','Everton',2,'Chelsea',0,'terminado').
evento('20182019',303,'16/03/2019','Burnley',1,'Leicester City',2,'terminado').
evento('20182019',304,'16/03/2019','Fulham',1,'Liverpool',2,'terminado').
evento('20182019',305,'30/03/2019','Fulham',0,'Manchester City',2,'terminado').
evento('20182019',306,'30/03/2019','Leicester City',2,'Bournemouth',0,'terminado').
evento('20182019',307,'30/03/2019','manchester united',2,'Watford',1,'terminado').
evento('20182019',308,'30/03/2019','Brighton',0,'Southampton',1,'terminado').
evento('20182019',309,'30/03/2019','Cardiff',1,'Chelsea',2,'terminado').
evento('20182019',310,'30/03/2019','Arsenal',2,'Newcastle',0,'terminado').
evento('20182019',311,'30/03/2019','Crystal Palace',2,'Huddersfield Town',0,'terminado').
evento('20182019',312,'30/03/2019','Burnley',2,'Wolves',0,'terminado').
evento('20182019',313,'30/03/2019','West Ham',0,'Everton',2,'terminado').
evento('20182019',314,'30/03/2019','Liverpool',2,'Tottenham',1,'terminado').
evento('20182019',315,'02/04/2019','Wolves',2,'manchester united',1,'terminado').
evento('20182019',316,'02/04/2019','Chelsea',3,'Brighton',0,'terminado').
evento('20182019',317,'02/04/2019','Tottenham',2,'Crystal Palace',0,'terminado').
evento('20182019',318,'02/04/2019','Watford',4,'Fulham',1,'terminado').
evento('20182019',319,'03/04/2019','Manchester City',2,'Cardiff',0,'terminado').
evento('20182019',320,'03/04/2019','Huddersfield Town',1,'Leicester City',4,'terminado').
evento('20182019',321,'03/04/2019','Bournemouth',1,'Burnley',3,'terminado').
evento('20182019',322,'03/04/2019','Chelsea',2,'West Ham',0,'terminado').
evento('20182019',323,'03/04/2019','Southampton',1,'Liverpool',3,'terminado').
evento('20182019',324,'03/04/2019','Newcastle',0,'Crystal Palace',1,'terminado').
evento('20182019',325,'03/04/2019','Everton',1,'Arsenal',0,'terminado').
evento('20182019',326,'12/04/2019','Leicester City',0,'Newcastle',1,'terminado').
evento('20182019',327,'12/04/2019','Brighton',0,'Bournemouth',5,'terminado').
evento('20182019',328,'12/04/2019','Fulham',2,'Everton',0,'terminado').
evento('20182019',329,'12/04/2019','manchester united',2,'West Ham',1,'terminado').
evento('20182019',330,'12/04/2019','Liverpool',2,'Chelsea',0,'terminado').
evento('20182019',331,'12/04/2019','Tottenham',4,'Huddersfield Town',0,'terminado').
evento('20182019',332,'12/04/2019','Southampton',3,'Wolves',1,'terminado').
evento('20182019',333,'12/04/2019','Burnley',2,'Cardiff',0,'terminado').
evento('20182019',334,'12/04/2019','Crystal Palace',1,'Manchester City',3,'terminado').
evento('20182019',335,'12/04/2019','Watford',0,'Arsenal',1,'terminado').
evento('20182019',336,'16/04/2019','Brighton',0,'Cardiff',2,'terminado').
evento('20182019',337,'20/04/2019','Manchester City',1,'Tottenham',0,'terminado').
evento('20182019',338,'20/04/2019','West Ham',2,'Leicester City',2,'terminado').
evento('20182019',339,'20/04/2019','Wolves',0,'Brighton',0,'terminado').
evento('20182019',340,'20/04/2019','Everton',4,'manchester united',0,'terminado').
evento('20182019',341,'20/04/2019','Arsenal',2,'Crystal Palace',3,'terminado').
evento('20182019',342,'20/04/2019','Chelsea',2,'Burnley',2,'terminado').
evento('20182019',343,'20/04/2019','Huddersfield Town',1,'Watford',2,'terminado').
evento('20182019',344,'20/04/2019','Bournemouth',0,'Fulham',1,'terminado').
evento('20182019',345,'20/04/2019','Newcastle',3,'Southampton',1,'terminado').
evento('20182019',346,'20/04/2019','Cardiff',0,'Liverpool',2,'terminado').
evento('20182019',347,'23/04/2019','Watford',1,'Southampton',1,'terminado').
evento('20182019',348,'23/04/2019','Tottenham',1,'Brighton',0,'terminado').
evento('20182019',349,'23/04/2019','Wolves',3,'Arsenal',1,'terminado').
evento('20182019',350,'23/04/2019','manchester united',0,'Manchester City',2,'terminado').
evento('20182019',351,'26/04/2019','Liverpool',5,'Huddersfield Town',0,'terminado').
evento('20182019',352,'26/04/2019','Southampton',3,'Bournemouth',3,'terminado').
evento('20182019',353,'26/04/2019','Watford',1,'Wolves',2,'terminado').
evento('20182019',354,'26/04/2019','Brighton',1,'Newcastle',1,'terminado').
evento('20182019',355,'26/04/2019','Burnley',0,'Manchester City',1,'terminado').
evento('20182019',356,'26/04/2019','Tottenham',0,'West Ham',1,'terminado').
evento('20182019',357,'26/04/2019','Fulham',1,'Cardiff',0,'terminado').
evento('20182019',358,'26/04/2019','Crystal Palace',0,'Everton',0,'terminado').
evento('20182019',359,'26/04/2019','Leicester City',3,'Arsenal',0,'terminado').
evento('20182019',360,'26/04/2019','manchester united',1,'Chelsea',1,'terminado').
evento('20182019',361,'03/05/2019','Everton',2,'Burnley',0,'terminado').
evento('20182019',362,'03/05/2019','West Ham',3,'Southampton',0,'terminado').
evento('20182019',363,'03/05/2019','Cardiff',2,'Crystal Palace',3,'terminado').
evento('20182019',364,'03/05/2019','Chelsea',3,'Watford',0,'terminado').
evento('20182019',365,'03/05/2019','Arsenal',1,'Brighton',1,'terminado').
evento('20182019',366,'03/05/2019','Bournemouth',1,'Tottenham',0,'terminado').
evento('20182019',367,'03/05/2019','Wolves',1,'Fulham',0,'terminado').
evento('20182019',368,'03/05/2019','Newcastle',2,'Liverpool',3,'terminado').
evento('20182019',369,'03/05/2019','Huddersfield Town',1,'manchester united',1,'terminado').
evento('20182019',370,'03/05/2019','Manchester City',1,'Leicester City',0,'terminado').
evento('20182019',371,'12/05/2019','Tottenham',2,'Everton',2,'terminado').
evento('20182019',372,'12/05/2019','Watford',1,'West Ham',4,'terminado').
evento('20182019',373,'12/05/2019','Leicester City',0,'Chelsea',0,'terminado').
evento('20182019',374,'12/05/2019','Liverpool',2,'Wolves',0,'terminado').
evento('20182019',375,'12/05/2019','Brighton',1,'Manchester City',4,'terminado').
evento('20182019',376,'12/05/2019','manchester united',0,'Cardiff',2,'terminado').
evento('20182019',377,'12/05/2019','Southampton',1,'Huddersfield Town',1,'terminado').
evento('20182019',378,'12/05/2019','Fulham',0,'Newcastle',4,'terminado').
evento('20182019',379,'12/05/2019','Crystal Palace',5,'Bournemouth',3,'terminado').
evento('20182019',380,'12/05/2019','Burnley',1,'Arsenal',3,'terminado').
evento('20192020',381,'09/08/2019','Liverpool',4,'Norwich City',1,'terminado').
evento('20192020',382,'09/08/2019','Crystal Palace',0,'Everton',0,'terminado').
evento('20192020',383,'09/08/2019','Watford',0,'Brighton',3,'terminado').
evento('20192020',384,'09/08/2019','Tottenham',3,'Aston Villa',1,'terminado').
evento('20192020',385,'09/08/2019','Newcastle',0,'Arsenal',1,'terminado').
evento('20192020',386,'09/08/2019','West Ham',0,'Manchester City',5,'terminado').
evento('20192020',387,'09/08/2019','Burnley',3,'Southampton',0,'terminado').
evento('20192020',388,'09/08/2019','Bournemouth',1,'Sheffield United',1,'terminado').
evento('20192020',389,'09/08/2019','Leicester City',0,'Wolves',0,'terminado').
evento('20192020',390,'09/08/2019','manchester united',4,'Chelsea',0,'terminado').
evento('20192020',391,'17/08/2019','Arsenal',2,'Burnley',1,'terminado').
evento('20192020',392,'17/08/2019','Brighton',1,'West Ham',1,'terminado').
evento('20192020',393,'17/08/2019','Norwich City',3,'Newcastle',1,'terminado').
evento('20192020',394,'17/08/2019','Manchester City',2,'Tottenham',2,'terminado').
evento('20192020',395,'17/08/2019','Chelsea',1,'Leicester City',1,'terminado').
evento('20192020',396,'17/08/2019','Southampton',1,'Liverpool',2,'terminado').
evento('20192020',397,'17/08/2019','Everton',1,'Watford',0,'terminado').
evento('20192020',398,'17/08/2019','Aston Villa',1,'Bournemouth',2,'terminado').
evento('20192020',399,'17/08/2019','Sheffield United',1,'Crystal Palace',0,'terminado').
evento('20192020',400,'17/08/2019','Wolves',1,'manchester united',1,'terminado').
evento('20192020',401,'23/08/2019','Aston Villa',2,'Everton',0,'terminado').
evento('20192020',402,'23/08/2019','Brighton',0,'Southampton',2,'terminado').
evento('20192020',403,'23/08/2019','Watford',1,'West Ham',3,'terminado').
evento('20192020',404,'23/08/2019','Liverpool',3,'Arsenal',1,'terminado').
evento('20192020',405,'23/08/2019','Tottenham',0,'Newcastle',1,'terminado').
evento('20192020',406,'23/08/2019','Norwich City',2,'Chelsea',3,'terminado').
evento('20192020',407,'23/08/2019','manchester united',1,'Crystal Palace',2,'terminado').
evento('20192020',408,'23/08/2019','Sheffield United',1,'Leicester City',2,'terminado').
evento('20192020',409,'23/08/2019','Bournemouth',1,'Manchester City',3,'terminado').
evento('20192020',410,'23/08/2019','Wolves',1,'Burnley',1,'terminado').
evento('20192020',411,'31/08/2019','Southampton',1,'manchester united',1,'terminado').
evento('20192020',412,'31/08/2019','Manchester City',4,'Brighton',0,'terminado').
evento('20192020',413,'31/08/2019','Leicester City',3,'Bournemouth',1,'terminado').
evento('20192020',414,'31/08/2019','Crystal Palace',1,'Aston Villa',0,'terminado').
evento('20192020',415,'31/08/2019','Burnley',0,'Liverpool',3,'terminado').
evento('20192020',416,'31/08/2019','Arsenal',2,'Tottenham',2,'terminado').
evento('20192020',417,'31/08/2019','Newcastle',1,'Watford',1,'terminado').
evento('20192020',418,'31/08/2019','West Ham',2,'Norwich City',0,'terminado').
evento('20192020',419,'31/08/2019','Chelsea',2,'Sheffield United',2,'terminado').
evento('20192020',420,'31/08/2019','Everton',3,'Wolves',2,'terminado').
evento('20192020',421,'14/09/2019','Liverpool',3,'Newcastle',1,'terminado').
evento('20192020',422,'14/09/2019','Sheffield United',0,'Southampton',1,'terminado').
evento('20192020',423,'14/09/2019','Wolves',2,'Chelsea',5,'terminado').
evento('20192020',424,'14/09/2019','Norwich City',3,'Manchester City',2,'terminado').
evento('20192020',425,'14/09/2019','Watford',2,'Arsenal',2,'terminado').
evento('20192020',426,'14/09/2019','manchester united',1,'Leicester City',0,'terminado').
evento('20192020',427,'14/09/2019','Brighton',1,'Burnley',1,'terminado').
evento('20192020',428,'14/09/2019','Tottenham',4,'Crystal Palace',0,'terminado').
evento('20192020',429,'14/09/2019','Bournemouth',3,'Everton',1,'terminado').
evento('20192020',430,'14/09/2019','Aston Villa',0,'West Ham',0,'terminado').
evento('20192020',431,'20/09/2019','Southampton',1,'Bournemouth',3,'terminado').
evento('20192020',432,'20/09/2019','Burnley',2,'Norwich City',0,'terminado').
evento('20192020',433,'20/09/2019','Manchester City',8,'Watford',0,'terminado').
evento('20192020',434,'20/09/2019','West Ham',2,'manchester united',0,'terminado').
evento('20192020',435,'20/09/2019','Arsenal',3,'Aston Villa',2,'terminado').
evento('20192020',436,'20/09/2019','Leicester City',2,'Tottenham',1,'terminado').
evento('20192020',437,'20/09/2019','Everton',0,'Sheffield United',2,'terminado').
evento('20192020',438,'20/09/2019','Newcastle',0,'Brighton',0,'terminado').
evento('20192020',439,'20/09/2019','Crystal Palace',1,'Wolves',1,'terminado').
evento('20192020',440,'20/09/2019','Chelsea',1,'Liverpool',2,'terminado').
evento('20192020',441,'28/09/2019','Sheffield United',0,'Liverpool',1,'terminado').
evento('20192020',442,'28/09/2019','Aston Villa',2,'Burnley',2,'terminado').
evento('20192020',443,'28/09/2019','Wolves',2,'Watford',0,'terminado').
evento('20192020',444,'28/09/2019','Chelsea',2,'Brighton',0,'terminado').
evento('20192020',445,'28/09/2019','Leicester City',5,'Newcastle',0,'terminado').
evento('20192020',446,'28/09/2019','Crystal Palace',2,'Norwich City',0,'terminado').
evento('20192020',447,'28/09/2019','Bournemouth',2,'West Ham',2,'terminado').
evento('20192020',448,'28/09/2019','Tottenham',2,'Southampton',1,'terminado').
evento('20192020',449,'28/09/2019','Everton',1,'Manchester City',3,'terminado').
evento('20192020',450,'28/09/2019','manchester united',1,'Arsenal',1,'terminado').
evento('20192020',451,'05/10/2018','Brighton',3,'Tottenham',0,'terminado').
evento('20192020',452,'05/10/2018','Norwich City',1,'Aston Villa',5,'terminado').
evento('20192020',453,'05/10/2018','Burnley',1,'Everton',0,'terminado').
evento('20192020',454,'05/10/2018','Southampton',1,'Chelsea',4,'terminado').
evento('20192020',455,'05/10/2018','Arsenal',1,'Bournemouth',0,'terminado').
evento('20192020',456,'05/10/2018','Liverpool',2,'Leicester City',1,'terminado').
evento('20192020',457,'05/10/2018','Watford',0,'Sheffield United',0,'terminado').
evento('20192020',458,'05/10/2018','West Ham',1,'Crystal Palace',2,'terminado').
evento('20192020',459,'05/10/2018','Manchester City',0,'Wolves',2,'terminado').
evento('20192020',460,'05/10/2018','Newcastle',1,'manchester united',0,'terminado').
evento('20192020',461,'19/10/2019','Everton',2,'West Ham',0,'terminado').
evento('20192020',462,'19/10/2019','Tottenham',1,'Watford',1,'terminado').
evento('20192020',463,'19/10/2019','Chelsea',1,'Newcastle',0,'terminado').
evento('20192020',464,'19/10/2019','Leicester City',2,'Burnley',1,'terminado').
evento('20192020',465,'19/10/2019','manchester united',1,'Liverpool',1,'terminado').
evento('20192020',466,'19/10/2019','Aston Villa',2,'Brighton',1,'terminado').
evento('20192020',467,'19/10/2019','Wolves',1,'Southampton',1,'terminado').
evento('20192020',468,'19/10/2019','Bournemouth',0,'Norwich City',0,'terminado').
evento('20192020',469,'19/10/2019','Crystal Palace',0,'Manchester City',2,'terminado').
evento('20192020',470,'19/10/2019','Sheffield United',1,'Arsenal',0,'terminado').
evento('20192020',471,'25/10/2019','Southampton',0,'Leicester City',9,'terminado').
evento('20192020',472,'25/10/2019','Watford',0,'Bournemouth',0,'terminado').
evento('20192020',473,'25/10/2019','West Ham',1,'Sheffield United',1,'terminado').
evento('20192020',474,'25/10/2019','Newcastle',1,'Wolves',1,'terminado').
evento('20192020',475,'25/10/2019','Liverpool',2,'Tottenham',1,'terminado').
evento('20192020',476,'25/10/2019','Manchester City',3,'Aston Villa',0,'terminado').
evento('20192020',477,'25/10/2019','Brighton',3,'Everton',2,'terminado').
evento('20192020',478,'25/10/2019','Burnley',2,'Chelsea',4,'terminado').
evento('20192020',479,'25/10/2019','Arsenal',2,'Crystal Palace',2,'terminado').
evento('20192020',480,'25/10/2019','Norwich City',1,'manchester united',3,'terminado').
evento('20192020',481,'01/10/2019','Bournemouth',1,'manchester united',0,'terminado').
evento('20192020',482,'01/10/2019','Aston Villa',1,'Liverpool',2,'terminado').
evento('20192020',483,'01/10/2019','Sheffield United',3,'Burnley',0,'terminado').
evento('20192020',484,'01/10/2019','Manchester City',2,'Southampton',1,'terminado').
evento('20192020',485,'01/10/2019','Crystal Palace',0,'Leicester City',2,'terminado').
evento('20192020',486,'01/10/2019','West Ham',2,'Newcastle',3,'terminado').
evento('20192020',487,'01/10/2019','Arsenal',1,'Wolves',1,'terminado').
evento('20192020',488,'01/10/2019','Brighton',2,'Norwich City',0,'terminado').
evento('20192020',489,'01/10/2019','Watford',1,'Chelsea',2,'terminado').
evento('20192020',490,'01/10/2019','Everton',1,'Tottenham',1,'terminado').
evento('20192020',491,'08/11/2019','Norwich City',0,'Watford',2,'terminado').
evento('20192020',492,'08/11/2019','Chelsea',2,'Crystal Palace',0,'terminado').
evento('20192020',493,'08/11/2019','Newcastle',2,'Bournemouth',1,'terminado').
evento('20192020',494,'08/11/2019','Tottenham',1,'Sheffield United',1,'terminado').
evento('20192020',495,'08/11/2019','Wolves',2,'Aston Villa',1,'terminado').
evento('20192020',496,'08/11/2019','Liverpool',3,'Manchester City',1,'terminado').
evento('20192020',497,'08/11/2019','Burnley',3,'West Ham',0,'terminado').
evento('20192020',498,'08/11/2019','Southampton',1,'Everton',2,'terminado').
evento('20192020',499,'08/11/2019','Leicester City',2,'Arsenal',0,'terminado').
evento('20192020',500,'08/11/2019','manchester united',3,'Brighton',1,'terminado').
evento('20192020',501,'23/11/2019','West Ham',2,'Tottenham',3,'terminado').
evento('20192020',502,'23/11/2019','Brighton',0,'Leicester City',2,'terminado').
evento('20192020',503,'23/11/2019','Crystal Palace',1,'Liverpool',2,'terminado').
evento('20192020',504,'23/11/2019','Everton',0,'Norwich City',2,'terminado').
evento('20192020',505,'23/11/2019','Sheffield United',3,'manchester united',3,'terminado').
evento('20192020',506,'23/11/2019','Arsenal',2,'Southampton',2,'terminado').
evento('20192020',507,'23/11/2019','Watford',0,'Burnley',3,'terminado').
evento('20192020',508,'23/11/2019','Bournemouth',1,'Wolves',2,'terminado').
evento('20192020',509,'23/11/2019','Manchester City',2,'Chelsea',1,'terminado').
evento('20192020',510,'23/11/2019','Aston Villa',2,'Newcastle',0,'terminado').
evento('20192020',511,'30/11/2019','Newcastle',2,'Manchester City',2,'terminado').
evento('20192020',512,'30/11/2019','Burnley',0,'Crystal Palace',2,'terminado').
evento('20192020',513,'30/11/2019','Tottenham',3,'Bournemouth',2,'terminado').
evento('20192020',514,'30/11/2019','Norwich City',2,'Arsenal',2,'terminado').
evento('20192020',515,'30/11/2019','Leicester City',2,'Everton',1,'terminado').
evento('20192020',516,'30/11/2019','Liverpool',2,'Brighton',1,'terminado').
evento('20192020',517,'30/11/2019','Chelsea',0,'West Ham',1,'terminado').
evento('20192020',518,'30/11/2019','Southampton',2,'Watford',1,'terminado').
evento('20192020',519,'30/11/2019','Wolves',1,'Sheffield United',1,'terminado').
evento('20192020',520,'30/11/2019','manchester united',2,'Aston Villa',2,'terminado').
evento('20192020',521,'02/12/2019','Crystal Palace',1,'Bournemouth',0,'terminado').
evento('20192020',522,'02/12/2019','Chelsea',2,'Aston Villa',1,'terminado').
evento('20192020',523,'02/12/2019','Southampton',2,'Norwich City',1,'terminado').
evento('20192020',524,'02/12/2019','manchester united',2,'Tottenham',1,'terminado').
evento('20192020',525,'02/12/2019','Sheffield United',0,'Newcastle',2,'terminado').
evento('20192020',526,'02/12/2019','Burnley',1,'Manchester City',4,'terminado').
evento('20192020',527,'02/12/2019','Leicester City',2,'Watford',0,'terminado').
evento('20192020',528,'02/12/2019','Wolves',2,'West Ham',0,'terminado').
evento('20192020',529,'02/12/2019','Liverpool',5,'Everton',2,'terminado').
evento('20192020',530,'02/12/2019','Arsenal',1,'Brighton',2,'terminado').
evento('20192020',531,'07/12/2019','Everton',3,'Chelsea',1,'terminado').
evento('20192020',532,'07/12/2019','Tottenham',5,'Burnley',0,'terminado').
evento('20192020',533,'07/12/2019','Manchester City',1,'manchester united',2,'terminado').
evento('20192020',534,'07/12/2019','Newcastle',2,'Southampton',1,'terminado').
evento('20192020',535,'07/12/2019','Brighton',2,'Wolves',2,'terminado').
evento('20192020',536,'07/12/2019','Bournemouth',0,'Liverpool',3,'terminado').
evento('20192020',537,'07/12/2019','Watford',0,'Crystal Palace',0,'terminado').
evento('20192020',538,'07/12/2019','Aston Villa',1,'Leicester City',4,'terminado').
evento('20192020',539,'07/12/2019','Norwich City',1,'Sheffield United',2,'terminado').
evento('20192020',540,'07/12/2019','West Ham',1,'Arsenal',3,'terminado').
evento('20192020',541,'14/12/2019','Liverpool',2,'Watford',0,'terminado').
evento('20192020',542,'14/12/2019','Sheffield United',2,'Aston Villa',0,'terminado').
evento('20192020',543,'14/12/2019','Burnley',1,'Newcastle',0,'terminado').
evento('20192020',544,'14/12/2019','manchester united',1,'Everton',1,'terminado').
evento('20192020',545,'14/12/2019','Arsenal',0,'Manchester City',3,'terminado').
evento('20192020',546,'14/12/2019','Chelsea',0,'Bournemouth',1,'terminado').
evento('20192020',547,'14/12/2019','Leicester City',1,'Norwich City',1,'terminado').
evento('20192020',548,'14/12/2019','Southampton',0,'West Ham',1,'terminado').
evento('20192020',549,'14/12/2019','Wolves',1,'Tottenham',2,'terminado').
evento('20192020',550,'14/12/2019','Crystal Palace',1,'Brighton',1,'terminado').
evento('20192020',551,'21/12/2019','Everton',0,'Arsenal',0,'terminado').
evento('20192020',552,'21/12/2019','Aston Villa',1,'Southampton',3,'terminado').
evento('20192020',553,'21/12/2019','Brighton',0,'Sheffield United',1,'terminado').
evento('20192020',554,'21/12/2019','manchester united',3,'Leicester City',1,'terminado').
evento('20192020',555,'21/12/2019','Tottenham',0,'Chelsea',2,'terminado').
evento('20192020',556,'21/12/2019','Newcastle',1,'Crystal Palace',0,'terminado').
evento('20192020',557,'21/12/2019','Norwich City',1,'Wolves',2,'terminado').
evento('20192020',558,'21/12/2019','Bournemouth',0,'Burnley',1,'terminado').
evento('20192020',559,'21/12/2019','Watford',2,'manchester united',0,'terminado').
evento('20192020',560,'26/12/2019','Tottenham',2,'Brighton',1,'terminado').
evento('20192020',561,'26/12/2019','Sheffield United',1,'Watford',1,'terminado').
evento('20192020',562,'26/12/2019','Aston Villa',1,'Norwich City',0,'terminado').
evento('20192020',563,'26/12/2019','Everton',1,'Burnley',0,'terminado').
evento('20192020',564,'26/12/2019','Leicester City',0,'Liverpool',4,'terminado').
evento('20192020',565,'26/12/2019','Bournemouth',1,'Arsenal',1,'terminado').
evento('20192020',566,'26/12/2019','Chelsea',0,'Southampton',2,'terminado').
evento('20192020',567,'26/12/2019','Crystal Palace',2,'West Ham',1,'terminado').
evento('20192020',568,'26/12/2019','manchester united',4,'Newcastle',1,'terminado').
evento('20192020',569,'26/12/2019','Wolves',3,'Manchester City',2,'terminado').
evento('20192020',570,'28/12/2019','Brighton',2,'Bournemouth',0,'terminado').
evento('20192020',571,'28/12/2019','Newcastle',1,'Everton',2,'terminado').
evento('20192020',572,'28/12/2019','Norwich City',2,'Tottenham',2,'terminado').
evento('20192020',573,'28/12/2019','Burnley',0,'manchester united',2,'terminado').
evento('20192020',574,'28/12/2019','Liverpool',1,'Wolves',0,'terminado').
evento('20192020',575,'28/12/2019','Southampton',1,'Crystal Palace',1,'terminado').
evento('20192020',576,'28/12/2019','Watford',3,'Aston Villa',0,'terminado').
evento('20192020',577,'28/12/2019','West Ham',1,'Leicester City',2,'terminado').
evento('20192020',578,'28/12/2019','Arsenal',1,'Chelsea',2,'terminado').
evento('20192020',579,'28/12/2019','Manchester City',2,'Sheffield United',0,'terminado').
evento('20192020',580,'01/01/2020','Brighton',1,'Chelsea',1,'terminado').
evento('20192020',581,'01/01/2020','Newcastle',0,'Leicester City',3,'terminado').
evento('20192020',582,'01/01/2020','Southampton',1,'Tottenham',0,'terminado').
evento('20192020',583,'01/01/2020','Manchester City',2,'Everton',1,'terminado').
evento('20192020',584,'01/01/2020','Arsenal',2,'manchester united',0,'terminado').
evento('20192020',585,'01/01/2020','Burnley',1,'Aston Villa',2,'terminado').
evento('20192020',586,'01/01/2020','Watford',2,'Wolves',1,'terminado').
evento('20192020',587,'01/01/2020','West Ham',4,'Bournemouth',0,'terminado').
evento('20192020',588,'01/01/2020','Norwich City',1,'Crystal Palace',1,'terminado').
evento('20192020',589,'01/01/2020','Liverpool',2,'Sheffield United',0,'terminado').
evento('20192020',590,'10/01/2020','Sheffield United',1,'West Ham',0,'terminado').
evento('20192020',591,'10/01/2020','Everton',1,'Brighton',0,'terminado').
evento('20192020',592,'10/01/2020','manchester united',4,'Norwich City',0,'terminado').
evento('20192020',593,'10/01/2020','Wolves',1,'Newcastle',1,'terminado').
evento('20192020',594,'10/01/2020','Bournemouth',0,'Watford',3,'terminado').
evento('20192020',595,'10/01/2020','Crystal Palace',1,'Arsenal',1,'terminado').
evento('20192020',596,'10/01/2020','Leicester City',1,'Southampton',2,'terminado').
evento('20192020',597,'10/01/2020','Chelsea',3,'Burnley',0,'terminado').
evento('20192020',598,'10/01/2020','Tottenham',0,'Liverpool',1,'terminado').
evento('20192020',599,'10/01/2020','Aston Villa',1,'Manchester City',6,'terminado').
evento('20192020',600,'18/01/2020','Watford',0,'Tottenham',0,'terminado').
evento('20192020',601,'18/01/2020','Manchester City',2,'Crystal Palace',2,'terminado').
evento('20192020',602,'18/01/2020','Arsenal',1,'Sheffield United',1,'terminado').
evento('20192020',603,'18/01/2020','Southampton',2,'Wolves',3,'terminado').
evento('20192020',604,'18/01/2020','Burnley',2,'Leicester City',1,'terminado').
evento('20192020',605,'18/01/2020','Liverpool',2,'manchester united',0,'terminado').
evento('20192020',606,'18/01/2020','Brighton',1,'Aston Villa',1,'terminado').
evento('20192020',607,'18/01/2020','West Ham',1,'Everton',1,'terminado').
evento('20192020',608,'18/01/2020','Norwich City',1,'Bournemouth',0,'terminado').
evento('20192020',609,'18/01/2020','Newcastle',1,'Chelsea',0,'terminado').
evento('20192020',610,'21/01/2020','Bournemouth',3,'Brighton',1,'terminado').
evento('20192020',611,'21/01/2020','Crystal Palace',0,'Southampton',2,'terminado').
evento('20192020',612,'21/01/2020','Everton',2,'Newcastle',2,'terminado').
evento('20192020',613,'21/01/2020','Leicester City',4,'West Ham',1,'terminado').
evento('20192020',614,'21/01/2020','manchester united',0,'Burnley',2,'terminado').
evento('20192020',615,'21/01/2020','Aston Villa',2,'Watford',1,'terminado').
evento('20192020',616,'21/01/2020','Sheffield United',0,'Manchester City',1,'terminado').
evento('20192020',617,'21/01/2020','Chelsea',2,'Arsenal',2,'terminado').
evento('20192020',618,'21/01/2020','Tottenham',2,'Norwich City',1,'terminado').
evento('20192020',619,'21/01/2020','Wolves',1,'Liverpool',2,'terminado').
evento('20192020',620,'21/01/2020','West Ham',0,'Liverpool',2,'terminado').
evento('20192020',621,'01/02/2020','Leicester City',2,'Chelsea',2,'terminado').
evento('20192020',622,'01/02/2020','Crystal Palace',0,'Sheffield United',1,'terminado').
evento('20192020',623,'01/02/2020','Bournemouth',2,'Aston Villa',1,'terminado').
evento('20192020',624,'01/02/2020','Liverpool',4,'Southampton',0,'terminado').
evento('20192020',625,'01/02/2020','Burnley',0,'Arsenal',0,'terminado').
evento('20192020',626,'01/02/2020','Newcastle',0,'Norwich City',0,'terminado').
evento('20192020',627,'01/02/2020','West Ham',3,'Brighton',3,'terminado').
evento('20192020',628,'01/02/2020','Watford',2,'Everton',3,'terminado').
evento('20192020',629,'01/02/2020','manchester united',0,'Wolves',0,'terminado').
evento('20192020',630,'01/02/2020','Tottenham',2,'manchester united',0,'terminado').
evento('20192020',631,'08/02/2020','Everton',3,'Crystal Palace',1,'terminado').
evento('20192020',632,'08/02/2020','Sheffield United',2,'Bournemouth',1,'terminado').
evento('20192020',633,'08/02/2020','Southampton',1,'Burnley',2,'terminado').
evento('20192020',634,'08/02/2020','Aston Villa',2,'Tottenham',3,'terminado').
evento('20192020',635,'08/02/2020','Chelsea',0,'manchester united',2,'terminado').
evento('20192020',636,'08/02/2020','Brighton',1,'Watford',1,'terminado').
evento('20192020',637,'08/02/2020','Wolves',0,'Leicester City',0,'terminado').
evento('20192020',638,'08/02/2020','Norwich City',0,'Liverpool',1,'terminado').
evento('20192020',639,'08/02/2020','Arsenal',4,'Newcastle',0,'terminado').
evento('20192020',640,'08/02/2020','Manchester City',2,'West Ham',0,'terminado').
evento('20192020',641,'22/02/2020','Chelsea',2,'Tottenham',1,'terminado').
evento('20192020',642,'22/02/2020','Burnley',3,'Bournemouth',0,'terminado').
evento('20192020',643,'22/02/2020','Southampton',2,'Aston Villa',0,'terminado').
evento('20192020',644,'22/02/2020','Wolves',3,'Norwich City',0,'terminado').
evento('20192020',645,'22/02/2020','Arsenal',3,'Everton',2,'terminado').
evento('20192020',646,'22/02/2020','Crystal Palace',1,'Newcastle',0,'terminado').
evento('20192020',647,'22/02/2020','Sheffield United',1,'Brighton',1,'terminado').
evento('20192020',648,'22/02/2020','Leicester City',0,'Manchester City',1,'terminado').
evento('20192020',649,'22/02/2020','manchester united',3,'Watford',0,'terminado').
evento('20192020',650,'22/02/2020','Liverpool',3,'West Ham',2,'terminado').
evento('20192020',651,'28/02/2020','Norwich City',1,'Leicester City',0,'terminado').
evento('20192020',652,'28/02/2020','Bournemouth',2,'Chelsea',2,'terminado').
evento('20192020',653,'28/02/2020','West Ham',3,'Southampton',1,'terminado').
evento('20192020',654,'28/02/2020','Everton',1,'manchester united',1,'terminado').
evento('20192020',655,'28/02/2020','Brighton',0,'Crystal Palace',1,'terminado').
evento('20192020',656,'28/02/2020','Newcastle',0,'Burnley',0,'terminado').
evento('20192020',657,'28/02/2020','Watford',3,'Liverpool',0,'terminado').
evento('20192020',658,'28/02/2020','Tottenham',2,'Wolves',3,'terminado').
evento('20192020',659,'07/03/2020','Liverpool',2,'Bournemouth',1,'terminado').
evento('20192020',660,'07/03/2020','Arsenal',1,'West Ham',0,'terminado').
evento('20192020',661,'07/03/2020','Sheffield United',1,'Norwich City',0,'terminado').
evento('20192020',662,'07/03/2020','Burnley',1,'Tottenham',1,'terminado').
evento('20192020',663,'07/03/2020','manchester united',2,'Manchester City',0,'terminado').
evento('20192020',664,'07/03/2020','Wolves',0,'Brighton',0,'terminado').
evento('20192020',665,'07/03/2020','Southampton',0,'Newcastle',1,'terminado').
evento('20192020',666,'07/03/2020','Crystal Palace',1,'Watford',0,'terminado').
evento('20192020',667,'07/03/2020','Chelsea',4,'Everton',0,'terminado').
evento('20192020',668,'07/03/2020','Leicester City',4,'Aston Villa',0,'terminado').
evento('20192020',669,'17/06/2020','Aston Villa',0,'Sheffield United',0,'terminado').
evento('20192020',670,'17/06/2020','Manchester City',3,'Arsenal',0,'terminado').
evento('20192020',671,'19/06/2020','Norwich City',0,'Southampton',3,'terminado').
evento('20192020',672,'19/06/2020','Watford',1,'Leicester City',1,'terminado').
evento('20192020',673,'19/06/2020','West Ham',0,'Wolves',2,'terminado').
evento('20192020',674,'19/06/2020','Newcastle',3,'Sheffield United',0,'terminado').
evento('20192020',675,'19/06/2020','Everton',0,'Liverpool',0,'terminado').
evento('20192020',676,'19/06/2020','Tottenham',1,'manchester united',1,'terminado').
evento('20192020',677,'19/06/2020','Brighton',2,'Arsenal',1,'terminado').
evento('20192020',678,'19/06/2020','Bournemouth',0,'Crystal Palace',2,'terminado').
evento('20192020',679,'19/06/2020','Aston Villa',1,'Chelsea',2,'terminado').
evento('20192020',680,'19/06/2020','Manchester City',5,'Burnley',0,'terminado').
evento('20192020',681,'23/06/2020','Leicester City',0,'Brighton',0,'terminado').
evento('20192020',682,'23/06/2020','Norwich City',0,'Everton',1,'terminado').
evento('20192020',683,'23/06/2020','manchester united',3,'Sheffield United',0,'terminado').
evento('20192020',684,'23/06/2020','Liverpool',4,'Crystal Palace',0,'terminado').
evento('20192020',685,'23/06/2020','Burnley',1,'Watford',0,'terminado').
evento('20192020',686,'23/06/2020','Tottenham',2,'West Ham',0,'terminado').
evento('20192020',687,'23/06/2020','Newcastle',1,'Aston Villa',1,'terminado').
evento('20192020',688,'23/06/2020','Wolves',1,'Bournemouth',0,'terminado').
evento('20192020',689,'23/06/2020','Southampton',0,'Arsenal',2,'terminado').
evento('20192020',690,'23/06/2020','Chelsea',2,'Manchester City',1,'terminado').
evento('20192020',691,'27/06/2020','Aston Villa',0,'Wolves',1,'terminado').
evento('20192020',692,'27/06/2020','Crystal Palace',0,'Burnley',1,'terminado').
evento('20192020',693,'27/06/2020','Arsenal',4,'Norwich City',0,'terminado').
evento('20192020',694,'27/06/2020','Everton',2,'Leicester City',1,'terminado').
evento('20192020',695,'27/06/2020','Sheffield United',3,'Tottenham',1,'terminado').
evento('20192020',696,'27/06/2020','Watford',1,'Southampton',3,'terminado').
evento('20192020',697,'27/06/2020','Brighton',0,'manchester united',3,'terminado').
evento('20192020',698,'27/06/2020','Bournemouth',1,'Newcastle',4,'terminado').
evento('20192020',699,'27/06/2020','West Ham',3,'Chelsea',2,'terminado').
evento('20192020',700,'27/06/2020','Manchester City',4,'Liverpool',0,'terminado').
evento('20192020',701,'04/07/2020','Norwich City',0,'Brighton',1,'terminado').
evento('20192020',702,'04/07/2020','manchester united',5,'Bournemouth',2,'terminado').
evento('20192020',703,'04/07/2020','Chelsea',3,'Watford',0,'terminado').
evento('20192020',704,'04/07/2020','Newcastle',2,'West Ham',2,'terminado').
evento('20192020',705,'04/07/2020','Southampton',1,'Manchester City',0,'terminado').
evento('20192020',706,'04/07/2020','Leicester City',3,'Crystal Palace',0,'terminado').
evento('20192020',707,'04/07/2020','Wolves',0,'Arsenal',2,'terminado').
evento('20192020',708,'04/07/2020','Burnley',1,'Sheffield United',1,'terminado').
evento('20192020',709,'04/07/2020','Liverpool',2,'Aston Villa',0,'terminado').
evento('20192020',710,'04/07/2020','Tottenham',1,'Everton',0,'terminado').
evento('20192020',711,'07/07/2020','Watford',2,'Norwich City',1,'terminado').
evento('20192020',712,'07/07/2020','Arsenal',1,'Leicester City',1,'terminado').
evento('20192020',713,'07/07/2020','Manchester City',5,'Newcastle',0,'terminado').
evento('20192020',714,'07/07/2020','Brighton',1,'Liverpool',3,'terminado').
evento('20192020',715,'07/07/2020','Bournemouth',0,'Tottenham',0,'terminado').
evento('20192020',716,'07/07/2020','Crystal Palace',2,'Chelsea',3,'terminado').
evento('20192020',717,'07/07/2020','Sheffield United',1,'Wolves',0,'terminado').
evento('20192020',718,'07/07/2020','West Ham',0,'Burnley',1,'terminado').
evento('20192020',719,'07/07/2020','Everton',1,'Southampton',1,'terminado').
evento('20192020',720,'07/07/2020','Aston Villa',0,'manchester united',3,'terminado').
evento('20192020',721,'11/07/2020','Watford',2,'Newcastle',1,'terminado').
evento('20192020',722,'11/07/2020','Liverpool',1,'Burnley',1,'terminado').
evento('20192020',723,'11/07/2020','Brighton',0,'Manchester City',5,'terminado').
evento('20192020',724,'11/07/2020','Aston Villa',2,'Crystal Palace',0,'terminado').
evento('20192020',725,'11/07/2020','Bournemouth',4,'Leicester City',1,'terminado').
evento('20192020',726,'11/07/2020','Norwich City',0,'West Ham',4,'terminado').
evento('20192020',727,'11/07/2020','Sheffield United',3,'Chelsea',0,'terminado').
evento('20192020',728,'11/07/2020','Wolves',3,'Everton',0,'terminado').
evento('20192020',729,'11/07/2020','Tottenham',2,'Arsenal',1,'terminado').
evento('20192020',730,'11/07/2020','manchester united',2,'Southampton',2,'terminado').
evento('20192020',731,'14/07/2020','Chelsea',1,'Norwich City',0,'terminado').
evento('20192020',732,'14/07/2020','Manchester City',2,'Bournemouth',1,'terminado').
evento('20192020',733,'14/07/2020','Arsenal',2,'Liverpool',1,'terminado').
evento('20192020',734,'14/07/2020','Leicester City',2,'Sheffield United',0,'terminado').
evento('20192020',735,'14/07/2020','Crystal Palace',0,'manchester united',2,'terminado').
evento('20192020',736,'14/07/2020','Burnley',1,'Wolves',1,'terminado').
evento('20192020',737,'14/07/2020','Newcastle',1,'Tottenham',3,'terminado').
evento('20192020',738,'14/07/2020','Everton',1,'Aston Villa',1,'terminado').
evento('20192020',739,'14/07/2020','Southampton',1,'Brighton',1,'terminado').
evento('20192020',740,'14/07/2020','West Ham',3,'Watford',1,'terminado').
evento('20192020',741,'18/07/2020','Norwich City',0,'Burnley',2,'terminado').
evento('20192020',742,'18/07/2020','Tottenham',3,'Leicester City',0,'terminado').
evento('20192020',743,'18/07/2020','Brighton',0,'Newcastle',0,'terminado').
evento('20192020',744,'18/07/2020','Watford',0,'Manchester City',4,'terminado').
evento('20192020',745,'18/07/2020','manchester united',1,'West Ham',1,'terminado').
evento('20192020',746,'18/07/2020','Bournemouth',0,'Southampton',2,'terminado').
evento('20192020',747,'18/07/2020','Sheffield United',0,'Everton',1,'terminado').
evento('20192020',748,'18/07/2020','Wolves',2,'Crystal Palace',0,'terminado').
evento('20192020',749,'18/07/2020','Aston Villa',1,'Arsenal',0,'terminado').
evento('20192020',750,'18/07/2020','Liverpool',5,'Chelsea',3,'terminado').
evento('20192020',751,'26/07/2020','Burnley',1,'Brighton',2,'terminado').
evento('20192020',752,'26/07/2020','Chelsea',2,'Wolves',0,'terminado').
evento('20192020',753,'26/07/2020','Newcastle',1,'Liverpool',3,'terminado').
evento('20192020',754,'26/07/2020','Manchester City',5,'Norwich City',0,'terminado').
evento('20192020',755,'26/07/2020','Leicester City',0,'manchester united',2,'terminado').
evento('20192020',756,'26/07/2020','Crystal Palace',1,'Tottenham',1,'terminado').
evento('20192020',757,'26/07/2020','West Ham',1,'Aston Villa',1,'terminado').
evento('20192020',758,'26/07/2020','Arsenal',3,'Watford',2,'terminado').
