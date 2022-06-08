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

es_un_equipo(Nombre):- equipo(Nombre,_).
% --> Listas de equipo alguna vez admitido en las temporadas 2018/19 o 2019/20
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


% evento deportivo ya reducido para compactar info
% evento('Temporada',Partido,'FECHA','Equipo Local',Goles a Favor Local,'Equipo Visitante',Goles a Favor Visitante,'status').
evento('20182019',1,'10/08/2018',15,2,12,1,'terminado').
evento('20182019',2,'11/08/2018',11,0,7,3,'terminado').
evento('20182019',3,'11/08/2018',21,2,4,0,'terminado').
evento('20182019',4,'11/08/2018',23,2,9,2,'terminado').
evento('20182019',5,'12/08/2018',13,4,22,0,'terminado').
evento('20182019',6,'11/08/2018',16,1,20,2,'terminado').
evento('20182019',7,'11/08/2018',3,2,6,0,'terminado').
evento('20182019',8,'11/08/2018',10,0,8,2,'terminado').
evento('20182019',9,'12/08/2018',19,0,5,0,'terminado').
evento('20182019',10,'12/08/2018',1,0,14,2,'terminado').
evento('20182019',11,'18/08/2018',6,0,16,0,'terminado').
evento('20182019',12,'18/08/2018',22,1,3,2,'terminado').
evento('20182019',13,'18/08/2018',9,2,19,1,'terminado').
evento('20182019',14,'19/08/2018',14,6,11,1,'terminado').
evento('20182019',15,'19/08/2018',4,3,15,2,'terminado').
evento('20182019',16,'18/08/2018',20,3,10,1,'terminado').
evento('20182019',17,'18/08/2018',12,2,23,0,'terminado').
evento('20182019',18,'18/08/2018',7,3,1,2,'terminado').
evento('20182019',19,'19/08/2018',5,1,21,3,'terminado').
evento('20182019',20,'19/08/2018',8,0,13,2,'terminado').
evento('20182019',21,'25/08/2018',23,1,14,1,'terminado').
evento('20182019',22,'25/08/2018',1,3,22,1,'terminado').
evento('20182019',23,'25/08/2018',11,0,6,0,'terminado').
evento('20182019',24,'26/08/2018',21,2,8,1,'terminado').
evento('20182019',25,'26/08/2018',16,1,7,2,'terminado').
evento('20182019',26,'25/08/2018',19,1,12,2,'terminado').
evento('20182019',27,'25/08/2018',3,2,9,2,'terminado').
evento('20182019',28,'25/08/2018',13,1,4,0,'terminado').
evento('20182019',29,'26/08/2018',10,4,5,2,'terminado').
evento('20182019',30,'26/08/2018',15,0,20,3,'terminado').
evento('20182019',31,'01/09/2018',12,1,13,2,'terminado').
evento('20182019',32,'01/09/2018',9,1,11,1,'terminado').
evento('20182019',33,'01/09/2018',7,2,3,0,'terminado').
evento('20182019',34,'01/09/2018',14,2,16,1,'terminado').
evento('20182019',35,'01/09/2018',21,2,20,1,'terminado').
evento('20182019',36,'01/09/2018',8,0,19,2,'terminado').
evento('20182019',37,'01/09/2018',4,2,10,2,'terminado').
evento('20182019',38,'01/09/2018',22,0,23,1,'terminado').
evento('20182019',39,'01/09/2018',6,2,1,3,'terminado').
evento('20182019',40,'01/09/2018',5,0,15,2,'terminado').
evento('20182019',41,'15/09/2018',20,1,13,2,'terminado').
evento('20182019',42,'15/09/2018',3,4,12,2,'terminado').
evento('20182019',43,'15/09/2018',11,0,8,1,'terminado').
evento('20182019',44,'15/09/2018',21,1,15,2,'terminado').
evento('20182019',45,'15/09/2018',9,1,22,3,'terminado').
evento('20182019',46,'15/09/2018',14,3,10,0,'terminado').
evento('20182019',47,'15/09/2018',16,1,1,2,'terminado').
evento('20182019',48,'15/09/2018',7,4,6,1,'terminado').
evento('20182019',49,'15/09/2018',23,1,5,0,'terminado').
evento('20182019',50,'15/09/2018',19,2,4,2,'terminado').
evento('20182019',51,'22/09/2018',10,1,21,1,'terminado').
evento('20182019',52,'22/09/2018',8,0,16,0,'terminado').
evento('20182019',53,'22/09/2018',6,0,14,5,'terminado').
evento('20182019',54,'22/09/2018',5,4,3,0,'terminado').
evento('20182019',55,'22/09/2018',22,0,7,0,'terminado').
evento('20182019',56,'22/09/2018',15,1,23,1,'terminado').
evento('20182019',57,'22/09/2018',12,3,11,1,'terminado').
evento('20182019',58,'22/09/2018',13,3,19,0,'terminado').
evento('20182019',59,'22/09/2018',4,1,20,2,'terminado').
evento('20182019',60,'22/09/2018',1,2,9,0,'terminado').
evento('20182019',61,'29/09/2018',22,3,15,1,'terminado').
evento('20182019',62,'29/09/2018',1,2,21,0,'terminado').
evento('20182019',63,'29/09/2018',16,0,12,2,'terminado').
evento('20182019',64,'29/09/2018',9,3,10,0,'terminado').
evento('20182019',65,'29/09/2018',6,1,5,2,'terminado').
evento('20182019',66,'29/09/2018',11,0,20,2,'terminado').
evento('20182019',67,'29/09/2018',23,2,19,0,'terminado').
evento('20182019',68,'29/09/2018',14,2,4,0,'terminado').
evento('20182019',69,'29/09/2018',7,1,13,1,'terminado').
evento('20182019',70,'29/09/2018',3,2,8,1,'terminado').
evento('20182019',71,'05/10/2018',4,1,22,0,'terminado').
evento('20182019',72,'05/10/2018',8,0,23,1,'terminado').
evento('20182019',73,'05/10/2018',12,1,9,2,'terminado').
evento('20182019',74,'05/10/2018',15,3,16,2,'terminado').
evento('20182019',75,'05/10/2018',19,0,7,3,'terminado').
evento('20182019',76,'05/10/2018',5,1,11,1,'terminado').
evento('20182019',77,'05/10/2018',21,0,3,4,'terminado').
evento('20182019',78,'05/10/2018',20,1,6,0,'terminado').
evento('20182019',79,'05/10/2018',10,1,1,5,'terminado').
evento('20182019',80,'05/10/2018',13,0,14,0,'terminado').
evento('20182019',81,'20/10/2018',7,2,15,2,'terminado').
evento('20182019',82,'20/10/2018',16,0,4,1,'terminado').
evento('20182019',83,'20/10/2018',6,4,10,2,'terminado').
evento('20182019',84,'20/10/2018',14,5,5,0,'terminado').
evento('20182019',85,'20/10/2018',9,2,8,0,'terminado').
evento('20182019',86,'20/10/2018',22,0,20,1,'terminado').
evento('20182019',87,'20/10/2018',3,0,19,0,'terminado').
evento('20182019',88,'20/10/2018',23,0,21,2,'terminado').
evento('20182019',89,'20/10/2018',11,0,13,1,'terminado').
evento('20182019',90,'20/10/2018',1,3,12,1,'terminado').
evento('20182019',91,'27/10/2018',4,1,23,0,'terminado').
evento('20182019',92,'27/10/2018',13,4,6,1,'terminado').
evento('20182019',93,'27/10/2018',21,3,11,0,'terminado').
evento('20182019',94,'27/10/2018',5,0,7,4,'terminado').
evento('20182019',95,'27/10/2018',15,2,9,1,'terminado').
evento('20182019',96,'27/10/2018',19,0,16,0,'terminado').
evento('20182019',97,'27/10/2018',10,0,3,3,'terminado').
evento('20182019',98,'27/10/2018',12,1,22,1,'terminado').
evento('20182019',99,'27/10/2018',8,2,1,2,'terminado').
evento('20182019',100,'27/10/2018',20,0,14,1,'terminado').
evento('20182019',101,'03/11/2018',3,1,15,2,'terminado').
evento('20182019',102,'03/11/2018',9,3,4,1,'terminado').
evento('20182019',103,'03/11/2018',6,0,12,1,'terminado').
evento('20182019',104,'03/11/2018',23,2,20,3,'terminado').
evento('20182019',105,'03/11/2018',7,3,8,1,'terminado').
evento('20182019',106,'03/11/2018',16,1,21,0,'terminado').
evento('20182019',107,'03/11/2018',22,4,5,2,'terminado').
evento('20182019',108,'03/11/2018',1,1,13,1,'terminado').
evento('20182019',109,'03/11/2018',14,6,19,1,'terminado').
evento('20182019',110,'03/11/2018',11,1,10,0,'terminado').
evento('20182019',111,'10/11/2018',6,2,4,1,'terminado').
evento('20182019',112,'10/11/2018',12,0,5,0,'terminado').
evento('20182019',113,'10/11/2018',16,2,3,1,'terminado').
evento('20182019',114,'10/11/2018',13,2,10,0,'terminado').
evento('20182019',115,'10/11/2018',7,0,9,0,'terminado').
evento('20182019',116,'10/11/2018',1,1,23,1,'terminado').
evento('20182019',117,'10/11/2018',19,1,21,1,'terminado').
evento('20182019',118,'10/11/2018',11,1,22,1,'terminado').
evento('20182019',119,'10/11/2018',8,0,20,1,'terminado').
evento('20182019',120,'10/11/2018',14,3,15,1,'terminado').
evento('20182019',121,'24/11/2018',22,0,14,4,'terminado').
evento('20182019',122,'24/11/2018',4,1,12,1,'terminado').
evento('20182019',123,'24/11/2018',21,0,13,3,'terminado').
evento('20182019',124,'24/11/2018',20,3,7,1,'terminado').
evento('20182019',125,'24/11/2018',23,0,11,2,'terminado').
evento('20182019',126,'24/11/2018',9,1,6,0,'terminado').
evento('20182019',127,'24/11/2018',15,0,8,0,'terminado').
evento('20182019',128,'24/11/2018',10,3,19,2,'terminado').
evento('20182019',129,'24/11/2018',3,1,1,2,'terminado').
evento('20182019',130,'24/11/2018',5,1,16,2,'terminado').
evento('20182019',131,'30/11/2018',6,2,23,1,'terminado').
evento('20182019',132,'30/11/2018',12,2,21,0,'terminado').
evento('20182019',133,'30/11/2018',16,0,22,3,'terminado').
evento('20182019',134,'30/11/2018',19,2,15,2,'terminado').
evento('20182019',135,'30/11/2018',1,4,20,2,'terminado').
evento('20182019',136,'30/11/2018',14,3,3,1,'terminado').
evento('20182019',137,'30/11/2018',8,2,5,0,'terminado').
evento('20182019',138,'30/11/2018',11,1,4,2,'terminado').
evento('20182019',139,'30/11/2018',7,2,10,0,'terminado').
evento('20182019',140,'30/11/2018',13,1,9,0,'terminado').
evento('20182019',141,'04/12/2018',3,2,11,1,'terminado').
evento('20182019',142,'04/12/2018',4,3,8,1,'terminado').
evento('20182019',143,'04/12/2018',10,1,12,1,'terminado').
evento('20182019',144,'04/12/2018',5,1,13,3,'terminado').
evento('20182019',145,'04/12/2018',15,2,1,2,'terminado').
evento('20182019',146,'04/12/2018',22,3,6,1,'terminado').
evento('20182019',147,'04/12/2018',21,1,14,2,'terminado').
evento('20182019',148,'04/12/2018',23,2,7,1,'terminado').
evento('20182019',149,'04/12/2018',9,1,16,1,'terminado').
evento('20182019',150,'04/12/2018',20,3,19,1,'terminado').
evento('20182019',151,'08/12/2018',3,0,13,4,'terminado').
evento('20182019',152,'08/12/2018',15,4,10,1,'terminado').
evento('20182019',153,'08/12/2018',6,1,19,0,'terminado').
evento('20182019',154,'08/12/2018',7,2,14,0,'terminado').
evento('20182019',155,'08/12/2018',16,1,23,2,'terminado').
evento('20182019',156,'08/12/2018',5,1,4,0,'terminado').
evento('20182019',157,'08/12/2018',1,1,11,0,'terminado').
evento('20182019',158,'08/12/2018',22,3,8,2,'terminado').
evento('20182019',159,'08/12/2018',12,0,20,2,'terminado').
evento('20182019',160,'08/12/2018',9,2,21,2,'terminado').
evento('20182019',161,'15/12/2018',14,3,9,1,'terminado').
evento('20182019',162,'15/12/2018',23,2,3,0,'terminado').
evento('20182019',163,'15/12/2018',20,1,5,0,'terminado').
evento('20182019',164,'15/12/2018',10,0,22,2,'terminado').
evento('20182019',165,'15/12/2018',4,1,7,2,'terminado').
evento('20182019',166,'15/12/2018',8,1,12,0,'terminado').
evento('20182019',167,'15/12/2018',11,0,16,1,'terminado').
evento('20182019',168,'15/12/2018',21,3,6,2,'terminado').
evento('20182019',169,'15/12/2018',19,3,1,2,'terminado').
evento('20182019',170,'15/12/2018',13,3,15,1,'terminado').
evento('20182019',171,'21/12/2018',23,0,13,2,'terminado').
evento('20182019',172,'21/12/2018',11,1,19,3,'terminado').
evento('20182019',173,'21/12/2018',14,2,8,3,'terminado').
evento('20182019',174,'21/12/2018',7,0,12,1,'terminado').
evento('20182019',175,'21/12/2018',6,1,15,5,'terminado').
evento('20182019',176,'21/12/2018',1,3,5,1,'terminado').
evento('20182019',177,'21/12/2018',3,2,4,0,'terminado').
evento('20182019',178,'21/12/2018',16,0,10,0,'terminado').
evento('20182019',179,'21/12/2018',22,0,21,2,'terminado').
evento('20182019',180,'21/12/2018',9,2,20,6,'terminado').
evento('20182019',181,'26/12/2018',10,1,23,1,'terminado').
evento('20182019',182,'26/12/2018',13,4,16,0,'terminado').
evento('20182019',183,'26/12/2018',12,2,14,1,'terminado').
evento('20182019',184,'26/12/2018',15,3,11,1,'terminado').
evento('20182019',185,'26/12/2018',21,1,7,2,'terminado').
evento('20182019',186,'26/12/2018',5,1,9,5,'terminado').
evento('20182019',187,'26/12/2018',8,0,6,0,'terminado').
evento('20182019',188,'26/12/2018',20,5,3,0,'terminado').
evento('20182019',189,'26/12/2018',4,1,1,1,'terminado').
evento('20182019',190,'26/12/2018',19,1,22,2,'terminado').
evento('20182019',191,'29/12/2018',21,1,16,1,'terminado').
evento('20182019',192,'29/12/2018',4,1,9,0,'terminado').
evento('20182019',193,'29/12/2018',10,1,11,0,'terminado').
evento('20182019',194,'29/12/2018',13,5,1,1,'terminado').
evento('20182019',195,'29/12/2018',19,1,14,3,'terminado').
evento('20182019',196,'29/12/2018',15,4,3,1,'terminado').
evento('20182019',197,'29/12/2018',20,1,23,3,'terminado').
evento('20182019',198,'29/12/2018',12,0,6,1,'terminado').
evento('20182019',199,'29/12/2018',8,0,7,1,'terminado').
evento('20182019',200,'29/12/2018',5,2,22,0,'terminado').
evento('20182019',201,'01/01/2019',9,0,12,1,'terminado').
evento('20182019',202,'01/01/2019',6,0,20,3,'terminado').
evento('20182019',203,'01/01/2019',7,0,19,0,'terminado').
evento('20182019',204,'01/01/2019',11,1,5,2,'terminado').
evento('20182019',205,'01/01/2019',3,3,21,3,'terminado').
evento('20182019',206,'01/01/2019',1,4,10,1,'terminado').
evento('20182019',207,'01/01/2019',23,0,8,2,'terminado').
evento('20182019',208,'01/01/2019',22,2,4,2,'terminado').
evento('20182019',209,'01/01/2019',16,0,15,2,'terminado').
evento('20182019',210,'01/01/2019',14,2,13,1,'terminado').
evento('20182019',211,'12/01/2019',22,1,1,0,'terminado').
evento('20182019',212,'12/01/2019',8,1,21,2,'terminado').
evento('20182019',213,'12/01/2019',4,0,13,1,'terminado').
evento('20182019',214,'12/01/2019',7,2,16,1,'terminado').
evento('20182019',215,'12/01/2019',20,0,15,1,'terminado').
evento('20182019',216,'12/01/2019',5,2,10,1,'terminado').
evento('20182019',217,'12/01/2019',6,0,11,0,'terminado').
evento('20182019',218,'12/01/2019',12,1,19,2,'terminado').
evento('20182019',219,'12/01/2019',9,2,3,0,'terminado').
evento('20182019',220,'12/01/2019',14,3,23,0,'terminado').
evento('20182019',221,'19/01/2019',23,4,12,3,'terminado').
evento('20182019',222,'19/01/2019',21,0,5,0,'terminado').
evento('20182019',223,'19/01/2019',15,2,4,1,'terminado').
evento('20182019',224,'19/01/2019',19,2,9,1,'terminado').
evento('20182019',225,'19/01/2019',11,0,14,3,'terminado').
evento('20182019',226,'19/01/2019',13,4,8,3,'terminado').
evento('20182019',227,'19/01/2019',16,3,6,0,'terminado').
evento('20182019',228,'19/01/2019',3,2,22,0,'terminado').
evento('20182019',229,'19/01/2019',1,2,7,0,'terminado').
evento('20182019',230,'19/01/2019',10,1,20,2,'terminado').
evento('20182019',231,'29/01/2019',1,2,6,1,'terminado').
evento('20182019',232,'29/01/2019',23,3,22,0,'terminado').
evento('20182019',233,'29/01/2019',16,2,14,1,'terminado').
evento('20182019',234,'29/01/2019',19,1,8,1,'terminado').
evento('20182019',235,'29/01/2019',13,1,12,1,'terminado').
evento('20182019',236,'29/01/2019',10,4,4,2,'terminado').
evento('20182019',237,'29/01/2019',11,0,9,1,'terminado').
evento('20182019',238,'29/01/2019',15,2,5,2,'terminado').
evento('20182019',239,'29/01/2019',3,4,7,0,'terminado').
evento('20182019',240,'29/01/2019',20,2,21,1,'terminado').
evento('20182019',241,'02/02/2019',20,1,16,0,'terminado').
evento('20182019',242,'02/02/2019',7,5,11,0,'terminado').
evento('20182019',243,'02/02/2019',5,1,19,1,'terminado').
evento('20182019',244,'02/02/2019',6,2,3,0,'terminado').
evento('20182019',245,'02/02/2019',14,3,1,1,'terminado').
evento('20182019',246,'02/02/2019',9,1,23,3,'terminado').
evento('20182019',247,'02/02/2019',4,0,21,0,'terminado').
evento('20182019',248,'02/02/2019',8,2,10,0,'terminado').
evento('20182019',249,'02/02/2019',12,0,15,1,'terminado').
evento('20182019',250,'02/02/2019',22,1,13,1,'terminado').
evento('20182019',251,'09/02/2019',10,0,15,3,'terminado').
evento('20182019',252,'09/02/2019',11,1,1,2,'terminado').
evento('20182019',253,'09/02/2019',8,1,22,1,'terminado').
evento('20182019',254,'09/02/2019',4,1,5,3,'terminado').
evento('20182019',255,'09/02/2019',14,6,7,0,'terminado').
evento('20182019',256,'09/02/2019',21,1,9,0,'terminado').
evento('20182019',257,'09/02/2019',19,1,6,2,'terminado').
evento('20182019',258,'09/02/2019',13,3,3,0,'terminado').
evento('20182019',259,'09/02/2019',20,3,12,1,'terminado').
evento('20182019',260,'09/02/2019',23,1,16,1,'terminado').
evento('20182019',261,'06/02/2019',9,0,14,2,'terminado').
evento('20182019',262,'22/02/2019',22,3,10,1,'terminado').
evento('20182019',263,'22/02/2019',5,2,20,1,'terminado').
evento('20182019',264,'22/02/2019',16,2,11,0,'terminado').
evento('20182019',265,'22/02/2019',1,2,19,0,'terminado').
evento('20182019',266,'22/02/2019',6,1,21,5,'terminado').
evento('20182019',267,'22/02/2019',3,1,23,1,'terminado').
evento('20182019',268,'22/02/2019',12,1,8,4,'terminado').
evento('20182019',269,'22/02/2019',15,0,13,0,'terminado').
evento('20182019',270,'26/02/2019',11,1,23,0,'terminado').
evento('20182019',271,'26/02/2019',12,2,4,1,'terminado').
evento('20182019',272,'26/02/2019',19,2,10,0,'terminado').
evento('20182019',273,'26/02/2019',8,1,15,3,'terminado').
evento('20182019',274,'26/02/2019',14,1,22,0,'terminado').
evento('20182019',275,'26/02/2019',6,0,9,3,'terminado').
evento('20182019',276,'26/02/2019',16,2,5,0,'terminado').
evento('20182019',277,'26/02/2019',1,5,3,1,'terminado').
evento('20182019',278,'26/02/2019',13,5,21,0,'terminado').
evento('20182019',279,'26/02/2019',7,2,20,0,'terminado').
evento('20182019',280,'02/03/2019',20,1,1,1,'terminado').
evento('20182019',281,'02/03/2019',4,1,11,0,'terminado').
evento('20182019',282,'02/03/2019',15,3,19,2,'terminado').
evento('20182019',283,'02/03/2019',22,2,16,0,'terminado').
evento('20182019',284,'02/03/2019',10,1,7,2,'terminado').
evento('20182019',285,'02/03/2019',5,1,8,3,'terminado').
evento('20182019',286,'02/03/2019',3,0,14,1,'terminado').
evento('20182019',287,'02/03/2019',23,2,6,0,'terminado').
evento('20182019',288,'02/03/2019',21,2,12,1,'terminado').
evento('20182019',289,'02/03/2019',9,0,13,0,'terminado').
evento('20182019',290,'09/03/2019',8,1,4,2,'terminado').
evento('20182019',291,'09/03/2019',19,2,20,1,'terminado').
evento('20182019',292,'09/03/2019',6,2,22,0,'terminado').
evento('20182019',293,'09/03/2019',14,3,21,1,'terminado').
evento('20182019',294,'09/03/2019',7,1,23,1,'terminado').
evento('20182019',295,'09/03/2019',11,0,3,2,'terminado').
evento('20182019',296,'09/03/2019',12,3,10,1,'terminado').
evento('20182019',297,'09/03/2019',16,3,9,2,'terminado').
evento('20182019',298,'09/03/2019',13,4,5,2,'terminado').
evento('20182019',299,'09/03/2019',1,2,15,0,'terminado').
evento('20182019',300,'16/03/2019',22,4,11,3,'terminado').
evento('20182019',301,'16/03/2019',3,2,16,2,'terminado').
evento('20182019',302,'16/03/2019',9,2,7,0,'terminado').
evento('20182019',303,'16/03/2019',5,1,12,2,'terminado').
evento('20182019',304,'16/03/2019',10,1,13,2,'terminado').
evento('20182019',305,'30/03/2019',10,0,14,2,'terminado').
evento('20182019',306,'30/03/2019',12,2,3,0,'terminado').
evento('20182019',307,'30/03/2019',15,2,21,1,'terminado').
evento('20182019',308,'30/03/2019',4,0,19,1,'terminado').
evento('20182019',309,'30/03/2019',6,1,7,2,'terminado').
evento('20182019',310,'30/03/2019',1,2,16,0,'terminado').
evento('20182019',311,'30/03/2019',8,2,11,0,'terminado').
evento('20182019',312,'30/03/2019',5,2,23,0,'terminado').
evento('20182019',313,'30/03/2019',22,0,9,2,'terminado').
evento('20182019',314,'30/03/2019',13,2,20,1,'terminado').
evento('20182019',315,'02/04/2019',23,2,15,1,'terminado').
evento('20182019',316,'02/04/2019',7,3,4,0,'terminado').
evento('20182019',317,'02/04/2019',20,2,8,0,'terminado').
evento('20182019',318,'02/04/2019',21,4,10,1,'terminado').
evento('20182019',319,'03/04/2019',14,2,6,0,'terminado').
evento('20182019',320,'03/04/2019',11,1,12,4,'terminado').
evento('20182019',321,'03/04/2019',3,1,5,3,'terminado').
evento('20182019',322,'03/04/2019',7,2,22,0,'terminado').
evento('20182019',323,'03/04/2019',19,1,13,3,'terminado').
evento('20182019',324,'03/04/2019',16,0,8,1,'terminado').
evento('20182019',325,'03/04/2019',9,1,1,0,'terminado').
evento('20182019',326,'12/04/2019',12,0,16,1,'terminado').
evento('20182019',327,'12/04/2019',4,0,3,5,'terminado').
evento('20182019',328,'12/04/2019',10,2,9,0,'terminado').
evento('20182019',329,'12/04/2019',15,2,22,1,'terminado').
evento('20182019',330,'12/04/2019',13,2,7,0,'terminado').
evento('20182019',331,'12/04/2019',20,4,11,0,'terminado').
evento('20182019',332,'12/04/2019',19,3,23,1,'terminado').
evento('20182019',333,'12/04/2019',5,2,6,0,'terminado').
evento('20182019',334,'12/04/2019',8,1,14,3,'terminado').
evento('20182019',335,'12/04/2019',21,0,1,1,'terminado').
evento('20182019',336,'16/04/2019',4,0,6,2,'terminado').
evento('20182019',337,'20/04/2019',14,1,20,0,'terminado').
evento('20182019',338,'20/04/2019',22,2,12,2,'terminado').
evento('20182019',339,'20/04/2019',23,0,4,0,'terminado').
evento('20182019',340,'20/04/2019',9,4,15,0,'terminado').
evento('20182019',341,'20/04/2019',1,2,8,3,'terminado').
evento('20182019',342,'20/04/2019',7,2,5,2,'terminado').
evento('20182019',343,'20/04/2019',11,1,21,2,'terminado').
evento('20182019',344,'20/04/2019',3,0,10,1,'terminado').
evento('20182019',345,'20/04/2019',16,3,19,1,'terminado').
evento('20182019',346,'20/04/2019',6,0,13,2,'terminado').
evento('20182019',347,'23/04/2019',21,1,19,1,'terminado').
evento('20182019',348,'23/04/2019',20,1,4,0,'terminado').
evento('20182019',349,'23/04/2019',23,3,1,1,'terminado').
evento('20182019',350,'23/04/2019',15,0,14,2,'terminado').
evento('20182019',351,'26/04/2019',13,5,11,0,'terminado').
evento('20182019',352,'26/04/2019',19,3,3,3,'terminado').
evento('20182019',353,'26/04/2019',21,1,23,2,'terminado').
evento('20182019',354,'26/04/2019',4,1,16,1,'terminado').
evento('20182019',355,'26/04/2019',5,0,14,1,'terminado').
evento('20182019',356,'26/04/2019',20,0,22,1,'terminado').
evento('20182019',357,'26/04/2019',10,1,6,0,'terminado').
evento('20182019',358,'26/04/2019',8,0,9,0,'terminado').
evento('20182019',359,'26/04/2019',12,3,1,0,'terminado').
evento('20182019',360,'26/04/2019',15,1,7,1,'terminado').
evento('20182019',361,'03/05/2019',9,2,5,0,'terminado').
evento('20182019',362,'03/05/2019',22,3,19,0,'terminado').
evento('20182019',363,'03/05/2019',6,2,8,3,'terminado').
evento('20182019',364,'03/05/2019',7,3,21,0,'terminado').
evento('20182019',365,'03/05/2019',1,1,4,1,'terminado').
evento('20182019',366,'03/05/2019',3,1,20,0,'terminado').
evento('20182019',367,'03/05/2019',23,1,10,0,'terminado').
evento('20182019',368,'03/05/2019',16,2,13,3,'terminado').
evento('20182019',369,'03/05/2019',11,1,15,1,'terminado').
evento('20182019',370,'03/05/2019',14,1,12,0,'terminado').
evento('20182019',371,'12/05/2019',20,2,9,2,'terminado').
evento('20182019',372,'12/05/2019',21,1,22,4,'terminado').
evento('20182019',373,'12/05/2019',12,0,7,0,'terminado').
evento('20182019',374,'12/05/2019',13,2,23,0,'terminado').
evento('20182019',375,'12/05/2019',4,1,14,4,'terminado').
evento('20182019',376,'12/05/2019',15,0,6,2,'terminado').
evento('20182019',377,'12/05/2019',19,1,11,1,'terminado').
evento('20182019',378,'12/05/2019',10,0,16,4,'terminado').
evento('20182019',379,'12/05/2019',8,5,3,3,'terminado').
evento('20182019',380,'12/05/2019',5,1,1,3,'terminado').
evento('20192020',381,'09/08/2019',13,4,17,1,'terminado').
evento('20192020',382,'09/08/2019',8,0,9,0,'terminado').
evento('20192020',383,'09/08/2019',21,0,4,3,'terminado').
evento('20192020',384,'09/08/2019',20,3,2,1,'terminado').
evento('20192020',385,'09/08/2019',16,0,1,1,'terminado').
evento('20192020',386,'09/08/2019',22,0,14,5,'terminado').
evento('20192020',387,'09/08/2019',5,3,19,0,'terminado').
evento('20192020',388,'09/08/2019',3,1,18,1,'terminado').
evento('20192020',389,'09/08/2019',12,0,23,0,'terminado').
evento('20192020',390,'09/08/2019',15,4,7,0,'terminado').
evento('20192020',391,'17/08/2019',1,2,5,1,'terminado').
evento('20192020',392,'17/08/2019',4,1,22,1,'terminado').
evento('20192020',393,'17/08/2019',17,3,16,1,'terminado').
evento('20192020',394,'17/08/2019',14,2,20,2,'terminado').
evento('20192020',395,'17/08/2019',7,1,12,1,'terminado').
evento('20192020',396,'17/08/2019',19,1,13,2,'terminado').
evento('20192020',397,'17/08/2019',9,1,21,0,'terminado').
evento('20192020',398,'17/08/2019',2,1,3,2,'terminado').
evento('20192020',399,'17/08/2019',18,1,8,0,'terminado').
evento('20192020',400,'17/08/2019',23,1,15,1,'terminado').
evento('20192020',401,'23/08/2019',2,2,9,0,'terminado').
evento('20192020',402,'23/08/2019',4,0,19,2,'terminado').
evento('20192020',403,'23/08/2019',21,1,22,3,'terminado').
evento('20192020',404,'23/08/2019',13,3,1,1,'terminado').
evento('20192020',405,'23/08/2019',20,0,16,1,'terminado').
evento('20192020',406,'23/08/2019',17,2,7,3,'terminado').
evento('20192020',407,'23/08/2019',15,1,8,2,'terminado').
evento('20192020',408,'23/08/2019',18,1,12,2,'terminado').
evento('20192020',409,'23/08/2019',3,1,14,3,'terminado').
evento('20192020',410,'23/08/2019',23,1,5,1,'terminado').
evento('20192020',411,'31/08/2019',19,1,15,1,'terminado').
evento('20192020',412,'31/08/2019',14,4,4,0,'terminado').
evento('20192020',413,'31/08/2019',12,3,3,1,'terminado').
evento('20192020',414,'31/08/2019',8,1,2,0,'terminado').
evento('20192020',415,'31/08/2019',5,0,13,3,'terminado').
evento('20192020',416,'31/08/2019',1,2,20,2,'terminado').
evento('20192020',417,'31/08/2019',16,1,21,1,'terminado').
evento('20192020',418,'31/08/2019',22,2,17,0,'terminado').
evento('20192020',419,'31/08/2019',7,2,18,2,'terminado').
evento('20192020',420,'31/08/2019',9,3,23,2,'terminado').
evento('20192020',421,'14/09/2019',13,3,16,1,'terminado').
evento('20192020',422,'14/09/2019',18,0,19,1,'terminado').
evento('20192020',423,'14/09/2019',23,2,7,5,'terminado').
evento('20192020',424,'14/09/2019',17,3,14,2,'terminado').
evento('20192020',425,'14/09/2019',21,2,1,2,'terminado').
evento('20192020',426,'14/09/2019',15,1,12,0,'terminado').
evento('20192020',427,'14/09/2019',4,1,5,1,'terminado').
evento('20192020',428,'14/09/2019',20,4,8,0,'terminado').
evento('20192020',429,'14/09/2019',3,3,9,1,'terminado').
evento('20192020',430,'14/09/2019',2,0,22,0,'terminado').
evento('20192020',431,'20/09/2019',19,1,3,3,'terminado').
evento('20192020',432,'20/09/2019',5,2,17,0,'terminado').
evento('20192020',433,'20/09/2019',14,8,21,0,'terminado').
evento('20192020',434,'20/09/2019',22,2,15,0,'terminado').
evento('20192020',435,'20/09/2019',1,3,2,2,'terminado').
evento('20192020',436,'20/09/2019',12,2,20,1,'terminado').
evento('20192020',437,'20/09/2019',9,0,18,2,'terminado').
evento('20192020',438,'20/09/2019',16,0,4,0,'terminado').
evento('20192020',439,'20/09/2019',8,1,23,1,'terminado').
evento('20192020',440,'20/09/2019',7,1,13,2,'terminado').
evento('20192020',441,'28/09/2019',18,0,13,1,'terminado').
evento('20192020',442,'28/09/2019',2,2,5,2,'terminado').
evento('20192020',443,'28/09/2019',23,2,21,0,'terminado').
evento('20192020',444,'28/09/2019',7,2,4,0,'terminado').
evento('20192020',445,'28/09/2019',12,5,16,0,'terminado').
evento('20192020',446,'28/09/2019',8,2,17,0,'terminado').
evento('20192020',447,'28/09/2019',3,2,22,2,'terminado').
evento('20192020',448,'28/09/2019',20,2,19,1,'terminado').
evento('20192020',449,'28/09/2019',9,1,14,3,'terminado').
evento('20192020',450,'28/09/2019',15,1,1,1,'terminado').
evento('20192020',451,'05/10/2018',4,3,20,0,'terminado').
evento('20192020',452,'05/10/2018',17,1,2,5,'terminado').
evento('20192020',453,'05/10/2018',5,1,9,0,'terminado').
evento('20192020',454,'05/10/2018',19,1,7,4,'terminado').
evento('20192020',455,'05/10/2018',1,1,3,0,'terminado').
evento('20192020',456,'05/10/2018',13,2,12,1,'terminado').
evento('20192020',457,'05/10/2018',21,0,18,0,'terminado').
evento('20192020',458,'05/10/2018',22,1,8,2,'terminado').
evento('20192020',459,'05/10/2018',14,0,23,2,'terminado').
evento('20192020',460,'05/10/2018',16,1,15,0,'terminado').
evento('20192020',461,'19/10/2019',9,2,22,0,'terminado').
evento('20192020',462,'19/10/2019',20,1,21,1,'terminado').
evento('20192020',463,'19/10/2019',7,1,16,0,'terminado').
evento('20192020',464,'19/10/2019',12,2,5,1,'terminado').
evento('20192020',465,'19/10/2019',15,1,13,1,'terminado').
evento('20192020',466,'19/10/2019',2,2,4,1,'terminado').
evento('20192020',467,'19/10/2019',23,1,19,1,'terminado').
evento('20192020',468,'19/10/2019',3,0,17,0,'terminado').
evento('20192020',469,'19/10/2019',8,0,14,2,'terminado').
evento('20192020',470,'19/10/2019',18,1,1,0,'terminado').
evento('20192020',471,'25/10/2019',19,0,12,9,'terminado').
evento('20192020',472,'25/10/2019',21,0,3,0,'terminado').
evento('20192020',473,'25/10/2019',22,1,18,1,'terminado').
evento('20192020',474,'25/10/2019',16,1,23,1,'terminado').
evento('20192020',475,'25/10/2019',13,2,20,1,'terminado').
evento('20192020',476,'25/10/2019',14,3,2,0,'terminado').
evento('20192020',477,'25/10/2019',4,3,9,2,'terminado').
evento('20192020',478,'25/10/2019',5,2,7,4,'terminado').
evento('20192020',479,'25/10/2019',1,2,8,2,'terminado').
evento('20192020',480,'25/10/2019',17,1,15,3,'terminado').
evento('20192020',481,'01/10/2019',3,1,15,0,'terminado').
evento('20192020',482,'01/10/2019',2,1,13,2,'terminado').
evento('20192020',483,'01/10/2019',18,3,5,0,'terminado').
evento('20192020',484,'01/10/2019',14,2,19,1,'terminado').
evento('20192020',485,'01/10/2019',8,0,12,2,'terminado').
evento('20192020',486,'01/10/2019',22,2,16,3,'terminado').
evento('20192020',487,'01/10/2019',1,1,23,1,'terminado').
evento('20192020',488,'01/10/2019',4,2,17,0,'terminado').
evento('20192020',489,'01/10/2019',21,1,7,2,'terminado').
evento('20192020',490,'01/10/2019',9,1,20,1,'terminado').
evento('20192020',491,'08/11/2019',17,0,21,2,'terminado').
evento('20192020',492,'08/11/2019',7,2,8,0,'terminado').
evento('20192020',493,'08/11/2019',16,2,3,1,'terminado').
evento('20192020',494,'08/11/2019',20,1,18,1,'terminado').
evento('20192020',495,'08/11/2019',23,2,2,1,'terminado').
evento('20192020',496,'08/11/2019',13,3,14,1,'terminado').
evento('20192020',497,'08/11/2019',5,3,22,0,'terminado').
evento('20192020',498,'08/11/2019',19,1,9,2,'terminado').
evento('20192020',499,'08/11/2019',12,2,1,0,'terminado').
evento('20192020',500,'08/11/2019',15,3,4,1,'terminado').
evento('20192020',501,'23/11/2019',22,2,20,3,'terminado').
evento('20192020',502,'23/11/2019',4,0,12,2,'terminado').
evento('20192020',503,'23/11/2019',8,1,13,2,'terminado').
evento('20192020',504,'23/11/2019',9,0,17,2,'terminado').
evento('20192020',505,'23/11/2019',18,3,15,3,'terminado').
evento('20192020',506,'23/11/2019',1,2,19,2,'terminado').
evento('20192020',507,'23/11/2019',21,0,5,3,'terminado').
evento('20192020',508,'23/11/2019',3,1,23,2,'terminado').
evento('20192020',509,'23/11/2019',14,2,7,1,'terminado').
evento('20192020',510,'23/11/2019',2,2,16,0,'terminado').
evento('20192020',511,'30/11/2019',16,2,14,2,'terminado').
evento('20192020',512,'30/11/2019',5,0,8,2,'terminado').
evento('20192020',513,'30/11/2019',20,3,3,2,'terminado').
evento('20192020',514,'30/11/2019',17,2,1,2,'terminado').
evento('20192020',515,'30/11/2019',12,2,9,1,'terminado').
evento('20192020',516,'30/11/2019',13,2,4,1,'terminado').
evento('20192020',517,'30/11/2019',7,0,22,1,'terminado').
evento('20192020',518,'30/11/2019',19,2,21,1,'terminado').
evento('20192020',519,'30/11/2019',23,1,18,1,'terminado').
evento('20192020',520,'30/11/2019',15,2,2,2,'terminado').
evento('20192020',521,'02/12/2019',8,1,3,0,'terminado').
evento('20192020',522,'02/12/2019',7,2,2,1,'terminado').
evento('20192020',523,'02/12/2019',19,2,17,1,'terminado').
evento('20192020',524,'02/12/2019',15,2,20,1,'terminado').
evento('20192020',525,'02/12/2019',18,0,16,2,'terminado').
evento('20192020',526,'02/12/2019',5,1,14,4,'terminado').
evento('20192020',527,'02/12/2019',12,2,21,0,'terminado').
evento('20192020',528,'02/12/2019',23,2,22,0,'terminado').
evento('20192020',529,'02/12/2019',13,5,9,2,'terminado').
evento('20192020',530,'02/12/2019',1,1,4,2,'terminado').
evento('20192020',531,'07/12/2019',9,3,7,1,'terminado').
evento('20192020',532,'07/12/2019',20,5,5,0,'terminado').
evento('20192020',533,'07/12/2019',14,1,15,2,'terminado').
evento('20192020',534,'07/12/2019',16,2,19,1,'terminado').
evento('20192020',535,'07/12/2019',4,2,23,2,'terminado').
evento('20192020',536,'07/12/2019',3,0,13,3,'terminado').
evento('20192020',537,'07/12/2019',21,0,8,0,'terminado').
evento('20192020',538,'07/12/2019',2,1,12,4,'terminado').
evento('20192020',539,'07/12/2019',17,1,18,2,'terminado').
evento('20192020',540,'07/12/2019',22,1,1,3,'terminado').
evento('20192020',541,'14/12/2019',13,2,21,0,'terminado').
evento('20192020',542,'14/12/2019',18,2,2,0,'terminado').
evento('20192020',543,'14/12/2019',5,1,16,0,'terminado').
evento('20192020',544,'14/12/2019',15,1,9,1,'terminado').
evento('20192020',545,'14/12/2019',1,0,14,3,'terminado').
evento('20192020',546,'14/12/2019',7,0,3,1,'terminado').
evento('20192020',547,'14/12/2019',12,1,17,1,'terminado').
evento('20192020',548,'14/12/2019',19,0,22,1,'terminado').
evento('20192020',549,'14/12/2019',23,1,20,2,'terminado').
evento('20192020',550,'14/12/2019',8,1,4,1,'terminado').
evento('20192020',551,'21/12/2019',9,0,1,0,'terminado').
evento('20192020',552,'21/12/2019',2,1,19,3,'terminado').
evento('20192020',553,'21/12/2019',4,0,18,1,'terminado').
evento('20192020',554,'21/12/2019',15,3,12,1,'terminado').
evento('20192020',555,'21/12/2019',20,0,7,2,'terminado').
evento('20192020',556,'21/12/2019',16,1,8,0,'terminado').
evento('20192020',557,'21/12/2019',17,1,23,2,'terminado').
evento('20192020',558,'21/12/2019',3,0,5,1,'terminado').
evento('20192020',559,'21/12/2019',21,2,15,0,'terminado').
evento('20192020',560,'26/12/2019',20,2,4,1,'terminado').
evento('20192020',561,'26/12/2019',18,1,21,1,'terminado').
evento('20192020',562,'26/12/2019',2,1,17,0,'terminado').
evento('20192020',563,'26/12/2019',9,1,5,0,'terminado').
evento('20192020',564,'26/12/2019',12,0,13,4,'terminado').
evento('20192020',565,'26/12/2019',3,1,1,1,'terminado').
evento('20192020',566,'26/12/2019',7,0,19,2,'terminado').
evento('20192020',567,'26/12/2019',8,2,22,1,'terminado').
evento('20192020',568,'26/12/2019',15,4,16,1,'terminado').
evento('20192020',569,'26/12/2019',23,3,14,2,'terminado').
evento('20192020',570,'28/12/2019',4,2,3,0,'terminado').
evento('20192020',571,'28/12/2019',16,1,9,2,'terminado').
evento('20192020',572,'28/12/2019',17,2,20,2,'terminado').
evento('20192020',573,'28/12/2019',5,0,15,2,'terminado').
evento('20192020',574,'28/12/2019',13,1,23,0,'terminado').
evento('20192020',575,'28/12/2019',19,1,8,1,'terminado').
evento('20192020',576,'28/12/2019',21,3,2,0,'terminado').
evento('20192020',577,'28/12/2019',22,1,12,2,'terminado').
evento('20192020',578,'28/12/2019',1,1,7,2,'terminado').
evento('20192020',579,'28/12/2019',14,2,18,0,'terminado').
evento('20192020',580,'01/01/2020',4,1,7,1,'terminado').
evento('20192020',581,'01/01/2020',16,0,12,3,'terminado').
evento('20192020',582,'01/01/2020',19,1,20,0,'terminado').
evento('20192020',583,'01/01/2020',14,2,9,1,'terminado').
evento('20192020',584,'01/01/2020',1,2,15,0,'terminado').
evento('20192020',585,'01/01/2020',5,1,2,2,'terminado').
evento('20192020',586,'01/01/2020',21,2,23,1,'terminado').
evento('20192020',587,'01/01/2020',22,4,3,0,'terminado').
evento('20192020',588,'01/01/2020',17,1,8,1,'terminado').
evento('20192020',589,'01/01/2020',13,2,18,0,'terminado').
evento('20192020',590,'10/01/2020',18,1,22,0,'terminado').
evento('20192020',591,'10/01/2020',9,1,4,0,'terminado').
evento('20192020',592,'10/01/2020',15,4,17,0,'terminado').
evento('20192020',593,'10/01/2020',23,1,16,1,'terminado').
evento('20192020',594,'10/01/2020',3,0,21,3,'terminado').
evento('20192020',595,'10/01/2020',8,1,1,1,'terminado').
evento('20192020',596,'10/01/2020',12,1,19,2,'terminado').
evento('20192020',597,'10/01/2020',7,3,5,0,'terminado').
evento('20192020',598,'10/01/2020',20,0,13,1,'terminado').
evento('20192020',599,'10/01/2020',2,1,14,6,'terminado').
evento('20192020',600,'18/01/2020',21,0,20,0,'terminado').
evento('20192020',601,'18/01/2020',14,2,8,2,'terminado').
evento('20192020',602,'18/01/2020',1,1,18,1,'terminado').
evento('20192020',603,'18/01/2020',19,2,23,3,'terminado').
evento('20192020',604,'18/01/2020',5,2,12,1,'terminado').
evento('20192020',605,'18/01/2020',13,2,15,0,'terminado').
evento('20192020',606,'18/01/2020',4,1,2,1,'terminado').
evento('20192020',607,'18/01/2020',22,1,9,1,'terminado').
evento('20192020',608,'18/01/2020',17,1,3,0,'terminado').
evento('20192020',609,'18/01/2020',16,1,7,0,'terminado').
evento('20192020',610,'21/01/2020',3,3,4,1,'terminado').
evento('20192020',611,'21/01/2020',8,0,19,2,'terminado').
evento('20192020',612,'21/01/2020',9,2,16,2,'terminado').
evento('20192020',613,'21/01/2020',12,4,22,1,'terminado').
evento('20192020',614,'21/01/2020',15,0,5,2,'terminado').
evento('20192020',615,'21/01/2020',2,2,21,1,'terminado').
evento('20192020',616,'21/01/2020',18,0,14,1,'terminado').
evento('20192020',617,'21/01/2020',7,2,1,2,'terminado').
evento('20192020',618,'21/01/2020',20,2,17,1,'terminado').
evento('20192020',619,'21/01/2020',23,1,13,2,'terminado').
evento('20192020',620,'21/01/2020',22,0,13,2,'terminado').
evento('20192020',621,'01/02/2020',12,2,7,2,'terminado').
evento('20192020',622,'01/02/2020',8,0,18,1,'terminado').
evento('20192020',623,'01/02/2020',3,2,2,1,'terminado').
evento('20192020',624,'01/02/2020',13,4,19,0,'terminado').
evento('20192020',625,'01/02/2020',5,0,1,0,'terminado').
evento('20192020',626,'01/02/2020',16,0,17,0,'terminado').
evento('20192020',627,'01/02/2020',22,3,4,3,'terminado').
evento('20192020',628,'01/02/2020',21,2,9,3,'terminado').
evento('20192020',629,'01/02/2020',15,0,23,0,'terminado').
evento('20192020',630,'01/02/2020',20,2,15,0,'terminado').
evento('20192020',631,'08/02/2020',9,3,8,1,'terminado').
evento('20192020',632,'08/02/2020',18,2,3,1,'terminado').
evento('20192020',633,'08/02/2020',19,1,5,2,'terminado').
evento('20192020',634,'08/02/2020',2,2,20,3,'terminado').
evento('20192020',635,'08/02/2020',7,0,15,2,'terminado').
evento('20192020',636,'08/02/2020',4,1,21,1,'terminado').
evento('20192020',637,'08/02/2020',23,0,12,0,'terminado').
evento('20192020',638,'08/02/2020',17,0,13,1,'terminado').
evento('20192020',639,'08/02/2020',1,4,16,0,'terminado').
evento('20192020',640,'08/02/2020',14,2,22,0,'terminado').
evento('20192020',641,'22/02/2020',7,2,20,1,'terminado').
evento('20192020',642,'22/02/2020',5,3,3,0,'terminado').
evento('20192020',643,'22/02/2020',19,2,2,0,'terminado').
evento('20192020',644,'22/02/2020',23,3,17,0,'terminado').
evento('20192020',645,'22/02/2020',1,3,9,2,'terminado').
evento('20192020',646,'22/02/2020',8,1,16,0,'terminado').
evento('20192020',647,'22/02/2020',18,1,4,1,'terminado').
evento('20192020',648,'22/02/2020',12,0,14,1,'terminado').
evento('20192020',649,'22/02/2020',15,3,21,0,'terminado').
evento('20192020',650,'22/02/2020',13,3,22,2,'terminado').
evento('20192020',651,'28/02/2020',17,1,12,0,'terminado').
evento('20192020',652,'28/02/2020',3,2,7,2,'terminado').
evento('20192020',653,'28/02/2020',22,3,19,1,'terminado').
evento('20192020',654,'28/02/2020',9,1,15,1,'terminado').
evento('20192020',655,'28/02/2020',4,0,8,1,'terminado').
evento('20192020',656,'28/02/2020',16,0,5,0,'terminado').
evento('20192020',657,'28/02/2020',21,3,13,0,'terminado').
evento('20192020',658,'28/02/2020',20,2,23,3,'terminado').
evento('20192020',659,'07/03/2020',13,2,3,1,'terminado').
evento('20192020',660,'07/03/2020',1,1,22,0,'terminado').
evento('20192020',661,'07/03/2020',18,1,17,0,'terminado').
evento('20192020',662,'07/03/2020',5,1,20,1,'terminado').
evento('20192020',663,'07/03/2020',15,2,14,0,'terminado').
evento('20192020',664,'07/03/2020',23,0,4,0,'terminado').
evento('20192020',665,'07/03/2020',19,0,16,1,'terminado').
evento('20192020',666,'07/03/2020',8,1,21,0,'terminado').
evento('20192020',667,'07/03/2020',7,4,9,0,'terminado').
evento('20192020',668,'07/03/2020',12,4,2,0,'terminado').
evento('20192020',669,'17/06/2020',2,0,18,0,'terminado').
evento('20192020',670,'17/06/2020',14,3,1,0,'terminado').
evento('20192020',671,'19/06/2020',17,0,19,3,'terminado').
evento('20192020',672,'19/06/2020',21,1,12,1,'terminado').
evento('20192020',673,'19/06/2020',22,0,23,2,'terminado').
evento('20192020',674,'19/06/2020',16,3,18,0,'terminado').
evento('20192020',675,'19/06/2020',9,0,13,0,'terminado').
evento('20192020',676,'19/06/2020',20,1,15,1,'terminado').
evento('20192020',677,'19/06/2020',4,2,1,1,'terminado').
evento('20192020',678,'19/06/2020',3,0,8,2,'terminado').
evento('20192020',679,'19/06/2020',2,1,7,2,'terminado').
evento('20192020',680,'19/06/2020',14,5,5,0,'terminado').
evento('20192020',681,'23/06/2020',12,0,4,0,'terminado').
evento('20192020',682,'23/06/2020',17,0,9,1,'terminado').
evento('20192020',683,'23/06/2020',15,3,18,0,'terminado').
evento('20192020',684,'23/06/2020',13,4,8,0,'terminado').
evento('20192020',685,'23/06/2020',5,1,21,0,'terminado').
evento('20192020',686,'23/06/2020',20,2,22,0,'terminado').
evento('20192020',687,'23/06/2020',16,1,2,1,'terminado').
evento('20192020',688,'23/06/2020',23,1,3,0,'terminado').
evento('20192020',689,'23/06/2020',19,0,1,2,'terminado').
evento('20192020',690,'23/06/2020',7,2,14,1,'terminado').
evento('20192020',691,'27/06/2020',2,0,23,1,'terminado').
evento('20192020',692,'27/06/2020',8,0,5,1,'terminado').
evento('20192020',693,'27/06/2020',1,4,17,0,'terminado').
evento('20192020',694,'27/06/2020',9,2,12,1,'terminado').
evento('20192020',695,'27/06/2020',18,3,20,1,'terminado').
evento('20192020',696,'27/06/2020',21,1,19,3,'terminado').
evento('20192020',697,'27/06/2020',4,0,15,3,'terminado').
evento('20192020',698,'27/06/2020',3,1,16,4,'terminado').
evento('20192020',699,'27/06/2020',22,3,7,2,'terminado').
evento('20192020',700,'27/06/2020',14,4,13,0,'terminado').
evento('20192020',701,'04/07/2020',17,0,4,1,'terminado').
evento('20192020',702,'04/07/2020',15,5,3,2,'terminado').
evento('20192020',703,'04/07/2020',7,3,21,0,'terminado').
evento('20192020',704,'04/07/2020',16,2,22,2,'terminado').
evento('20192020',705,'04/07/2020',19,1,14,0,'terminado').
evento('20192020',706,'04/07/2020',12,3,8,0,'terminado').
evento('20192020',707,'04/07/2020',23,0,1,2,'terminado').
evento('20192020',708,'04/07/2020',5,1,18,1,'terminado').
evento('20192020',709,'04/07/2020',13,2,2,0,'terminado').
evento('20192020',710,'04/07/2020',20,1,9,0,'terminado').
evento('20192020',711,'07/07/2020',21,2,17,1,'terminado').
evento('20192020',712,'07/07/2020',1,1,12,1,'terminado').
evento('20192020',713,'07/07/2020',14,5,16,0,'terminado').
evento('20192020',714,'07/07/2020',4,1,13,3,'terminado').
evento('20192020',715,'07/07/2020',3,0,20,0,'terminado').
evento('20192020',716,'07/07/2020',8,2,7,3,'terminado').
evento('20192020',717,'07/07/2020',18,1,23,0,'terminado').
evento('20192020',718,'07/07/2020',22,0,5,1,'terminado').
evento('20192020',719,'07/07/2020',9,1,19,1,'terminado').
evento('20192020',720,'07/07/2020',2,0,15,3,'terminado').
evento('20192020',721,'11/07/2020',21,2,16,1,'terminado').
evento('20192020',722,'11/07/2020',13,1,5,1,'terminado').
evento('20192020',723,'11/07/2020',4,0,14,5,'terminado').
evento('20192020',724,'11/07/2020',2,2,8,0,'terminado').
evento('20192020',725,'11/07/2020',3,4,12,1,'terminado').
evento('20192020',726,'11/07/2020',17,0,22,4,'terminado').
evento('20192020',727,'11/07/2020',18,3,7,0,'terminado').
evento('20192020',728,'11/07/2020',23,3,9,0,'terminado').
evento('20192020',729,'11/07/2020',20,2,1,1,'terminado').
evento('20192020',730,'11/07/2020',15,2,19,2,'terminado').
evento('20192020',731,'14/07/2020',7,1,17,0,'terminado').
evento('20192020',732,'14/07/2020',14,2,3,1,'terminado').
evento('20192020',733,'14/07/2020',1,2,13,1,'terminado').
evento('20192020',734,'14/07/2020',12,2,18,0,'terminado').
evento('20192020',735,'14/07/2020',8,0,15,2,'terminado').
evento('20192020',736,'14/07/2020',5,1,23,1,'terminado').
evento('20192020',737,'14/07/2020',16,1,20,3,'terminado').
evento('20192020',738,'14/07/2020',9,1,2,1,'terminado').
evento('20192020',739,'14/07/2020',19,1,4,1,'terminado').
evento('20192020',740,'14/07/2020',22,3,21,1,'terminado').
evento('20192020',741,'18/07/2020',17,0,5,2,'terminado').
evento('20192020',742,'18/07/2020',20,3,12,0,'terminado').
evento('20192020',743,'18/07/2020',4,0,16,0,'terminado').
evento('20192020',744,'18/07/2020',21,0,14,4,'terminado').
evento('20192020',745,'18/07/2020',15,1,22,1,'terminado').
evento('20192020',746,'18/07/2020',3,0,19,2,'terminado').
evento('20192020',747,'18/07/2020',18,0,9,1,'terminado').
evento('20192020',748,'18/07/2020',23,2,8,0,'terminado').
evento('20192020',749,'18/07/2020',2,1,1,0,'terminado').
evento('20192020',750,'18/07/2020',13,5,7,3,'terminado').
evento('20192020',751,'26/07/2020',5,1,4,2,'terminado').
evento('20192020',752,'26/07/2020',7,2,23,0,'terminado').
evento('20192020',753,'26/07/2020',16,1,13,3,'terminado').
evento('20192020',754,'26/07/2020',14,5,17,0,'terminado').
evento('20192020',755,'26/07/2020',12,0,15,2,'terminado').
evento('20192020',756,'26/07/2020',8,1,20,1,'terminado').
evento('20192020',757,'26/07/2020',22,1,2,1,'terminado').
evento('20192020',758,'26/07/2020',1,3,21,2,'terminado').
