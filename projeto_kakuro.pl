%AFONSO FREITAS - 99173
:- [codigo_comum].

elemento_comum_var([VarP|_], L2) :- elemento_pertence_var(VarP, L2),!.
elemento_comum_var([_|VarR], L2) :- elemento_comum_var(VarR,L2).

elemento_pertence_var(El, [P|_]) :- El == P,var(El).
elemento_pertence_var(El, [_|R]) :- elemento_pertence_var(El, R).

elemento_pertence_geral(El, [P|_]) :- El == P.
elemento_pertence_geral(El, [_|R]) :- elemento_pertence_geral(El, R).

%dado um elemento e uma lista retorna o primeiro indice desse elemento
indiceDe([Elemento|_], El, 0):- El == Elemento,!.
indiceDe([_|R], Elemento, Indice):-
  indiceDe(R, Elemento, Indice1),
  !,
  Indice is Indice1+1.


%ESPACO = espaco(Soma,Vars), onde Vars e uma lista do tipo Var
% construtor
cria_espaco(Soma,Variaveis,espaco(Soma,Variaveis)).

% seletores
soma_espaco(espaco(Soma,_), Soma).
espacos_livres(espaco(_,Variaveis), Variaveis).
espacos_elemento_comum_var(Espaco1,Espaco2) :- 
    espacos_livres(Espaco1,List1),
    espacos_livres(Espaco2,List2),
    elemento_comum_var(List1,List2).    

%modificadores
preenche_espaco(espaco(_,Variaveis),Nova_Var) :- Variaveis = Nova_Var.
adiciona_espaco_livre(Novo_espaco,espaco(Soma,Variaveis),espaco(Soma,X)) :- 
    append(Novo_espaco,Variaveis,X).

%PAR_POS
% construtor
cria_posnum(Pos,Num,(Pos,Num)).

% seletores
posnum_posicao((Pos,_), Pos).
posnum_numero((_,Num),Num).    

%modificadores


%-------------------------------------------------------------------------------
%     combinacoes_soma(N, Els, Soma, Combs),  
% em  que N eh um  inteiro,Els eh uma lista de inteiros, eh Soma e um inteiro, 
% significa que Combs eh a lista ordenada cujos elementos sao as combinacoes N a N,
% dos elementos de Els cuja soma eh Soma.           
%-------------------------------------------------------------------------------

combinacoes_soma(N, Els, Soma, L) :- 
    findall(Com_P, 
    (combinacao(N,Els,Com_P),sumlist(Com_P, Sum),Sum == Soma), 
    L).

%-------------------------------------------------------------------------------
%     permutacoes_soma(N, Els, Soma, Perms),  
% em  que "N" eh um  inteiro,"Els" e uma lista de inteiros, e "Soma" eh um inteiro,
% significa que Perms eh a lista ordenada cujos elementos sao as permutacoes 
% das combinacoes N a N, dos elementos de Els cuja soma eh "Soma".          
%-------------------------------------------------------------------------------

permutacoes_soma(N, Els, Soma, L) :- 
    findall(X,
    (combinacoes_soma(N,Els,Soma,Res),member(El,Res),permutation(El,X))
    ,Nord),
    sort(0,@=<,Nord,L).

%-------------------------------------------------------------------------------
%     espaco_fila(Fila, Esp, H_V), 
% em que "Fila" e uma fila (linha ou coluna) de um puzzle, "H_V" e um dos atomos
% h ou v, conforme se trate de uma fila horizontal ou vertical,
% respectivamente, significa que "Esp" e um espaco de "Fila", 
% tal como descrito na Seccao 2.1,no passo 1.          
%-------------------------------------------------------------------------------

espaco_fila(Fila,Esp,v) :- espaco_fila_ver(Fila,Esp).
espaco_fila(Fila,Esp,h)  :- espaco_fila_hor(Fila,Esp).

%funcao auxiliar de espaco_fila para quando analisa-se uma coluna
espaco_fila_ver([P,S|R],Esp) :- 
    (is_list(P),
    var(S),
    P = [El|_],
    cria_espaco(El,_,Esp),
    novo_espaco(Esp,[S|R]))
    ;
    espaco_fila_ver([S|R],Esp).

%funcao auxiliar de espaco_fila para quando analisa-se uma linha
espaco_fila_hor([P,S|R],Esp) :- 
    (is_list(P),
    var(S),
    P = [_,El|_],
    cria_espaco(El,_,Esp),
    novo_espaco(Esp,[S|R]));
    espaco_fila_hor([S|R],Esp).

%adiciona a um espaco as variaveis seguidas
novo_espaco(Esp,[P|R]) :- 
    var(P),
    novo_espaco(EspN,R),
    !,
    adiciona_espaco_livre([P],EspN,Esp).

novo_espaco(espaco(_,[]),_).

%-------------------------------------------------------------------------------
%     espacos_fila(H_V, Fila, Espacos), 
% em que "Fila" e uma fila (linha ou coluna) de uma grelha e "H_V" e um dos atomos
% h ou v, significa que "Espacos" e a lista de todos os espacos de "Fila", 
% da esquerda para a direita.          
%-------------------------------------------------------------------------------

espacos_fila(H_V,Fila,Espacos) :- 
    bagof(X,espaco_fila(Fila,X,H_V), L) -> Espacos = L
    ;
    Espacos = [].

%-------------------------------------------------------------------------------
%      espacos_puzzle(Puzzle, Espacos),  
% em que "Puzzle" e um puzzle,significa que "Espacos",
% e a lista de espacos de "Puzzle", tal como descrito na seccao 2.1 
%-------------------------------------------------------------------------------

espacos_puzzle(Puzzle,Espacos) :-
    bagof(X,Fila^L_L^(member(Fila,Puzzle),espacos_fila(h,Fila,L_L),member(X,L_L)),L1),
    bagof(X,Fila^L_L^PuzzleT^(mat_transposta(Puzzle,PuzzleT),member(Fila,PuzzleT),espacos_fila(v,Fila,L_L),member(X,L_L)),L2),
    append(L1,L2,Espacos).

%-------------------------------------------------------------------------------
%      espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
% em que "Espacos" e uma lista de espacos e "Esp" e um espaco,
% significa que "Esps_com" e a lista de espacos com variaveis em comum com "Esp"
% exceptuando "Esp". 
%-------------------------------------------------------------------------------

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    bagof(X,(member(X,Espacos),
    espacos_elemento_comum_var(X,Esp)),Esp_mais),
    exclude(==(Esp),Esp_mais,Esps_com),!.

%-------------------------------------------------------------------------------
%      permutacoes_soma_espacos(Espacos, Perms_soma), 
% em  que "Espacos" e  uma lista de espacos,  significa que "Perms_soma" 
% e a lista de listas de 2 elementos,  em que o primeiro elemento e um espaco 
% de Espacose e o segundo elemento e a lista ordenada de permutacoes cuja soma 
% e igual a soma do espaco.
%-------------------------------------------------------------------------------

permutacoes_soma_espacos([],[]).

permutacoes_soma_espacos([Espaco1|E_Res], [Perm1|Perm_R]) :-
    permutacoes_soma_espacos(E_Res, Perm_R),!,
    soma_espaco(Espaco1,Soma_Aux),
    espacos_livres(Espaco1,Var_aux),
    length(Var_aux,Len),
    permutacoes_soma(Len,[1,2,3,4,5,6,7,8,9],Soma_Aux,Lista),
    Perm1 = [Espaco1,Lista].

%-------------------------------------------------------------------------------
%      permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma),  
% em  que "Perm" e uma permutacao, "Esp" e um  espaco, "Espacos"  e uma  lista 
% de  espacos, e "Perms_soma" e uma lista de listas tal como obtida pelo predicado
% anterior, significa que "Perm" e uma permutacao possivel para o espaco "Esp",
% tal como descrito na seccao 2.1, no passo 2.
%-------------------------------------------------------------------------------

%encontra_permutacoes(Esp,Perms_soma,Perms)
%com um dado espaco, "Esp", "Perms" sao as permutacoes possiveis para esse espaco
%essas permutacoes sao dadas numa lista de Permutacoes "Perms_soma"

encontra_permutacoes(EspP,[Perms_somaP|R],Perms) :-
    Perms_somaP = [Espaco,Permutacoes|_],
    EspP == Espaco -> Perms = Permutacoes;
    encontra_permutacoes(EspP,R,Perms),!.

%lista_valores_possiveis(Espacos,Perms_soma,Valores),
%"Valores" corresponde a uma lista com todos os valores,
%difereenetes de  "Perms_soma" que uma lista de "Espacos" pode tomar 

lista_valores_possiveis([],_,[]).

lista_valores_possiveis([EspP|EspR],Perms_soma,[ValP|ValR]) :-
    lista_valores_possiveis(EspR,Perms_soma,ValR),
    !,
    encontra_permutacoes(EspP,Perms_soma,Perm_Uteis),
    setof(X,El^(member(El,Perm_Uteis),nth0(0,El,X)),ValP).

%perm_possivel(Perm,Valores),
%verifica se uma permutacao "Perm" e possivel existir
%com os "Valores" dados

perm_possivel(_,[]) :- !.

perm_possivel([PermP|Res],[Val1|R]) :-
    perm_possivel(Res,R),
    !,
    member(PermP,Val1).

%permutacao_possivel_espaco_aux(Valores,Perms,Perm),
%dado uma lista de "Valores" e uma lista de "Perms",
%seleciona "Perm"'s que sejam possiveis existir 

permutacao_possivel_espaco_aux(Valores,[Perm_P|R],Perm) :-
    (perm_possivel(Perm_P,Valores),Perm = Perm_P);
    permutacao_possivel_espaco_aux(Valores,R,Perm).

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
    espacos_com_posicoes_comuns(Espacos,Esp,Espacos_comuns),
    lista_valores_possiveis(Espacos_comuns,Perms_soma,Valores),
    encontra_permutacoes(Esp,Perms_soma,Perm_Uteis),
    permutacao_possivel_espaco_aux(Valores,Perm_Uteis,Perm).

%-------------------------------------------------------------------------------
%      permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp,Perms_poss), 
% em  que "Espacos" e  uma  lista  de  espacos,"Perms_soma" e  uma  lista 
% de listas tal como obtida pelo predicado permutacoes_soma_espacos,  e "Esp" e um 
% espaco,  significa que "Perms_poss" e uma lista de 2 elementos em que o primeiro
% e a lista de variaveis de "Esp" e o segundo e a lista ordenada de 
% permutacoes possiveis para o espaco "Esp", tal como descrito na Seccao 2.1, no passo 2.
%-------------------------------------------------------------------------------

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp,Perms_poss) :-
    findall(X,permutacao_possivel_espaco(X, Esp, Espacos, Perms_soma),L),
    espacos_livres(Esp,Var),
    Perms_poss = [Var,L].

%-------------------------------------------------------------------------------
%      permutacoes_possiveis_espacos(Espacos, Perms_poss_esps),
% em que "Espacos" e uma lista de espacos, significa que "Perms_poss_esps" 
% e a lista de permutacoes possiveis, tal como descrito na Seccao 2.1, no passo 2.
%-------------------------------------------------------------------------------

permutacoes_possiveis_espacos(Espacos, Perms_poss) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    permutacoes_possiveis_espacos_aux(Espacos,Espacos,Perms_soma, Perms_poss).

permutacoes_possiveis_espacos_aux([],_,_,[]).

permutacoes_possiveis_espacos_aux([EspP|R],EspF,Perms_soma,[PermP|R1]):-
    permutacoes_possiveis_espacos_aux(R,EspF,Perms_soma,R1),
    !,
    permutacoes_possiveis_espaco(EspF,Perms_soma,EspP,PermP).



%-------------------------------------------------------------------------------
%      numeros_comuns(Lst_Perms, Numeros_comuns), 
% em que Lst_Perms e uma lista de permutacoes, significa que Numeros_comuns 
% e uma lista de pares(pos, numero),significando  que  todas  as  listas  
% de Lst_Perms contem o  numero "numero" na  posicao "pos".
%-------------------------------------------------------------------------------

numeros_comuns(List_Perms,Numeros_comuns) :-
    List_Perms = [L1|_],
    length(L1,N),
    findall(X,(between(0,N,V),elementos_pos_comuns(List_Perms,V,X)),Numeros_comuns).

%elementos_pos_comuns(Perms,Indice,Elemento)
%recebe uma lista de permutacoes "Perms" e um indice "Indice"
%e verifica se em cada uma das Permutacoes no mesmo "Indice" existe o mesmo valor

elementos_pos_comuns(Perms,Ind,El) :-
    findall(X,(member(El,Perms),nth1(Ind,El,X)),L),
    L = [Num|_],
    delete(L,Num,[]),
    cria_posnum(Ind,Num,El).

%-------------------------------------------------------------------------------
%      atribui_comuns(Perms_Possiveis),  
% em que Perms_Possiveis e  uma  lista  de permutacoes  possiveis,  actualiza
% esta  lista  atribuindo a cada  espaco  numeros  comuns a todas as 
% permutacoes possiveis para esse espaco.
%-------------------------------------------------------------------------------

atribui_comuns([]).

atribui_comuns([Lista|R]) :-
    atribui_comuns(R),!,
    Lista = [Vars,Perm],
    numeros_comuns(Perm,Numeros_comuns),
    atribui_comuns_aux(Vars,Numeros_comuns).

%atribui_comun_aux(Vars,Pares)
%recebe uma lista de "Vars" e verifica se para cada 
%Par de "Pares" e unificavel, se for unifica

atribui_comuns_aux(_,[]) :- !.

atribui_comuns_aux(Livres,[PrimeiroPar|R]) :-
    atribui_comuns_aux(Livres,R),
    !,
    posnum_posicao(PrimeiroPar,Pos),
    posnum_numero(PrimeiroPar,Num),
    nth1(Pos,Livres,Num).

%-------------------------------------------------------------------------------
%      retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis),
% em que Perms_Possiveis e uma lista de permutacoes possiveis, significa que
% Novas_Perms_Possiveis e o resultado de tirar permutacoes impossiveis de 
% Perms_Possiveis, tal como descrito na Seccao 2.1, no passo 3b.
%-------------------------------------------------------------------------------

retira_impossiveis([],[]).

retira_impossiveis([Pri|R],[NewP|R1]) :-
    retira_impossiveis(R,R1),
    !,
    Pri = [PossiveisVars,Perm],
    elementos_nao_var(PossiveisVars,El),
    retira_impossiveis_aux(El,Perm,PossiveisVars,NovaPerm),
    NewP = [PossiveisVars,NovaPerm].

%para um dado numero de "Numeros" retira as Permutacoes que em relacao 
%as "PossiveisVars" sao invalidas, executa recursivamente ate Numeros
% chegar a "[]" quando isso acontece unifica "PossiveisVars"com "NovaPerm"
retira_impossiveis_aux([],NovaPerm,_,NovaPerm).

retira_impossiveis_aux([P|R],Perm,PossiveisVars,NovaPerm) :-
    indiceDe(PossiveisVars,P,Ind),
    findall(X, (member(X,Perm),nth0(Ind,X,El),El == P),L),
    L \== [],
    retira_impossiveis_aux(R,L,PossiveisVars,NovaPerm).

%dado uma lista com elementos extrai aqueles que nao sao do tipo Var

elementos_nao_var([],[]) :- !.

elementos_nao_var([P|R],L1) :- var(P),!,elementos_nao_var(R,L1).
elementos_nao_var([P|R],[P|Re]) :- elementos_nao_var(R,Re).

%-------------------------------------------------------------------------------
%      simplifica(Perms_Possiveis, Novas_Perms_Possiveis),
% em que Perms_Possiveis e uma lista de permutacoes possiveis, significa que
% Novas_Perms_Possiveis e o resultado  de  simplificar Perms_Possiveis, tal
% como  descrito  na  Seccao  2.1,  no  passo  3.  
%-------------------------------------------------------------------------------

simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis,Perms_Possiveis_temp),
    (Perms_Possiveis_temp == Perms_Possiveis -> 
    Novas_Perms_Possiveis = Perms_Possiveis_temp;
    simplifica(Perms_Possiveis_temp, Novas_Perms_Possiveis)).


%-------------------------------------------------------------------------------
%      inicializa(Puzzle, Perms_Possiveis), em que Puzzle e um puzzle, significa
%quePerms_Possiveis e a lista de permutacoes possiveis simplificada para Puzzle.
%-------------------------------------------------------------------------------

inicializa(Puzzle,Perms_Possiveis) :-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms),
    simplifica(Perms, Perms_Possiveis).

%-------------------------------------------------------------------------------
%      escolhe_menos_alternativas(Perms_Possiveis, Escolha),
%em que Perms_Possiveis e uma lista de permutacoes possiveis, significa que 
%Escolha e o elemento de Perms_Possiveis escolhido segundo o criterio 
%indicado na Seccao 2.2, em Perms_Possiveis tiverem  associadas  listas  
%de permutacoes unitarias, o predicado devolve "falso".
%-------------------------------------------------------------------------------

escolhe_menos_alternativas(PermP, Escolha) :-
    escolhe_menos_alternativas_aux(PermP,L,EscolhaP),
    L > 1,
    delete(EscolhaP,[],[Escolha|_]).

len_valido(PossLen,AntLen) :- 
    (PossLen > 1, PossLen =< AntLen;
    AntLen < 1,PossLen > 1),!.

escolhe_menos_alternativas_aux([],0,[]). 

%precorre uma lista com todas as permutacoes possiveis
%e adiciona a uma lista Escolha apenas permutacoes possiveis com menos 
%Permutacoes que a anterior, apenas se o numero de Permutacoes for maior que 1
%ou caso nao exista uma Permutacao anterior.

escolhe_menos_alternativas_aux([PermP|Res],V, [EscolhaP|R]) :-
    escolhe_menos_alternativas_aux(Res,N, R),!,
    PermP = [_,Perm],
    length(Perm,Len),
    (len_valido(Len,N) -> EscolhaP = PermP, V is Len;
    V is N,EscolhaP = []).

%-------------------------------------------------------------------------------
%      experimenta_perm(Escolha, Perms_Possiveis,Novas_Perms_Possiveis),  
%em  que "Perms_Possiveis" e uma lista de permutacoes  possiveis, e, 
%"Nova_Perm_Possiveis" e o resultado da unificacao de um elemento de "Escolha",
%com "Perms_Possiveis"
%-------------------------------------------------------------------------------

experimenta_perm(Escolha,Perms_Possiveis, Novas_Perms_Possiveis) :-
    Escolha = [Esp,Lst_Perms],
    member(Perm,Lst_Perms),
    experimenta_perm1(Esp,Perm,Perms_Possiveis,Novas_Perms_Possiveis).

%experimenta_perm1(Esp,Perm,Perms_Possiveis,Novas_Perms_Possiveis)
%Unifica "Esp" com "Perm" se e altera "Novas_Perms_possiveis" possivel, 
%caso contrario examina o proximo elemento de "Perms_Possiveis"

experimenta_perm1(Esp,Perm,[Perm_Possivel|R],[NovaPerm|R]) :-
    Perm_Possivel = [Vars,_],
    Esp == Vars,
    NovaPerm = [Perm,[Perm]],
    Esp = Perm.

experimenta_perm1(Esp,Perm,[Perm_Possivel|R],[Perm_Possivel|R1]) :-
    experimenta_perm1(Esp,Perm,R,R1).

%-------------------------------------------------------------------------------
%      resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis),em que "Perms_Possiveis"
%e uma lista de permutacoes possiveis,significa que "Novas_Perms_Possiveis" e o 
%resultado de aplicar o algoritmo descrito na Seccao 2.2 a "Perms_Possiveis".
%-------------------------------------------------------------------------------

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
    ((Perms_Possiveis == Perms_Possiveis,\+ escolhe_menos_alternativas(Perms_Possiveis, _)) 
    -> Novas_Perms_Possiveis = Perms_Possiveis;
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    resolve_aux_verificacao(Perms_Possiveis,Escolha,L),
    resolve_aux(L,Novas_Perms_Possiveis)).

resolve_aux_verificacao(Perms_Possiveis,Escolha, Novas_Perms_Possiveis) :-
    experimenta_perm(Escolha,Perms_Possiveis,Lista_possivel),
    permutacoes_validas(Lista_possivel),
    retira_impossiveis(Lista_possivel,Possivel1),
    simplifica(Possivel1,Possivel),
    Novas_Perms_Possiveis = Possivel.

%permutacoes_validas(Perms)
%verifica se existe um elemento da lista de "Perms" repetido nos espacos 
%de uma Permutacao

permutacoes_validas([]) :- !.

permutacoes_validas([Perms|R]) :-
    permutacoes_validas(R),
    !,
    Perms = [EspVar|_],
    list_to_set(EspVar,Esp), 
    Esp == EspVar.

%-------------------------------------------------------------------------------
%      resolve(Puz), em quePuze um puzzle, resolve esse puzzle, isto e, apos a 
%invocacaodeste predicado a grelha de Puz tem todas as variaveis substituidas 
%por numeros querespeitam as restricoes Puz.
%-------------------------------------------------------------------------------
resolve(Puz) :-
    inicializa(Puz, Perms_Possiveis),
    resolve_aux(Perms_Possiveis, _).
