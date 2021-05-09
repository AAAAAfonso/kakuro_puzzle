:- [codigo_comum].

elemento_comum_var([VarP|_], L2) :- elemento_pertence_var(VarP, L2),!.
elemento_comum_var([_|VarR], L2) :- elemento_comum_var(VarR,L2).

elemento_pertence_var(El, [P|_]) :- El == P,var(El).
elemento_pertence_var(El, [_|R]) :- elemento_pertence_var(El, R).

elemento_pertence_geral(El, [P|_]) :- El == P.
elemento_pertence_geral(El, [_|R]) :- elemento_pertence_geral(El, R).

indiceEl([Element|_], El, 0):- El == Element,!.
indiceEl([_|Tail], Element, Index):-
  indiceEl(Tail, Element, Index1),
  !,
  Index is Index1+1.


%ESPACO
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
adiciona_espaco_livre(Novo_espaco,espaco(Soma,Variaveis),Sub) :- append(Novo_espaco,Variaveis,X),Sub = espaco(Soma,X).

%PAR_POS
% construtor
cria_posnum(Pos,Num,(Pos,Num)).

% seletores
posnum_posicao((Pos,_), Pos).
posnum_numero((_,Num),Num).    

%modificadores
muda_posicao_posnum(PosN, (_,Num),(PosN,Num)).
muda_numero_posnum(NumN, (Pos,_),(Pos,NumN)).

%-------------------------------------------------------------------------------
%     combinacoes_soma(N, Els, Soma, Combs),  
%em  que N eh um  inteiro,Els eh uma lista de inteiros, eh Soma e um inteiro, 
%significa que Combs eh a lista ordenada cujos elementos sao as combinacoes N a N,
%dos elementos de Els cuja soma eh Soma.           
%-------------------------------------------------------------------------------

combinacoes_soma(N, Els, Soma, L) :- 
    findall(Com_P, 
    (combinacao(N,Els,Com_P),sumlist(Com_P, Sum),Sum == Soma), 
    L).

%-------------------------------------------------------------------------------
%     permutacoes_soma(N, Els, Soma, Perms),  
%em  que "N" eh um  inteiro,"Els" e uma lista de inteiros, e "Soma" eh um inteiro,
%significa que Perms eh a lista ordenada cujos elementos sao as permutacoes 
%das combinacoes N a N, dos elementos de Els cuja soma eh "Soma".          
%-------------------------------------------------------------------------------

permutacoes_soma(N, Els, Soma, L) :- 
    findall(X,
    (combinacoes_soma(N,Els,Soma,Res),member(El,Res),permutation(El,X))
    ,Nord),
    sort(0,@=<,Nord,L).


novo_espaco(Esp,[P|R]) :- 
    var(P),
    novo_espaco(EspN,R),
    !,
    adiciona_espaco_livre([P],EspN,Esp).

novo_espaco(espaco(_,[]),_).

espaco_fila_ver([P,S|R],Esp) :- 
    (is_list(P),
    var(S),
    P = [El|_],
    cria_espaco(El,_,Esp),
    novo_espaco(Esp,[S|R]))
    ;
    espaco_fila_ver([S|R],Esp).

espaco_fila_hor([P,S|R],Esp) :- 
    (is_list(P),
    var(S),
    P = [_,El|_],
    cria_espaco(El,_,Esp),
    novo_espaco(Esp,[S|R]));
    espaco_fila_hor([S|R],Esp).


espaco_fila(Fila,Esp,v) :- espaco_fila_ver(Fila,Esp).
espaco_fila(Fila,Esp,h)  :- espaco_fila_hor(Fila,Esp).

espacos_fila(H_V,Fila,Espacos) :- bagof(X,espaco_fila(Fila,X,H_V), L) -> Espacos = L; Espacos = [].

%todas_as_colunas(Puzzle) :-

espaco_puzzle_aux([],[]).

espaco_puzzle_aux([Espaco1|E_Res],[Esp_R|Resto]) :- 
    Esp_R = Espaco1,
    espaco_puzzle_aux(E_Res,Resto).

espacos_puzzle(Puzzle,Espacos) :-
    bagof(X,Fila^L_L^(member(Fila,Puzzle),espacos_fila(h,Fila,L_L),member(X,L_L)),L1),
    bagof(X,Fila^L_L^PuzzleT^(mat_transposta(Puzzle,PuzzleT),member(Fila,PuzzleT),espacos_fila(v,Fila,L_L),member(X,L_L)),L2),
    append(L1,L2,Todos_espacos),
    !,espaco_puzzle_aux(Todos_espacos,Espacos).

remove_iguais([],_,[]).

remove_iguais([EspacoPossivel|R],Vars,[EspacoPossivel|R1]) :-
    remove_iguais(R,Vars,R1),
    espacos_livres(EspacoPossivel,VarPossivel),
    VarPossivel \== Vars.

remove_iguais([EspacoPossivel|R],Vars,R1) :-
    remove_iguais(R,Vars,R1),
    espacos_livres(EspacoPossivel,VarPossivel),
    VarPossivel == Vars.


espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    bagof(X,(member(X,Espacos),
    espacos_elemento_comum_var(X,Esp)),Esp_mais),
    espacos_livres(Esp,Vars),
    remove_iguais(Esp_mais,Vars,Esps_com),!.

permutacoes_soma_espacos([],[]).

permutacoes_soma_espacos([Espaco1|E_Res], [Perm1|Perm_R]) :-
    permutacoes_soma_espacos(E_Res, Perm_R),!,
    soma_espaco(Espaco1,Soma_Aux),
    espacos_livres(Espaco1,Var_aux),
    length(Var_aux,Len),
    permutacoes_soma(Len,[1,2,3,4,5,6,7,8,9],Soma_Aux,Lista),
    Perm1 = [Espaco1,Lista].

encontra_permutacoes(EspP,[Perms_somaP|R],Perms) :-
    Perms_somaP = [Espaco,Permutacoes|_],
    EspP == Espaco -> Perms = Permutacoes;
    encontra_permutacoes(EspP,R,Perms),!.

%lista_valores_possiveis([],_,[]).

lista_valores_possiveis([],_,[]).

lista_valores_possiveis([EspP|EspR],Perms_soma,[ValP|ValR]) :-
    lista_valores_possiveis(EspR,Perms_soma,ValR),
    !,
    encontra_permutacoes(EspP,Perms_soma,Perm_Uteis),
    setof(X,El^(member(El,Perm_Uteis),nth0(0,El,X)),ValP).

perm_possivel(_,[]) :- !.

perm_possivel([PermP|Res],[Val1|R]) :-
    perm_possivel(Res,R),
    !,
    member(PermP,Val1).

permutacao_possivel_espaco_aux(Valores,[Perm_P|R],Perm) :-
    (perm_possivel(Perm_P,Valores),Perm = Perm_P);
    permutacao_possivel_espaco_aux(Valores,R,Perm).

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
    espacos_com_posicoes_comuns(Espacos,Esp,Espacos_comuns),
    lista_valores_possiveis(Espacos_comuns,Perms_soma,Valores),
    encontra_permutacoes(Esp,Perms_soma,Perm_Uteis),
    permutacao_possivel_espaco_aux(Valores,Perm_Uteis,Perm).

%Predicadopermutacoes_possiveis_espaco

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp,Perms_poss) :-
    findall(X,permutacao_possivel_espaco(X, Esp, Espacos, Perms_soma),L),
    espacos_livres(Esp,Var),
    Perms_poss = [Var,L].

%Predicadopermutacoes_possiveis_espacos

permutacoes_possiveis_espacos_aux([],_,_,[]).

permutacoes_possiveis_espacos_aux([EspP|R],EspF,Perms_soma,[PermP|R1]):-
    permutacoes_possiveis_espacos_aux(R,EspF,Perms_soma,R1),
    !,
    permutacoes_possiveis_espaco(EspF,Perms_soma,EspP,PermP).

permutacoes_possiveis_espacos(Espacos, Perms_poss) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    permutacoes_possiveis_espacos_aux(Espacos,Espacos,Perms_soma, Perms_poss).



%Predicado numeros_comuns

elementos_pos_comuns(Perm,N,El) :-
    findall(X,(member(El,Perm),nth1(N,El,X)),L),
    L = [Num|_],
    delete(L,Num,[]),
    cria_posnum(N,Num,El).

numeros_comuns(List_Perms,Numeros_comuns) :-
    List_Perms = [L1|_],
    length(L1,N),
    findall(X,(between(0,N,V),elementos_pos_comuns(List_Perms,V,X)),Numeros_comuns).

atribui_comuns_aux(_,[]) :- !.

atribui_comuns_aux(Livres,[PrimeiroPar|R]) :-
    atribui_comuns_aux(Livres,R),
    !,
    posnum_posicao(PrimeiroPar,Pos),
    posnum_numero(PrimeiroPar,Num),
    nth1(Pos,Livres,Num).

atribui_comuns([]).

atribui_comuns([Lista|R]) :-
    atribui_comuns(R),!,
    Lista = [L,Prem],
    numeros_comuns(Prem,Numeros_comuns),
    atribui_comuns_aux(L,Numeros_comuns).

eh_tudo_livre([],[]) :- !.

eh_tudo_livre([P|R],L1) :- var(P),!,eh_tudo_livre(R,L1).
eh_tudo_livre([P|R],[P|Re]) :- eh_tudo_livre(R,Re).

retira_impossiveis_aux([],NovaPrem,_,NovaPrem).

retira_impossiveis_aux([P|R],Prem,PossiveisVars,NovaPrem) :-
    indiceEl(PossiveisVars,P,Ind),
    findall(X, (member(X,Prem),nth0(Ind,X,El),El == P),L),
    L \== [],
    retira_impossiveis_aux(R,L,PossiveisVars,NovaPrem).

retira_impossiveis([],[]).

retira_impossiveis([Pri|R],[NewP|R1]) :-
    retira_impossiveis(R,R1),
    !,
    Pri = [PossiveisVars,Prem],
    eh_tudo_livre(PossiveisVars,El),
    retira_impossiveis_aux(El,Prem,PossiveisVars,NovaPrem),
    NewP = [PossiveisVars,NovaPrem].


simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis,Perms_Possiveis_temp),
    (Perms_Possiveis_temp == Perms_Possiveis -> 
    Novas_Perms_Possiveis = Perms_Possiveis_temp;
    simplifica(Perms_Possiveis_temp, Novas_Perms_Possiveis)).

inicializa(Puzzle,Perms_Possiveis) :-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms),
    simplifica(Perms, Perms_Possiveis).

len_valido(N,V) :- (N > 1, N =< V;V < 1,N > 1),!.

escolhe_menos_alternativas_aux([],0,[]). 

%precorre uma lista com todas as permutacoes possiveis
%e adiciona a uma lista Escolha apenas permutacoes possiveis com menos 
%premutacoes que a anterior, apenas se o numero de premutacoes for maior que 1.
escolhe_menos_alternativas_aux([PermP|Res],V, [EscolhaP|R]) :-
    escolhe_menos_alternativas_aux(Res,N, R),!,
    PermP = [_,Prem],
    length(Prem,Len),
    (len_valido(Len,N) -> EscolhaP = PermP, V is Len;
    V is N,EscolhaP = []).

escolhe_menos_alternativas(PermP, Escolha) :-
    escolhe_menos_alternativas_aux(PermP,L,EscolhaP),
    L > 1,
    delete(EscolhaP,[],[Escolha|_]).

substitui_elementos([],_,_,_,[]) :- !.

substitui_elementos([PrimeiroEsp|RestoEsp],Esp,EspBase,Perm,[PrimeiroEsp|Resto]) :-
    \+ elemento_pertence_var(PrimeiroEsp,Esp),
    substitui_elementos(RestoEsp,Esp,EspBase,Perm,Resto),!.

substitui_elementos([PrimeiroEsp|RestoEsp],Esp,EspBase,Perm,[Elemento|Resto]) :-
    indiceEl(Esp,PrimeiroEsp,Ind),
    nth0(Ind,Perm,PossivelEl),
%    \+ elemento_pertence_geral(PossivelEl,EspBase),
    Elemento = PossivelEl,
    substitui_elementos(RestoEsp,Esp,EspBase,Perm,Resto),!.

experimenta_perm_aux(EspBase,PermBase,Esp,_,Nova_EspPerm) :- 
    \+ elemento_comum_var(Esp,EspBase),
    Nova_EspPerm = [EspBase,PermBase],!.

experimenta_perm_aux(EspBase,_,Esp,Perm,Nova_EspPerm) :-
    EspBase == Esp, 
    Nova_EspPerm = [Perm,[Perm]],!.

experimenta_perm_aux(EspBase,PermBase,Esp,Perm,Nova_EspPerm) :-
    substitui_elementos(EspBase,Esp,EspBase,Perm,Nova_Esp),
    Nova_EspPerm = [Nova_Esp,PermBase].

experimenta_perm1(_,_,[],[]) :- !.

experimenta_perm1(Esp,Perm,[Perms_Possiveis|PermsR],[Novas_PermP|Resto]) :-
    experimenta_perm1(Esp,Perm,PermsR,Resto),
    !,
    Perms_Possiveis = [EspBase,PermBase],
    experimenta_perm_aux(EspBase,PermBase,Esp,Perm,Novas_PermP).

experimenta_perm(Escolha,Perms_Possiveis, Novas_Perms_Possiveis) :-
    Escolha = [Esp,Lst_Perms],
    member(Perm,Lst_Perms),
    experimenta_perm1(Esp,Perm,Perms_Possiveis,Novas_Perms_Possiveis).

%--------------------------------------------------------------------%

permutacoes_validas([]) :- !.

permutacoes_validas([Perms|R]) :-
    permutacoes_validas(R),
    !,
    Perms = [EspVar|_],
    list_to_set(EspVar,Esp), 
    Esp == EspVar.


resolve_aux_verificacao(Perms_Possiveis,Escolha, Novas_Perms_Possiveis) :-
    experimenta_perm(Escolha,Perms_Possiveis,Lista_possivel),
    permutacoes_validas(Lista_possivel),
    retira_impossiveis(Lista_possivel,Possivel1),
    simplifica(Possivel1,Possivel),
    Novas_Perms_Possiveis = Possivel.

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
    ((Perms_Possiveis == Perms_Possiveis,\+ escolhe_menos_alternativas(Perms_Possiveis, _)) 
    -> Novas_Perms_Possiveis = Perms_Possiveis;
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    resolve_aux_verificacao(Perms_Possiveis,Escolha,L),
    resolve_aux(L,Novas_Perms_Possiveis)).

precorre_todos_espacos([],[]).

precorre_todos_espacos([Espaco|Resto],[Perms|R]) :-
    precorre_todos_espacos(Resto,R),
    !,
    Perms = [El,_],
    preenche_espaco(Espaco,El).

resolve(Puzzle) :-
    espacos_puzzle(Puzzle,Espacos),
    inicializa(Puzzle, Perms_Possiveis),
    resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis),
    precorre_todos_espacos(Espacos,Novas_Perms_Possiveis).
