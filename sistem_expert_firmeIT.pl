
:-use_module(library(lists)).
:-use_module(library(system)).
:-use_module(library(file_systems)).
:-op(900,fy,not).
:-dynamic fapt/3.
:-dynamic interogat/1.
:-dynamic scop/1.
:-dynamic interogabil/3.
:-dynamic regula/3.
:-dynamic intrebare_curenta/3.
:-dynamic descriere/5. %adaugat pentru incarcarea descrierii

not(P):-P,!,fail.
not(_).

scrie_lista([]):-nl.
scrie_lista([H|T]) :-
write(H), tab(1),
scrie_lista(T).
			 
afiseaza_fapte :-
write('Fapte existente în baza de cunostinte:'),
nl,nl, write(' (Atribut,valoare) '), nl,nl,
listeaza_fapte,nl.

listeaza_fapte:-  % TREBUIE MODIFICAT PT CERINTA f
fapt(av(Atr,Val),FC,_), 
write('('),write(Atr),write(','),
write(Val), write(')'),
write(','), write(' certitudine '),
FC1 is integer(FC),write(FC1),
nl,fail.
listeaza_fapte.

lista_float_int([],[]).
lista_float_int([Regula|Reguli],[Regula1|Reguli1]):-
(Regula \== utiliz,
Regula1 is integer(Regula);
Regula ==utiliz, Regula1=Regula),
lista_float_int(Reguli,Reguli1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pornire :-
	retractall(interogat(_)), % in interogat am atributele pt care l-am intrb deja pe utilizator 
	retractall(fapt(_,_,_)), %%in fapt avem 3 param: structura atributvaloare: ex: av(anotimp,vara), factorul de certitudine ex: 80 , istoricul:ex: [utiliz] sau o lista cu id-uri de reguli[3,17].
	retractall(intrebare_curenta(_,_,_)),
	repeat,
	write('Introduceti una din urmatoarele optiuni: '),
	nl,nl,
	write(' (Incarca Consulta Reinitiaza  Afisare_fapte  Cum   Iesire) '),
	nl,nl,write('|: '),citeste_linie([H|T]),
	executa([H|T]), H == iesire.

executa([incarca]) :- 
	incarca_reguli('reguli.txt'),/*incarca_descriere('descriere.txt'),*/!,nl,
	write('Fisierul dorit a fost incarcat'),nl.
executa([consulta]) :- 
	scopuri_princ,!.
executa([reinitiaza]) :- 
	retractall(interogat(_)),
	retractall(fapt(_,_,_)),close_all_streams,!.
executa([afisare_fapte]) :-
	afiseaza_fapte,!.
executa([cum|L]) :- cum(L),!.  % pt interogare scriem: 'cum atribut este valoare...'
executa([iesire]):-!.
executa([_|_]) :-
	write('Comanda incorecta! '),nl.
	
sorteaza(Lscop) :- %ordoneaza solutiile descrescator dupa fc
    setof(solutie(FC,Atr,Val),Istoric^fapt(av(Atr,Val),FC,Istoric),Laux),   %ia toat din fapt si ordoneaza dupa fc
    reverse(Laux,L),
	findall(solutie(FC,actor,Val), member(solutie(FC,actor,Val),L),Lscop).	  %ia doar atributul care contine scopul deci solutia
	
afiseaza_solutii(L) :- %afiseaza solutiile pe ecran, aici le si sortam inainte de afisare
	sorteaza(L),nl,
	(length(L,Length),Length==0,write('Nu exista solutie!'), nl,!;
	nl,afiseaza_scop(L),nl ).

scopuri_princ :-
	scop(Atr),determina(Atr),afiseaza_solutii(L),!.
scopuri_princ. % caz de oprire

determina(Atr) :-
	realizare_scop(av(Atr,_),_,[scop(Atr)]),!.
determina(_). % oprire

afiseaza_scop([solutie(FC,Atr,Val)|T]) :- %daca solutia are fc< 50 nu e considerata solutie si nu se afiseaza; foloseste scrie_scop ca sa afiseze formatat
	FC >= 50,scrie_scop(av(Atr,Val),FC),
	nl, afiseaza_scop(T),fail.
afiseaza_scop(_):- nl.

scrie_scop(av(Atr,Val),FC) :-
transformare(av(Atr,Val), X),
scrie_lista(X),tab(2),
write(' '),
write('factorul de certitudine este '),
FC1 is integer(FC),write(FC1).


realizare_scop(not Scop,Not_FC,Istorie) :- 
	realizare_scop(Scop,FC,Istorie),
	Not_FC is - FC, !.%poate fi apelat si pt testarea premiselor, nu doar pt aflare scop
realizare_scop(Scop,FC,_) :-
	fapt(Scop,FC,_), !.%in fapt se memoreaza cunostintele ; verificam daca pt acest scop avem deja val
realizare_scop(Scop,FC,Istorie) :-
	pot_interoga(Scop,Istorie),
	!,realizare_scop(Scop,FC,Istorie).
realizare_scop(Scop,FC_curent,Istorie) :-
	fg(Scop,FC_curent,Istorie). %fg primeste scop si calculeaza fc_curent si istoric
		
fg(Scop,FC_curent,Istorie) :- %N id-ul regulii; in premise avem structurile av in Lista
	regula(N, premise(Lista),concluzie(Scop,FC)),
	demonstreaza(N,Lista,FC_premise,Istorie), % demonstreaza face produsul, ajusteaza fc-ul regulii in fct de noile info
	ajusteaza(FC,FC_premise,FC_nou),
	actualizeaza(Scop,FC_nou,FC_curent,N),
	FC_curent == 100,!.
fg(Scop,FC,_) :- 
	fapt(Scop,FC,_).

pot_interoga(av(Atr,_),Istorie) :-  %verifica daca nu s-a pus deja intrebarea legata de Atr, interogat retine atr care au deja val
	not interogat(av(Atr,_)),
	interogabil(Atr,Optiuni,Mesaj),  % optiuni si mesajul afisat/intrebarea
	interogheaza(Atr,Mesaj,Optiuni,Istorie),nl,
	asserta( interogat(av(Atr,_)) ).  % introduce in lista la inceput atr care a fost interogat

cum([]) :- %cerem cum atr val
	write('Scop? '),nl,
	write('|:'),citeste_linie(Linie),nl,
	transformare(Scop,Linie), cum(Scop).
cum(L) :- 
	transformare(Scop,L),nl, cum(Scop).
cum(not Scop) :-  %cazul pt fc negativ
	fapt(Scop,FC,Reguli),
	lista_float_int(Reguli,Reguli1),
	FC < -20,transformare(not Scop,PG), %daca e fc negativ , apelam transf ca sa afisam noi formatat
	append(PG,[a,fost,derivat,cu, ajutorul, 'regulilor: '|Reguli1],LL),
	scrie_lista(LL),nl,afis_reguli(Reguli),fail.
cum(Scop) :-
	fapt(Scop,FC,Reguli),
	lista_float_int(Reguli,Reguli1),
	FC > 20,transformare(Scop,PG),
	append(PG,[a,fost,derivat,cu, ajutorul, 'regulilor: '|Reguli1],LL),
	scrie_lista(LL),nl,afis_reguli(Reguli),
	fail.
cum(_).

afis_reguli([]).
afis_reguli([N|X]) :-
	afis_regula(N),
	premisele(N),
	afis_reguli(X).
afis_regula(N) :-
	regula(N, premise(Lista_premise),concluzie(Scop,FC)), %premise(Lista_premise) este lista de av-uri
	NN is integer(N),
	scrie_lista(['rg//',NN]),  %aici modificam pt F (afisarea demonstratiei)
	scrie_lista(['concluzie']),
	transformare2(Scop,Scop_tr), 
	append(['   '],Scop_tr,L1),
	FC1 is integer(FC),
	append(L1,['cu fact_cert//', FC1],LL),
	scrie_lista(LL),
	scrie_lista(['daca ->']),
	scrie_lista_premise(Lista_premise), %si aici tot pt F 
	nl.

scrie_lista_premise([]).
scrie_lista_premise([H|T]) :-
	transformare2(H,H_tr),
	tab(5),scrie_lista(H_tr),
	scrie_lista_premise(T).

transformare2(av(A,da),[A]) :- !.  %definim transformare2 unde modificam in functie de regulile noastre 
transformare2(not av(A,da), ['nu','(',A,')']) :- !.
transformare2(av(A,V),['+',A,'=','?',V]).
	
transformare(av(A,da),[A]) :- !.
transformare(not av(A,da), [not,A]) :- !.
transformare(av(A,nu),[not,A]) :- !.
transformare(av(A,V),[A,este,V]).

premisele(N) :-
	regula(N, premise(Lista_premise), _),
	!, cum_premise(Lista_premise).
			
cum_premise([]).
cum_premise([Scop|X]) :-
	cum(Scop),
	cum_premise(X).
		
% AM adaugat nu_stiu si nu_conteaza
interogheaza(Atr,Mesaj,[da,nu],Istorie) :-
	!,write(Mesaj),
	append([da],[nu],Opt1),   
	append(Opt1,[nu_stiu],Opt2),
	append(Opt2,[nu_conteaza],Opt),
	scrie_lista(Opt),
	nl, %afis intrebarea
	de_la_utiliz(X,Istorie,[da,nu,nu_stiu,nu_conteaza]), 
	det_val_fc(X,Val,FC),
	asserta( fapt(av(Atr,Val),FC,[utiliz]) ).

interogheaza(Atr,Mesaj,Optiuni,Istorie) :-
	write(Mesaj),nl,
	citeste_opt(VLista,Optiuni,Istorie),
	assert_fapt(Atr,VLista).


citeste_opt(X,Optiuni,Istorie) :-
	append(Optiuni,[nu_stiu],Opt2),
	append(Opt2,[nu_conteaza],Opt),
	scrie_lista(Opt),
	de_la_utiliz(X,Istorie,Opt).

de_la_utiliz(X,Istorie,Lista_opt) :-
	repeat,write(': '),citeste_linie(X),
	proceseaza_raspuns(X,Istorie,Lista_opt).

proceseaza_raspuns([de_ce],Istorie,_) :-    
	nl,afis_istorie(Istorie),!,fail.

proceseaza_raspuns([X],_,Lista_opt):-
	member(X,Lista_opt).
proceseaza_raspuns([X,fc,FC],_,Lista_opt):-
	member(X,Lista_opt),float(FC).

assert_fapt(Atr,[Val,fc,FC]) :-
	!,asserta( fapt(av(Atr,Val),FC,[utiliz]) ).
assert_fapt(Atr,[Val]) :-
	asserta( fapt(av(Atr,Val),100,[utiliz])).

det_val_fc([nu],da,-100).
det_val_fc([nu,FC],da,NFC) :- 
	NFC is -FC.
det_val_fc([nu,fc,FC],da,NFC) :-
	NFC is -FC.
det_val_fc([Val,FC],Val,FC).
det_val_fc([Val,fc,FC],Val,FC).
det_val_fc([Val],Val,100).
		
afis_istorie([]) :- nl.
afis_istorie([scop(X)|T]) :-
	scrie_lista([scop,X]),!,
	afis_istorie(T).
afis_istorie([N|T]) :-
	afis_regula(N),!,afis_istorie(T).

demonstreaza(N,ListaPremise,Val_finala,Istorie) :-
	dem(ListaPremise,100,Val_finala,[N|Istorie]),!.

dem([],Val_finala,Val_finala,_).
dem([H|T],Val_actuala,Val_finala,Istorie) :-
	realizare_scop(H,FC,Istorie),
	Val_interm is min(Val_actuala,FC),
	Val_interm >= 20,
	dem(T,Val_interm,Val_finala,Istorie).
 
actualizeaza(Scop,FC_nou,FC,RegulaN) :-
	fapt(Scop,FC_vechi,_), %ia din baza de cun scopul si fc 
	combina(FC_nou,FC_vechi,FC),% daca ajung la aceeasi sol pe mai multe cai, combin fc de la cele 2 reguli 
	retract( fapt(Scop,FC_vechi,Reguli_vechi) ),
	asserta( fapt(Scop,FC,[RegulaN | Reguli_vechi]) ),!.
actualizeaza(Scop,FC,FC,RegulaN) :-
	asserta( fapt(Scop,FC,[RegulaN]) ).

ajusteaza(FC1,FC2,FC) :-
	X is FC1 * FC2 / 100,
	FC is round(X).
combina(FC1,FC2,FC) :-
	FC1 >= 0,FC2 >= 0,
	X is FC2*(100 - FC1)/100 + FC1,
	FC is round(X).
combina(FC1,FC2,FC) :-
	FC1 < 0,FC2 < 0,
	X is - ( -FC1 -FC2 * (100 + FC1)/100),
	FC is round(X).
combina(FC1,FC2,FC) :-
	(FC1 < 0; FC2 < 0),
	(FC1 > 0; FC2 > 0),
	FCM1 is abs(FC1),FCM2 is abs(FC2),
	MFC is min(FCM1,FCM2),
	X is 100 * (FC1 + FC2) / (100 - MFC),
	FC is round(X).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Reguli + Descriere %%%%

incarca_reguli(F) :-
	retractall(interogat(_)),retractall(fapt(_,_,_)),
	retractall(scop(_)),retractall(interogabil(_,_,_)),
	retractall(regula(_,_,_)),
	see(F),incarca_regulile,seen,!.
	

incarca_regulile :-
	repeat,citeste_propozitie(L),
	proceseaza(L),L == [end_of_file],nl.

proceseaza([end_of_file]):-!.
proceseaza(L) :-
	trad(R,L,[]),assertz(R), !.	% trad va face parsarea
trad(scop(X)) --> [scop,/,X,\].  %Predicat DCG  transforma o propozitie de genul Scopul este cursa_avion  in scop(cursa_avion)
trad(interogabil(Atr,M,P)) --> 
	afiseaza(Atr,P),['atribut',/,Atr,\],lista_optiuni(M).
	
trad(regula(N,premise(Daca),concluzie(Atunci,F))) --> 
	identificator(N),atunci(Atunci,F),daca(Daca).
trad('Eroare la parsare'-L,L,_).

lista_optiuni(M) --> ['optiuni','['],lista_de_optiuni(M).
lista_de_optiuni([Element]) -->  [Element,']'].
lista_de_optiuni([Element|T]) --> [Element,/,\],lista_de_optiuni(T).

afiseaza(_,P) -->  ['intrebare',':','enunt',/,P,\].
afiseaza(P,P) -->  [].
identificator(N) --> ['regula',/,N,\].

daca(Daca) --> ['conditiile',':'],lista_premise(Daca).

lista_premise([Daca]) --> propoz(Daca).
lista_premise([Prima|Celalalte]) --> propoz(Prima),[','],lista_premise(Celalalte).

atunci(Atunci,FC) -->['concluzie'],propoz(Atunci),['f_c',/,FC,\].
atunci(Atunci,100) --> propoz(Atunci).

propoz(not av(Atr,da)) --> [/,'not',Atr,\]. 
propoz(av(Atr,Val)) --> [Atr,/,Val,\].
propoz(av(Atr,da)) --> [/,Atr,\].

incarca_descriere(F) :- 
	retractall(descriere(_,_,_,_,_)),
	see(F),incarca_descrierea,seen,!.
	
incarca_descrierea :-
repeat,citeste_descriere(L),
proceseaza(L), L==[end_of_file],nl .

citeste_descriere(L):-
 citeste_linie(Lin), 
(Lin==[end_of_file],L=Lin,!;
 Lin=['-','-'|_], L=[],!;
 citeste_descriere(RestLinii),append(Lin,RestLinii,L) ).

trad(descriere(Actor,Descriere,Site,CaleImagine,ListaGenuri)) -->
	nume_actor(Actor),
	desc(Descriere),
	site(Site),
	cale(CaleImagine),
	lista_premise_desc(ListaGenuri).

nume_actor(Actor) --> ['(','actor',/,/,/,Actor,'descriere'].

desc(Descriere) --> [/,/,/,'[',Descriere,']'].

site(Site) --> ['site',/,/,/,Site,'imagine'].

cale(Cale) --> [/,/,/,Cale,'genuri',':'].


lista_premise_desc([Gen]) --> propoz_desc(Gen),[')']. 
lista_premise_desc([Prima|Celalalte]) --> propoz_desc(Prima),[';'],lista_premise_desc(Celalalte).

propoz_desc(lista_genuri(Gen)) --> ['(',Gen,')'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

citeste_linie([Cuv|Lista_cuv]) :-
	get_code(Car),  %get_code citeste codul ascii, calculeaza cuvantul si il pune in Cuv si Car1 e car imediat dupa 
	citeste_cuvant(Car, Cuv, Car1), 
	rest_cuvinte_linie(Car1, Lista_cuv). 
	  
% -1 este codul ASCII pt EOF

rest_cuvinte_linie(-1, []):-!.    
rest_cuvinte_linie(Car,[]) :-(Car==13;Car==10), !.
rest_cuvinte_linie(Car,[Cuv1|Lista_cuv]) :-
citeste_cuvant(Car,Cuv1,Car1),      
rest_cuvinte_linie(Car1,Lista_cuv).

citeste_propozitie([Cuv|Lista_cuv]) :-
	get_code(Car),citeste_cuvant(Car, Cuv, Car1), 
	rest_cuvinte_propozitie(Car1, Lista_cuv). 
	 
rest_cuvinte_propozitie(-1, []):-!.    
rest_cuvinte_propozitie(Car,[]) :-Car==46, !.
rest_cuvinte_propozitie(Car,[Cuv1|Lista_cuv]) :-
	citeste_cuvant(Car,Cuv1,Car1),      
	rest_cuvinte_propozitie(Car1,Lista_cuv).

citeste_cuvant(-1,end_of_file,-1):-!.
citeste_cuvant(Caracter,Cuvant,Caracter1) :-   
	caracter_cuvant(Caracter),!, 
	name(Cuvant, [Caracter]),get_code(Caracter1). %name obtine din codul ascii caracterul , get_code ia codul ascii al urm caracter
citeste_cuvant(Caracter, Numar, Caracter1) :-
	caracter_numar(Caracter),!,
	citeste_tot_numarul(Caracter, Numar, Caracter1). 

citeste_tot_numarul(Caracter,Numar,Caracter1):-
	determina_lista(Lista1,Caracter1),
	append([Caracter],Lista1,Lista),
	transforma_lista_numar(Lista,Numar). %echivalent cu number_codes, creaza numarul

determina_lista(Lista,Caracter1):-
	get_code(Caracter), 
	(caracter_numar(Caracter),
	determina_lista(Lista1,Caracter1),
	append([Caracter],Lista1,Lista); 
	\+(caracter_numar(Caracter)),
	Lista=[],Caracter1=Caracter). 

transforma_lista_numar([],0).
transforma_lista_numar([H|T],N):-
	transforma_lista_numar(T,NN), 
	lungime(T,L), Aux is exp(10,L),
	HH is H-48,N is HH*Aux+NN.

lungime([],0).
lungime([_|T],L):-
	lungime(T,L1),
	L is L1+1.

tab(N):- N>0,write(' '), N1 is N-1, tab(N1).
tab(0).

% 39 este codul ASCII pt '


citeste_cuvant(Caracter,Cuvant,Caracter1) :-
	Caracter==39,!,
	pana_la_urmatorul_apostrof(Lista_caractere),
	L=[Caracter|Lista_caractere],
	name(Cuvant, L),get_code(Caracter1).        

pana_la_urmatorul_apostrof(Lista_caractere):-
	get_code(Caracter),
	(Caracter == 39,Lista_caractere=[Caracter];
	Caracter\==39,
	pana_la_urmatorul_apostrof(Lista_caractere1),
	Lista_caractere=[Caracter|Lista_caractere1]).

citeste_cuvant(Caracter,Cuvant,Caracter1) :-          
	caractere_in_interiorul_unui_cuvant(Caracter),!,              
	((Caracter>64,Caracter<91),!,
	Caracter_modificat is Caracter+32;
	Caracter_modificat is Caracter),                              
	citeste_intreg_cuvantul(Caractere,Caracter1),
	name(Cuvant,[Caracter_modificat|Caractere]).        

citeste_intreg_cuvantul(Lista_Caractere,Caracter1) :-
	get_code(Caracter),
	(caractere_in_interiorul_unui_cuvant(Caracter),
	((Caracter>64,Caracter<91),!, 
	Caracter_modificat is Caracter+32;
	Caracter_modificat is Caracter),
	citeste_intreg_cuvantul(Lista_Caractere1, Caracter1),
	Lista_Caractere=[Caracter_modificat|Lista_Caractere1]; \+(caractere_in_interiorul_unui_cuvant(Caracter)),
	Lista_Caractere=[], Caracter1=Caracter).

citeste_cuvant(_,Cuvant,Caracter1) :-                
get_code(Caracter),       
citeste_cuvant(Caracter,Cuvant,Caracter1). 

caracter_cuvant(C):-member(C,[44,59,58,63,33,46,41,40,47,92,95,93,91]).

% am specificat codurile ASCII pentru , ; : ? ! . ) (
% Am adaugat in plus codurile pentru parsare : reguli,intrebari / \ _ ] [  si descriere (sunt deja mentionate de la inceput)

caractere_in_interiorul_unui_cuvant(C):-
	C>64,C<91;C>47,C<58;
	C==45;C==95;C>96,C<123.
caracter_numar(C):-C<58,C>=48.
