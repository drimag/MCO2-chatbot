:- use_module(library(random)).

% declaring the diseases and symptoms
diseases([dengue, pneumonia, acs, tuberculosis, uti, hypertension, scabies, asthma, acgas, heatstroke]).

disease(Disease) :-
    diseases(Diseases),
    member(Disease, Diseases).

symptoms_dengue([cough, fever, m_j_pain, nausea_vomit, runny_nose, dengue_rash]).
symptoms_pneumonia([cough, fever, m_j_pain, nausea_vomit, runny_nose, crackels, shortness_breath]).
symptoms_acs([heaviness, chest_pain, doom, pain_l_s, acs_sweat]).
symptoms_tuberculosis([cough, cough_blood, night_sweats, fever, chest_pain, fatigue]).
symptoms_uti([pain_urine, f_u_urine, inc_voiding, blood_urine, pain_uti, fever_chills]).
symptoms_hypertension([headache, dizziness, shortness_breath, high_bp]).
symptoms_scabies([rash_scabies, itches, burrows, sores, crusts]).
symptoms_asthma([shortness_breath, wheezing, persistent_cough, chest_tight]).
symptoms_acgas([diarrhea, fever, cramps, dehydration, nausea_vomit]).
symptoms_heatstroke([high_body_temp, headache, dizziness, nausea_vomit, disorientation]).

mins_dengue([fever, m_j_pain, dengue_rash]).
mins_acs([chest_pain, pain_l_s]).
mins_pneumonia([cough, fever, crackels]).
mins_tuberculosis([cough, fever, night_sweats]).
mins_uti([pain_urine, f_u_urine]).
mins_hypertension([high_bp]).
mins_scabies([itches, rash_scabies]).
mins_asthma([wheezing, shortness_breath]).
mins_acgas([diarrhea]).
mins_heatstroke([high_body_temp, disorientation]).


symptom_of(dengue, Symptom) :-
    symptoms_dengue(Symptoms),
    member(Symptom,Symptoms).
symptom_of(pneumonia, Symptom) :-
    symptoms_pneumonia(Symptoms),
    member(Symptom,Symptoms).
symptom_of(acs, Symptom) :-
    symptoms_acs(Symptoms),
    member(Symptom,Symptoms).
symptom_of(tuberculosis, Symptom) :-
    symptoms_tuberculosis(Symptoms),
    member(Symptom,Symptoms).
symptom_of(uti, Symptom) :-
    symptoms_uti(Symptoms),
    member(Symptom,Symptoms).
symptom_of(hypertension, Symptom) :-
    symptoms_hypertension(Symptoms),
    member(Symptom,Symptoms).
symptom_of(scabies, Symptom) :-
    symptoms_scabies(Symptoms),
    member(Symptom,Symptoms).
symptom_of(asthma, Symptom) :-
    symptoms_asthma(Symptoms),
    member(Symptom,Symptoms).
symptom_of(acgas, Symptom) :-
    symptoms_acgas(Symptoms),
    member(Symptom,Symptoms).
symptom_of(heatstroke, Symptom) :-
    symptoms_heatstroke(Symptoms),
    member(Symptom,Symptoms).

min_symptom_of(dengue, Symptom) :-
    mins_dengue(Symptoms),
    member(Symptom,Symptoms).
min_symptom_of(pneumonia, Symptom) :-
    mins_pneumonia(Symptoms),
    member(Symptom,Symptoms).
min_symptom_of(acs, Symptom) :-
    mins_acs(Symptoms),
    member(Symptom,Symptoms).
min_symptom_of(tuberculosis, Symptom) :-
    mins_tuberculosis(Symptoms),
    member(Symptom,Symptoms).
min_symptom_of(uti, Symptom) :-
    mins_uti(Symptoms),
    member(Symptom,Symptoms).
min_symptom_of(hypertension, Symptom) :-
    mins_hypertension(Symptoms),
    member(Symptom,Symptoms).
min_symptom_of(scabies, Symptom) :-
    mins_scabies(Symptoms),
    member(Symptom,Symptoms).
min_symptom_of(asthma, Symptom) :-
    mins_asthma(Symptoms),
    member(Symptom,Symptoms).
min_symptom_of(acgas, Symptom) :-
    mins_acgas(Symptoms),
    member(Symptom,Symptoms).
min_symptom_of(heatstroke, Symptom) :-
    mins_heatstroke(Symptoms),
    member(Symptom,Symptoms).

symptom(Symptom) :-
    setof(Symp, Disease^(disease(Disease), symptom_of(Disease, Symp)), Symptoms),
    member(Symptom, Symptoms).

symptoms(Symptoms) :-
    setof(Symptom, symptom(Symptom), Symptoms).



% definitions for when a disease is likely/ when the system will diagnose a disease

hi_sus(dengue) :-
    mins_possible(dengue),
    (have_trisk(turniquet); have_trisk(close_contact)).

hi_sus(acs) :-
    have_symptom(chest_pain),
    have_symptom(heaviness),
    (
        at_least( 3,
                [
                 have_trisk(smoking),
                 have_trisk(obesity),
                 have_trisk(high_bp),
                 have_trisk(high_cholesterol),
                 have_trisk(family_history)
                ]
                );
        have_trisk(old)
    ).

hi_sus(tuberculosis) :-
    mins_possible(tuberculosis),
    have_trisk(long_time).

hi_sus(uti) :-
    mins_possible(uti).

hi_sus(scabies) :-
    mins_possible(scabies),
    have_trisk(close_contact).

hi_sus(asthma) :-
    mins_possible(asthma).

hi_sus(pneumonia) :-
    mins_possible(pneumonia).

hi_sus(hypertension) :-
    mins_possible(hypertension),
    have_trisk(monitor_bp).

hi_sus(heatstroke) :-
    mins_possible(heatstroke).

hi_sus(acgas) :-
    mins_possible(acgas).





% patient symptoms added to this list
:- dynamic patient_symptoms/1.
patient_symptoms([]).

% symptoms the patient doesnt have
:- dynamic not_symptoms/1.
not_symptoms([]).

% patient trisks
:- dynamic patient_trisks/1.
patient_trisks([]).


% a patient has Symptom if it is in list patient_symptoms
have_symptom(Symptom) :-
    patient_symptoms(Symptoms),
    symptom(Symptom),
    member(Symptom,Symptoms).

% a patient has a Trisk if it is in list patient_trisks
have_trisk(Trisk) :-
    patient_trisks(Trisks),
    member(Trisk, Trisks).

% a patient does not have a Symptom if it is in not_symptoms
not_have_symptom(Symptom) :-
    not_symptoms(NotSymptoms),
    symptom(Symptom),
    member(Symptom, NotSymptoms).


% based on patient symptoms, declares which diseases are still possible ()
% a disease is still possible if the patient doesnt have any symptoms not of the symptoms of that disease
possible(Disease) :-
    disease(Disease),
    setof(Symptom, symptom_of(Disease, Symptom), Symptoms),
    patient_symptoms(PatientSymptoms),
    subset(PatientSymptoms,Symptoms).

% declares based on patient symptoms if a patient has the minimum symptoms for a disease to be diagnosed
mins_possible(Disease) :-
    disease(Disease),
    setof(Symptom, min_symptom_of(Disease, Symptom), MinSymptoms),
    patient_symptoms(PatientSymptoms),
    subset(MinSymptoms,PatientSymptoms).

% just puts the possible diseases into a list
possible_diseases(Diseases) :-
    setof(Disease, possible(Disease), Diseases).

% puts the mins diseases in a list
mins_diseases(Diseases) :-
    setof(Disease, mins_possible(Disease), Diseases).


% based on possible diseases, returns the common symptoms
common_symptoms(Symptoms) :-
    possible_diseases(Diseases),
    findall(DiseaseSymptoms, (member(Disease, Diseases), findall(Symptom, symptom_of(Disease, Symptom), DiseaseSymptoms)), AllSymptoms),
    intersect_lists(AllSymptoms,Symptoms).

% based on possible diseases, returns the possible symptoms, excludes
% cant be symptoms
all_possible_symptoms(Symptoms) :-
    possible_diseases(Diseases),
    not_symptoms(NotSymptoms),
    findall(DiseaseSymptoms, (member(Disease, Diseases), findall(Symptom, (symptom_of(Disease, Symptom), not(member(Symptom, NotSymptoms))), DiseaseSymptoms)), AllSymptoms),
    flatten(AllSymptoms, Symptoms1),
    list_to_set(Symptoms1, Symptoms).

% intersection of lists
intersect_lists([], []).
intersect_lists([L|Ls], Intersection) :-
    foldl(intersection, Ls, L, Intersection).


% adds a symptom to the list patient_symptoms
add_patient_symptom(Symptom) :-
    patient_symptoms(Symptoms),
    append(Symptoms, [Symptom], NewSymptoms),
    retract(patient_symptoms(Symptoms)),
    assertz(patient_symptoms(NewSymptoms)).

add_patient_trisk(Trisk) :-
    patient_trisks(PatientTrisks),
    append(PatientTrisks, [Trisk], NewTrisks),
    retract(patient_trisks(PatientTrisks)),
    assertz(patient_trisks(NewTrisks)).

add_not_symptom(Symptom) :-
    not_symptoms(NotSymptoms),
    append(NotSymptoms, [Symptom], NewSymptoms),
    retract(not_symptoms(NotSymptoms)),
    assertz(not_symptoms(NewSymptoms)).

count_unasked_symptoms(AllSymptoms, PatientSymptoms, NotSymptoms, Count) :-
    append(PatientSymptoms, NotSymptoms, AskedSymptoms),
    findall(Symptom, (member(Symptom, AllSymptoms), \+ member(Symptom, AskedSymptoms)), UnaskedSymptoms),
    length(UnaskedSymptoms, Count).

at_least(0, _) :- !.
at_least(N, [H|T]) :-
    (call(H), N1 is N-1; N1 = N),
    at_least(N1, T).

% asking the user for a particular Symptom
ask(Symptom) :-
    (
        Symptom == cough ->
        format('do you have a cough? (y/n)~n');

        Symptom == fever ->
        format('do you have a fever? (y/n)~n');

        Symptom == m_j_pain ->
        format('do you have muscle or joint pain? (y/n)~n');

        Symptom == nausea_vomit ->
        format('are you experiencing nausa, or do you feel like vomiting? (y/n)~n');

        Symptom == runny_nose ->
        format('do you have a runny nose? (y/n)~n');

        Symptom == dengue_rash ->
        format('with the help of a nurse, do you have the rash associated with dengue? (y/n)~n');

        Symptom == crackels ->
        format('with the help of a nurse, do you have crackels? (y/n)~n');

        Symptom == shortness_breath ->
        format('are you experiencing shortness of breath? (y/n)~n');

        Symptom == heaviness ->
        format('do you a feeling of heaviness? (y/n)~n');

        Symptom == chest_pain ->
        format('do you have chest pain? (y/n)~n');

        Symptom == doom ->
        format('do you have a feeling of impending doom? (y/n)~n');

        Symptom == pain_l_s ->
        format('are you experiencing pain or discomfort in the left arm, shoulder, jaw, or neck? (y/n)~n');

        Symptom == acs_sweat ->
        format('are you sweating more than normal? (y/n)~n');

        Symptom == cough_blood ->
        format('are you coughing blood? (y/n)~n');

        Symptom == night_sweats ->
        format('do you have night sweats? (y/n)~n');

        Symptom == fatigue ->
        format('are you experiencing fatigue? (y/n)~n');

        Symptom == cough_blood ->
        format('are you coughing blood? (y/n)~n');

        Symptom == pain_urine ->
        format('is it painful to urinate? (y/n)~n');

        Symptom == f_u_urine ->
        format('do you have frequent or urgent urination? (y/n)~n');

        Symptom == inc_voiding ->
        format('is there a sense of incomplete voiding? (y/n)~n');

        Symptom == blood_urine ->
        format('is there blood in your urine? (y/n)~n');

        Symptom == fever_chills ->
        format('do you have a fever and chills? (y/n)~n');

        Symptom == dizziness ->
        format('do you feel dizzy? (y/n)~n');

        Symptom == rash_scabies ->
        format('with the help of a nurse, do you have the rash associated with scabies? (y/n)~n');

        Symptom == itches ->
        format('do you have itches? (y/n)~n');

        Symptom == burrows ->
        format('with the help of a nurse, are there burrows in your skin? (y/n)~n');

        Symptom == sores ->
        format('are there sores on your skin? (y/n)~n');

        Symptom == crusts ->
        format('are there crusts on your skin? (y/n)~n');

        Symptom == wheezing ->
        format('with the help of a nurse, do you have wheezing? (y/n)~n');

        Symptom == persistent_cough ->
        format('do you have persistent cough? (y/n)~n');

        Symptom == chest_tight ->
        format('is there a tightness in your chest? (y/n)~n');

        Symptom == diarrhea ->
        format('do you have diarrhea? (y/n)~n');

        Symptom == cramps ->
        format('do you have cramps? (y/n)~n');

        Symptom == dehydration ->
        format('are you feeling dehydrated? (y/n)~n');

        Symptom == high_body_temp ->
        format('with the help of a nurse, do you have a high body temperature? (y/n)~n');

        Symptom == disorientation ->
        format('do you have a feeling of disorientation? (y/n)~n')

    ),

    read(Answer),

    (   Answer == y -> add_patient_symptom(Symptom) ;
    Answer == n -> add_not_symptom(Symptom) ; true).


ask_trisk(Trisk) :-
    (
        Trisk == turniquet ->
        format('with the help of a nurse, perform the turniquet test. does the tests suggest that you have dengue? (y/n)~n');

        Trisk == close_contact ->
        format('have you had close contact with others with similar symptoms? (y/n)~n');

        Trisk == old ->
        format('are you older than 45? (y/n)~n');

        Trisk == long_time ->
        format('have you been experiencing your symptoms for more than 2 weeks? (y/n)~n');

        Trisk == monitor_bp ->
        format('with the help of a nurse, have you had consistent high blood pressure? (y/n)~n');

        Trisk == smoking ->
        format('do you smoke? (y/n)~n');

        Trisk == family_history ->
        format('does your family have a history of heart problems? (y/n)~n');

        Trisk == obesity ->
        format('with the help of a nurse, are you considered obese? (y/n)~n');

        Trisk == high_bp ->
        format('with the help of a nurse, do you have high blood pressure? (y/n)~n');

        Trisk == high_cholesterol ->
        format('with the help of a nurse, do you have high cholesterol? (y/n)~n')
    ),

    read(Answer),
    (Answer == y -> add_patient_trisk(Trisk) ; true).


ask_trisks_acs([]).
ask_trisks_acs([H|T]) :-
    \+ at_least(3, [have_trisk(smoking), have_trisk(obesity), have_trisk(high_bp), have_trisk(high_cholesterol), have_trisk(family_history)]),
    ask_trisk(H),
    ask_trisks_acs(T).


ask_trisks(Disease) :-
    Disease == dengue -> (
                   ask_trisk(turniquet),
                   (
                       have_trisk(turniquet) -> true ;
                       ask_trisk(close_contact)
                   )
               );
    Disease == tuberculosis -> ask_trisk(long_time);
    Disease == hypertension -> ask_trisk(monitor_bp);
    Disease == scabies -> ask_trisk(close_contact);
    Disease == acs -> (
                   ask_trisk(old),
                   (
                       have_trisk(old) -> true ;

                       ask_trisks_acs([smoking,
                                       family_history,
                                       obesity,
                                       high_bp,
                                       high_cholesterol]
                                     )
                   )
               ).



% condition for ending the symptoms interview, may add more conditions
pi_not_over() :-
    (
        possible_diseases(PossibleDiseases),
        length(PossibleDiseases, Count),
        Count > 1
    ),
    (
        symptoms(AllSymptoms),
        patient_symptoms(PatientSymptoms),
        not_symptoms(NotSymptoms),
        count_unasked_symptoms(AllSymptoms, PatientSymptoms, NotSymptoms, CountUA),
        CountUA > 0
    ),
    (
        all_possible_symptoms(PossibleSymptoms),
        common_symptoms(CommonSymptoms),
        subtract(PossibleSymptoms, CommonSymptoms, SymptomsOI),
        length(SymptomsOI, CountSOI),
        CountSOI > 0
    ).

% loop for asking for symptoms
possible_interview() :-
    all_possible_symptoms(PossibleSymptoms),
    common_symptoms(CommonSymptoms),
    subtract(PossibleSymptoms, CommonSymptoms, SymptomsOI),
    random_member(SymptomOI, SymptomsOI),
    ask(SymptomOI),
    pi_not_over() -> possible_interview() ; true.


ask_list([]).
ask_list([H|T]) :-
    ask(H),
    ask_list(T).

scan_mins(Disease) :-
     possible(Disease),
     setof(Symptom, min_symptom_of(Disease, Symptom), MinSymptoms),
     patient_symptoms(PatientSymptoms),
     subtract(MinSymptoms, PatientSymptoms, AskSymptoms),
     ask_list(AskSymptoms).

scan_trisks(Disease) :-
    mins_possible(Disease),
    ask_trisks(Disease).

trisks_interview([]).
trisks_interview([H|T]) :-
    scan_mins(H),
    scan_trisks(H),
    trisks_interview(T).


% loop for asking for trisks (test/risks)

suggest(Diseases) :-
    length(Diseases, Length),
    (
        Length == 0 ->
        write("We are not able to diagnose you with any diseases based on your symptoms and risk factors, we suggest getting tested at an appropriate medical facility")
        ;
        format("We are not able to diagnose you with any diseases based on your symptoms and risk factors, but we suggest that you get tested for ~w in an appropriate medical facility.", [Diseases])
     ).

diagnose(Disease) :-
    format("We diagnose you with ~s", [Disease]).

interview() :-
     possible_interview(),
     possible_diseases(PossibleDiseases),
     (trisks_interview(PossibleDiseases) ; true),
     possible_diseases(PossibleDiseases),
     (
         ( possible(Disease), hi_sus(Disease) ) ->
         diagnose(Disease); suggest(PossibleDiseases)
     ).



undo :-
    retractall(patient_symptoms(_)),
    retractall(not_symptoms(_)),
    retractall(patient_trisks(_)).
