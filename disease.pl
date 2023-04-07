% declaring the diseases and symptoms
diseases([dengue, pneumonia, acs, tuberculosis, uti, hypertension, scabies, asthma]).

disease(Disease) :-
    diseases(Diseases),
    member(Disease, Diseases).

symptoms_dengue([cough, fever, m_j_pain, nausea_vomit, runny_nose, dengue_rash]).
symptoms_pneumonia([cough, fever, m_j_pain, nausea_vomit, runny_nose, crackels, shortness_breath]).
symptoms_acs([heaviness, chest_pain, doom, pain_l_s, acs_sweat, shortness_breath]).
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






%
% end of declaring diseases and symptoms
%

% definitions for when a disease is likely

hi_sus(dengue) :-
    mins_possible(dengue),
    (trisk(turniquet); trisk(close_contact)).

hi_sus(acs) :-
    have_symptom(chest_pain),
    have_symptom(heaviness),
    (risky(acs); risk(old)).

hi_sus(tuberculosis) :-
   mins_possible(tuberculosis),
   trisk(long_time).

hi_sus(uti) :-
    mins_possible(uti).

hi_sus(scabies) :-
    mins_possible(scabies),
    trisk(close_contact).

hi_sus(asthma) :-
    mins_possible(asthma).

hi_sus(pneumonia) :-
    mins_possible(pneumonia).

hi_sus(hypertension) :-
    mins_possible(hypertension),
    trisk(monitor_bp).

hi_sus(heatstroke) :-
    mins_possible(heatstroke).

hi_sus(acgas) :-
    mins_possible(acgas).





% patient symptoms added to this list
:- dynamic patient_symptoms/1.
patient_symptoms([]).

:- dynamic not_symptoms/1.
not_symptoms([]).

% but says that if a symptom is in patient symptoms then the patient has
% a given symptom
%
have_symptom(Symptom) :-
    patient_symptoms(Symptoms),
    symptom(Symptom),
    member(Symptom,Symptoms).

not_have_symptom(Symptom) :-
    not_symptoms(NotSymptoms),
    symptom(Symptom),
    member(Symptom, NotSymptoms).

% based on patient symptoms, declares which diseases are still possible ()
possible(Disease) :-
    disease(Disease),
    setof(Symptom, symptom_of(Disease, Symptom), Symptoms),
    patient_symptoms(PatientSymptoms),
    subset(PatientSymptoms,Symptoms).

mins_possible(Disease) :-
    disease(Disease),
    setof(Symptom, min_symptom_of(Disease, Symptom), MinSymptoms),
    patient_symptoms(PatientSymptoms),
    subset(MinSymptoms,PatientSymptoms).

% just puts the possible diseases into a list
possible_diseases(Diseases) :-
    setof(Disease, possible(Disease), Diseases).

% list of diseases where the patient has that minimum symptoms for it to
% be considered
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

% not really used
diagnose(Disease) :-
    possible(Disease),
    \+ ((possible(OtherDisease), OtherDisease \= Disease)), !.


% adds a symptom to the list patient_symptoms
add_patient_symptom(Symptom) :-
    patient_symptoms(Symptoms),
    append(Symptoms, [Symptom], NewSymptoms),
    retract(patient_symptoms(Symptoms)),
    assertz(patient_symptoms(NewSymptoms)).

add_not_symptom(Symptom) :-
    not_symptoms(NotSymptoms),
    append(NotSymptoms, [Symptom], NewSymptoms),
    retract(not_symptoms(NotSymptoms)),
    assertz(not_symptoms(NewSymptoms)).

count_unasked_symptoms(AllSymptoms, PatientSymptoms, NotSymptoms, Count) :-
    append(PatientSymptoms, NotSymptoms, AskedSymptoms),
    subtract(AllSymptoms, AskedSymptoms, UnaskedSymptoms),
    length(UnaskedSymptoms, Count).

% condition for ending the symptoms interview, may add more conditions
pi_not_over() :-
    possible_diseases(PossibleDiseases),
    symptoms(AllSymptoms),
    patient_symptoms(PatientSymptoms),
    not_symptoms(NotSymptoms),
    (
        length(PossibleDiseases, Count),
        Count > 1
    ) ;
    (
        count_unasked_symptoms(AllSymptoms, PatientSymptoms, NotSymptoms, CountUA),
        CountUA > 0
    ).


% asking the user for a particular Symptom
ask(Symptom) :-
    format('do you have ~w? (y/n)~n', [Symptom]),
    read(Answer),
    (   Answer == y -> add_patient_symptom(Symptom) ;
    Answer == n -> add_not_symptom(Symptom) ; true).

ask_list([]).
ask_list([H|T]) :-
    ask(H),
    ask_list(T).

scan(Disease) :-
     possible(Disease),
     setof(Symptom, min_symptom_of(Disease, Symptom), MinSymptoms),
     patient_symptoms(PatientSymptoms),
     subtract(MinSymptoms, PatientSymptoms, AskSymptoms),
     ask_list(AskSymptoms).

% loop for asking for symptoms
possible_interview() :-
    pi_not_over(),
    all_possible_symptoms(PossibleSymptoms),
    common_symptoms(CommonSymptoms),
    subtract(PossibleSymptoms, CommonSymptoms, SymptomsOI),
    write(SymptomsOI),
    nth0(0, SymptomsOI, SymptomOI),
    ask(SymptomOI),
    symptoms_interview().

% loop for asking for trisks (test/risks)
trisks_interview() :-
    possible(PossibleDisease),
    scan(PossibleDisease),

    trisks_interview().

suggest(Diseases) :-
    length(Diseases, Length),
    (
        Length == 0 ->
        write("We are not able to diagnose you with any diseases based on your symptoms and risk factors")
        ;
        format("We are not able to diagnose you with any diseases based on your symptoms and risk factors, but we suggest that you get tested for ~s in an appropriate medical facility.", [Diseases])
     ).

interview() :-
    possible_interview(),
    trisks_interview(),
    possible_diseases(PossibleDiseases),
    (
        ( possible(Disease), hi_sus(Disease) ) ->
        diagnose(Disease); suggest(PossibleDiseases)
    ).
