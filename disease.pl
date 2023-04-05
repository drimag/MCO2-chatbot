% declaring the diseases and symptoms
diseases([dengue, pneumonia, acs, tuberculosis, uti, hypertension, scabies, asthma]).

disease(Disease) :-
    diseases(Diseases),
    member(Disease, Diseases).

symptoms_dengue([cough, fever, m_j_pain, nausea_vomit, runny_nose, dengue_rash]).
symptoms_pneumonia([cough, fever, m_j_pain, nausea_vomit, runny_nose, crackels]).
symptoms_acs([heaviness, chest_pain, doom, pain_l_s, acs_sweat]).
symptoms_tuberculosis([cough, cough_blood, night_sweats, fever, chest_pain, fatigue, long_time]).
symptoms_uti([pain_urine, f_u_urine, inc_voiding, blood_urine, pain_uti, fever_chills]).
symptoms_hypertension([long_bp, nape_pain, easily_tired]).
symptoms_scabies([rash_scabies, itches, burrows, sores, crusts]).
symptoms_asthma([hard_breathing, shortness_breath, wheezing]).

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

symptom(Symptom) :-
    setof(Symp, Disease^(disease(Disease), symptom_of(Disease, Symp)), Symptoms),
    member(Symptom, Symptoms).
%
% end of declaring diseases and symptoms
%

% helper, determines if a certain number of conditions is true or not.

at_least(0, _) :- !.
at_least(N, [H|T]) :-
    (call(H), N1 is N-1; N1 = N),
    at_least(N1, T).

% definitions for when a disease is likely
have(flu_like) :-
    at_least(3, [have_symptom(cough), have_symptom(fever), have_symptom(m_j_pain), have_symptom(nausea_vomit), have_symptom(runny_nose)]).


% likely(dengue) :-
%    have(flu_like),

likely(acs) :-
    have_symptom(chest_pain),
    have_symptom(heaviness),
    (risky(acs); risk(old)).

% likely(tuberculosis) :-
%    have_symptom(long_time),

likely(uti) :-
    have_symptom(pain_urine);
    (at_least(3, [have_symptom(f_u_urine), have_symptom(inc_voiding), have_symptom(blood_urine), have_symptom(pain_uti), have_symptom(fever_chills)])).

% likely(scabies) :-
%

% likely(asthma) :-

likely(pneumonia) :-
    have(flu_like),
    have_symptom(crackels).





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

% based on patient symptoms, declares which diseases are still possible ()
possible(Disease) :-
    disease(Disease),
    setof(Symptom, symptom_of(Disease, Symptom), Symptoms),
    patient_symptoms(PatientSymptoms),
    subset(PatientSymptoms,Symptoms).

% just puts the possible diseases into a list
possible_diseases(Diseases) :-
    setof(Disease, possible(Disease), Diseases).

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

% condition for ending the interview, may add more conditions
interview_not_over() :-
    aggregate_all(count, possible(Disease), Count),
    write(Count),
    Count > 1, !.

% asking the user for a particular Symptom
ask(Symptom) :-
    format('do you have ~w? (y/n)~n', [Symptom]),
    read(Answer),
    (   Answer == y -> add_patient_symptom(Symptom) ;
    Answer == n -> add_not_symptom(Symptom) ; true).

% the whole loop for the whole thing
interview() :-
    interview_not_over(),
    all_possible_symptoms(PossibleSymptoms),
    common_symptoms(CommonSymptoms),
    subtract(PossibleSymptoms, CommonSymptoms, SymptomsOI),
    write(SymptomsOI),
    nth0(0, SymptomsOI, SymptomOI),
    ask(SymptomOI),
    interview().
