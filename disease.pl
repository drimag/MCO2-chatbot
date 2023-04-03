diseases([dengue, pneumonia, acs, tuberculosis, uti, hypertension, scabies, asthma]).

disease(Disease) :-
    diseases(Diseases),
    member(Disease, Diseases).

symptoms_dengue([cough, fever, m_j_pain, nausea_vomit, runny_nose, dengue_rash]).
symptoms_pneumonia([cough, fever, m_j_pain, nausea_vomit, runny_nose, crackels]).
symptoms_acs([heaviness, chest_pain, doom, pain_l_s, acs_sweat]).
symptoms_tuberculosis([cough, cough_blood, night_sweats, fever, chest_pain, fatigue, long_time]).
symptoms_uti([pain_urine, f_u_urine, inc_voiding, blood_urine, pain_uti, fever_chills]).
symptoms_hypertension([long_bp]).
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
    setof(S, D^(disease(D), symptom_of(D, S)), Symptoms),
    member(Symptom, Symptoms).

:- dynamic patient_symptoms/1.
patient_symptoms([cough, fever, m_j_pain, nausea_vomit, runny_nose, dengue_rash]).

have_symptom(Symptom) :-
    patient_symptoms(Symptoms),
    symptom(Symptom),
    member(Symptom,Symptoms).

possible(Disease) :-
    disease(Disease),
    setof(Symptom, symptom_of(Disease, Symptom), Symptoms),
    patient_symptoms(PatientSymptoms),
    subset(PatientSymptoms,Symptoms).

diagnose(Disease) :-
    possible(Disease),
    \+ ((possible(OtherDisease), OtherDisease \= Disease)), !.

add_patient_symptom(Symptom) :-
    patient_symptoms(Symptoms),
    append(Symptoms, [Symptom], NewSymptoms),
    retract(patient_symptoms(Symptoms)),
    assertz(patient_symptoms(NewSymptoms)).

interview_over() :-
    disease(Disease),
    aggregate_all(count, possible(Disease), Count),
    Count == 1, !.

interview(Symptom) :-
    format('do you have ~w? (y/n)~n', [Symptom]),
    read(Answer),
    Answer == y -> add_patient_symptom(Symptom).

