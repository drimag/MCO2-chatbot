% user inputs
:- dynamic(symptom/1).

symptom_of(cough, dengue).
symptom_of(fever, dengue).
symptom_of(m_j_pain, dengue).
symptom_of(nausea_vomit, dengue).
symptom_of(runny_nose, dengue).


symptom_of(cough, pneumonia).
symptom_of(fever, pneumonia).
symptom_of(m_j_pain, pneumonia).
symptom_of(nausea_vomit, pneumonia).
symptom_of(runny_nose, pneumonia).
symptom_of(crackels, pneumonia).

symptom_of(chest_pain, acs).
symptom_of(heaviness, acs).
symptom_of(acs_sweat, acs).
symptom_of(doom, acs).

disease(acs).
disease(dengue).
disease(pneumonia).

has(june, cough).
has(june, fever).
has(june, m_j_pain).
has(june, nausea_vomit).
has(june, runny_nose).
has(june, crackels).

has(june, chest_pain).
has(june, heaviness).
has(june, acs_sweat).
has(june, doom).


% testing something can just ignore
h(x).
h(a).

gender(yes) :- h(s), !.
gender(yes) :- h(x), !.
gender(yes) :- h(a), !.
gender(yes) :- h(s), h(x), h(a), !.


gender(i) :-
    gender(yes).
% end of testing

diagnose(Disease, Patient) :-
    disease(Disease),
    \+ (symptom_of(Symptom, Disease), \+ has(Patient, Symptom)).

%
% diagnose(disease(X)) :-
%    not(disease(dengue)), not(disease(acs)) -> disease(pneumonia).

