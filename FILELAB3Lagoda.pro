implement main
    open core, stdio, file

domains
    medicine = string.
    pharmacy = string.
    patient = string.
    prescription = string.

class predicates
    print_list : (string*) nondeterm.
    print_list : (main::pharmacyDb*) nondeterm.
    find_patients_by_pharmacy : (pharmacy Pharmacy, main::pharmacyDb* Patients) nondeterm.
    find_prescriptions_by_patient : (patient Patient, main::pharmacyDb* Prescriptions) nondeterm.
    count_medicines_in_pharmacy : (pharmacy Pharmacy, main::pharmacyDb* MedicineList, integer Count) determ.
    average_prescription_length : (main::pharmacyDb* Prescriptions, real Average) determ.

clauses
    print_list([]).
    print_list([X | Y]) :-
        write(X),
        nl,
        print_list(Y).

    find_patients_by_pharmacy(Pharmacy, Patients) :-
        Patients = [patient(Name, Age, Gender) || patient(Name, Age, Gender) : main::patient(Name, Age, Gender), main::pharmacy(Pharmacy, _) ].

    find_prescriptions_by_patient(Patient, Prescriptions) :-
        Prescriptions = [prescription(Name, Dosage, Duration) || prescription(Name, Dosage, Duration) : main::prescription(Name, Dosage, Duration), main::patient(Patient, _, _) ].

    count_medicines_in_pharmacy(Pharmacy, MedicineList, Count) :-
        Count = length([ medicine(Name) || medicine(Name) in MedicineList, main::pharmacy(Pharmacy, _) ]).

    average_prescription_length(Prescriptions, Average) :-
        Average = laverage([ Duration || prescription(_, _, Duration) in Prescriptions ]).

clauses
    run() :-
        file::consult("facts.pl", pharmacyDb),
        fail.
    run() :-
        write("Patients for pharmacy:"),
        nl,
        find_patients_by_pharmacy("ABC Pharmacy", Patients),
        print_list(Patients),
        nl,
        write("Prescriptions for patient:"),
        nl,
        find_prescriptions_by_patient("John", Prescriptions),
        print_list(Prescriptions),
        nl,
        write("Count of medicines in pharmacy:"),
        nl,
        count_medicines_in_pharmacy("XYZ Pharmacy", [ medicine(_) || medicine(_) ], Count),
        write(Count),
        nl,
        write("Average prescription length:"),
        nl,
        average_prescription_length([ prescription(_, _, Duration) || prescription(_, _, Duration) ], Average),
        write(Average),
        nl,
        fail.
    run() :-
        stdio::write("End test\n").

end implement main

goal
    console::runUtf8(main::run).
