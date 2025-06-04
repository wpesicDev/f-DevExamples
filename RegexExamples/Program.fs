open System.Text.RegularExpressions

let regex2: Regex = Regex("^\\d{2,4}$") //String mit 2 bis 4 Zahlen

let testRegex (v: string) : string =
    match v with
    | v when regex2.IsMatch(v) -> "match"
    | _ -> "no match"
    
printfn $"""{testRegex "123"}""" // Ausgabe: "match"
printfn $"""{testRegex "1"}""" // Ausgabe: "no match"
printfn $"""{testRegex "12345"}""" // Ausgabe: "no match"


let isValidEmail (email: string) =
    let pattern = @"^[a-zA-Z0-9]{2,}.[a-zA-Z0-9]+@[a-zA-Z0-9]+\.(com|ch)$"
    Regex.IsMatch(email, pattern)

printfn "%b" (isValidEmail "xx.xx@xx.com") // true
printfn "%b" (isValidEmail "xx.xx@xx.ch")  // true
printfn "%b" (isValidEmail "x@xx.com")     // false
printfn "%b" (isValidEmail "xx@xx.ch")     // false


// Funktion zur Prüfung der Telefonnummer
let isValidPhoneNumber (phoneNumber: string) =
    let pattern = @"^\+41 \d{2} \d{3} \d{2} \d{2}$"
    Regex.IsMatch(phoneNumber, pattern)

// Funktion zur Filterung von Kontakten mit ungültigen Telefonnummern (Tuple)
let filterInvalidContacts contacts =
    contacts |> List.filter (fun (_, _, phone) -> not (isValidPhoneNumber phone))

// Tests
let contactsTuple = [
    ("Hans", "Meier", "+41 79 123 12 12");
    ("Anna", "Müller", "12345");
    ("Peter", "Schmidt", "+41 78 456 78 90")
]
let invalidContactsTuple = filterInvalidContacts contactsTuple
printfn "%A" invalidContactsTuple // [("Anna", "Müller", "12345")]


// Record-Typ für Kontakte
type Contact = { FirstName: string; LastName: string; PhoneNumber: string }

// Funktion zur Filterung von Kontakten mit ungültigen Telefonnummern (Record)
let filterInvalidContactsRecord contacts =
    contacts |> List.filter (fun contact -> not (isValidPhoneNumber contact.PhoneNumber))

// Tests
let contactsRecord = [
    { FirstName = "Hans"; LastName = "Meier"; PhoneNumber = "+41 79 123 12 12" };
    { FirstName = "Anna"; LastName = "Müller"; PhoneNumber = "12345" };
    { FirstName = "Peter"; LastName = "Schmidt"; PhoneNumber = "+41 78 456 78 90" }
]
let invalidContactsRecord = filterInvalidContactsRecord contactsRecord
printfn "%A" invalidContactsRecord // [{ FirstName = "Anna"; LastName = "Müller"; PhoneNumber = "12345" }]
