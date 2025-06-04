type Member = { 
    Name: string 
    MembershipType: string    // z.B. "Basic", "Premium", "Student" 
    MonthlyFee: float 
    MonthsActive: int 
    PersonalTraining: bool 
} 

let members = [ 
    { Name = "Anna"; MembershipType = "Premium"; MonthlyFee = 100.0; MonthsActive = 14; PersonalTraining = true } 
    { Name = "Ben"; MembershipType = "Student"; MonthlyFee = 55.0; MonthsActive = 9; PersonalTraining = false } 
    { Name = "Carla"; MembershipType = "Basic"; MonthlyFee = 70.0; MonthsActive = 18; PersonalTraining = false } 
    { Name = "Daniel"; MembershipType = "Premium"; MonthlyFee = 100.0; MonthsActive = 6; PersonalTraining = true } 
    { Name = "Elena"; MembershipType = "Basic"; MonthlyFee = 70.0; MonthsActive = 24; PersonalTraining = false } 
]

let calculateJahresBetrag m =
    m.MonthlyFee * 12.0
    
let getNameGesamtbetrag members =
    members |> List.map (fun m -> (m.Name, calculateJahresBetrag m))
    
printfn $"Jahres Betrag der Members {getNameGesamtbetrag members}"

let getMembersWithPT members =
    members |> List.filter (fun m -> m.PersonalTraining = true)

let countMembersWithPT members =
    members |> getMembersWithPT |> List.length

printfn $"Members with PT {getMembersWithPT members}"
printfn $"Members with PT count {countMembersWithPT members}"

let groupByAboTyp members =
    members |> List.groupBy (fun x -> x.MembershipType)
    
printfn $"GroupBy Membershiptype {groupByAboTyp members}"

let getAVGByMonthlyFee members =
    members |> List.averageBy (fun m -> m.MonthlyFee) 

let getAVGMonthlyFeePerAboTyp members =
    members |> groupByAboTyp |> List.map (fun (membershipType, members) ->  (membershipType, getAVGByMonthlyFee members))

printfn $"Get AVG MonthlyFee Per Abo Typ {getAVGMonthlyFeePerAboTyp members}"

let increaseMonthlyFee members percentInFloat =
    members |> List.map (fun membere -> {membere with MonthlyFee = membere.MonthlyFee * percentInFloat})
    
let calculateGesamtBezahlt membere =
    float membere.MonthsActive * membere.MonthlyFee

let getAllMembersIncreasedMonthlyFee members =
    increaseMonthlyFee members 1.05 |> List.map (fun m -> (m.Name, calculateGesamtBezahlt m))

printfn $"Get All Members increased Monthly Fee to 5 Percent More {getAllMembersIncreasedMonthlyFee members}"
    
let getIncreasedMonthlyFeeAboTypAVG members =
    increaseMonthlyFee members 1.05 |> getAVGMonthlyFeePerAboTyp

printfn $"Get increased AboType Monthly Fee AVG: {getIncreasedMonthlyFeeAboTypAVG members}"