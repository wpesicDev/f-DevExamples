type Member = { 
    Name: string 
    MembershipType: string 
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

let calculateGesamtBetragName members =
    members |> List.map (fun m -> (m.Name, m.MonthlyFee * 12.0))
    
printfn $"Function a {calculateGesamtBetragName members}"

let MembersWithPersonalTraining members = [for m in members do if m.PersonalTraining = true then yield m]

let CountMembersWithPersonalTraining members =
    MembersWithPersonalTraining members |> List.length
    
printfn $"Function b {MembersWithPersonalTraining members}"
printfn $"Function print b {CountMembersWithPersonalTraining members}"

let groupAboType members =  
    members  
    |> List.groupBy (fun m -> m.MembershipType)  

printfn $"Function c group function {groupAboType members}"


let CalculateAvgMonthlyFee members =  
    let totalFee = List.sumBy (fun m -> m.MonthlyFee) members  
    let countMembers = List.length members  
    totalFee / float countMembers

let MonthlyAVGperAboType members =
    groupAboType members |> List.map (fun (MembershipType, members) -> (MembershipType, CalculateAvgMonthlyFee members))
printfn $"Avg monthlyfee {MonthlyAVGperAboType members}"

let increaseMonthlyFee members =
    members |> List.map (fun m ->  {m with MonthlyFee = m.MonthlyFee * 1.05})
    
let listNewMonthlyFee members  =
    increaseMonthlyFee members |> List.map (fun m -> (m.Name, m.MonthlyFee, m.MonthlyFee * float m.MonthsActive))
printfn $"IncreaseMonthlyFee: {increaseMonthlyFee members}"

let calculateTotalIncome members= 
    List.fold (fun acc m -> acc + (m.MonthlyFee * float m.MonthsActive)) 0.0 members 