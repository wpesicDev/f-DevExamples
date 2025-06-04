    type Student = {
        Name: string
        Martikelnummer: int
        Noten: List<float>
    }

    let members = [ 
        { Name = "Anna"
          Martikelnummer = 3
          Noten = [2.0; 4.0; 5.0] } 
        { Name = "Ben"
          Martikelnummer = 4
          Noten = [3; 6; 4] } 
        { Name = "Carla"; Martikelnummer = 5; Noten = [2; 5.3; 3] } 
        { Name = "Elena"; Martikelnummer = 55; Noten = [2; 3; 3]} 
    ]       

    let calculateAvgNoten student =
        let sum = List.average student.Noten
        sum


    // calculateMedian
    let getMedian noten =
        let sorted = List.sort noten
        let length = List.length noten
        
        if length % 2 = 0 then
            (sorted[length / 2] + sorted[(length / 2) - 1] / 2.0 )
        else
            sorted[length / 2]
        
    printfn $"median of noten {getMedian members[0].Noten}"


    let proofPassed student =
        let avg = calculateAvgNoten student
        let sumUngenugende = [for n in student.Noten do if n < 4 then yield n] |> List.sum 
        avg > 4.0 && sumUngenugende - 4.0 <= -1

    let hatErBestanden student =
        if proofPassed student then "passed" else "notpassed"
        

    printfn $"proofpassed 1 {proofPassed members[0]}"
    printfn $"proofpassed 2 {proofPassed members[1]}"
    printfn $"proofpassed 2 {hatErBestanden members[1]}"

    let allStudents students =
        printfn $"{List.map (fun student -> (student.Name, hatErBestanden student)) students}"
    allStudents members


            
