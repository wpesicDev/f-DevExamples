module TestStudentRecords
           
open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Program

[<TestClass>]
type TestClass () =
    let student = { Name = "Anna"; Martikelnummer = 3; Noten = [2.0; 4.0; 5.0; 6.0; 2.0;] } 
    [<TestMethod>]
    member this.CalculateAVGNotenTest () =
        let avg = calculateAvgNoten student
        Assert.AreEqual(avg, List.sum student.Noten / 5.0)
    
    [<TestMethod>]
    member this.CalculateMedianTests () =
        let median = getMedian student.Noten
        Assert.AreEqual(median, 4.0)
        
    [<TestMethod>]
    member this.PassedTest () =
        let median = hatErBestanden student
        Assert.AreEqual(median, "notpassed")
    