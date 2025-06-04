type Zeit = int * int * int
let timeTest1 : Zeit = (20, 3, 50)
let timeTest2 : Zeit = (1, 2, 50)
let toSeconds (h, m, s) =
    let hSek = h * 3600
    let minSek = m * 60
    hSek + minSek + s
    
let  fromSeconds sek =
    let h = sek / 3600
    let rest = sek % 3600
    
    let min = rest / 60
    let s = rest % 60
    
    h, min, s
    
let addTime time1 time2 =
    let sumSeconds = toSeconds time1 + toSeconds time2
    fromSeconds sumSeconds

let subTime time1 time2 =
    let sumSeconds = toSeconds time1 - toSeconds time2
    fromSeconds sumSeconds

let sumTimes = addTime timeTest1 timeTest2

printf $"{sumTimes}"