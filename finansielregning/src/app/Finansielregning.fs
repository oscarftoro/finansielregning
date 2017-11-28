module Finansielregning

open System


//Fremskrivningsformlen
//den formel viser, hvordan en kapitals værdi vokser, 
//når der tilskrives en fast rente pr. termin
//K_n = K_0 * (1 + r)^n

//beregne størelsen K_n 
// K_0: double - nutidsværdi
// r: double - (1 + r) skrivningsfaktoren over en termin
// n: int  - (1 + r)^n  fremskrivningsfakmatoren over n terminer

let fformlen (k0 : double) (r: double) (n: int) =
  k0 * Math.Pow ((1. + r), n |> double)


//den effektive rente over en periode
let effektiveRente (r: double) (n: int) =
  Math.Pow((1. + r),n |> double) - 1.


//tilbageskrivningsformlen
//til at finde en kapitals nutidsværdi, 
//når man kender dens fremtidsværdi
// Nutidsværdien K0 af en kapital
// K0 = Kn/(1+r)^n = Kn * (1 + r)^(-n)
// sætning 2
let tsformlen (kn: double) (r: double) (n: int) =
  kn*Math.Pow((1.+r),-n |> double)

//for at finde kn
let tsformlen' (k0: double) (r: double) (n: int) =
  k0/Math.Pow((1.+r),-n |> double)
 
//9238.45426
//tsformlen 10000.0 0.02 4;;

//tsformlen 25000.0 0.055 6;;
//18131.14583



//nth root: anvendelse af newtons metode ifølge wikipedia
//https://en.wikipedia.org/wiki/Nth_root_algorithm
let nthroot n A =
    let rec f x =
        let m = n - 1.
        let x' = (m * x + A/x**m) / n
        match abs(x' - x) with
        | t when t < abs(x * 1e-9) -> x'
        | _ -> f x'
    f (A / double n)

//SÆTNING 3
//bestemelse af rentefonden
//ved kapitalfremskrivning
let rentefonden (k0: double) (kn: double) (n: int) =
  (nthroot (n |> double) (kn/k0)) - 1.
//sætning 4
//antal terminer n for nutidsværdien  af kapital K_0,
//der i løbet af n terminer er vokset til
//fremtidsværdien K_n gennem rentetilskrivninger
// med fast rente r per termin
// ln(K_n/K_0)/ ln(1 + r)

//hvor mange terminer n, der skal gå, inden k0 er vokset til kn
let nutidsvaerdien k0 kn r =
  Math.Log(kn/k0)/Math.Log(1.0 + r)

//Sætning 5
//  for en kapital, der over n terminer forrentes med r_1 i 1. termin,
// r_2 i 2. termin,..., r_n i n'te termin, er den gennemsnitlige rente
// per termin, r, bestemt ved
// r = ((1+r_1)*(1+r_2)...(1+r_n) - 1)^1/n

let renteMedVarTerm (rs: double list) n =
   (nthroot n (List.fold(fun acc r -> (1. + r) * acc) 1. rs)) - 1.

//renteMedVarTerm [0.03;0.02;0.03;0.03;0.05;0.03] 6.;;
// 0.03162785878

//FREMTIDSVÆRDI
//AF EN ANNUITET

//ved en annuitet forstårs en række lige store ydelser
//fortaget med lige store tidsmellemrum(terminer) til en fast rente 
//per temrin

//Sætning 6
//Summen af en kvotientrække bestemmes på følgende måde
// S = y * (a^n - 1)/(a - 1) hvor a <> 1

let Sumkvotientraekke y a n =
  (y * (Math.Pow(a,n) - 1.))/(a - 1.)

//Sumkvotientraekke 500. 1.01 4.;;
//val it : float = 2030.2005

//geometrisk række eller kvotientrække forstås en sum S, ef formen
// S = y + y * a^1 + y * a^2 + ... + y * a^(n-2) + y * a^(n-1)
// hvor y og a er reelle tal


//Sætning 7
//værdien A_n af annuitet med n ydelser, renten r per temin og 
//ydelsens størrelse y beregnes som:
// A_n = y + y * (1 + r) + y * (1 + r)²+...+y*(1+r)^(n-1) =
// y * (1+r)^n - 1 / r
//renta temporal constante postpagable
let annuitet y r n =
  y * (((1. + r)**n) - 1.) / r
//> annuitet 2500.0 0.02 17.;;
//val it : float = 50030.1774

let ydelse an r n =
  (an * r) / (((1.+r)**n) - 1.)

//ukendt antal ydelser n
let antalYdelser y an r =
 Math.Log((an*r/y)+1.)/Math.Log(1.+r) 


//ukendt rente r
let ukendtRente  y n =
  let r = 666. (*hvordan kan vi beregne r ? *)
  (y * (1. + r)**n)/ r
  
  //let nutidsvaerdienAlleYdelser y a0 r = 
 //> antalYdelser 2000. 100000. 0.05;;
 //val it : float = 25.67654751



[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
