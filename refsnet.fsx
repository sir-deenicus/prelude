open System

#r "prelude/bin/release/prelude.dll"

open Prelude.Common
open Prelude.SimpleGraphs

let getAssemblies str = 
    let path = IO.Path.GetDirectoryName str
    let globalSeen = Hashset()
    let globalFails = Hashset()
    let localLibs = Hashset()
    let g = FastStringGraph()

    let root = Reflection.Assembly.LoadFile str
    let root_name = root.GetName().Name
    let _ = globalSeen.Add root_name

    let rec getAll (a:Reflection.Assembly) =  
         
         let refs = a.GetReferencedAssemblies()
         let assemblyNames = set(refs |> Array.map (fun a -> a.Name))
  
         let newAssemblies = assemblyNames - (set globalSeen)
         if newAssemblies = Set.empty then ()
         else
            let a_name = a.GetName().Name
            let _ = g.InsertVertex (a_name)
            let refsnew = refs |> Array.filter (fun a -> newAssemblies.Contains a.Name) 
            newAssemblies |> Seq.iter (fun n -> g.InsertVertex n; g.InsertEdge(a_name, n); globalSeen.Add n |> ignore) 

            Array.iter getAll  
                       (refsnew |> Array.map(fun a ->  
                                    a, errorFall { let! _ = lazy (Reflection.Assembly.Load a)
                                                   let _ = localLibs.Add (a.Name)
                                                   let! a2 = lazy (Reflection.Assembly.LoadFile (combinePaths [path; a.Name + ".dll"]))
                                                   return a2}) 

                                |> Array.filterMap (function (a, (_, None)) -> globalFails.Add a; false | _ -> true) (snd >> snd >> Option.get))
             
          
    getAll root
    globalSeen, globalFails, localLibs , g

     
let seen, fails,local,g  = getAssemblies @"dllv1"

let seen2, fails2,locals2,g2  = getAssemblies @"dllv2"

(set seen2) - (set seen)
(set seen) - (set seen2) 

dispStringTree 0 None g2 (Seq.head seen2)

pairapply Seq.toArray (locals2,seen2)

fails2 |> Seq.toArray |> Array.map (fun a -> a.Name)   
