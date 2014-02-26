module Prelude.Parallel.AgentDataStructures
open Prelude.Common

//Trivial extension of Tomas Petrecik's work
type Agent<'a> = MailboxProcessor<'a>

type DataComm<'k,'v> =
    | Check of AsyncReplyChannel<bool> 
    | Put 
    | PutCheck of AsyncReplyChannel<bool> 
    | Get of AsyncReplyChannel<'v>
    | GetOrAdd of 'v * AsyncReplyChannel<'v>
    | GetDef of 'v * AsyncReplyChannel<'v>
    | TryGet of AsyncReplyChannel<'v option>
    | GetDict of AsyncReplyChannel<Dict<'k,'v>>

/// Agent that implements a simple concurrent set. The agent exposes a 
/// member that adds value to the set and returns whether the value
/// was already present. As well as checking whether something is an element
type ConcurrentSetAgent<'T when 'T: equality>() = 
  let agent = Agent.Start(fun inbox -> async {
    let hashSet = new System.Collections.Generic.HashSet<'T>(HashIdentity.Structural) 
    while true do
      let! v, msg = inbox.Receive() 
      match msg with
         | Check reply -> reply.Reply(hashSet.Contains v) 
         | PutCheck reply -> reply.Reply(hashSet.Add(v)) 
         | _ -> ()
          })
           
  member x.AsyncContains(v) = agent.PostAndReply(fun reply -> v, Check reply)
  member x.AsynAdd (v) = agent.PostAndAsyncReply(fun reply -> v, PutCheck reply) 
  member x.Add (v) = agent.PostAndReply(fun reply -> v, PutCheck reply)

///a simple concurrent dictionary
type ConcurrentDictionaryAgent<'K,'V when 'K: equality>(dictionary:Dict<'K,'V>) =   
  let agent = Agent.Start(fun inbox -> async { 
    while true do
      let! (k,v), msg = inbox.Receive() 
      match msg with
         | Check reply -> reply.Reply(dictionary.ContainsKey k) 
         | Get reply -> reply.Reply(dictionary.[k]) 
         | TryGet reply -> reply.Reply(dictionary.tryFind k) 
         | GetDef (def,reply) -> reply.Reply(dictionary.getOrDef k def) 
         | GetOrAdd (def,reply) -> reply.Reply(dictionary.GetElseAdd k def) 
         | Put -> dictionary.Add(k,v)
         | GetDict reply -> reply.Reply(dictionary) 
         | _ -> ()
          })
           
  let messanger k msg = agent.PostAndAsyncReply(fun reply -> ((k,Unchecked.defaultof<'V>), msg reply))
  new() = ConcurrentDictionaryAgent(Dict(HashIdentity.Structural))
  member x.AsyncContainsKey(k) = agent.PostAndAsyncReply(fun reply -> (k,Unchecked.defaultof<'V>), Check reply)
   ///Prefer the use eof GetOrAdd
  member x.AsyncAdd (k:'K,v:'V) = agent.Post((k,v), Put)
  member x.AsyncGet (k:'K) = messanger k Get
  member x.Item k = messanger k Get
  member x.AsyncGetDef (k:'K)  def= agent.PostAndAsyncReply(fun reply -> (k,Unchecked.defaultof<'V>), GetDef(def,reply))
  member x.AsyncTryGet (k:'K) =  messanger k TryGet
  member x.AsyncGetOrAdd (k:'K) def = agent.PostAndAsyncReply(fun reply -> (k,Unchecked.defaultof<'V>), GetOrAdd(def,reply))
  member x.ToRegularDict() = agent.PostAndReply(fun reply -> (Unchecked.defaultof<'K>,Unchecked.defaultof<'V>), GetDict reply)
   