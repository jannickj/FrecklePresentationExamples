// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Gtk

open Freckle



let setupWindow () =
    let window = new Window ("helloworld")
    window.SetSizeRequest(1000,1000)
    let bar = new TextView()
    bar.SetSizeRequest(700, 20)
    let results = new TextView()
    results.SetSizeRequest(700, 700)
    results.Editable <- false
    let wall = new Fixed()
    wall.Put(bar, 20, 20)
    wall.Put(results, 20, 50)
    window.Add(wall)
    window.ShowAll()

    (bar, results)

module Mailbox = 
    let readSync mb = Mailbox.read mb |> Async.RunSynchronously


[<EntryPoint>]
let main argv = 
    Application.Init ()
    
    let (bar, results) = setupWindow  ()

    let setBar text =
        bar.Buffer.Text <- text

    let setResults text = 
        results.Buffer.Text <- String.concat "\n" text

    let clock = Clock.synchronized (Clock.systemUtc)
    

    let evt =
        bar.Buffer.Changed
        |> Async.AwaitEvent
        |> FSharp.Helpers.Async.map (fun _ -> bar.Buffer.Text)

    let sampling mb () = 
        sample {
            let keyFeed = Mailbox.readSync mb
            let! p = Sample.period

            do! keyFeed
                |> Feed.timeStamp
                |> Feed.groupBy (fun (t1,_) (t2,_) -> Time.between t1 t2 < Time.ofMilliseconds 200)
                |> Feed.groupBy (fun f1 f2 -> Feed.testLength 2 f1 = 2 && Feed.testLength 2 f2 = 2)
                |> Feed.map (Feed.take 1)
                |> Feed.join
                |> Feed.map (Feed.take 1)
                |> Feed.join
                |> Feed.map snd
                |> Feed.delay (Time.ofMilliseconds 200)
                |> Feed.foldPast (fun () s -> setResults [s]) () 

            Application.RunIteration()
        }

    async {
        let! mb = Mailbox.createWithExpiration (Expire.After (Time.ofSeconds 10)) clock
        do! Mailbox.listenTo evt  mb
        let sampler = (sampling mb)
                      >> SampleAsync.ofSample
        do! Sample.sampleForever clock sampler ()
    } |> Async.StartImmediate
    
    0 // return an integer exit code
