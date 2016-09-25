// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Gtk

open Freckle

let sampling keyFeed resultsMb setResults () = 
    sample {
        do! keyFeed
            |> Feed.debouncing
            |> Feed.delay (Time.ofMilliseconds 200)
            |> Feed.actionSynced_ (fun s -> Mailbox.postPromiseSilent (FakeDB.call s) resultsMb)
        
        let! resultsFeed = Mailbox.readSync resultsMb
        
        do! resultsFeed
            |> Feed.map (Seq.truncate 40)
            |> Feed.actionSynced_ (fun ls -> setResults ls)

        Application.RunIteration()
    }


open FSharp.Helpers

let setupWindow () =
    let window = new Window ("Search Bar Example")
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

    (bar, results, wall)

[<EntryPoint>]
let main argv = 
    async {
        Application.Init ()
    
        let (bar, results, wall) = setupWindow  ()

        let setBar text =
            bar.Buffer.Text <- text

        let setResults text = 
            results.Buffer.Text <- String.concat "\n" text

        let clock = Clock.synchronized (Clock.systemUtc)
        
        let! mbResults = Mailbox.createWithExpiration (Expire.After (Time.ofSeconds 10)) clock
        let! mbKeys = Mailbox.createWithExpiration (Expire.After (Time.ofSeconds 10)) clock

        let evt =
            bar.Buffer.Changed
            |> Async.AwaitEvent
            |> FSharp.Helpers.Async.map (fun evt -> bar.Buffer.Text)  
        do! Mailbox.listenTo evt mbKeys

        let sampler s = 
            sample {
                let! keysFeed = Mailbox.readSync mbKeys                
                let! res = sampling keysFeed mbResults setResults s
                
                return res
            }
            |> SampleAsync.ofSample
        do! Sample.sampleForever clock sampler ()
    
    } |> Async.StartImmediate
    
    0 // return an integer exit code
