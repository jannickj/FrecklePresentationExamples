//Exercise:
// Add support for immidate trigger 
// If you don't know what immidiate is goto: https://css-tricks.com/debouncing-throttling-explained-examples/
// Basically its when an closely tied sequence of events 
// gets squished into a single event at the beginning


open System
open Gtk

open Freckle

let sampling keyFeed resultsMb setResults () = 
    sample {
        let postString s = Mailbox.postPromiseSilent (FakeDB.call s) resultsMb
        do! keyFeed
            |> Feed.map (fun s () -> postString s)
            |> Feed.delay (Time.ofMilliseconds 200)
            |> Feed.debouncing
            |> Feed.planSynced_ 
            
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
        
        let mbResults = Mailbox.createWithTTL (Time.ofSeconds 10) clock
        let mbKeys = Mailbox.createWithTTL (Time.ofSeconds 10) clock

        do! bar.Buffer.Changed
            |> Async.AwaitEvent
            |> FSharp.Helpers.Async.map (fun evt -> bar.Buffer.Text)  
            |> Mailbox.listenTo' mbKeys

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
