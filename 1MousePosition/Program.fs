open System
open Gtk
open Freckle
open FSharp.Helpers

let mousePrint mouseFeed (textView : TextView) =
    feed {
        let! (x, y) = mouseFeed
        return fun () -> textView.Buffer.Text <- (sprintf "(%f, %f)" x y)
    }

let sampler mousePositionSource textView () = 
    sample {
        let! mousePos = Mailbox.readSync mousePositionSource
        let! res = mousePrint mousePos textView
                   |> Feed.planSynced_ 
                
        Application.RunIteration()
        return res
    }
    |> SampleAsync.ofSample


let setupWindow () =
    let window = new Window ("Mouse Position")
    window.SetSizeRequest(500,500)
    let textView = new TextView()
    textView.Editable <- false
    window.Add(textView)
    window.ShowAll()
    textView

[<EntryPoint>]
let main argv = 
    async {
        Application.Init ()    
        let textView = setupWindow  ()

        let clock = Clock.synchronized (Clock.systemUtc)

        let! mousePositionSource = Mailbox.createWithExpiration (Expire.After (Time.ofSeconds 10)) clock
        do! textView.MotionNotifyEvent
            |> Async.AwaitEvent
            |> Async.map (fun evt -> evt.Event.X, evt.Event.Y)  
            |> flip Mailbox.listenTo mousePositionSource
            
        do! Sample.sampleForever clock (sampler mousePositionSource textView) ()
    
    } |> Async.StartImmediate    
    0
