type Position = 
    { X : float
      Y : float
    }
type Vector =
    { Vx : float
      Vy : float
    }
type Square = 
    { Width  : float
      Height : float
    }

type ObjectType = Fighter
                | Astroid
                | Bullet

type Object = 
    { Position : Position
      Boundary : Square
      Direction : Vector
      Acceleration : Vector
      Type : ObjectType
    }
type Pilot =
    { Forward : bool
      Backward : bool
      Left     : bool
      Right    : bool
    }
type GameState =
    { Objects : Object list
      Pilot : Pilot
    }

let move (pilot : Pilot) (o : Object) =
    let pos = o.Position
    let dir = o.Direction
    let acc = o.Acceleration
    let newAccVal dirPos dirNeg =
        if dirPos && not dirNeg then 0.04
        elif dirNeg && not dirPos then -0.04
        else 0.0
    let acc' = if o.Type = Fighter 
               then { Vx = newAccVal pilot.Forward pilot.Backward
                      Vy = newAccVal pilot.Left pilot.Right
                    }
               else acc
    { o with Position  = { X = pos.X + dir.Vx; Y = pos.Y + dir.Vy }
             Direction = { Vx = dir.Vx + acc'.Vx; Vy = dir.Vy + acc'.Vy }
             Acceleration = acc'
    }

let isColliding (o1 : Object) (o2 : Object) =
    let pos1 = o1.Position
    let pos2 = o2.Position
    let bound1 = o1.Boundary
    let bound2 = o2.Boundary
    pos1.X < pos2.X + bound2.Width
    && pos1.X + bound1.Width > pos2.X
    && pos1.Y < pos2.Y + bound2.Height
    && bound1.Height + pos1.Y > pos2.Y

let update (state : GameState) = 
    let os = state.Objects
          |> List.map (move state.Pilot)
          |> List.indexed

    let isHit (idx, o) = 
        List.exists (fun (idx2, o2) -> isColliding o o2 && idx <> idx2) os
    
    let newOs = List.filter (not << isHit) os

    { state with Objects = List.map snd newOs }

let setVx vx dir = { dir with Vx = float vx }
let setVy vy dir = { dir with Vy = float vy }
let inline updateAcc f o = { o with Acceleration = f o.Acceleration }

let changePilot f (gs : GameState) = 
    { gs with Pilot = f gs.Pilot }

let bound = 
    function
    | Fighter -> { Width = 2.0; Height = 2.0 }
    | Astroid -> { Width = 1.0; Height = 1.0 }
    | Bullet -> { Width = 1.0; Height = 1.0 }

let inline createObject otype x y =
    { Position = { X = float x; Y = float y }
      Boundary = bound otype
      Direction = { Vx = 0.0; Vy = 0.0 }
      Acceleration = { Vx = 0.0; Vy = 0.0 }
      Type = otype
    }

open Freckle
open OpenTK.Input

let inputUpdate gs keyActivation  =
    let (key, activation) = keyActivation
    match key with
    | Key.W  -> changePilot (fun p -> { p with Forward = activation }) gs
    | Key.S -> changePilot (fun p -> { p with Backward = activation }) gs
    | Key.A -> changePilot (fun p -> { p with Left = activation }) gs
    | Key.D -> changePilot (fun p -> { p with Right = activation }) gs
    | _ -> gs

let stateSampler inputMb stateMb gs =
    sample {
        let! input = Mailbox.readSync inputMb
        let! pulses = Feed.pulse 20
        let! gs' = Feed.foldPast inputUpdate gs input
        let! gs'' = Feed.foldPast (fun s _ ->  update s) gs' pulses
        do! pulses
            |> Feed.debouncing
            |> Feed.map (fun _ -> fun () -> Mailbox.postSync gs'' stateMb)
            |> Feed.planSynced_
        return gs''

    }

let gameComponent inputMb clock =
    async {
        let stateMb = Mailbox.createWithTTL (Time.ofSeconds 1) clock
        let initPilot = { Forward = false; Backward = false; Left = false; Right = false }
        let initState = { Objects = [createObject Fighter 2 0; createObject Astroid 0 0]; Pilot = initPilot }
        do! Sample.sampleForever clock ((stateSampler inputMb stateMb) >> SampleAsync.ofSample) initState
            |> Async.StartChild
            |> Async.Ignore
        return stateMb
    }
    
open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open System.Drawing
open System.Drawing.Imaging


type Texture = Texture of int
type ViewState =
    { Fighter : Texture
      Astroid : Texture
      Bullet  : Texture
    }

let getTexture (o : Object) (vs : ViewState) =
    match o.Type with
    | Fighter -> vs.Fighter
    | Bullet -> vs.Bullet
    | Astroid -> vs.Astroid

let loadImage (bmp : Bitmap) =
    let target = TextureTarget.Texture2D
    GL.Enable(EnableCap.Texture2D)
    let name = GL.GenTexture()
    GL.BindTexture(target, name)
    GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear);
    GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Linear)

    let sizeRect = Rectangle(0,0, bmp.Width, bmp.Height)
    let bmpData = bmp.LockBits(sizeRect, ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb)

    GL.TexImage2D(target, 0, PixelInternalFormat.Rgba, bmp.Width, bmp.Height, 0, PixelFormat.Bgra, PixelType.UnsignedByte, bmpData.Scan0)
    bmp.UnlockBits(bmpData)
    Texture name

let toBmp (i : Image) = new Bitmap(i)

let load (vs : ViewState ref) =
    GL.Hint(HintTarget.PerspectiveCorrectionHint, HintMode.Nicest)

    let loadTexture = toBmp >> loadImage
    let bullet = Image.FromFile("resources/bullet.png") |> loadTexture
    let fighter = Image.FromFile("resources/fighter.png")  |> loadTexture
    let astroid = Image.FromFile("resources/astroid.png")  |> loadTexture
    vs.Value <- { Fighter = fighter; Bullet = bullet; Astroid = astroid }

let renderTexture (bound : Square) (Texture name) =
    GL.BindTexture(TextureTarget.Texture2D, name)
    GL.Begin(PrimitiveType.Quads)
    let halfW = float32 <| bound.Width / 2.0
    let halfH = float32 <| bound.Height / 2.0
    GL.TexCoord2(0.0f, 1.0f); GL.Vertex2(-halfW, -halfH)
    GL.TexCoord2(1.0f, 1.0f); GL.Vertex2(halfW, -halfH)
    GL.TexCoord2(1.0f, 0.0f); GL.Vertex2(halfW, halfH)
    GL.TexCoord2(0.0f, 0.0f); GL.Vertex2(-halfW, halfH)
    GL.End()

let view = ref Matrix4.Identity
let renderState (game : GameWindow) (vs: ViewState) (state : GameState) =
    GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
    GL.MatrixMode(MatrixMode.Modelview)
    GL.Enable(EnableCap.Texture2D);
    for o in state.Objects do
        let dir = o.Direction
        let angle = 
            if dir.Vx = 0.0 && dir.Vy = 0.0 then 0.0f
            else Vector3.CalculateAngle(Vector3(float32 dir.Vx, float32 dir.Vy, 0.0f), Vector3(1.0f, 0.0f, 0.0f))
        let angle2 = angle
        let angle2 = if dir.Vy < 0.0 then angle2 + 3.14f else angle2
        
        printfn "%f,%f = %f -> %f" dir.Vx dir.Vy (angle * (180.0f / 3.14f)) (angle2 * (180.0f / 3.14f))
        let m = Matrix4.CreateRotationZ(angle2) *
                Matrix4.CreateTranslation(float32 o.Position.X, float32 o.Position.Y, 0.0f) *
                Matrix4.CreateScale(0.05f) 
                * Matrix4.CreateRotationZ(1.5707f)
                
        GL.LoadMatrix(ref m)
        renderTexture o.Boundary (getTexture o vs)
    game.SwapBuffers()
    
let render game vs gsMb =
    sample {
        let! gs = Mailbox.readSync gsMb
        do! Feed.actionSynced_ (renderState game vs) gs
    }

let initOpenGl clock gsMb =    
    let game = new GameWindow(1200,1200)
    let vs = ref Unchecked.defaultof<_>

    let now = Clock.nowSynced clock
    let token = ref now

    let sampler _ = token := Sample.sampleOnce_ clock (render game vs.Value gsMb) token.Value

    game.Load.Add(fun _ -> load vs)
    game.RenderFrame.Add(sampler)
    game


[<EntryPoint>]
let main argv = 
    async {
        let clock = Clock.systemUtc
                    |> Clock.synchronized
        let inputMb = Mailbox.createWithTTL (Time.ofSeconds 10) clock
        let! gsMb = gameComponent inputMb clock
        let game = initOpenGl clock gsMb
        let toKey (evt : KeyboardKeyEventArgs) = evt.Key
        let keyDown key = (key, true)
        let keyUp key = (key, false)
        do! Mailbox.ListenToMappedEvent (toKey >> keyDown) game.KeyDown inputMb
        do! Mailbox.ListenToMappedEvent (toKey >> keyUp) game.KeyUp inputMb
        game.Run()
    } |> Async.StartImmediate
    0 // return an integer exit code
