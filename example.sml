fun strinit s =
  String.substring (s, 0, size s - 1)

fun noncanon () =
  let
    val oldattr = Posix.TTY.TC.getattr Posix.FileSys.stdin
    val {iflag, oflag, cflag, lflag, cc, ispeed, ospeed} =
      Posix.TTY.fieldsOf oldattr
    val lflag = Posix.TTY.L.clear
      (lflag, Posix.TTY.L.flags [Posix.TTY.L.echo, Posix.TTY.L.icanon])
    val newattr = Posix.TTY.termios
      { iflag = iflag
      , oflag = oflag
      , cflag = cflag
      , lflag = lflag
      , cc = cc
      , ispeed = ispeed
      , ospeed = ospeed
      }
    val () =
      Posix.TTY.TC.setattr (Posix.FileSys.stdin, Posix.TTY.TC.sanow, newattr)
  in
    fn () =>
      Posix.TTY.TC.setattr (Posix.FileSys.stdin, Posix.TTY.TC.sanow, oldattr)
  end

val pollIn = OS.IO.pollIn o valOf o OS.IO.pollDesc o Posix.FileSys.fdToIOD
val pollOut = OS.IO.pollOut o valOf o OS.IO.pollDesc o Posix.FileSys.fdToIOD

fun forever f =
  (f (); forever f)

fun repeat 0 f = ()
  | repeat n f =
      (f (); repeat (n - 1) f)


(* Message sent by model to view. *)
type ModelMsg = string * int

val puModelMsg: ModelMsg Pickle.pu = Pickle.pairGen (Pickle.string, Pickle.int)

(* Message sent by view to model. *)
type ViewMsg = string

val puViewMsg: ViewMsg Pickle.pu = Pickle.string

fun modelProc input output =
  let
    val in_poll = Channel.pollIn input
    val field = ref "foo"
    val state = ref 5
    fun handleMsg s = field := s
    fun read () =
      case OS.IO.poll ([in_poll], SOME (Time.fromSeconds 0)) of
        [] => ()
      | _ =>
          let
            fun loop () =
              if Channel.canInput input then
                (handleMsg (Channel.receive input); loop ())
              else
                ()
          in
            loop ()
          end
    fun react () =
      ( Channel.send output (!field, !state)
      ; read ()
      ; state := !state + 1
      ; OS.Process.sleep (Time.fromSeconds 1)
      )
  in
    forever react
  end

datatype viewstate = VIEWSTATE of {fields: (string * int) list, selected: int}

local
  fun out s = TextIO.output (TextIO.stdOut, s)
in
  fun cursorUp () = out ("\027[1A")
end

fun renderView (VIEWSTATE {fields, selected}) =
  let
    fun line i [] = ()
      | line i ((field, v) :: rest) =
          let
            val prefix = if i = selected then " * " else "   "
          in
            print (prefix ^ field ^ ": " ^ Int.toString v ^ "\n");
            line (i + 1) rest
          end
  in
    line 0 fields;
    repeat (length fields) cursorUp
  end

fun viewNext (VIEWSTATE {fields, selected}) =
  if selected < length fields - 1 then
    VIEWSTATE {fields = fields, selected = selected + 1}
  else
    VIEWSTATE {fields = fields, selected = selected}

fun viewPrev (VIEWSTATE {fields, selected}) =
  if selected > 0 then VIEWSTATE {fields = fields, selected = selected - 1}
  else VIEWSTATE {fields = fields, selected = selected}

fun viewIncField f1 d (VIEWSTATE {fields, selected}) =
  let
    fun onField (f2, v2) =
      if f1 = f2 then (f2, v2 + d) else (f2, v2)
  in
    VIEWSTATE {fields = map onField fields, selected = selected}
  end

fun viewCurField (VIEWSTATE {fields, selected}) =
  #1 (List.nth (fields, selected))

val initialViewState = VIEWSTATE
  {fields = [("foo", 1), ("bar", 2), ("baz", 3)], selected = 0}

fun viewProc (input: ModelMsg Channel.channel) (output: ViewMsg Channel.channel) =
  let
    exception Stop
    val in_poll = Channel.pollIn input
    val out_poll = Channel.pollOut output
    val stdin_poll = pollIn Posix.FileSys.stdin
    val pending = ref []
    val view = ref initialViewState
    fun handleMsg (prop, value) =
      view := viewIncField prop value (!view)

    fun nextIn () =
      case TextIO.input1 TextIO.stdIn of
        NONE => raise Stop
      | SOME c => c

    fun addMsg msg =
      pending := msg :: !pending

    fun onPoll ready =
      if OS.IO.infoToPollDesc ready = in_poll then
        handleMsg (Channel.receive input)
      else if OS.IO.infoToPollDesc ready = stdin_poll then
        case nextIn () of
          #"q" => raise Stop
        | #"\027" =>
            (* Escape character - means more will be available. *)
            (case (nextIn (), ord (nextIn ())) of
               (#"[", 65) => (* arrow up *)
                 (view := viewPrev (!view); addMsg (viewCurField (!view)))
             | (#"[", 66) => (* arrow down *)
                 (view := viewNext (!view); addMsg (viewCurField (!view)))
             | _ => ())
        | c => ()
      else if OS.IO.infoToPollDesc ready = out_poll then
        ( List.app (fn l => Channel.send output l) (rev (!pending))
        ; pending := []
        )
      else
        ()
    fun react () =
      let
        val pollers =
          if null (!pending) then [in_poll, stdin_poll]
          else [in_poll, out_poll, stdin_poll]
      in
        renderView (!view);
        List.app onPoll (OS.IO.poll (pollers, NONE))
      end
    val restore = noncanon ()
  in
    forever react
    handle Stop => restore ()
  end

fun main () =
  let
    val model_c = Channel.new puModelMsg
    val view_c = Channel.new puViewMsg
  in
    case Posix.Process.fork () of
      NONE => modelProc view_c model_c
    | SOME model_pid => viewProc model_c view_c
  end

val () = main ()
