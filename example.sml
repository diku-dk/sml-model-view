fun strinit s =
  String.substring (s, 0, size s - 1)

val pollIn = OS.IO.pollIn o valOf o OS.IO.pollDesc o Posix.FileSys.fdToIOD
val pollOut = OS.IO.pollOut o valOf o OS.IO.pollDesc o Posix.FileSys.fdToIOD

fun forever f =
  (f (); forever f)

type ModelMsg = string * int

val puModelMsg: ModelMsg Pickle.pu = Pickle.pairGen (Pickle.string, Pickle.int)

type ViewMsg = string

val puViewMsg: ViewMsg Pickle.pu = Pickle.string

fun modelProc input output =
  let
    val in_poll = Channel.pollIn input
    fun handleMsg s =
      print ("model: received: " ^ s ^ "\n")
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
    val state = ref 5
    fun react () =
      ( Channel.send output ("state", !state)
      ; read ()
      ; state := !state + 1
      ; OS.Process.sleep (Time.fromSeconds 1)
      )
  in
    forever react
  end

fun viewProc input output =
  let
    val in_poll = Channel.pollIn input
    val out_poll = Channel.pollOut output
    val pending = ref []
    val stdin_poll = pollIn Posix.FileSys.stdin

    fun handleMsg (prop, value) =
      print ("view: " ^ prop ^ " = " ^ Int.toString value ^ "\n")

    fun onPoll ready =
      if OS.IO.infoToPollDesc ready = in_poll then
        handleMsg (Channel.receive input)
      else if OS.IO.infoToPollDesc ready = stdin_poll then
        case TextIO.inputLine TextIO.stdIn of
          NONE => raise Fail "stdin closed"
        | SOME l => pending := (l :: !pending)
      else if OS.IO.infoToPollDesc ready = out_poll then
        ( List.app (fn l => Channel.send output (strinit l)) (!pending)
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
        List.app onPoll (OS.IO.poll (pollers, NONE))
      end
  in
    forever react
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
