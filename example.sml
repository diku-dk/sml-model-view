(* These produce nonblocking streams. *)
fun fdToInstream fd =
  TextIO.mkInstream (TextIO.StreamIO.mkInstream
    (Posix.IO.mkTextReader {fd = fd, name = "<pipe>", initBlkMode = true}, ""))

fun fdToOutstream fd =
  TextIO.mkOutstream (TextIO.StreamIO.mkOutstream
    ( Posix.IO.mkTextWriter
        { fd = fd
        , name = "<pipe>"
        , initBlkMode = true
        , appendMode = false
        , chunkSize = 1024
        }
    , IO.NO_BUF
    ))


val pollIn = OS.IO.pollIn o valOf o OS.IO.pollDesc o Posix.FileSys.fdToIOD
val pollOut = OS.IO.pollOut o valOf o OS.IO.pollDesc o Posix.FileSys.fdToIOD

fun forever f =
  (f (); forever f)

type ModelMsg = string * int

val puModelMsg: ModelMsg Pickle.pu = Pickle.pairGen (Pickle.string, Pickle.int)

val pickleModelMsg = String.toString o Pickle.pickle puModelMsg

val unpickleModelMsg = Pickle.unpickle puModelMsg o valOf o String.fromString

type ViewMsg = string

val puViewMsg: ViewMsg Pickle.pu = Pickle.string

val pickleViewMsg = String.toString o Pickle.pickle puViewMsg

val unpickleViewMsg = Pickle.unpickle puViewMsg o valOf o String.fromString

fun strinit s =
  String.substring (s, 0, size s - 1)

fun modelProc (inf_fd, inf) (outf_fd, outf) =
  let
    val in_poll = pollIn inf_fd
    fun handleMsg s =
      print ("model: received: " ^ s ^ "\n")
    val getMsg = unpickleViewMsg o strinit o valOf o TextIO.inputLine
    fun read () =
      case OS.IO.poll ([in_poll], SOME (Time.fromSeconds 0)) of
        [] => ()
      | _ =>
          let
            fun loop () =
              case TextIO.canInput (inf, 1) of
                NONE => ()
              | SOME x => (handleMsg (getMsg inf); loop ())
          in
            loop ()
          end
    val state = ref 0
    fun react () =
      ( TextIO.output (outf, pickleModelMsg ("state", !state) ^ "\n")
      ; read ()
      ; state := !state + 1
      ; OS.Process.sleep (Time.fromSeconds 5)
      )
  in
    forever react
  end

fun viewProc (inf_fd, inf) (outf_fd, outf) =
  let
    val in_poll = pollIn inf_fd
    val out_poll = pollOut outf_fd
    val pending = ref []
    val stdin_poll = pollIn Posix.FileSys.stdin

    fun handleMsg (prop, value) =
      print ("view: " ^ prop ^ " = " ^ Int.toString value ^ "\n")

    val getMsg = unpickleModelMsg o strinit o valOf o TextIO.inputLine

    fun onPoll ready =
      if OS.IO.infoToPollDesc ready = in_poll then
        handleMsg (getMsg inf)
      else if OS.IO.infoToPollDesc ready = stdin_poll then
        case TextIO.inputLine TextIO.stdIn of
          NONE => raise Fail "stdin closed"
        | SOME l => pending := (l :: !pending)
      else if OS.IO.infoToPollDesc ready = out_poll then
        ( List.app
            (fn l => TextIO.output (outf, pickleViewMsg (strinit l) ^ "\n"))
            (!pending)
        ; pending := []
        ; TextIO.flushOut outf
        )
      else
        ()
    fun react () =
      List.app onPoll (OS.IO.poll ([in_poll, out_poll, stdin_poll], NONE))
  in
    forever react
  end

fun main () =
  let
    val {infd = ctp_in_fd, outfd = ctp_out_fd} = Posix.IO.pipe {}
    val {infd = ptc_in_fd, outfd = ptc_out_fd} = Posix.IO.pipe {}
    val ctp_in = fdToInstream ctp_in_fd
    val ctp_out = fdToOutstream ctp_out_fd
    val ptc_in = fdToInstream ptc_in_fd
    val ptc_out = fdToOutstream ptc_out_fd
  in
    case Posix.Process.fork () of
      NONE =>
        ( TextIO.closeOut ctp_out
        ; TextIO.closeIn ptc_in
        ; modelProc (ctp_in_fd, ctp_in) (ptc_in_fd, ptc_out)
        )
    | SOME model_pid =>
        ( TextIO.closeIn ctp_in
        ; TextIO.closeOut ptc_out
        ; viewProc (ptc_in_fd, ptc_in) (ctp_out_fd, ctp_out)
        )
  end

val () = main ()
