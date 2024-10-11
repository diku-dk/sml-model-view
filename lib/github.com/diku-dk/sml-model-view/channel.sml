structure Channel: CHANNEL =
struct
  fun strinit s =
    String.substring (s, 0, size s - 1)

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

  type 'a channel =
    { in_fd: Posix.FileSys.file_desc
    , in_stream: TextIO.instream
    , out_fd: Posix.FileSys.file_desc
    , out_stream: TextIO.outstream
    , pu: 'a Pickle.pu
    }
  fun new pu =
    let
      val {infd = in_fd, outfd = out_fd} = Posix.IO.pipe {}
    in
      { in_fd = in_fd
      , out_fd = out_fd
      , in_stream = fdToInstream in_fd
      , out_stream = fdToOutstream out_fd
      , pu = pu
      }
    end

  fun pollIn (c: 'a channel) =
    (OS.IO.pollIn o valOf o OS.IO.pollDesc o Posix.FileSys.fdToIOD) (#in_fd c)

  fun pollOut (c: 'a channel) =
    (OS.IO.pollOut o valOf o OS.IO.pollDesc o Posix.FileSys.fdToIOD) (#out_fd c)

  fun send (c: 'a channel) (msg: 'a) =
    TextIO.output
      (#out_stream c, String.toString (Pickle.pickle (#pu c) msg) ^ "\n")

  fun receive (c: 'a channel) =
    (Pickle.unpickle (#pu c) o valOf o String.fromString o strinit o valOf
     o TextIO.inputLine) (#in_stream c)

  fun canInput (c: 'a channel) =
    isSome (TextIO.canInput (#in_stream c, 1))
end
