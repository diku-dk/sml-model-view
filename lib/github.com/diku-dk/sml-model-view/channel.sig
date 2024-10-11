(** Channel library for IPC.

    The idea is that you create a channel before you fork. What is
    written to the channel in one process can be read by the other. A
    process should never both read and write to the same channel.

 **)

signature CHANNEL =
sig
  type 'a channel
  val new: 'a Pickle.pu -> 'a channel
  val send: 'a channel -> 'a -> unit
  val receive: 'a channel -> 'a
  val pollIn: 'a channel -> OS.IO.poll_desc
  val pollOut: 'a channel -> OS.IO.poll_desc
  val canInput: 'a channel -> bool
end
