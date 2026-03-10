include Util.Deque

type t = Packet.t Util.Deque.t [@@deriving yojson]
