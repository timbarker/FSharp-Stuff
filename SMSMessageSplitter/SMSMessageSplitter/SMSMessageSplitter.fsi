module SMSMessageSplitter

val splitGSM : (string -> string list)
val splitUnicode : (string -> string list)

val truncateGSM : (string -> string)
val truncateUnicode : (string -> string)