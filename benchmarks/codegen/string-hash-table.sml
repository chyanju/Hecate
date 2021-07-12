structure StringHashTable :> MONO_HASH_TABLE
  where type Key.hash_key = string
  = HashTableFn(
    type hash_key=string
    val hashVal = HashString.hashString
    val sameKey = op =)