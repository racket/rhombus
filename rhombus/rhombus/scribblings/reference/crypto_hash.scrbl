#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/crypto_hash)

@title(~tag: "crypto_hash"){Cryptographic Hashing}

@docmodule(rhombus/crypto_hash)

@doc(
  fun crypto_hash.sha1(in :: Bytes || Port.Input) :: String
  fun crypto_hash.sha224(in :: Bytes || Port.Input) :: String
  fun crypto_hash.sha256(in :: Bytes || Port.Input) :: String
){

 Computes the SHA-1, SHA-224, or SHA-256 hash of the content of
 @rhombus(in). The result is reported as a string of 2-digit hexadecimal
 numbers, one per byte in the computed hash.

@examples(
  ~hidden:
    import rhombus/crypto_hash
  ~repl:
    crypto_hash.sha1(#"apple")
)

}

@doc(
  fun crypto_hash.sha1_bytes(in :: Bytes || Port.Input) :: Bytes
  fun crypto_hash.sha224_bytes(in :: Bytes || Port.Input) :: Bytes
  fun crypto_hash.sha256_bytes(in :: Bytes || Port.Input) :: Bytes
){

 Computes the SHA-1, SHA-224, or SHA-256 hash of the content of
 @rhombus(in). The result is reported as a byte string containing the
 bits of the hash.

@examples(
  ~hidden:
    import rhombus/crypto_hash
  ~repl:
    crypto_hash.sha1_bytes(#"apple")
)

}
