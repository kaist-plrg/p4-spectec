/* ECDSA public key recovery with libsecp256k1, as hook_KRYPTO_ecdsaRecover of
   the blockchain plugin (plugin-c/crypto.cpp) */

#include <string.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <secp256k1.h>
#include <secp256k1_recovery.h>

static secp256k1_context *ctx = NULL;

/* hash, r, s: 32 bytes each; recid: v - 27. The 64 bytes of the uncompressed
   public key without its prefix, or "" when the signature does not recover. */
CAMLprim value kore_ecdsa_recover(value hash, value recid, value r, value s) {
  CAMLparam4(hash, recid, r, s);
  unsigned char sig64[64], ser[65];
  size_t len = sizeof(ser);
  secp256k1_ecdsa_recoverable_signature sig;
  secp256k1_pubkey key;
  if (ctx == NULL)
    ctx = secp256k1_context_create(SECP256K1_CONTEXT_VERIFY | SECP256K1_CONTEXT_SIGN);
  memcpy(sig64, String_val(r), 32);
  memcpy(sig64 + 32, String_val(s), 32);
  if (!secp256k1_ecdsa_recoverable_signature_parse_compact(ctx, &sig, sig64, Int_val(recid))
      || !secp256k1_ecdsa_recover(ctx, &key, &sig, (const unsigned char *)String_val(hash)))
    CAMLreturn(caml_alloc_string(0));
  secp256k1_ec_pubkey_serialize(ctx, ser, &len, &key, SECP256K1_EC_UNCOMPRESSED);
  CAMLreturn(caml_alloc_initialized_string(64, (const char *)ser + 1));
}
