/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2017       .                                          */
/*    Fabrice Le Fessant, INRIA & OCamlPro SAS <fabrice@lefessant.net>    */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

#include <secp256k1_recovery.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>

value secp256k1_ecdsa_recoverable_signature_parse_compact_ml
(
 value ctx_v,
 value sig_v,     /* Out */
 value input64_v, /* In */
 value recid_v    /* In */
 )
{
  const secp256k1_context* ctx =
    (const secp256k1_context*)
    String_val(ctx_v);
  secp256k1_ecdsa_recoverable_signature* sig =
    (secp256k1_ecdsa_recoverable_signature*)
    String_val(sig_v);
  const unsigned char *input64 =
    (const unsigned char *)
    String_val(input64_v);
  int recid = Int_val(recid_v);  
  int res = secp256k1_ecdsa_recoverable_signature_parse_compact(
                                                                ctx,
                                                                sig,
                                                                input64,
                                                                recid);
  return Val_int(res);
}

value secp256k1_ecdsa_recoverable_signature_convert_ml
(
 value ctx_v,
 value sig_v,   /* Out */
 value sigin_v  /* In */
 )
{
  const secp256k1_context* ctx =
   (const secp256k1_context*)
    String_val(ctx_v);
  secp256k1_ecdsa_signature* sig =
    (secp256k1_ecdsa_signature*)
    String_val(sig_v);
  const secp256k1_ecdsa_recoverable_signature* sigin =
    (const secp256k1_ecdsa_recoverable_signature*)
    String_val(sigin_v);

  secp256k1_ecdsa_recoverable_signature_convert(ctx,sig,sigin);
  return Val_unit;
}



value secp256k1_ecdsa_recoverable_signature_serialize_compact_ml
(
 value ctx_v,
 value output64_v,
 value sig_v
)
{
  const secp256k1_context* ctx =
    (const secp256k1_context*)
    String_val(ctx_v); 
  unsigned char *output64 =
    (unsigned char *)
    String_val(output64_v);
  int recid;
  const secp256k1_ecdsa_recoverable_signature* sig =
    (secp256k1_ecdsa_recoverable_signature*)
    String_val(sig_v);

  secp256k1_ecdsa_recoverable_signature_serialize_compact(ctx,
                                                          output64,
                                                          &recid,
                                                          sig);
  return Val_int(recid);
}

value secp256k1_ecdsa_recoverable_sign_ml
(
 value ctx_v,
 value sig_v,
 value msg32_v,
 value seckey_v
 ){
  const secp256k1_context* ctx =
    (const secp256k1_context*)
    String_val(ctx_v); 
  secp256k1_ecdsa_recoverable_signature *sig =
    (secp256k1_ecdsa_recoverable_signature *)
    String_val(sig_v);
  const unsigned char *msg32 =
    (const unsigned char *)
    String_val(msg32_v);
  const unsigned char *seckey =
    (const unsigned char *)
    String_val(seckey_v);
  secp256k1_nonce_function noncefp = NULL;
  const void *ndata = NULL;

  int res = secp256k1_ecdsa_sign_recoverable(ctx,
                                             sig,
                                             msg32,
                                             seckey,
                                             noncefp,
                                             ndata);
  return Val_int(res);
}


value secp256k1_ecdsa_recoverable_recover_ml
(
 value ctx_v,
 value pubkey_v, /* Out */
 value sig_v,    /* In */
 value msg32_v   /* In */
 ) 
{
    const secp256k1_context* ctx =
      (const secp256k1_context*)
      String_val(ctx_v); 
    secp256k1_pubkey *pubkey =
      (secp256k1_pubkey *)
      String_val(pubkey_v);
    const secp256k1_ecdsa_recoverable_signature *sig =
      (secp256k1_ecdsa_recoverable_signature *)
      String_val(sig_v);
    const unsigned char *msg32 =
      (const unsigned char *)
      String_val(msg32_v);
    
    int res = secp256k1_ecdsa_recover(ctx, pubkey, sig, msg32);
    return Val_int(res);
}



value secp256k1_context_create_ml(value flags_v)
{
  unsigned int flags = SECP256K1_CONTEXT_NONE;
  while( flags_v != Val_unit ){
    if( Int_val( Field(flags_v, 0) ) == 0 ){
      flags |= SECP256K1_CONTEXT_VERIFY;
    } else {
      flags |= SECP256K1_CONTEXT_SIGN;
    }
    flags_v = Field( flags_v, 1);
  }

  return (value)secp256k1_context_create(flags);
}

value secp256k1_ec_pubkey_parse_ml
(
 value ctx_v,
 value pubkey_v,
 value input_v
 )
{
  const secp256k1_context* ctx =
      (const secp256k1_context*)
      String_val(ctx_v); 
    secp256k1_pubkey* pubkey =
      (secp256k1_pubkey *)
      String_val(pubkey_v);
    const unsigned char *input =
      (const unsigned char *)
      String_val(input_v);
    size_t inputlen = caml_string_length(input_v);

    int res =  secp256k1_ec_pubkey_parse(ctx, pubkey, input, inputlen);
    return Val_int(res);
}


value secp256k1_ec_pubkey_serialize_ml
(
 value ctx_v,
 value output_v,
 value pubkey_v,
 value flags_v
 ){
  const secp256k1_context* ctx =
      (const secp256k1_context*)
      String_val(ctx_v); 
    unsigned char *output =
      (unsigned char*)
      String_val(output_v);
    size_t outputlen = caml_string_length(output_v);
    const secp256k1_pubkey* pubkey =
      (secp256k1_pubkey *)
      String_val(pubkey_v);
    unsigned int flags = 0;

  while( flags_v != Val_unit ){
    if( Int_val( Field(flags_v, 0) ) == 0 ){
      flags |= SECP256K1_EC_COMPRESSED;
    } else {
      flags |= SECP256K1_EC_UNCOMPRESSED;
    }
    flags_v = Field( flags_v, 1);
  }
  secp256k1_ec_pubkey_serialize(
                                ctx,
                                output,
                                &outputlen,
                                pubkey,
                                flags
                                );
  return Val_int(outputlen);
}

value secp256k1_ecdsa_signature_parse_compact_ml
(
 value ctx_v,
 value sig_v,
 value input64_v
 ){
  const secp256k1_context* ctx =
    (const secp256k1_context*)
    String_val(ctx_v);
  secp256k1_ecdsa_signature* sig =
    (secp256k1_ecdsa_signature*)
    String_val(sig_v);
  const unsigned char *input64 =
    (const unsigned char*)
    String_val(input64_v);

  int res = secp256k1_ecdsa_signature_parse_compact(ctx,
                                                    sig,
                                                    input64);
  return Val_int(res);
}

value secp256k1_ecdsa_signature_parse_der_ml
(
 value ctx_v,
 value sig_v,
 value input_v
 ){
  const secp256k1_context* ctx =
    (const secp256k1_context*)
    String_val(ctx_v);
  secp256k1_ecdsa_signature* sig =
    (secp256k1_ecdsa_signature*)
    String_val(sig_v);
  const unsigned char *input =
    (const unsigned char*)
    String_val(input_v);
  size_t inputlen = caml_string_length(input_v);
  
  int res = secp256k1_ecdsa_signature_parse_der(ctx,
                                                sig,
                                                input,
                                                inputlen);
  return Val_int(res);
}



value secp256k1_ecdsa_signature_serialize_der_ml
(
 value ctx_v,
 value output_v,
 value sig_v
)
{
  const secp256k1_context* ctx =
    (const secp256k1_context*)
    String_val(ctx_v); 
  unsigned char *output =
    (unsigned char *)
    String_val(output_v);
  size_t outputlen = caml_string_length(output_v);
  const secp256k1_ecdsa_signature* sig =
    (secp256k1_ecdsa_signature*)
    String_val(sig_v);

  secp256k1_ecdsa_signature_serialize_der(ctx,
                                          output,
                                          &outputlen,
                                          sig);
  return Val_int(outputlen);
}

value secp256k1_ecdsa_signature_serialize_compact_ml
(
 value ctx_v,
 value output64_v,
 value sig_v
)
{
  const secp256k1_context* ctx =
    (const secp256k1_context*)
    String_val(ctx_v); 
  unsigned char *output64 =
    (unsigned char *)
    String_val(output64_v);
  const secp256k1_ecdsa_signature* sig =
    (secp256k1_ecdsa_signature*)
    String_val(sig_v);

  secp256k1_ecdsa_signature_serialize_compact(ctx,
                                              output64,
                                              sig);
  return Val_unit;
}

value secp256k1_ecdsa_verify_ml
(
 value ctx_v,
 value sig_v,
 value msg32_v,
 value pubkey_v
 )
{
  const secp256k1_context* ctx =
    (const secp256k1_context*)
    String_val(ctx_v);
  const secp256k1_ecdsa_signature *sig =
    (const secp256k1_ecdsa_signature *)
    String_val(sig_v);
  const unsigned char *msg32 =
    (const unsigned char *)
    String_val(msg32_v);
  const secp256k1_pubkey *pubkey =
    (const secp256k1_pubkey *)
    String_val(pubkey_v);

    int res = secp256k1_ecdsa_verify(ctx,
                                     sig,
                                     msg32,
                                     pubkey);
    return Val_int(res);
}

value secp256k1_ecdsa_sign_ml
(
 value ctx_v,
 value sig_v,
 value msg32_v,
 value seckey_v
 )
{
  const secp256k1_context* ctx =
    (const secp256k1_context*)
    String_val(ctx_v);
  secp256k1_ecdsa_signature *sig =
    (secp256k1_ecdsa_signature *)
    String_val(sig_v);
  const unsigned char *msg32 =
    (const unsigned char *)
    String_val(msg32_v);
  const unsigned char *seckey =
    (const unsigned char *)
    String_val(seckey_v);
  secp256k1_nonce_function noncefp = NULL;
  const void *ndata = NULL;
  
  int res = secp256k1_ecdsa_sign(ctx,
                                 sig,
                                 msg32,
                                 seckey,
                                 noncefp,
                                 ndata);
  return Val_int(res);
}

value secp256k1_ec_pubkey_create_ml
(
 value ctx_v,
 value pubkey_v, /* out */
 value seckey_v  /* in */
 )
{
 const secp256k1_context* ctx =
    (const secp256k1_context*)
    String_val(ctx_v);
 secp256k1_pubkey *pubkey =
   (secp256k1_pubkey *)
   String_val(pubkey_v);
 const unsigned char *seckey =
   (const unsigned char *)
   String_val(seckey_v); /* 32-byte privkey */

 int res = secp256k1_ec_pubkey_create(ctx,
                                      pubkey,
                                      seckey);
 return Val_int(res);
}

#if 0

/** Convert a signature to a normalized lower-S form.
 *
 *  Returns: 1 if sigin was not normalized, 0 if it already was.
 *  Args: ctx:    a secp256k1 context object
 *  Out:  sigout: a pointer to a signature to fill with the normalized form,
 *                or copy if the input was already normalized. (can be NULL if
 *                you're only interested in whether the input was already
 *                normalized).
 *  In:   sigin:  a pointer to a signature to check/normalize (cannot be NULL,
 *                can be identical to sigout)
 *
 *  With ECDSA a third-party can forge a second distinct signature of the same
 *  message, given a single initial signature, but without knowing the key. This
 *  is done by negating the S value modulo the order of the curve, 'flipping'
 *  the sign of the random point R which is not included in the signature.
 *
 *  Forgery of the same message isn't universally problematic, but in systems
 *  where message malleability or uniqueness of signatures is important this can
 *  cause issues. This forgery can be blocked by all verifiers forcing signers
 *  to use a normalized form.
 *
 *  The lower-S form reduces the size of signatures slightly on average when
 *  variable length encodings (such as DER) are used and is cheap to verify,
 *  making it a good choice. Security of always using lower-S is assured because
 *  anyone can trivially modify a signature after the fact to enforce this
 *  property anyway.
 *
 *  The lower S value is always between 0x1 and
 *  0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5D576E7357A4501DDFE92F46681B20A0,
 *  inclusive.
 *
 *  No other forms of ECDSA malleability are known and none seem likely, but
 *  there is no formal proof that ECDSA, even with this additional restriction,
 *  is free of other malleability. Commonly used serialization schemes will also
 *  accept various non-unique encodings, so care should be taken when this
 *  property is required for an application.
 *
 *  The secp256k1_ecdsa_sign function will by default create signatures in the
 *  lower-S form, and secp256k1_ecdsa_verify will not accept others. In case
 *  signatures come from a system that cannot enforce this property,
 *  secp256k1_ecdsa_signature_normalize must be called before verification.
 */
SECP256K1_API int secp256k1_ecdsa_signature_normalize(
    const secp256k1_context* ctx,
    secp256k1_ecdsa_signature *sigout,
    const secp256k1_ecdsa_signature *sigin
) SECP256K1_ARG_NONNULL(1) SECP256K1_ARG_NONNULL(3);

/** Verify an ECDSA secret key.
 *
 *  Returns: 1: secret key is valid
 *           0: secret key is invalid
 *  Args:    ctx: pointer to a context object (cannot be NULL)
 *  In:      seckey: pointer to a 32-byte secret key (cannot be NULL)
 */
SECP256K1_API SECP256K1_WARN_UNUSED_RESULT int secp256k1_ec_seckey_verify(
    const secp256k1_context* ctx,
    const unsigned char *seckey
) SECP256K1_ARG_NONNULL(1) SECP256K1_ARG_NONNULL(2);

/** Compute the public key for a secret key.
 *
 *  Returns: 1: secret was valid, public key stores
 *           0: secret was invalid, try again
 *  Args:   ctx:        pointer to a context object, initialized for signing (cannot be NULL)
 *  Out:    pubkey:     pointer to the created public key (cannot be NULL)
 *  In:     seckey:     pointer to a 32-byte private key (cannot be NULL)
 */
SECP256K1_API SECP256K1_WARN_UNUSED_RESULT int secp256k1_ec_pubkey_create(
    const secp256k1_context* ctx,
    secp256k1_pubkey *pubkey,
    const unsigned char *seckey
) SECP256K1_ARG_NONNULL(1) SECP256K1_ARG_NONNULL(2) SECP256K1_ARG_NONNULL(3);

/** Tweak a private key by adding tweak to it.
 * Returns: 0 if the tweak was out of range (chance of around 1 in 2^128 for
 *          uniformly random 32-byte arrays, or if the resulting private key
 *          would be invalid (only when the tweak is the complement of the
 *          private key). 1 otherwise.
 * Args:    ctx:    pointer to a context object (cannot be NULL).
 * In/Out:  seckey: pointer to a 32-byte private key.
 * In:      tweak:  pointer to a 32-byte tweak.
 */
SECP256K1_API SECP256K1_WARN_UNUSED_RESULT int secp256k1_ec_privkey_tweak_add(
    const secp256k1_context* ctx,
    unsigned char *seckey,
    const unsigned char *tweak
) SECP256K1_ARG_NONNULL(1) SECP256K1_ARG_NONNULL(2) SECP256K1_ARG_NONNULL(3);

/** Tweak a public key by adding tweak times the generator to it.
 * Returns: 0 if the tweak was out of range (chance of around 1 in 2^128 for
 *          uniformly random 32-byte arrays, or if the resulting public key
 *          would be invalid (only when the tweak is the complement of the
 *          corresponding private key). 1 otherwise.
 * Args:    ctx:    pointer to a context object initialized for validation
 *                  (cannot be NULL).
 * In/Out:  pubkey: pointer to a public key object.
 * In:      tweak:  pointer to a 32-byte tweak.
 */
SECP256K1_API SECP256K1_WARN_UNUSED_RESULT int secp256k1_ec_pubkey_tweak_add(
    const secp256k1_context* ctx,
    secp256k1_pubkey *pubkey,
    const unsigned char *tweak
) SECP256K1_ARG_NONNULL(1) SECP256K1_ARG_NONNULL(2) SECP256K1_ARG_NONNULL(3);

/** Tweak a private key by multiplying it by a tweak.
 * Returns: 0 if the tweak was out of range (chance of around 1 in 2^128 for
 *          uniformly random 32-byte arrays, or equal to zero. 1 otherwise.
 * Args:   ctx:    pointer to a context object (cannot be NULL).
 * In/Out: seckey: pointer to a 32-byte private key.
 * In:     tweak:  pointer to a 32-byte tweak.
 */
SECP256K1_API SECP256K1_WARN_UNUSED_RESULT int secp256k1_ec_privkey_tweak_mul(
    const secp256k1_context* ctx,
    unsigned char *seckey,
    const unsigned char *tweak
) SECP256K1_ARG_NONNULL(1) SECP256K1_ARG_NONNULL(2) SECP256K1_ARG_NONNULL(3);

/** Tweak a public key by multiplying it by a tweak value.
 * Returns: 0 if the tweak was out of range (chance of around 1 in 2^128 for
 *          uniformly random 32-byte arrays, or equal to zero. 1 otherwise.
 * Args:    ctx:    pointer to a context object initialized for validation
 *                 (cannot be NULL).
 * In/Out:  pubkey: pointer to a public key obkect.
 * In:      tweak:  pointer to a 32-byte tweak.
 */
SECP256K1_API SECP256K1_WARN_UNUSED_RESULT int secp256k1_ec_pubkey_tweak_mul(
    const secp256k1_context* ctx,
    secp256k1_pubkey *pubkey,
    const unsigned char *tweak
) SECP256K1_ARG_NONNULL(1) SECP256K1_ARG_NONNULL(2) SECP256K1_ARG_NONNULL(3);

/** Updates the context randomization.
 *  Returns: 1: randomization successfully updated
 *           0: error
 *  Args:    ctx:       pointer to a context object (cannot be NULL)
 *  In:      seed32:    pointer to a 32-byte random seed (NULL resets to initial state)
 */
SECP256K1_API SECP256K1_WARN_UNUSED_RESULT int secp256k1_context_randomize(
    secp256k1_context* ctx,
    const unsigned char *seed32
) SECP256K1_ARG_NONNULL(1);

/** Add a number of public keys together.
 *  Returns: 1: the sum of the public keys is valid.
 *           0: the sum of the public keys is not valid.
 *  Args:   ctx:        pointer to a context object
 *  Out:    out:        pointer to a public key object for placing the resulting public key
 *                      (cannot be NULL)
 *  In:     ins:        pointer to array of pointers to public keys (cannot be NULL)
 *          n:          the number of public keys to add together (must be at least 1)
 */
SECP256K1_API SECP256K1_WARN_UNUSED_RESULT int secp256k1_ec_pubkey_combine(
    const secp256k1_context* ctx,
    secp256k1_pubkey *out,
    const secp256k1_pubkey * const * ins,
    size_t n
) SECP256K1_ARG_NONNULL(2) SECP256K1_ARG_NONNULL(3);

/** Copies a secp256k1 context object.
 *
 *  Returns: a newly created context object.
 *  Args:    ctx: an existing context to copy (cannot be NULL)
 */
SECP256K1_API secp256k1_context* secp256k1_context_clone(
    const secp256k1_context* ctx
) SECP256K1_ARG_NONNULL(1) SECP256K1_WARN_UNUSED_RESULT;

/** Destroy a secp256k1 context object.
 *
 *  The context pointer may not be used afterwards.
 *  Args:   ctx: an existing context to destroy (cannot be NULL)
 */
SECP256K1_API void secp256k1_context_destroy(
    secp256k1_context* ctx
);


#endif
