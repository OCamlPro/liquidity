/**************************************************************************/
/*                                                                        */
/*    Copyright (c) 2017       .                                          */
/*    Fabrice Le Fessant, INRIA & OCamlPro SAS <fabrice@lefessant.net>    */
/*                                                                        */
/*    All rights reserved. No warranty, explicit or implicit, provided.   */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>

#include "blake2.h"

value blake2b_ml(value out_s, value in_s, value key_s)
{
  void* out = String_val(out_s);
  void* in = String_val(in_s);
  void* key = String_val(key_s);
  size_t outlen = caml_string_length(out_s);
  size_t inlen = caml_string_length(in_s);
  size_t keylen = caml_string_length(key_s);
  
  int res = blake2b(out, outlen, in, inlen, key, keylen);
  
  return Val_int(res);
}


value blake2b_size_of_context_ml(value unit_v)
{
  return Val_int(sizeof(blake2b_state));
}

value blake2b_init_ml(value ctx_v, value outlen_v)
{
  blake2b_state* ctx = (blake2b_state*)String_val(ctx_v);
  size_t outlen = Int_val(outlen_v);
  blake2b_init( ctx, outlen );
  
  return ctx_v;
}

value blake2b_init_key_ml(value ctx_v, value outlen_v,value key_v)
{
  blake2b_state* ctx = (blake2b_state*)String_val(ctx_v);
  size_t outlen = Int_val(outlen_v);
  void* key = String_val(key_v);
  size_t keylen = caml_string_length(key_v);
  blake2b_init_key( ctx, outlen, key, keylen );
  
  return Val_unit;
}

value blake2b_update_ml(value ctx_v, value input_v)
{
  blake2b_state* ctx = (blake2b_state*)String_val(ctx_v);
  void *input = String_val(input_v);
  size_t inlen = caml_string_length(input_v);

  blake2b_update( ctx, input, inlen );
  return Val_unit;
}

value blake2b_final_ml(value ctx_v, value output_v)
{
  blake2b_state* ctx = (blake2b_state*)String_val(ctx_v);
  void *output = String_val(output_v);
  int outlen = caml_string_length(output_v);
  blake2b_final( ctx, output,outlen );
  return Val_unit;
}
