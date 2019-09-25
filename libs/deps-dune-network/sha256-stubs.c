
#include <caml/mlvalues.h>
#include <caml/alloc.h>

#include "sha256.h"

value sha256_ml(value out_v, value in_v, value is224_v)
{
  void* out = String_val(out_v);
  void* in = String_val(in_v);
  size_t outlen = caml_string_length(out_v);
  size_t inlen = caml_string_length(in_v);
  int is224 = Bool_val(is224_v);
  
  if( outlen != 32 ) return Val_int(-1);

  mbedtls_sha256( in, inlen, out, is224);
  
  return Val_int(0);
}

value sha256_size_of_context_ml(value unit_v)
{
  return Val_int(sizeof(mbedtls_sha256_context));
}

value sha256_init_ml(value ctx_v)
{
  mbedtls_sha256_context* ctx = (mbedtls_sha256_context*)String_val(ctx_v);
  mbedtls_sha256_init( ctx );
  mbedtls_sha256_starts( ctx, 0 ); // 0 = is224 
  return ctx_v;
}

value sha256_update_ml(value ctx_v, value input_v)
{
  mbedtls_sha256_context* ctx = (mbedtls_sha256_context*)String_val(ctx_v);
  void *input = String_val(input_v);
  size_t inlen = caml_string_length(input_v);

  mbedtls_sha256_update( ctx, input, inlen );
  return Val_unit;
}

value sha256_final_ml(value ctx_v, value output_v)
{
  mbedtls_sha256_context* ctx = (mbedtls_sha256_context*)String_val(ctx_v);
  void *output = String_val(output_v);
  mbedtls_sha256_finish( ctx, output );
  return Val_unit;
}
