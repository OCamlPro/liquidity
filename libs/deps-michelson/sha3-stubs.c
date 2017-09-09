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

#include "sha3-ref.h"

value sha3_size_of_context_ml(value unit_v)
{
  return Val_int(sizeof(sha3_context));
}

value sha3_init_ml(value ctx_v, value kind_v)
{
  sha3_context* ctx = (sha3_context*)String_val(ctx_v);
  int kind = Int_val(kind_v);
  if(kind == 0) sha3_Init256( ctx );
  else if(kind == 1) sha3_Init384( ctx );
  else sha3_Init512( ctx );
  return ctx_v;
}

value sha3_update_ml(value ctx_v, value input_v)
{
  sha3_context* ctx = (sha3_context*)String_val(ctx_v);
  void *input = String_val(input_v);
  size_t inlen = caml_string_length(input_v);

  sha3_Update( ctx, input, inlen );
  return Val_unit;
}

value sha3_final_ml(value ctx_v, value output_v)
{
  sha3_context* ctx = (sha3_context*)String_val(ctx_v);
  void *output = String_val(output_v);
  void const * res = sha3_Finalize( ctx );
  
  memcpy(output, res, caml_string_length(output_v));

  return Val_unit;
}
