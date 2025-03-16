# YT
IVH <- local({
  # .Internal vhash128 - Dependency-free R hash function. Outputs 128-bit hashes of raw(n) data - v0.2.0
  # https://dull.systems/Internal_vhash128

  # Version history:
  # 0.2.0  (2025-03-16) First public release
  # --------------------------------------------------------------------------

  assert <- function(expr, m) if(!isTRUE(expr)) stop(m)
  ; assert(getRversion() >= '3.0.0', 'This tool requires R >= 3.0.0') # because of bitwShiftR

  vhash <- function(x) 0
  if('vhash' %in% base::builtins(internal = TRUE)){
    # Back in November 2021 Luke Tierney contributed the `vhash` internal R function to 
    # `R/src/main/unique.c` along with this comment:
    # > hash tables (temporary support for R-level experimenting and debugging)
    # We rely on the portion of that function that hashes raw(n) input.
    # At the time of writing, it remains available and unchanged across 4.1.3 <= R <= 4.4.3.
    vhash <- function(v) return(.Internal(vhash(v, 31)))
  }

  vhash_works_as_expected <- function(vhash_f){
    return(vhash_f(charToRaw('')) == 236295992 && vhash_f(charToRaw('dolorem ipsum')) == 238522098)
  }

  if(!vhash_works_as_expected(vhash)){
    vhash <- function(s){
      # Fallback for R < 4.1.3 or in case `vhash` is ever dropped or altered.
      # It's comically slow for anything other than small inputs.
      to_unsigned <- function(x){
        res <- x %% 0x100000000
        out_of_integer_range_mask <- (res >= 2^31)
        res[out_of_integer_range_mask] <- res[out_of_integer_range_mask] - 2^32
        return(as.integer(res))
      }

      scatter_bytes <- function(x){
        res <- to_unsigned(3141592653 * as.integer(x))
        res <- bitwShiftR(res, 1)
        return(res)
      }

      product_modulo <- function(a, b, mod){
        res <- 0
        a <- a %% mod
        while(b > 0){
          if(b %% 2) res <- (res + a) %% mod
          a <- (2*a) %% mod
          b <- b %/% 2
        }
        return(res)
      }

      scatter_int <- function(x){
        res <- to_unsigned(product_modulo(x, 3141592653, 2^32))
        res <- bitwShiftR(res, 1)
        return(res)
      }

      res <- 48 + length(s)*100
      ss <- scatter_bytes(s)
      for(e in ss) res <- bitwXor(to_unsigned(res), e) * 97
      res <- scatter_int(res)
      return(res)
    }
  }
  ; assert(vhash_works_as_expected(vhash), 'The `vhash` base hash function does not behave as expected')

  vhash128 <- function(v){
    # Extension of `vhash` that produces 128-bit hashes.
    # It passes https://github.com/aappleby/smhasher, except for the "seed" tests, because this function lacks a seed.
    
    ; assert(is.raw(v), '`vhash128` can only hash objects of type raw(n)')
    h0 <- vhash(v)                                              # hash the whole `v`
    length(v) <- max(length(v), 16)                             # pad `v` if shorter than 16 bytes
    lengths <- length(v) %/% 4  + (seq(0, 3) < length(v) %% 4)  # divide `v` into four roughly equal portions
  
    # Generate 31 bits of hash for each of the four independent blocks.
    # `h0` is xor'ed as a seed to make all blocks dependent on all data.
    result <- raw(16)
    prev_hash <- writeBin(h0, con = raw(), endian = 'little')
    from <- 1
    for(i in seq(0, 3)){
      v[from:(from+3)] <- xor(v[from:(from+3)], prev_hash)      # xor prev_hash into first 4 bytes of current block
      n <- lengths[i+1]
      
      target_bytes <- (4*i+1):(4*(i+1))
      result[target_bytes] <- writeBin(vhash(v[from:(from+n-1)]), con = raw(), endian = 'little') # hash one block
      
      from <- from + n
      prev_hash <- xor(prev_hash, result[target_bytes])         # combine latest hash into prev
    }
    
    # Repurpose bits 27--30 of `h0` as 32nd bit of each of the four blocks of `result` to get the full 128-bit hash
    for(i in seq_len(4)) if(bitwAnd(h0, bitwShiftL(1, 31-i)) > 0) result[[4*i]] <- result[[4*i]] | as.raw(c(0x80))
    
    return(result)
  }

  vhash128_test <- function(){
    ; assert(paste(vhash128(charToRaw('')), collapse = '') == '3156ba7b88b82c73f0da542a6f1c75ff', 
             '`vhash128` does not behave as expected')
    ; assert(paste(vhash128(charToRaw('ABCDEFGHIJKLMNOPQRSTUVWXYZ')), collapse = '') == 
               '8034c2519633ff56b0f7ca94b2587c90', '`vhash128` does not behave as expected')
  }

  vhash128_benchmark <- function(){
    vhash128_test()
    inputs <- list(raw(1e0), raw(1e1), raw(1e2), raw(1e3), raw(1e4), raw(1e5))
    res <- numeric(0)
    for(input in inputs){
      t0 <- Sys.time()
      vhash128(input)
      t1 <- Sys.time()
      res <- c(res, t1-t0)
    }
    return(res)
  }

  return(list(hash = vhash128, test = vhash128_test, benchmark = vhash128_benchmark))
})
