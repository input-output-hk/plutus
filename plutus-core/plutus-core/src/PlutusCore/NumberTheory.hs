{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module PlutusCore.NumberTheory where

import           Numeric.GMP.Raw.Safe (mpz_invert, mpz_powm, mpz_probab_prime_p)
import           Numeric.GMP.Utils    (withInInteger, withOutInteger_)
import           System.IO.Unsafe     (unsafePerformIO)

-- Modular exponentiation
-- Function returns undefined when e=0.
-- Function evaluates 0^0 = 1
powMod :: forall a. Integral a => a -> a -> a -> Integer
powMod b e m =
  unsafePerformIO $
    withOutInteger_ $ \rop ->
      withInInteger (toInteger b) $ \bop ->
        withInInteger (toInteger e) $ \eop ->
          withInInteger (toInteger m) $ \mop ->
              mpz_powm rop bop eop mop

-- Modular multiplicative inverse
-- Function returns undefined when e=0
-- Function returns undefined when inverse does not exist
-- Function returns 0 when abs(m)=1
invert :: forall a. Integral a => a -> a -> Integer
invert i m =
  unsafePerformIO $
    withOutInteger_ $ \rop ->
      withInInteger (toInteger i) $ \iop ->
        withInInteger (toInteger m) $ \mop ->
            mpz_invert rop iop mop

-- Primality test
-- MPZ function returns 2 if n is definitely prime
-- MPZ function returns 1 if n is probably prime (without being certain)
-- MPZ function returns 0 if n is definitely non-prime.
probablyPrime :: forall a. Integral a => a -> a -> Integer
probablyPrime n l =
  toInteger $
    unsafePerformIO $
        withInInteger (toInteger n) $ \nop ->
              mpz_probab_prime_p nop (fromIntegral l)
