#!/usr/bin/env python3

import random
import argparse
from typing import List, Optional

class MillerRabin:
    def __init__(self, rounds: int = 40):
        """Initialize Miller-Rabin test with specified number of rounds.

        Args:
            rounds (int): Number of rounds for testing. Default is 40.
        """
        self.rounds = rounds
        # Known bases that deterministically test numbers up to 2^64
        self.KNOWN_BASES = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]

    def _mod_pow(self, base: int, exponent: int, modulus: int) -> int:
        """Perform modular exponentiation efficiently.

        Args:
            base: Base number
            exponent: Exponent
            modulus: Modulus

        Returns:
            (base ^ exponent) % modulus
        """
        result = 1
        base = base % modulus
        while exponent > 0:
            if exponent & 1:
                result = (result * base) % modulus
            base = (base * base) % modulus
            exponent >>= 1
        return result

    def _check_composite(self, n: int, a: int, d: int, r: int) -> bool:
        """Check if n is composite using a witness value a.

        Args:
            n: Number to test
            a: Witness value
            d: Odd factor of n-1
            r: Power of 2 in factorization of n-1

        Returns:
            True if definitely composite, False if probably prime
        """
        x = self._mod_pow(a, d, n)
        if x == 1 or x == n - 1:
            return False

        for _ in range(r - 1):
            x = (x * x) % n
            if x == n - 1:
                return False
            if x == 1:
                return True
        return True

    def is_prime(self, n: int, specific_bases: Optional[List[int]] = None) -> bool:
        """Test if a number is prime using Miller-Rabin primality test.

        Args:
            n: Number to test for primality
            specific_bases: Optional list of specific bases to use for testing

        Returns:
            True if probably prime, False if definitely composite
        """
        if n <= 1 or n == 4:
            return False
        if n <= 3:
            return True

        # Find r and d such that n-1 = d * 2^r
        r = 0
        d = n - 1
        while d % 2 == 0:
            r += 1
            d //= 2

        # Use either provided bases, known small bases, or random bases
        if specific_bases:
            bases = specific_bases
        elif n < 1_373_653:
            bases = [2, 3]
        elif n < 9_080_191:
            bases = [31, 73]
        elif n < 4_759_123_141:
            bases = [2, 7, 61]
        elif n < 2**64:
            bases = self.KNOWN_BASES
        else:
            bases = [random.randrange(2, min(n - 2, 2**32)) for _ in range(self.rounds)]

        # Test with chosen bases
        for a in bases:
            if a >= n:
                a %= n
            if self._check_composite(n, a, d, r):
                return False
        return True

def main():
    parser = argparse.ArgumentParser(description='Miller-Rabin Primality Test')
    parser.add_argument('number', type=int, help='Number to test for primality')
    parser.add_argument('-r', '--rounds', type=int, default=40,
                        help='Number of rounds for testing (default: 40)')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Show additional information')
    args = parser.parse_args()

    miller_rabin = MillerRabin(rounds=args.rounds)
    is_prime = miller_rabin.is_prime(args.number)

    if args.verbose:
        print(f"Testing {args.number} for primality with {args.rounds} rounds...")
        if args.number < 2**64:
            print("Using deterministic test with fixed bases")
        else:
            print("Using probabilistic test with random bases")

    if is_prime:
        print(f"{args.number} is probably prime")
        if args.number < 2**64:
            print("This result is deterministic up to 2^64")
    else:
        print(f"{args.number} is composite")

if __name__ == "__main__":
    main()
