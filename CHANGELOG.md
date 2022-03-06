# Revision history for neither-data

## 0.2.3.2 -- 2022-03-06

* Clarify README
* Add links to GitHub and Hackage
* Improve Nix flake, renamed `ghc-version` to `ghcVersion`

## 0.2.3.1 -- 2022-02-16

* Moved files in ./doc to top-level
* Moved built haddock files to ./docs
* Added Haddock comment for the Neither type itself

## 0.2.3.0 -- 2022-02-16

* Downgraded the compiler version from 9.2.1 to 8.10.7  
  (This brings the two Nix build ways in line with each other and permits stack)
* Added Stack as a build method
* Added explanatory comments that serve as haddock documentation
* Added documentation in ./doc that lists instances and explains missing ones
* Built Haddock documentation as HTML to ./haddock
* Added instances for:
  * `Data`
  * `Real`
  * `Integral`
  * `Fractional`
  * `Floating`
  * `RealFrac`
  * `Bits`
  * `FiniteBits`
  * `Eq1`
  * `Ord1`
  * `Eq2`
  * `Ord2`

## 0.2.2.0 -- 2022-02-15

* Added instances for:
  * `Num`

## 0.2.1.0 -- 2022-02-15

* Added instances for:
  * `Traversable`
  * `Bifoldable`
  * `Bitraversable`
  * `IsString`
  * `Ix`

## 0.2.0.0 -- 2022-02-14

* Renamed to neither-data
* Added different build methods & updated the cabal one

## 0.1.1.0 -- 2022-02-14

* Removed non-canonical methods
* Added instances for:
  * `MonadFail`
  * `MonadIO`
  * `Foldable`

## 0.1.0.0 -- 2022-02-14

* First version. Released on an unsuspecting world.
