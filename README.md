# README UNDER DEVELOPMENT

# Optimization Algorithms

This haskell library provide a bunch of algorithms to solve optimization problems (e.g. knacksap problem, TSP, scheduling, etc). Also include algorithms to optimize functions in continuous search spaces.

This project has been developed as part of my bachelor thesis and to improve my Haskell. So, I have trying to minimize the use of others libraries, except the strictly necessary ones. In a near future, it will be upgrade using good third-party libraries to improve performance, among other things. E.g. Data.Vector instead of Data.List.

## Getting Started

To install this library and use it in your haskell project, you can downoad it using `stack` or `cabal`.

#### Stack

```
stack install optimization-algorithms
```

#### Cabal

```
cabal install optimization-algorithms
```

### Installing from source

#### Unix or Linux

Download it:

```
git clone https://github.com/mizunno/optimization-algorithms.git
```

Unpack the tar file:
```
tar xzf optimization-algorithms-XXX.tar.gz
```

Move into:
```
cd optimization-algorithms-XXX/
```

Install it globally:
```
runhaskell Setup configure
runhaskell Setup build
sudo runhaskell Setup install 
```

For more information about installation, see https://wiki.haskell.org/Cabal/How_to_install_a_Cabal_package

#### Windows

See https://wiki.haskell.org/Cabal/How_to_install_a_Cabal_package