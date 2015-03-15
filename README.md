# pushbullet

Pushbullet API written in haskell

## Installation

clone the source directory and add the dependency 'pusbullet' to your package in the .cabal file and then run

```bash
cabal sandbox add-source /path/to/clone/
cabal configure
cabal install
```

## Usage

The only way to send a message is with the sendPush function.

```haskell

sendPush (pushSecret "SomethingReallySecret") Nothing $ PushNote "Test" "Some message"
```

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

Submit a pull request
