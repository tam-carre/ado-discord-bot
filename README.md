# ado-discord-bot

## Features

- Notifications for YouTube community posts with DeepL translation
- Notifications for [Secret Base](https://ado-dokidokihimitsukichi-daigakuimo.com/) livestreams and videos
- Notifications for YouTube livestreams
- Relay of fan-made YouTube live translations & Ado's own livechat messages

### Not yet implemented

- Notifications for new Instagram posts (might be unrealistic due to Instagram
bot detection)

## Tests/Linting

Interactions with HTTP APIs are thoroughly tested in the [`test`](https://github.com/tam-carre/ado-discord-bot/tree/main/test) folder, as these functionalities are critical and the type system does not guarantee full correctness. It tests both hardcoded dummy data and real network calls.

[Custom HLint rules](https://github.com/tam-carre/ado-discord-bot/blob/main/.hlint.yaml#L3290)
are used to make custom wrappers/utility functions discoverable.

## Some screenshots

TODO:
- Secret Base notification screenshot for vids and streams

![1667490505](https://user-images.githubusercontent.com/1331748/199791318-230ca83f-92bb-41b8-b438-4d1fa843a7e6.png)
![1667484508](https://user-images.githubusercontent.com/1331748/199791372-81935228-055d-499c-b4f7-8769209f5880.png)
![1668256126](https://user-images.githubusercontent.com/1331748/201473949-e0c88812-22e2-4005-9356-d10bc9f8e550.png)

## Running

TODO:
- Check this works
- List not automatically installed C libraries

```sh
# First install the latest Cabal using GHCup:
# https://www.haskell.org/ghcup/

# Install node modules
cd masterchat
npm install

# Go back to root directory
cd ..

# Build and run
cabal build && cabal run ado-discord-bot-exe
```
