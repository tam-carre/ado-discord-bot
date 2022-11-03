# ado-discord-bot

## Features

- Notifications for YouTube community posts with DeepL translation
- Notifications for [Secret Base](https://ado-dokidokihimitsukichi-daigakuimo.com/) livestreams
- Notifications for YouTube livestreams
- Relay of fan-made YouTube live translations & Ado's own livechat messages

### Not yet implemented

- Notifications for new Instagram posts

## Some screenshots

TODO: Secret Base notification screenshot

![1667490505](https://user-images.githubusercontent.com/1331748/199791318-230ca83f-92bb-41b8-b438-4d1fa843a7e6.png)
![1667484508](https://user-images.githubusercontent.com/1331748/199791372-81935228-055d-499c-b4f7-8769209f5880.png)
![1667490496](https://user-images.githubusercontent.com/1331748/199791403-3f2a90b7-2708-49e2-9e3e-005892741b75.png)

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
