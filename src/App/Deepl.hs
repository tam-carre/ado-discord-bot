module App.Deepl (translate) where

import App.Deepl.Internal (Tl (..), translateApi)
import App.Network        (fetchJson)
import App.Utils          ((<&>>=))

----------------------------------------------------------------------------------------------------

translate ∷ MonadIO m ⇒ Text → m (Either Text Text)
translate txt =
  fetchJson (translateApi txt)
    <&>>= \(Tl srclang tl) → case srclang of
      "EN" → Left "[DeepL] Source text was already English"
      _    → Right tl
