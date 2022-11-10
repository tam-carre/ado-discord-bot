{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

module NotificationsSecretBaseSpec (spec) where

-- Ado Bot modules
import Notifications.SecretBase.Internal (Lives (..), SecretBaseLive (..))

-- Downloaded libraries
import Test.Hspec (Spec, shouldBe, it)
import Data.Aeson (eitherDecode)

-------------------------------------------------------------------------------

spec :: Spec
spec =
  it "Should succeed parsing a valid payload" $ do
    let
      expectedResult =
        Right $ Lives
          [ SecretBaseLive
            { _thumb = "https://ado-dokidokihimitsukichi-daigakuimo.com/public_html/contents/video_pages/7646/thumbnail_path"
            , _desc = "Watch at <https://ado-dokidokihimitsukichi-daigakuimo.com/video/smWXYmwkUvjjgDReUjDbE4Le>"
            , _title = "jptxt"
            , _url = "https://ado-dokidokihimitsukichi-daigakuimo.com/video/smWXYmwkUvjjgDReUjDbE4Le"
            , _started = True
            }
          ]

      validPayload = "{ \"data\": { \"video_pages\": { \"list\": [ { \"active_video_filename\": null, \"closed_at\": null, \"content_code\": \"smWXYmwkUvjjgDReUjDbE4Le\", \"description\": null, \"live_finished_at\": null, \"live_scheduled_end_at\": \"2022-09-30 15:30:00\", \"live_scheduled_start_at\": \"2022-09-30 14:00:00\", \"live_started_at\": \"2022-09-30 14:00:45\", \"released_at\": \"2022-09-30 13:32:38\", \"start_with_free_part_flg\": false, \"thumbnail_url\": \"https://ado-dokidokihimitsukichi-daigakuimo.com/public_html/contents/video_pages/7646/thumbnail_path\", \"title\": \"jptxt\", \"user_video_play_history\": null, \"video_aggregate_info\": { \"id\": 32153609, \"number_of_comments\": 9670, \"total_views\": 20034 }, \"video_comment_setting\": { \"allow_comment_flg\": true, \"disp_main_comment_flg\": true, \"disp_number_of_comment_flg\": true, \"disp_number_of_views_flg\": true, \"disp_rightside_comment_flg\": true }, \"video_delivery_target\": { \"display_name\": \"jptxt\", \"id\": 1 }, \"video_free_periods\": [] } ], \"total\": 1 } } }"

    eitherDecode @Lives validPayload `shouldBe` expectedResult
