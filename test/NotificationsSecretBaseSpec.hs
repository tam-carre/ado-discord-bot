{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module NotificationsSecretBaseSpec (spec) where

-- Ado Bot modules
import Notifications.SecretBase.Internal (Lives (..), SecretBaseLive (..), Vids (..))

-- Downloaded libraries
import Test.Hspec (Spec, shouldBe, it)
import Data.Aeson (eitherDecode)

-------------------------------------------------------------------------------

spec :: Spec
spec = do
  it "Should succeed parsing a valid payload for livestreams" $ do
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

  it "should succeed parsing a valid payload for vids" $ do
    let
      validPayload = "{\"data\":{\"video_pages\":{\"list\":[{\"active_video_filename\":{\"id\":18289,\"length\":1375,\"video_filename_type\":{\"id\":1,\"value\":\"dummy\"}},\"content_code\":\"dummy\",\"released_at\":\"2022-11-11 12:30:00\",\"start_with_free_part_flg\":false,\"thumbnail_url\":\"dummy\",\"title\":\"dummy\",\"user_video_play_history\":{\"id\":164197071,\"last_played_time\":174},\"video_aggregate_info\":{\"id\":42334732,\"number_of_comments\":113,\"total_views\":3625},\"video_comment_setting\":{\"allow_comment_flg\":true,\"disp_main_comment_flg\":true,\"disp_number_of_comment_flg\":true,\"disp_number_of_views_flg\":true,\"disp_rightside_comment_flg\":true},\"video_delivery_target\":{\"id\":2},\"video_free_periods\":[]},{\"active_video_filename\":{\"id\":16009,\"length\":6207,\"video_filename_type\":{\"id\":3,\"value\":\"dummy\"}},\"content_code\":\"dummy\",\"released_at\":\"2022-09-30 13:32:38\",\"start_with_free_part_flg\":false,\"thumbnail_url\":\"dummy\",\"title\":\"dummy\",\"user_video_play_history\":{\"id\":148429191,\"last_played_time\":0},\"video_aggregate_info\":{\"id\":32153609,\"number_of_comments\":14808,\"total_views\":57772},\"video_comment_setting\":{\"allow_comment_flg\":true,\"disp_main_comment_flg\":true,\"disp_number_of_comment_flg\":true,\"disp_number_of_views_flg\":true,\"disp_rightside_comment_flg\":true},\"video_delivery_target\":{\"id\":1},\"video_free_periods\":[]},{\"active_video_filename\":{\"id\":15348,\"length\":611,\"video_filename_type\":{\"id\":1,\"value\":\"dummy\"}},\"content_code\":\"dummy\",\"released_at\":\"2022-09-22 11:10:00\",\"start_with_free_part_flg\":false,\"thumbnail_url\":\"dummy\",\"title\":\"dummy\",\"user_video_play_history\":null,\"video_aggregate_info\":{\"id\":30161718,\"number_of_comments\":531,\"total_views\":9118},\"video_comment_setting\":{\"allow_comment_flg\":true,\"disp_main_comment_flg\":true,\"disp_number_of_comment_flg\":true,\"disp_number_of_views_flg\":true,\"disp_rightside_comment_flg\":true},\"video_delivery_target\":{\"id\":1},\"video_free_periods\":[]},{\"active_video_filename\":{\"id\":13781,\"length\":5456,\"video_filename_type\":{\"id\":3,\"value\":\"dummy\"}},\"content_code\":\"dummy\",\"released_at\":\"2022-08-30 11:11:36\",\"start_with_free_part_flg\":false,\"thumbnail_url\":\"dummy\",\"title\":\"dummy\",\"user_video_play_history\":null,\"video_aggregate_info\":{\"id\":24878920,\"number_of_comments\":22409,\"total_views\":74094},\"video_comment_setting\":{\"allow_comment_flg\":true,\"disp_main_comment_flg\":true,\"disp_number_of_comment_flg\":true,\"disp_number_of_views_flg\":true,\"disp_rightside_comment_flg\":true},\"video_delivery_target\":{\"id\":1},\"video_free_periods\":[]},{\"active_video_filename\":{\"id\":10829,\"length\":5354,\"video_filename_type\":{\"id\":3,\"value\":\"dummy\"}},\"content_code\":\"dummy\",\"released_at\":\"2022-07-31 11:42:17\",\"start_with_free_part_flg\":false,\"thumbnail_url\":\"dummy\",\"title\":\"dummy\",\"user_video_play_history\":null,\"video_aggregate_info\":{\"id\":18393007,\"number_of_comments\":9974,\"total_views\":31367},\"video_comment_setting\":{\"allow_comment_flg\":true,\"disp_main_comment_flg\":true,\"disp_number_of_comment_flg\":true,\"disp_number_of_views_flg\":true,\"disp_rightside_comment_flg\":true},\"video_delivery_target\":{\"id\":1},\"video_free_periods\":[]},{\"active_video_filename\":{\"id\":9105,\"length\":1160,\"video_filename_type\":{\"id\":1,\"value\":\"dummy\"}},\"content_code\":\"dummy\",\"released_at\":\"2022-07-09 15:00:00\",\"start_with_free_part_flg\":false,\"thumbnail_url\":\"dummy\",\"title\":\"dummy\",\"user_video_play_history\":null,\"video_aggregate_info\":{\"id\":14522910,\"number_of_comments\":230,\"total_views\":18187},\"video_comment_setting\":{\"allow_comment_flg\":true,\"disp_main_comment_flg\":true,\"disp_number_of_comment_flg\":true,\"disp_number_of_views_flg\":true,\"disp_rightside_comment_flg\":true},\"video_delivery_target\":{\"id\":2},\"video_free_periods\":[]},{\"active_video_filename\":{\"id\":8628,\"length\":7125,\"video_filename_type\":{\"id\":3,\"value\":\"dummy\"}},\"content_code\":\"dummy\",\"released_at\":\"2022-06-30 10:08:50\",\"start_with_free_part_flg\":false,\"thumbnail_url\":\"dummy\",\"title\":\"dummy\",\"user_video_play_history\":null,\"video_aggregate_info\":{\"id\":13074752,\"number_of_comments\":13351,\"total_views\":25295},\"video_comment_setting\":{\"allow_comment_flg\":true,\"disp_main_comment_flg\":true,\"disp_number_of_comment_flg\":true,\"disp_number_of_views_flg\":true,\"disp_rightside_comment_flg\":true},\"video_delivery_target\":{\"id\":1},\"video_free_periods\":[]},{\"active_video_filename\":{\"id\":6865,\"length\":7168,\"video_filename_type\":{\"id\":3,\"value\":\"dummy\"}},\"content_code\":\"dummy\",\"released_at\":\"2022-06-01 16:00:00\",\"start_with_free_part_flg\":true,\"thumbnail_url\":\"dummy\",\"title\":\"dummy\",\"user_video_play_history\":{\"id\":155539943,\"last_played_time\":7146},\"video_aggregate_info\":{\"id\":9188138,\"number_of_comments\":26614,\"total_views\":91415},\"video_comment_setting\":{\"allow_comment_flg\":true,\"disp_main_comment_flg\":true,\"disp_number_of_comment_flg\":true,\"disp_number_of_views_flg\":true,\"disp_rightside_comment_flg\":true},\"video_delivery_target\":{\"id\":1},\"video_free_periods\":[{\"elapsed_ended_time\":3344,\"elapsed_started_time\":0,\"id\":5664}]},{\"active_video_filename\":{\"id\":6406,\"length\":755,\"video_filename_type\":{\"id\":1,\"value\":\"dummy\"}},\"content_code\":\"dummy\",\"released_at\":\"2022-05-25 11:40:00\",\"start_with_free_part_flg\":false,\"thumbnail_url\":\"dummy\",\"title\":\"dummy\",\"user_video_play_history\":null,\"video_aggregate_info\":{\"id\":8236275,\"number_of_comments\":79,\"total_views\":4028},\"video_comment_setting\":{\"allow_comment_flg\":true,\"disp_main_comment_flg\":true,\"disp_number_of_comment_flg\":true,\"disp_number_of_views_flg\":true,\"disp_rightside_comment_flg\":true},\"video_delivery_target\":{\"id\":1},\"video_free_periods\":[]},{\"active_video_filename\":{\"id\":6094,\"length\":499,\"video_filename_type\":{\"id\":1,\"value\":\"dummy\"}},\"content_code\":\"dummy\",\"released_at\":\"2022-05-20 13:00:00\",\"start_with_free_part_flg\":false,\"thumbnail_url\":\"dummy\",\"title\":\"dummy\",\"user_video_play_history\":null,\"video_aggregate_info\":{\"id\":7660724,\"number_of_comments\":595,\"total_views\":10991},\"video_comment_setting\":{\"allow_comment_flg\":true,\"disp_main_comment_flg\":true,\"disp_number_of_comment_flg\":true,\"disp_number_of_views_flg\":true,\"disp_rightside_comment_flg\":true},\"video_delivery_target\":{\"id\":1},\"video_free_periods\":[]},{\"active_video_filename\":{\"id\":5090,\"length\":217,\"video_filename_type\":{\"id\":1,\"value\":\"dummy\"}},\"content_code\":\"dummy\",\"released_at\":\"2022-04-30 12:03:08\",\"start_with_free_part_flg\":false,\"thumbnail_url\":\"dummy\",\"title\":\"dummy\",\"user_video_play_history\":null,\"video_aggregate_info\":{\"id\":5644560,\"number_of_comments\":114,\"total_views\":3931},\"video_comment_setting\":{\"allow_comment_flg\":true,\"disp_main_comment_flg\":true,\"disp_number_of_comment_flg\":true,\"disp_number_of_views_flg\":true,\"disp_rightside_comment_flg\":true},\"video_delivery_target\":{\"id\":1},\"video_free_periods\":[]},{\"active_video_filename\":{\"id\":3474,\"length\":569,\"video_filename_type\":{\"id\":1,\"value\":\"dummy\"}},\"content_code\":\"dummy\",\"released_at\":\"2022-04-04 00:25:16\",\"start_with_free_part_flg\":false,\"thumbnail_url\":\"dummy\",\"title\":\"dummy\",\"user_video_play_history\":null,\"video_aggregate_info\":{\"id\":3418278,\"number_of_comments\":241,\"total_views\":11733},\"video_comment_setting\":{\"allow_comment_flg\":true,\"disp_main_comment_flg\":false,\"disp_number_of_comment_flg\":true,\"disp_number_of_views_flg\":true,\"disp_rightside_comment_flg\":true},\"video_delivery_target\":{\"id\":1},\"video_free_periods\":[]}],\"total\":26}}}"

    isRight (eitherDecode @Vids validPayload) `shouldBe` True
