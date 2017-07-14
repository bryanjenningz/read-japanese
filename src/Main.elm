module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)


type alias Model =
    { input : String
    , selectedIndex : Int
    , dict : Dict String String
    , savedWords : List SavedWord
    , route : Route
    }


type alias SavedWord =
    { word : Word
    , points : Int
    }


type alias Word =
    { kanji : String
    , kana : String
    , definition : String
    }


type StudyResponse
    = Fail
    | Ok
    | Good


type Route
    = Read
    | Study
    | Saved
    | Profile


type Msg
    = Input String
    | Select Int
    | GoToRoute Route


(?:) : Bool -> ( a, a ) -> a
(?:) condition ( ifTrue, ifFalse ) =
    if condition then
        ifTrue
    else
        ifFalse


init : ( Model, Cmd Msg )
init =
    ( Model mockText 0 mockDict mockSavedWords Read, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col-12 col-sm-8 offset-sm-2", style [ ( "height", "100%" ) ] ]
            [ case model.route of
                Read ->
                    viewRead model

                Study ->
                    viewStudy model

                Saved ->
                    viewSaved model

                Profile ->
                    viewProfile model
            , viewRoutes model.route
            ]
        ]


viewRead : Model -> Html Msg
viewRead model =
    let
        ( entry, entryLength ) =
            lookupEntry model.dict model.selectedIndex model.input
    in
        div []
            [ {- textarea
                     [ onInput Input
                     , class "form-control"
                     , placeholder "Paste in Japanese text"
                     , value model.input
                     ]
                     []
                 ,
              -}
              div [ class "p-4" ]
                (List.indexedMap
                    (viewCharacter model.selectedIndex entryLength)
                    (String.split "" model.input)
                )
            , viewEntry entry
            ]


viewEntry : String -> Html Msg
viewEntry entry =
    div
        [ class "card"
        , style
            [ ( "position", "absolute" )
            , ( "left", "0" )
            , ( "right", "0" )
            , ( "bottom", "10%" )
            ]
        ]
        [ div [ class "card-block" ]
            (List.map
                (\e ->
                    div [ class "row" ]
                        [ div [ class "col-2" ]
                            [ button [ class "btn btn-primary" ]
                                [ text "+" ]
                            ]
                        , div [ class "col-10" ] [ text e ]
                        ]
                )
                (String.split "\n" entry
                    |> List.filter (not << String.isEmpty << String.trim)
                )
            )
        ]


lookupEntry : Dict String String -> Int -> String -> ( String, Int )
lookupEntry dict index text =
    if isBetween 0 (String.length text - 1) index then
        case Dict.get (String.dropLeft index text) dict of
            Nothing ->
                lookupEntry dict index (String.dropRight 1 text)

            Just entry ->
                ( entry, String.length text - index )
    else
        ( "", 0 )


isBetween : Int -> Int -> Int -> Bool
isBetween low high value =
    low <= value && value <= high


viewCharacter : Int -> Int -> Int -> String -> Html Msg
viewCharacter selectedIndex entryLength index character =
    h3
        [ onClick (Select index)
        , class
            ("d-inline-block "
                ++ (characterColor
                        (isBetween
                            selectedIndex
                            (selectedIndex + entryLength - 1)
                            index
                        )
                   )
            )
        ]
        [ text character ]


characterColor : Bool -> String
characterColor isSelected =
    isSelected ?: ( "bg-primary text-white", "" )


viewStudy : Model -> Html Msg
viewStudy model =
    div [] [ text "study" ]


viewSaved : Model -> Html Msg
viewSaved model =
    div []
        (List.indexedMap
            (\i savedWord ->
                div [ class "card" ]
                    [ div [ class "card-block" ]
                        [ div [ class "row" ]
                            [ div [ class "col-2" ]
                                [ text savedWord.word.kanji ]
                            , div [ class "col-2" ]
                                [ text savedWord.word.kana ]
                            , div [ class "col-8" ]
                                [ text savedWord.word.definition ]
                            ]
                        ]
                    ]
            )
            model.savedWords
        )


viewProfile : Model -> Html Msg
viewProfile model =
    div [ class "jumbotron mb-0", style [ ( "height", "100%" ) ] ]
        [ h1 [ class "text-center" ]
            [ text "Profile" ]
        , div [ class "card" ]
            [ div [ class "card-block" ]
                [ h4 [ class "text-center" ]
                    [ text "You've learned 6 words today" ]
                , div [ class "progress my-4" ]
                    [ div [ class "progress-bar", style [ ( "width", "60%" ) ] ]
                        [ text "6 / 10" ]
                    ]
                , h5 [ class "text-center" ]
                    [ text "Your daily goal is 10 new words" ]
                ]
            ]
        , button [ class "btn btn-primary btn-block my-4" ]
            [ text "Log Out" ]
        ]


viewRoutes : Route -> Html Msg
viewRoutes route =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "bottom", "0" )
            , ( "width", "100%" )
            ]
        ]
        [ div [ class "row" ]
            [ viewRoute route Read "Read"
            , viewRoute route Study "Study"
            , viewRoute route Saved "Saved"
            , viewRoute route Profile "Profile"
            ]
        ]


viewRoute : Route -> Route -> String -> Html Msg
viewRoute currentRoute route routeText =
    div [ class "col-3 px-0" ]
        [ div [ class "card", onClick (GoToRoute route) ]
            [ div
                [ class
                    ("card-block text-center "
                        ++ routeColor (currentRoute == route)
                    )
                ]
                [ text routeText ]
            ]
        ]


routeColor : Bool -> String
routeColor isCurrentRoute =
    isCurrentRoute ?: ( "bg-primary text-white", "text-primary" )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            ( { model | input = input }, Cmd.none )

        Select index ->
            ( { model | selectedIndex = index }, Cmd.none )

        GoToRoute route ->
            ( { model | route = route }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Mock text from this url: https://news.yahoo.co.jp/pickup/6246844


mockText : String
mockText =
    """負けて１番悔い無しの王者相手に"""


mockSavedWords : List SavedWord
mockSavedWords =
    [ SavedWord (Word "負ける" "まける" "(v1,vi) to lose; to be defeated; (P)") 1
    , SavedWord (Word "番" "ばん" "(n) number (in a series); (one's) turn; watch; guard; lookout; bout, match (sumo)") 1
    , SavedWord (Word "悔い" "くい" "(n) regret; repentance; (P)") 1
    , SavedWord (Word "無し" "なし" "(n,n-suf,uk) without; (P)") 1
    , SavedWord (Word "王者" "おうじゃ" "(n) king; monarch; ruler; (P)") 1
    , SavedWord (Word "相手" "あいて" "(n) companion; partner; company; other party; addressee; opponent (sports, etc.); (P)") 1
    ]


mockDict : Dict String String
mockDict =
    Dict.fromList
        [ ( "負けて"
          , """
        負ける まける (v1,vi) to lose; to be defeated; (P)
        負け  まけ  (n) defeat; loss; losing (a game); (P)
        負 おい  (n) wooden box carried on one's back to store items for a pilgrimage
        負 ふ (n) negative; minus
        """
          )
        , ( "け"
          , """
        家   け   (suf) house (e.g. of Tokugawa); family; (P)
        毛   け   (n) hair; fur; (P)
        け   (prt) particle indicating that the speaker is trying to recall some information
        ケ   (ctr) (read as "ka") counter for the ichi-ni-san counting system (usu. directly preceding the item being counted); a noun read using its on-yomi
        仮   け   (n) lacking substance and existing in name only; something without substance
        卦   け   (n) divination sign
        殊   け   (n,adj-na,n-pref,arch) difference (from one another); different thing; other; unusual; extraordinary
        """
          )
        , ( "て"
          , """
        て (prt) casual quoting particle; (P)
        手   て   (n,col) hand; arm; forepaw; foreleg; handle; hand; worker; help; trouble; care; effort; means; way; trick; move; technique; workmanship; hand; handwriting; kind; type; sort; one's hands; (P)
          """
          )
        , ( "１"
          , """
        一 いち  (num,pref,suf) one; best in; the most (...) in (where an adjective follows); (P)
        一   ひと  (num,pref,suf) one; best in; the most (...) in (where an adjective follows); (P)
        一   ひとつ (n,n-adv,io) one; for one thing (often used in itemized lists); (after a noun) only; (with a verb in negative form) (not) even; just (i.e. "just try it"); (P)
          """
          )
        , ( "番"
          , """
        番 つがい (n,uk) (one) pair (e.g. of birds); brace; couple; joint; hinge
        番 ばん  (n) number (in a series); (one's) turn; watch; guard; lookout; bout, match (sumo)
        """
          )
        , ( "悔い"
          , """
        悔い  くい  (n) regret; repentance; (P)
        悔いる くいる (v1,vt) to regret
        """
          )
        , ( "い"
          , """
        五    い   (num) five; (P)
        位   い   (ctr) place; rank; decimal place; counter for ghosts; (P)
        胃   い   (n) stomach; Chinese "stomach" constellation (one of the 28 mansions); (P)
        イ   (n) 1st in a sequence denoted by the iroha system; 1st note in the diatonic scale (used in key names, etc.)
        五十  い   (n) fifty
        井   い   (n) well
        亥   い   (n) twelfth sign of the Chinese zodiac (The Boar, 9pm-11pm, north-northwest, October)
        """
          )
        , ( "無し"
          , """
        無し  なし  (n,n-suf,uk) without; (P)
        無 むち  (adj-na,n) ignorance; (P)
        無 む (n,pref) nothing; naught; nought; nil; zero; un-; non-
        """
          )
        , ( "し"
          , """
        し (prt,conj) (at the end of a phrase) notes one (of several) reasons; (P)
        四   し   (num) four; (P)
        市   し   (n,n-suf) city; (P)
        し   (n,obsc) 10^24 (kanji is JIS X 0212 kuten 4906); septillion (American); (obs) quadrillion (British)
        シ   (n) ti; si; 7th note in the tonic solfa representation of the diatonic scale
        仕   し   (n) official; civil service
        其   し   (n,arch) that; you; oneself; themself
        """
          )
        , ( "の"
          , """
        乃 の (prt,uk) indicates possessive; verb and adjective nominalizer (nominaliser); substituting for "ga" in subordinate phrases; indicates a confident conclusion; emotional emphasis (sentence end) (fem); (P)
        埜 の (n,n-pref) plain; field; hidden (structural) member; wild; lacking a political post; (P)
        野 の (n,n-pref) plain; field; hidden (structural) member; wild; lacking a political post; (P)
        の (prt) indicates possessive among other uses (for full details and examples see the main entry (linked))
        之 の (prt,arch) possessive (used on tombs, etc.)
        """
          )
        , ( "王者"
          , """
        王者  おうじゃ  (n) king; monarch; ruler; (P)
        王 おう  (n,n-suf) king; ruler; sovereign; monarch; king (for senior player) (shogi)
        """
          )
        , ( "者"
          , """
        者   もの  (n) person (rarely used w.o. a qualifier); (P)
        者   もん  (n) person (rarely used w.o. a qualifier); (P)
        者   しゃ  (n,suf) someone of that nature; someone doing that work
        """
          )
        , ( "相手"
          , """
        相手  あいて (n) companion; partner; company; other party; addressee; opponent (sports, etc.); (P)
        相 あい  (pref) together; mutually; fellow
        相 こもごも  (adv,uk) alternately; in succession
        相 さが  (n) one's nature; one's destiny; custom; tradition; habit
        相 しょう (suf) minister of state
        相 そう  (n) appearance; look; countenance; a 'seeming' that fortune-tellers relate to one's fortune; aspect; phase
        相 なりくせ  (n,ok) one's nature; one's destiny; custom; tradition; habit
        """
          )
        , ( "手"
          , """
        手 て   (n,col) hand; arm; forepaw; foreleg; handle; hand; worker; help; trouble; care; effort; means; way; trick; move; technique; workmanship; hand; handwriting; kind; type; sort; one's hands; (P)
        """
          )
        , ( "に"
          , """
        に (prt) indicates such things as location of person or thing, location of short-term action, etc.; (P)
        二 に (num) two; (P)
        荷 に (n) load; baggage; cargo; freight; goods; burden; responsibility; (P)
        ニ (n) 4th in a sequence denoted by the iroha system; 4th note in the diatonic scale (used in key names, etc.)
        丹 に (n) red earth (i.e. containing cinnabar or minium); vermilion
        似 に (suf) takes after (his mother)
        尼 に (n,n-suf,abbr) bhikkhuni (fully ordained Buddhist nun)
        """
          )
        ]
