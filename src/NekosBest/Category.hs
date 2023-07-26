module NekosBest.Category (
    NbCategory(..),
    allCategories
) where

data NbCategory =
    Baka |
    Bite |
    Blush |
    Bored |
    Cry |
    Cuddle |
    Dance |
    Facepalm |
    Feed |
    Happy |
    Highfive |
    Hug |
    Kiss |
    Laugh |
    Neko |
    Pat |
    Poke |
    Pout |
    Shrug |
    Slap |
    Sleep |
    Smile |
    Smug |
    Stare |
    Think |
    Thumbsup |
    Tickle |
    Wave |
    Wink |
    Kitsune |
    Waifu |
    Handhold |
    Kick |
    Punch |
    Shoot |
    Husbando |
    Yeet |
    Nod |
    Nom |
    Nope deriving (Show, Eq)

allCategories :: [NbCategory]
allCategories = [Baka,Bite,Blush,Bored,Cry,Cuddle,Dance,Facepalm,Feed,Happy,Highfive,Hug,Kiss,Laugh,Neko,Pat,Poke,Pout,Shrug,Slap,Sleep,Smile,Smug,Stare,Think,Thumbsup,Tickle,Wave,Wink,Kitsune,Waifu,Handhold,Kick,Punch,Shoot,Husbando,Yeet,Nod,Nom,Nope]