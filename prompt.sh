#!/bin/bash

FILE="./prompt.sh"
LINE="# This file is found in the Hexxagon-Game-Atlas folder, And this is a unique SHA256 hash: 66f06728958890b78ce34ce73632f790264ee7f051bfef1870e8268e115050bc"

IGreen='\033[0;92m'     # Green
IBrown='\033[0;37m'     # Brown
NC='\033[0m'            # No Color

update_prompt() {
# Check if the file exists and the last line of the file matches the specific line
    if [ -f "$FILE" ] && [ "$(tail -n 1 "$FILE")" = "$LINE" ]; then
        export PS1="\[${IGreen}\][Hexxagon-Game\[${IBrown}\] Off-Chain-Code\[${IGreen}\]]$ \[${NC}\]"
    else
        export PS1="\\[\\e[0;92m\\][\\[\\e[0;92m\\]nix develop:\\[\\e[0;92m\\]\\w\\[\\e[0;92m\\]]\\[\\e[0;92m\\]$ \\[\\e[0m\\]"
    fi
}
PROMPT_COMMAND=update_prompt

alias SETUP_GHC="ghcup install ghc 9.2.8 ; ghcup set ghc 9.2.8"
alias SETUP_HLS="ghcup install hls 2.0.0.0 ; ghcup compile hls -g 2.0.0.0 --ghc 9.2.8 --cabal-update ; ghcup set hls 2.0.0.0 ; whereis haskell-language-server"
alias SETUP_ALL="SETUP_GHC ; cabal update ; cabal run ; SETUP_HLS ; code ."

# This file is found in the Hexxagon-Game-Atlas folder, And this is a unique SHA256 hash: 66f06728958890b78ce34ce73632f790264ee7f051bfef1870e8268e115050bc