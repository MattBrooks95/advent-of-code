#FILENAME="day9-short.txt"
#FILENAME="day9-short-2.txt"
FILENAME="day21-input.txt"

#https://stackoverflow.com/questions/59895/how-do-i-get-the-directory-where-a-bash-script-is-located-from-within-the-script
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
#echo "scriptDir:$SCRIPT_DIR"
INPUTS_PATH="../inputs/$FILENAME"

pushd "$SCRIPT_DIR/aoc2022"
stack run -- "$INPUTS_PATH"
#stack run --profile -- "$INPUTS_PATH" +RTS -p
popd

#stack exec --profile --  aoc2022-exe $SCRIPT_DIR/advent-of-code/2022/2022/inputs/$FILENAME
#stack run --profile -- $SCRIPT_DIR/advent-of-code/2022/2022/inputs/$FILENAME +RTS -p
