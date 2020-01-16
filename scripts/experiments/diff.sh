## This is not a standalone script; but meant to be passed to
## scripts/run-experiments.sh


## Process the arguments to this experiment uniformly
root="${BASH_SOURCE%/*}"
source "$root/process-arguments.sh"

if $showHeader; then
  echo "Location  MinSharingHeight  MergeResult"
  exit 0
fi

#######################
## Actual experiment ##


height=1
mode="nonest"
parser=""
stdiff=false
while [[ "$#" -gt 0 ]]; do
  arg=$1;
  shift;
  case $arg in
    -h|--height) height=$1; shift;;
    -m|--mode)   mode=$1; shift;;
    -p|--parser) parser=$1; shift;;
    -s|--stdiff) stdiff=true;;
    *) echo "Unknown experiment argument: $arg"; exit 1 ;;
  esac
done

timeout="30s"
function doDiff() {
  local a=$1
  local b=$2
  local hdr=""
  if $stdiff; then
    hdr="$prefix _ stdiff"
    str=$(timeout "${timeout}" stdiff -p $parser stdiff --with-stats "$1" "$2")
    res=$?
  else
    hdr="$prefix $height $mode"
    str=$(timeout "${timeout}" hdiff -p $parser diff --with-stats "$1" "$2")
    res=$?
  fi
  case $res in
    0)   output "$hdr $str"          ;;
    124) output "$hdr _ _ _ timeout" ;;
    *)   output "$hdr unknown($res)" ;;
  esac
}

echo "[$$]: Running at $prefix"

doDiff "$fo" "$fa" 
doDiff "$fo" "$fb" 
doDiff "$fo" "$fm" 
doDiff "$fa" "$fb"
