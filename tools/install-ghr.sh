set -o errexit -o verbose

if test ! "$TRAVIS_TAG"
then
  echo 'This is not a release build.'
else
    if [ "$TRAVIS_OS_NAME" = "linux" ]
    then
        ARCH="linux"
    else
        ARCH="darwin"
    fi
  echo "Installing ghr for ${ARCH}"
  URL="https://github.com/tcnksm/ghr/releases/download/v0.10.2/ghr_v0.10.2_${ARCH}_386.tar.gz"
  curl -L ${URL} > ghr.tar.gz
  mkdir -p "$HOME/bin"
  export PATH="$HOME/bin:$PATH"
  tar -xvzf ghr.zip -d "$HOME/bin"
  rm ghr.tar.gz
fi
