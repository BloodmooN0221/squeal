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
  ARCHIVE='ghr.tar.gz'
  URL="https://github.com/tcnksm/ghr/releases/download/v0.10.2/ghr_v0.10.2_${ARCH}_386.tar.gz"
  curl -L ${URL} > "${ARCHIVE}"
  mkdir -p "$HOME/bin"
  export PATH="$HOME/bin:$PATH"
  tar -xvzf "${ARCHIVE}" -C "/tmp/"
  cp "/tmp/ghr_v0.10.2_${ARCH}_386/ghr" "${HOME}/bin/"
  echo "contents of ${HOME}/bin:"
  ls -l "${HOME}/bin"
  rm "${ARCHIVE}"
fi
