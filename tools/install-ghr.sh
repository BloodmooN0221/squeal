set -o errexit -o verbose

if test ! "$TRAVIS_TAG"
then
  echo 'This is not a release build.'
else
    PROJ_NAME="ghr"
    ARCHIVE_VERSION="v0.10.2"
    if [ "$TRAVIS_OS_NAME" = "linux" ]
    then
        ARCH="linux"
        EXT="tar.gz"
        ARCHIVE="${PROJ_NAME}.${EXT}"
    else
        ARCH="darwin"
        EXT="zip"
        ARCHIVE="${PROJ_NAME}.${EXT}"
    fi
  echo "Installing ${PROJ_NAME} for ${ARCH}"
  URL="https://github.com/tcnksm/${PROJ_NAME}/releases/download/${ARCHIVE_VERSION}/${PROJ_NAME}_${ARCHIVE_VERSION}_${ARCH}_386.${EXT}"
  curl -L ${URL} > "${ARCHIVE}"
  mkdir -p "$HOME/bin"
  export PATH="$HOME/bin:$PATH"
  if [ "$ARCH" = "linux" ]
  then
      tar -xvzf "${ARCHIVE}" --strip-components 1 -C "${HOME}/bin/"
  else
      unzip -j "${ARCHIVE}" -d "${HOME}/bin/"
  fi
  rm "${ARCHIVE}"
fi
