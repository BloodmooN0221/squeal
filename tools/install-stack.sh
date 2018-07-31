set -o errexit -o verbose

if test -f "$HOME/.local/bin/stack"
then
  echo 'Stack is already installed.'
else
  echo "Installing Stack for $TRAVIS_OS_NAME..."
  URL="https://www.stackage.org/stack/$TRAVIS_OS_NAME-x86_64"
  curl --location "$URL" > stack.tar.gz
  tar -xvzf stack.tar.gz --strip-components 1
  mkdir -p "$HOME/.local/bin"
  mv stack "$HOME/.local/bin/"
  rm stack.tar.gz
fi

stack --version
