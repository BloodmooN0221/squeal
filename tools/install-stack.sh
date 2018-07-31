set -o errexit -o verbose

if test -f "$HOME/.local/bin/stack"
then
  echo 'Stack is already installed.'
else
  echo "Installing Stack for $TRAVIS_OS_NAME..."
  curl -sSL https://get.haskellstack.org/ | sh
  # URL="https://www.stackage.org/stack/$TRAVIS_OS_NAME-x86_64"
  # curl --location "$URL" > /tmp/stack.tar.gz
  # tar -xvzf /tmp/stack.tar.gz --strip-components 1
  # mkdir -p "$HOME/.local/bin"
  # mv stack "$HOME/.local/bin/"
  # rm /tmp/stack.tar.gz
fi

stack --version
