export RACKET_VERSION=7.0
git clone https://github.com/greghendershott/travis-racket.git
cat travis-racket/install-racket.sh | bash # pipe to bash not sh!
export PATH="/usr/racket/bin/:$PATH"
