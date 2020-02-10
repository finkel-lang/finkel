#!/bin/sh

# Functions for Travis CI
# ------------------------
#
# This file is sourced in "before_install" section of
# ".travis.yml". Environment variable set by Travis (e.g.;
# $TRAVIS_OS_NAME) could be referred from this script.
#
# The functions with OS name suffix are specific to each OS.


# Initialization
# --------------

travis_init () {
    case "$TRAVIS_OS_NAME" in
        linux | osx)
            case "$EXEC" in
                stack)
                    export PATH="$HOME/.local/bin:$PATH"
                    export STACK="stack --resolver=$RESOLVER"
                    ;;
                cabal)
                    export PATH=$"HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
                    ;;
            esac
            ;;
        windows)
            export STACK="./stack.exe --resolver=$RESOLVER"
            ;;
    esac
}


# Linux
# -----

travis_install_linux () {
    case "$EXEC" in
        stack)
            mkdir -p ~/.local/bin
            url=https://get.haskellstack.org/stable/linux-x86_64.tar.gz
            travis_retry curl -L $url | \
                tar xz --wildcards --strip-components=1 \
                    -C ~/.local/bin "*/stack"
            ;;
        cabal)
            mkdir -p ~/.ghcup/bin
            url=https://gitlab.haskell.org/haskell/ghcup/raw/master/ghcup
            travis_retry curl -L $url > ~/.ghcup/bin/ghcup
            chmod +x ~/.ghcup/bin/ghcup
            ghcup -c install-cabal
            ghcup -c install $GHC_VERSION
            ghcup -c set $GHC_VERSION
            which cabal
            which ghc
            cabal --version
            ghc --version
            travis_retry cabal v2-update
            ;;
    esac
}

travis_script_linux () {
    case "$EXEC" in
        stack)
            $STACK --no-terminal --install-ghc test --only-dependencies
            $STACK --no-terminal build --fast --test --coverage \
                   --no-run-tests \
                   finkel-kernel finkel-setup finkel-lang p02 finkel-tool
            $STACK --no-terminal build --fast --test --coverage \
                   finkel-kernel finkel-setup finkel-lang finkel-tool
            ;;
        cabal)
            cabal v2-build all
            cabal v2-test all
            cabal v2-haddock all
            ;;
    esac
}

travis_after_success_linux () {
    case "$EXEC" in
        stack)
            $STACK install hpc-codecov
            HPCROOT=$($STACK path --local-hpc-root)
            DISTDIR=$($STACK path --dist-dir)
            TIX=$(find $HPCROOT -name 'all.tix')
            hpc-codecov \
                --src=kernel --mix=kernel/$DISTDIR/hpc \
                --src=setup --mix=setup/$DISTDIR/hpc \
                --src=lang --mix=lang/$DISTDIR/hpc \
                --src=tool --mix=tool/$DISTDIR/hpc \
                --out=codecov.json --verbose $TIX
            curl -s https://codecov.io/bash | bash -s
            ;;
        cabal)
            echo Not taking codecov for cabal-install yet
            ;;
    esac
}


# OSX
# ---

travis_install_osx () {
    which stack
    stack --version
}

travis_script_osx () {
    travis_script_linux
}

travis_after_success_osx () {
    travis_after_success_linux
}


# Windows
# -------

# See: https://docs.travis-ci.com/user/reference/windows/

travis_install_windows () {
    url=https://get.haskellstack.org/stable/windows-x86_64.zip
    travis_retry curl --silent --output stack.zip --location $url
    7z x stack.zip stack.exe
    echo STACK=$STACK
    $STACK --version || echo "no stack"
}

travis_script_windows () {
    travis_script_linux
}

travis_after_success_windows () {
    echo "Windows after success not yet written"
}


# Entry points for ".travis.yml"
# ------------------------------

travis_install () {
    travis_install_${TRAVIS_OS_NAME}
}

travis_script () {
    travis_script_${TRAVIS_OS_NAME}
}

travis_after_success () {
    travis_after_success_${TRAVIS_OS_NAME}
}


travis_init
