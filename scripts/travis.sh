#!/bin/sh

# Functions for Travis CI
# ------------------------
#
# This file is sourced in "before_install" section of ".travis.yml". Environment
# variable set by Travis (e.g.; $TRAVIS_OS_NAME) could be referred from this
# script.
#
# The functions with OS name suffix are specific to each OS.


# Auxiliary
# ---------

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

gen_stack_files () {
    raw=raw.githubusercontent.com
    tmpl=https://$raw/finkel-lang/finkel/master/tool/finkel.hsfiles

    cat > $HOME/.stack/config.yaml <<EOF
templates:
  params:
    author-name: finkel
    author-email: finkel@dum.my
    copyright: Copyright (c) 1000-3000 Finkel Project
    github-username: finkel-lang
EOF

    cat > stack.yaml <<EOF
resolver: lts-15.2
ghc-options:
  "\$everything": -O0
packages:
  - my-first-package
  - my-second-package
  - my-new-package
extra-deps:
  - git: https://github.com/finkel-lang/finkel
    commit: $TRAVIS_COMMIT
    subdirs:
      - kernel
      - fkc
      - setup
      - lang
      - tool
      - finkel
EOF

    $STACK new my-new-package --omit-packages $tmpl
}

build_doc_pkgs_stack () {
    ( cd doc/code && gen_stack_files && $STACK build --fast --test )
}

build_doc_pkgs_cabal () {
    ( cd doc/code && cabal v2-build all && cabal v2-test all )
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
                   finkel-kernel fkc finkel-setup \
                   finkel-lang finkel-tool finkel
            $STACK --no-terminal build --fast --test --coverage \
                   finkel-kernel fkc finkel-setup \
                   finkel-lang finkel-tool finkel
            build_doc_pkgs_stack
            ;;
        cabal)
            cabal v2-build all
            cabal v2-test all
            cabal v2-haddock all
            build_doc_pkgs_cabal
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
    $STACK --no-terminal --install-ghc test --only-dependencies
    $STACK --no-terminal build --fast --test finkel-kernel
    $STACK --no-terminal build --fast --test fkc
    $STACK --no-terminal build --fast --test finkel-setup
    $STACK --no-terminal build --fast --test finkel-lang
    $STACK --no-terminal build --fast --test finkel-tool
    $STACK --no-terminal build --fast --test finkel
}

travis_after_success_osx () {
    echo "OSX after success not yet written"
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
    travis_script_osx
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
